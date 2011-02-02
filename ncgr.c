
#include <stdlib.h> /* NULL, size_t, ssize_t */
#include <stdio.h> /* FILE, feof, ferror */
#include <string.h> /* memcpy, memset */

#include <sys/types.h> /* ssize_t */

#include "common.h" /* OKAY, FAIL, NOMEM, struct buffer, struct dim, u8, u16, u32, FREAD, FREE, assert, warn, buffer_alloc  */
#include "nitro.h" /* struct format_info, struct standard_header, magic_t, format_header */

#include "ncgr.h"


/* NCGR */

struct CHAR {
	struct {
		magic_t magic;
		u32 size;

		u16 height;
		u16 width;

		u32 bit_depth;
		u32 padding;
		u32 tiled;

		u32 data_size;
		u32 unknown;
	} header;

	struct buffer *buffer;
};

struct NCGR {
	struct standard_header header;

	struct CHAR char_;
	//struct CPOS cpos;
};

static int
ncgr_read(void *buf, FILE *fp)
{
	struct NCGR *self = buf;
	FREAD(fp, &self->header, 1);

	assert(self->header.chunk_count == 1 || self->header.chunk_count == 2);

	FREAD(fp, &self->char_.header, 1);
	if (ferror(fp) || feof(fp)) {
		return FAIL;
	}

	assert(self->char_.header.magic == (magic_t)'CHAR');

	self->char_.buffer = buffer_alloc(self->char_.header.data_size);
	if (self->char_.buffer == NULL) {
		return NOMEM;
	}

	FREAD(fp, self->char_.buffer->data, self->char_.buffer->size);
	if (ferror(fp) || feof(fp)) {
		return FAIL;
	}

	return OKAY;
}

static void
ncgr_free(void *buf) {
	struct NCGR *self = buf;

	if (self != NULL ||
	    self->header.magic == (magic_t)'NCGR') {
		FREE(self->char_.buffer);
	}
}

struct format_info NCGR_format = {
	format_header('NCGR', struct NCGR),
	
	.read = ncgr_read,
	.free = ncgr_free,
};

int
ncgr_get_dim(struct NCGR *self, struct dim *dim)
{
	assert(self != NULL);
	assert(dim != NULL);

	if (self->char_.header.height == 0xffff) {
		// no dimensions, so we'll just have to guess
		int size;
		dim->width = 64;
		switch (self->char_.header.bit_depth) {
		case 3: size = self->char_.header.data_size * 2; break;
		case 4: size = self->char_.header.data_size; break;
		default: assert(!"bit depth is not 3 or 4");
		}
		// poor man's ceil()
		dim->height = (size + dim->width - 1) / dim->width;
	} else {
		dim->height = self->char_.header.height * 8;
		dim->width = self->char_.header.width * 8;
	}

	return OKAY;
}

struct buffer *
ncgr_get_pixels(struct NCGR *self)
{
	assert(self != NULL);
	assert(self->char_.buffer != NULL);

	struct buffer *buffer = self->char_.buffer;

	struct dim dim;
	size_t size;

	ncgr_get_dim(self, &dim);
	size = dim.height * dim.width;

	struct buffer *pixels = buffer_alloc(size);
	if (pixels == NULL) {
		return NULL;
	}
	memset(pixels->data, 0, size);

	/* unpack the pixels */

	size_t i;
	switch (self->char_.header.bit_depth) {
	case 3:
		// 4 bits per pixel
		assert(buffer->size * 2 == (unsigned)size);
		for (i = 0; i < buffer->size; i++) {
			u8 byte = buffer->data[i];
			pixels->data[i*2] = byte & 0x0f;
			pixels->data[i*2 + 1] = (byte >> 4) & 0x0f;
		}
		break;
	case 4:
		// 8 bits per pixel
		assert(buffer->size == (unsigned)size);
		for (i = 0; i < buffer->size; i++) {
			pixels->data[i] = buffer->data[i];
		}
		break;
	default:
		warn("Unknown bit depth: %d", self->char_.header.bit_depth);
		FREE(pixels);
		return NULL;
	}
	assert(self->char_.header.bit_depth == 3);

	/* untile the image, if necessary */

	if ((self->char_.header.tiled & 0xff) == 0) {
		// let's allocate a few kilobytes of data on the stack - yeah!
		u8 tmp_px[dim.height][dim.width];
		memset(tmp_px, 0, size);

		int x, y, tx, ty, cx, cy, i;
		i = 0;
		for (y = 0; y < dim.height / 8; y++) {
		for (x = 0; x < dim.width / 8; x++) {
			for (ty = 0; ty < 8; ty++) {
			for (tx = 0; tx < 8; tx++) {
				cy = y * 8 + ty;
				cx = x * 8 + tx;
				tmp_px[cy][cx] = pixels->data[i];
				i++;
			}
			}
		}
		}

		memcpy(pixels->data, tmp_px, size);
	}

	return pixels;
}

u8
ncgr_get_pixel(struct NCGR *self, int tile, u32 x, u32 y, u32 cellwidth)
{
	assert(self != NULL);

	size_t offset = 0;

	if (self->char_.header.tiled == 0) {
		// tiled
		offset += tile * 64;
		offset += y / 8 * (cellwidth / 8) * 64;
		offset += x / 8 * 64;
		offset += y % 8 * 8;
		offset += x % 8;

		//offset += x % 8 + (x/8 + y/8 * cellwidth + y % 8) * 8;
	} else {
		u16 width = self->char_.header.width;
		// not tiled
		//offset += x % 8 + (x/8 + y/8 * cellwidth + y % 8) * 8;
		offset += tile / width * (width * 64);
		offset += tile % width * 8;
		offset += y * width * 8;
		offset += x;
	}

	u8 pixel;
	if (self->char_.header.bit_depth == 3) {
		offset /= 2;
		pixel = self->char_.buffer->data[offset];
		if (x & 1) {
			pixel >>= 4;
		} else {
			pixel &= 0xf;
		}
	} else {
		pixel = self->char_.buffer->data[offset];
	}

	return pixel;
}

/******************************************************************************/

/* Pokemon and trainer images are encrypted with a simple
 * stream cipher based on the pokemon rng. */

#define MULT 0x41c64e6dL
#define ADD 0x6073L

void
ncgr_decrypt_dp(struct NCGR *self)
{
	const ssize_t size = self->char_.buffer->size / sizeof(u16);
	u16 *data = (u16*)self->char_.buffer->data;

	u16 seed = data[size - 1];
	for (ssize_t i = size - 1; i >= 0; i--) {
		data[i] ^= seed;
		seed = seed * MULT + ADD;
	}
}

void
ncgr_decrypt_pt(struct NCGR *self)
{
	const ssize_t size = self->char_.buffer->size / sizeof(u16);
	u16 *data = (u16*)self->char_.buffer->data;

	u16 seed = data[0];
	for (ssize_t i = 0; i < size; i++) {
		data[i] ^= seed;
		seed = seed * MULT + ADD;
	}
}

#undef MULT
#undef ADD
