/* ncgr.c - NCGR (graphic resource) support
 *
 * Copyright © 2011 magical
 *
 * This file is part of spriterip; it is licensed under the GNU GPLv3
 * and comes with NO WARRANTY. See rip.c for details.
 */

#include <stdlib.h> /* NULL, size_t, ssize_t */
#include <stdio.h> /* FILE, feof, ferror */
#include <string.h> /* memcpy, memset */

#include <sys/types.h> /* ssize_t */

#include "common.h" /* OKAY, FAIL, NOMEM, struct buffer, struct dim, u8, u16, u32, FREAD, FREE, assert, warn, buffer_alloc  */
#include "nitro.h" /* struct format_info, struct nitro, magic_t, format_header */

#include "ncgr.h"


/* NCGR */

struct CHAR {
	struct {
		magic_t magic;
		u32 size;

		u16 height;
		u16 width;

		u32 bit_depth;
		// see GBATEK. http://nocash.emubase.de/gbatek.htm#dsvideoobjs
		u32 vram_mode;
		u32 tiled;

		u32 data_size;
		u32 unknown;
	} header;

	struct buffer *buffer;
};

struct NCGR {
	struct nitro header;

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

	//warn("vram_mode: %x", self->char_.header.vram_mode);
	//warn("tiled: %x", self->char_.header.tiled);

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

	if (self != NULL &&
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

static size_t
get_boundary_size(struct NCGR *self)
{
	// OBJ mode
	if (1) {
		if ((self->char_.header.vram_mode & 0x10) == 0) {
			return 5;
		} else {
			return 5 + ((self->char_.header.vram_mode >> 20) & 0x3);
		}
	} else {
		//XXX bitmap mode
	}
	return 5;
}

/* unpack a linear block of pixels from the character data into a buffer */
static int
unpack(struct NCGR *self, size_t start, size_t size, u8 *dest)
{
	struct buffer *buffer = self->char_.buffer;
	switch (self->char_.header.bit_depth) {
	case 3:
		// 4 bits per pixel
		//warn("%u + %u / 2 <= %u", start, size, buffer->size);
		assert((start + size) / 2 <= buffer->size);
		size_t i;
		for (i = 0; i < size / 2; i++) {
			u8 byte = buffer->data[start / 2 + i];
			dest[i*2]     = byte        & 0x0f;
			dest[i*2 + 1] = (byte >> 4) & 0x0f;
		}
		break;
	case 4:
		// 8 bits per pixel
		assert((start + size) <= buffer->size);
		memcpy(dest, buffer->data + start, size);
		break;
	default:
		warn("Unknown bit depth: %d", self->char_.header.bit_depth);
		return FAIL;
	}
	return OKAY;
}

static void
untile(struct buffer *pixels, struct dim dim)
{
	// let's allocate a few kilobytes of data on the stack - yeah!
	u8 tmp_px[dim.height][dim.width];
	//memset(tmp_px, 0, pixels->size);

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

	memcpy(pixels->data, tmp_px, pixels->size);
}

struct buffer *
ncgr_get_pixels(struct NCGR *self)
{
	assert(self != NULL);
	assert(self->char_.buffer != NULL);

	struct dim dim;
	size_t size;

	ncgr_get_dim(self, &dim);
	size = dim.height * dim.width;

	struct buffer *pixels = buffer_alloc(size);
	if (pixels == NULL) {
		return NULL;
	}

	if (unpack(self, 0, size, pixels->data)) {
		FREE(pixels);
		return NULL;
	}
	assert(self->char_.header.bit_depth == 3);

	if ((self->char_.header.tiled & 0xff) == 0) {
		untile(pixels, dim);
	}

	return pixels;
}


struct buffer *
ncgr_get_cell_pixels(struct NCGR *self, u16 tile, struct dim cell_dim)
{
	assert(self != NULL);
	assert(self->char_.buffer != NULL);

	size_t size = cell_dim.height * cell_dim.width;

	struct buffer *pixels = buffer_alloc(size);
	if (pixels == NULL) {
		return NULL;
	}

	if ((self->char_.header.tiled & 0xff) == 0) {
		size_t start = (tile << get_boundary_size(self)) * 2;

		if (unpack(self, start, size, pixels->data)) {
			goto error;
		}

		untile(pixels, cell_dim);
	} else {
		u16 width = self->char_.header.width; // width in tiles
		assert(width != 0xffff);

		size_t start_x, start_y, start;
		int y;

		start_y = (tile / width) * 8;
		start_x = (tile % width) * 8;
		for (y = 0; y < cell_dim.height; y++) {
			// In at least one case (Hydreigon), there are objs
			// whose dimensions (incorrectly, i assume) extend
			// beyond the boundaries of the character data.
			if (!((start_y + y) / 8 < self->char_.header.height)) {
				warn("obj extends below character data"
				     " (tile=%d, dim=%dx%d)",
				     tile, cell_dim.width, cell_dim.height);
				break;
			}

			start = (start_y + y) * width * 8 + start_x;
			if (unpack(self, start, cell_dim.width,
			           &pixels->data[y * cell_dim.width])) {
				goto error;
			}
		}
	}

	return pixels;

error:
	FREE(pixels);
	return NULL;
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
