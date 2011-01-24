
#include <stdlib.h> /* EXIT_FAILURE, EXIT_SUCCESS, NULL, size_t, calloc, exit, free, malloc */
#include <stdio.h> /* SEEK_CUR, SEEK_SET, FILE, off_t, fclose, feof, ferror, fprintf, fopen, fread, fseeko, ftello, fwrite, perror, printf, putchar, sprintf, vfprintf */
#include <stdint.h> /* int16_t, uint8_t, uint16_t, uint32_t */
#include <stdarg.h> /* va_list, va_end, va_start */
#include <string.h> /* memcpy, memset */
#include <math.h> /* round */

#ifdef _WIN32
# include <direct.h> /* _mkdir */
# define mkdir(path,mode)  _mkdir(path)
#else
# include <sys/stat.h> /* mkdir */
#endif

#include <errno.h> /* EEXIST, errno */
#include <assert.h> /* assert */

#include "png.h" /* png_*, setjmp */
#include "zlib.h" /* Z_BEST_SPEED */


#define FILENAME "pokegra.narc"
#define OUTDIR "test"


#define fseeko fseek
#define ftello ftell
#define off_t int


#define UNUSED(x) ((void)x)

// Helpful macros
// ALLOC and CALLOC fill the size parameter automatically and
// set the variable to the returned value.
// FREE sets the variable to NULL
#define ALLOC(x) ((x) = malloc(sizeof(*(x))))
#define CALLOC(x, nmemb) ((x) = calloc(nmemb, sizeof(*(x))))
#define FREE(x) (free(x), ((x) = NULL))

// FREAD sets the size parameter automatically, and also moves
// the file pointer to the front
#define FREAD(fp, x, nmemb) (fread(x, sizeof(*(x)), nmemb, fp))

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;

typedef int16_t s16;

typedef u32 magic_t;

enum status {
	OKAY = 0,
	FAIL,
	ABORT,
	NOMEM,
};

/******************************************************************************/

/* A buffer is a sized 1-dimensional array of bytes. */
struct buffer {
	size_t size;
	u8 data[];
};

/* dim - dimension - a height and width. In other words, a 2d size. */
struct dim {
	int height;
	int width;
};

/* a color */
struct rgba {
	u8 r;
	u8 g;
	u8 b;
	u8 a;
};

struct palette {
	int bit_depth;
	int count;
	struct rgba *colors;
};

/* Specifically an indexed image - not truecolor. */
struct image {
	struct buffer *pixels;
	struct palette *palette;
	struct dim dim;
};

/******************************************************************************/

struct buffer *
buffer_alloc(size_t size)
{
	struct buffer *buffer = malloc(sizeof(struct buffer) + size);
	if (buffer != NULL) {
		buffer->size = size;
		memset(buffer->data, 0, buffer->size);
		return buffer;
	}
	return NULL;
}

/* There is no buffer_free() - just use free(). */

/******************************************************************************/

struct standard_header {
	magic_t magic;
	u16 bom;
	u16 version;

	u32 size;
	u16 header_size;
	u16 chunk_count;
};


/* NARC */
struct FATB {
	struct {
		magic_t magic;
		u32 size;

		u32 file_count;
	} header;

	struct fatb_record {
		u32 start;
		u32 end;
	}
	*records;
};

struct FNTB {
	struct {
		magic_t magic;
		u32 size;

		/*XXX*/
	} header;
};


struct NARC {
	struct standard_header header;

	struct FATB fatb;

	struct FNTB fntb;

	FILE *fp;

	/* offset to the beginning of the FIMG data */
	off_t data_offset;
};


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


/* NCLR */

struct PLTT {
	struct {
		magic_t magic;
		u32 size;

		u16 bit_depth;
		u16 unknown;

		u32 padding;

		u32 data_size;
		u32 color_count;
	} header;

	struct buffer *buffer;
};

struct NCLR {
	struct standard_header header;

	struct PLTT pltt;

	//struct PCMP pcmp;
};


/* NCER */

struct CEBK_celldata {
	u16 oam_count;
	u16 unknown;
	u32 oam_offset;
};

struct CEBK_celldata_ex {
	s16 x_max;
	s16 y_max;
	s16 x_min;
	s16 y_min;
};

// OAM = Object Attribute Map; see GBATEK for details.
struct CEBK_OAM {
	u16 obj_attribute[3];
};

struct CEBK_partitiondata {
	u32 start;
	u32 size;
};

struct CEBK {
	struct {
		magic_t magic;
		u32 size;

		u16 cell_count;
		u16 cell_type;

		u32 cell_data_offset;
		u32 flags;

		u32 partition_data_offset;

		u32 padding[2];
	} header;

	struct CEBK_celldata *cell_data;
	// May be NULL if cell_type == 0
	struct CEBK_celldata_ex *cell_data_ex;

	int oam_count;
	struct CEBK_OAM *oam_data;

	struct CEBK_partitiondata *partition_data;
};

struct NCER {
	struct standard_header header;

	struct CEBK cebk;
	//struct LABL labl;
	//struct UEXT uext;
};

/******************************************************************************/

static char magic_buf[5];
#define STRMAGIC(magic) (strmagic((magic), magic_buf))

static void
warn(const char *s, ...)
{
	va_list va;
	va_start(va, s);
	vfprintf(stderr, s, va);
	va_end(va);
	fprintf(stderr, "\n");
}

static char *
strmagic(magic_t magic, char *buf)
{
	for (int i = 0; i < 4; i++) {
		buf[i] = (char)((magic >> ((3 - i) * 8)) & 0xff);
	}
	buf[4] = '\0';
	return buf;
}

static inline int
maxval_from_bitdepth(int bit_depth)
{
	return (1 << bit_depth) - 1;
}


/******************************************************************************/


static int
narc_read(void *buf, FILE *fp)
{
	struct NARC *self = buf;
	assert(self != NULL);

	/* For efficiency, we stash the file pointer so we can lazily load
	 * the contained files - although this doesn't seem like the best
	 * solution */
	self->fp = fp;

	FREAD(fp, &self->header, 1);

	assert(self->header.chunk_count == 3);

	/* read the FATB chunk */
	FREAD(fp, &self->fatb.header, 1);
	assert(self->fatb.header.magic == (magic_t)'FATB');
	if (ferror(fp) || feof(fp)) {
		return FAIL;
	}

	CALLOC(self->fatb.records, self->fatb.header.file_count);

	if (self->fatb.records == NULL) {
		return NOMEM;
	}

	FREAD(fp, self->fatb.records, self->fatb.header.file_count);

	if (ferror(fp) || feof(fp)) {
		goto error;
	}


	/* skip the FNTB chunk */
	FREAD(fp, &self->fntb.header, 1);
	assert(self->fntb.header.magic == (magic_t)'FNTB');
	if (ferror(fp) || feof(fp)) {
		goto error;
	}
	fseeko(self->fp, (off_t)(self->fntb.header.size - sizeof(self->fntb.header)), SEEK_CUR);

	/* set the data offset */
	struct { magic_t magic; u32 size; } fimg_header;

	FREAD(fp, &fimg_header, 1);
	assert(fimg_header.magic == (magic_t)'FIMG');
	if (ferror(fp) || feof(fp)) {
		goto error;
	}

	self->data_offset = ftello(fp);

	return OKAY;

	error:
	FREE(self->fatb.records);
	return FAIL;
}

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
		FREE(self->char_.buffer);
		return FAIL;
	}

	return OKAY;
}

static int
nclr_read(void *buf, FILE *fp)
{
	struct NCLR *self = buf;
	FREAD(fp, &self->header, 1);

	FREAD(fp, &self->pltt.header, 1);
	if (ferror(fp) || feof(fp)) {
		return FAIL;
	}

	assert(self->pltt.header.magic == (magic_t)'PLTT');

	//assert(self->pltt.header.bit_depth == 4);
	assert(self->pltt.header.color_count == 16);

	self->pltt.buffer = buffer_alloc(self->pltt.header.data_size);
	if (self->pltt.buffer == NULL) {
		return NOMEM;
	}

	FREAD(fp, self->pltt.buffer->data, self->pltt.buffer->size);
	if (ferror(fp) || feof(fp)) {
		FREE(self->pltt.buffer);
		return FAIL;
	}

	return OKAY;
}

static int
ncer_read(void *buf, FILE *fp)
{
	struct NCER *self = buf;
	assert(self != NULL);

	FREAD(fp, &self->header, 1);
	assert(self->header.magic == (magic_t)'NCER');
	assert(self->header.chunk_count == 3);

	FREAD(fp, &self->cebk.header, 1);
	assert(self->cebk.header.magic == (magic_t)'CEBK');

	if (CALLOC(self->cebk.cell_data,
	           self->cebk.header.cell_count) == NULL) {
		return NOMEM;
	}
	switch (self->cebk.header.cell_type) {
	case 0:
		FREAD(fp, self->cebk.cell_data, self->cebk.header.cell_count);
		break;
	case 1:
		if (CALLOC(self->cebk.cell_data_ex,
		           self->cebk.header.cell_count) == NULL) {
			FREE(self->cebk.cell_data);
			return NOMEM;
		}
		for (int i = 0; i < (signed long)self->cebk.header.cell_count; i++) {
			FREAD(fp, &self->cebk.cell_data[i], 1);
			FREAD(fp, &self->cebk.cell_data_ex[i], 1);
		}
		break;
	default:
		warn("Unknown cell type: %d", self->cebk.header.cell_type);
		return FAIL;
	}

	self->cebk.oam_count = 0;
	for (int i = 0; i < (signed long)self->cebk.header.cell_count; i++) {
		self->cebk.oam_count += self->cebk.cell_data[i].oam_count;
	}

	if (CALLOC(self->cebk.oam_data, self->cebk.oam_count) == NULL) {
		FREE(self->cebk.cell_data);
		FREE(self->cebk.cell_data_ex);
		return FAIL;
	}
	FREAD(fp, self->cebk.oam_data, self->cebk.oam_count);

	// partition_data?

	return OKAY;
}

static void
narc_free(void *buf)
{
	struct NARC *self = buf;

	if (self != NULL &&
	    self->header.magic == (magic_t)'CRAN') {
		FREE(self->fatb.records);
	}
}

static void
ncgr_free(void *buf) {
	struct NCGR *self = buf;

	if (self != NULL ||
	    self->header.magic == (magic_t)'NCGR') {
		FREE(self->char_.buffer);
	}
}

static void
nclr_free(void *buf) {
	struct NCLR *self = buf;

	if (self != NULL ||
	    self->header.magic == (magic_t)'NCLR') {
		FREE(self->pltt.buffer);
	}
}

static void
ncer_free(void *buf)
{
	struct NCER *self = buf;
	if (self != NULL &&
	    self->header.magic == (magic_t)'NCER') {
		FREE(self->cebk.cell_data);
		FREE(self->cebk.cell_data_ex);
		FREE(self->cebk.oam_data);
		FREE(self->cebk.partition_data);
	}
}


static const struct format_info {
	magic_t magic;

	size_t size;

	int (*init)(void *);
	int (*read)(void *, FILE *);
	void (*free)(void *);
} *formats[] = {
	#define F & (struct format_info)
	/* the NARC signature is big-endian for some reason */
	F{'CRAN', sizeof(struct NARC), NULL, narc_read, narc_free},

	/* lesser formats */
	F{'NCGR', sizeof(struct NCGR), NULL, ncgr_read, ncgr_free},
	F{'NCLR', sizeof(struct NCLR), NULL, nclr_read, nclr_free},
	F{'NCER', sizeof(struct NCER), NULL, ncer_read, ncer_free},

	/* known but unsupported formats */
	#define UNSUPPORTED(m) F{.magic = (magic_t)m}
	UNSUPPORTED('NANR'),
	UNSUPPORTED('NMAR'),
	UNSUPPORTED('NMCR'),
	#undef UNSUPPORTED

	#undef F
	NULL
};


static const struct format_info *
format_lookup(magic_t magic)
{
	const struct format_info **fmt = formats;

	while (*fmt != NULL) {
		if ((*fmt)->magic == magic) {
			return *fmt;
		}
		fmt++;
	}

	return NULL;
}

static void *nitro_read(FILE *);

static void *
narc_load_file(struct NARC *self, int index)
{
	assert(self != NULL);
	assert(self->fp != NULL);

	assert(0 <= index && index < (signed long)self->fatb.header.file_count);

	struct fatb_record record = self->fatb.records[index];

	assert(record.start <= record.end);
	off_t chunk_size = record.end - record.start;

	if (chunk_size <= 4) {
		return NULL;
	}

	fseeko(self->fp, self->data_offset + record.start, SEEK_SET);

	if (ferror(self->fp)) {
		return NULL;
	}

	return nitro_read(self->fp);
}

static void *
nitro_read(FILE *fp)
{
	assert(fp != NULL);

	void *chunk;

	magic_t magic;
	FREAD(fp, &magic, 1);
	fseeko(fp, -(signed)(sizeof(magic)), SEEK_CUR);

	const struct format_info *fmt = format_lookup(magic);

	if (fmt == NULL) {
		warn("Unknown format: %08x", magic);
		return NULL;
	} else if (fmt->size == 0) {
		warn("Unsupported format: %s", STRMAGIC(magic));
		chunk = calloc(1, sizeof(struct standard_header));
	} else {
		chunk = calloc(1, fmt->size);
	}

	if (chunk == NULL) {
		return NULL;
	}

	/* time to actually load it */

	if (fmt->init != NULL) {
		if (fmt->init(chunk)) {
			goto error;
		}
	} else {
		/* it's already zeroed; there's nothing more to do */
	}

	if (fmt->read != NULL) {
		if (fmt->read(chunk, fp)) {
			goto error;
		}
	} else {
		fread(chunk, sizeof(struct standard_header), 1, fp);
	}

	if (ferror(fp) || feof(fp)) {
		goto error;
	}

	return chunk;

	error:
	FREE(chunk);

	return NULL;
}


void
nitro_free(void *chunk)
{
	struct standard_header *header = chunk;
	if (chunk != NULL) {
		const struct format_info *fmt = format_lookup(header->magic);

		if (fmt != NULL && fmt->free != NULL) {
			fmt->free(chunk);
		}
	}
}

/******************************************************************************/

static struct palette *
nclr_get_palette(struct NCLR *self, int index)
{
	UNUSED(index);
	assert(index == 0);

	assert(self != NULL);

	if (!(0 <= index && index < (signed long)self->header.chunk_count)) {
		return NULL;
	}

	struct PLTT *pltt = &self->pltt;

	assert(pltt->buffer != NULL);

	int count = 16;

	/*
	switch (pltt->header.bit_depth) {
	case 3: count = 16; break;
	case 4: count = 256; break;
	default: assert(!"Unknown bit depth");
	};
	*/

	struct palette *palette;

	if (ALLOC(palette) == NULL) {
		return NULL;
	}
	if (CALLOC(palette->colors, count) == NULL) {
		FREE(palette);
		return NULL;
	}

	assert(pltt->buffer->size == sizeof(u16) * count);

	palette->count = count;
	palette->bit_depth = 5; // XXX

	/* unpack the colors */

	u16 *colors16 = (u16 *)pltt->buffer->data;
	for (int i = 0; i < count; i++) {
		palette->colors[i].r = colors16[i] & 0x1f;
		palette->colors[i].g = (colors16[i] >> 5) & 0x1f;
		palette->colors[i].b = (colors16[i] >> 10) & 0x1f;

		/* The first palette entry is always transparent;
		 * the rest are not. */
		palette->colors[i].a = (i == 0) ? 31 : 0;
	}

	return palette;
}

static int
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

static struct buffer *
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
	switch(self->char_.header.bit_depth) {
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

/* Pokemon and trainer images are encrypted with a simple
 * stream cipher based on the pokemon rng. */

#define MULT 0x41c64e6dL
#define ADD 0x6073L

static void
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

static void
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

/******************************************************************************/

static int
image_write_pam(struct image *self, FILE *fp)
{
	assert(self != NULL);
	assert(self->pixels != NULL);
	assert(self->palette != NULL);

	int maxval = maxval_from_bitdepth(self->palette->bit_depth);

	fprintf(fp, "P7\n");
	fprintf(fp, "WIDTH %d\n", self->dim.width);
	fprintf(fp, "HEIGHT %d\n", self->dim.height);
	fprintf(fp, "DEPTH 4\n");
	fprintf(fp, "TUPLTYPE RGB_ALPHA\n");
	fprintf(fp, "MAXVAL %d\n", maxval);
	fprintf(fp, "ENDHDR\n");

	for (size_t i = 0; i < self->pixels->size; i++) {
		/* XXX bounds check for colors[]  */
		struct rgba *color = &self->palette->colors[self->pixels->data[i]];
		fwrite(color, 1, sizeof(color), fp);
	}

	return ferror(fp) ? FAIL : OKAY;
}

static int
image_write_png(struct image *self, FILE *fp)
{
	assert(self != NULL);
	assert(self->pixels != NULL);
	assert(self->palette != NULL);

	const int bit_depth = self->palette->bit_depth;

	png_bytepp row_pointers = NULL;
	png_colorp palette = NULL;
	png_color_8 sig_bit;
	png_byte trans[1] = {0};

	CALLOC(row_pointers, self->dim.height);
	CALLOC(palette, self->palette->count);

	if (row_pointers == NULL || palette == NULL) {
		FREE(row_pointers);
		FREE(palette);
		return NOMEM;
	}

	/* expand the palette */

	double factor = 255.0 / (double)maxval_from_bitdepth(bit_depth);
	for (int i = 0; i < self->palette->count; i++) {
		struct rgba *color = &self->palette->colors[i];
		palette[i].red = (int)round(color->r * factor);
		palette[i].green = (int)round(color->g * factor);
		palette[i].blue = (int)round(color->b * factor);
	}

	/* set the row pointers */

	for (int i = 0; i < self->dim.height; i++) {
		row_pointers[i] = &self->pixels->data[i * self->dim.width];
	}

	/* set the significant bits */

	sig_bit.red = bit_depth;
	sig_bit.green = bit_depth;
	sig_bit.blue = bit_depth;


	png_structp png = png_create_write_struct(
		PNG_LIBPNG_VER_STRING, NULL,  NULL, NULL);
	if (!png) {
		return NOMEM; // leak
	}

	png_infop info = png_create_info_struct(png);
	if (!info) {
		png_destroy_write_struct(&png, (png_infopp)NULL);
		return NOMEM; // leak
	}

	if (setjmp(png_jmpbuf(png))) {
		png_destroy_write_struct(&png, &info);
		return FAIL; // leak
	}

	png_init_io(png, fp);

	// We're going to recompress the images with advdef later; no sense
	// wasting time now.
	png_set_compression_level(png, Z_BEST_SPEED);

	png_set_IHDR(png, info,
		self->dim.width, self->dim.height,
		4, /* bit depth */
		PNG_COLOR_TYPE_PALETTE,
		PNG_INTERLACE_NONE,
		PNG_COMPRESSION_TYPE_DEFAULT,
		PNG_FILTER_TYPE_DEFAULT
	);

	png_set_PLTE(png, info, palette, self->palette->count);
	png_set_tRNS(png, info, trans, 1, NULL);
	png_set_sBIT(png, info, &sig_bit);

	png_set_rows(png, info, row_pointers);

	png_write_png(png, info, PNG_TRANSFORM_PACKING, NULL);

	png_destroy_write_struct(&png, &info);
	FREE(row_pointers);
	FREE(palette);
	return OKAY;
}

static void image_free(struct image *);

static int
image_init(struct image *self, struct NCGR *ncgr, struct NCLR *nclr)
{
	assert(self != NULL);
	assert(ncgr != NULL);
	assert(nclr != NULL);

	const int palette_index = 0;

	struct buffer *pixels = ncgr_get_pixels(ncgr);
	struct palette *palette = nclr_get_palette(nclr, palette_index);

	if (pixels != NULL && palette != NULL) {
		memset(self, 0, sizeof(*self));
		ncgr_get_dim(ncgr, &self->dim);
		return OKAY;
	} else {
		image_free(self);
		return FAIL;
	}
}

static void
image_free(struct image *self)
{
	if (self->palette != NULL) {
		FREE(self->palette->colors);
		FREE(self->palette);
	}
	FREE(self->pixels);
}


/******************************************************************************/

static struct NARC *
open_narc(const char *filename)
{
	assert(filename != NULL);

	FILE *fp = fopen(filename, "rb");
	if (fp == NULL) {
		goto error;
	}

	struct NARC *narc = nitro_read(fp);

	if (narc == NULL) {
		goto error;
	}

	if (narc->header.magic != (magic_t)'CRAN') {
		warn("Not a NARC");
		goto error;
	}

	return narc;

	error:
	if (errno) {
		perror("Failed to load NARC");
	} else {
		warn("Failed to load NARC");
	}
	exit(EXIT_FAILURE);
}


static int
list(void)
{
	struct NARC *narc;
	struct standard_header *chunk;

	narc = open_narc(FILENAME);

	for (int i = 0; i < (signed long)narc->fatb.header.file_count; i++) {
		chunk = narc_load_file(narc, i);
		if (chunk != NULL) {
			printf("%3d %s\n", i, STRMAGIC(chunk->magic));
		} else {
			printf("%3d (null)\n", i);
		}
	}

	exit(EXIT_SUCCESS);
}


static void
write_sprite(struct image *image, char *outfile)
{
	FILE *outfp = fopen(outfile, "wb");
	if (outfp != NULL) {
		if (image_write_png(image, outfp)) {
			warn("Error writing %s.", outfile);
		}
		fclose(outfp);
	} else {
		perror(outfile);
	}
}

static void
rip_sprites(void)
{
	struct NARC *narc = open_narc(FILENAME);
	char outfile[256] = "";

	const struct sprite_dirs {
		const char *normal;
		const char *shiny;
	} const dirs[] = {
		{"back/female", "back/shiny/female"},
		{"back", "back/shiny"},
		{"female", "shiny/female"},
		{"", "shiny"},
	};

	#define MKDIR(dir) \
	if (mkdir(OUTDIR "/" dir, 0755)) { \
		switch (errno) { \
		case 0: \
		case EEXIST: \
			break; \
		default: \
			perror("mkdir: " OUTDIR "/" dir); \
			exit(EXIT_FAILURE); \
		} \
	}

	MKDIR("female")
	MKDIR("shiny")
	MKDIR("shiny/female")
	MKDIR("back")
	MKDIR("back/female")
	MKDIR("back/shiny")
	MKDIR("back/shiny/female")

	struct image image;
	memset(&image, 0, sizeof(image));

	for (int n = 1; n <= 493; n++) {
		struct NCLR *normal_nclr = narc_load_file(narc, n*6 + 4);
		struct NCLR *shiny_nclr = narc_load_file(narc, n*6 + 5);

		if (normal_nclr == NULL || shiny_nclr == NULL) {
			if (errno) perror(NULL);
			else warn("Error reading palettes.");
			exit(EXIT_FAILURE);
		}

		assert(normal_nclr->header.magic == (magic_t)'NCLR');
		assert(shiny_nclr->header.magic == (magic_t)'NCLR');

		struct palette *normal_palette = nclr_get_palette(normal_nclr, 0);
		struct palette *shiny_palette = nclr_get_palette(shiny_nclr, 0);

		nitro_free(normal_nclr);
		nitro_free(shiny_nclr);

		FREE(normal_nclr);
		FREE(shiny_nclr);

		if (normal_palette == NULL || shiny_palette == NULL) {
			if (errno) perror(NULL);
			else warn("Error loading palettes.");
			exit(EXIT_FAILURE);
		}

		for (int i = 0; i < 4; i++) {
			const struct sprite_dirs *d = &dirs[i];

			struct NCGR *ncgr = narc_load_file(narc, n*6 + i);
			if (ncgr == NULL) {
				// this is fine
				continue;
			}

			assert(ncgr->header.magic == (magic_t)'NCGR');
			ncgr_decrypt_pt(ncgr);

			sprintf(outfile, "%s/%s/%d.png", OUTDIR, d->normal, n);

			image.pixels = ncgr_get_pixels(ncgr);
			if (image.pixels == NULL) {
				warn("Error ripping %s.", outfile);
				continue;
			}

			ncgr_get_dim(ncgr, &image.dim);

			nitro_free(ncgr);
			FREE(ncgr);

			image.palette = normal_palette;
			write_sprite(&image, outfile);

			sprintf(outfile, "%s/%s/%d.png", OUTDIR, d->shiny, n);
			image.palette = shiny_palette;
			write_sprite(&image, outfile);

			FREE(image.pixels);
		}

		FREE(normal_palette->colors);
		FREE(shiny_palette->colors);

		FREE(normal_palette);
		FREE(shiny_palette);
	}

	printf("done\n");
	exit(EXIT_SUCCESS);
}

/* for d/p */
static void
rip_trainers(void)
{
	struct NARC *narc = open_narc(FILENAME);
	char outfile[256] = "";
	const int trainer_count = narc->fatb.header.file_count / 2;

	struct image image = {};

	for (int n = 0; n < trainer_count; n++) {
		sprintf(outfile, "%s/%d.png", OUTDIR, n);

		struct NCGR *ncgr = narc_load_file(narc, n*2 + 0);
		if (ncgr == NULL) {
			if (errno) perror(outfile);
			continue;
		}
		assert(ncgr->header.magic == (magic_t)'NCGR');

		struct NCLR *nclr = narc_load_file(narc, n*2 + 1);
		if (nclr == NULL) {
			if (errno) perror(outfile);
			nitro_free(ncgr);
			FREE(ncgr);
			continue;
		}
		assert(nclr->header.magic == (magic_t)'NCLR');

		image.palette = nclr_get_palette(nclr, 0);
		if (image.palette == NULL) {
			warn("Error ripping %s.", outfile);
			continue; // leak
		}

		nitro_free(nclr);
		FREE(nclr);

		ncgr_decrypt_pt(ncgr);

		image.pixels = ncgr_get_pixels(ncgr);
		if (image.pixels == NULL) {
			warn("Error ripping %s.", outfile);
			continue; // leak
		}

		ncgr_get_dim(ncgr, &image.dim);

		nitro_free(ncgr);
		FREE(ncgr);

		write_sprite(&image, outfile);

		FREE(image.pixels);
		FREE(image.palette->colors);
		FREE(image.palette);
	}

	printf("done\n");
	exit(EXIT_SUCCESS);
}

/* for hg/ss */
static void
rip_trainers2(void)
{
	struct NARC *narc = open_narc(FILENAME);
	char outfile[256] = "";
	const int trainer_count = narc->fatb.header.file_count / 5;

	MKDIR("frames");

	struct image image = {};

	for (int n = 0; n < trainer_count; n++) {
		struct NCLR *nclr = narc_load_file(narc, n*5 + 1);
		if (nclr == NULL) {
			if (errno) perror(NULL);
			continue;
		}
		assert(nclr->header.magic == (magic_t)'NCLR');

		image.palette = nclr_get_palette(nclr, 0);
		if (image.palette == NULL) {
			if (errno) perror(NULL);
			nitro_free(nclr);
			FREE(nclr);
			continue;
		}

		nitro_free(nclr);
		FREE(nclr);

		int spriteindex;
		for (int i = 0; i < 2; i++) {
			switch (i) {
			case 0:
				spriteindex = 0;
				sprintf(outfile, "%s/frames/%d.png", OUTDIR, n);
				break;
			case 1:
				spriteindex = 4;
				sprintf(outfile, "%s/%d.png", OUTDIR, n);
				break;
			}
			puts(outfile);

			struct NCGR *ncgr = narc_load_file(narc, n*5 + spriteindex);
			if (ncgr == NULL) {
				if (errno) perror(outfile);
				continue;
			}
			assert(ncgr->header.magic == (magic_t)'NCGR');

			if (i == 1) {
				/* pt for platinum, dp for hgss */
				ncgr_decrypt_dp(ncgr);
			}

			image.pixels = ncgr_get_pixels(ncgr);
			if (image.pixels == NULL) {
				warn("Error ripping %s.", outfile);
				nitro_free(ncgr);
				FREE(ncgr);
				continue;
			}

			ncgr_get_dim(ncgr, &image.dim);

			nitro_free(ncgr);
			FREE(ncgr);

			write_sprite(&image, outfile);

			FREE(image.pixels);
		}
		FREE(image.palette->colors);
		FREE(image.palette);
	}

	printf("done\n");
	exit(EXIT_SUCCESS);
}

/******************************************************************************/

int
main(int argc, char *argv[])
{
	UNUSED(argc);
	UNUSED(argv);

	list();
	//rip_sprites();
	//rip_trainers();
	//rip_trainers2();
}
