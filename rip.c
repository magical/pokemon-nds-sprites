
#include <stdlib.h> /* EXIT_SUCCESS, EXIT_FAILURE, size_t, calloc, exit, free, malloc */
#include <stdio.h> /* SEEK_CUR, SEEK_SET, FILE, off_t, feof, ferror, fopen, fread, fseeko, ftello, perror, printf, putchar, sprintf, vfprintf */
#include <stdint.h> /* uint8_t, uint16_t, uint32_t */
#include <stdarg.h> /* va_list, va_end, va_start */
#include <string.h> /* memset */
#include <math.h> /* round */

#include <sys/stat.h> /* mkdir */

#include <error.h> /* EEXIST */
#include <errno.h> /* errno */
#include <assert.h> /* assert */

#include "png.h" /* png_*, setjmp */

#define FILENAME "pokegra.narc"
#define OUTDIR "test"

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;

typedef u32 magic_t;


#define fseeko fseek
#define ftello ftell
#define off_t int

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

	FILE *fp;

	struct FATB fatb;

	/* pointer to the FATB, if loaded */
	struct FNTB fntb;

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

	u8 *data;
};

struct NCGR {
	struct standard_header header;

	struct CHAR char_;

	//struct CPOS cpos;
};


/* NCLR */

struct NCLR {
	struct standard_header header;

	struct PLTT *palettes;
};

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

	u8 *data;
};

static void
warn(const char *s, ...)
{
	va_list va;
	va_start(va, s);
	vfprintf(stderr, s, va);
	va_end(va);
	fprintf(stderr, "\n");
}

static void
pmagic(magic_t magic)
{
	for (int i = 0; i < 4; i++) {
		putchar((magic >> ((3 - i) * 8)) & 0xff);
	}
	putchar('\n');
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

static char magic_buf[5];
#define STRMAGIC(magic) (strmagic((magic), magic_buf))

static int
ncgr_read(void *buf, FILE *fp)
{
	struct NCGR *self = buf;
	fread(&self->header, sizeof(self->header), 1, fp);

	assert(self->header.chunk_count == 1 || self->header.chunk_count == 2);

	fread(&self->char_.header, sizeof(self->char_.header), 1, fp);
	if (ferror(fp) || feof(fp)) {
		return 1;
	}
	assert(self->char_.header.magic == (magic_t)'CHAR');

	self->char_.data = malloc(self->char_.header.data_size);
	if (self->char_.data == NULL) {
		return 1;
	}

	fread(self->char_.data, 1, self->char_.header.data_size, fp);

	/* XXX */

	return ferror(fp) || feof(fp);
}

static int
nclr_read(void *buf, FILE *fp)
{
	struct NCLR *self = buf;
	fread(&self->header, sizeof(self->header), 1, fp);

	self->palettes = calloc(self->header.chunk_count, sizeof(*self->palettes));
	if (self->palettes == NULL) {
		return 1;
	}

	for (int i = 0; i < self->header.chunk_count; i++) {
		struct PLTT *chunk = &self->palettes[i];
		fread(&chunk->header, sizeof(chunk->header), 1, fp);
		if (ferror(fp) || feof(fp)) {
			goto error;
		}
		assert(chunk->header.magic == (magic_t)'PLTT');

		chunk->data = calloc(chunk->header.data_size, 1);
		if (chunk->data == NULL) {
			goto error;
		}
		fread(chunk->data, chunk->header.data_size, 1, fp);
	}

	if (ferror(fp) || feof(fp)) {
		goto error;
	}
	return 0;

	error:
	for (int i = 0; i < self->header.chunk_count; i++) {
		free(self->palettes[i].data);
	}
	free(self->palettes);

	return 1;
}

static const struct format_info {
	magic_t magic;

	size_t size;

	int (*init)(void *);
	int (*read)(void *, FILE *);
} formats[] = {
	{'NCGR', sizeof(struct NCGR), NULL, ncgr_read},
	{'NCLR', sizeof(struct NCLR), NULL, nclr_read},

	/* known but unsupported formats */
	{'NCER'},
	{'NMAR'},
	{'NMCR'},
	{'NANR'},
	{0},
};


static void
narc_init(struct NARC *narc)
{
	memset(narc, 0, sizeof(*narc));
}

static int
narc_load(struct NARC *narc, const char *filename)
{
	assert(narc != NULL);
	assert(filename != NULL);

	FILE *fp = fopen(filename, "rb");
	if (fp == NULL) {
		return 1;
	}

	narc->fp = fp;
	fread(&narc->header, sizeof(narc->header), 1, fp);

	/* 'NARC' is big-endian for some reason */
	if (narc->header.magic != (magic_t)'CRAN') {
		warn("Not a NARC");
		return 1;
	}

	/* read the FATB chunk */
	fread(&narc->fatb.header, sizeof(narc->fatb.header), 1, fp);
	assert(narc->fatb.header.magic == (magic_t)'FATB');
	if (ferror(fp) || feof(fp)) {
		return 1;
	}

	narc->fatb.records = calloc(narc->fatb.header.file_count,
	                            sizeof(*narc->fatb.records));

	if (narc->fatb.records == NULL) {
		return 1;
	}

	fread(narc->fatb.records,
	      sizeof(*narc->fatb.records),
	      narc->fatb.header.file_count,
	      fp);

	if (ferror(fp) || feof(fp)) {
		goto error;
	}


	/* skip the FNTB chunk */
	fread(&narc->fntb.header, sizeof(narc->fntb.header), 1, fp);
	assert(narc->fntb.header.magic == (magic_t)'FNTB');
	if (ferror(fp) || feof(fp)) {
		goto error;
	}
	fseeko(narc->fp, (off_t)(narc->fntb.header.size - sizeof(narc->fntb.header)), SEEK_CUR);


	/* set the data offset */
	struct { magic_t magic; u32 size } fimg_header;

	fread(&fimg_header, sizeof(fimg_header), 1, fp);
	assert(fimg_header.magic == (magic_t)'FIMG');
	if (ferror(fp) || feof(fp)) {
		goto error;
	}

	narc->data_offset = ftello(fp);

	return 0;

	error:
	free(narc->fatb.records);
	return 1;
}


static void *
narc_load_file(struct NARC *narc, int index)
{
	assert(narc != NULL);
	assert(narc->fp != NULL);

	void *chunk;

	assert(0 <= index && index < narc->fatb.header.file_count);

	struct fatb_record record = narc->fatb.records[index];

	assert(record.start <= record.end);
	off_t chunk_size = record.end - record.start;

	if (chunk_size <= 4) {
		return NULL;
	}

	fseeko(narc->fp, narc->data_offset + record.start, SEEK_SET);

	magic_t magic;

	fread(&magic, 1, sizeof(magic), narc->fp);

	fseeko(narc->fp, -(signed)(sizeof(magic)), SEEK_CUR);

	const struct format_info *fmt = formats;

	while (fmt->magic != 0) {
		if (fmt->magic == magic) {
			break;
		}
		fmt++;
	}

	if (fmt->magic == 0) {
		warn("Unknown format: %s", STRMAGIC(magic));
		return NULL;
	}

	if (fmt->size == 0) {
		warn("Unsupported format: %s", STRMAGIC(magic));
		chunk = calloc(1, sizeof(struct standard_header));
	} else {
		chunk = calloc(1, fmt->size);
	}

	if (chunk == NULL) {
		return NULL;
	}

	if (fmt->init != NULL) {
		if (fmt->init(chunk)) {
			goto error;
		}
	}

	if (fmt->read != NULL) {
		if (fmt->read(chunk, narc->fp)) {
			goto error;
		}
	} else {
		fread(chunk, sizeof(struct standard_header), 1, narc->fp);
	}

	if (ferror(narc->fp) || feof(narc->fp)) {
		goto error;
	}

	return chunk;

	error:
	free(chunk);
	return NULL;
}


static int
list(void)
{
	struct NARC narc;
	struct standard_header *chunk;

	narc_init(&narc);
	if (narc_load(&narc, FILENAME)) {
		if (errno) perror(NULL);
		exit(EXIT_FAILURE);
	}

	for (int i = 0; i < narc.fatb.header.file_count; i++) {
		chunk = narc_load_file(&narc, i);
		if (chunk != NULL) {
			pmagic((chunk)->magic);
		} else {
			printf("(null)\n");
		}
	}

	exit(EXIT_SUCCESS);
}


#define MULT 0x41c64e6dL
#define ADD 0x6073L

static void
unscramble_dp(u16 *data, int size)
{
	u16 seed = data[size - 1];
	for (int i = size - 1; i >= 0; i--) {
		data[i] ^= seed;
		seed = seed * MULT + ADD;
	}
}

static void
unscramble_pt(u16 *data, int size)
{
	u16 seed = data[0];
	for (int i = 0; i < size; i++) {
		data[i] ^= seed;
		seed = seed * MULT + ADD;
	}
}


struct rgba {
	u8 r;
	u8 g;
	u8 b;
	u8 a;
};

static struct rgba *
nclr_get_colors(struct NCLR *self, int index)
{
	assert(self != NULL);
	assert(self->palettes != NULL);

	if (!(0 <= index && index < self->header.chunk_count)) {
		return NULL;
	}

	struct PLTT *palette = &self->palettes[index];
	struct rgba *colors = NULL;

	int size = palette->header.color_count;
	if (palette->header.bit_depth == 4) {
		// 8 bpp
		size = 256;
	} else if (size > 256) {
		size -= 256;
	}

	colors = calloc(size, sizeof(struct rgba));
	if (colors == NULL) {
		return NULL;
	}

	/* unpack the colors */

	u16 *colors16 = (u16 *)palette->data;
	for (int i = 0; i < size; i++) {
		colors[i].r = colors16[i] & 0x1f;
		colors[i].g = (colors16[i] >> 5) & 0x1f;
		colors[i].b = (colors16[i] >> 10) & 0x1f;

		colors[i].a = (i == 0) ? 31 : 0;
	}

	return colors;
}

static u8 *
ncgr_get_pixels(struct NCGR *self, int *height_out, int *width_out)
{
	assert(self != NULL);
	assert(self->char_.data != NULL);

	assert(self->char_.header.width != 0xffff);

	int width, height, size;
	width = self->char_.header.width * 8;
	height = self->char_.header.height * 8;
	size = width * height;

	u8 *pixels = malloc(size);

	if (pixels == NULL) {
		return NULL;
	}


	/* unpack the pixels */

	int i;
	switch(self->char_.header.bit_depth) {
	case 3:
		// 4 bits per pixel
		assert(self->char_.header.data_size * 2 == size);
		for (i = 0; i < self->char_.header.data_size; i++) {
			u8 byte = self->char_.data[i];
			pixels[i*2] = byte & 0x0f;
			pixels[i*2 + 1] = (byte >> 4) & 0x0f;
		}
		break;
	case 4:
		// 8 bits per pixel
		assert(self->char_.header.data_size == size);
		for (i = 0; i < self->char_.header.data_size; i++) {
			pixels[i] = self->char_.data[i];
		}
		break;
	default:
		warn("Unknown bit depth: %d", self->char_.header.bit_depth);
		return NULL;
	}
	assert(self->char_.header.bit_depth == 3);

	/* untile the image, if necessary */

	if (self->char_.header.tiled == 0) {
		u8 tmp_px[height][width];

		int x, y, tx, ty, cx, cy, i;
		i = 0;
		for (y = 0; y < height / 8; y++) {
		for (x = 0; x < width / 8; x++) {
			for (ty = 0; ty < 8; ty++) {
			for (tx = 0; tx < 8; tx++) {
				cy = y * 8 + ty;
				cx = x * 8 + tx;
				tmp_px[cy][cx] = pixels[i];
				i++;
			}
			}
		}
		}

		memcpy(pixels, tmp_px, size);
	}

	*height_out = height;
	*width_out = width;
	return pixels;
}

static int
write_pam(u8 *pixels, struct rgba *colors, int height, int width, FILE *fp)
{
	if (pixels == NULL || colors == NULL) {
		return 1;
	}

	int size = width * height;

	fprintf(fp, "P7\n");
	fprintf(fp, "WIDTH %d\n", width);
	fprintf(fp, "HEIGHT %d\n", height);
	fprintf(fp, "DEPTH 4\n");
	fprintf(fp, "TUPLTYPE RGB_ALPHA\n");
	fprintf(fp, "MAXVAL 31\n"); // XXX
	fprintf(fp, "ENDHDR\n");

	for (int i = 0; i < size; i++) {
		/* XXX bounds checks */
		struct rgba color = colors[pixels[i]];
		fwrite(&color, 1, sizeof(color), fp);
	}

	if (ferror(fp)) {
		return 1;
	}
	return 0;
}

static int
ncgr_to_pam(struct NCGR *sprite, struct NCLR *palette, int palette_index, FILE *fp)
{
	assert(sprite != NULL);
	assert(palette != NULL);

	int height, width;
	u8 *pixels = ncgr_get_pixels(sprite, &height, &width);
	struct rgba *colors = nclr_get_colors(palette, palette_index);

	int status = 0;

	if (pixels != NULL && colors != NULL) {
		status = write_pam(pixels, colors, height, width, fp);
	} else {
		status = 1;
	}

	free(pixels);
	free(colors);
	return status;
}


static int
write_png(u8 *pixels, struct rgba *colors, int height, int width, FILE *fp)
{
	const int color_count = 16; /* XXX */
	const int bit_depth = 5; /* XXX */

	png_bytepp row_pointers = calloc(height, sizeof(*row_pointers));
	png_colorp palette = calloc(color_count, sizeof(*palette));
	png_color_8 sig_bit;
	png_byte trans[1] = {0};

	if (row_pointers == NULL || palette == NULL) {
		free(row_pointers);
		free(palette);
		return 1;
	}

	/* expand the palette */

	double factor = 255.0 / (double)((1 << bit_depth) - 1);
	for (int i = 0; i < color_count; i++) {
		palette[i].red = (int)round(colors[i].r * factor);
		palette[i].green = (int)round(colors[i].g * factor);
		palette[i].blue = (int)round(colors[i].b * factor);
	}

	/* set the row pointers */

	for (int i = 0; i < height; i++) {
		row_pointers[i] = &pixels[i * width];
	}

	/* set the significant bits */

	sig_bit.red = bit_depth;
	sig_bit.green = bit_depth;
	sig_bit.blue = bit_depth;


	png_structp png = png_create_write_struct(
		PNG_LIBPNG_VER_STRING, NULL,  NULL, NULL);
	if (!png) {
		return 1;
	}

	png_infop info = png_create_info_struct(png);
	if (!info) {
		png_destroy_write_struct(&png, (png_infopp)NULL);
		return 1;
	}

	if (setjmp(png_jmpbuf(png))) {
		png_destroy_write_struct(&png, &info);
		return 1;
	}

	png_init_io(png, fp);

	png_set_IHDR(png, info, width, height,
		4, /* bit depth */
		PNG_COLOR_TYPE_PALETTE,
		PNG_INTERLACE_NONE,
		PNG_COMPRESSION_TYPE_DEFAULT,
		PNG_FILTER_TYPE_DEFAULT
	);

	png_set_PLTE(png, info, palette, color_count);
	png_set_tRNS(png, info, trans, 1, NULL);
	png_set_sBIT(png, info, &sig_bit);

	png_set_rows(png, info, row_pointers);

	png_write_png(png, info, PNG_TRANSFORM_PACKING, NULL);

	png_destroy_write_struct(&png, &info);
	free(row_pointers);
	free(palette);
	return 0;
}


static int
ncgr_to_png(struct NCGR *sprite, struct NCLR *palette, int palette_index, FILE *fp)
{
	assert(sprite != NULL);
	assert(palette != NULL);

	int height, width;
	u8 *pixels = ncgr_get_pixels(sprite, &height, &width);
	struct rgba *colors = nclr_get_colors(palette, palette_index);

	int status = 0;

	if (pixels != NULL && colors != NULL) {
		status = write_png(pixels, colors, height, width, fp);
	} else {
		status = 1;
	}

	free(pixels);
	free(colors);
	return status;
}


static void
write_sprite(u8 *pixels, int height, int width, struct NCLR *palette, char *outfile)
{
	FILE *outfp = fopen(outfile, "wb");
	if (outfp != NULL) {
		struct rgba *colors = nclr_get_colors(palette, 0);

		if (colors != NULL) {
			if (write_png(pixels, colors, height, width, outfp)) {
				warn("Error writing %s.", outfile);
			}
			free(colors);
		} else {
			warn("Error ripping %s.", outfile);
		}
		fclose(outfp);
	} else {
		perror(NULL);
	}
}

int
main(int argc, char *argv[])
{
	struct NARC narc;

	narc_init(&narc);
	if (narc_load(&narc, FILENAME)) {
		if (errno) perror(NULL);
		exit(EXIT_FAILURE);
	}

	char outfile[256] = "";

	const struct sprite_dirs {
		const char *normal;
		const char *shiny;
	} const dirs[] = {
		{"back/female", "back/shiny/female"},
		{"back", "back/shiny"},
		{"female", "shiny/female"},
		{"", "shiny"},
		{NULL},
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

	for (int n = 1; n <= 493; n++) {
		struct NCLR *normal_palette = narc_load_file(&narc, n*6 + 4);
		struct NCLR *shiny_palette = narc_load_file(&narc, n*6 + 5);

		if (normal_palette == NULL || shiny_palette == NULL) {
			if (errno) perror(NULL);
			exit(EXIT_FAILURE);
		}

		assert(normal_palette->header.magic == (magic_t)'NCLR');
		assert(shiny_palette->header.magic == (magic_t)'NCLR');

		for (int i = 0; i < 4; i++) {
			struct NCGR *sprite = narc_load_file(&narc, n*6 + i);
			if (sprite == NULL) {
				// this is fine
				continue;
			}

			assert(sprite->header.magic == (magic_t)'NCGR');
			unscramble_dp((u16 *)sprite->char_.data,
				      sprite->char_.header.data_size/sizeof(u16));

			int height, width;
			u8 *pixels = ncgr_get_pixels(sprite, &height, &width);
			if (pixels == NULL) {
				warn("Error ripping %s.", outfile);
				continue;
			}

			const struct sprite_dirs *d = &dirs[i];

			sprintf(outfile, "%s/%s/%d.png", OUTDIR, d->normal, n);
			write_sprite(pixels, height, width, normal_palette, outfile);

			sprintf(outfile, "%s/%s/%d.png", OUTDIR, d->shiny, n);
			write_sprite(pixels, height, width, shiny_palette, outfile);

			free(pixels);
		}
	}

	printf("done\n");
	exit(EXIT_SUCCESS);
}
