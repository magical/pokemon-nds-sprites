
#include <stdlib.h> /* size_t, calloc, free */
#include <stdio.h> /* SEEK_CUR, SEEK_SET, FILE, off_t, ferror, fopen, fread, fseeko, ftello, perror, putchar, vfprintf */
#include <stdint.h> /* uint8_t, uint16_t, uint32_t */
#include <stdarg.h> /* va_list, va_end, va_start */
#include <string.h> /* memset */
#include <errno.h> /* errno */

#include <assert.h> /* assert */

#define FILENAME "pokegra.narc"

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;

typedef u32 magic_t;

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

void warn(const char *s, ...)
{
	va_list va;
	va_start(va, s);
	vfprintf(stderr, s, va);
	va_end(va);
	fprintf(stderr, "\n");
}

void pmagic(magic_t magic)
{
	for (int i = 0; i < 4; i++) {
		putchar((magic >> ((3 - i) * 8)) & 0xff);
	}
	putchar('\n');
}

char *strmagic(magic_t magic, char *buf)
{
	for (int i = 0; i < 4; i++) {
		buf[i] = (char)((magic >> ((3 - i) * 8)) & 0xff);
	}
	buf[4] = '\0';
	return buf;
}

static char magic_buf[5];

#define STRMAGIC(magic) (strmagic((magic), magic_buf))

int ncgr_read(void *buf, FILE *fp)
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

int nclr_read(void *buf, FILE *fp)
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

const struct format_info {
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


void narc_init(struct NARC *narc)
{
	memset(narc, 0, sizeof(*narc));
}

int narc_load(struct NARC *narc, const char *filename)
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


void *narc_load_file(struct NARC *narc, int index)
{
	assert(narc != NULL);
	assert(narc->fp != NULL);

	void *chunk;

	struct standard_header header;

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


int list(void)
{
	struct NARC narc;
	void *chunk;

	narc_init(&narc);
	if (narc_load(&narc, FILENAME)) {
		if (errno) perror(NULL);
		exit(EXIT_FAILURE);
	}

	for (int i = 0; i < narc.fatb.header.file_count; i++) {
		void *chunk = narc_load_file(&narc, i);
		if (chunk != NULL) {
			pmagic(((struct standard_header *)chunk)->magic);
		} else {
			printf("(null)\n");
		}
	}

	exit(EXIT_SUCCESS);
}


#define MULT 0x41c64e6dL
#define ADD 0x6073L

void unscramble_dp(u16 *data, int size)
{
	u16 seed = data[size - 1];
	for (int i = size - 1; i >= 0; i--) {
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

int ncgr_to_pam(struct NCGR *sprite, struct NCLR *palette)
{
	if (sprite->char_.header.width == 0xffff) {
		return 1;
	}

	int width, height, size;
	width = sprite->char_.header.width * 8;
	height = sprite->char_.header.height * 8;
	size = width * height;

	assert(palette->palettes != NULL);
	struct PLTT *pltt = &palette->palettes[0];

	int palette_size = pltt->header.color_count;
	if (pltt->header.bit_depth == 4) {
		// 8 bpp
		palette_size = 256;
	} else if (palette_size > 256) {
		palette_size -= 256;
	}

	u8 *pixels = NULL;
	struct rgba *colors = NULL;

	pixels = malloc(size);
	colors = calloc(palette_size, sizeof(struct rgba));

	if (pixels == NULL || colors == NULL) {
		free(colors);
		free(pixels);
		return 1;
	}

	int i;

	/* unpack the pixels */

	switch(sprite->char_.header.bit_depth) {
	case 3:
		// 4 bits per pixel
		assert(sprite->char_.header.data_size * 2 == size);
		for (i = 0; i < sprite->char_.header.data_size; i++) {
			u8 byte = sprite->char_.data[i];
			pixels[i*2] = byte & 0x0f;
			pixels[i*2 + 1] = (byte >> 4) & 0x0f;
		}
		break;
	case 4:
		// 8 bits per pixel
		assert(sprite->char_.header.data_size == size);
		for (i = 0; i < sprite->char_.header.data_size; i++) {
			pixels[i] = sprite->char_.data[i];
		}
		break;
	default:
		warn("Unknown bit depth: %d", sprite->char_.header.bit_depth);
		return 1;
	}
	assert(sprite->char_.header.bit_depth == 3);

	/* untile the image, if necessary */

	if (sprite->char_.header.tiled == 0) {
		u8 tmp_px[height][width];

		const int ht = height / 8;
		const int wt = width / 8;
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

	/* unpack the colors */

	u16 *colors16 = (u16 *)pltt->data;
	for (i = 0; i < palette_size; i++) {
		colors[i].r = colors16[i] & 0x1f;
		colors[i].g = (colors16[i] >> 5) & 0x1f;
		colors[i].b = (colors16[i] >> 10) & 0x1f;

		colors[i].a = (i == 0) ? 31 : 0;
	}

	/* write out the PAM */

	printf("P7\n");
	printf("WIDTH %d\n", width);
	printf("HEIGHT %d\n", height);
	printf("DEPTH 4\n");
	printf("TUPLTYPE RGB_ALPHA\n");
	printf("MAXVAL 31\n"); // XXX
	printf("ENDHDR\n");

	for (i = 0; i < size; i++) {
		struct rgba color = colors[pixels[i]];
		fwrite(&color, 1, sizeof(color), stdout);
	}

	return 0;
}


int main(int argc, char *argv[])
{
	struct NARC narc;
	void *chunk;

	narc_init(&narc);
	if (narc_load(&narc, FILENAME)) {
		if (errno) perror(NULL);
		exit(EXIT_FAILURE);
	}

	struct NCGR *sprite = narc_load_file(&narc, 9);
	struct NCLR *palette = narc_load_file(&narc, 10);
	if (sprite == NULL || palette == NULL) {
		if (errno) perror(NULL);
		exit(EXIT_FAILURE);
	}

	assert(sprite->header.magic == (magic_t)'NCGR');
	assert(palette->header.magic == (magic_t)'NCLR');

	unscramble_dp((u16 *)sprite->char_.data, sprite->char_.header.data_size/sizeof(u16));

	(void)ncgr_to_pam(sprite, palette);

	puts("done");
	exit(EXIT_SUCCESS);
}
