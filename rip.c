
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

struct NCGR {
	struct standard_header header;
	u8 *data;
	void *chunks;
};

struct CHAR {
	struct {
		magic_t magic;
		u32 size;

		u16 width;
		u16 height;

		u32 bit_depth;
		u32 padding;
		u32 tiled;

		u32 data_size;
		u32 unknown;
	} header;

	u8 *data;
};


/* NCLR */
struct NCLR {
	struct standard_header header;
	u8 *data;
	void *chunks;
};

struct PLTT {
	struct {
		magic_t magic;
		u32 size;

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

int ncgr_read(void *buf, FILE *fp)
{
	struct NCGR *chunk = buf;
	fread(&chunk->header, sizeof(chunk->header), 1, fp);

	/* XXX */

	return ferror(fp) || feof(fp);
}

int nclr_read(void *buf, FILE *fp)
{
	struct NCLR *chunk = buf;
	fread(&chunk->header, sizeof(chunk->header), 1, fp);

	/* XXX */

	return ferror(fp) || feof(fp);
}

const struct format_info {
	magic_t magic;

	size_t size;

	int (*init)(void *);
	int (*read)(void *, FILE *);
} formats[] = {
	{'NCGR', sizeof(struct NCGR), NULL, ncgr_read},
	{'NCLR', sizeof(struct NCLR), NULL, nclr_read},

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
		warn("Unknown format: %llc", fmt->magic);
		return NULL;
	}

	chunk = calloc(1, fmt->size);

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


int main(int argc, char *argv[])
{
	struct NARC narc;
	void *chunk;

	narc_init(&narc);
	if (narc_load(&narc, "pokegra.narc")) {
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
