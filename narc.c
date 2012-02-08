/* narc.c - NARC (archive) support
 *
 * Copyright © 2011 magical
 *
 * This file is part of spriterip; it is licensed under the GNU GPLv3
 * and comes with NO WARRANTY. See rip.c for details.
 */

#include <stdlib.h> /* NULL */
#include <stdio.h> /* FILE, SEEK_CUR, SEEK_SET, off_t, feof, ferror, fseeko, ftello */

#include "nitro.h" /* struct format_info, struct nitro, magic_t, format_header, nitro_read */
#include "common.h" /* OKAY, FAIL, NOMEM, assert, FREAD, CALLOC, FREE, u32 */

#include "narc.h"

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
	struct nitro header;

	struct FATB fatb;

	struct FNTB fntb;

	FILE *fp;

	/* offset to the beginning of the FIMG data */
	off_t data_offset;
};

static int
narc_read(void *buf, FILE *fp)
{
	struct NARC *self = buf;
	assert(self != NULL);

	/* For efficiency, we stash the file pointer so we can lazily load
	 * the contained files - although i'm not sure this is the best
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
		return FAIL;
	}


	/* skip the FNTB chunk */
	FREAD(fp, &self->fntb.header, 1);
	assert(self->fntb.header.magic == (magic_t)'FNTB');
	if (ferror(fp) || feof(fp)) {
		return FAIL;
	}
	fseeko(self->fp, (off_t)(self->fntb.header.size - sizeof(self->fntb.header)), SEEK_CUR);

	/* set the data offset */
	struct { magic_t magic; u32 size; } fimg_header;

	FREAD(fp, &fimg_header, 1);
	assert(fimg_header.magic == (magic_t)'FIMG');
	if (ferror(fp) || feof(fp)) {
		return FAIL;
	}

	self->data_offset = ftello(fp);
	if (self->data_offset == -1) {
		return FAIL;
	}

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

u32
narc_get_file_count(struct NARC *self)
{
	assert(self != NULL);
	return self->fatb.header.file_count;
}

u32
narc_get_file_size(struct NARC *self, int index)
{
	assert(self != NULL);
	assert(self->fp != NULL);

	assert(0 <= index && index < (signed long)self->fatb.header.file_count);

	struct fatb_record record = self->fatb.records[index];

	assert(record.start <= record.end);
	u32 chunk_size = record.end - record.start;

	return chunk_size;
}

void *
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

	return nitro_read(self->fp, chunk_size);
}

/* the NARC signature is big-endian for some reason */
struct format_info NARC_format = {
	format_header('CRAN', struct NARC),
	
	.read = narc_read,
	.free = narc_free,
};
