
#include <stdlib.h> /* NULL, malloc */
#include <stdio.h> /* FILE, SEEK_CUR, feof, ferror, fread, fseeko */

#include <string.h> /* memcpy, memset */

#include "common.h" /* OKAY, FAIL, ABORT, NOMEM, FREAD, assert, struct dim */
#include "nitro.h"

#include "narc.h" /* NARC_format */
#include "ncgr.h" /* NCGR_format */
#include "nclr.h" /* NCLR_format */
#include "ncer.h" /* NCER_format */

#define D(h,w) {.height=h, .width=w}
// obj_sizes [size][shape]
struct dim obj_sizes[4][4] = {
	{D(8,8),   D(8,16),  D(16,8),  D(0,0)},
	{D(16,16), D(8,32),  D(32,8),  D(0,0)},
	{D(32,32), D(16,32), D(32,16), D(0,0)},
	{D(64,64), D(32,64), D(64,32), D(0,0)},
};
#undef D

const struct format_info * const formats[] = {
	&NARC_format,

	/* lesser formats */
	&NCGR_format,
	&NCLR_format,
	&NCER_format,

	/* known but unsupported formats */
	#define UNSUPPORTED(m) &(struct format_info){.magic = (magic_t)m}
	UNSUPPORTED('NANR'),
	UNSUPPORTED('NMAR'),
	UNSUPPORTED('NMCR'),
	#undef UNSUPPORTED

	#undef F
	NULL
};

const struct format_info *
format_lookup(magic_t magic)
{
	const struct format_info * const *fmt = formats;

	while (*fmt != NULL) {
		if ((*fmt)->magic == magic) {
			return *fmt;
		}
		fmt++;
	}

	return NULL;
}

char *
strmagic(magic_t magic, char *buf)
{
	for (int i = 0; i < 4; i++) {
		buf[i] = (char)((magic >> ((3 - i) * 8)) & 0xff);
	}
	buf[4] = '\0';
	return buf;
}

void *
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
		char magic_buf[MAGIC_BUF_SIZE];
		warn("Unsupported format: %s", strmagic(magic, magic_buf));
		chunk = malloc(sizeof(struct standard_header));
	} else {
		chunk = malloc(fmt->size);
	}

	if (chunk == NULL) {
		return NULL;
	}

	/* time to actually load it */

	// simply initializing the structure to all 0s would break on
	// architectures where the null pointer != 0.
	if (fmt->initializer != NULL) {
		memcpy(chunk, fmt->initializer, fmt->size);
	} else if (fmt->size > 0) {
		memset(chunk, 0, fmt->size);
	} else {
		memset(chunk, 0, sizeof(struct standard_header));
	}

	if (fmt->init != NULL) {
		if (fmt->init(chunk)) {
			goto error;
		}
	} else {
		/* it's already zeroed; there's nothing more to do */
	}

	if (fmt->read != NULL) {
		switch (fmt->read(chunk, fp)) {
		case OKAY:
			break;
		case ABORT:
			goto error;
		case FAIL:
		case NOMEM:
		default:
			if (fmt->free != NULL) {
				fmt->free(chunk);
			}
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

