/* nitro.c - Nitro object framework
 *
 * Copyright © 2011 magical
 *
 * This file is part of spriterip; it is licensed under the GNU GPLv3
 * and comes with NO WARRANTY. See rip.c for details.
 */

#include <stdlib.h> /* NULL, malloc */
#include <stdio.h> /* FILE, SEEK_CUR, feof, ferror, fread, fseeko */
#include <string.h> /* memcpy, memset */

#include <sys/types.h> /* off_t */

#include "common.h" /* OKAY, FAIL, ABORT, NOMEM, FREAD, assert, struct dim */
#include "lzss.h" /* lzss_decompress_buffer */
#include "nitro.h"

#include "narc.h" /* NARC_format */
#include "ncgr.h" /* NCGR_format */
#include "nclr.h" /* NCLR_format */
#include "ncer.h" /* NCER_format */
#include "nanr.h" /* NANR_format */
#include "nmcr.h" /* NMCR_format */
#include "nmar.h" /* NMAR_format */

#define D(w,h) {.height=h, .width=w}
// obj_sizes [size][shape]
struct dim obj_sizes[4][4] = {
	{D(8,8),   D(16,8),  D(8,16),  D(0,0)},
	{D(16,16), D(32,8),  D(8,32),  D(0,0)},
	{D(32,32), D(32,16), D(16,32), D(0,0)},
	{D(64,64), D(64,32), D(32,64), D(0,0)},
};
#undef D

const struct format_info * const formats[] = {
	&NARC_format,

	/* lesser formats */
	&NCGR_format,
	&NCLR_format,
	&NCER_format,
	&NANR_format,
	&NMCR_format,
	&NMAR_format,

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

static void *
nitro_read_nocompressed(FILE *fp, magic_t magic)
{
	void *chunk;
	const struct format_info *fmt = format_lookup(magic);

	if (fmt == NULL) {
		warn("Unknown format: %08x", magic);
		return NULL;
	} else if (fmt->size == 0) {
		char magic_buf[MAGIC_BUF_SIZE];
		warn("Unsupported format: %s", strmagic(magic, magic_buf));
		chunk = malloc(sizeof(struct nitro));
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
		memset(chunk, 0, sizeof(struct nitro));
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
		fread(chunk, sizeof(struct nitro), 1, fp);
	}

	if (ferror(fp) || feof(fp)) {
		goto error;
	}

	return chunk;

	error:
	FREE(chunk);

	return NULL;
}

static void *
nitro_read_compressed(struct buffer *buffer)
{
	struct buffer *decompressed = lzss_decompress_buffer(buffer);
	FREE(buffer);
	if (decompressed == NULL) {
		return NULL;
	}

	buffer = decompressed;

	FILE *fp = fmemopen(buffer->data, buffer->size, "rb");
	if (fp == NULL) {
		return NULL;
	}
	magic_t magic = *(magic_t *)buffer->data;
	// no recursing for us!
	void *chunk = nitro_read_nocompressed(fp, magic);
	fclose(fp);
	FREE(buffer);
	return chunk;
}

/* XXX get rid of the size parameter somehow */
void *
nitro_read(FILE *fp, off_t size)
{
	assert(fp != NULL);

	magic_t magic;
	FREAD(fp, &magic, 1);

	// back to the beginning
	if (fseeko(fp, -(sizeof(magic_t)), SEEK_CUR)) {
		return NULL;
	};

	int b = magic & 0xff;
	if (b == 0x10 || b == 0x11) {
		//probably compressed data
		assert(size > 0);
		struct buffer *buffer = buffer_alloc(size);
		if (buffer == NULL) {
			return NULL;
		}
		if (fread(buffer->data, 1, size, fp) != size) {
			return NULL;
		}
		if (lzss_check(buffer)) {
			// nitro_read_compressed will free the buffer
			return nitro_read_compressed(buffer);
		} else {
			// no idea what the file is -- bail
			FREE(buffer);
			return NULL;
		}
	}

	return nitro_read_nocompressed(fp, magic);
}


void
nitro_free(void *chunk)
{
	struct nitro *header = chunk;
	if (chunk != NULL) {
		const struct format_info *fmt = format_lookup(header->magic);

		if (fmt != NULL && fmt->free != NULL) {
			fmt->free(chunk);
		}
	}
}

