/* nitro.c - Nitro object framework
 *
 * Copyright © 2011 magical
 *
 * This file is part of spriterip; it is licensed under the GNU GPLv3
 * and comes with NO WARRANTY. See rip.c for details.
 */

#include "nitro.h"

#include <stdlib.h> /* NULL, malloc */
#include <stdio.h> /* FILE, SEEK_CUR, feof, ferror, fread, fseeko */
#include <string.h> /* memcpy, memset */

#include <sys/types.h> /* off_t */

#include "common.h" /* OKAY, FAIL, ABORT, NOMEM, FREAD, assert, struct dim */
#include "lzss.h" /* lzss_decompress_buffer */

#define D(w,h) {.height=h, .width=w}
// obj_sizes [size][shape]
struct dim obj_sizes[4][4] = {
	{D(8,8),   D(16,8),  D(8,16),  D(0,0)},
	{D(16,16), D(32,8),  D(8,32),  D(0,0)},
	{D(32,32), D(32,16), D(16,32), D(0,0)},
	{D(64,64), D(64,32), D(32,64), D(0,0)},
};
#undef D

const struct nitro_type * const nitro_types[] = {
	&NARC_type,

	/* lesser formats */
	&NCGR_type,
	&NCLR_type,
	&NCER_type,
	&NANR_type,
	&NMCR_type,
	&NMAR_type,

	NULL
};

/* Find the nitro_type associated with some magic number */
const struct nitro_type *
nitro_lookup(magic_t magic)
{
	const struct nitro_type * const *type = nitro_types;

	while (*type != NULL) {
		if ((*type)->magic == magic) {
			return *type;
		}
		type++;
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
	const struct nitro_type *type = nitro_lookup(magic);

	if (type == NULL) {
		warn("Unknown format: %08x", magic);
		return NULL;
	}

	assert(type->size > 0);
	chunk = malloc(type->size);
	if (chunk == NULL) {
		return NULL;
	}

	// Initialize the structure. This isn't _really_ valid, since the
	// representation of NULL and 0.0 might not be all zero bits,
	// but... on x86 that _is_ how they are represented, and the rest of
	// the code isn't quite portable anway...
	// The proper way would be for each type to implement an .init method
	// which manually initializes each field. Not difficult, just tedious.
	memset(chunk, 0, type->size);

	if (type->read != NULL) {
		switch (type->read(chunk, fp)) {
		case OKAY:
			break;
		case ABORT:
			goto error;
		case FAIL:
		case NOMEM:
		default:
			if (type->free != NULL) {
				type->free(chunk);
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
		const struct nitro_type *type = nitro_lookup(header->magic);

		if (type != NULL && type->free != NULL) {
			type->free(chunk);
		}
	}
}

