/* nclr.c - NCLR ("color resource"; palette) support
 *
 * Copyright © 2011 magical
 *
 * This file is part of spriterip; it is licensed under the GNU GPLv3
 * and comes with NO WARRANTY. See rip.c for details.
 */

#include <stdio.h> /* FILE, ferror, feof */

#include "common.h" /* OKAY, FAIL, NOMEM, struct buffer, struct palette, u8, u16, u32, assert, ALLOC, CALLOC, FREE, FREAD */
#include "nitro.h" /* struct format_info, struct nitro, magic_t, format_header */

#include "nclr.h"

/* NCLR */

struct PLTT {
	struct {
		magic_t magic;
		u32 size;

		u16 bit_depth;
		u16 unknown;

		u32 padding;

		u32 data_size;
		u32 data_offset;
	} header;

	struct buffer *buffer;
};

struct NCLR {
	struct nitro header;

	struct PLTT pltt;

	//struct PCMP pcmp;
};

static int
nclr_read(void *buf, FILE *fp)
{
	struct NCLR *self = buf;
	FREAD(fp, &self->header, 1);

	FREAD(fp, &self->pltt.header, 1);
	if (ferror(fp) || feof(fp)) {
		return FAIL;
	}

	assert(self->header.magic == (magic_t)'NCLR');
	assert(self->pltt.header.magic == (magic_t)'PLTT');

	//assert(self->pltt.header.bit_depth == 4);
	assert(self->pltt.header.data_offset == 16);

	self->pltt.buffer = buffer_alloc(self->pltt.header.data_size);
	if (self->pltt.buffer == NULL) {
		return NOMEM;
	}

	FREAD(fp, self->pltt.buffer->data, self->pltt.buffer->size);
	if (ferror(fp) || feof(fp)) {
		return FAIL;
	}

	return OKAY;
}

static void
nclr_free(void *buf) {
	struct NCLR *self = buf;

	if (self != NULL &&
	    self->header.magic == (magic_t)'NCLR') {
		FREE(self->pltt.buffer);
	}
}

struct format_info NCLR_format = {
	format_header('NCLR', struct NCLR),

	.read = nclr_read,
	.free = nclr_free,
};

struct palette *
nclr_get_palette(struct NCLR *self, int index)
{
	assert(self != NULL);
	assert(index == 0);

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

	assert(pltt->buffer->size >= sizeof(u16) * count);

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

