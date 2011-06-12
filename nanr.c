/* nanr - NANR (animation resource) support
 *
 * Copyright © 2011 magical
 *
 * This file is part of spriterip; it is licensed under the GNU GPLv3
 * and comes with NO WARRANTY. See rip.c for details.
 */

#include "nanr.h"

#include <stdio.h> /* FILE */
#include <math.h> /* sin, cos */

#include "nitro.h" /* struct standard_header, struct format_info, magic_t, format_header */
#include "common.h" /* OKAY, FAIL, NOMEM, u8, u16, u32, struct buffer */
#include "ncer.h" /* struct NCER */
#include "ncgr.h" /* struct NCGR */

struct acell {
	u32 frame_count;

	/* 0 = 4 bytes; 1 = 16 bytes; 2 = 8 bytes */
	u16 frame_type;

	u16 unknown[3];

	/* start of frame data, relative to ABNK.frame_data_offset */
	u32 frame_offset;
};

struct frame {
	/* start of transformation data, relative to ABNK.transformation_data_offset */
	u32 data_offset;

	/* the amount of time which the frame lasts (60 fps) */
	u16 frame_duration;

	/* always 0xBEEF */
	u16 padding;
};

struct frame_data_0 {
	u16 cell_index;

	u16 unknown;
};

struct frame_data_1 {
	u16 cell_index;

	s16 theta; // actually 1.1.14 fixed-point

	v16 x_mag;
	v16 x_unknown;
	v16 y_mag;
	v16 y_unknown;

	s16 x;
	s16 y;
};

struct frame_data_2 {
	u16 cell_index;
	u16 padding;

	s16 x;
	s16 y;
};

struct ABNK {
	struct {
		magic_t magic;
		u32 size;

		u16 acell_count;
		u16 frame_count;

		u32 acell_data_offset;
		u32 frame_data_offset;
		u32 frame_data_data_offset;

		u32 padding[2];
	} header;

	struct buffer *data;

	struct acell *acells;
	struct frame *frames;
	u8 *frame_data;
};



struct NANR {
	struct standard_header header;
	struct ABNK abnk;
};

static int
nanr_read(void *buf, FILE *fp)
{
	struct NANR *self = buf;
	assert(self != NULL);

	FREAD(fp, &self->header, 1);
	assert(self->header.magic == NANR_MAGIC);

	FREAD(fp, &self->abnk.header, 1);

	if (ferror(fp) || feof(fp)) {
		return FAIL;
	}

	size_t data_size = self->abnk.header.size - sizeof(self->abnk.header);
	self->abnk.data = buffer_alloc(data_size);
	if (self->abnk.data == NULL) {
		return NOMEM;
	}

	if (fread(self->abnk.data->data, data_size, 1, fp) != 1) {
		return FAIL;
	}

	size_t base = sizeof(self->abnk.header) - 8;
	assert(base == 0x18);

	/* XXX bounds checks - invalid data could walk all over memory */
	self->abnk.acells = (struct acell *)(self->abnk.data->data +
	    (self->abnk.header.acell_data_offset - base));
	self->abnk.frames = (struct frame *)(self->abnk.data->data +
	    (self->abnk.header.frame_data_offset - base));
	self->abnk.frame_data = self->abnk.data->data +
	    (self->abnk.header.frame_data_data_offset - base);

	return OKAY;
}

static void
nanr_free(void *buf)
{
	struct NANR *self = buf;
	if (self != NULL &&
	    self->header.magic == NANR_MAGIC) {
		free(self->abnk.data);
	}
}

struct format_info NANR_format = {
	format_header(NANR_MAGIC, struct NANR),

	.read = nanr_read,
	.free = nanr_free,
};

static int
get_frame_data(struct NANR *self, struct acell *acell, int frame_index,
               int *cell_index, fx16 m[], struct coords *offset)
{
	struct ABNK *abnk = &self->abnk;

	if (!(0 <= frame_index && frame_index < acell->frame_count)) {
		return FAIL;
	}

	struct frame *frames = (struct frame *)((u8 *)abnk->frames + acell->frame_offset);
	struct frame *frame = &frames[frame_index];

	void *data = (abnk->frame_data + frame->data_offset);

	switch (acell->frame_type) {
	case 0: {
		struct frame_data_0 *frame_data = data;
		*cell_index = frame_data->cell_index;
		m[0] = 0x100; m[1] = 0; m[2] = 0; m[3] = 0x100;
		*offset = (struct coords){0, 0};
		}; break;
	case 1: {
		struct frame_data_1 *frame_data = data;
		*cell_index = frame_data->cell_index;

		// floating point operations! :o
		// cheating, i know.
		// if this looks backwards, it's because we're forming the
		// *inverse* matrix.
		double theta = frame_data->theta * 0x1p-14;
		/*warn("\ntheta=%f x=%f y=%f", theta,
		                             frame_data->x_mag / 4096.0,
		                             frame_data->y_mag / 4096.0);*/
		m[0] = (fx16)( cos(theta) * 0x1000 / frame_data->x_mag * 0x100);
		m[1] = (fx16)(+sin(theta) * 0x1000 / frame_data->x_mag * 0x100);
		m[2] = (fx16)(-sin(theta) * 0x1000 / frame_data->y_mag * 0x100);
		m[3] = (fx16)( cos(theta) * 0x1000 / frame_data->y_mag * 0x100);

		/*m[0] = (fx16)(frame_data->x_mag / 4096.0 * 0x100);
		m[1] = (fx16)(0);
		m[2] = (fx16)(0);
		m[3] = (fx16)(frame_data->y_mag / 4096.0 * 0x100);*/
		/*double theta = frame_data->theta / 4096.0 / 8;
		m[0] = (fx16)( cos(theta) * frame_data->x_mag / 16);
		m[1] = (fx16)(+sin(theta) * frame_data->x_mag / 16);
		m[2] = (fx16)(-sin(theta) * frame_data->y_mag / 16);
		m[3] = (fx16)( cos(theta) * frame_data->y_mag / 16);*/

		offset->x = frame_data->x;
		offset->y = frame_data->y;
		}; break;
	case 2: {
		struct frame_data_2 *frame_data = data;
		*cell_index = frame_data->cell_index;
		m[0] = 0x100; m[1] = 0; m[2] = 0; m[3] = 0x100;
		offset->x = frame_data->x;
		offset->y = frame_data->y;
		}; break;
	}

	return OKAY;
}

int
nanr_draw_frame(struct NANR *self, int acell_index, int frame_index,
                struct NCER *ncer, struct NCGR *ncgr,
                struct image *image, struct coords frame_offset)
{
	assert(self != NULL);
	assert(self->header.magic == NANR_MAGIC);
	assert(ncer != NULL);
	assert(ncgr != NULL);
	assert(image != NULL);
	assert(image->pixels != NULL);

	struct ABNK *abnk = &self->abnk;

	if (!(0 <= acell_index && acell_index < abnk->header.acell_count)) {
		return FAIL;
	}

	struct acell *acell = &abnk->acells[acell_index];

	int cell_index;
	fx16 m[4];
	struct coords cell_offset;
	if (get_frame_data(self, acell, frame_index,
	                   &cell_index, m, &cell_offset)) {
		return FAIL;
	}

	cell_offset.x += frame_offset.x;
	cell_offset.y += frame_offset.y;
	return ncer_draw_cell_t(ncer, cell_index, ncgr, image, cell_offset, m);
}

int
nanr_get_cell_count(struct NANR *self)
{
	assert(self != NULL);
	assert(self->header.magic == NANR_MAGIC);

	return self->abnk.header.acell_count;
}

int
nanr_get_frame_count(struct NANR *self, int acell_index)
{
	assert(self != NULL);
	assert(self->header.magic == NANR_MAGIC);

	if (!(0 <= acell_index && acell_index < self->abnk.header.acell_count)) {
		return 0;
	}

	return self->abnk.acells[acell_index].frame_count;
}

/* Return the index of the frame which should be visible at the given tick. */
int
nanr_get_frame_at_tick(struct NANR *self, int acell_index, u16 tick)
{
	assert(self != NULL);
	assert(self->header.magic == NANR_MAGIC);

	if (!(0 <= acell_index && acell_index < self->abnk.header.acell_count)) {
		return -1;
	}

	struct acell *acell = &self->abnk.acells[acell_index];

	struct frame *frames = (struct frame *)((u8 *)self->abnk.frames + acell->frame_offset);

	u16 total = 0;
	for (int i = 0; i < acell->frame_count; i++) {
		struct frame *frame = &frames[i];

		if (tick < frame->frame_duration) {
			return i;
		} else {
			tick -= frame->frame_duration;
			total += frame->frame_duration;
		}
	}

	if (total == 0) {
		return 0;
	}

	tick = tick % total;

	for (int i = 0; ; i++) {
		struct frame *frame = &frames[i];
		if (tick < frame->frame_duration) {
			return i;
		} else {
			tick -= frame->frame_duration;
		}
	}
	return 0;
}
