/* nanr - NANR (animation resource) support and
 *        NMAR (mapped? animation resource) support
 *
 * Copyright © 2011 magical
 *
 * This file is part of spriterip; it is licensed under the GNU GPLv3
 * and comes with NO WARRANTY. See rip.c for details.
 */

#include "nanr.h"
#include "nmar.h"

#include <stdio.h> /* FILE */
#include <math.h> /* sin, cos */

#include "nitro.h" /* struct nitro, struct format_info, magic_t, format_header */
#include "common.h" /* OKAY, FAIL, NOMEM, u8, u16, u32, s32, struct buffer */
#include "nmcr.h" /* struct NMCR, nmcr_draw */
#include "ncer.h" /* struct NCER */
#include "ncgr.h" /* struct NCGR */

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

/* ABNK structure. Shared by NANR and NMAR. */

struct acell {
	u32 frame_count;

	/* 0 = 4 bytes; 1 = 16 bytes; 2 = 8 bytes */
	u16 frame_type;
	/* 1 in NANR, 2 in NMAR */
	u16 cell_type;

	u32 unknown;

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

	s16 theta; // angle in 65536 degrees

	s32 x_mag; // 1.19.12 fixed point
	s32 y_mag;

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

static int
abnk_read(struct ABNK *self, FILE *fp) {
	FREAD(fp, &self->header, 1);

	if (ferror(fp) || feof(fp)) {
		return FAIL;
	}

	size_t data_size = self->header.size - sizeof(self->header);
	self->data = buffer_alloc(data_size);
	if (self->data == NULL) {
		return NOMEM;
	}

	if (fread(self->data->data, data_size, 1, fp) != 1) {
		return FAIL;
	}

	size_t base = sizeof(self->header) - 8;
	assert(base == 0x18);

	/* XXX bounds checks - invalid data could walk all over memory */
	self->acells = (struct acell *)(self->data->data +
	    (self->header.acell_data_offset - base));
	self->frames = (struct frame *)(self->data->data +
	    (self->header.frame_data_offset - base));
	self->frame_data = self->data->data +
	    (self->header.frame_data_data_offset - base);

	return OKAY;
}


/* NANR */

struct NANR {
	struct nitro header;
	struct ABNK abnk;
};

static int
nanr_read(void *buf, FILE *fp)
{
	struct NANR *self = buf;
	assert(self != NULL);

	FREAD(fp, &self->header, 1);
	assert(self->header.magic == NANR_MAGIC);

	return abnk_read(&self->abnk, fp);
}

static void
nanr_free(void *buf)
{
	struct NANR *self = buf;
	if (self != NULL &&
	    self->header.magic == NANR_MAGIC) {
		FREE(self->abnk.data);
	}
}

struct format_info NANR_format = {
	format_header(NANR_MAGIC, struct NANR),

	.read = nanr_read,
	.free = nanr_free,
};


/* NMAR */

struct NMAR {
	struct nitro header;
	struct ABNK abnk;
};

static int
nmar_read(void *buf, FILE *fp)
{
	struct NMAR *self = buf;
	assert(self != NULL);

	FREAD(fp, &self->header, 1);
	assert(self->header.magic == NMAR_MAGIC);

	return abnk_read(&self->abnk, fp);
}

static void
nmar_free(void *buf)
{
	struct NMAR *self = buf;
	if (self != NULL &&
	    self->header.magic == NMAR_MAGIC) {
		FREE(self->abnk.data);
	}
}

struct format_info NMAR_format = {
	format_header(NMAR_MAGIC, struct NMAR),

	.read = nmar_read,
	.free = nmar_free,
};


/* Methods */

static int sin16(int theta)
{
	return (int)(sin(theta * (2*M_PI) / 0x10000) * 0x1000);
}

static int cos16(int theta)
{
	return (int)(cos(theta * (2*M_PI) / 0x10000) * 0x1000);
}

static int
get_frame_data(struct ABNK *abnk, struct acell *acell, int frame_index,
               int *cell_index, fx16 m[], struct coords *offset)
{
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

		int sin_theta = sin16(frame_data->theta);
		int cos_theta = cos16(frame_data->theta);

		m[0] = 0x100 *  cos_theta / frame_data->x_mag;
		m[1] = 0x100 * +sin_theta / frame_data->x_mag;
		m[2] = 0x100 * -sin_theta / frame_data->y_mag;
		m[3] = 0x100 *  cos_theta / frame_data->y_mag;

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
	if (get_frame_data(&self->abnk, acell, frame_index,
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

int
nmar_get_cell_count(struct NMAR *self)
{
	assert(self != NULL);
	assert(self->header.magic == NMAR_MAGIC);

	return self->abnk.header.acell_count;
}

int
nmar_get_period(struct NMAR *self, int acell_index)
{
	assert(self != NULL);
	assert(self->header.magic == NMAR_MAGIC);

	int period = 0;

	if (!(0 <= acell_index && acell_index < self->abnk.header.acell_count)) {
		return -1;
	}

	struct acell *acell = &self->abnk.acells[acell_index];
	struct frame *frames = (void *)self->abnk.frames + acell->frame_offset;

	for (int i = 0; i < acell->frame_count; i++) {
		period += frames[i].frame_duration;
	}

	return period;
}

int
nmar_draw_frame(struct NMAR *self, int acell_index, int frame_index, int tick,
                struct NMCR *nmcr, struct NANR *nanr, struct NCER *ncer, struct NCGR *ncgr,
                struct image *image, struct coords offset)
{
	assert(self != NULL);
	assert(self->header.magic == NMAR_MAGIC);
	assert(nmcr != NULL);
	assert(nanr != NULL);
	assert(ncer != NULL);
	assert(ncgr != NULL);
	assert(image != NULL);

	if (!(0 <= acell_index && acell_index < self->abnk.header.acell_count)) {
		return FAIL;
	}

	struct acell *acell = &self->abnk.acells[acell_index];

	int cell_index;
	fx16 m[4];
	struct coords o;
	if (get_frame_data(&self->abnk, acell, frame_index, &cell_index, m, &o)) {
		return FAIL;
	}

	o.x += offset.x;
	o.y += offset.y;

	return nmcr_draw(nmcr, cell_index, tick, nanr, ncer, ncgr, image, o);
}

int
nmar_draw(struct NMAR *self, int acell_index, int tick,
          struct NMCR *nmcr, struct NANR *nanr, struct NCER *ncer, struct NCGR *ncgr,
          struct image *image, struct coords offset)
{
	assert(self != NULL);
	assert(self->header.magic == NMAR_MAGIC);
	assert(nmcr != NULL);
	assert(nanr != NULL);
	assert(ncer != NULL);
	assert(ncgr != NULL);
	assert(image != NULL);

	if (!(0 <= acell_index && acell_index < self->abnk.header.acell_count)) {
		return FAIL;
	}

	//warn("%d %d", acell_index, tick);
	struct acell *acell = &self->abnk.acells[acell_index];
	struct frame *frames = (void *)self->abnk.frames + acell->frame_offset;

	u32 frame_tick = 0;
	u32 prev_index = -1;
	for (u16 i = 0; i < acell->frame_count; i++) {
		u32 duration = frames[i].frame_duration;
		u32 cell_index = *(u16*)(self->abnk.frame_data + frames[i].data_offset);
		if (cell_index != prev_index) {
			frame_tick = 0;
		}
		//warn("frame %d: duration %d index %d", i, duration, *(u16*)((void *)self->abnk.frame_data + frames[i].data_offset));
		if (tick < duration) {
			return nmar_draw_frame(self, acell_index, i, frame_tick + tick,
			                       nmcr, nanr, ncer, ncgr, image, offset);
		}
		tick -= duration;
		frame_tick += duration;
		prev_index = cell_index;
	}
	return FAIL;
}
