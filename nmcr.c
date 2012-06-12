/* nmcr - NMCR (mapped cell resource) support
 *
 * Copyright © 2011 magical
 *
 * This file is part of spriterip; it is licensed under the GNU GPLv3
 * and comes with NO WARRANTY. See rip.c for details.
 */

#include "nmcr.h"

#include "nitro.h" /* struct nitro, struct format_info, magic_t, format_header */
#include "ncgr.h" /* struct NCGR */
#include "ncer.h" /* struct NCER */
#include "nanr.h" /* struct NANR, nanr_draw_frame */
#include "image.h" /* struct image */
#include "common.h" /* struct coords, u8, u16, u32, FREAD */

struct map_header {
	u16 count;
	u16 unknown_count;
	u32 offset;
};

struct map_data {
	u16 acell_index;
	s16 x;
	s16 y;
	u8 unknown;
	u8 priority;
};

struct MCBK {
	struct {
		magic_t magic;
		u32 size;

		u16 count;
		u16 padding;

		u32 header_offset;
		u32 data_offset;

		u32 padding2[2];
	} header;

	struct buffer *data;

	struct map_header *map_headers;
	struct map_data *map_data;
};

struct NMCR {
	struct nitro header;
	struct MCBK mcbk;
};


static int
nmcr_read(void *buf, FILE *fp)
{
	struct NMCR *self = buf;
	assert(self != NULL);

	FREAD(fp, &self->header, 1);
	assert(self->header.magic == NMCR_MAGIC);

	FREAD(fp, &self->mcbk.header, 1);

	if (ferror(fp) || feof(fp)) {
		return FAIL;
	}

	size_t data_size = self->mcbk.header.size - sizeof(self->mcbk.header);
	self->mcbk.data = buffer_alloc(data_size);
	if (self->mcbk.data == NULL) {
		return NOMEM;
	}
	if (fread(self->mcbk.data->data, data_size, 1, fp) != 1) {
		return FAIL;
	}

	size_t base = sizeof(self->mcbk.header) - 8;
	assert(base == 0x14);

	/* XXX bounds checks - invalid data could walk all over memory */
	self->mcbk.map_headers = (struct map_header *)(self->mcbk.data->data +
	    (self->mcbk.header.header_offset - base));
	self->mcbk.map_data = (struct map_data *)(self->mcbk.data->data +
	    (self->mcbk.header.data_offset - base));

	return OKAY;
}

static void
nmcr_free(void *buf)
{
	struct NMCR *self = buf;
	if (self != NULL &&
	    self->header.magic == NMCR_MAGIC) {
		FREE(self->mcbk.data);
	}
}

struct format_info NMCR_format = {
	format_header(NMCR_MAGIC, struct NMCR),
	
	.read = nmcr_read,
	.free = nmcr_free,
};

int
nmcr_draw(struct NMCR *self, int index, int tick,
          struct NANR *nanr, struct NCER *ncer, struct NCGR *ncgr,
          struct image *image, struct coords offset)
{
	assert(self != NULL);
	assert(self->header.magic == NMCR_MAGIC);
	assert(nanr != NULL);
	assert(ncer != NULL);
	assert(ncgr != NULL);
	assert(image != NULL);

	struct MCBK *mcbk = &self->mcbk;

	if (!(0 <= index && index < mcbk->header.count)) {
		return FAIL;
	}

	struct map_header *header = &mcbk->map_headers[index];
	struct map_data *datas =
	    (void *)((u8*)mcbk->map_data + header->offset);

	for (int i = 0; i < header->count; i++) {
		struct map_data *data = &datas[i];
		struct coords cell_offset = {
			.x = offset.x + data->x,
			.y = offset.y + data->y,
		};

		//warn("%d %d", data->priority, data->unknown);

		int frame = nanr_get_frame_at_tick(nanr, data->acell_index, tick);
		if (frame < 0) {
			return FAIL;
		}

		if (nanr_draw_frame(nanr, data->acell_index, frame, ncer, ncgr, image, cell_offset)) {
			return FAIL;
		}
	}

	return OKAY;
}
