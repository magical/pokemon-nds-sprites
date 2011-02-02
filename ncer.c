/* NCER */

#include <stdlib.h> /* NULL, size_t */
#include <stdio.h> /* FILE */

#include "nitro.h" /* struct format_info, struct standard_header, struct OAM, magic_t, format_header */
#include "ncgr.h" /* struct NCGR, ncgr_get_pixel */
#include "image.h" /* struct image */
#include "common.h" /* OKAY, FAIL, NOMEM, CALLOC, FREAD, assert, struct dim, struct coords, u8, u16, u32, s16 */

#include "ncer.h"

struct CEBK_celldata {
	u16 oam_count;
	u16 unknown;
	u32 oam_offset;
};

// XXX huh?
struct CEBK_celldata_ex {
	s16 x_max;
	s16 y_max;
	s16 x_min;
	s16 y_min;
};

struct CEBK_partitiondata {
	u32 start;
	u32 size;
};

struct CEBK {
	struct {
		magic_t magic;
		u32 size;

		u16 cell_count;
		u16 cell_type;

		u32 cell_data_offset;
		u32 flags;

		u32 partition_data_offset;

		u32 padding[2];
	} header;

	struct CEBK_celldata *cell_data;
	// May be NULL if cell_type == 0
	struct CEBK_celldata_ex *cell_data_ex;

	int oam_count;
	struct OAM *oam_data;

	struct CEBK_partitiondata *partition_data;
};

struct NCER {
	struct standard_header header;

	struct CEBK cebk;
	//struct LABL labl;
	//struct UEXT uext;
};


static int
ncer_read(void *buf, FILE *fp)
{
	struct NCER *self = buf;
	assert(self != NULL);

	FREAD(fp, &self->header, 1);
	assert(self->header.magic == (magic_t)'NCER');
	assert(self->header.chunk_count == 3 || self->header.chunk_count == 1);

	FREAD(fp, &self->cebk.header, 1);
	assert(self->cebk.header.magic == (magic_t)'CEBK');

	if (CALLOC(self->cebk.cell_data,
	           self->cebk.header.cell_count) == NULL) {
		return NOMEM;
	}
	switch (self->cebk.header.cell_type) {
	case 0:
		FREAD(fp, self->cebk.cell_data, self->cebk.header.cell_count);
		break;
	case 1:
		if (CALLOC(self->cebk.cell_data_ex,
		           self->cebk.header.cell_count) == NULL) {
			return NOMEM;
		}
		for (int i = 0; i < (signed long)self->cebk.header.cell_count; i++) {
			FREAD(fp, &self->cebk.cell_data[i], 1);
			FREAD(fp, &self->cebk.cell_data_ex[i], 1);
		}
		break;
	default:
		warn("Unknown cell type: %d", self->cebk.header.cell_type);
		return FAIL;
	}

	self->cebk.oam_count = 0;
	for (int i = 0; i < (signed long)self->cebk.header.cell_count; i++) {
		self->cebk.oam_count += self->cebk.cell_data[i].oam_count;
	}

	if (CALLOC(self->cebk.oam_data, self->cebk.oam_count) == NULL) {
		return FAIL;
	}

	FREAD(fp, self->cebk.oam_data, self->cebk.oam_count);

	// partition_data?

	return OKAY;
}


static void
ncer_free(void *buf)
{
	struct NCER *self = buf;
	if (self != NULL &&
	    self->header.magic == (magic_t)'NCER') {
		FREE(self->cebk.cell_data);
		FREE(self->cebk.cell_data_ex);
		FREE(self->cebk.oam_data);
		FREE(self->cebk.partition_data);
	}
}

struct format_info NCER_format = {
	format_header('NCER', struct NCER),
	NULL, /* info */
	ncer_read,
	ncer_free
};

/******************************************************************************/

/* Public functions */

int
ncer_draw_cell(struct NCER *self, int index, struct NCGR *ncgr, struct image *image, struct coords frame_offset)
{
	assert(self != NULL);
	assert(self->header.magic == (magic_t)'NCER');
	assert(ncgr != NULL);
	assert(image != NULL);
	assert(image->pixels != NULL);

	struct CEBK_celldata *cell = self->cebk.cell_data + index;

	for (int i = 0; i < cell->oam_count; i++) {
		struct OAM *oam = (struct OAM *)((u8 *)self->cebk.oam_data + cell->oam_offset) + i;

		// x and y specify the position of the top-left corner of the
		// frame.
		// coordinates for rotations have the origin at center of the
		// frame.
		//printf("x = %d, y = %d\n", oam->x, oam->y);

		// the real dimensions of the cell
		struct dim cell_dim = obj_sizes[oam->obj_size][oam->obj_shape];

		//warn("cell_dim = {.height = %d, .width = %d}", cell_dim.height, cell_dim.width);
		//warn("tile_index = %d", oam->tile_index);

		// the dimensions of the "on-screen" frame
		// is either the same as cell_dim, or double
		struct dim frame_dim = cell_dim;
		if (oam->rs_mode & 2) {
			frame_dim.height *= 2;
			frame_dim.width *= 2;
		}

		// the affine transform matrix
		// XXX this is the identity matrix; grab a real one
		s16 m[4] = {0x0100, 0, 0, 0x0100};

		struct coords transform_offset = {frame_dim.width / 2, frame_dim.height / 2};

		const int rs_mode = oam->rs_mode;
		int x, y;
		// fixed-point 8.8
		s16 x_prime_fx, y_prime_fx;
		int x_prime, y_prime;

		//warn("transform_offset = {%d, %d}", transform_offset.x, transform_offset.y);

		for (y = 0; y < frame_dim.height; y++) {
		for (x = 0; x < frame_dim.width; x++) {
			if (oam->y + frame_offset.y + y < 0 ||
			    oam->x + frame_offset.x + x < 0) {
				continue;
			}

			if (rs_mode & 1) {
				// affine transformation!
				// multiply the matrix by the sprite coordinates;
				// origin at the center of the frame
				x_prime_fx = (x - transform_offset.x) * m[0] + (y - transform_offset.y) * m[1];
				y_prime_fx = (x - transform_offset.x) * m[2] + (y - transform_offset.y) * m[3];
				// grab the integer portion and convert the coordinates back
				x_prime = (x_prime_fx >> 8) + cell_dim.width / 2;
				y_prime = (y_prime_fx >> 8) + cell_dim.height / 2;
			} else {
				// XXX implement flips
				x_prime = x;
				y_prime = y;
			}

			//warn("x = %d, y = %d; x_prime = %d, y_prime = %d", x, y, x_prime, y_prime);

			// check whether the transformed coordinates are within the
			// cell data.
			if (0 <= x_prime && x_prime < cell_dim.width &&
			    0 <= y_prime && y_prime < cell_dim.height) {
				//draw the pixel
				int pixel_offset = (oam->y + frame_offset.y + y) * image->dim.width
				                 + (oam->x + frame_offset.x + x);
				if (0 < pixel_offset && (size_t)pixel_offset < image->pixels->size) {
					// XXX this is stupid; can't we just grab the whole chunk of pixels?
					image->pixels->data[pixel_offset] =
						ncgr_get_pixel(ncgr, oam->tile_index,
						               x_prime, y_prime, cell_dim.width);
				}
			}
		}
		}
	}

	return OKAY;
}


int
ncer_draw_boxes(struct NCER *self, int index, struct image *image, struct coords offset)
{
	assert(self != NULL);
	assert(self->header.magic == (magic_t)'NCER');
	assert(image != NULL);
	assert(image->pixels != NULL);

	struct CEBK_celldata *cell = self->cebk.cell_data + index;

	for (int i = 0; i < cell->oam_count; i++) {
		struct OAM *oam = (struct OAM *)((u8 *)self->cebk.oam_data + cell->oam_offset) + i;

		// the real dimensions of the cell
		struct dim frame_dim = obj_sizes[oam->obj_size][oam->obj_shape];

		struct coords topleft;
		topleft.x = oam->x + offset.x;
		topleft.y = oam->y + offset.y;

		if (oam->rs_mode & 2) {
			frame_dim.height *= 2;
			frame_dim.width *= 2;
		}

		struct coords bottomright = {
			.x = topleft.x + frame_dim.width,
			.y = topleft.y + frame_dim.height,
		};

		image_draw_square(image, topleft, bottomright);
	}
	return OKAY;
}


