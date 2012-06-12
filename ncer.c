/* ncer.c - NCER (cell resource) support
 *
 * Copyright © 2011 magical
 *
 * This file is part of spriterip; it is licensed under the GNU GPLv3
 * and comes with NO WARRANTY. See rip.c for details.
 */

#include <stdlib.h> /* NULL, size_t */
#include <stdio.h> /* FILE, stdout */
#include <limits.h> /* INT_MAX, INT_MIN */

#include "nitro.h" /* struct format_info, struct nitro, struct OBJ, magic_t, format_header */
#include "ncgr.h" /* struct NCGR, ncgr_get_pixel */
#include "image.h" /* struct image */
#include "common.h" /* OKAY, FAIL, NOMEM, CALLOC, FREAD, assert, struct dim, struct coords, u8, u16, u32, s16, fx16 */

#include "ncer.h"

struct CEBK_celldata {
	u16 obj_count;
	u16 unknown;
	u32 obj_offset;
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

	int obj_count;
	struct OBJ *obj_data;

	struct CEBK_partitiondata *partition_data;
};

struct NCER {
	struct nitro header;

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

	self->cebk.obj_count = 0;
	for (int i = 0; i < (signed long)self->cebk.header.cell_count; i++) {
		self->cebk.obj_count += self->cebk.cell_data[i].obj_count;
	}

	if (CALLOC(self->cebk.obj_data, self->cebk.obj_count) == NULL) {
		return FAIL;
	}

	FREAD(fp, self->cebk.obj_data, self->cebk.obj_count);

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
		FREE(self->cebk.obj_data);
		FREE(self->cebk.partition_data);
	}
}

struct format_info NCER_format = {
	format_header('NCER', struct NCER),

	.read = ncer_read,
	.free = ncer_free,
};

/******************************************************************************/

/* Public functions */

void
ncer_dump(struct NCER *self, FILE *fp)
{
	assert(self != NULL);
	assert(nitro_get_magic(self) == (magic_t)'NCER');

	if (fp == NULL) {
		fp = stdout;
	}

	char magic_buf[MAGIC_BUF_SIZE];

	fprintf(fp, "ncer.magic = %s\n", strmagic(self->header.magic, magic_buf));
	fprintf(fp, "ncer.size = %u\n", self->header.size);
	fprintf(fp, "ncer.cebk.cell_count = %u\n", self->cebk.header.cell_count);
	fprintf(fp, "ncer.cebk.cell_type = %u\n", self->cebk.header.cell_type);
	fprintf(fp, "ncer.cebk.flags = %x\n", self->cebk.header.flags);


	for (int i = 0; i < (signed long)self->cebk.header.cell_count; i++) {
		struct CEBK_celldata *cell = &self->cebk.cell_data[i];
		fprintf(fp, "ncer.cebk.cell[%d].obj_count = %u\n", i, cell->obj_count);
		fprintf(fp, "ncer.cebk.cell[%d].unknown = %u\n", i, cell->unknown);
		fprintf(fp, "ncer.cebk.cell[%d].obj_offset = %u\n", i, cell->obj_offset);
		struct OBJ *objs = (struct OBJ*)((u8 *)self->cebk.obj_data + cell->obj_offset);

		for (int j = 0; j < cell->obj_count; j++) {
			struct OBJ *obj = &objs[j];
			const struct dim *d = &obj_sizes[obj->obj_size][obj->obj_shape];

			fprintf(fp,
				"obj[%d] = {\n"
				"\t.y = %d,\n"
				"\t.x = %d,\n"
				"\t.color_mode = %d,\n"
				"\t.rs_mode = %u,\n"
				"\t.rs_param = %u,\n"
				"\t.obj_mode = %u,\n"
				"\t.obj_shape = %u,\n"
				"\t.obj_size = %u,\n"
				"\t.tile_index = %u,\n"
				"\t.palette_index = %u,\n"
				"\t.priority = %u,\n"
				"\t.dim = {.height=%d, .width=%d},\n"
				"}\n",
				j,
				obj->y, obj->x,
				obj->color_mode,
				obj->rs_mode, obj->rs_param,
				obj->obj_mode, obj->obj_shape, obj->obj_size,
				obj->tile_index, obj->palette_index,
				obj->priority,
				d->height, d->width);
		}
	}

	//printf("sizeof(OAM) = %u", (unsigned int) sizeof(struct OBJ));
}

#define check_range(a, b, c) ((a) <= (b) && (b) < (c))

/* Render an object onto an image, possibly applying a transformation. */
// XXX this largely duplicates obj_draw. refactor.
static int
image_render(struct image *self, struct coords offset,
             struct image *source, fx16 transform[4], struct coords center)
{
	int y, x;
	int y2, x2;

	//warn("[%f,%f,%f,%f]", transform[0]/256.0, transform[1]/256.0,
	//                      transform[2]/256.0, transform[3]/256.0);
	//warn("size=%d,%d center=%d,%d", source->dim.width, source->dim.height,
	//                                center.x, center.y);

	for (y = 0; y < source->dim.height; y++) {
	for (x = 0; x < source->dim.width; x++) {
		int pixel_index = (offset.y + y - center.y) * self->dim.width
		                + (offset.x + x - center.x);
		if (!check_range(0, offset.y+y-center.y, self->dim.height) ||
		    !check_range(0, offset.x+x-center.x, self->dim.width)) {
			continue;
		}
		if (!check_range(0, pixel_index, self->pixels->size)) {
			continue;
		}
		if (self->pixels->data[pixel_index] != 0) {
			continue;
		}

		if (transform) {
			/* An arithmetic right shift on a twos-complement
			 * signed integer is equivalent to a floored division,
			 * which is exactly what we want. */
			x2 = (((x - center.x) * transform[0] +
			       (y - center.y) * transform[1]) >> 8) + center.x;
			y2 = (((x - center.x) * transform[2] +
			       (y - center.y) * transform[3]) >> 8) + center.y;
		} else {
			x2 = x;
			y2 = y;
		}

		// check whether the transformed coordinates are within the
		// cell data.
		int source_pixel_index = y2 * source->dim.width + x2;
		if (!check_range(0, x2, source->dim.width) ||
		    !check_range(0, y2, source->dim.height)) {
			continue;
		}
		if (!check_range(0, source_pixel_index, source->pixels->size)) {
			continue;
		}

		//draw the pixel
		self->pixels->data[pixel_index] =
		    source->pixels->data[source_pixel_index];
	}
	}
	return OKAY;
}

static int
obj_draw(struct OBJ *obj, struct NCGR *ncgr, struct image *image,
         struct coords frame_offset, fx16 transform[4])
{
	// x and y specify the position of the top-left corner of the frame.
	// coordinates for rotations have the origin at center of the frame.
	//printf("x = %d, y = %d\n", obj->x, obj->y);

	// the real dimensions of the cell
	struct dim cell_dim = obj_sizes[obj->obj_size][obj->obj_shape];

	//warn("cell_dim = {.height = %d, .width = %d}", cell_dim.height, cell_dim.width);
	//warn("tile_index = %d", obj->tile_index);

	struct buffer *pixels =
	    ncgr_get_cell_pixels(ncgr, obj->tile_index, cell_dim);
	if (pixels == NULL) {
		return FAIL;
	}

	// the dimensions of the "on-screen" frame
	// is either the same as cell_dim, or double
	struct dim frame_dim = cell_dim;
	if (obj->rs_mode & 2) {
		frame_dim.height *= 2;
		frame_dim.width *= 2;
	}

	struct coords transform_offset = {frame_dim.width / 2, frame_dim.height / 2};

	int rs_mode = obj->rs_mode;
	int x, y;
	// fixed-point 8.8
	s16 x_prime_fx, y_prime_fx;
	int x_prime, y_prime;

	if (transform == NULL) { rs_mode &= ~1; }

	//warn("transform_offset = {%d, %d}", transform_offset.x, transform_offset.y);

	for (y = 0; y < frame_dim.height; y++) {
	for (x = 0; x < frame_dim.width; x++) {
		if (obj->y + frame_offset.y + y < 0 ||
		    obj->x + frame_offset.x + x < 0) {
			continue;
		}

		if (rs_mode & 1) {
			// affine transformation!
			// multiply the matrix by the sprite coordinates;
			// origin at the center of the frame
			x_prime_fx = (x - transform_offset.x) * transform[0] + (y - transform_offset.y) * transform[1];
			y_prime_fx = (x - transform_offset.x) * transform[2] + (y - transform_offset.y) * transform[3];
			// grab the integer portion and convert the coordinates back
			x_prime = (x_prime_fx >> 8) + transform_offset.x;
			y_prime = (y_prime_fx >> 8) + transform_offset.y;
		} else {
			// XXX implement flips
			x_prime = x;
			y_prime = y;
		}

		if (rs_mode & 2) {
			x_prime -= cell_dim.width / 2;
			y_prime -= cell_dim.height / 2;
		}

		//warn("x = %d, y = %d; x_prime = %d, y_prime = %d", x, y, x_prime, y_prime);

		// check whether the transformed coordinates are within the
		// cell data.
		if (0 <= x_prime && x_prime < cell_dim.width &&
		    0 <= y_prime && y_prime < cell_dim.height) {
			//draw the pixel
			int pixel_offset = (obj->y + frame_offset.y + y) * image->dim.width
					 + (obj->x + frame_offset.x + x);
			if (0 <= pixel_offset && (size_t)pixel_offset < image->pixels->size) {
				/*u8 pixel = pixels->data[y_prime * cell_dim.width + x_prime];
				if (pixel != 0) {
					image->pixels->data[pixel_offset] = pixel;
				}*/
				if (image->pixels->data[pixel_offset] == 0) {
					image->pixels->data[pixel_offset] =
					  pixels->data[y_prime * cell_dim.width + x_prime];
				}
			}
		}
	}
	}
	FREE(pixels);
	return OKAY;
}

static int
render(struct NCER *self, int index, struct NCGR *ncgr, struct image *image, struct coords offset)
{
	struct CEBK_celldata *cell = self->cebk.cell_data + index;
	struct OBJ *objs = (void *)((u8*)self->cebk.obj_data + cell->obj_offset);

	for (int i = 0; i < cell->obj_count; i++) {
		struct OBJ *obj = &objs[i];

		/*if(obj->rs_mode == 1) {
			warn("transform, not doubled");
		} else if (!(obj->rs_mode & 1)) {
			warn("not transformed");
		}*/

		if (obj_draw(obj, ncgr, image, offset, NULL)) {
			return FAIL;
		}
	}
	return OKAY;
}

static void
ncer_get_size(struct NCER *self, int index, struct dim *dim, struct coords *center)
{
	struct CEBK_celldata *cell = self->cebk.cell_data + index;
	struct OBJ *objs = (void *)((u8*)self->cebk.obj_data + cell->obj_offset);

	struct coords topleft = {INT_MAX, INT_MAX},
	              bottomright = {INT_MIN, INT_MIN};

	#define min(a,b) ((a) < (b) ? (a) : (b))
	#define max(a,b) ((a) > (b) ? (a) : (b))

	for (int i = 0; i < cell->obj_count; i++) {
		struct OBJ *obj = &objs[i];
		struct dim cell_dim = obj_sizes[obj->obj_size][obj->obj_shape];
		if (obj->rs_mode & 2) {
			cell_dim.width *= 2;
			cell_dim.height *= 2;
		}
		topleft.x = min(topleft.x, obj->x);
		topleft.y = min(topleft.y, obj->y);
		bottomright.x = max(bottomright.x, obj->x + cell_dim.width);
		bottomright.y = max(bottomright.y, obj->y + cell_dim.height);
	}

	dim->width = bottomright.x - topleft.x;
	dim->height = bottomright.y - topleft.y;
	center->x = 0 - topleft.x;
	center->y = 0 - topleft.y;
}

int
ncer_draw_cell_t(struct NCER *self, int index, struct NCGR *ncgr, struct image *image, struct coords offset, fx16 transform[4])
{
	assert(self != NULL);
	assert(self->header.magic == (magic_t)'NCER');
	assert(ncgr != NULL);
	assert(image != NULL);
	assert(image->pixels != NULL);

	//transform[0] = 0; transform[1] = -0x100; transform[2] = 0x100; transform[3] = 0;
	//transform = (s16[4]){0xb5,-0xb5,0xb5,0xb5};

	/* First we render the objs as-is onto a blank image, then we
	 * render and transform that image onto the destination image. */

	/* I first tried implementing this by just fixing up the obj
	 * coordinates, but that didn't really work - it left very
	 * visible gaps between the objs. */
	struct image cell_image;
	struct coords center;
	ncer_get_size(self, index, &cell_image.dim, &center);
	cell_image.pixels = buffer_alloc(cell_image.dim.width *
	                                 cell_image.dim.height);
	if (cell_image.pixels == NULL) {
		return NOMEM;
	}

	if (render(self, index, ncgr, &cell_image, center)) {
		return FAIL;
	}

	if (image_render(image, offset, &cell_image, transform, center)) {
		return FAIL;
	}

	FREE(cell_image.pixels);

	return OKAY;
}

int
ncer_draw_cell(struct NCER *self, int index, struct NCGR *ncgr, struct image *image, struct coords offset)
{
	assert(self != NULL);
	assert(self->header.magic == (magic_t)'NCER');
	assert(ncgr != NULL);
	assert(image != NULL);
	assert(image->pixels != NULL);

	return render(self, index, ncgr, image, offset);
}

int
ncer_draw_boxes(struct NCER *self, int index, struct image *image, struct coords offset)
{
	assert(self != NULL);
	assert(self->header.magic == (magic_t)'NCER');
	assert(image != NULL);
	assert(image->pixels != NULL);

	struct CEBK_celldata *cell = self->cebk.cell_data + index;

	for (int i = 0; i < cell->obj_count; i++) {
		struct OBJ *obj = (struct OBJ *)((u8 *)self->cebk.obj_data + cell->obj_offset) + i;

		// the real dimensions of the cell
		struct dim frame_dim = obj_sizes[obj->obj_size][obj->obj_shape];

		struct coords topleft;
		topleft.x = obj->x + offset.x;
		topleft.y = obj->y + offset.y;

		if (obj->rs_mode & 2) {
			frame_dim.height *= 2;
			frame_dim.width *= 2;
		}

		struct coords bottomright = {
			.x = topleft.x + frame_dim.width - 1,
			.y = topleft.y + frame_dim.height - 1,
		};

		image_draw_square(image, topleft, bottomright);
	}
	return OKAY;
}

int
ncer_get_cell_count(struct NCER *self)
{
	assert(self != NULL);
	assert(self->header.magic == (magic_t)'NCER');

	return self->cebk.header.cell_count;
}


int
ncer_get_cell_dim(struct NCER *self, int index, struct dim *dim, struct coords *center)
{
	assert(self != NULL);
	assert(self->header.magic == (magic_t)'NCER');
	assert(dim != NULL);
	assert(center != NULL);

	assert(0 <= index && index < self->cebk.header.cell_count);

	ncer_get_size(self, index, dim, center);
	return OKAY;
}
