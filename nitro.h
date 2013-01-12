/*
 * Copyright © 2011 magical
 *
 * This file is part of spriterip; it is licensed under the GNU GPLv3
 * and comes with NO WARRANTY. See rip.c for details.
 */
#ifndef NITRO_H
#define NITRO_H

#include <stdio.h> /* FILE */
#include <sys/types.h> /* off_t */

#include "common.h" /* struct buffer, struct palette, struct dim, struct coords,
                       u8, u16, u32, s16, fx16 */
#include "image.h" /* struct image */


typedef u32 magic_t;

// This is the common header used by most of the file formats found in the
// DS Pokemon games.
struct nitro {
	magic_t magic;
	u16 bom;
	u16 version;

	u32 size;
	u16 header_size;
	u16 chunk_count;
};


// An OBJ is a moveable sprite with first-class support from the DS's video
// system. They are sometimes called OAMs, which more properly refers to the
// region of Memory where Object Attributes are stored. See GBATEK for details.
// XXX add some union magic
struct OBJ {
	s16 y:8;
	u16 rs_mode:2;
	u16 obj_mode:2;
	u16 mosaic_flag:1;
	u16 color_mode:1;
	u16 obj_shape:2;

	s16 x:9;
	u16 rs_param:5;
	u16 obj_size:2;

	u16 tile_index:10;
	u16 priority:2;
	u16 palette_index:4;
};

// obj_sizes [size][shape]
extern struct dim obj_sizes[4][4];

struct nitro_type {
	magic_t magic;

	size_t size;
	void *initializer;

	int (*init)(void *);
	int (*read)(void *, FILE *);
	void (*free)(void *);
};

//extern const struct format_info * const formats[];

#define nitro_type_init(magic_, type) \
	.magic = magic_, \
	.size = sizeof (type), \
	.initializer = &(type){}

/* Public functions */

extern const struct nitro_type *nitro_lookup(magic_t magic);
// size may be zero unless you're reading compressed data
extern void *nitro_read(FILE *fp, off_t size);
extern void nitro_free(void *chunk);

static inline magic_t
nitro_get_magic(void *chunk)
{
	if (chunk != NULL) {
		return ((struct nitro *)chunk)->magic;
	}
	return 0;
}

#define MAGIC_BUF_SIZE 5

/* convert a magic_t to a string. you must supply a char array at least 
 * MAGIC_BUF_SIZE bytes long to hold the resulting string */
extern char *strmagic(magic_t magic, char *buf);


/* nitro format structure declarations */
struct NARC;
struct NCGR;
struct NCLR;
struct NCER;
struct NANR;
struct NMCR;
struct NMAR;

#define NARC_MAGIC ((magic_t)'CRAN')
#define NCGR_MAGIC ((magic_t)'NCGR')
#define NCLR_MAGIC ((magic_t)'NCLR')
#define NCER_MAGIC ((magic_t)'NCER')
#define NANR_MAGIC ((magic_t)'NANR')
#define NMCR_MAGIC ((magic_t)'NMCR')
#define NMAR_MAGIC ((magic_t)'NMAR')

extern struct nitro_type NARC_type;
extern struct nitro_type NCGR_type;
extern struct nitro_type NCLR_type;
extern struct nitro_type NCER_type;
extern struct nitro_type NANR_type;
extern struct nitro_type NMCR_type;
extern struct nitro_type NMAR_type;



/* NARC */
extern void *narc_load_file(struct NARC *self, int index);
extern u32 narc_get_file_size(struct NARC *self, int index);
extern u32 narc_get_file_count(struct NARC *self);


/* NCGR */
extern int ncgr_get_dim(struct NCGR *self, struct dim *dim);
extern struct buffer *ncgr_get_pixels(struct NCGR *self);
extern struct buffer *ncgr_get_cell_pixels(struct NCGR *self, u16 tile, struct dim cell_dim);

extern void ncgr_decrypt_dp(struct NCGR *self);
extern void ncgr_decrypt_pt(struct NCGR *self);


/* NCLR */
extern struct palette *nclr_get_palette(struct NCLR *self, int index);


/* NCER */
extern int ncer_draw_cell_t(struct NCER *self, int index, struct NCGR *ncgr, struct image *image, struct coords frame_offset, fx16 transform[4]);
extern int ncer_draw_cell(struct NCER *self, int index, struct NCGR *ncgr, struct image *image, struct coords frame_offset);
extern int ncer_draw_boxes(struct NCER *self, int index, struct image *image, struct coords offset);
extern int ncer_get_cell_count(struct NCER *self);
extern int ncer_get_cell_dim(struct NCER *self, int index, struct dim *dim, struct coords *center);
void ncer_dump(struct NCER *self, FILE *fp);


/* NANR */
extern int nanr_draw_frame(struct NANR *self, int acell_index, int frame_index,
                           struct NCER *ncer, struct NCGR *ncgr,
                           struct image *image, struct coords frame_offset);
extern int nanr_get_cell_count(struct NANR *nanr);
extern int nanr_get_frame_count(struct NANR *nanr, int acell_index);
extern int nanr_get_frame_at_tick(struct NANR *nanr, int acell_index, u16 tick);


/* NMCR */
extern int nmcr_draw(struct NMCR *self, int index, int tick,
                     struct NANR *nanr, struct NCER *ncer, struct NCGR *ncgr,
                     struct image *image, struct coords offset);


/* NMAR */
extern int nmar_get_cell_count(struct NMAR *self);
extern int nmar_get_period(struct NMAR *self, int acell_index);
extern int nmar_draw_frame(struct NMAR *self, int acell_index, int frame_index, int tick,
                           struct NMCR *nmcr, struct NANR *nanr, struct NCER *ncer, struct NCGR *ncgr,
                           struct image *image, struct coords offset);
extern int nmar_draw(struct NMAR *self, int acell_index, int tick,
                     struct NMCR *nmcr, struct NANR *nanr, struct NCER *ncer, struct NCGR *ncgr,
                     struct image *image, struct coords offset);


#endif /* NITRO_H */
