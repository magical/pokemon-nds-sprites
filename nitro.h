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

#include "common.h" /* struct dim, u8, u16, u32, s16 */

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

struct format_info {
	magic_t magic;

	size_t size;
	void *initializer;

	int (*init)(void *);
	int (*read)(void *, FILE *);
	void (*free)(void *);
};

//extern const struct format_info * const formats[];

#define format_header(magic_, type) \
	.magic = magic_, \
	.size = sizeof (type), \
	.initializer = &(type){}

/* Public functions */

extern const struct format_info *format_lookup(magic_t magic);
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

#endif /* NITRO_H */
