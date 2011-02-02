#ifndef NITRO_H
#define NITRO_H

#include <stdio.h> /* FILE */

#include "common.h" /* struct dim, u8, u16, u32, s16 */

typedef u32 magic_t;

struct standard_header {
	magic_t magic;
	u16 bom;
	u16 version;

	u32 size;
	u16 header_size;
	u16 chunk_count;
};


// OAM = Object Attribute Memory; see GBATEK for details.
// XXX oam isn't a great name - call this "obj" instead?
// XXX add some union magic
struct OAM {
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
extern void *nitro_read(FILE *fp);
extern void nitro_free(void *chunk);

static inline magic_t
nitro_get_magic(void *chunk)
{
	if (chunk != NULL) {
		return ((struct standard_header *)chunk)->magic;
	}
	return 0;
}

#define MAGIC_BUF_SIZE 5

/* convert a magic_t to a string. you must supply a char array at least 
 * MAGIC_BUF_SIZE bytes long to hold the resulting string */
extern char *strmagic(magic_t magic, char *buf);

#endif /* NITRO_H */
