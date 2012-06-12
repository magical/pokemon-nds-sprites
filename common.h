/* common.h - Common macro, type, structure, and function declarations.
 *
 * Copyright © 2011 magical
 *
 * This file is part of spriterip; it is licensed under the GNU GPLv3
 * and comes with NO WARRANTY. See rip.c for details.
 */
#ifndef COMMON_H
#define COMMON_H

#include <stdlib.h> /* NULL, size_t, calloc, free, malloc */
#include <stdio.h> /* fread */
#include <stdint.h> /* int16_t, uint8_t, uint16_t, uint32_t */

/* assert() is part of the exported API of common.h */
#include <assert.h> /* assert */

#ifdef _WIN32
# define fseeko fseek
# define ftello ftell
# define off_t int
#endif

// Helpful macros

#define UNUSED(x) ((void)x)

// ALLOC and CALLOC fill the size parameter automatically and
// set the variable to the returned value.
// FREE sets the variable to NULL
#define ALLOC(x) ((x) = malloc(sizeof(*(x))))
#define CALLOC(x, nmemb) ((x) = calloc(nmemb, sizeof(*(x))))
#define FREE(x) (free(x), ((x) = NULL))

// FREAD sets the size parameter automatically, and also moves
// the file pointer to the front
#define FREAD(fp, x, nmemb) (fread(x, sizeof(*(x)), nmemb, fp))

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;

typedef int16_t s16;
typedef int32_t s32;

typedef s16 v16; // 1.3.12 fixed-point
typedef s16 fx16; // 1.7.8 fixed-point

enum status {
	OKAY = 0,
	FAIL,
	ABORT,
	NOMEM,
};

/******************************************************************************/

/* A buffer is a sized 1-dimensional array of bytes. */
struct buffer {
	size_t size;
	u8 data[];
};

/* dim - dimension - a height and width. In other words, a 2d size. */
struct dim {
	int height;
	int width;
};

struct coords {
	int x;
	int y;
};

/* a color */
struct rgba {
	u8 r;
	u8 g;
	u8 b;
	u8 a;
};

struct palette {
	int bit_depth;
	int count;
	struct rgba *colors;
};


/******************************************************************************/

extern void warn(const char *s, ...);

extern struct buffer *buffer_alloc(size_t size);

/* There is no buffer_free() - just use free(). */

#endif /* COMMON_H */
