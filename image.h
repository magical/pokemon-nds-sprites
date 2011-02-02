#ifndef IMAGE_H
#define IMAGE_H

#include <stdio.h> /* FILE */
#include "common.h" /* struct buffer, struct coords, struct dim, struct palette */

/* an indexed image */
struct image {
	struct buffer *pixels;
	struct palette *palette;
	struct dim dim;
};

extern int image_write_pam(struct image *self, FILE *fp);
extern int image_write_png(struct image *self, FILE *fp);

extern int image_draw_line(struct image *self, struct coords start, struct coords end);
extern int image_draw_square(struct image *self, struct coords start, struct coords end);

/*
extern int image_init(struct image *self, struct NCGR *ncgr, struct NCLR *nclr);
extern void image_free(struct image *);
*/

#endif /* IMAGE_H */
