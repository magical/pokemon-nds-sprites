/*
 * Copyright © 2011 magical
 *
 * This file is part of spriterip; it is licensed under the GNU GPLv3
 * and comes with NO WARRANTY. See rip.c for details.
 */
#ifndef IMAGE_H
#define IMAGE_H

#include <stdio.h> /* FILE */
#include "common.h" /* struct buffer, struct coords, struct dim, struct palette, u16 */

/* an indexed image */
struct image {
	struct buffer *pixels;
	struct palette *palette;
	struct dim dim;
};

extern int image_write_pam(struct image *self, FILE *fp);
extern int image_write_png(struct image *self, FILE *fp);
extern int image_write_gif(struct image *self, FILE *fp);

// gif animations
struct GifFileType;

extern struct GifFileType *image_gif_new(struct image *self, const char *outfile);
extern int image_gif_add_frame(struct image *self, struct GifFileType *gif, u16 delay);
extern int image_gif_close(struct GifFileType *gif);

extern int image_draw_line(struct image *self, struct coords start, struct coords end);
extern int image_draw_square(struct image *self, struct coords start, struct coords end);

/*
extern int image_init(struct image *self, struct NCGR *ncgr, struct NCLR *nclr);
extern void image_free(struct image *);
*/

#endif /* IMAGE_H */
