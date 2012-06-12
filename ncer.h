/*
 * Copyright © 2011 magical
 *
 * This file is part of spriterip; it is licensed under the GNU GPLv3
 * and comes with NO WARRANTY. See rip.c for details.
 */
#ifndef NCER_H
#define NCER_H

#include "nitro.h" /* struct format_info */
#include "image.h" /* struct image */
#include "common.h" /* struct coords, fx16 */

/* declare the NCER structure */
struct NCER;

extern struct format_info NCER_format;

/* forward declaration of NCGR */
struct NCGR;

extern int ncer_draw_cell_t(struct NCER *self, int index, struct NCGR *ncgr, struct image *image, struct coords frame_offset, fx16 transform[4]);
extern int ncer_draw_cell(struct NCER *self, int index, struct NCGR *ncgr, struct image *image, struct coords frame_offset);
extern int ncer_draw_boxes(struct NCER *self, int index, struct image *image, struct coords offset);
extern int ncer_get_cell_count(struct NCER *self);
extern int ncer_get_cell_dim(struct NCER *self, int index, struct dim *dim, struct coords *center);
void ncer_dump(struct NCER *self, FILE *fp);

#endif /* NCER_H */
