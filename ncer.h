#ifndef NCER_H
#define NCER_H

#include "nitro.h" /* struct format_info */
#include "image.h" /* struct image */
#include "common.h" /* struct coords */

/* declare the NCER structure */
struct NCER;

extern struct format_info NCER_format;

/* forward declaration of NCGR */
struct NCGR;

extern int ncer_draw_cell(struct NCER *self, int index, struct NCGR *ncgr, struct image *image, struct coords frame_offset);
extern int ncer_draw_boxes(struct NCER *self, int index, struct image *image, struct coords offset);

#endif /* NCER_H */
