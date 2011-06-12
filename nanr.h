/*
 * Copyright © 2011 magical
 *
 * This file is part of spriterip; it is licensed under the GNU GPLv3
 * and comes with NO WARRANTY. See rip.c for details.
 */
#ifndef NANR_H
#define NANR_H

#include "nitro.h" /* struct format_info, magic_t */
#include "common.h" /* struct coords, u16 */
#include "image.h" /* struct image */
#include "ncgr.h" /* struct NCGR */
#include "ncer.h" /* struct NCER */

struct NANR;

#define NANR_MAGIC ((magic_t)'NANR')

extern struct format_info NANR_format;

extern int nanr_draw_frame(struct NANR *self, int acell_index, int frame_index,
                           struct NCER *ncer, struct NCGR *ncgr,
                           struct image *image, struct coords frame_offset);
extern int nanr_get_cell_count(struct NANR *nanr);
extern int nanr_get_frame_count(struct NANR *nanr, int acell_index);
extern int nanr_get_frame_at_tick(struct NANR *nanr, int acell_index, u16 tick);

#endif /* NANR_H */
