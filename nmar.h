/*
 * Copyright © 2011 magical
 *
 * This file is part of spriterip; it is licensed under the GNU GPLv3
 * and comes with NO WARRANTY. See rip.c for details.
 */
#ifndef NMAR_H
#define NMAR_H

#include "nitro.h" /* struct format_info, magic_t */
#include "common.h" /* struct coords, u16 */
#include "image.h" /* struct image */
#include "nmcr.h" /* struct NMCR */
#include "ncgr.h" /* struct NCGR */
#include "ncer.h" /* struct NCER */

struct NMAR;

#define NMAR_MAGIC ((magic_t)'NMAR')

extern struct format_info NMAR_format;

extern int nmar_get_cell_count(struct NMAR *self);
extern int nmar_get_period(struct NMAR *self, int acell_index);
extern int nmar_draw_frame(struct NMAR *self, int acell_index, int frame_index, int tick,
                           struct NMCR *nmcr, struct NANR *nanr, struct NCER *ncer, struct NCGR *ncgr,
                           struct image *image, struct coords offset);
extern int nmar_draw(struct NMAR *self, int acell_index, int tick,
                     struct NMCR *nmcr, struct NANR *nanr, struct NCER *ncer, struct NCGR *ncgr,
                     struct image *image, struct coords offset);

#endif /* NANR_H */
