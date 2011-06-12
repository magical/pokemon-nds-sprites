/*
 * Copyright © 2011 magical
 *
 * This file is part of spriterip; it is licensed under the GNU GPLv3
 * and comes with NO WARRANTY. See rip.c for details.
 */
#ifndef NMCR_H
#define NMCR_H

#include "nitro.h" /* struct format_info, magic_t */
#include "ncgr.h" /* struct NCGR */
#include "ncer.h" /* struct NCER */
#include "nanr.h" /* struct NANR */
#include "common.h" /* struct coords */
#include "image.h" /* struct image */

struct NMCR;

#define NMCR_MAGIC ((magic_t)'NMCR')

extern struct format_info NMCR_format;

extern int nmcr_draw(struct NMCR *self, int index, int tick,
                     struct NANR *nanr, struct NCER *ncer, struct NCGR *ncgr,
                     struct image *image, struct coords offset);

#endif /* NMCR_H */
