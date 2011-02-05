/*
 * Copyright © 2011 magical
 *
 * This file is part of spriterip; it is licensed under the GNU GPLv3
 * and comes with NO WARRANTY. See rip.c for details.
 */
#ifndef NCGR_H
#define NCGR_H

#include "nitro.h" /* struct format_info */
#include "common.h" /* struct buffer, u8, u32 */

struct NCGR;

extern struct format_info NCGR_format;

extern int ncgr_get_dim(struct NCGR *self, struct dim *dim);
extern struct buffer *ncgr_get_pixels(struct NCGR *self);
extern struct buffer *ncgr_get_cell_pixels(struct NCGR *self, u16 tile, struct dim cell_dim);

extern void ncgr_decrypt_dp(struct NCGR *self);
extern void ncgr_decrypt_pt(struct NCGR *self);

#endif /* NCGR_H */
