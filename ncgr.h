#ifndef NCGR_H
#define NCGR_H

#include "nitro.h" /* struct format_info */
#include "common.h" /* struct buffer, u8, u32 */

struct NCGR;

extern struct format_info NCGR_format;

extern int ncgr_get_dim(struct NCGR *self, struct dim *dim);
extern struct buffer * ncgr_get_pixels(struct NCGR *self);
extern u8 ncgr_get_pixel(struct NCGR *self, int tile, u32 x, u32 y, u32 cellwidth);

extern void ncgr_decrypt_dp(struct NCGR *self);
extern void ncgr_decrypt_pt(struct NCGR *self);

#endif /* NCGR_H */
