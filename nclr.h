/*
 * Copyright © 2011 magical
 *
 * This file is part of spriterip; it is licensed under the GNU GPLv3
 * and comes with NO WARRANTY. See rip.c for details.
 */
#ifndef NCLR_H
#define NCLR_H

#include "nitro.h" /* struct format_info */
#include "common.h" /* struct palette */

struct NCLR;

extern struct format_info NCLR_format;

extern struct palette *nclr_get_palette(struct NCLR *self, int index);

#endif /* NCLR_H */
