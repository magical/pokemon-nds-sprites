/*
 * Copyright © 2011 magical
 *
 * This file is part of spriterip; it is licensed under the GNU GPLv3
 * and comes with NO WARRANTY. See rip.c for details.
 */
#ifndef NARC_H
#define NARC_H

#include "nitro.h" /* struct format_info */
#include "common.h" /* u32 */

struct NARC;

extern struct format_info NARC_format;

extern void *narc_load_file(struct NARC *self, int index);
extern u32 narc_get_file_size(struct NARC *self, int index);
extern u32 narc_get_file_count(struct NARC *self);

#endif /* NARC_H */
