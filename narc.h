#ifndef NARC_H
#define NARC_H

#include "nitro.h" /* struct format_info */
#include "common.h" /* u32 */

struct NARC;

extern struct format_info NARC_format;

extern void *narc_load_file(struct NARC *self, int index);
extern u32 narc_get_file_count(struct NARC *self);

#endif /* NARC_H */
