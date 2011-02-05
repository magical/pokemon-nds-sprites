/* common.c - Definitions of common routines
 *
 * Copyright © 2011 magical
 *
 * This file is part of spriterip; it is licensed under the GNU GPLv3
 * and comes with NO WARRANTY. See rip.c for details.
 */

#include <stdlib.h>  /* size_t, stderr, malloc */
#include <stdio.h> /* fprintf, vfprintf */
#include <stdarg.h> /* va_list, va_end, va_start */
#include <string.h> /* memset */

#include "common.h"

/******************************************************************************/

void
warn(const char *s, ...)
{
	va_list va;
	va_start(va, s);
	vfprintf(stderr, s, va);
	va_end(va);
	fprintf(stderr, "\n");
}

/******************************************************************************/

struct buffer *
buffer_alloc(size_t size)
{
	struct buffer *buffer = malloc(sizeof(*buffer) + size);
	if (buffer != NULL) {
		buffer->size = size;
		memset(buffer->data, 0, buffer->size);
		return buffer;
	}
	return NULL;
}

/* There is no buffer_free() - just use free(). */

