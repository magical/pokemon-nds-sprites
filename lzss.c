
#include <stdlib.h> /* NULL, size_t, perror */
#include <stdio.h> /* FILE, EOF, fclose, feof, ferror, fgetc, fmemopen, fputc, fread */

#include "common.h" /* OKAY, FAIL, FREE, assert, buffer_alloc, warn, u8, u16 */

#include "lzss.h"

/* internal function */
static int
handle_code(FILE *fp, FILE *out, u8 *buf, u16 *buf_pos, size_t n, size_t *i, int mode)
{
	int c, c2, c3;
	u16 count, disp;
	if (mode == LZSS11) {
		c = fgetc(fp);
		int indicator = c >> 4;
		switch (indicator) {
		case 1:
			// 16-bit count, 12-bit disp
			c2 = fgetc(fp);
			c3 = fgetc(fp);
			count = (c & 0xf) << 12
				| c2 << 4
				| c3 >> 4;
			count += 0x111;
			disp = (c3 & 0xf) << 8;
			break;
		case 0:
			// 8-bit count, 12-bit disp
			c2 = fgetc(fp);
			count = (c & 0xf) << 4
				| c2 >> 4;
			count += 0x11;
			disp = (c2 & 0xf) << 8;
			break;
		default:
			// 4-bit count, 12-bit disp
			count = indicator;
			disp = (c & 0xf) << 8;
			count += 1;
		}
		disp |= fgetc(fp);
		disp += 1;
	} else if (mode == LZSS10) {
		// 4-bit count, 12-bit disp
		c = fgetc(fp);
		count = c >> 4;
		count += 3;

		disp = (c & 0xf) << 8;
		disp |= fgetc(fp);
		disp += 3;
	} else {
		assert(!"unknown lzss mode");
	}

	if (ferror(fp) || feof(fp)) {
		return FAIL;
	}

	assert(disp <= *i);
	assert(disp < LZSS_BUF_SIZE);

	// should be just (buf_pos - disp), but hrgh
	u16 src_i = (u16)(*buf_pos + (u16)(LZSS_BUF_SIZE - disp)) % LZSS_BUF_SIZE;
	u16 dst_i = *buf_pos;
	for (u16 j = 0; j < count && *i < n; j++, (*i)++) {
		u8 c = buf[src_i];
		buf[dst_i] = c;
		fputc(c, out);

		src_i = (src_i + 1) % LZSS_BUF_SIZE;
		dst_i = (dst_i + 1) % LZSS_BUF_SIZE;
	}
	*buf_pos = dst_i;

	return OKAY;
}

/* Decompress up to n bytes of an LZSS stream. Does not read the signature. */
/* On FAIL, some garbage will probably have been written to the output file. */
int
lzss_decompress(FILE *fp, FILE *out, const size_t n, const int mode)
{
	assert(fp != NULL);
	assert(out != NULL);

	u8 lz_buf[LZSS_BUF_SIZE];
	u16 lz_buf_pos = 0;

	assert(mode == LZSS10 || mode == LZSS11);

	size_t i = 0;
	unsigned int bitmask;
	int flags, status;
	for (;;) {
		if ((flags = fgetc(fp)) == EOF) {
			return FAIL;
		}
		for (bitmask = 0x80; bitmask != 0; bitmask >>= 1) {
			if (flags & bitmask) {
				status = handle_code(
					fp, out, lz_buf, &lz_buf_pos, n, &i, mode);
				if (status) {
					return status;
				}
			} else {
				int c = fgetc(fp);
				if (c != EOF) {
					lz_buf[lz_buf_pos] = c;
					fputc(c, out);
				}
				lz_buf_pos = (lz_buf_pos + 1) % LZSS_BUF_SIZE;
				i++;
			}

			if (n <= i) {
				goto end;
			}
		}
		if (ferror(fp) || ferror(out) || feof(fp)) {
			return FAIL;
		}
	}

	end:
	return OKAY;
}

struct buffer *
lzss_decompress_file(FILE *fp)
{
	assert(fp != NULL);

	//<read the signature and size>
	int sig;
	size_t size;
	int mode;

	fread(&sig, 1, 1, fp);
	assert(sig == 0x11 || sig == 0x10);

	fread(&size, 3, 1, fp);

	if (ferror(fp) || feof(fp)) {
		return NULL;
	}

	struct buffer *buffer = buffer_alloc(size);
	if (buffer == NULL) {
		return NULL;
	}

	FILE *out = fmemopen(buffer->data, buffer->size, "wb");
	if (out == NULL) {
		perror("fmemopen");
		return NULL;
	}

	mode = (sig == 0x11) ? LZSS11 : LZSS10;

	int status = OKAY;
	if (lzss_decompress(fp, out, size, mode)) {
		warn("lzss_decompress failed");
		status = FAIL;
	}

	if (fclose(out)) {
		perror("lzss_decompress_file: fclose");
		status = FAIL;
	}

	if (status == OKAY) {
		return buffer;
	}

	FREE(buffer);
	return NULL;
}
