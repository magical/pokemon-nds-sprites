/* image.c - Routines for writing image files
 *
 * Copyright © 2011 magical
 *
 * This file is part of spriterip; it is licensed under the GNU GPLv3
 * and comes with NO WARRANTY. See rip.c for details.
 */

#include <stdlib.h> /* NULL, size_t */
#include <stdio.h> /* FILE, feof, ferror, fprintf, fwrite */

//#include <string.h> /* memset */
#include <math.h> /* round */

#include <png.h> /* png_*, setjmp */
#include <zlib.h> /* Z_BEST_SPEED */

#include <gif_lib.h> /* GifFileType, ColorMapType, EGif* */

#include "common.h" /* OKAY, FAIL, NOMEM, assert, CALLOC, FREE, struct buffer, struct coords, struct palette, struct rgba, u8 */

#include "image.h" /* struct image */

//#include "ncgr.h"
//#include "nclr.h"


static inline int
maxval_from_bitdepth(int bit_depth)
{
	return (1 << bit_depth) - 1;
}

int
image_write_pam(struct image *self, FILE *fp)
{
	assert(self != NULL);
	assert(self->pixels != NULL);
	assert(self->palette != NULL);

	int maxval = maxval_from_bitdepth(self->palette->bit_depth);

	fprintf(fp, "P7\n");
	fprintf(fp, "WIDTH %d\n", self->dim.width);
	fprintf(fp, "HEIGHT %d\n", self->dim.height);
	fprintf(fp, "DEPTH 4\n");
	fprintf(fp, "TUPLTYPE RGB_ALPHA\n");
	fprintf(fp, "MAXVAL %d\n", maxval);
	fprintf(fp, "ENDHDR\n");

	for (size_t i = 0; i < self->pixels->size; i++) {
		/* XXX bounds check for colors[]  */
		struct rgba *color = &self->palette->colors[self->pixels->data[i]];
		fwrite(color, 1, sizeof(color), fp);
	}

	return ferror(fp) ? FAIL : OKAY;
}

int
image_write_png(struct image *self, FILE *fp)
{
	assert(self != NULL);
	assert(self->pixels != NULL);
	assert(self->palette != NULL);

	const int bit_depth = self->palette->bit_depth;

	png_bytepp row_pointers = NULL;
	png_colorp palette = NULL;
	png_color_8 sig_bit;
	png_byte trans[1] = {0};

	CALLOC(row_pointers, self->dim.height);
	CALLOC(palette, self->palette->count);

	if (row_pointers == NULL || palette == NULL) {
		FREE(row_pointers);
		FREE(palette);
		return NOMEM;
	}

	/* expand the palette */

	double factor = 255.0 / (double)maxval_from_bitdepth(bit_depth);
	for (int i = 0; i < self->palette->count; i++) {
		struct rgba *color = &self->palette->colors[i];
		palette[i].red = (int)round(color->r * factor);
		palette[i].green = (int)round(color->g * factor);
		palette[i].blue = (int)round(color->b * factor);
	}

	/* set the row pointers */

	for (int i = 0; i < self->dim.height; i++) {
		row_pointers[i] = &self->pixels->data[i * self->dim.width];
	}

	/* set the significant bits */

	sig_bit.red = bit_depth;
	sig_bit.green = bit_depth;
	sig_bit.blue = bit_depth;


	png_structp png = png_create_write_struct(
		PNG_LIBPNG_VER_STRING, NULL,  NULL, NULL);
	if (!png) {
		return NOMEM; // leak
	}

	png_infop info = png_create_info_struct(png);
	if (!info) {
		png_destroy_write_struct(&png, (png_infopp)NULL);
		return NOMEM; // leak
	}

	if (setjmp(png_jmpbuf(png))) {
		png_destroy_write_struct(&png, &info);
		return FAIL; // leak
	}

	png_init_io(png, fp);

	// We're going to recompress the images with advdef later; no sense
	// wasting time now.
	png_set_compression_level(png, Z_BEST_SPEED);

	png_set_IHDR(png, info,
		self->dim.width, self->dim.height,
		4, /* bit depth */
		PNG_COLOR_TYPE_PALETTE,
		PNG_INTERLACE_NONE,
		PNG_COMPRESSION_TYPE_DEFAULT,
		PNG_FILTER_TYPE_DEFAULT
	);

	png_set_PLTE(png, info, palette, self->palette->count);
	png_set_tRNS(png, info, trans, 1, NULL);
	png_set_sBIT(png, info, &sig_bit);

	png_set_rows(png, info, row_pointers);

	png_write_png(png, info, PNG_TRANSFORM_PACKING, NULL);

	png_destroy_write_struct(&png, &info);
	FREE(row_pointers);
	FREE(palette);
	return OKAY;
}

void print_gif_error(int err)
{
	const char *text = GifErrorString(err);
	if (text != NULL) {
		fprintf(stderr, "GIF-LIB error: %s.\n", text);
	} else {
		fprintf(stderr, "GIF-LIB undefined error %d.\n", err);
	}
}

int
image_write_gif(struct image *self, FILE *fp)
{
	int err = 0;

	assert(self != NULL);
	assert(self->pixels != NULL);
	assert(self->palette != NULL);
	assert(self->palette->colors != NULL);

	int fd = fileno(fp);
	if (fd == -1) {
		return FAIL;
	}

	ColorMapObject *colors = NULL;
	GifFileType *gif = NULL;

	// Note: this is freed by EGifCloseFile
	colors = GifMakeMapObject(self->palette->count, NULL);
	if (colors == NULL) {
		fprintf(stderr, "error allocating color map\n");
		return FAIL;
	}

	int bit_depth = self->palette->bit_depth;

	double factor = 255.0 / (double)maxval_from_bitdepth(bit_depth);
	for (int i = 0; i < self->palette->count; i++) {
		struct rgba *c = &self->palette->colors[i];
		colors->Colors[i].Red = (int)round(c->r * factor);
		colors->Colors[i].Green = (int)round(c->g * factor);
		colors->Colors[i].Blue = (int)round(c->b * factor);
	}

	gif = EGifOpenFileHandle(fd, &err);
	if (gif == NULL) {
		GifFreeMapObject(colors);
		print_gif_error(err);
		return FAIL;
	}

	// "do or die"
	#define dod(expr) if ((expr) != GIF_OK) { goto giferror; }

	dod(EGifPutScreenDesc(gif, self->dim.width, self->dim.height,
	                      bit_depth - 1, 0, colors));

	dod(EGifPutComment(gif, "Ripped by magical."));


	u8 ext[4] = "\x01\x00\x00\x00";
	dod(EGifPutExtension(gif, GRAPHICS_EXT_FUNC_CODE, sizeof(ext), ext));
	dod(EGifPutImageDesc(gif, 0, 0, self->dim.width, self->dim.height,
	                     0, NULL));

	for (int y = 0; y < self->dim.height; y++) {
		u8 *row = self->pixels->data + self->dim.width * y;
		dod(EGifPutLine(gif, row, self->dim.width));
	}
	if (EGifCloseFile(gif, &err) != GIF_OK) {
		print_gif_error(err);
		return FAIL;
	}
	GifFreeMapObject(colors);
	return OKAY;

	#undef dod

giferror:
	if (gif != NULL) {
		print_gif_error(gif->Error);
		if (EGifCloseFile(gif, &err) != GIF_OK) {
			print_gif_error(err);
		}
	}
	return FAIL;
}

/* Open a new gif image and return a handle.
 * The image should have a palette and a dimension. The pixels are ignored.
 * Returns NULL on failure.
 */
GifFileType *
image_gif_new(struct image *self, const char *outfile)
{
	int err = 0;

	assert(self != NULL);
	assert(outfile != NULL);
	if (self->palette == NULL || self->palette->colors == NULL) {
		return NULL;
	}

	ColorMapObject *colors = NULL;
	GifFileType *gif = NULL;

	// Note: this is freed by EGifCloseFile
	colors = GifMakeMapObject(self->palette->count, NULL);
	if (colors == NULL) {
		fprintf(stderr, "error allocating color map\n");
		return NULL;
	}

	int bit_depth = self->palette->bit_depth;

	double factor = 255.0 / (double)maxval_from_bitdepth(bit_depth);
	for (int i = 0; i < self->palette->count; i++) {
		struct rgba *c = &self->palette->colors[i];
		colors->Colors[i].Red = (int)round(c->r * factor);
		colors->Colors[i].Green = (int)round(c->g * factor);
		colors->Colors[i].Blue = (int)round(c->b * factor);
	}

	gif = EGifOpenFileName(outfile, false, &err);
	if (gif == NULL) {
		GifFreeMapObject(colors);
		print_gif_error(err);
		return NULL;
	}

	if (EGifPutScreenDesc(gif, self->dim.width, self->dim.height,
	                      bit_depth - 1, 0, colors) != GIF_OK) {
		goto giferror;
	}

	// loop forever
	char app[] = "NETSCAPE2.0";
	char data[] = "\x01\x00\x00";
	if (EGifPutExtensionLeader(gif, APPLICATION_EXT_FUNC_CODE) != GIF_OK ||
	    EGifPutExtensionBlock(gif, sizeof(app) - 1, app) != GIF_OK ||
	    EGifPutExtensionBlock(gif, sizeof(data) - 1, data) != GIF_OK ||
	    EGifPutExtensionTrailer(gif) != GIF_OK) {
		goto giferror;
	}

	return gif;

giferror:
	if (gif != NULL) {
		print_gif_error(gif->Error);
		if (EGifCloseFile(gif, &err) != GIF_OK) {
			print_gif_error(err);
		}
	}
	if (colors != NULL) {
		GifFreeMapObject(colors);
	}
	return NULL;
}

/* Add a frame, given by the image, to an open gif. */
int
image_gif_add_frame(struct image *self, GifFileType *gif, u16 delay)
{
	assert(self != NULL);
	assert(gif != NULL);
	assert(self->pixels != NULL);

	// transparency extension
	u8 ext[4] = "\x09\x00\x00\x00";
	ext[1] = delay & 0xff;
	ext[2] = (delay >> 8) & 0xff;
	if (EGifPutExtension(gif, GRAPHICS_EXT_FUNC_CODE, sizeof(ext), ext) != GIF_OK) {
		return FAIL;
	}

	if (EGifPutImageDesc(gif, 0, 0, self->dim.width, self->dim.height,
	                     0, NULL) != GIF_OK) {
		return FAIL;
	}

	for (int y = 0; y < self->dim.height; y++) {
		u8 *row = self->pixels->data + self->dim.width * y;
		if (EGifPutLine(gif, row, self->dim.width) != GIF_OK) {
			return FAIL;
		}
	}
	return OKAY;
}

/* Close a gif. Can fail. */
int
image_gif_close(GifFileType *gif)
{
	int err = 0;
	if (EGifCloseFile(gif, &err) != GIF_OK) {
		print_gif_error(err);
		return FAIL;
	}

	return OKAY;
}

int
image_draw_line(struct image *self, struct coords start, struct coords end)
{
	assert(self != NULL);
	assert(self->pixels != NULL);

	double x, y;
	int tmp;

	if (start.y > end.y) {
		tmp = end.y;
		end.y = start.y;
		start.y = tmp;
	}
	if (start.x > end.x) {
		tmp = end.x;
		end.x = start.x;
		start.x = tmp;
	}

	x = start.x;
	y = start.y;

	double inc_x, inc_y;
	if (end.y - start.y > end.x - start.x) {
		inc_x = (double)(end.x - start.x) / (end.y - start.y);
		inc_y = 1.0;
	} else {
		inc_x = 1.0;
		inc_y = (double)(end.y - start.y) / (end.x - start.x);
	}

	const int width = self->dim.width;
	u8 (*pixels)[][width] = (u8 (*)[][width])self->pixels->data;

	//warn("(%d, %d) -> (%d, %d)", (int)x, (int)y, end.x, end.y);
	while (x <= (double)end.x  && y <= (double)end.y) {
		if (x < width && y < self->dim.height) {
			(*pixels)[(int)y][(int)x] = 1;
		}
		x += inc_x;
		y += inc_y;
	}

	return OKAY;
}

int
image_draw_square(struct image *self, struct coords start, struct coords end)
{
	assert(self != NULL);
	assert(self->pixels != NULL);

	// XXX check return codes

	// top line
	image_draw_line(self, (struct coords){.x = start.x, .y = start.y},
	                      (struct coords){.x = end.x, .y = start.y});
	// bottom line
	image_draw_line(self, (struct coords){.x = start.x, .y = end.y},
	                      (struct coords){.x = end.x, .y = end.y});
	// left line
	image_draw_line(self, (struct coords){.x = start.x, .y = start.y},
	                      (struct coords){.x = start.x, .y = end.y});
	// right line
	image_draw_line(self, (struct coords){.x = end.x, .y = start.y},
	                      (struct coords){.x = end.x, .y = end.y});

	return OKAY;
}

#if 0
int
image_init(struct image *self, struct NCGR *ncgr, struct NCLR *nclr)
{
	assert(self != NULL);
	assert(ncgr != NULL);
	assert(nclr != NULL);

	const int palette_index = 0;

	struct buffer *pixels = ncgr_get_pixels(ncgr);
	struct palette *palette = nclr_get_palette(nclr, palette_index);

	if (pixels != NULL && palette != NULL) {
		memset(self, 0, sizeof(*self));
		ncgr_get_dim(ncgr, &self->dim);
		return OKAY;
	} else {
		image_free(self);
		return FAIL;
	}
}

void
image_free(struct image *self)
{
	if (self->palette != NULL) {
		FREE(self->palette->colors);
		FREE(self->palette);
	}
	FREE(self->pixels);
}
#endif
