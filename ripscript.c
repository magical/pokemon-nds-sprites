/*
 * Copyright © 2011 magical
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <stdlib.h>
#include <stdio.h>
#include <libguile.h>

#include "common.h"
#include "nitro.h"
#include "narc.h"
#include "ncgr.h"
#include "nclr.h"
#include "nanr.h"
#include "nmcr.h"
#include "nmar.h"
#include "image.h"

static scm_t_bits nitro_tag;
static scm_t_bits image_tag;

static SCM hello_world(void)
{
	printf("hello, world\n");
	return SCM_BOOL_F;
}

static void assert_nitro_type(magic_t magic, SCM s_nitro)
{
	scm_assert_smob_type(nitro_tag, s_nitro);
	void *nitro = (void *) SCM_SMOB_DATA(s_nitro);
	if (nitro == NULL || nitro_get_magic(nitro) != magic) {
		scm_wrong_type_arg("", SCM_ARGn, s_nitro);
	}
}

static SCM load_narc(SCM s_filename)
{
	scm_dynwind_begin(0);

	char *filename = scm_to_locale_string(s_filename);
	scm_dynwind_free(filename);

	FILE *fp = fopen(filename, "rb");
	if (fp == NULL) {
		scm_syserror("load-narc");
	}

	struct NARC *narc = nitro_read(fp, 0);
	if (narc == NULL) {
		SCM symbol = scm_from_locale_symbol("misc-error");
		scm_error(symbol, "load-narc", "Could not load narc", SCM_UNDEFINED, SCM_UNDEFINED);
	}

	scm_dynwind_end();

	SCM_RETURN_NEWSMOB(nitro_tag, narc);
}

static SCM file_count(SCM obj)
{
	assert_nitro_type('CRAN', obj);
	void *data = (void *) SCM_SMOB_DATA(obj);
	struct NARC *narc = data;

	return scm_from_int(narc_get_file_count(narc));
}

static SCM get_file_size(SCM s_narc, SCM s_n)
{
	assert_nitro_type('CRAN', s_narc);
	void *data = (void *) SCM_SMOB_DATA(s_narc);
	struct NARC *narc = data;

	int n = scm_to_int(s_n);

	return scm_from_int(narc_get_file_size(narc, n));
}

static SCM get_magic(SCM obj);

static SCM load_narc_file(SCM s_narc, SCM s_n, SCM s_type)
{
	assert_nitro_type('CRAN', s_narc);
	void *data = (void *) SCM_SMOB_DATA(s_narc);
	struct NARC *narc = data;

	int n = scm_to_int(s_n);

	void *nitro = narc_load_file(narc, n);
	if (nitro == NULL) {
		SCM s = scm_from_locale_symbol("narc-error");
		scm_error(s, "narc-load-file", "Error loading file from narc", SCM_UNDEFINED, SCM_UNDEFINED);
	}

	SCM s_nitro;
	SCM_NEWSMOB(s_nitro, nitro_tag, nitro);

	if (s_type == SCM_UNDEFINED) { }
	else if (scm_is_symbol(s_type)) {
		SCM s_magic = get_magic(s_nitro);
		if (!scm_is_eq(s_magic, s_type)) { goto error; }
	} else {
		scm_wrong_type_arg("narc-load-file", SCM_ARG3, s_type);
	}

	return s_nitro;

error:
	{
	SCM s = scm_from_locale_symbol("narc-error");
	SCM args = scm_list_1(s_type);
	scm_error(s, "narc-load-file", "Expected a ~a", args, SCM_UNDEFINED);
	}
}

static SCM get_magic(SCM obj)
{
	scm_assert_smob_type(nitro_tag, obj);
	void *data = (void *) SCM_SMOB_DATA(obj);

	magic_t magic = nitro_get_magic(data);

	char buf[MAGIC_BUF_SIZE];
	strmagic(magic, buf);

	SCM s_magic = scm_from_locale_symbol(buf);

	return s_magic;
}

static size_t free_nitro(SCM obj)
{
	void *nitro = (void *) SCM_SMOB_DATA(obj);

	nitro_free(nitro);

	return 0;
}

static SCM make_image(SCM s_dim)
{
	struct image *image = scm_gc_malloc(sizeof(struct image), "image");
	image->pixels = NULL;
	image->palette = NULL;
	image->dim = (struct dim){0,0};

	SCM s_image;
	SCM_NEWSMOB(s_image, image_tag, image);

	if (s_dim != SCM_UNDEFINED) {
		struct dim dim;
		dim.width = scm_to_int(scm_car(s_dim));
		dim.height = scm_to_int(scm_cadr(s_dim));
		struct buffer *pixels = buffer_alloc(dim.width * dim.height);
		if (pixels == NULL) {
			scm_memory_error("make-image");
		}
		image->pixels = pixels;
		image->dim = dim;
	}

	return s_image;
}

static size_t free_image(SCM obj)
{
	struct image *image = (void *) SCM_SMOB_DATA(obj);
	free(image->pixels);
	if (image->palette != NULL) {
		free(image->palette->colors);
		free(image->palette);
	}
	scm_gc_free(image, sizeof(struct image), "image");
	return 0;
}

static SCM decrypt_pt(SCM obj)
{
	assert_nitro_type('NCGR', obj);
	struct NCGR *ncgr = (void *) SCM_SMOB_DATA(obj);
	ncgr_decrypt_pt(ncgr);

	return SCM_UNSPECIFIED;
}

static SCM decrypt_dp(SCM obj)
{
	assert_nitro_type('NCGR', obj);
	struct NCGR *ncgr = (void *) SCM_SMOB_DATA(obj);
	ncgr_decrypt_dp(ncgr);

	return SCM_UNSPECIFIED;
}

static SCM image_set_pixels_from_ncgr(SCM s_image, SCM s_ncgr)
{
	scm_assert_smob_type(image_tag, s_image);
	assert_nitro_type('NCGR', s_ncgr);

	struct image *image = (void *) SCM_SMOB_DATA(s_image);
	struct NCGR *ncgr = (void *) SCM_SMOB_DATA(s_ncgr);

	struct buffer *oldpixels = image->pixels;

	struct buffer *pixels = ncgr_get_pixels(ncgr);
	if (pixels == NULL) {
		SCM s = scm_from_locale_symbol("ncgr-error");
		scm_error(s, "image-set-pixels-from-ncgr", "Error getting pixels", SCM_UNDEFINED, SCM_UNDEFINED);
	}

	image->pixels = pixels;
	free(oldpixels);

	ncgr_get_dim(ncgr, &image->dim);

	return SCM_UNSPECIFIED;
}

static SCM image_set_palette_from_nclr(SCM s_image, SCM s_nclr)
{
	scm_assert_smob_type(image_tag, s_image);
	assert_nitro_type('NCLR', s_nclr);

	struct image *image = (void *) SCM_SMOB_DATA(s_image);
	struct NCLR *nclr = (void *) SCM_SMOB_DATA(s_nclr);

	struct palette *oldpalette = image->palette;

	struct palette *palette = nclr_get_palette(nclr, 0);
	if (palette == NULL) {
		SCM s = scm_from_locale_symbol("nclr-error");
		scm_error(s, "image-set-palette-from-nclr", "Error getting palette", SCM_UNDEFINED, SCM_UNDEFINED);
	}

	image->palette = palette;

	if (oldpalette != NULL) {
		free(oldpalette->colors);
		free(oldpalette);
	}

	return SCM_UNSPECIFIED;
}

static SCM image_save_png(SCM obj, SCM s_filename)
{
	scm_assert_smob_type(image_tag, obj);
	scm_dynwind_begin(0);

	struct image *image = (void *) SCM_SMOB_DATA(obj);
	char *filename = scm_to_locale_string(s_filename);
	scm_dynwind_free(filename);

	FILE *fp = fopen(filename, "wb");
	if (fp == NULL) {
		scm_syserror("image-save-png");
	}

	if (image_write_png(image, fp)) {
		fclose(fp);
		SCM s = scm_from_locale_symbol("image-error");
		scm_error(s, "image-save-png", "error", SCM_BOOL_F, SCM_BOOL_F);
	}

	fclose(fp);

	scm_dynwind_end();

	return SCM_UNSPECIFIED;
}

static SCM image_save_gif(SCM obj, SCM s_filename)
{
	scm_assert_smob_type(image_tag, obj);
	scm_dynwind_begin(0);

	struct image *image = (void *) SCM_SMOB_DATA(obj);
	char *filename = scm_to_locale_string(s_filename);
	scm_dynwind_free(filename);

	FILE *fp = fopen(filename, "wb");
	if (fp == NULL) {
		scm_syserror("image-save-gif");
	}

	if (image_write_gif(image, fp)) {
		fclose(fp);
		SCM s = scm_from_locale_symbol("image-error");
		scm_error(s, "image-save-gif", "error", SCM_BOOL_F, SCM_BOOL_F);
	}

	fclose(fp);

	scm_dynwind_end();

	return SCM_UNSPECIFIED;
}

static void close_gif_handler(void *gif)
{
	image_gif_close(gif);
}

/* FPS is the number of ticks to increment by.
 * Callback should take a tickcount and return an image, or #f to stop the gif
 */
static SCM save_gif(SCM s_filename, SCM s_dim, SCM s_nclr, SCM s_fps, SCM callback)
{
	scm_dynwind_begin(0);
	char *filename = scm_to_locale_string(s_filename);
	scm_dynwind_free(filename);

	if (scm_is_false(scm_procedure_p(callback))) {
		scm_wrong_type_arg("save-gif", SCM_ARG2, callback);
	}

	assert_nitro_type((magic_t)'NCLR', s_nclr);

	SCM gif_error = scm_from_locale_string("gif-error");

	struct GifFileType *gif;
	do {
		struct image initial_image = {};
		struct NCLR *nclr = (void *) SCM_SMOB_DATA(s_nclr);
		initial_image.dim.width = scm_to_int(scm_car(s_dim));
		initial_image.dim.height = scm_to_int(scm_cadr(s_dim));
		initial_image.palette = nclr_get_palette(nclr, 0);
		if (initial_image.palette == NULL) {
			SCM s = scm_from_locale_string("nclr-error");
			scm_error(s, "save-gif", "error getting palette", SCM_BOOL_F, SCM_BOOL_F);
		}
		scm_dynwind_free(initial_image.palette);
		gif = image_gif_new(&initial_image, filename);
		if (gif == NULL) {
			scm_error(gif_error, "save-gif", "error creating gif", SCM_BOOL_F, scm_list_1(s_filename));
		}
		scm_dynwind_unwind_handler(close_gif_handler, gif, 0);
	} while(0);

	SCM ticks = scm_from_uint(0);
	u16 fps = scm_to_uint16(s_fps);
	do {
		SCM s_image = scm_call_1(callback, ticks);
		if (scm_is_false(s_image)) {
			break;
		}
		scm_assert_smob_type(image_tag, s_image);
		struct image *image = (void *) SCM_SMOB_DATA(s_image);
		if (image_gif_add_frame(image, gif, fps)) {
			scm_error(gif_error, "save-gif", "error adding frame", SCM_BOOL_F, scm_list_1(s_image));
		}
		ticks = scm_sum(ticks, s_fps);
	} while(1);

	if (image_gif_close(gif)) {
		scm_error(gif_error, "save-gif", "error closing gif", SCM_BOOL_F, SCM_BOOL_F);
	}

	scm_dynwind_end();

	return SCM_UNSPECIFIED;
}


static SCM nanr_draw_frame_s(SCM obj, SCM s_cell_index, SCM s_frame_index, SCM s_ncer, SCM s_ncgr, SCM s_image)
{
	assert_nitro_type(NANR_MAGIC, obj);
	assert_nitro_type('NCER', s_ncer);
	assert_nitro_type('NCGR', s_ncgr);
	scm_assert_smob_type(image_tag, s_image);

	int cell_index = scm_to_int(s_cell_index);
	int frame_index = scm_to_int(s_frame_index);

	struct NANR *nanr = (void *) SCM_SMOB_DATA(obj);
	struct NCER *ncer = (void *) SCM_SMOB_DATA(s_ncer);
	struct NCGR *ncgr = (void *) SCM_SMOB_DATA(s_ncgr);
	struct image *image = (void *) SCM_SMOB_DATA(s_image);

	struct coords offset = {50, 50};

	if (nanr_draw_frame(nanr, cell_index, frame_index, ncer, ncgr, image, offset)) {
		SCM s = scm_from_locale_symbol("misc-error");
		scm_error(s, "nanr-draw-frame", "error", SCM_BOOL_F, SCM_BOOL_F);
	}

	return SCM_UNSPECIFIED;
}

static SCM nanr_cell_count(SCM obj)
{
	assert_nitro_type(NANR_MAGIC, obj);
	struct NANR *nanr = (void *) SCM_SMOB_DATA(obj);

	return scm_from_int(nanr_get_cell_count(nanr));
}

static SCM nanr_frame_count(SCM obj, SCM s_cell_index)
{
	assert_nitro_type(NANR_MAGIC, obj);

	int cell_index = scm_to_int(s_cell_index);
	struct NANR *nanr = (void *) SCM_SMOB_DATA(obj);

	return scm_from_int(nanr_get_frame_count(nanr, cell_index));
}

static SCM nmcr_draw_cell_s(SCM obj, SCM s_cell_index, SCM s_frame_index, SCM s_nanr, SCM s_ncer, SCM s_ncgr, SCM s_image, SCM s_offset)
{
	assert_nitro_type(NMCR_MAGIC, obj);
	assert_nitro_type(NANR_MAGIC, s_nanr);
	assert_nitro_type('NCER', s_ncer);
	assert_nitro_type('NCGR', s_ncgr);
	scm_assert_smob_type(image_tag, s_image);

	int cell_index = scm_to_int(s_cell_index);
	int frame_index = scm_to_int(s_frame_index);

	struct NMCR *nmcr = (void *) SCM_SMOB_DATA(obj);
	struct NANR *nanr = (void *) SCM_SMOB_DATA(s_nanr);
	struct NCER *ncer = (void *) SCM_SMOB_DATA(s_ncer);
	struct NCGR *ncgr = (void *) SCM_SMOB_DATA(s_ncgr);
	struct image *image = (void *) SCM_SMOB_DATA(s_image);

	struct coords offset = {0, 0};
	if (s_offset != SCM_UNDEFINED) {
		offset.x = scm_to_int(scm_car(s_offset));
		offset.y = scm_to_int(scm_cadr(s_offset));
	}

	if (nmcr_draw(nmcr, cell_index, frame_index, nanr, ncer, ncgr, image, offset)) {
		SCM s = scm_from_locale_symbol("misc-error");
		scm_error(s, "nmcr-draw-cell", "error", SCM_BOOL_F, SCM_BOOL_F);
	}

	return SCM_UNSPECIFIED;
}

static SCM nmar_cell_count(SCM obj)
{
	assert_nitro_type(NMAR_MAGIC, obj);
	struct NMAR *nmar = (void *) SCM_SMOB_DATA(obj);

	return scm_from_int(nmar_get_cell_count(nmar));
}

static SCM nmar_period(SCM obj, SCM s_index)
{
	assert_nitro_type(NMAR_MAGIC, obj);
	struct NMAR *nmar = (void *) SCM_SMOB_DATA(obj);

	int index = scm_to_int(s_index);

	return scm_from_int(nmar_get_period(nmar, index));
}

static SCM nmar_draw_s(SCM obj, SCM s_cell_index, SCM s_tick, SCM s_nmcr, SCM s_nanr, SCM s_ncer, SCM s_ncgr, SCM s_image, SCM s_offset)
{
	assert_nitro_type(NMAR_MAGIC, obj);
	assert_nitro_type(NMCR_MAGIC, s_nmcr);
	assert_nitro_type(NANR_MAGIC, s_nanr);
	assert_nitro_type('NCER', s_ncer);
	assert_nitro_type('NCGR', s_ncgr);
	scm_assert_smob_type(image_tag, s_image);

	int cell_index = scm_to_int(s_cell_index);
	int tick = scm_to_int(s_tick);

	struct NMAR *nmar = (void *) SCM_SMOB_DATA(obj);
	struct NMCR *nmcr = (void *) SCM_SMOB_DATA(s_nmcr);
	struct NANR *nanr = (void *) SCM_SMOB_DATA(s_nanr);
	struct NCER *ncer = (void *) SCM_SMOB_DATA(s_ncer);
	struct NCGR *ncgr = (void *) SCM_SMOB_DATA(s_ncgr);
	struct image *image = (void *) SCM_SMOB_DATA(s_image);

	struct coords offset = {0, 0};
	if (s_offset != SCM_UNDEFINED) {
		offset.x = scm_to_int(scm_car(s_offset));
		offset.y = scm_to_int(scm_cadr(s_offset));
	}

	if (nmar_draw(nmar, cell_index, tick, nmcr, nanr, ncer, ncgr, image, offset)) {
		SCM s = scm_from_locale_symbol("misc-error");
		scm_error(s, "nmar-draw", "error", SCM_BOOL_F, SCM_BOOL_F);
	}

	return SCM_UNSPECIFIED;

}

static void
main_callback(void *data, int argc, char *argv[])
{
	(void)data;

	nitro_tag = scm_make_smob_type("nitro", 0);
	scm_set_smob_free(nitro_tag, free_nitro);

	image_tag = scm_make_smob_type("image", sizeof(struct image));
	scm_set_smob_free(image_tag, free_image);

	scm_c_define_gsubr("hello-world", 0, 0, 0, hello_world);
	scm_c_define_gsubr("load-narc", 1, 0, 0, load_narc);
	scm_c_define_gsubr("narc-file-count", 1, 0, 0, file_count);
	scm_c_define_gsubr("narc-get-file-size", 2, 0, 0, get_file_size);
	scm_c_define_gsubr("narc-load-file", 2, 1, 0, load_narc_file);
	scm_c_define_gsubr("get-magic", 1, 0, 0, get_magic);
	scm_c_define_gsubr("make-image", 0, 1, 0, make_image);
	scm_c_define_gsubr("ncgr-decrypt-pt", 1, 0, 0, decrypt_pt);
	scm_c_define_gsubr("ncgr-decrypt-dp", 1, 0, 0, decrypt_dp);
	scm_c_define_gsubr("image-set-pixels-from-ncgr", 2, 0, 0, image_set_pixels_from_ncgr);
	scm_c_define_gsubr("image-set-palette-from-nclr", 2, 0, 0, image_set_palette_from_nclr);
	scm_c_define_gsubr("image-save-png", 2, 0, 0, image_save_png);
	scm_c_define_gsubr("image-save-gif", 2, 0, 0, image_save_gif);
	scm_c_define_gsubr("save-gif", 5, 0, 0, save_gif);
	scm_c_define_gsubr("nanr-draw-frame", 6, 0, 0, nanr_draw_frame_s);
	scm_c_define_gsubr("nanr-cell-count", 1, 0, 0, nanr_cell_count);
	scm_c_define_gsubr("nanr-frame-count", 2, 0, 0, nanr_frame_count);
	scm_c_define_gsubr("nmcr-draw", 7, 1, 0, nmcr_draw_cell_s);
	scm_c_define_gsubr("nmar-cell-count", 1, 0, 0, nmar_cell_count);
	scm_c_define_gsubr("nmar-period", 2, 0, 0, nmar_period);
	scm_c_define_gsubr("nmar-draw", 8, 1, 0, nmar_draw_s);

	scm_shell(argc, argv);
}

int
main(int argc, char *argv[])
{
	scm_boot_guile(argc, argv, main_callback, NULL);
	return 0;
}
