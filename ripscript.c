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

static SCM make_image(void)
{
	struct image *image = scm_gc_malloc(sizeof(struct image), "image");
	image->pixels = NULL;
	image->palette = NULL;
	image->dim = (struct dim){0,0};
	SCM_RETURN_NEWSMOB(image_tag, image);
}

static size_t free_image(SCM obj)
{
	struct image *image = (void *) SCM_SMOB_DATA(obj);
	free(image->pixels);
	if (image->palette != NULL) {
		free(image->palette->colors);
		free(image->palette);
	}
	scm_gc_free(image, sizeof(struct image),"image");
	return 0;
}

static SCM decrypt_pt(SCM obj)
{
	assert_nitro_type('NCGR', obj);
	struct NCGR *ncgr = (void *) SCM_SMOB_DATA(obj);
	ncgr_decrypt_pt(ncgr);

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

	image_write_png(image, fp);

	fclose(fp);

	scm_dynwind_end();

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
	scm_c_define_gsubr("make-image", 0, 0, 0, make_image);
	scm_c_define_gsubr("ncgr-decrypt-pt", 1, 0, 0, decrypt_pt);
	scm_c_define_gsubr("image-set-pixels-from-ncgr", 2, 0, 0, image_set_pixels_from_ncgr);
	scm_c_define_gsubr("image-set-palette-from-nclr", 2, 0, 0, image_set_palette_from_nclr);
	scm_c_define_gsubr("image-save-png", 2, 0, 0, image_save_png);

	scm_shell(argc, argv);
}

int
main(int argc, char *argv[])
{
	scm_boot_guile(argc, argv, main_callback, NULL);
	return 0;
}
