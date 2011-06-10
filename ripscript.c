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

#include "nitro.h"
#include "narc.h"

static scm_t_bits nitro_tag;


static SCM hello_world(void)
{
	printf("hello, world\n");
	return SCM_BOOL_F;
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
		scm_error(symbol, "load-narc", "Could not load narc", SCM_UNSPECIFIED, SCM_UNSPECIFIED);
	}

	scm_dynwind_end();

	SCM_RETURN_NEWSMOB(nitro_tag, narc);
}

static SCM file_count(SCM obj)
{
	scm_assert_smob_type(nitro_tag, obj);
	void *data = (void *) SCM_SMOB_DATA(obj);
	struct NARC *narc = data;

	return scm_from_int(narc_get_file_count(narc));
}

static SCM get_file_size(SCM s_narc, SCM s_n)
{
	scm_assert_smob_type(nitro_tag, s_narc);
	void *data = (void *) SCM_SMOB_DATA(s_narc);
	struct NARC *narc = data;

	int n = scm_to_int(s_n);

	return scm_from_int(narc_get_file_size(narc, n));
}

static SCM load_narc_file(SCM s_narc, SCM s_n)
{
	scm_assert_smob_type(nitro_tag, s_narc);
	void *data = (void *) SCM_SMOB_DATA(s_narc);
	struct NARC *narc = data;

	int n = scm_to_int(s_n);

	void *nitro = narc_load_file(narc, n);
	if (nitro == NULL) {
		SCM s = scm_from_locale_symbol("narc-error");
		scm_error(s, "narc-load-file", "Error loading file from narc", SCM_UNSPECIFIED, SCM_UNSPECIFIED);
	}

	SCM_RETURN_NEWSMOB(nitro_tag, nitro);
}

static SCM get_magic(SCM obj)
{
	scm_assert_smob_type(nitro_tag, obj);
	void *data = (void *) SCM_SMOB_DATA(obj);

	magic_t magic = nitro_get_magic(data);

	char buf[MAGIC_BUF_SIZE];
	strmagic(magic, buf);

	SCM s_magic = scm_from_locale_string(buf);

	return s_magic;
}

static size_t free_nitro(SCM obj)
{
	void *nitro = (void *) SCM_SMOB_DATA(obj);

	nitro_free(nitro);

	return 0;
}

static void
main_callback(void *data, int argc, char *argv[])
{
	(void)data;

	nitro_tag = scm_make_smob_type("nitro", 0);
	scm_set_smob_free(nitro_tag, free_nitro);

	scm_c_define_gsubr("hello-world", 0, 0, 0, hello_world);
	scm_c_define_gsubr("load-narc", 1, 0, 0, load_narc);
	scm_c_define_gsubr("narc-file-count", 1, 0, 0, file_count);
	scm_c_define_gsubr("narc-get-file-size", 2, 0, 0, get_file_size);
	scm_c_define_gsubr("narc-load-file", 2, 0, 0, load_narc_file);
	scm_c_define_gsubr("get-magic", 1, 0, 0, get_magic);
	scm_shell(argc, argv);
}

int
main(int argc, char *argv[])
{
	scm_boot_guile(argc, argv, main_callback, NULL);
	return 0;
}
