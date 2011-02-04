
#include <stdlib.h> /* EXIT_FAILURE, EXIT_SUCCESS, NULL, exit */
#include <stdio.h> /* FILE, fclose, fopen, fwrite, perror, printf, sprintf */
//#include <stdarg.h> /* va_list, va_end, va_start */
#include <limits.h> /* INT_MAX */

#ifdef _WIN32
# include <direct.h> /* _mkdir */
# define mkdir(path,mode)  _mkdir(path)
#else
# include <sys/stat.h> /* mkdir */
#endif

#include <errno.h> /* EEXIST, errno */

#include "common.h" /* FREE, ... */
#include "image.h"
#include "lzss.h"
#include "nitro.h"

#include "narc.h"
#include "ncgr.h"
#include "nclr.h"
#include "ncer.h"

#define FILENAME "pokegra.narc"
#define OUTDIR "test"


#define fseeko fseek
#define ftello ftell
#define off_t int

/******************************************************************************/

static char magic_buf[MAGIC_BUF_SIZE];
#define STRMAGIC(magic) (strmagic((magic), magic_buf))

/******************************************************************************/

static struct NARC *
open_narc(const char *filename)
{
	assert(filename != NULL);

	FILE *fp = fopen(filename, "rb");
	if (fp == NULL) {
		goto error;
	}

	struct NARC *narc = nitro_read(fp, 0);

	if (narc == NULL) {
		goto error;
	}

	if (nitro_get_magic(narc) != (magic_t)'CRAN') {
		warn("Not a NARC");
		goto error;
	}

	return narc;

	error:
	if (errno) {
		perror("Failed to load NARC");
	} else {
		warn("Failed to load NARC");
	}
	exit(EXIT_FAILURE);
}


static int
list(void)
{
	struct NARC *narc;
	struct standard_header *chunk;

	narc = open_narc(FILENAME);

	u32 count = narc_get_file_count(narc);
	assert(count <= INT_MAX);
	for (int i = 0; i < (int)count; i++) {
		chunk = narc_load_file(narc, i);
		if (chunk != NULL) {
			printf("%3d %s\n", i, STRMAGIC(nitro_get_magic(chunk)));
		} else {
			printf("%3d (null)\n", i);
		}
	}

	exit(EXIT_SUCCESS);
}


static void
write_sprite(struct image *image, char *outfile)
{
	FILE *outfp = fopen(outfile, "wb");
	if (outfp != NULL) {
		if (image_write_png(image, outfp)) {
			warn("Error writing %s.", outfile);
		}
		fclose(outfp);
	} else {
		perror(outfile);
	}
}

static void
rip_sprites(void)
{
	struct NARC *narc = open_narc(FILENAME);
	char outfile[256] = "";

	const struct sprite_dirs {
		const char *normal;
		const char *shiny;
	} const dirs[] = {
		{"back/female", "back/shiny/female"},
		{"back", "back/shiny"},
		{"female", "shiny/female"},
		{"", "shiny"},
	};

	#define MKDIR(dir) \
	if (mkdir(OUTDIR "/" dir, 0755)) { \
		switch (errno) { \
		case 0: \
		case EEXIST: \
			break; \
		default: \
			perror("mkdir: " OUTDIR "/" dir); \
			exit(EXIT_FAILURE); \
		} \
	}

	MKDIR("female")
	MKDIR("shiny")
	MKDIR("shiny/female")
	MKDIR("back")
	MKDIR("back/female")
	MKDIR("back/shiny")
	MKDIR("back/shiny/female")

	struct image image = {};

	for (int n = 1; n <= 493; n++) {
		struct NCLR *normal_nclr = narc_load_file(narc, n*6 + 4);
		struct NCLR *shiny_nclr = narc_load_file(narc, n*6 + 5);

		if (normal_nclr == NULL || shiny_nclr == NULL) {
			if (errno) perror(NULL);
			else warn("Error reading palettes.");
			exit(EXIT_FAILURE);
		}

		assert(nitro_get_magic(normal_nclr) == (magic_t)'NCLR');
		assert(nitro_get_magic(shiny_nclr) == (magic_t)'NCLR');

		struct palette *normal_palette = nclr_get_palette(normal_nclr, 0);
		struct palette *shiny_palette = nclr_get_palette(shiny_nclr, 0);

		nitro_free(normal_nclr);
		nitro_free(shiny_nclr);

		FREE(normal_nclr);
		FREE(shiny_nclr);

		if (normal_palette == NULL || shiny_palette == NULL) {
			if (errno) perror(NULL);
			else warn("Error loading palettes.");
			exit(EXIT_FAILURE);
		}

		for (int i = 0; i < 4; i++) {
			const struct sprite_dirs *d = &dirs[i];

			struct NCGR *ncgr = narc_load_file(narc, n*6 + i);
			if (ncgr == NULL) {
				// this is fine
				continue;
			}

			assert(nitro_get_magic(ncgr) == (magic_t)'NCGR');
			ncgr_decrypt_pt(ncgr);

			sprintf(outfile, "%s/%s/%d.png", OUTDIR, d->normal, n);

			image.pixels = ncgr_get_pixels(ncgr);
			if (image.pixels == NULL) {
				warn("Error ripping %s.", outfile);
				continue;
			}

			ncgr_get_dim(ncgr, &image.dim);

			nitro_free(ncgr);
			FREE(ncgr);

			image.palette = normal_palette;
			write_sprite(&image, outfile);

			sprintf(outfile, "%s/%s/%d.png", OUTDIR, d->shiny, n);
			image.palette = shiny_palette;
			write_sprite(&image, outfile);

			FREE(image.pixels);
		}

		FREE(normal_palette->colors);
		FREE(shiny_palette->colors);

		FREE(normal_palette);
		FREE(shiny_palette);
	}

	printf("done\n");
	exit(EXIT_SUCCESS);
}

/* for d/p */
static void
rip_trainers(void)
{
	struct NARC *narc = open_narc(FILENAME);
	char outfile[256] = "";
	const int trainer_count = narc_get_file_count(narc) / 2;

	struct image image = {};

	for (int n = 0; n < trainer_count; n++) {
		sprintf(outfile, "%s/%d.png", OUTDIR, n);

		struct NCGR *ncgr = narc_load_file(narc, n*2 + 0);
		if (ncgr == NULL) {
			if (errno) perror(outfile);
			continue;
		}
		assert(nitro_get_magic(ncgr) == (magic_t)'NCGR');

		struct NCLR *nclr = narc_load_file(narc, n*2 + 1);
		if (nclr == NULL) {
			if (errno) perror(outfile);
			nitro_free(ncgr);
			FREE(ncgr);
			continue;
		}
		assert(nitro_get_magic(nclr) == (magic_t)'NCLR');

		image.palette = nclr_get_palette(nclr, 0);
		if (image.palette == NULL) {
			warn("Error ripping %s.", outfile);
			continue; // leak
		}

		nitro_free(nclr);
		FREE(nclr);

		ncgr_decrypt_pt(ncgr);

		image.pixels = ncgr_get_pixels(ncgr);
		if (image.pixels == NULL) {
			warn("Error ripping %s.", outfile);
			continue; // leak
		}

		ncgr_get_dim(ncgr, &image.dim);

		nitro_free(ncgr);
		FREE(ncgr);

		write_sprite(&image, outfile);

		FREE(image.pixels);
		FREE(image.palette->colors);
		FREE(image.palette);
	}

	printf("done\n");
	exit(EXIT_SUCCESS);
}

/* for hg/ss */
static void
rip_trainers2(void)
{
	struct NARC *narc = open_narc(FILENAME);
	char outfile[256] = "";
	const int trainer_count = narc_get_file_count(narc) / 5;

	MKDIR("frames");

	struct image image = {};

	for (int n = 0; n < trainer_count; n++) {
		struct NCLR *nclr = narc_load_file(narc, n*5 + 1);
		if (nclr == NULL) {
			if (errno) perror(NULL);
			continue;
		}
		assert(nitro_get_magic(nclr) == (magic_t)'NCLR');

		image.palette = nclr_get_palette(nclr, 0);
		if (image.palette == NULL) {
			if (errno) perror(NULL);
			nitro_free(nclr);
			FREE(nclr);
			continue;
		}

		nitro_free(nclr);
		FREE(nclr);

		int spriteindex;
		for (int i = 0; i < 2; i++) {
			switch (i) {
			case 0:
				spriteindex = 0;
				sprintf(outfile, "%s/frames/%d.png", OUTDIR, n);
				break;
			case 1:
				spriteindex = 4;
				sprintf(outfile, "%s/%d.png", OUTDIR, n);
				break;
			}
			puts(outfile);

			struct NCGR *ncgr = narc_load_file(narc, n*5 + spriteindex);
			if (ncgr == NULL) {
				if (errno) perror(outfile);
				continue;
			}
			assert(nitro_get_magic(ncgr) == (magic_t)'NCGR');

			if (i == 1) {
				/* pt for platinum, dp for hgss */
				ncgr_decrypt_dp(ncgr);
			}

			image.pixels = ncgr_get_pixels(ncgr);
			if (image.pixels == NULL) {
				warn("Error ripping %s.", outfile);
				nitro_free(ncgr);
				FREE(ncgr);
				continue;
			}

			ncgr_get_dim(ncgr, &image.dim);

			nitro_free(ncgr);
			FREE(ncgr);

			write_sprite(&image, outfile);

			FREE(image.pixels);
		}
		FREE(image.palette->colors);
		FREE(image.palette);
	}

	printf("done\n");
	exit(EXIT_SUCCESS);
}

#if 0
static void
dump_ncer(void)
{
	FILE *fp = fopen("venu.ncer", "rb");
	if (fp == NULL) {
		warn("Unable to open NCER");
		exit(EXIT_FAILURE);
	}

	struct NCER *ncer = nitro_read(fp, 0);
	if (ncer == NULL) {
		warn("Unable to read NCER");
		if (errno) {
			perror(NULL);
		}
	}
	assert(nitro_get_magic(ncer) == (magic_t)'NCER');

	printf("ncer.magic = %s\n", STRMAGIC(ncer->header.magic));
	printf("ncer.size = %u\n", ncer->header.size);
	printf("ncer.cebk.cell_count = %u\n", ncer->cebk.header.cell_count);
	printf("ncer.cebk.cell_type = %u\n", ncer->cebk.header.cell_type);
	printf("ncer.cebk.flags = %x\n", ncer->cebk.header.flags);


	for (int i = 0; i < (signed long)ncer->cebk.header.cell_count; i++) {
		struct CEBK_celldata *cell = &ncer->cebk.cell_data[i];
		printf("ncer.cebk.cell[%d].oam_count = %u\n", i, cell->oam_count);
		printf("ncer.cebk.cell[%d].unknown = %u\n", i, cell->unknown);
		printf("ncer.cebk.cell[%d].oam_offset = %u\n", i, cell->oam_offset);
		struct OAM *oams = (struct OAM*)((u8 *)ncer->cebk.oam_data + cell->oam_offset);

		for (int j = 0; j < cell->oam_count; j++) {
			struct OAM *oam = &oams[j];
			const struct dim *d = &obj_sizes[oam->obj_size][oam->obj_shape];

			printf("oam[%d] = {\n"
			       "\t.y = %d,\n"
			       "\t.x = %d,\n"
			       "\t.color_mode = %d,\n"
			       "\t.rs_mode = %u,\n"
			       "\t.rs_param = %u,\n"
			       "\t.obj_mode = %u,\n"
			       "\t.obj_shape = %u,\n"
			       "\t.obj_size = %u,\n"
			       "\t.tile_index = %u,\n"
			       "\t.palette_index = %u,\n"
			       "\t.dim = {.height=%d, .width=%d},\n"
			       "}\n",
				j, oam->y, oam->x,
				oam->color_mode,
				oam->rs_mode, oam->rs_param,
				oam->obj_mode, oam->obj_shape, oam->obj_size,
				oam->tile_index, oam->palette_index,
				d->height, d->width);
		}
	}

	printf("sizeof(OAM) = %u", (unsigned int) sizeof(struct OAM));

	exit(EXIT_SUCCESS);
}
#endif

static void *
open_nitro(const char *filename, magic_t magic)
{
	char magicbuf[5];

	FILE *fp = fopen(filename, "rb");
	if (fp == NULL) {
		warn("Unable to open %s", filename);
		exit(EXIT_FAILURE);
	}

	void *file = nitro_read(fp, 0);
	if (file == NULL) {
		warn("Unable to read %s", filename);
		if (errno) {
			perror(NULL);
		}
		exit(EXIT_FAILURE);
	}
	if (*(magic_t *)file != magic) {
		warn("\"%s\" is not a %s", filename, strmagic(magic, magicbuf));
		exit(EXIT_FAILURE);
	}

	//no fclose()
	return file;
}

static void
render_ncer(void)
{
	struct NCER *ncer = open_nitro("venu.ncer", 'NCER');
	struct NCGR *ncgr = open_nitro("venu-parts.ncgr", 'NCGR');
	struct NCLR *nclr = open_nitro("venu.nclr", 'NCLR');

	struct image image = {};

	image.dim = (struct dim){96, 96};
	image.pixels = buffer_alloc(96 * 96);
	image.palette = nclr_get_palette(nclr, 0);

	if (image.palette == NULL || image.pixels == NULL) {
		warn("error");
		exit(EXIT_FAILURE);
	}

	nitro_free(nclr);
	FREE(nclr);

	/*
	printf("ncer.magic = %s\n", STRMAGIC(ncer->header.magic));
	printf("ncer.size = %u\n", ncer->header.size);
	printf("ncer.cebk.cell_count = %u\n", ncer->cebk.header.cell_count);
	printf("ncer.cebk.cell_type = %u\n", ncer->cebk.header.cell_type);
	*/

	int status = OKAY;
	const int i = 2;
	struct coords offset = {.x = 48, .y = 48};

	if (ncer_draw_cell(ncer, i, ncgr, &image, offset)) {
		warn("error drawing cell %d; bailing", i);
		status = FAIL;
		goto cleanup;
	}
	/*if (ncer_draw_boxes(ncer, i, &image, offset)) {
		warn("error drawing boxes for cell %d; bailing", i);
		status = FAIL;
		goto cleanup;
	}*/

	FILE *fp = fopen("out.png", "wb");
	if (fp == NULL) {
		warn("Could not open \"out.png\"");
		status = FAIL;
		goto cleanup;
	}
	if (image_write_png(&image, fp)) {
		warn("Error writing image");
		status = FAIL;
		goto cleanup;
	}


	cleanup:

	FREE(image.pixels);
	FREE(image.palette->colors);
	FREE(image.palette);

	nitro_free(ncgr);
	nitro_free(ncer);

	FREE(ncgr);
	FREE(ncer);
}
/******************************************************************************/

int
main(int argc, char *argv[])
{
	UNUSED(argc);
	UNUSED(argv);

	list();
	//rip_sprites();
	//rip_trainers();
	//rip_trainers2();
	//dump_ncer();
	//render_ncer();
}
