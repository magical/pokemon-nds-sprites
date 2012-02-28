#include <stdbool.h>
#include <string.h> /* memset */

#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>

#include "nitro.h"
#include "narc.h"
#include "ncgr.h"
#include "nclr.h"
#include "ncer.h"
#include "nanr.h"
#include "nmar.h"
#include "nmcr.h"
#include "image.h"

static struct dim anim_dim = {.width = 192, .height = 128};
static struct coords anim_offset = {.x = 96, .y = 112};

struct anim_state {
	struct NCGR *ncgr;
	struct NCLR *nclr;
	struct NCER *ncer;
	struct NANR *nanr;
	struct NMCR *nmcr;
	struct NMAR *nmar;

	struct image image;

	int tick;
	int period;
};

static struct {
	struct NARC *narc;

	GtkWidget *window;
	GtkWidget *img;
	cairo_surface_t *surface;

	guint timer;


	int n;
	bool shiny;

	struct anim_state anim;
} state;

struct NARC *
narc_open(const char *path)
{
	FILE *fp = fopen(path, "rb");
	if (fp == NULL) {
		return NULL;
	}

	struct NARC *narc = nitro_read(fp, 0);
	if (narc == NULL) {
		fclose(fp);
		return NULL;
	}

	if (nitro_get_magic(narc) != (magic_t)'CRAN') {
		nitro_free(narc);
		fclose(fp);
		return NULL;
	}

	return narc;
}

void *
load_file(const char *path, magic_t magic)
{
	FILE *fp = fopen(path, "rb");
	if (fp == NULL) {
		return NULL;
	}

	void *nitro = nitro_read(fp, 0);
	if (nitro == NULL) {
		fclose(fp);
		return NULL;
	}

	if (nitro_get_magic(nitro) != magic) {
		nitro_free(nitro);
		fclose(fp);
		return NULL;
	}

	fclose(fp);
	return nitro;
}

void image_to_surface(struct image *image, cairo_surface_t *surface)
{
	struct rgba rgba;
	int p;
	int i;
	uint32_t c;
	int a, r, g, b;
	uint32_t *data = (uint32_t *)cairo_image_surface_get_data(surface);

	for (i = 0; i < image->pixels->size; i++) {
		p = image->pixels->data[i]; 
		rgba = image->palette->colors[p];
		if (rgba.a == 0) {
			// transparent
			data[i] = 0;
		} else if (rgba.a == 0x1f) {
			// opaque
			c = (((0xff             )     ) << 24)
			  | (((rgba.r * 255 + 15) / 31) << 16)
			  | (((rgba.g * 255 + 15) / 31) << 8)
			  | (((rgba.b * 255 + 15) / 31)     );

			data[i] = c;
		} else {
			a = (rgba.a * 255 + 15) / 31;
			r = (rgba.r * 255 + 15) / 31;
			g = (rgba.g * 255 + 15) / 31;
			b = (rgba.b * 255 + 15) / 31;

			r = (r * a + 0x7f) / 255;
			g = (g * a + 0x7f) / 255;
			b = (b * a + 0x7f) / 255;

			c = (a << 24)
			  | (r << 16)
			  | (g << 8)
			  | (b << 0);

			data[i] = c;
		}
	}
	cairo_surface_mark_dirty(surface);
}

void set_image(cairo_surface_t *new_image)
{
	int width, height;

	cairo_surface_t *old_image;

	if (new_image != NULL) {
		old_image = state.surface;
		state.surface = cairo_surface_reference(new_image);
		cairo_surface_destroy(old_image);

		width = cairo_image_surface_get_width(new_image);
		height = cairo_image_surface_get_height(new_image);
		gtk_widget_set_size_request(state.img, width, height);

		gtk_widget_queue_draw(state.img);
	}
}

void load_image(int n, bool shiny)
{
	struct NCGR *ncgr;
	struct NCLR *nclr;
	struct NCER *ncer;

	cairo_surface_t *surface;
	struct image image;
	struct coords offset = {0,0};

	ncgr = narc_load_file(state.narc, n * 20);
	nclr = narc_load_file(state.narc, n * 20 + (shiny ? 19 : 18));
	ncer = load_file("bw-pokemon.ncer", (magic_t)'NCER');

	ncgr_get_dim(ncgr, &image.dim);
	//image.pixels = ncgr_get_pixels(ncgr);
	image.palette = nclr_get_palette(nclr, 0);

	image.pixels = buffer_alloc(image.dim.height * image.dim.width);
	ncer_draw_cell(ncer, 0, ncgr, &image, offset);

	surface = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, image.dim.width, image.dim.height);
	image_to_surface(&image, surface);
	set_image(surface);
	cairo_surface_destroy(surface);

	free(image.pixels);
	free(image.palette->colors);
	free(image.palette);
	nitro_free(ncer);
	nitro_free(nclr);
	nitro_free(ncgr);
}

void load_anim(struct anim_state *anim, struct NARC *narc, int n)
{
	int base = n * 20;
	anim->ncgr = narc_load_file(narc, base + 2);
	anim->nclr = narc_load_file(narc, base + 18);
	anim->ncer = narc_load_file(narc, base + 4);
	anim->nanr = narc_load_file(narc, base + 5);
	anim->nmcr = narc_load_file(narc, base + 6);
	anim->nmar = narc_load_file(narc, base + 7);

	anim->image.dim = anim_dim;
	anim->image.pixels = buffer_alloc(anim_dim.width * anim_dim.height);
	anim->image.palette = nclr_get_palette(anim->nclr, 0);

	anim->tick = 0;
	anim->period = nmar_get_period(anim->nmar, 0);
}

void close_anim(struct anim_state *anim)
{
	nitro_free(anim->ncgr);
	nitro_free(anim->nclr);
	nitro_free(anim->ncer);
	nitro_free(anim->nanr);
	nitro_free(anim->nmcr);
	nitro_free(anim->nmar);

	if (anim->image.palette) {
		free(anim->image.palette->colors);
	}
	free(anim->image.palette);
	free(anim->image.pixels);
}

void render_anim(struct anim_state *anim, cairo_surface_t *surface)
{
	memset(anim->image.pixels->data, 0, anim->image.pixels->size);

	nmar_draw(anim->nmar, 0, anim->tick, anim->nmcr, anim->nanr, anim->ncer, anim->ncgr, &anim->image, anim_offset);

	image_to_surface(&anim->image, surface);
}

void reload_image(void)
{
	cairo_surface_t *surface;

	surface = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, anim_dim.width, anim_dim.height);
	render_anim(&state.anim, surface);
	set_image(surface);
	cairo_surface_destroy(surface);
}

gboolean timer_callback(gpointer data)
{
	state.anim.tick = (state.anim.tick + 1) % state.anim.period;
	reload_image();
	return TRUE; // keep timer
}

void set_n(int n)
{
	if (n != state.n) {
		state.n = n;
		close_anim(&state.anim);
		load_anim(&state.anim, state.narc, n);
		if (state.timer) {
			g_source_remove(state.timer);
		}
		state.timer = g_timeout_add(1000/60, timer_callback, NULL);
		reload_image();

		char title[10];
		sprintf(title, "view: %03d", n);
		gtk_window_set_title(GTK_WINDOW(state.window), title);
	}
}

gboolean image_expose_event(GtkWidget *widget, GdkEventExpose *event, gpointer data)
{
	cairo_t *cr;

	cr = gdk_cairo_create(widget->window);

	cairo_set_source_surface(cr, state.surface, 0, 0);
	cairo_paint(cr);

	cairo_destroy(cr);

	return FALSE;
}


GtkWidget *image_widget(void)
{
	GtkWidget *da;

	da = gtk_drawing_area_new();
	g_signal_connect(da, "expose-event", G_CALLBACK(image_expose_event), NULL);

	return da;
}

void image_set_from_png(GtkWidget *da, const char *path)
{
	set_image(cairo_image_surface_create_from_png(path));
}

gboolean keypress(GtkWidget *window, GdkEventKey *event, gpointer data)
{
	//fprintf(stderr, "Keypress: %x\n", event->keyval);
	if (event->keyval == GDK_KEY_Up) {
		set_n(state.n + 1);
		return TRUE;
	} else if (event->keyval == GDK_KEY_Down) {
		set_n(state.n - 1);
		return TRUE;
	} else if (event->keyval == GDK_KEY_Page_Up) {
		set_n(state.n + 10);
		return TRUE;
	} else if (event->keyval == GDK_KEY_Page_Down) {
		set_n(state.n - 10);
		return TRUE;
	} else if (event->keyval == GDK_KEY_q) {
		gtk_main_quit();
		return TRUE;
	} else if (event->keyval == GDK_KEY_s) {
		state.shiny = !state.shiny;
		reload_image();
		return TRUE;
	}
	return FALSE;
}

int main(int argc, char *argv[])
{
	GtkWidget *window;

	state.n = 0;

	gtk_init(&argc, &argv);

	if (argc < 2) {
		state.narc = narc_open("pokegra-w.narc");
	} else {
		state.narc = narc_open(argv[1]);
	}
	if (state.narc == NULL) {
		fprintf(stderr, "Unable to open NARC.");
		return 1;
	}

	window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	state.window = window;

	g_signal_connect_swapped(G_OBJECT(window), "destroy",
	                         G_CALLBACK(gtk_main_quit), NULL);
	g_signal_connect(G_OBJECT(window), "key-press-event",
	                 G_CALLBACK(keypress), NULL);

	state.img = image_widget();
	set_n(1);

	gtk_container_add(GTK_CONTAINER(window), state.img);

	gtk_widget_show_all(window);

	gtk_main();

	return 0;
}
