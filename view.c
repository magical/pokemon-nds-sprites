#include <stdbool.h>

#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>

#include "nitro.h"
#include "narc.h"
#include "ncgr.h"
#include "nclr.h"
#include "ncer.h"
#include "image.h"

static GtkWidget *img_widget = NULL;
static struct NARC *narc = NULL;
static cairo_surface_t *image = NULL;
static int image_n = 1;
static bool image_shiny = false;

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
	uint32_t *data = (uint32_t *)cairo_image_surface_get_data(surface);

	for (i = 0; i < image->pixels->size; i++) {
		p = image->pixels->data[i]; 
		if (p == 0) {
			// transparent
		} else {
			rgba = image->palette->colors[p];

			c = (((0xff             )     ) << 24)
			  | (((rgba.r * 255 + 15) / 31) << 16)
			  | (((rgba.g * 255 + 15) / 31) << 8)
			  | (((rgba.b * 255 + 15) / 31)     );

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
		old_image = image;
		image = cairo_surface_reference(new_image);
		cairo_surface_destroy(old_image);

		width = cairo_image_surface_get_width(new_image);
		height = cairo_image_surface_get_height(new_image);
		gtk_widget_set_size_request(img_widget, width, height);

		gtk_widget_queue_draw(img_widget);
	}
}

void load_image(int n)
{
	struct NCGR *ncgr;
	struct NCLR *nclr;
	struct NCER *ncer;

	cairo_surface_t *surface;
	struct image image;
	struct coords offset = {0,0};

	ncgr = narc_load_file(narc, n * 20);
	nclr = narc_load_file(narc, n * 20 + (image_shiny ? 19 : 18));
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

gboolean image_expose_event(GtkWidget *widget, GdkEventExpose *event, gpointer data)
{
	cairo_t *cr;

	cr = gdk_cairo_create(widget->window);

	cairo_set_source_surface(cr, image, 0, 0);
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
	image = cairo_image_surface_create_from_png(path);

	set_image(image);
}

gboolean keypress(GtkWidget *window, GdkEventKey *event, gpointer data)
{
	//fprintf(stderr, "Keypress: %x\n", event->keyval);
	if (event->keyval == GDK_KEY_Up) {
		image_n += 1;
		load_image(image_n);
		return TRUE;
	} else if (event->keyval == GDK_KEY_Down) {
		image_n -= 1;
		load_image(image_n);
		return TRUE;
	} else if (event->keyval == GDK_KEY_Page_Up) {
		image_n += 10;
		load_image(image_n);
		return TRUE;
	} else if (event->keyval == GDK_KEY_Page_Down) {
		image_n -= 10;
		load_image(image_n);
		return TRUE;
	} else if (event->keyval == GDK_KEY_q) {
		gtk_main_quit();
		return TRUE;
	} else if (event->keyval == GDK_KEY_s) {
		image_shiny = !image_shiny;
		load_image(image_n);
		return TRUE;
	}
	return FALSE;
}

int main(int argc, char *argv[])
{
	GtkWidget *window;

	gtk_init(&argc, &argv);

	if (argc < 2) {
		narc = narc_open("pokegra-w.narc");
	} else {
		narc = narc_open(argv[1]);
	}
	if (narc == NULL) {
		fprintf(stderr, "Unable to open NARC.");
		return 1;
	}

	window = gtk_window_new(GTK_WINDOW_TOPLEVEL);

	g_signal_connect_swapped(G_OBJECT(window), "destroy",
	                         G_CALLBACK(gtk_main_quit), NULL);
	g_signal_connect(G_OBJECT(window), "key-press-event",
	                 G_CALLBACK(keypress), NULL);

	img_widget = image_widget();
	//image_set_from_png(img, "/usr/share/icons/hicolor/256x256/apps/firefox.png");
	load_image(image_n);

	gtk_container_add(GTK_CONTAINER(window), img_widget);

	gtk_widget_show_all(window);

	gtk_main();

	return 0;
}
