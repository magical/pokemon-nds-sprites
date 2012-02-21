redo-ifchange nitro.a
clang -o "$3" view.c nitro.a $(pkg-config --cflags --libs gtk+-2.0) -lm -lpng -lgif
