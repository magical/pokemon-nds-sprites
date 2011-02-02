
CC=clang
mingwCC=i486-mingw32-gcc

# Warnings are good! Enable all of them. -Wall -Wextra
#
# Except i have rather a lot of unused functions (TODO: command-line
# interface), so don't warn about them. -Wno-unused-function.
#
# gcc complains about my multi-character char constants (clang doesn't,
# however). -Wno-multichar
#
# Most of my structs are layed out to mirror the file format, so i can be
# lazy and use fread(). If the compiler adds padding, things will probably
# blow up. -Wpadded
# (Unfortunately, clang doesn't recognize this flag.)
warnings:=-Wall -Wextra -Wno-unused-function

ifeq (gcc,$(CC))
warnings+=-Wno-multichar -Wpadded
endif

# _POSIX_C_SOURCE>=200809 is needed for fmemopen(3)
CFLAGS=-g -O2 -std=c99 -D_POSIX_C_SOURCE=200809L $(warnings)
LDFLAGS=-lpng -lm -lz

sources=rip.c common.c lzss.c image.c nitro.c narc.c ncgr.c nclr.c ncer.c
objects=$(sources:.c=.o)

rip: $(objects)
	$(CC) -o $@ $(objects) $(CFLAGS) $(LDFLAGS)

rip.exe: $(objects)
	$(mingwCC) -o $@ $(objects) $(CFLAGS) $(LDFLAGS)

rip.o: rip.c common.h lzss.h image.h nitro.h narc.h ncgr.h nclr.h ncer.h Makefile
common.o: common.c common.h Makefile
lzss.o: lzss.c lzss.h common.h Makefile
nitro.o: nitro.c nitro.h narc.h ncgr.h nclr.h ncer.h common.h Makefile
narc.o: narc.c narc.h nitro.h common.h Makefile
ncgr.o: ncgr.c ncgr.h nitro.h common.h Makefile
nclr.o: nclr.c nclr.h nitro.h common.h Makefile
ncer.o: ncer.c ncer.h nitro.h ncgr.h image.h common.h Makefile
image.o: image.c image.h common.h

.PHONY: clean
clean:
	rm $(objects)
