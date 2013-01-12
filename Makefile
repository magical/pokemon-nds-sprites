
CC=clang
mingwCC=i486-mingw32-gcc

# Warnings are good! Enable all of them. -Wall -Wextra
#
# Except rip.c has a lot of unused functions (TODO: command-line
# interface), so don't warn about them. -Wno-unused-function.
#
# gcc complains about my multi-character char constants (clang doesn't,
# however). -Wno-multichar
#
# Most of my structs are layed out to mirror the file format so i can be
# lazy and use fread(). If the compiler adds padding, things will probably
# blow up. -Wpadded
warnings:=-Wall -Wextra -Wno-unused-function -Wpadded

ifeq (gcc,$(CC))
warnings+=-Wno-multichar
endif

# _POSIX_C_SOURCE>=200809 is needed for fmemopen(3)
CFLAGS=-g -O2 -std=c99 -D_POSIX_C_SOURCE=200809L -fno-omit-frame-pointer -fwrapv $(warnings)
LDFLAGS=-lpng -lm -lz -lgif

sources=common.c lzss.c image.c nitro.c narc.c ncgr.c nclr.c ncer.c nanr.c nmcr.c
objects=$(sources:.c=.o)

.PHONY: all
all: rip ripscript

rip: rip.o nitro.a Makefile
	$(CC) -o $@ $< nitro.a $(CFLAGS) $(LDFLAGS)

rip.exe: rip.o nitro.a Makefile
	$(mingwCC) -o $@ $< nitro.a $(CFLAGS) $(LDFLAGS)

ripscript: ripscript.o nitro.a Makefile
	$(CC) -o $@ $< nitro.a $(LDFLAGS) -lguile -pthread

nitro.a: $(objects) Makefile
	rm -f $@
	$(AR) rcs $@ $(objects)

rip.o: rip.c common.h lzss.h image.h nitro.h Makefile
ripscript.o: ripscript.c common.h image.h nitro.h Makefile
common.o: common.c common.h Makefile
lzss.o: lzss.c lzss.h common.h Makefile
nitro.o: nitro.c nitro.h common.h Makefile
narc.o: narc.c nitro.h common.h Makefile
ncgr.o: ncgr.c nitro.h common.h Makefile
nclr.o: nclr.c nitro.h common.h Makefile
ncer.o: ncer.c nitro.h image.h common.h Makefile
nanr.o: nanr.c nitro.h image.h common.h Makefile
nmcr.o: nmcr.c nitro.h image.h common.h Makefile
image.o: image.c image.h common.h Makefile

.PHONY: clean
clean:
	rm rip.o ripscript.o $(objects)
