
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
warnings=-Wall -Wextra -Wno-unused-function -Wno-multichar -Wpadded

# _POSIX_C_SOURCE>=200809 is needed for fmemopen(3)
CFLAGS=-g -O2 -std=c99 -D_POSIX_C_SOURCE=200809L $(warnings)
LDFLAGS=-lpng -lm -lz

rip: rip.c Makefile
	$(CC) -o $@ $< $(CFLAGS) $(LDFLAGS)

rip.exe: rip.c Makefile
	$(mingwCC) -o $@ $< $(CFLAGS) $(LDFLAGS)
