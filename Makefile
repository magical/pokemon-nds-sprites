
CC=clang
mingwCC=i486-mingw32-gcc

rip: rip.c
	$(CC) -std=c99 -g -o $@ $< -O2 -lpng -Wall -Wextra -Wno-unused-function -Wno-multichar

rip.exe: rip.c
	$(mingwCC) -std=c99 -g -o $@ $< -O2 -lpng -Wall -Wextra -Wno-unused-function -Wno-multichar
