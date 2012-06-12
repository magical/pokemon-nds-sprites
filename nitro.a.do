objs="nitro.o narc.o ncgr.o nclr.o ncer.o nanr.o nmcr.o image.o common.o lzss.o"
redo-ifchange $objs
ar rcs "$3" $objs
