#!/bin/sh

# the compression test: compress the scripts to out.dat, and 
# decompress them to the subdirectory test/
make -f makefile.compressor

rm -rf test/

./compressor - ../scripts/ out.dat 
./decompressor - ./test/ out.dat
ls -l out.dat
