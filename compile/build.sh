#/bin/sh
yacas -pc cplusplusdriver compile3 doall
# c++ -c test.cc -O4 -I/root/myprojects/yacas/src -I/root/myprojects/yacas/src/plat/linux32
# strip test.o
make -f makefile.plugin
strip *.so
ls -l *.so

