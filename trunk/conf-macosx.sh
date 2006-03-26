#!/bin/sh
# ./configure CFLAGS="-O9 -DDISABLE_DYNAMIC" CXXFLAGS="-O9 -DDISABLE_DYNAMIC" --enable-server --disable-shared
# ./configure CXXFLAGS="-O9 -DDISABLE_DYNAMIC -DNO_GLOBALS" CFLAGS="-O9 -DDISABLE_DYNAMIC -DNO_GLOBALS" --enable-server --enable-ps-doc --enable-pdf-doc --enable-archive --disable-shared  

./configure CXXFLAGS="-O9 -DDISABLE_DYNAMIC" CFLAGS="-O9 -DDISABLE_DYNAMIC" --enable-server --enable-archive --disable-shared  

# 

