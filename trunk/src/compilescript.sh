#!/bin/sh

# 
# This script is used by Ayal to compile little Yacas scripts into
# plugins. It refers to the source code directory on my computer
# because I need to be able to change the compiler and test it quickly.
# A similar script could be written that is general and can be used
# by others. It uses libtool so it should work on other systems also.
# 
# Usage:
#     compilescript.sh <name>
# will compile <name>.ys to lib<name>.cpp first, and then invoke
# libtool to compile it into a plugin. You can then load the plugin
# through 
#     Dll'Load("lib<name>");
#
# when in Yacas
#
# Ayal
# 

cat > yacas_compile_init.txt << _END
Set(LoadPlugIns,False);
Use("yacasinit.ys");
_END

cat > yacas_compile_run.txt << _END
Echo("Reading file ",FindFile("$1.ys"));
CompileCpp("$1.ys","lib$1");
_END

/Users/ayalpink/yacas/src/yacas --rootdir /Users/ayalpink/yacas/scripts/ -pc --init yacas_compile_init.txt yacas_compile_run.txt



/bin/sh /Users/ayalpink/yacas/libtool --mode=compile g++ -DHAVE_CONFIG_H -I/Users/ayalpink/yacas/src/ -I/Users/ayalpink/yacas/ -I/Users/ayalpink/yacas/src/plat/linux32    -O9 -Wall -pedantic -c -o lib$1.lo `test -f 'lib$1.cpp' || echo './'`lib$1.cpp
/bin/sh /Users/ayalpink/yacas/libtool --mode=link g++  -O9 -Wall -pedantic   -o lib$1.la -rpath /usr/local/lib/yacas  lib$1.lo  


rm yacas_compile_init.txt yacas_compile_run.txt
