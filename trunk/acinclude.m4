dnl Read in the libtool macros, which are distributed together with
dnl Yacas. It is mentioned in the libtool documentation that it is a good
dnl idea to do so, as otherwise we would distribute a ltconfig and
dnl ltmain.sh from a certain version, but a libtool.m4 from the local
dnl machine would be used, which might have a different version.

include(./libtool.m4)
