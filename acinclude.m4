# Read in the libtool macros, which are distributed together with
# Yacas. It is mentioned in the libtool documentation that it is a good
# idea to do so, as otherwise we would distribute a ltconfig and
# ltmain.sh from a certain version, but a libtool.m4 from the local
# machine would be used, which might have a different version.

include [./libtool.m4]