## Makefile.am -- Process this file with automake to produce Makefile.in

EXTRA_DIST = README.TXT makefile.examples

noinst_PROGRAMS = example1 example2 example3
example1_SOURCES = example1.c
example2_SOURCES = example2.c
example3_SOURCES = example3.c
INCLUDES = -I$(top_srcdir)/src

## We might want to link against the installed libraries, as soon as the libraries
## are indeed installed.
LDADD = -L../src -lcyacas -lyacas -lyacasplatform -l@NUMBERS_LIB_SHORT@ @LIBLTDL@

## We can only check whether "example2" works after the script files are installed.
installcheck: example2
	test `./example2` = "Cos(x);"
