## Makefile.am -- Process this file with automake to produce Makefile.in

## SUBDIRS lists the directories that make will descend into.
## Note: this list needs to be synchronized with "configure.in"

## Interdependency: to get a working "yacas", the source in "src" need to be
## compiled, and "packages.ys" in "scripts" need to be generated. We need a
## working "yacas" to make the manual in "manualmaker", and to run the test
## suite in "tests". In addition, one of the tests to be run is generated in
## "manualmaker".

SUBDIRS = . docs inprogress YacasNotebook scripts src ramscripts essays \
	manualmaker colorcode plugins proteus tests manmake compile embed 

## A script to be installed in the directory for binaries
bin_SCRIPTS = yacas_client

## EXTRA_DIST contains the files automake does not know about, that need to go
## in the distribution. Note that scripts are not distributed by default.

EXTRA_DIST = yacas.sgml yacas.spec yacas.spec.in makemake mkhomepage mkmanual \
	yacas.sh doxy_yacas README.beos README.Win32 ltmain.sh libtool.m4 \
	workinprogress.ys upload.txt readme.bin.win32.txt $(bin_SCRIPTS)

# The target "test" is an alias for "check"
test: check

# the "distcheck" target does "make dvi", we want it to build the texdocs
dvi: texdocs

texdocs:
	cd manualmaker ; make ps-docs 

backup:
	-rm yacas-*.tar.gz
	make dist
	yacas --patchload -pc docs/downloads.html.in > downloads.html
	ftp -i ftp.xs4all.nl < upload.txt

debug:
	cd src; make -f makefile.debug
