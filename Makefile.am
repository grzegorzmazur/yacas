## Makefile.am -- Process this file with automake to produce Makefile.in

## SUBDIRS lists the directories that make will descend into.
## Note: this list needs to be synchronized with "configure.in"

## Interdependency: to get a working "yacas", the source in "src" need to be
## compiled, and "packages.ys" in "scripts" need to be generated. We need a
## working "yacas" to make the manual in "manmake", and to run the test
## suite in "tests". In addition, one of the tests to be run is generated in
## "manmake".

SUBDIRS = . docs libltdl src scripts \
	plugins manmake tests embed \
	JavaYacas 

## A script to be installed in the directory for binaries
bin_SCRIPTS = yacas_client

## These scripts are needed to support plotting with GMT
PLOTTINGSCRIPTS = yacas-gmtplot2d.sh yacas-gmtplot3d.sh

## EXTRA_DIST contains the files automake does not know about, that need to go
## in the distribution. Note that scripts are not distributed by default.

EXTRA_DIST = yacas.sgml yacas.spec yacas.spec.in makemake mkhomepage \
	yacas.sh doxy_yacas README.Win32 ltmain.sh \
	upload.txt readme.bin.win32.txt \
	makewin.bat $(bin_SCRIPTS) $(PLOTTINGSCRIPTS) conf-macosx.sh

# The target "test" is an alias for "check"
test: check

# the "distcheck" target does "make dvi", we want it to build the texdocs
dvi: texdocs

texdocs:
	cd manmake ; make ps-docs pdf-docs

# this target will create all HTML docs in case they are disabled by configure

htmldocs:
	cd manmake; make manicon.html

backup:
	-rm yacas-*.tar.gz
	make dist
	yacas --patchload -pc docs/downloads.html.in > downloads.html
	ftp -i ftp.xs4all.nl < upload.txt

## Install the plotting scripts
install-data-local: $(PLOTTINGSCRIPTS)
	$(mkinstalldirs) $(DESTDIR)$(pkgdatadir)/
	-for file in $(PLOTTINGSCRIPTS); do \
	   $(INSTALL_DATA) $(srcdir)/$$file $(DESTDIR)$(pkgdatadir)/$$file; \
	done
