SUBDIRS = docs inprogress YacasNotebook scripts src ramscripts essays manualmaker colorcode plugins proteus tests manmake compile

EXTRA_DIST = yacas.sgml yacas.spec yacas.spec.in makemake mkhomepage mkmanual \
	yacas.sh yacas_client doxy_yacas yacas.el README.beos README.Win32 \
	workinprogress.ys upload.txt readme.bin.win32.txt

install-data-local:
	-cp yacas_client $(bindir)/

test:
	cd scripts ; make test

texdocs:
	cd manualmaker ; make texdocs

backup:
	-rm yacas-*.tar.gz
	make dist
	yacas --patchload -pc docs/downloads.html.in > downloads.html
	ftp -i ftp.xs4all.nl < upload.txt

debug:
	cd src; make -f makefile.debug

# these targets will help "make checkdist" do the right thing
dvi: texdocs
installcheck: test

# This target untars the dist file and tries a VPATH configuration.  Then
# it guarantees that the distribution is self-contained by making another
# tarfile.
# Note that this is currently a cludge and conflicts with automatically generated "distcheck" target, but I don't know how to fix this.
distcheck: dist
	-rm -rf $(distdir)
	GZIP=$(GZIP_ENV) $(TAR) zxf $(distdir).tar.gz
	mkdir $(distdir)/=inst
	dc_install_base=`cd $(distdir)/=inst && pwd`; \
	cd $(distdir) \
	  && ../configure --srcdir=. --prefix=$$dc_install_base \
	  && $(MAKE) $(AM_MAKEFLAGS) \
	  && $(MAKE) $(AM_MAKEFLAGS) test \
	  && $(MAKE) $(AM_MAKEFLAGS) texdocs \
	  && $(MAKE) $(AM_MAKEFLAGS) install \
	  && $(MAKE) $(AM_MAKEFLAGS) installcheck \
	  && $(MAKE) $(AM_MAKEFLAGS) dist
	-rm -rf $(distdir)
	@banner="$(distdir).tar.gz is ready for distribution"; \
	dashes=`echo "$$banner" | sed s/./=/g`; \
	echo "$$dashes"; \
	echo "$$banner"; \
	echo "$$dashes"
	
