SUBDIRS = docs inprogress YacasNotebook scripts src ramscripts manualmaker colorcode plugins proteus tests manmake compile

EXTRA_DIST = yacas.sgml yacas.spec yacas.spec.in makemake mkhomepage mkmanual yacas.sh yacas_client doxy_yacas yacas.el README.beos workinprogress.ys upload.txt

install-data-local:
	-cp yacas_client $(bindir)/

test:
	cd scripts ; make test

backup:
	-rm yacas-*.tar.gz
	make dist
	ftp -i ftp.xs4all.nl < upload.txt

