SUBDIRS = docs inprogress YacasNotebook scripts src ramscripts manualmaker colorcode plugins proteus tests

EXTRA_DIST = yacas.sgml yacas.spec yacas.spec.in makemake mkhomepage mkmanual yacas.sh yacas_client doxy_yacas yacas.el README.beos workinprogress

install-data-local:
	-cp yacas_client $(bindir)/

test:
	cd scripts ; ../src/yacas examples/tests


