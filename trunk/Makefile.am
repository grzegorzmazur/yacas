SUBDIRS = docs inprogress YacasNotebook scripts src ramscripts manualmaker colorcode plugins proteus

EXTRA_DIST = yacas.spec yacas.spec.in makemake mkhomepage mkmanual yacas.sh yacas_client doxy_yacas yacas.el README.beos

install-data-local:
	-cp yacas_client $(bindir)/

test:
	cd scripts ; ../src/yacas examples/tests


