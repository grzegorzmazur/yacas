SUBDIRS = docs inprogress scripts src ramscripts manualmaker colorcode plugins

EXTRA_DIST = yacas.spec yacas.spec.in makemake mkhomepage mkmanual yacas.sh yacas_client doxy_yacas yacas.el

install-data-local:
	-cp yacas_client $(bindir)/

test:
	cd scripts ; ../src/yacas examples/tests


