

EXTRA_DIST = ABIN-grammar.txt ABIN.ys all.ys makefile.essays essays.book.txt\
	yacasinit.ys MaintenanceHowto.txt .cvsignore \
	Plugin_factory.txt funcplug.ys txt2yacasdoc.txt

all: essays

clean:
	make clean -f makefile.essays

essays:
	make -f makefile.essays

texdocs:
	make texdocs -f makefile.essays
