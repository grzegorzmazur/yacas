## Makefile.am -- Process this file with automake to produce Makefile.in

## EXTRA_DIST contains the files automake does not know about, 
## that need to go in the distribution.

noinst_PROGRAMS = riptestfromyart autocompleter

riptestfromyart_SOURCES = riptestfromyart.cpp

autocompleter_SOURCES = autocompleter.cpp


# These files make up the home page. 

EXTRA_DIST = \
	about.html basicindex.html \
	consoleedit.html consoleview.html consoledescr.html credits.html faq.html \
	gpl.html homepage.html links.html \
	placeholder.html plain.html recent.html recentindex.html articles.html \
	tabber.css tabber.js tutorials.html yacas.css yacas.html \
	yacas.js datahub.js yacaslogo.gif stop.gif journal.js \
	cookie.js autocompleter.css \
	tutorialtext1.html back-dark.gif back.gif back.texhtml \
	$(YART_FILES) newtonconvergence.html newdesign.html \
	range_control.html range_control.css range_control.js \
	journaldescr.html \
        downloads.html.in \
	README autocompleteraccess.js examples-static.txt \
	paper1.tex paper2.tex 

YART_FILES = newtonconvergence.yart 

## The file "yacaslogo.gif" is used in the HTML documentation

htmldir = $(scriptdir)/documentation
html_DATA = yacaslogo.gif

autocompleterarray.js: autocompleter examples-static.txt ../JavaYacas/hints.txt
	cd ../JavaYacas ; make hints.txt ; cd ../docs/
	./autocompleter ../JavaYacas/hints.txt autocompleterarray.js

check: riptestfromyart
	for file in $(YART_FILES) ; do echo "Running test code from $$file..." ; ./riptestfromyart $$file temp.yts ; ../src/yacas -pc --rootdir ../scripts temp.yts ; done

