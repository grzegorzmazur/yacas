#!/bin/sh

# Convert Yacas books to TeX
# Usage: book2txt.sh bookname.chapt
# result: bookname.chapt.tex

if [ x"$1" = x ]; then
	echo "Convert Yacas docs to TeX"
	echo "Usage: `basename $0` bookname.chapt"
else
ourdir=`dirname $0`
if [ -r "$1" ]; then
	rm -f "$1.tex"
	echo 'ToFile("'"$1"'.tex") [ Use("'"$ourdir"'/book2TeX.ys"); Load("'"$1"'"); TeXFinishUp(); ];' | yacas -f
	if [ -s "$1.tex" ]; then
		echo "File '$1.tex' was created."
		#perl -e 'while(<>) { s/([%])/\\$1/g; s/\{\\(?:text)?tt\{(.*[\\#&\$^_].*)\}\}/\\verb|$1|/g; print; }' < "$1.tex" > "$1.tex.tmp"
		#mv "$1.tex.tmp" "$1.tex"
	else
		echo "book2TeX: Some problem generating file '$1.tex', aborted."
	fi
else
	echo "Error: cannot read input file '$1'."
fi
fi
