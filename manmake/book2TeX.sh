#!/bin/sh

# Convert Yacas books to TeX
# Usage: book2txt.sh bookname.chapt
# result: bookname.chapt.tex

if [ x"$1" = x ]; then
	echo "Convert Yacas docs to TeX"
	echo "Usage: `basename $0` bookname.chapt [outputname.tex]"
else
if [ x"$2" = x ]; then
	target="$1.tex"
else
	target="$2"
fi
ourdir=`dirname $0`
if [ -r "$1" ]; then
	rm -f "$target"
	echo 'ToFile("'"$target"'") [ Use("'"$ourdir"'/book2TeX.ys"); Load("'"$1"'"); TeXFinishUp(); ];' | yacas -f
	if [ -s "$target" ]; then
		echo "File '$target' was created."
		#perl -e 'while(<>) { s/([%])/\\$1/g; print; }' < "$target" > "$target.tmp"
		#mv "$target.tmp" "$target"
	else
		echo "book2TeX: Some problem generating file '$target', aborted."
	fi
else
	echo "Error: cannot read input file '$1'."
fi
fi
