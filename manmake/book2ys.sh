#!/bin/sh

# Convert Yacas books to Yacas code
# Usage: book2tys.sh bookname.chapt
# result: bookname.chapt.ys

if [ x"$1" = x ]; then
	echo "Extract code from Yacas docs"
	echo "Usage: `basename $0` bookname.chapt [outputname.ys]"
else
if [ x"$2" = x ]; then
	target="$1.ys"
else
	target="$2"
fi
ourdir=`dirname $0`
yacas="$ourdir/../src/yacas --rootdir $ourdir/../scripts/"

if [ -r "$1" ]; then
	rm -f "$target"
	echo 'ToFile("'"$target"'") [ Use("'"$ourdir"'/book2ys.ys"); Load("'"$1"'"); ];' | $yacas -f
	if [ -s "$target" ]; then
		echo "File '$target' was created."
		#perl -e 'while(<>) { s/([%])/\\$1/g; print; }' < "$target" > "$target.tmp"
		#mv "$target.tmp" "$target"
	else
		echo "book2ys: Some problem generating file '$target', aborted."
	fi
else
	echo "Error: cannot read input file '$1'."
fi
fi
