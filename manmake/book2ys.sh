#!/bin/sh

# Convert Yacas books to Yacas code
# Usage: book2tys.sh bookname.chapt
# result: bookname.chapt.ys

ourdir=`dirname $0`

if [ x"$1" = x-run ]; then
	shift
	yacas="$1"
	shift
else
	yacas="$ourdir/../src/yacas --rootdir $ourdir/../scripts/"
fi

if [ x"$1" = x ]; then
	echo "`basename $0`: extract code from Yacas docs"
	echo "Usage: `basename $0` [-run /path/yacas] bookname.chapt [outputname.ys]"
else
if [ x"$2" = x ]; then
	target="$1.ys"
else
	target="$2"
fi

if [ -r "$1" ]; then
	rm -f "$target"
	echo 'ToFile("'"$target"'") [ Use("'"$ourdir"'/book2ys.ys"); Load("'"$1"'"); ];' | $yacas -f
	if [ -s "$target" ]; then
		echo "File '$target' was created."
		#perl -e 'while(<>) { s/([%])/\\$1/g; print; }' < "$target" > "$target.tmp"
		#mv "$target.tmp" "$target"
	else
		echo "`basename $0`: Some problem generating file '$target', aborted."
	fi
else
	echo "Error: cannot read input file '$1'."
fi
fi
