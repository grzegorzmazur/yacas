#!/bin/sh

# Convert Yacas books to XML
# Usage: book2xml.sh bookname.book
# result: bookname.book.xml

ourdir=`dirname $0`

if [ x"$1" = x-run ]; then
	shift
	yacas="$1"
	shift
else
	yacas="$ourdir/../src/yacas --rootdir $ourdir/../scripts/"
fi

if [ x"$1" = x ]; then
	echo "`basename $0`: convert Yacas docs to XML"
	echo "Usage: `basename $0` [-run /path/yacas] bookname.book [outputname.xml]"
else
if [ x"$2" = x ]; then
	target="$1.xml"
else
	target="$2"
fi

if [ -r "$1" ]; then
	rm -f "$target"
	echo 'ToFile("'"$target"'") [ Use("'"$ourdir"'/book2xml.ys"); Load("'"$1"'"); XMLFinishUp(); ];' | $yacas -f
	if [ -s "$target" ]; then
		echo "File '$target' was created."
		# replace right double quotes by left ones where possible
		# avoid {verbatim}; do not replace "" inside {}
		#perl -e '$out_of_verbatim=1; while(<>) { if($out_of_verbatim) { 1 while s/^([^{}|]*(?:\{[^{}]*\}[^{}|]*|\\verb\|[^|]*\|[^{}|]*)*[^{}|]*[- ()|\[\]\t])"/$1``/g; s/^"/``/; if (/\\begin\{verbatim\}/) {$out_of_verbatim=0; } } elsif(/\\end\{verbatim\}/) { $out_of_verbatim=1 }; print; }' < "$target" > "$target.tmp"
		#mv "$target.tmp" "$target"
	else
		echo "`basename $0`: Some problem generating file '$target', aborted."
	fi
else
	echo "Error: cannot read input file '$1'."
fi
fi
