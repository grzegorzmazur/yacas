#!/bin/sh

# Convert Yacas books to text
# Usage: book2txt.sh bookname.chapt
# result: bookname.chapt.txt

if [ x"$1" = x ]; then
	echo "Convert Yacas docs to formatted plain text"
	echo "Usage: `basename $0` bookname.chapt"
else
while [ x"$1" != x ]
do
	ourdir=`dirname $0`
	if [ -r "$1" ]; then
		rm -f "$1.txt"
		echo 'ToFile("'"$1"'.txt") [ Use("'"$ourdir"'/book2txt.ys"); Load("'"$1"'"); ];' | yacas -f
		if [ -s "$1.txt" ]; then
			echo "File '$1.txt' was created."
			perl -e 'undef $/; $_=<>; s/<br>/\n/gi; s/<p>/\n/gi; s/<\/p>//gi; s/ +\n/\n/g; s/\n\n\n+/\n\n/g; s/\n ([^ ])/\n$1/g; print;' < "$1.txt" > "$1.txt.tmp"
			mv "$1.txt.tmp" "$1.txt"
		else
			echo "book2txt: Some problem generating file '$1.txt', aborted."
		fi
	else
		echo "Error: cannot read input file '$1'."
	fi
	shift
done
fi
