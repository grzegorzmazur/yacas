#!/bin/sh

# Plot a set of x-y-z datafiles using Generic Mapping Tools (GMT), create a PS file.
# Requirements: sh, expr, rm, printf, surface, gmtmath, minmax, gmtset, grdview

# Usage: yacas-gmtplot3d.sh -o psfile -grid x y [-zrange z1 z2] datafile1 datafile2 ...

# Output to psfile, input from the given files with x-y-z data (ascii format).
# Example: yacas-gmtplot3d.sh -o 1.ps /path/data1 /another/data2 data3
# Without the -zrange option the range will be set automatically.
# The -grid option is mandatory and gives the numbers of points in the grid in the x and y directions


# The directory with the ps file should be writable.

# check that the -o option is given and that psfile is given
if [ x"$1" = x-o -a x"$2" != x ]
then
	psfile="$2"
	shift
	shift
else
	echo "Error: option '-o psfile' not given"
	exit
fi

# base name for temp files
tmpfile="$psfile".gmtplot$$.tmp

# name of script file
scriptfile="$tmpfile".sh
minmaxfile="$tmpfile".minmax

# check that the -grid option is given
if [ x"$1" = x-grid -a x"$3" != x ]
then
	shift
	xgrid="$1"
	ygrid="$2"
	shift
	shift
else
	echo "Error: option '-grid x y' not given"
	exit
fi

# check if zrange options are given
if [ x"$1" = x-zrange -a x"$2" != x ]
then
	shift
	zlimits="$1 $2"
	shift
	shift
else
	zlimits=""
fi
# process minmax data, generate combined limits from all data files
minmax -C $* | cut -f2-7 | minmax -C > "$minmaxfile"
xlimits=`cut -f2,5 "$minmaxfile"`
ylimits=`cut -f6,9 "$minmaxfile"`
[ x"$zlimits" = x ] && zlimits=`cut -f10,13 "$minmaxfile"`

# select annotation, 2 per decimal interval
xticks=`gmtmath -Q $xlimits - ABS LOG10 FLOOR 10 EXCH POW 2 / =`
yticks=`gmtmath -Q $ylimits - ABS LOG10 FLOOR 10 EXCH POW 2 / =`;
zticks=`gmtmath -Q $zlimits - ABS LOG10 FLOOR 10 EXCH POW 2 / =`;
# 5 ticks per annotation
xframe=`gmtmath -Q $xticks 5 / =`
yframe=`gmtmath -Q $yticks 5 / =`
zframe=`gmtmath -Q $zticks 5 / =`
# Main options: R, B, J
rxyvalue=-R`printf %f/%f/%f/%f $xlimits $ylimits`
rvalue="$rxyvalue"`printf /%f/%f $zlimits`
bvalue=-Ba${xticks}f${xframe}/a${yticks}f${yframe}/a${zticks}f${zframe}SWNEZ+	# SW means south and west boundaries drawn and annotated
jvalue="-JX4i/3i -JZ3i"	# linear projection, 4 by 3 inches
evalue="-E120/30"

# start writing the script

echo "#!/bin/sh
# script to produce plots using gmt
rvalue='$rvalue'
bvalue='$bvalue'
jvalue='$jvalue'
options=\"$rvalue $bvalue $jvalue $evalue -P\"
gmtset PAPER_MEDIA A4+
psbasemap \$options -K > '$psfile'
"> "$scriptfile"

# loop over given data files, gather their names, write the script
# different graphs will have different pen color
pen=0
overlay="-O"	# All ps output after the first one are overlays
width=0	# in points

while [ x"$1" != x ]
do
	file="$1"
	[ ! -r "$file" ] && echo "Warning: cannot read file '$file'"
	# do not say -K for the last file
	if [ x"$2" != x ]
	then
		contoption=-K
	else
		contoption=""
	fi
	# calculate pen color
	xpen=`expr $pen / 3`
	# generate the grid position x between 0 and 255
	x=0
	factor=256
	while [ $xpen -gt 0 ]
	do
		factor=`expr $factor / 2`
		x=`expr $x + $factor \* \( $xpen % 2 \)`
		xpen=`expr $xpen / 2`
	done
	# generate color using the bisection RGB scheme that maps integers n = 0, 1, ... to floats 0<=x<1 using the formula
	# x = Sum(k,0,Infinity, 2^(-k-1)*Mod(Floor(n/2^k), 2)

	case `expr $pen % 3` in
	0) # blue+red
		rcolor=`expr 255 - $x`
		gcolor=0
		bcolor=$x
	;;
	1) # green+blue
		rcolor=0
		gcolor=$x
		bcolor=`expr 255 - $x`
	;;
	2) # red+green
		rcolor=$x
		gcolor=`expr 255 - $x`
		bcolor=0
	;;
	esac
	# prepare grid values
	grdfile="$file".grd
	gridspacing=-I`gmtmath -Q $xlimits - ABS $xgrid / =`/`gmtmath -Q $ylimits - ABS $ygrid / =`
	# prepare color table
	cptfile="$file".cpt
	# interpolate between the current pen color and greyish white
	printf "%f 192 192 192 %f $rcolor $gcolor $bcolor" $zlimits > "$cptfile"
	# print the command to the script file
# -Qsm means with grid lines, -Qs without grid lines
	echo "
blockmean $gridspacing $rxyvalue "$file" | surface $gridspacing $rxyvalue -T0.25 -G$grdfile
grdview -Qs -C"$cptfile" -S -Wm${width}p/${rcolor}/$gcolor/$bcolor \$rvalue \$jvalue $evalue '$grdfile' $overlay $contoption -P >> '$psfile'
rm -f '$grdfile'
" >> "$scriptfile"
	pen=`expr $pen + 1`
	overlay="-O"	# All ps output after the first one are overlays
	shift
done

echo "echo Produced '$psfile'" >> "$scriptfile"
sh "$scriptfile"

# clean up: retain sh file for posterity
rm -f "$minmaxfile"
