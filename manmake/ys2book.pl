#!/usr/bin/perl

# Usage: perl ys2book.pl < file.ys > file.chapt.txt

# This will make a documentation chapter out of a Yacas script file.
# All comments /* ... */  or /// ... become doc text.
# Comments such as // ... remain code comments. All Yacas code becomes code sample code blocks.
# Multiple indentation of code blocks: TAB is converted to 2 spaces.

$inside_comment = 0;

while(<STDIN>)
{
	chomp;
	if ($inside_comment)
	{
		if (m|(.*)\*/\s*(.*)$|)	# */ comment finished, possibly some code follows
		{
			$inside_comment = 0;
			print "$1\n\t$2\n";
		}
		else	# still inside a /* */ block
		{
			s/^\s*\*//;	# remove Ayal's initial *'s
			print "$_\n";
		}
	}
	else
	{
		if (m|^\s*///+\s*(.*)$|)	# /// comment
		{
			print "$1\n";
		}
		elsif (m|^\s*/\*\s*(.*)\*/$|)	# /* comment started and finished
		{
			print "$1\n";
		}
		elsif (m|^\s*/\*\s*(.*)$|)	# /* comment started
		{
			print "$1\n";
			$inside_comment = 1;
		}
		else
		{	 # Code block: convert indentation: TAB <-- 2 spaces
			s/\t/  /g;
			# FIXME: need to wrap long lines of code

			print "\t$_\n";
		}
	}
}
