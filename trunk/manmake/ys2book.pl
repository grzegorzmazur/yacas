#!/usr/bin/perl

# Usage: perl ys2book.pl [-strip] < file.ys > file.chapt.txt

# This will make a documentation chapter out of a Yacas script file.
# All comments /** ... */  or /// ... become doc text.
# Comments such as // ... remain code comments. All Yacas code becomes code sample code blocks unless -strip option is given.
# Multiple indentation of code blocks: TAB is converted to 2 spaces.

$inside_comment = 0;

$want_code = ("@ARGV" =~ /-strip/) ? 0 : 1;

sub print_code_line
{
# FIXME: need to wrap long lines of code
	my $text = shift;
	if ($want_code)
	{
		print $text;
	}
}

while(<STDIN>)
{
	chomp;
	if ($inside_comment)
	{
		if (m|(.*)\*/\s*(.*)$|)	# */ comment finished, possibly some code follows
		{
			$inside_comment = 0;
			print "$1\n";
			print_code_line("$2\n");
		}
		else	# still inside a /* */ block
		{
			s/^\s*\*//;	# remove Ayal's initial *'s
			print "$_\n";
		}
	}
	else
	{
		if (m|^\s*///+\s*([^\s/].*)$|)	# /// comment
		{
			print "$1\n";
		}
		elsif (m|^\s*///+\s*$|)	# empty /// comment
		{
			print "\n";
		}
		elsif (m|^\s*/\*\*\s*([^\s].*)\*/$|)	# /** comment started and finished on the same line
		{
			print "$1\n";
		}
		elsif (m|^\s*/\*\*\s*([^\s].*)$|)	# /** comment started
		{
			print "$1\n";
			$inside_comment = 1;
		}
		else
		{	 # Code block: convert indentation: TAB <-- 2 spaces
			s/\t/  /g;
			print_code_line($_);
		}
	}
}
