#!/usr/bin/perl -w

# Extract In>/Out> examples from txt documentation, create ys test scripts using Verify().
# Usage: perl txt2example.pl file.chapt.txt > file.yts

$in_EG = 0;	# within an example block
$have_in = 0; # within an In> line
$have_out = 0; # within an Out> line
$in_text = "";	# text after In>
$out_text = ""; # text after Out>

$filename = $ARGV[0];
open(INFILE, "$filename") || die "txt2example.pl: Error: cannot open file '$filename'\n";

$line = 0;	# line number

while(<INFILE>) {
	$line++;
	chomp;
	# Only care about certain lines: start with *EG or *E.G., finish with a * label, each example line must be either In> or Out> and TAB-indented
	# The lines labeled "test" or not labeled are selected for export, all other lines are not (e.g. we can write "*E.G. notest" and it will not be tested)
	if ($in_EG == 1)
	{
		if (/^\*/)
		{	# finish *EG block
			&print_test($in_text, $out_text);
			$in_EG = 0;
			&finish_eg();
			$out_text = $in_text = "";
			$have_in = $have_out = 0;
		}
		if ($have_in == 1)
		{
			if (m/^\tIn\>\s*(.*)$/)
			{	# continuation In>, need to trim the preceding backslash
				$piece = $1;
				$in_text =~ s/\\$//;
				$in_text .= $piece;
			}
			elsif (m/^\tOut\>\s*(.*)$/)
			{	# Out> line started
				$out_text = $1;
				$have_in = 0;
				$have_out = 1;
			}
			elsif (m/^\t(\s*[^ \t].*)$/)
			{	# nonempty In> continuation line, need to trim the preceding backslash
				$piece = $1;
				$in_text =~ s/\\$//;
				$in_text .= $piece;
			}
			else
			{	# none of the above, ignore this line, clear flags, print nothing
				$have_in = 0;
				$out_text = $in_text = "";
			}
		}
		elsif ($have_out == 1)
		{
			if (/^\tOut\>\s*(.*)$/)
			{	# continuation Out>, need to trim the preceding backslash
				$piece = $1;
				$out_text =~ s/\\$//;
				$out_text .= $piece;
			}
			elsif (/^\tIn\>\s*(.*)$/)
			{	# In> line started, print test, clear flags
				&print_test($in_text, $out_text);
				$in_text = $1;
				$out_text = "";
				$have_in = 1;
				$have_out = 0;
			}
			elsif (/^\t(\s*[^ \t].*)$/)
			{	# nonempty Out> continuation line, need to trim the preceding backslash
				$piece = $1;
				$out_text =~ s/\\$//;
				$out_text .= $piece;
			}
			else
			{	# none of the above, ignore this line, clear flags, print test
				&print_test($in_text, $out_text);
				$have_out = 0;
				$out_text = $in_text = "";
			}
		}
		else	# we are outside of any in/out blocks
		{
			if (/^\tIn\>\s*(.*)$/)
			{	# In> line started, set flags
				$in_text = $1;
				$out_text = "";
				$have_in = 1;
				$have_out = 0;
			}
			else
			{	# nothing interesting, ignore this line, clear flags, print test
				$out_text = $in_text = "";
			}
		}
	}
	else
	{	# if not inside *EG: check if it is starting
		if (/^\*(EG|E\.G\.)\s\s*test\s*$/ # *EG test or *E.G. test
			or /^\*(EG|E\.G\.)\s*$/
			or /^\*TEST/
		)
		{	# starting EG block
			$in_EG = 1;
			&start_eg();
		}		
	}
}

sub print_test
{
	my ($in_text, $out_text) = (@_);
	# do not print unless both are non-empty
	$in_text =~ s/;\s*$//;	# trim the final ';' from the In> string
	$out_text =~ s/;\s*$//;	# trim the final ';' from the Out> string
	print << "EOF1" unless ($in_text eq "" or $out_text eq "");
Verify($in_text, $out_text, "at line $line in file $filename");
EOF1

}

sub start_eg	# what to print into the test file in front of each EG block
{
	print << "EOF2";
/* Testing EG block at line $line in file $filename */
Builtin'Precision'Set(10);Clear(x);Clear(y);Clear(z);Clear(a);Clear(b); Clear(A);Clear(B);Clear(v);Clear(p);
EOF2
}

sub finish_eg
{
}

