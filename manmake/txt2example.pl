#!/usr/bin/perl -w

# Extract In>/Out> examples from txt documentation, create ys test scripts using Verify().
# Usage: perl txt2example.pl < file.chapt.txt > file.yts

$in_EG = 0;	# within an example block
$have_in = 0; # within an In> line
$have_out = 0; # within an Out> line
$in_text = "";	# text after In>
$out_text = ""; # text after Out>

while(<STDIN>) {
	chomp;
	# Only care about certain lines: start with *EG or *E.G., finish with a * label, each line must be either In> or Out> and TAB-indented
	# The lines labeled "test" are selected for export, all other lines are not
	if ($in_EG == 1)
	{
		if (/^\*/)
		{	# finish *EG block
			&print_test($in_text, $out_text);
			$in_EG = 0;
			$out_text = $in_text = "";
			$have_in = $have_out = 0;
		}
		if ($have_in == 1)
		{
			if (/^\tIn\>\s*(.*)$/)
			{	# continuation In>, need to trim the preceding backslash
				$in_text =~ s/\\$//;
				$in_text .= $1;
			}
			elsif (/^\tOut\>\s*(.*)$/)
			{	# Out> line started
				$out_text = $1;
				$have_in = 0;
				$have_out = 1;
			}
			elsif (/^\t\s*(.*)$/)
			{	# continuation line, need to trim the preceding backslash
				$in_text =~ s/\\$//;
				$in_text .= $1;
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
				$in_text =~ s/\\$//;
				$in_text .= $1;
			}
			elsif (/^\tIn\>\s*(.*)$/)
			{	# In> line started, print test, clear flags
				&print_test($in_text, $out_text);
				$in_text = $1;
				$out_text = "";
				$have_in = 1;
				$have_out = 0;
			}
			elsif (/^\t\s*(.*)$/)
			{	# continuation line, need to trim the preceding backslash
				$in_text =~ s/\\$//;
				$in_text .= $1;
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
		if (/^\*(EG|E\.G\.)\s\s*test/i # *EG test or *E.G. test
			or /^\*(EG|E\.G\.)/i
		)
		{
			$in_EG = 1;
		}		
	}
}

sub print_test
{
	my ($in_text, $out_text) = (@_);
	# do not print unless both are non-empty
	$out_text =~ s/;$//;	# trim the final ';' from the Out> string
	print << "EOF" unless ($in_text eq "" or $out_text eq "");
Verify($in_text, $out_text);

EOF

}


