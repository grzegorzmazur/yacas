#!/usr/bin/perl -w

# convert plain ascii to Yacas manual code
# Syntax of ascii files:
# Paragraphs are separated by blank lines.
# Chapter is triple Tab indented; Section is double Tab indented.
# sample code is single Tab indented (note that linebreaks in sample code
# must be also Tab indented or else you get two sample code blocks)
# Ordinary text is not indented.
# Itemized text is marked by * in the first position, followed by Tab
# Enumerated text is marked by * followed by Tab, number and period.
# Text surrounded by <i>...</i> becomes emphasized (must be on a single line!)
# Text surrounded by {} becomes fixed-width (must not contain more than four nested sets of {} inside!)
# Text surrounded by <* *> is made into hyperlinks into the manual (SeeAlso()), e.g. <*FlatCopy, Take,Length*> is translated into SeeAlso({"FlatCopy", "Take", "Length"})
# 
# Use with care, test output with manualmaker

$have_Text = 0;	# Means we have a Text() declared somewhere above
$have_par = 1;	# Means we already have a new paragraph
$in_text = 0;	# Means we are inside quotes of the Text()
$in_itemized = 0;
$in_enum = 0;
$in_htmlcommand = 0;

while (<STDIN>) {
	chomp;
	s/"/\\"/g;
	
	if (/^\t\t\t(.*)$/) {	# Chapter
		&finish_text();
		print "\tChapter()\"", &escape_term($1), "\";\n\n";
		$have_par = 1;
	} elsif (/^\t\t(.*)$/) {	# Section
		&finish_text();
		print "\tSection()\"", &escape_term($1), "\";\n\n";
		$have_par = 1;
	} elsif (/^\t(.*)$/) {	# Sample code
		if (not $in_htmlcommand) {
			&close_quote();
			print ":HtmlCommand(\"\n";
			$in_htmlcommand = 1;
		}
		print "$1\n";
		$have_par = 0;
		$in_text = 1;
	} elsif (/^\s*$/) {	# New paragraph
		if ($in_itemized or $in_enum) {
			&close_quote();
			print ")\n";
			$in_itemized = $in_enum = 0;
		}
		if (not $have_par) {
			&close_quote();
			print ":HtmlNewParagraph()\n\n";
			$have_par = 1;
		}
	} elsif (/^\*\t[0-9]+\. (.*)$/) {	# Enum
		&close_quote();
		if ($in_enum) {
			print ":Item()\"", &escape_term($1), "\n";
		} else {
			print ":Enumerate() (\nItem()\"", &escape_term($1), "\n";
			$in_enum = 1;
		}
		$in_text = 1;
		$have_par = 0;
	} elsif (/^\*\t(.*)$/) {	# Itemized
		&close_quote();
		if ($in_itemized) {
			print ":Item()\"", &escape_term($1), "\n";
		} else {
			print ":Itemize() (\nItem()\"", &escape_term($1), "\n";
			$in_itemized = 1;
		}
		$in_text = 1;
		$have_par = 0;
	} else {	# plain text
		if (not $have_Text) {
			print "Text()\"";
			$have_Text = $in_text = 1;
		}
		&open_quote();
		print &escape_term($_), "\n";
		$have_par = 0;
		$in_text = 1;
	}
	
}

# Finish

&finish_text();

sub close_quote {
	if ($in_text) {
		print "\"" ;
	}
	if ($in_htmlcommand) {
		print ")";
	}
	$in_htmlcommand = $in_text = 0;
}
sub open_quote {
	if (not $in_text) {
		print ":\"" ;
		$in_text = 1;
	}
}

sub finish_text {
	&close_quote();
	if ($in_itemized or $in_enum) {
		print "\n);" ;
		$in_itemized = $in_enum = 0;
	} else {
		print ";" if ($have_Text);
	}
	$have_Text = 0;
}

# This is called on a piece of plain text which should be inside quotes
# (extra quotes need to be supplied as necessary)
# We need to convert special escapes to their Yacas representation
sub escape_term {
	my ($text) = @_;
# this would only allow one level of nested braces:
#	$text =~ s/\{((?:[^{}]*\{[^{}]*\})*[^{}]*)\}/":HtmlTerm("$1"):"/g;
	# {HtmlTerm}. Need to get the interior braces. Build the regex recursively:
	$regexes[0]='[^{}]*';	# if this is 'nobraces', then
#	oneornobraces = (nobraces {nobraces})* nobraces ;
	$nmax = 4;	# Max nesting level
	for($i=0; $i<$nmax; ++$i) {
		$regexes[$i+1]="(?:" . $regexes[$i] . "\{" . $regexes[$i] . "\})*" . $regexes[$i];
	}
	$text =~ s/\{($regexes[$nmax])\}/":HtmlTerm("$1"):"/go;
	# <i>emphasis</i>
	$text =~ s/<i>(.*)<\/i>/":HtmlEmphasis("$1"):"/gi;
	# <see also>
	$text =~ s/<\*(([^*]|\*[^>])+)\*>/&make_seealso($1);/ge;
	$text;
}

sub make_seealso {
	my ($text) = @_;
	"\":SeeAlso({\"" . join("\", \"", split(/, */, $text)) . "\"}):\"";
}
