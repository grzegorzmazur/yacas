#!/usr/bin/perl -w

# convert plain ascii to Yacas manual code
# Syntax of ascii files is described in "txt2yacasdoc" essay
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
	s/\\/\\\\/g;
	s/"/\\"/g;
	# First deal with TAB-indented lines
	if (not $in_htmlcommand and /^\t\t\t\t([^ ].*)$/) {	# Book
		&finish_text();
		print "\tBook()\"", &escape_term($1), "\";\n\n";
		$have_par = 1;
	} elsif (not $in_htmlcommand and /^\t\t\t([^ ].*)$/) {	# Chapter
		&finish_text();
		print "\tChapter()\"", &escape_term($1), "\";\n\n";
		$have_par = 1;
	} elsif (not $in_htmlcommand and /^\t\t([^ ].*)$/) {	# Section
		&finish_text();
		print "\tSection()\"", &escape_term($1), "\";\n\n";
		$have_par = 1;
	} elsif (not $in_htmlcommand and /^\t    ([^ ].*)$/) {	# Subsection
		&finish_text();
		print "\tSubSection()\"", &escape_term($1), "\";\n\n";
		$have_par = 1;
	} elsif ((not $in_htmlcommand and /^\t( {0,2}[^ \t].*)$/) or ($in_htmlcommand and /^\t(.*)$/)) {	# Sample code (HtmlCommand); cannot start with empty line
		&start_text();
		if (not $in_htmlcommand) {
			&close_quote();
			print ":HtmlCommand(\"\n";
			$in_htmlcommand = 1;
		}
		print "$1\n";
		$have_par = 0;
		$in_text = 1;
	}	# Now not TAB-indented lines
	elsif (/^\s*$/) {	# New paragraph
	if (0 == 1) {	# This is for debugging only: breaks HTML docs
		&finish_text();
		&start_text();
	}
		if (not $have_par) {
			&close_quote();
			&start_text();
			if ($in_itemized or $in_enum or $in_htmlcommand) {
				print ")\n";
				$in_itemized = $in_enum = $in_htmlcommand = 0;
			}
			print ":HtmlNewParagraph()\n\n";
			$have_par = 1;
		}
	} elsif (/^\*\t[0-9]+\. (.*)$/) {	# Enum
		&close_quote();
		&start_text();
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
		&start_text();
		if ($in_itemized) {
			print ":Item()\"", &escape_term($1), "\n";
		} else {
			print ":Itemize() (\nItem()\"", &escape_term($1), "\n";
			$in_itemized = 1;
		}
		$in_text = 1;
		$have_par = 0;
	}	# Now non-TAB indented markup
	elsif (/^\*INCLUDE\s*([^ ].*)\s*$/) {	# Include another file
		&finish_text();
		$have_par = 1;
		print "IncludeFile(\"$1\");\n\n";
		
	} else {	# plain text - process it last, after every other markup is done
	# plain text closes HtmlCommand environment but does not close itemize or enum
		if ($in_htmlcommand) {
			&close_quote();	# this will close HtmlCommand too
		}
		&start_text();
		&open_quote();
		print &escape_term($_), "\n";
		$have_par = 0;
		$in_text = 1;
	}
}

# Finish up

&finish_text();
print "\n";

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
sub start_text {	# start a Text() block if necessary
	if ($have_Text == 0) {
		&close_quote();
		print "Text()\"";
		$have_Text = $in_text = 1;
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
	$regex='[^{}]*';	# if this is 'nobraces', then
#	oneornobraces = (nobraces {nobraces})* nobraces ;
	$nmax = 4;	# Max nesting level
	for($i=0; $i<$nmax; ++$i) {
		$regex="(?:$regex\{$regex\})*$regex";
	}
	$text =~ s/\{($regex)\}/":HtmlTerm("$1"):"/go;
	# math
	$text =~ s/\$\$([^\$]+)\$\$/":TeXMathD($1):"/g;
	$text =~ s/\$([^\$]+)\$/":TeXMath($1):"/g;
	# <i>emphasis</i>
	$text =~ s/<i>((?:[^<]|<[^\/]|<\/[^i]|<\/i[^>])*)<\/i>/":HtmlEmphasis("$1"):"/gi;
	# explicit hyperlinks
	$text =~ s/<\*((?:[^*]|\*[^>])+)\*>/&make_link($1);/ge;
	$text;
}

sub make_link {
	my ($text) = @_;
	if ($text =~ /^((?:ftp|http|file):\/\/.+)$/i) {	# Web URL
		return "\":HtmlLink(\"$1\", \"$1\", \"\", \"\"):\"";
	} elsif ($text =~ /^([^|]+)\|([^|]+)$/) {	# URL with anchored text
		return "\":HtmlLink(\"$1\", \"$2\", \"\", \"\"):\"";
	} else {
		return "\":SeeAlso({\"" . join("\", \"", split(/, */, $text)) . "\"}):\"";
	}
}
