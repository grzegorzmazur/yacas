#!/usr/bin/perl -w

# convert plain ascii to Yacas manual code
# Syntax of ascii files is described in "txt2yacasdoc" essay
#
# Use with care, test output with manualmaker

$debug_pars = 0;	# This is for debugging only: breaks HTML docs, but it is useful because it makes all paragraphs into separate Text() invocations and then it's a lot easier to debug book2TeX or manualmaker in Yacas - just look at the resulting .tex or .book file to see where it stopped generating.

$have_Text = 0;	# Means we have a Text() declared somewhere above
$have_par = 1;	# Means we already have a new paragraph
$in_text = 0;	# Means we are inside quotes of the Text()
$in_itemized = 0;
$in_enum = 0;
$in_htmlcommand = 0;

%star_labels = (
	"CALL" => "Topical() \"Calling format:\";",
	"PARMS" => "Topical() \"Parameters:\";",
	"DESC" => "Topical() \"Description:\";",
	"STD" => "StandardLib();",
	"CORE" => "BuiltIn();",
	"E.G." => "Topical() \"Examples:\";",
);

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
	}
	# HtmlCommand processed here
	elsif ((not $in_htmlcommand and /^\t( {0,2}[^ \t].*)$/) or ($in_htmlcommand and /^\t(.*)$/)) {	# Sample code (HtmlCommand); cannot start with empty line
		&start_text();
		if (not $in_htmlcommand) {
			if ($debug_pars) {
				&finish_text();
				&start_text();
			}
			&close_quote();
			print ":HtmlCommand(\"\n";
			$in_htmlcommand = 1;
		}
		print "$1\n" unless ($1 eq "*");	# Special feature to allow code examples that start with indented lines
		$have_par = 0;
		$in_text = 1;
	}	# Now not TAB-indented lines
	elsif (/^\s*$/) {	# New paragraph
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
	}
	#############################################################
	#			Star-based markup
	#############################################################
	elsif (/^\*\t[0-9]+\. (.*)$/) {	# Enum
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
	elsif (/^\*INCLUDE\s\s*([^ ].*)\s*$/i) {	# Include another file
		&finish_text();
		$have_par = 1;
		print "IncludeFile(\"$1\");\n\n";
	}
	#############################################################
	# stuff for refman
	#############################################################
	elsif (/^\*(CALL|PARMS|DESC|STD|CORE|E\.G\.)\s*$/) {	# labels without parameters
		&finish_text();
		$have_par = 1;
		print $star_labels{$1} . "\n";
	} elsif (/^\*(?:HEAD)\s\s*(.*)$/) {	# Topical()
		&finish_text();
		$have_par = 1;
		print "Topical()\"" . &escape_term($1) . "\";\n";
	} elsif (/^\*(?:A)\s\s*(.*)$/) {	# anchor
		&finish_text();
		$have_par = 1;
		print "AddBody(HtmlAnchor() \"" . $1 . "\");\n";
	} elsif (/^\*SEE\s\s*(.*)$/) {	# SeeAlso()
		$names = $1;
		$names =~ s/\s*$//;
		$names =~ s/^\s*//;
		&finish_text();
		$have_par = 1;
		print "Topical()\"See also:\";\nSeeAlso({";
		$have_prev_name = 0;
		foreach $name (split(/, /, $names)) {
			print ((($have_prev_name == 1) ? ", " : "") . "\"$name\"");
			$have_prev_name = 1;
		}
		print "});\n";
	} elsif (/^\*(?:CMD|FUNC)\s\s*(.*)\s*---\s*(.*)$/) {	# CmdDescription()
		$names = $1;
		$title = $2;
		&finish_text();
		$have_par = 1;
		print "\n";	# separate from previous command (has no effect on docs)
		# Trim extra spaces
		$names =~ s/\s*$//;
		$names =~ s/^\s*//;
		$title =~ s/\s*$//;
		$title =~ s/^\s*//;
		if ($names =~ /, /) {	# Section describes several commands, need to add anchors
			foreach $name (split(/, /, $names)) {
				print "AddBody(HtmlAnchor() \"" . $name . "\");\n";
			}
		}
		print "CmdDescription(\"" . $names . "\", \"" . $title . "\");\n";
	} elsif (/^\*INTRO\s\s*(.*)$/) {	# ChapterIntro()
		$text = $1;
		&finish_text();
		$have_par = ($text =~ /^\s*$/) ? 1 : 0;
		print "ChapterIntro()\"" . $text . "\n";
		$in_text = 1;		
	} else {
	#############################################################
	# plain text - process it last, after every other markup is done
	#############################################################
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
