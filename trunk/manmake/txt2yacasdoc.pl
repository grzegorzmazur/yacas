#!/usr/bin/perl -w

# convert plain ascii to Yacas manual code
# Syntax of ascii files is described in "txt2yacasdoc" essay
#
# Use with care, test output with manualmaker

$debug_pars = ("@ARGV" =~ /-debug/) ? 1 : 0;	# This is for debugging only: breaks HTML docs, but it is useful because it makes all paragraphs into separate Text() invocations and then it's a lot easier to debug book2TeX or manualmaker in Yacas - just look at the resulting .tex or .book file to see where it stopped generating.

$have_Text = 0;	# Means we have a Text() declared somewhere above
$have_par = 1;	# Means we already have a new paragraph
$in_quotes = 0;	# Means we are inside quotes of the Text()
$in_itemized = 0;
$in_enum = 0;
$in_htmlcommand = 0;

%star_labels = (
	"CALL" => "Topical() \"Calling format:\";",
	"PARMS" => "Topical() \"Parameters:\";",
	"DESC" => "Topical() \"Description:\";",
	"UNIX" => "UnixSpecific();",
	"MSWIN" => "MSWinSpecific();",
	"MAC" => "MacSpecific();",
	"STD" => "StandardLib();",
	"CORE" => "BuiltIn();",
	"E.G." => "Topical() \"Examples:\";",
	"EG" => "Topical() \"Example:\";",
);

%intro_labels = (
	"BLURB" => "Blurb()",
	"INTRO" => "ChapterIntro()",
	"REM" => "DocumentationComment()",
);

while (<STDIN>) {
	chomp;
	s/\\/\\\\/g;	# escape all backslashes in the source text
	s/"/\\"/g;	# escape all quotes in the source text
	# First deal with TAB-indented lines
	if (not $in_htmlcommand and /^\t\t\t\t([^ ].*)$/) {	# Book
		&finish_text_close_quote();
		print "\tBook()\"", &escape_term($1), "\";\n\n";
		$have_par = 1;
	} elsif (not $in_htmlcommand and /^\t\t\t([^ ].*)$/) {	# Chapter
		&finish_text_close_quote();
		print "\tChapter()\"", &escape_term($1), "\";\n\n";
		$have_par = 1;
	} elsif (not $in_htmlcommand and /^\t\t([^ ].*)$/) {	# Section
		&finish_text_close_quote();
		print "\tSection()\"", &escape_term($1), "\";\n\n";
		$have_par = 1;
	} elsif (not $in_htmlcommand and /^\t    ([^ ].*)$/) {	# Subsection
		&finish_text_close_quote();
		print "\tSubSection()\"", &escape_term($1), "\";\n\n";
		$have_par = 1;
	}
	#############################################################
	# HtmlCommand processed here
	#############################################################
	elsif ((not $in_htmlcommand and /^\t( {0,2}[^ \t].*)$/) or ($in_htmlcommand and /^\t(.*)$/)) {	# Sample code (HtmlCommand); cannot start with empty line
		&start_text_open_quote();
		if (not $in_htmlcommand) {
			if ($debug_pars) {
				&finish_text_close_quote();
				&start_text_open_quote();
			}
			&close_quote();
			print ":HtmlCommand(\"\n";
			$in_htmlcommand = 1;
		}
		print "$1\n" unless ($1 eq "*");	# Special feature to allow code examples that start with indented lines
		$have_par = 0;
		$in_quotes = 1;
	}	# Now processing not-TAB-indented lines
	#############################################################
	# New paragraph	
	#############################################################
	elsif (/^\s*$/) {
		if ($have_par == 0) {
			&finish_text_close_quote();
			&start_text_open_quote();
			print "\":HtmlNewParagraph():\n\n\"";
			$have_par = 1;
		}
	}
	#############################################################
	#			Star-based markup
	#############################################################
	elsif (/^\*\t[0-9]+\. (.*)$/) {	# Enum
			if ($debug_pars) {
				&finish_text_close_quote();
				&start_text_open_quote();
			}
		&start_text_open_quote();
		&close_quote();
		if ($in_enum) {
			print ":Item()\"", &escape_term($1), "\n";
		} else {
			print ":Enumerate() (\nItem()\"", &escape_term($1), "\n";
			$in_enum = 1;
		}
		$in_quotes = 1;
		$have_par = 0;
	} elsif (/^\*\t(.*)$/) {	# Itemized
			if ($debug_pars) {
				&finish_text_close_quote();
				&start_text_open_quote();
			}
		&start_text_open_quote();
		&close_quote();
		if ($in_itemized) {
			print ":Item()\"", &escape_term($1), "\n";
		} else {
			print ":Itemize() (\nItem()\"", &escape_term($1), "\n";
			$in_itemized = 1;
		}
		$in_quotes = 1;
		$have_par = 0;
	}	# Now non-TAB indented markup
	elsif (/^\*INCLUDE\s\s*([^ ].*)\s*$/i) {	# Include another file
		&finish_text_close_quote();
		$have_par = 1;
		print "IncludeFile(\"$1\");\n\n";
	}
	elsif (/^\*EVAL\s\s*([^ ].*)\s*$/i) {	# Evaluate a Yacas expression and insert its result as text
		$expression = $1;
		# un-escape quotes and backslash again
		$expression =~ s/\\"/"/g;
		$expression =~ s/\\\\/\\/g;
		&finish_text_close_quote();
		$have_par = 0;	# even if we had a paragraph before us, now we don't
		print "DocEval($expression);\n";	# this will insert inline Yacas code
	}
	#############################################################
	# stuff for refman
	#############################################################
	elsif (/^\*([-A-Z.]+)\s*$/ and defined($star_labels{$1})) {	# labels without parameters
		&finish_text_close_quote();
		$have_par = 1;
		print $star_labels{$1} . "\n";
	} elsif (/^\*(?:HEAD)\s\s*(.*)$/) {	# Topical()
		&finish_text_close_quote();
		$have_par = 1;
		print "Topical()\"" . &escape_term($1) . "\";\n";
	} elsif (/^\*(?:A|AA)\s\s*(.*)$/) {	# anchor
		&finish_text_close_quote();
		$have_par = 1;
		print "AddBody(AddAnchor() \"" . $1 . "\");\n";
	} elsif (/^\*SEE\s\s*(.*)$/) {	# SeeAlso()
		$names = $1;
		$names =~ s/\s*$//;
		$names =~ s/^\s*//;
		&finish_text_close_quote();
		$have_par = 1;
		print "Topical()\"See also:\";\nSeeAlso({";
		$have_prev_name = 0;
		foreach $name (split(/,[ \t]+/, $names)) {
			print ((($have_prev_name == 1) ? ", " : "") . "\"$name\"");
			$have_prev_name = 1;
		}
		print "});\n";
	} elsif (/^\*SEE\s*(.*)$/) {	# empty SeeAlso()
		&finish_text_close_quote();
		$have_par = 1;
		# do nothing
	} elsif (/^\*(?:CMD|FUNC)\s\s*(.*)\s*---\s*(.*)$/) {	# CmdDescription()
		$names = $1;
		$title = $2;
		&finish_text_close_quote();
		$have_par = 1;
		print "\n";	# separate from previous command (has no effect on docs)
		# Trim extra spaces
		$names =~ s/\s*$//;
		$names =~ s/^\s*//;
		$title =~ s/\s*$//;
		$title =~ s/^\s*//;
		if ($names =~ /, /) {	# Section describes several commands, need to add anchors
			foreach $name (split(/,[ \t]+/, $names)) {
				print "AddBody(HtmlAnchor() \"" . $name . "\");\n";
			}
			print "CmdDescriptionMany";
		} else {
			print "CmdDescriptionSingle";
		}
		print "(\"" . $names . "\", \"" . &escape_term($title) . "\");\n";
	} elsif (/^\*(INTRO|BLURB|REM)\s*(.*)$/) {	# ChapterIntro(), Blurb(), DocComment()
		$label = $1;
		$text = $2;
		&finish_text_close_quote();
		$have_par = ($text =~ /^\s*$/) ? 1 : 0;
		print $intro_labels{$label} . "\"" . &escape_term($text) . "\n";
		$in_quotes = 1;
		$have_Text = 1;
	} else {
	#############################################################
	# plain text - process it last, after every other markup is done
	#############################################################
	# plain text closes HtmlCommand environment but does not close itemize or enum
		if ($in_htmlcommand) {
			&close_quote();	# this will close HtmlCommand too
		}
		&start_text_open_quote();
		print &escape_term($_), "\n";
		$have_par = 0;
		$in_quotes = 1;
	}
}

# Finish up

&finish_text_close_quote();
print "\n";

sub close_quote {
	if ($in_quotes) {
		print "\"" ;
		$in_quotes = 0;
	}
	if ($in_htmlcommand) {
		print ")";
		$in_htmlcommand = 0;
	}
}
sub open_quote {
	if (not $in_quotes) {
		print ":\"" ;
		$in_quotes = 1;
	}
}
sub start_text_open_quote {	# start a Text() block if necessary
	if ($have_Text == 0) {
		&close_quote();
		print "Text()\"";
		$have_Text = $in_quotes = 1;
	} elsif (not $in_quotes) {
		&open_quote();
	}
}

sub finish_text_close_quote {
	&close_quote();
	if ($in_itemized) {
		print "\n)";
		$in_itemized = 0;
	}
	if ($in_enum) {
		print "\n)";
		$in_enum = 0;
	}
	if ($have_Text) {
		print ";";
		$have_Text = 0;
	}
}

# This routine is called on a piece of plain text which should be inside quotes
# (extra quotes need to be supplied as necessary)
# It will add escapes when it finds certain special symbols 
# using their Yacas representation
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
	# special escapes \< \> for HTML
	$text =~ s/\\\\</":Lt():"/g;
	$text =~ s/\\\\>/":Gt():"/g;
	# math
	$text =~ s/\$\$([^\$"]+)\$\$([^- \t]+)/":TeXMathD($1,\"$2\"):"/g;
	$text =~ s/\$\$([^\$"]+)\$\$/":TeXMathD($1):"/g;
	$text =~ s/\$([^\$"]+)\$/":TeXMath($1):"/g;
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
		return "\":SeeAlso({\"" . join("\", \"", split(/,[ \t]*/, $text)) . "\"}):\"";
	}
}
