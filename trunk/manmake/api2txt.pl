#!/usr/bin/perl -w

# Convert yacasapi.cc to txt documentation

# Usage: api2txt.pl < yacasapi.cpp > yacasapi.txt

%special_syntax = (	# these are the words that describe the special syntax functions
	"infix" => "infix",
	"prefix" => "prefix",
	"postfix" => "postfix",
	"bodied" => "bodied",
);

$in_addition = 0;
%core_special = ();
%core_functions = ();

while(<STDIN>) {
	chomp;
	# Only care about certain lines
	if (/SetCommand\(([^ \t]+)\s*,\s*"([^ "\t]+)"/) {
	# SetCommand(LispCommand      ,"Command");
		# store name and print later
		$core_functions{$2} = 1;
	} elsif (/^\s*OPERATOR\((infix|prefix|postfix|bodied)\s*,([^ \t]+)\s*,([^ "\t]+)\s*\);/) {
	# matches "OPERATOR(infix,0,_);" and sets $1="infix", $2="0", $3="_"
	if ($in_addition == 0) {
		$in_addition = 1;
	}
	if (!defined($core_special{$3}))
	{
		$core_special{$3} = "";	# avoid uninitialized values
	}
	$core_special{$3} .= "\n{" . $3 . "} --- " . $special_syntax{$1} . " (prec. {" . $2 . "}).\n";
	}
}

# now print everything
foreach $name (sort keys %core_functions) {
	print "\t$name\n";
}
print <<"EOF1";

In addition, the following functions are declared with special
syntax and precedence:

EOF1
foreach $name (sort keys %core_special) {
	print $core_special{$name};
}

