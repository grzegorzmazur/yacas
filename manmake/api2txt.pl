#!/usr/bin/perl -w

# Convert yacasapi.cc to txt documentation

# Usage: api2txt.pl < yacasapi.cpp > yacasapi.txt

%special_syntax = (
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
	} elsif (/(infix|prefix|postfix|bodied)operators.SetOperator\(([^ \t]+)\s*,hash.LookUp\("([^ "\t]+)"\)/) {
	# infixoperators.SetOperator(0,hash.LookUp("_"));
	if ($in_addition == 0) {
		$in_addition = 1;
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

