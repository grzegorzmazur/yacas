#!/usr/bin/perl -w

# script to extract function propotypes from headers and to prepare a stub file with most of the functions (the rest will have to be done by hand)

# Usage: cat /usr/include/gsl/*.h | perl -w extract_stub.pl > outputfile.stub

# prints the resulting stub to standard output

# extracts all prototypes of the form "double func(double arg, ...)"

while(<STDIN>)
{
	# only match lines that contain a single function prototype, and if all arguments are doubles or integers
	# function must return double or int, also all arguments must have this type
	# specify type here
	$type = "double|int|unsigned|long|unsigned long||unsigned int";
	
	if (/^\s*($type)\s+([0-9a-z_]+)\((const )?($type) [0-9a-z_]+(, ?(const )?($type) [0-9a-z_]+)*\);\s*$/)
	{
		# now $1 is the type, $2 is the function name
		$return_type = $1;
		$func_name = $2;
		$yacas_name = $func_name;
		$yacas_name =~ s/_/\'/g;	# need to replace _ chars
		
		# create an argument list of the form {{"int", "x"}, {"double", "y"}}
		$text = $_;
		# strip everything except argument list
		$text =~ s/^\s*($type)\s+([0-9a-z_]+)\((.*)\);\s*$/$3/;
		# prettyprint
		$arg_list = $text;
		# strip extra spaces
		$arg_list =~ s/  +/ /g;
		# add }{ between arguments and separate them from the types
		$arg_list =~ s/ ([^ ]+), */", "$1"}, {"/g;
		# remove "const" from all types - do we need this?
		#$arg_list =~ s/const +//g;
		# separate the last argument
		$arg_list =~ s/ ([^ ]+)$/", "$1/;
		printf "StubAPICFunction(\"%s\", \"%s\", \"%s\", {{\"%s\"}});\n", $return_type, $func_name, $yacas_name, $arg_list;
	}
};

