#!/usr/bin/perl -w

##Module-------------------------------------------------------------------------- 
#
# The ReadInWriteOutAllStrip.pl script is used for removing all lines with station
# 'A182' from files from dataset 19.009. This script puts these removed lines in
# files with the name 'droppedRecs_NESOB96_ARS_YYMMDD_v2.txt'. The other lines are
# put into files with the name 'NESOB96_ARS_YYMMDD_v2.0qc'.
#
# The script goes through the given input directory. The script loops over each
# file. The script then looks for lines containing 'A182' and inserts it into a
# 'droppedRecs_NESOB96_ARS_YYMMDD_v2.txt' file. Each input file will generate a
# corresponding 'droppedRecs_NESOB96_ARS_YYMMDD_v2.txt' file and a corresponding
# 'NESOB96_ARS_YYMMDD_v2.0qc' file. 
#
# Inputs: The input files are from dataset 19.009 with the name pattern:
# 'NESOB96_ARS_YYMMDD.0qc' in 0qc format.
#
# Execute command:
# ReadInWriteOutAllStrip.pl
#
# Outputs: Each input file will create corresponding
# 'droppedRecs_NESOB96_ARS_YYMMDD_v2.txt' and 'NESOB96_ARS_YYMMDD_v2.0qc' files.
#
# Assumptions:
#
# 0. Users will search for all HARDCODED values.
#
# 1. The input data is located at the directory listed in the variable '$dir". The
#    output directory is also located at the hardcoded value below. 
#
# 2. The HARDCODED values are up to date.
#
# 3. The script assumes the structure of the files are similar and that there are
#    no header lines.
#
# Author: Daniel Choi October 2021
# Version 1.0
#
##Module--------------------------------------------------------------------------
use strict;
use warnings; 

printf "\nReadInWriteOutAllStrip.pl  began on ";print scalar localtime;printf "\n";

# Change debug value to 1 if debugging

my $debug = 0;

# Add location of input directory

my $dir = '../input'; # HARDCODED

# Loop through each file in directory

foreach my $input_filename (glob("$dir/*.0qc")) {
        
        if ($debug) {

		print "The input file is $input_filename .\n";   

	}        

	open (IN, "$input_filename") || die "Can't open for reading\n";

	# Remove directory and file extension for renaming the output file.
	
	my $filename = $input_filename;
	
	$filename =~ s{\.[^.]+$}{};
	$filename =~ s{^.*/}{};

	# Form output file names.

	my $output_dropped_A182 = $filename;
	$output_dropped_A182 = "droppedRecs_$output_dropped_A182\_v2.txt";
	my $output_corrected_0qc = "$filename\_v2.0qc";
	
	if ($debug) {

		print "The ouput files are $output_dropped_A182 and $output_corrected_0qc .\n";

	}

	# Define the output location

	open (QC, ">../output/$output_corrected_0qc") || die "Can't open file for writing\n";  # HARDCODED output directory
	open (DROP, ">../output/$output_dropped_A182") || die "Can't open file for writing\n"; # HARDCODED output directory
	print "Checking File $output_corrected_0qc\n";        

	# Loop through all lines in a file 

	my $dropped_line_count = 0;

	while (<IN>)
	{


		my $fileline = "$_";

		# Take one line

		chomp($fileline);
		
		# Pattern match to find all lines containing "A182" and add those lines to dropped files.
		
		if ($fileline =~ m/A182/) {
			
			print DROP "$fileline\n";
			$dropped_line_count ++;	
		}

		# All other lines are put in 0qc files.
		
		else {

			print QC "$fileline\n";
		
		}
		
	}

	# END of while loop for the lines in each file.
	
	print "Input $filename.0qc has $dropped_line_count dropped lines.\n";

	if ($debug) {
		
		print "$output_corrected_0qc and $output_dropped_A182 are created successfully.\n";

	}

	close IN;

}

# END of foreach loop for files in directory.

printf "\nReadInWriteOutAllStrip.pl  ended on ";print scalar localtime;printf "\n";

