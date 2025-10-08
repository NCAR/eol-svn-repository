#! /usr/bin/perl -w
#------------------------------------------------------------------------------------------------------
# The PACS_Daily_Precip_Converter.pl is written to correct the format of the 
# PACS: Pan American Climate Studies Mexican Daily Precipitation Data (15.108).
# The data are missing observation hours, which are all unknown (value = 99).
# Additionally, the date format of the data needs to be changed from YYYY/MM/DD
# to YYYY/MM and the hour and minute entries need to be removed.
# 
# This s/w goes through the data once. After removing the extraneous date information 
# and writing it to the new file, it copies the network name, station name, latitude,
# longitude, and occurence value (and the proper formatting that accompanies those 
# entries) directly to the new file. Then, looping through the trios of precipitation
# values, it copies the three values directly and adds the observation value of 99
# to the new file. Each line of the new output file should have exactly 527 characters
# (including a return char), and each output file should have the same number of
# lines as the corresponding input file. 
# 
# Inputs: 124 MXPCP_YYYYMM.pqcf files containing daily precipitation data from dataset
# 	15.108 located in the input directory. Users need to specify the input directory
# 	in the HARD-CODED section below.
# 	Example input file: MXPCP_199008.pqcf
# 
# Execute command:
#   perl PACS_Daily_Precip_Converter.pl 
# 
# Outputs: 124 correctly formatted daily *.pqcf precipitation files that include observation
# 	hours. The output files will be renamed MXPCP_YYYYMM_Ver2.pqcf.
# 	Example output file (corresponding to above input file): MXPCP_199008_Ver2.pqcf 
# 
# Assumptions and Warnings:
#  0. User will search for HARD-CODED and fill in the information needed:
#  input directory, output directory, and debug. Turning on debug will print
#  entry and exit statements, line count statements, and character count
#  per line statements for every file.  
#  
#  1. That the input data is from dataset 15.108 and is in daily *.pqcf format.
#  The input data contains date, network, and station information, latitude,
#  longitude, occurence value, and 31 trios of precipiation values. 
#
#  2. That the HARD-CODED elements in this software have been updated for
#  the current project and data, as needed.
#
#  3. That the only missing values from the precipiation values are the
#  observation hours, which are all set to missing (99).
#
#  4. That the date is in the form of YYYY/MM/DD HH:mm where YYYY is the year,
#  MM is the month, DD the day, HH the hour, and mm the minute. This s/w
#  will only enter the year and month into the new output file.
#
#  5. This s/w assumes that the missing value for all precipitation values is
#  "-999.99.
#
#  6. This s/w assumes that the network information, station information, latitude,
#  longitude, and occurence value are all formatted correctly in daily pqcf format.
#
#  7. All input records are in the input directory and will be written to output files.
#
#  8. User must create input and output directories before running code.
#
# Author Summer Stafford October 2021
# Version PACS Daily Precipitation Converter  1.0
#               Originally developed for PACS Mexican Daily Precipitation Data (15.018) 
#
#--------------------------------------------------------------------------------------------------------
use strict;

printf "\nPACS_Daily_Precip_Converter.pl began on ";print scalar localtime;printf "\n";

&main();
printf "\nPACS_Daily_Precip_Converter.pl ended on ";print scalar localtime;printf "\n";

#--------------------------------------------------------------------------------------------------------
# @signature void main()
# <p>Collect hard coded information and send to readDataFiles() function.</p>
#--------------------------------------------------------------------------------------------------------
sub main{
	#HARD-CODED
	my $input_dir = "../input"; #Location of all input files with extension *.pqcf
	my $output_dir = "../output"; #Location to write output files
	my $DEBUG = 1; # 1 = turn on debug statements, 0 = turn off debug statements
	readDataFiles($input_dir, $output_dir, $DEBUG);
}

#-------------------------------------------------------------------------------------------------------
# readDataFiles()
# Open input directory and collect names of all *.pqcf files, send each file to parseFile(), print total output files created.
#-------------------------------------------------------------------------------------------------------
sub readDataFiles{
	my ($input, $output, $DEBUG) = @_;

	# Open input directory and read in all *.pqcf files	
	opendir(my $RAW, $input) or die("Can't read input directory ".$input);
	my @files = grep(/.pqcf$/i,sort(readdir($RAW)));
    closedir($RAW);
	my $files_total = @files;
	printf "\nProcessing $files_total input files...\n";

	# Send each file to be edited and written to output file	
	foreach my $file(@files){
		parseFile($file, $input, $output, $DEBUG);
	}

	# Count output files and display
	opendir(my $OUTPUT, $output) or die("Can't read output directory ".$output);
    my @out_files = grep(/.pqcf$/i,sort(readdir($OUTPUT)));
    closedir($OUTPUT); 
    my $out_files_total = @out_files;
    printf "\n$out_files_total output files created.\n";

}

#-------------------------------------------------------------------------------------------------------
# parseFile()
# Delete day, hour, and second text from each line and add observation hour of 99 to each precipitation value triple. 
#-------------------------------------------------------------------------------------------------------
sub parseFile{
	my ($file, $input, $output, $DEBUG) = @_;
	
	printf("\nProcessing file: %s\n",$file);
	open(my $FILE,$input."/".$file) or die("Can't open file: ".$file);

	# Collect all lines in the file as entries in a list
	my @lines = <$FILE>;
    close($FILE);

	# Create output file name, list for corrected lines, and lists to check characters and precip values
	my $name = substr $file, 0, -5;
	my $outfile = sprintf($name."_Ver2.pqcf");
	my @corrected_lines = ();
	my @quadNumbers = ();
	my @charNumbers = ();
	printf("\tOutput file name is %s\n", $outfile);

	#Process each line	
	foreach my $LINE(@lines){
		my @line =split (/\s+/, $LINE);
		my @line_corrected = ();
		
		#Remove day from date
		my $date = $line[0];
		my $date_corrected = substr $date, 0, -3;
		push(@line_corrected, $date_corrected);
	
		#Add network, station, latitude, longitude, and occurence value to corrected line 
		my $station_info = substr $LINE, 20, 53;
		push(@line_corrected, $station_info);
		
		#Loop through each triple and add observation hour
		my $size = @line;
		for (my $j = 7; $j < $size; $j = $j + 3){
			my $precip_value = sprintf '%7s', $line[$j];
			push(@line_corrected, ($precip_value, $line[$j+1], $line[$j+2], "99"));
		}
		
		#Convert array into string and add into corrected lines array 
		my $lineString = join(" ", @line_corrected);
		push(@corrected_lines, $lineString);

		#Add character count and number of precip quads into arrays for debugging
		if ($DEBUG){
			push(@quadNumbers, (scalar(@line_corrected) - 2)/ 4);
			push(@charNumbers, length($lineString));
		}
	}
	#Check for 526 characters and 31 quads for debugging
	if ($DEBUG){
		if ((@charNumbers == grep { $_ eq "526" } @charNumbers) and (@quadNumbers == grep { $_ eq "31" } @quadNumbers)) {
			printf "\tEvery line in output file has 526 characters and 31 precipitation values.\n";	
		}
		#Print problematic line numbers
		else{
			printf "\tThe following lines have an incorrect number of characters:\n";
			my $size = @charNumbers;
			for (my $j = 0; $j < $size; $j = $j + 1){
				if (@charNumbers[$j] != 526){
					printf "\t\t".($j + 1)."\n";
				}
			}
		}	
		#Print number of lines per file
		my $output_lines = @corrected_lines;
		printf "\t$output_lines lines written to output file.\n";
	}

	#Write corrected lines to ouput file
	open(my $OUT,">".$output."/".$outfile) or die("Can't open output file for $file\n");
	print $OUT "$_\n" for @corrected_lines; 
	close($OUT);

}
