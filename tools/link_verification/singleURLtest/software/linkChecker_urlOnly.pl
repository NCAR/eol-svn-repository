#! /usr/bin/perl -w
## project_linkChecker.pl - URL Metrics for Master List Project pages
## @author Alley Robinson
## @date 7 November 2018
##
## 
## This script will check the status of all URL's that were presented in a text file. This is based on linkChecker.pl
## ASSUMPTIONS:
##	1. There are ONLY url's, one per row, in the input file
##	2. The user will specify the input file in the command line when running the script
##
## HOW TO RUN:
##       perl project_linkChecker.pl <file_name>
##
## INPUT:
##	Specified file that contains URLS	       
#	
#	#*********************************************
#	# SAMPLE OF FILE PROVIDED:
#	# http://data.eol.ucar.edu/master_list/?project=ACE-ENA
#	# https://www.eol.ucar.edu/field_projects/ace-ena
#	# https://www.eol.ucar.edu/field_projects/ace-ena
#	#
#	#***********************************************
#	
#
## OUTPUT:
## 	singleURLStatus.txt - A text file that contains the type of URL that was processed, the URL itself, and its status
#
#	#*****************************************************
#	# SAMPLE OF OUTPUT FILE:
#	# URL DETAILS:
#	# http://data.eol.ucar.edu/master_list/?project=ACE-ENA::Works
#	# http://data.eol.ucar.edu/master_list/?project=ACE-ENA::Broken
#	#
#	# *********************************
#	# METRICS/INFORMATION:
#	# Total Links Checked: xxx
#	# Total Broken Links Counted: xxx (xxx%)
#	# Total Working Links Counted: xxx (xxx%) 
#	#*****************************************************
#
#
#
#
# TEST CASE INPUT:
# CHECK 1. All working links
# CHECK	2. All working links with some links that are known to lag
# CHECK	3. All broken links
# CHECK	4. All broken links with working links that lag
# CHECK	5. Mix of manually put in working + broken links
# CHECK	6. Column from spreadsheet that all work
# CHECK	7. All lagged links
# CHECK	8. Password links
#
#
##*******************************************************************************************************************************
use strict;
use warnings;

printf "Welcome! linkChecker.pl  began on ";print scalar localtime;printf "\n";

# Declaring Global Variables
my $debug = 0;
my $inputFile = $ARGV[0];

# Checking for arguments
if(!defined $inputFile)
{
        die "ERROR: Please add your input file to the command line. Usage: perl project_linkChecker.pl <file_name>";
}

# Seeing if the input file exists
unless (-e $inputFile)
{
                die "ERROR: $inputFile does not exist!\n";
}

if ($debug) { print "RETRIEVED FILE: $inputFile \n"; }

# Calling the parseFile method
parseFile();


printf "\nSee you later! linkChecker.pl ended on ";print scalar localtime;printf "\n";


sub parseFile
{
	# Declare variables
	my $outputFile       = "./singleURLStatus.txt";
	my $totalUrlCount    = 0;
	my $goodUrlCount     = 0;
	my $goodPercentage   = 0;
	my $brokenUrlCount   = 0;
	my $brokenPercentage = 0;
	my $result;

	# Open all the files!
	open(my $input_FH, '<', $inputFile)   or die "Can't open $inputFile!\n";
        open(my $output_FH, '>', $outputFile)   or die "Can't open $outputFile!\n";

        print "PROCESSING: $inputFile \n";
        my @lines = <$input_FH>;

	foreach my $url (@lines)
        {
		chomp($url);
		 print "Gathering info for URL: $url \n"; 
                
		# Gathering the info for the url:
		$result = system("wget --spider -nd -nv --timeout=60 --tries=3 -a singleURL_wget_log.txt $url");

		if($result == 0)
                {
				$totalUrlCount++;
				$goodUrlCount++;
				print $output_FH "$url\::Works \n";        
                }
		elsif($result == 1536)
		{
				$totalUrlCount++;
                                $goodUrlCount++;
                                print $output_FH "$url\::Works, Needs Login Information \n";
		}

                else
             	{
				$totalUrlCount++;
				$brokenUrlCount++;
                                print $output_FH "$url\::Broken \n";
                }

        } # end for each line

	$goodPercentage   = ($goodUrlCount / $totalUrlCount) * 100;
	$brokenPercentage = ($brokenUrlCount / $totalUrlCount) * 100;

	my $roundedGoodRatio = sprintf "%.2f", $goodPercentage;
	my $roundedBadRatio  = sprintf "%.2f", $brokenPercentage;

	print $output_FH "\n\n**************************************************************************\n";	
	print $output_FH "There were $totalUrlCount URLs processed \n";
	print $output_FH "There were $goodUrlCount ($roundedGoodRatio %) working URLs \n";
	print $output_FH "There were $brokenUrlCount ($roundedBadRatio %) broken URLs \n";

	print "\n\n**************************************************************************\n";
        print "There were $totalUrlCount URLs processed \n";
        print "There were $goodUrlCount ($roundedGoodRatio %) working URLs \n";
        print "There were $brokenUrlCount ($roundedBadRatio %) broken URLs \n";


	print "End Processing \n";

	# Close the files!	
	close $input_FH;
	close $output_FH;	

}
