#! /usr/bin/perl -w
## project_linkChecker.pl - URL Metrics for Master List Project pages
## @author Alley Robinson
## @date 25 September 2018
## 
## This script will check the status of all URL's that were presented in a text file by Don Stott. This is based on linkChecker.pl
##
## HOW TO RUN:
##       perl project_linkChecker.pl <file_name>
##
## INPUT:
##	Specified file that contains URLS	       
#	
#	#*********************************************
#	# SAMPLE OF FILE PROVIDED:
#	# ************************* 1. row *******************
#	# project_id: ACE_ENA
#	#        url: http://data.eol.ucar.edu/master_list/?project=ACE-ENA
#	# home_page_url: https://www.eol.ucar.edu/field_projects/ace-ena
#	# logo_url: https://www.eol.ucar.edu/field_projects/ace-ena
#	#
#	#***********************************************
#	
#
## OUTPUT:
## 	projectURLStatus.txt - A text file that contains the type of URL that was processed, the URL itself, and its status (GOOD or BROKEN)
##
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

parseFile();

printf "\nSee you later! linkChecker.pl ended on ";print scalar localtime;printf "\n";


sub parseFile
{
	# Declare variables
	my $outputFile     = "./projectURLStatus.txt";
	my $logFile        = "./urlStatus.log";
	my $totalUrlCount  = 0;
	my $goodUrlCount   = 0;
	my $brokenUrlCount = 0;
	my $result;

	# Open all the files!
	open(my $input_FH, '<', $inputFile)   or die "Can't open $inputFile!\n";
        open(my $output_FH, '>', $outputFile)   or die "Can't open $outputFile!\n";
	open(my $log_FH, '>', $logFile)   or die "Can't open $logFile!\n";


        print "PROCESSING: $inputFile \n";
        my @lines = <$input_FH>;

	#**********************************************
	# SAMPLE OF FILE PROVIDED:
	# ************************* 1. row *******************
	# project_id: ACE_ENA
	# 	 url: http://data.eol.ucar.edu/master_list/?project=ACE-ENA
	# home_page_url: https://www.eol.ucar.edu/field_projects/ace-ena
	# logo_url: https://www.eol.ucar.edu/field_projects/ace-ena
	#
	#***********************************************

	foreach my $line (@lines)
        {

		# Skip the rows that have ********* 1. row ******* in them
	 	next if $line =~ /row/;
	
		# Skip the rows that have the project id in them
		next if $line =~ /project_id/;

		# Now to deal with the splitting of the title and the URL
		 my @data = split(": ",$line);
       		 my $title = trim($data[0]);
         	 my $url   = trim($data[1]);

	
		 print $log_FH "Gathering info for URL: $url \n"; 
                
		# Gathering the info for the url:
		$result = system("wget --spider -nd -nv --timeout=5 --tries=2 -a wget_log.txt $url");

		if($result == 0)
                {
				$totalUrlCount++;
				$goodUrlCount++;
				print $output_FH "$title \t $url \t is GOOD \n";        
                }
                else
             	{
				$totalUrlCount++;
				$brokenUrlCount++;
                                print $output_FH "$title \t $url \t is BROKEN \n";
                }

        	 print $log_FH "***********************************************\n"; 
        } # end for each line


	print "There were $totalUrlCount URLs processed \n";
	print "There were $goodUrlCount working URLs \n";
	print "There were $brokenUrlCount broken URLs \n";

	print $log_FH "End Processing \n";
	
	close $input_FH;
	close $output_FH;
	close $log_FH;
	

}

sub trim {
    my ($line) = @_;
    return $line if (!defined($line));
    $line =~ s/^\s+//;
    $line =~ s/\s+$//;
    return $line;
}

