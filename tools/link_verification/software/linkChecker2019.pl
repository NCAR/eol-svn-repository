#! /usr/bin/perl -w
# linkChecker.pl - URL Metrics for EPIC Master List Web Page
# @author Alley Robinson
# @date 18 May 2018
# This script is going to go into the Master List's generated dataset list, take all the URL's into a separate text file, and output which ones are bad or broken
#
# HOW TO RUN:
# 	perl linkChecker.pl <project name> 
# 	The project's name is in lower case, due to Master List's directory structure
#
# INPUT:
# 	The master list's generated dataset list for a project: dataset_list.html
# 	"/net/web/data/html/master_lists/generated/$project/dataset_list.html"
#
# OUTPUT:
# 	<project name>_goodURL.txt      - List that contains the dataset name and the URL for all working URL's, separated by a tab
# 	<project name>_brokenURL.txt    - List that contains the dataset name and the URL for all broken URL's, separated by a tab
# 	<project name>_unknownURL.txt   - List that contains the dataset name and anything that is NOT an HTTP or FTP link, separated by a tab
# 	<project name>_results.txt      - Outputs some statistics about the datasets and the URL's
# 	<project name>_tempURLName.txt  - A temp file that is used for intermediate steps for generating the URL list that will be used for checking
# 	<project name>_finalURLName.txt - This file is in the final list of URL's to be checked
#	<project name>_wget_log.txt - This file has the results from the wget command
#
#
# VERSION 2:
# 	It has come to the team’s attention that a few of the URLs on the broken links output are actually not broken. 
# 	After some investigation, we have determined it was because of the large amount of URLs the software is checking. 
# 	The logic inside of the linkChecker software gave one second to check if they URL was valid or not. 
# 	We have increased the time it takes to check the URLs status to ten seconds, and the results were changed. 
# 	--timeout=10
#
# 	There were also links that were in the broken files that were working, but contain a self-signed certificate. The code was changed with 
# 	--no-check-certificate to bypass the SSL.   
#
# VERSION 3:
#       Added some fixes for the script. There was an issue with overcounting for the totalURL count. Other test variables and tests were added
#       to check addition. Some comments were added in for clarity.
#
#*****************************************************************************************************************************************************************
use strict;
use warnings;
use HTML::TokeParser;

printf "Welcome! linkChecker.pl  began on ";print scalar localtime;printf "\n";

# Declaring Global Variables:
my $debug = 0;

# Main:
my $project = $ARGV[0];

if(!defined $project)
{
	die "ERROR: Please add a project. Usage: perl linkChecker.pl <project_name>";
}

if($project ne lc $project)
{
	die "ERROR: The project format is not correct. Please use lower case";
}

if ($debug) { print "Testing Project: $project\n"; }
getURLlist();
checkLinks();
printStats();

printf "\nSee you later! linkChecker.pl ended on ";print scalar localtime;printf "\n";

#**********************************************
# @signature getURLlist()
# This function will go into the project's master list
# generated file and grab the URL's from the HTML source
# and send those results to a text file
#
#**********************************************
sub getURLlist
{		
	my $temp_URLNameFile = "./$project\_tempURLName.txt";
	my $final_URLNameFile = "./$project\_finalURLName.txt";
	
	
	# Master list Location
	my $datasetList   = "/net/web/data/html/master_lists/generated/$project/dataset_list.html";

	# Test list
#	my $datasetList = "/net/work/software/tools/work_2019/input_masterlists/$project\_dataset_list.html";

	# Setting up the HTML Parser:
	my $toke = HTML::TokeParser->new($datasetList);
	
	
	if ($debug) { print "Master List Dataset File is: $datasetList\n;" }
	
	# Final Files will be used to check the links:
	open(my $input,'<:encoding(UTF-8)',$datasetList) or die("Can't read the Master List $datasetList\n");
	open(my $temp, '>', $temp_URLNameFile) or die ("$temp_URLNameFile did NOT open\n");
	open(my $output, '>', $final_URLNameFile) or die ("$final_URLNameFile did NOT open\n");
	
	# Parse the file and extract the URLs to actualLinks.txt in the user's home directory
		while (my $token = $toke->get_tag("a"))
		{
			my $url = $token->[1]{href} || "null";
			my $title = $toke->get_trimmed_text("/a");
			
			print $temp "$title \t $url \n";
		}

	system("grep -v 'null' $temp_URLNameFile > $final_URLNameFile");



	# Close the files!
	close $input;
	close $temp; 
	close $output;


	

	# This was originally commented

	# Delete the temp file:
	unlink $temp_URLNameFile;
} 

#*************************************************
# @signature checkLinks()
# This function will check the links produced by
# getURLlist() inside of the actualLinks.txt file
#
# It will return (list of broken or good links)
#*************************************************
sub checkLinks
{  
	print "Verifying URL's for $project\n";
	my $result;

	# Input file:
	my $URLNameFile = "./$project\_finalURLName.txt";
	
	# Output files:
	my $goodLinksFile = "./$project\_goodURL.txt";
	my $badLinksFile  = "./$project\_brokenURL.txt";
	my $warningsFile  = "./$project\_unknownURL.txt";	
	
	# Open the files!
	open(my $URLName_FH, '<:encoding(UTF-8)',$URLNameFile) or die "Can't read the $URLNameFile\n";
	open(my $goodLinksHandle, '>', $goodLinksFile) or die "Can't open $goodLinksFile!\n";
	open(my $badLinksHandle, '>', $badLinksFile)   or die "Can't open $badLinksFile!\n";
	open(my $warningsHandle, '>', $warningsFile)   or die "Can't open $warningsFile!\n";
	 
	
	print "Processing $URLNameFile\n";	
	

	my @lines = <$URLName_FH>;

	# Now to check the URL status
	foreach my $line (@lines)
	{
		my @data = split("\t",$line);
		my $title = $data[0];
		my $url   = $data[1];


		if($url =~ "ftp:")
		{
			$url = substr $url, index($url, 'ftp:');
			$line = join ("\t",$title,$url);
			if ($debug) { print "$line \n"; }
		}

		if($url =~ "&")
		{
			$url =~ s/&/\\&/;
			if ($debug) { print "New URL: $url \n"; }
		}

#***************** NEW Specific Test Case (Don't Uncomment, Still Needs Work)

#		if($url =~ "submit.x=" && $url =~ "submit.y=")
#                {
#                        $url = substr $url, 0, index($url, 'submit.x=');
#			$line = join ("\t",$title,$url);

#                        if ($debug) { print "New URL: $url \n"; }
#                }
	


#****************  Change timeout from 10s     

		if ($debug) { print "Gathering info for URL: $url \n"; }
		$result = system("wget --spider -nd -nv --timeout=10 --tries=2 --no-check-certificate -a $project\_wget_log.txt $url");
			
		# Splitting the result into good or bad	or warnings

		if($result == 0)
		{
			if ($url !~ "http" && $url !~ "ftp:")
 	           	{	
        	               	print $warningsHandle $line;
                	       	if ($debug) { print "went to unknownURL.txt\n";}
               		}
			else
			{
				print $goodLinksHandle "$title \t $url";
				if ($debug) { print "went to goodURL.txt\n";}
			}
		}
		else
		{	
			if ($line !~ "http" && $url !~ "ftp:")
                	{
                        	print $warningsHandle $line;
                        	if ($debug) { print "went to unknownURL.txt\n";}
                	}
	 		else
			{
				print $badLinksHandle $line;
				if ($debug) { print "went to brokenURL.txt\n"; }
			}
		}

	if ($debug) { print "***********************************************\n"; }
		                       
	}

	# Close the files!!!
	close $goodLinksHandle;
	close $badLinksHandle;
	close $URLName_FH;

	print "End Processing for $project \n----------------------------------------";
}


#***************************************************
# @signature printStats()
# This function will provide statistics of the script
#
# TODO: Which stats?
#***************************************************
sub printStats
{
	if ($debug) { print "Gathering Metrics and Information \n"; }
	
	# Initializing Files:
	
	# Input:
	my $URLNameFile     = "./$project\_finalURLName.txt";
	my $goodLinksFile   = "./$project\_goodURL.txt";
        my $brokenLinksFile = "./$project\_brokenURL.txt";
        my $warningsFile    = "./$project\_unknownURL.txt";
	
	# Output:
	my $statsFile = "./$project\_results.txt";

	# Initializing the statistics variables;
	
	my $totalURLCount     = 0;
	my $okayURLCount      = 0;
	my $brokenURLCount    = 0;
	my $warningsURLCount  = 0;
	
	my $ftpCount         = 0;
	my $documentsCount   = 0;

	my $totalCodiacCount = 0;
	my $goodCodiacCount     = 0;
	my $brokenCodiacCount   = 0;
	my $warningsCodiacCount = 0;

	my $ucProject = uc $project;

	my $goodRatio;
	my $brokenRatio;
	my $warningsRatio;
	my $ftpRatio;
	my $documentsRatio;

	# NEW VARIABLES
	

	# ( neither =  Not ftp or Document )
#	my $neitherRatio;
#	my $neitherCount;
	
	

	# Open the files:
	open(my $totalInfo, '<:encoding(UTF-8)', $URLNameFile) or die "Could not open $URLNameFile\n";
	open(my $warnings, '<:encoding(UTF-8)', $warningsFile) or die "Could not open $warningsFile\n";
	open(my $brokenLinks, '<:encoding(UTF-8)', $brokenLinksFile) or die "Could not open $brokenLinksFile\n";
	open(my $goodLinks, '<:encoding(UTF-8)', $goodLinksFile) or die "Could not open $goodLinksFile\n";
	open(my $stats, '>', $statsFile) or die "Could not open $statsFile\n";	

	# Total Counts:
	while (my $totalLine = <$totalInfo>)
	{
		$totalURLCount++;
		if ($totalLine =~ m/codiac/)
		{
			$totalCodiacCount++;
#			$totalURLCount++;
		}
		if ($totalLine =~ m/ftp:/)
		{
			$ftpCount++;
#			$totalURLCount++;
		}
		if ($totalLine =~ m/Document/)
		{
			$documentsCount++;
#			$totalURLCount++;
		}

	# NEW EDIT
		
#		if ($totalLine !~ m/ftp:/ && $totalLine !~ m/Document/)
#		{
#	        	$neitherCount++;
#		}

	}

	if ($totalURLCount == 0)
	{
		print $stats "There are 0 datasets in $ucProject \n";
		return 0;
	}

	print $stats "There are $totalURLCount URL's in $ucProject\n";

	# FTP Stats (under total count)
	$ftpRatio = ($ftpCount / $totalURLCount) * 100;
	my $roundedFtpRatio = sprintf "%.2f", $ftpRatio;

	print $stats "There are $ftpCount ($roundedFtpRatio%) FTP Links in $ucProject\n";
	
	# Document Stats (under total count)
	$documentsRatio = ($documentsCount / $totalURLCount) * 100;
	my $roundedDocumentsRatio = sprintf "%.2f", $documentsRatio;
	
	print $stats "There are $documentsCount ($roundedDocumentsRatio%) Documents in $ucProject\n";

	# NEW  Neither ftp or Documents Stats (under total count)             
	
#	$neitherRatio = ($neitherCount / $totalURLCount) * 100;
#        my $roundedNeitherRatio = sprintf "%.2f", $neitherRatio;

#        print $stats "There are $neitherCount ($roundedNeitherRatio%) non-ftp, non-Documents in $ucProject\n";


	# Good Link Metrics:
	while (my $okayline = <$goodLinks>)
	{	
		$okayURLCount++;
		if ($okayline =~ m/codiac/)
		{
			$goodCodiacCount++;
		}
	}

	$goodRatio = ($okayURLCount / $totalURLCount) * 100;
	my $roundedGoodRatio = sprintf "%.2f", $goodRatio;

	print $stats "There are $okayURLCount ($roundedGoodRatio%)  working URL's in $ucProject\n";

	# Broken Link Metrics:
	while (my $brokenline = <$brokenLinks>)
	{
		$brokenURLCount++;
		if ($brokenline =~ m/codiac/)
		{
			$brokenCodiacCount++;
		}
	}

	$brokenRatio = ($brokenURLCount / $totalURLCount) * 100;
	my $roundedBrokenRatio = sprintf "%.2f", $brokenRatio;

	print $stats "There are $brokenURLCount ($roundedBrokenRatio%) broken URL's in $ucProject\n";	

	# Warning Metrics:
        while (my $warningsline = <$warnings>)
        {
                $warningsURLCount++;
                if ($warningsline =~ m/codiac/)
                {
                        $warningsCodiacCount++;
                }
        }

        $warningsRatio = ($warningsURLCount / $totalURLCount) * 100;
        my $roundedWarningsRatio = sprintf "%.2f", $warningsRatio;

	print $stats "There are $warningsURLCount ($roundedWarningsRatio%)  Unknown URL's in $ucProject\n";

	# EMDAC Metrics:
	print $stats "Found $totalCodiacCount internal EOL CODIAC datasets with $goodCodiacCount working URL's and $brokenCodiacCount broken URL's \n";
	


	# Close the files!
	close $totalInfo;
	close $warnings;
	close $brokenLinks;
	close $goodLinks;
	close $stats;
}



