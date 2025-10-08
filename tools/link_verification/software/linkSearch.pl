# linkSearch.pl - a small script that searches a broken URL backwards to find any valid parts of the link.
# @author Alley Robinson
# @date 25 June 2018
# 
# HOW TO RUN:
#     perl linkSearch.pl <project> 
#             
# INPUT:
#     The brokenURL.txt file from a given project that is created from the linkChecker.pl script.
# 
# OUTPUT:
#     <project name>_valid_URLSegments.txt - a file that will contain any valid segments of the broken URL path
#     <project name>_invalid_URLPaths.txt   - a file that will contain the invalid URL paths if the whole path is invalid
#      
#
#********************************************************************************************
use strict;
use warnings;

printf "Welcome! linkSearch.pl  began on ";print scalar localtime;printf "\n";

# Declaring Global Variables:
my $debug = 1;

# Main:

my $project = $ARGV[0];
my $numSegments        = 1;
my $numGoodSegments    = 0;
my $numBrokenSegments  = 0;
my $numUnknownSegments = 0;

if(!defined $project)
{
        die "ERROR: Please add a project. Usage: perl linkChecker.pl <project_name>";
}

if($project ne lc $project)
{
        die "ERROR: The project format is not correct. Please use lower case";
}

if ($debug) { print "Testing Project: $project\n"; }

getBrokenURLFile();
parseFile();
printStats();

printf "\nSee you later! linkSearch.pl ended on ";print scalar localtime;printf "\n";
                

#***********************************************
# @signature getBrokenURLFile()
# This function will retrieve the broken URL file 
# that has been generated from the linkChecker.pl script.
#
#***********************************************
sub getBrokenURLFile
{

	my $brokenURLFile = "./$project\_brokenURL.txt";

	unless (-e $brokenURLFile)
	{
		print "WARNING: $brokenURLFile does not exist!\n";
		print "Running linkChecker.pl to generate file: $brokenURLFile\n";
		system("perl ./linkChecker.pl $project");
	}

	if ($debug) { print "RETRIEVED FILE: $brokenURLFile \n"; }
}

#***********************************************
# @signature parseFile()
# This function will parse the brokenURL text file.
#
#***********************************************
sub parseFile
{
	# Input file:
	my $brokenURLFile = "./$project\_brokenURL.txt";

	# Output files:
	my $resultSegmentsFile = "./$project\_segmentURLStatus.txt";
	
	 

	# Open all the files!
	open(my $URLName_FH, '<', $brokenURLFile)   or die "Can't open $brokenURLFile!\n";
	open(my $result_FH, '>', $resultSegmentsFile)   or die "Can't open $resultSegmentsFile!\n";
	

	print "PROCESSING: $brokenURLFile \n";
	my @lines = <$URLName_FH>;
	
        foreach my $line (@lines)
        {
 		# Split the line into title and URL 
		my @data = split("\t",$line);
        	my $title = $data[0];
        	my $url   = $data[1];
        	
		#Now to do the work on the URL:
		#*******************************
		# Sample URL:
		# http://data.eol.ucar.edu/datafile/nph-get/485.097/readme_dsd.sfc
		#      ^^                 ^        ^       ^       ^
		# urlData[0] = http:
		# urlData[1] = 
		# urlData[2] = data.eol.ucar.edu
		# urlData[3] = datafile
		# urlData[4] = nph-get
		# urlData[5] = 485.097
		# urlData[6] = readme_dsd.sfc
		#*******************************	
		
		if ($debug) { print "SPLITTING URL: $url \n"; }

		# Split the URL by the / character:
		my @urlData = split("/", $url);	

		my $mainURL = join ("/", $urlData[0], $urlData[1], $urlData[2]);
                print $result_FH "CHECKING: $url \n";
				
		
		# Checking the domain of each URL:
		my $result = system("wget --spider -nd -nv --timeout=1 --tries=2 -a $project\_search_log.txt $mainURL");
		                
		if ($result == 0)
                {
                         if ($mainURL !~ "http" && $mainURL !~ "ftp:")
	                 {
                                 print $result_FH  "$title \t $mainURL \t is UNKNOWN \n";
				 $numUnknownSegments++;
                         }

                         else
                         {
                                 print $result_FH "$title \t $mainURL \t is GOOD \n";
				 $numGoodSegments++;
                         }
                }

                else
                {
                         if ($mainURL !~ "http" && $mainURL !~ "ftp:")
                         {
                                print $result_FH "$title \t $mainURL /t is UNKNOWN \n";
				$numUnknownSegments++; 
                         }
                         else
                         {
                                print $result_FH "$title \t $mainURL \t is BROKEN \n";
				$numBrokenSegments++;
                         }
                }
 
		# Appending each /.../ to the end of the domain and checking that URL:
		for my $i (3..$#urlData) 
		{
			$mainURL = $mainURL . "/$urlData[$i]";
			if ($debug) { print "CHECKING: $mainURL \n"; }

			my $result = system("wget --spider -nd -nv --timeout=1 --tries=2 -a $project\_search_log.txt $mainURL");
		
			if ($result == 0)
	                {
        	                if ($mainURL !~ "http" && $mainURL !~ "ftp:")
                	        {
                                	print $result_FH  "$title \t $mainURL \t is UNKNOWN \n";
					$numUnknownSegments++;
                       		}
		
                	        else
                        	{
                                	print $result_FH "$title \t $mainURL \t is GOOD \n";
					$numGoodSegments++;
                        	}
                	}
			
   	             	else
                	{
                        	if ($mainURL !~ "http" && $mainURL !~ "ftp:")
                        	{
                         	         print $result_FH  "$title \t $mainURL \t is UNKNOWN \n";
					 $numUnknownSegments++; 
                        	}
                        	else
                        	{
                                	print $result_FH "$title \t $mainURL \t is BROKEN \n";
					$numBrokenSegments++; 
                                }
                	}
			$numSegments++;
		}

		print $result_FH "**********************************************************************\n";
	
	} # end foreach my $line (@lines) 
		
		
} # end function


#******************************************
# @signature printStats()
# This function will print out some stats/info
# about the program
#******************************************
sub printStats
{
	if ($debug) { print "Generating metrics/information \n"; }

	# Input Files:
	my $brokenInput       = "./$project\_brokenURL.txt";
	my $segmentStatusFile = "./$project\_segmentURLStatus.txt";

	# Output File: 
	my $statsFile = "./$project\_segmentResults.txt";

	# Initializing the Statistics Variables:
	my $totalURLCount          = 0;
        my $goodSegmentsCount      = 0;
        my $brokenSegmentsCount    = 0;
        my $warningsSegmentsCount  = 0;

        my $ftpCount         = 0;
        my $documentsCount   = 0;
	my $totalCodiacCount = 0;

        my $ucProject = uc $project;

        my $goodRatio;
        my $brokenRatio;
        my $warningsRatio;
        my $ftpRatio;
        my $documentsRatio;

	# Open the files!
	open(my $input, '<:encoding(UTF-8)', $brokenInput) or die "Could not open $brokenInput\n";
        open(my $segmentStatus, '<:encoding(UTF-8)', $segmentStatusFile) or die "Could not open $segmentStatusFile\n";
        open(my $stats, '>', $statsFile) or die "Could not open $statsFile\n";


	# TOTAL COUNTS: 
        while (my $totalLine = <$input>)
        {
		$totalURLCount++;

                if ($totalLine =~ m/codiac/)
                {
        		$totalURLCount++;
	                $totalCodiacCount++;
                }
                if ($totalLine =~ m/ftp:/)
                {
			$totalURLCount++;
                        $ftpCount++;
                }
                if ($totalLine =~ m/Document/)
                {
			$totalURLCount++;
                        $documentsCount++;
                }

        }

	print $stats "There are $totalURLCount URLs with $numSegments URL SEGMENTS checked in $ucProject\n";

	# FTP Stats (derived from total counts):
	$ftpRatio = ($ftpCount / $numSegments) * 100;
        my $roundedFtpRatio = sprintf "%.2f", $ftpRatio;

	print $stats "There are $ftpCount ($roundedFtpRatio%) FTP URLs checked in $ucProject\n";

	# Document Stats (derived from total counts):
	$documentsRatio = ($documentsCount / $numSegments) * 100;
        my $roundedDocumentsRatio = sprintf "%.2f", $documentsRatio;

        print $stats "There are $documentsCount ($roundedDocumentsRatio%) DOCUMENTS checked in $ucProject\n";

	# EMDAC Stats
	print $stats "There are $totalCodiacCount internal EOL CODIAC datasets checked. \n";

	# GOOD SEGMENT COUNTS:
        $goodRatio = ($numGoodSegments / $numSegments) * 100;
        my $roundedGoodRatio = sprintf "%.2f", $goodRatio;

        print $stats "There are $numGoodSegments ($roundedGoodRatio%) WORKING URL SEGMENTS in $ucProject\n";

	# BAD SEGMENT COUNTS:
	$brokenRatio = ($numBrokenSegments / $numSegments) * 100;
        my $roundedBrokenRatio = sprintf "%.2f", $brokenRatio;

        print $stats "There are $numBrokenSegments ($roundedBrokenRatio%) BROKEN URL SEGMENTS in $ucProject\n";

	# WARNING SEGMENT COUNTS:
        $warningsRatio = ($numUnknownSegments / $numSegments) * 100;
        my $roundedWarningsRatio = sprintf "%.2f", $warningsRatio;

        print $stats "There are $numUnknownSegments ($roundedWarningsRatio%)  UNKNOWN URL SEGMENTS in $ucProject\n";

	# Close all the files!
	close $input;
        close $segmentStatus;
        close $stats;


}
