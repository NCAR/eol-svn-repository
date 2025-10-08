#! /usr/bin/perl -w

##---------------------------------------------------------------------------------------------
#
# The purpose of this script is to create a monthly gzipped tar file of the HPCN station
# readings from the /ingest/hpcn directory and move it to the /net/archive/data/operational/hpcn
# directory. The tar.gz monthly file is also inserted into the database for dataset id 100.015
# and a HTML file is created that displays the monhtly inventory of readings from the individual
# stations. The HTML file contains station information in a table, where the rows represent
# stations and the columns represent data for the data file for the six months contained in the
# table. Missing stations, new stations, outlier file size, and large compression ratios are all
# indicated in the HTML file. New and missing stations are determined by checking the data on the
# /ingest/hpcn/HPCNstations.txt file, which lists a comma separated station id, city, and state on
# each line. If a new station is found, an email is sent to Scot Loehrer and Linda Cully informing
# them that a new station has been added. The new station is also added to the HPCNstations.txt
# file. 
# 
# The HTML file is stored in the /net/web/dmg/html/projects/operational/surface/hpcn/docs/inventory
# directory. Each HTML file contains readings for six months, so a year is broken up into two
# inventory files: one from January through June, the other from July through December. This script
# is to be run as a cron job and automatically gets the current date from the system. The script 
# then searches for readings from the three months prior to the current month, so if the scrip
# is run in April, it will check for readings from January, February, and March. If a gzipped
# tar file has already been archived for a particular month and the new tar file is not different,
# there is no change. However, if the new tar file is different, an email is sent to Scot Loehrer 
# and Linda Cully informing them of the situation. For each month, the 100.015 dataset is updated
# to include the most recent ending date, and max/min longitude and lattitude values are also
# updated as needed. 
#
# UPDATE: This script now includes an HTML inventory file back-up prior to adding the current 
#    month's data.
#
# For testing purposes, the main has a section that can be uncommented that allows the user to
# run the script manually, passing the month and year to be archived as a command line option.
# In addition, testing can be conducted on merlot using the
# MySqlTestDatabase and MySqlTestDataset perl modules. In this case, be sure to comment out the
# MySqlDatabase and MySqlDataset perl modules. In addition, the directories referred to by the
# variables $ingest_dir, $stationsFile, and $binary_archive can be changed to a local directory
# for testing. 
#
# This script runs automatically as a cron script via the crontab on the machine
# named aeolus (as of 23 July 2014). This script runs as the "joss" user on Aeolus (port 23).
# You can find the crontab by logging into aeolus (ssh -p23 joss@aeolus). Then "cd /var/spool/cron".
# List the crontab (crontab -l) or edit the crontab (crontab -e). Note that the crontab -e
# generally puts you into the vi editor.  You must find someone who knows the joss user password to
# do the joss user login. This is a group login (e.g. ask J. Aquino, J. Allison, D. Stott, L. Cully
# for the joss user login.). Note that he system admin group does not know the login.
# You can find a copy of the crontab that runs this script by going into subversion into
# /ingest/crontab/trunk/joss_aeolus_crontab. In that script search for "HPCN" to see the
# location and script (HPCNfiles.pl) that is executed.   Note that in the crontab line for this
# HPCN cron script, you will see 5 numbers or asterisks. These determine the time/days when
# the script will be run. The asterisk slots stand (from left to right) for min, hr, day, month, year.
# The crontab line for this script is
#     30 10 15 * * /net/work/operational/surface/HPCN/trunk/src/HPCNfiles.pl
# so this script runs at 10:30 on the 15th of every month and every year. We run on the 15th
# because S. Loehrer has determined that the HPCN data comes into the /ftp ingest area on
# about the 7th of each month.
#
#
# 23 July 2014 - Update - LE Cully
#    Updated this script. Removed email to Amanda Orin. Investigated and add info (above)
#    about how this script is run via cron.
#
# This script was last updated by Amanda Orin on 07/17/2013.
# This script was completed by Nicolas Metts on 04/26/2011. 
# The original author was Alexandra Boughton.
#
# 9 March 2015 - Update - cbsnyder
#    Removed Orin from e-mail list and added cbsnyder.
#
# March - 10 April 2015 - Update - cbsnyder (Brooks)
#	 Two separate areas of the script were found to be causing problems for the case of new 
#    stations being added. The first was in the readDirectoryForBinary subroutine. Here there
#    was found to be several lines of useless code. The line that implements a split on the 
#    first element of the stationsList was removing the first station and causing it to enter
#    into the newStations array several times. This has been commented out and a note added.
#    The second problem area for new stations was found in the createMonthlyInventoryFile 
#	 subroutine near the top. A for loop appends an "A" to the beginning of each station ID
#	 in the @newStations array even though there already is an "A" on the beginning. This has
#    been commented out and a note added.
#
#    The above issues caused problems with the generation of the HTML inventory file in the case
#    of new stations, however neither caused the major problem in which the entire HTML inventory
#    file became completely distorted and unreadable. This problem occurs under 2 conditions:
#    1. The script is run (normally without MM YYYY arguments) during months January - April. 
#	 2. The scrip is run with the previous HTML inventory file in place. If the script is run
#    during this period without an existing HTML inventory file in the inventory directory, 
#    then the @previousData array remains empty for the first run through createMonthlyInventoryFile()
#    and the script runs without problem. If the HTML inventory file is present and @previousData is
#    populated on the first run (when $monthIndex = 0), for some reason the complicated process of 
#    bulding and rebuilding the HTML inventory file from a combination of html from the previous
#    file in @previousData and new data from the binary files in @inventoryData falls out of alignment
#    and the script seems to end (without error output) before it ever makes it into the
#    createMonthlyInventoryFile() a second time. The specific problem in the code has not been
#    located yet (the problem may require as much as re-designing the majority of the script).
#    However, a workaround has been implemented. In the main() section a conditional statement 
#    allows the previous HTML inventory file to be removed (moved to filename.bak) before running
#    the script for months January - April. The script will then run correctly and look the same
#    as it would had the previous HTML file been present. The is not and can not be removed for
#    months May - June because the previous file is needed to fill in the data for months Jan - Apr. 
#    (otherwise these months would all read "No File"). Fortunately, testing is showing that the
#    problem does not occur when the script is run with the previous file for May or June. This 
#    may have to do with the value that is passed in as a month index to createMonthlyInventoryFile()
#    which does not work when @previousData is populated on the first run through that subroutine. 
#
# 21 April 2015 - Update
# The compression ratio check has been dropped down to 5 from 10 as request by Scot L. - cbsnyder
#
#
##---------------------------------------------------------------------------------------------

use strict;
use lib "/net/work/lib/perl/Utilities";
use DpgDate;
use File::Copy;
use File::Path;
use File::stat;
use File::Basename qw( dirname ); # For calls to script from other directories
use Cwd;
use Cwd qw(abs_path); # For calls to script from other directories
use Class::Struct;
use lib "/net/work/lib/perl/mysql"; # For Production
#use lib "lib"; # For Testing Only

##--------------------------------------------------------------------------
# Libraries to send e-mails  
# uncomment when ready to use function : sendEmail in readDirectoryForBinary
##--------------------------------------------------------------------------

use Email::MIME;
use Email::MIME::Creator;
#use MySqlTestDatabase;			# The test database
#use MySqlTestDataset;			# The test dataset Module
use MySqlMSSFile;
use MySqlFile;
use Archive::Tar;			# To be used with sferic, see lines 397-401
use MySqlDatabase;			# The production database
use MySqlDataset;			# The production dataset Module

##-------------------------------------------------------------------
# global variables
##-------------------------------------------------------------------
#my $database = MySqlTestDatabase->new(("zithupdate"), ("change-456"));		# Database object for testing
my $database = MySqlDatabase->new(("zithupdate"), ("change-999"));

#recepient of emails about stations that are added to HPCNstations.txt 
my $recipients = "cully\@ucar.edu,loehrer\@ucar.edu,cbsnyder\@ucar.edu";
#my $recipients = "cbsnyder\@ucar.edu";  #for testing

my $report = "";

my $archive_ident = "100.015"; 	#The HPCN operational dataset

# Temporary directories for TESTING
#my $ingest_dir = "/net/work/operational/surface/HPCN/ingest";
#my $work_dir = "/net/work/operational/surface/HPCN";
#my $stationsFile = 'HPCNstations.txt';
#my $inactiveStationsFile = "HPCNinactive.txt";
#my $stationsDataFile = "/h/eol/metts/HPCN/HPCNdata.txt";
#my $binary_archive = "/net/work/operational/surface/HPCN/monthlyFiles";
#my $inventoryDir = "/net/web/dmg/html/projects/operational/surface/hpcn/docs/inventory/test";

my $script_dir = dirname(abs_path($0)); # To call the script from other directories as user joss

# Directories to be used in PRODUCTION
my $ingest_dir = "/net/ftp/pub/data/incoming/hpcn"; # Must be on sferic 	#"/net/ingest/hpcn";
my $stationsFile = $script_dir."/HPCNstations.txt"; #"/net/ingest/hpcn/HPCNstations.txt";
my $inactiveStationsFile = $script_dir."/HPCNinactive.txt";
my $work_dir = "/net/work/operational/surface/HPCN";
my $binary_archive = "/net/archive/data/operational/hpcn";
my $inventoryDir = "/net/web/dmg/html/projects/operational/surface/hpcn/docs/inventory";
my @stations;
my $stationFound;

my $logDir = "/net/work/operational/surface/HPCN/logs/"; # For production
#my $logDir = "/net/work/tmp/orin/HPCN/logs/"; # For testing
my $logFile = createLogFile();


#run the script
&main();

##------------------------------------------------------------------------
# @signature void main(int month, int year)
# <p>Take an inventory of the HPCN data that has been ftp'd to eol. Searches for
# files from the previous three months by obtaining the current system time and
# iterating through the previous months, starting with the earliest. So if the
# current month is April, the script will check January, February, and March, in
# that order.
#
##------------------------------------------------------------------------
sub main {
	open(LOGFILE, ">>$logFile") or die "unable to open file $logFile\n";
    
    # save original settings. You could also use lexical typeglobs.
    *OLD_STDOUT = *STDOUT;
    *OLD_STDERR = *STDERR;

    # reassign STDOUT, STDERR
    *STDOUT = *LOGFILE;
    *STDERR = *LOGFILE;
    
    
    
#	print "$stationsFile\n$inactiveStationsFile\n";
	my $monthRange = 0;
	my $halfYear = "";
    # This is how the script is generally run from the cron tab. - Brooks 
	if (scalar(@ARGV) == 0) {
   		my @time = localtime(time);
   		my $month = $time[4];
   		my $year = $time[5] + 1900;
   		my $curMonth = $month;
   		my $curYear = $year;
		my $oldYear = $year;

########################################################################################
#### This code is added in as a work-around to fix a problem with the creation of   ####
#### the monthly inventory HTML files. For months January - April the script will   ####
#### generate a bad HTML file. This is due to the interplay of the month index      #### 
#### when < 4 and the use of the @previousData array (containing the content of the ####
#### HTML from the previous inventory file) to generate the new inventory file.     ####
#### For these months the script has no problem executing when a previous inventory ####
#### file is not present (and does not need a previous file to fill in data for the ####
#### first 3 months, so this workaround should effectively solve the issue. -Brooks ####
########################################################################################

    if ($month < 4) {

        my $begMonth = "07";
        my $endMonth = "12";
        # The beginning and end month for the next 6 month period
        my $nextBegMonth = "01";
        my $nextEndMonth = "06";
        # The next and previous year
        my $nextYear = int($year) + 1;
        my $prevYear = $year;
        my @monthList = ("July", "August", "September", "October", "November", "December");
        if ($month < 7) {
            $begMonth = "01";
            $endMonth = "06";
            $nextBegMonth = "07";
            $nextEndMonth = "12";
            $nextYear = $year;
            $prevYear = int($year) - 1;
            @monthList = ("January", "February", "March", "April", "May", "June");
        }

        my $fileName = sprintf("%s/HPCN_inventory_%s%s_%s%s.html",$inventoryDir, $year, $begMonth, $year, $endMonth);


       my $backupfile = $fileName . "\.bak";

       if (-e $fileName) {
           if (!move($fileName, $backupfile)) {
                printAndLog("error moving file :" . $fileName . "\n");
                return 0;
            }
       }
    }

########################################################################################
#########################################################################################

		
        # The script loops through the previous 3 months.
   		for (my $i = 2; $i >= 0; $i--)
   		{
			# month is the current month (but 0-based so for example April will be 3 not 4)
			$curMonth = $month - $i;
   			if ($curMonth <= 0) {
				$curMonth += 12;
				$curYear = $year - 1;
    			}
			# Check for existing HTML inventory file here.
			if( $curMonth >= 1 && $curMonth <= 6) {
				$monthRange = 1; 
			} else {
				$monthRange = 2;
			}
			if ( $halfYear ne $monthRange.$curYear ) {
				print "(1)Backing up HTMLInventory for half-year $monthRange of $curYear...";
				printAndLog("(1)Backing up HTMLInventory for half-year $monthRange of $curYear...");
				backUpHTMLInventory($monthRange, $curYear);
				print "(2) done.\n";
				printAndLog("(2) done.\n");
			}
			$halfYear = $monthRange.$curYear;
			if ($curMonth < 10)
			{
				$curMonth = "0" . $curMonth;
			}
			print "\n(3)Processing data from $curMonth/$curYear...\n";
			printAndLog("\nProcessing data from $curMonth/$curYear...\n");
			if ($curYear > 2011) { # Only process files post-2011 with the no-arguments call.
				# organizeBinary is where most other subroutines are called. This should run 3 times each time this script is run. -Brooks
				organizeBinary($curMonth, $curYear);
			}
			if ($i > 0) {
				$curYear = $oldYear; # Reset to the current year for the next iteration.
			}
    	}
		sendProcessedEmail($curYear, $monthRange);
		
##  	} elsif (scalar(@ARGV) == 1) {
##		if ($ARGV eq 'clean') {
#			system("rm /net/web/dmg/html/projects/operational/surface/hpcn/docs/inventory/HPCN_inventory_201107_201112.html");
#			system("rm /net/web/dmg/html/projects/operational/surface/hpcn/docs/inventory/test/*");
#			system("rm /net/work/operational/surface/HPCN/monthlyFiles/*");
#			system("rm /net/work/operational/surface/HPCN/tarInventories/test.*");
##		} else {
##			print("Arg: \"$ARGV\"");
##		}
	}
	elsif (scalar(@ARGV) == 2) {
   		my ($month, $year) = @ARGV;
		# Check for existing HTML inventory file here.
		if( $month >= 1 && $month <= 6) {
			$monthRange = 1; 
		} else {
			$monthRange = 2;
		}
		if ( $halfYear ne $monthRange.$year ) {
			print "(4)Backing up HTMLInventory for half-year $monthRange of $year...";
			printAndLog("(4)Backing up HTMLInventory for half-year $monthRange of $year...");
			backUpHTMLInventory($monthRange, $year);
			print "(5) done.\n";
			printAndLog("(5) done.\n");
		}
    	organizeBinary($month,$year);
		sendProcessedEmail($year, $monthRange);
	}
	else {
	 	printf("Usage: HPCNfiles.pl MM YYYY\n");
	 	
    	# restore STDOUT/STDERR
		*STDOUT = *OLD_STDOUT;
		*STDERR = *OLD_STDERR;
		
		close(OLD_STDOUT);
		close(OLD_STDERR);
		close(LOGFILE);
		
                exit(1);
	}
    
    print "(6) Restore stdout/stderr and END program!\n";
 
    # done, restore STDOUT/STDERR
    *STDOUT = *OLD_STDOUT;
    *STDERR = *OLD_STDERR;
    
    close(OLD_STDOUT);
    close(OLD_STDERR);
    close(LOGFILE);
	
	
}
##-----------------------------------------------------------------------------
## @signature void organizeBinary( int month, int year)
## this module will find all the files based on the format of the file name,
## it will check for missing files, move the files to the archive/operational location, 
## and inserts the files into the database to update the dataset. 

## @input $month, the month of the data that needs to be processed
## @input $year, the year of the data that needs to be processed

##-----------------------------------------------------------------------------
sub organizeBinary{
	my $month = shift;
	my $year = shift;

	# get the files from the ingest directory
	my ($f, $n) = &readDirectoryForBinary($month, $year);
	my @files = @$f;
	my @newStations = @$n;
	my @inventoryList;
	# The max and min lattitude and longitude for this month
	my $maxLat = -90;
	my $minLat = 90;
	my $maxLong = -180;
	my $minLong = 180;
	my $datesFound = 0;
	my $endDay = -1;
	my $julDay = "";
###	# remove "." and ".." from the list of files
###	shift(@files);
### 	shift(@files);
 	
	# Get a list of this missing files for this month and year
	my @missingFiles = createFileInventory($year, $month);
	my @saveFiles;
	databaseConnect();
	foreach my $file (@files) {
		my $fMonth = convertJulianDay($file);
		my $fYear = fileYear($file);
		my $fStation = fileStation($file);
		if ($fMonth == $month) {
			if ($fYear == $year) {
				$julDay = fileMonthJulian($file);
				push (@saveFiles, $file);
			}
		}
	}
	# Create a directory and copy the files for the current month into it
	my $tempDir = sprintf("%s/%s/%s", $work_dir, $year, $month );
	mkpath($tempDir);
	my @copyFiles;
	foreach my $mFile (@saveFiles) {
		my $start = sprintf("%s/%s",$ingest_dir,$mFile);
    		# Use the current directory
		my $end = cwd();

    		if (!copy($start,$end)) {
			#print "Error copying file :" . $mFile . "\n\tFrom:" . $start . "\n\tTo:" . $end . "\n";
			printAndLog("Error copying file :" . $mFile . "\n\tFrom:" . $start . "\n\tTo:" . $end . "\n");
			return 0;
		}
		$end = $tempDir;
		if (!copy($start, $end)) {
			#print "Error copying file :" . $mFile . "\n\tFrom:" . $start . "\n\tTo:" . $end . "\n";
			printAndLog("Error copying file :" . $mFile . "\n\tFrom:" . $start . "\n\tTo:" . $end . "\n");
			return 0;
		}
		my $tempFile = $mFile;
		# Make a text file that inventories the current files in the /ingest/hpcn
		# directory. If an archive file has already been created for this month
		# and year then make a new text file to compare with the old one. This
		# allows us to know if new files have been added to the ingest dir 
		# since the previous archive file was created.
		if (scalar(@saveFiles) > 0) {
			my $tarInventory = "$work_dir/tarInventories/$year.$month.old.txt";
#			my $tarInventory = "$work_dir/tarInventories/test.$year.$month.old.txt";
			if (-e ( $tarInventory)) # If $tarInventory exists
			{
				$tarInventory = "$work_dir/tarInventories/$year.$month.new.txt";
				#$tarInventory = "$work_dir/tarInventories/test.$year.$month.new.txt";
			}
			open(TARINVENTORY, ">$tarInventory") or die "Could not open $tarInventory";
			my @sortedFiles = sort(@saveFiles);
			for my $s (@sortedFiles) {
				my $fStat = stat("$ingest_dir/$s");
				my $fSize = $fStat->size;
				my $modDate = $fStat->mtime;
				print TARINVENTORY "$s $fSize $modDate\n";
			}
		}	
		$tempFile =~ s/$work_dir//g;
		my $copiedFile = $tempDir . "/" . $tempFile;
		push(@copyFiles, $copiedFile);	
	}
	my @missingStations = ();
	my %brokenFiles = ();
	# Take the data from the missingFiles array and append an "A" to
	# each station number and remove the trailing newline character
	foreach my $m (@missingFiles) {
		my $newM = $m;
		$newM =~ s/\"//g;
		$newM = "A" . $newM;
		$newM =~ s/\n//g;
		push(@missingStations, $newM);
	}
	# An array to store the data to be written to the inventory webpage
	my @htmlInventoryData = ();
	my $mIndex = 0;
	# A hash that maps a station number to a String containing the city and
	# state for that station separated by a comma
	my %cityStateInfo = getCityStateFromFile();
	
	# If there are no station readings for this month, add O for filesize and
	# NA for the compression ratio for each station in the htmlInventoryData
	# array
	if (scalar(@copyFiles) == 0) {
		foreach my $m (@missingStations)
		{
			my $mName = $m;
			my $cityState = $cityStateInfo{$mName};
			my @cityStateList = split(/,/, $cityState);
			my $city = shift(@cityStateList);
			my $state = shift(@cityStateList);
			push(@htmlInventoryData, $mName);
			push(@htmlInventoryData, $city );
			push(@htmlInventoryData, $state );
			push(@htmlInventoryData, "0" );
			push(@htmlInventoryData, "NA" );
			$mIndex++;
		}

	}
	foreach my $copyFile (@copyFiles) {
		my $fileName = $copyFile;
		my $modFileName = $fileName;
		$modFileName =~ s/$tempDir\///g;
		$modFileName =~ m/(A\d+)\./;
		my $stationNumber = $1;
		# Get the size of the .Z file
		my $compressedSize = 1;
		$compressedSize = (stat($fileName)->size)/1024;
		# Uncompress the .Z file and get the size of the uncompressed file
		my @args = ('gunzip', '-f', $fileName) or warn "$fileName failed the gunzip.\n"; # For sferic, since uncompress is not a utility on this server. Comment this if you wish to use "uncompress" below.
        	#my @args = ('uncompress', '-f', $fileName); # Uncomment if desired for testing.
        	my $uncompResult = system(@args);	
		$fileName =~ s/\.Z//g;
		my $uncompressedSize = 0;
		if ($fileName) {
			my $ferr = "";
			my $fs = stat($fileName) or $ferr.="Unable to stat $fileName\n";
			if ( $ferr eq "" ) {
				$uncompressedSize = (stat($fileName)->size)/1024;
			} else {
				$uncompressedSize = 0;
				printAndLog($ferr);
				$brokenFiles{ $stationNumber } = $modFileName;
			}
		}
		# Calculate the compression ratio from the compressed and uncompressed file sizes
		my $ratio = sprintf("%.2f", $uncompressedSize/$compressedSize);
		$compressedSize = int(sprintf("%.0f", $uncompressedSize)); 
		my $before = 1;
		# In order to have the data in sorted order, insert the data for missing files
		# in the appropriate location
		if ($mIndex < scalar(@missingStations)) {
			my $tempName = $stationNumber;
			$tempName =~ s/A//g;
			# There may be multiple missing stations that come before the current station
			# data, so push that data on the htmlInventoryData array before the current data
			while ($before == 1 and ($mIndex < scalar(@missingStations))) {
				my $mName = $missingStations[$mIndex];
				$mName =~ s/A//g;
				if (($tempName - $mName) > 0) {
					$mName = "A" . $mName;
					my $cityState = $cityStateInfo{$mName};
					my @cityStateList = split(/,/, $cityState);
					my $city = shift(@cityStateList);
					my $state = shift(@cityStateList);
					push(@htmlInventoryData, $mName);
					push(@htmlInventoryData, $city );
					push(@htmlInventoryData, $state );
					push(@htmlInventoryData, "0" );
					push(@htmlInventoryData, "NA" );
					$mIndex++;
				}
				else {
					$before = 0;
				}
			}
		}
		my $city = "";
		my $state = "";
		# If the city, state information is already available, get it from the hash
		if (exists($cityStateInfo{$stationNumber})) {
			my $cityState = $cityStateInfo{$stationNumber};
			my @cityStateList = split(/,/, $cityState);
			$city = shift(@cityStateList);
			$state = shift(@cityStateList);
		}
		# If the city, state information is not available, add it to the file containing
		# this data
		else {
			my $cityState = &getCityState($fileName);
			my @cityStateList = @$cityState;
			$city = shift(@cityStateList);
			$state = shift(@cityStateList);
			addStationInfo($stationNumber, $city, $state);
		}
		# Push the data for the current station on the htmlInventoryData array
		push(@htmlInventoryData, $stationNumber);
		push(@htmlInventoryData, $city);
		push(@htmlInventoryData, $state);
		if ( exists $brokenFiles{ $stationNumber } ) {
			push(@htmlInventoryData, $compressedSize );
			push(@htmlInventoryData, "Broken" );
		} else {
			push(@htmlInventoryData, $compressedSize);
			push(@htmlInventoryData, $ratio);
		}
		# Since the end day for the month will be the same for all files in the current month,
		# we only need to find it once
		if ($datesFound == 0) {
			my $tempFile = $copyFile;
			$tempFile =~ s/$tempDir\///g; 
			$endDay = endDay(convertJulianDay($tempFile), fileYear($tempFile));
			$datesFound = 1;
		}
		my $tempLat = getMaxlatFile($copyFile);
		my $tempLong = getMaxlongFile($copyFile);
		# Check if the maxLat, minLat, maxLong, and minLong attributes need to be updated
		if ($tempLat > $maxLat) {
			$maxLat = $tempLat;
		}
		if ($tempLat < $minLat) {
			$minLat = $tempLat;
		}
		if ($tempLong > $maxLong) {
			$maxLong = $tempLong;
		}
		if ($tempLong < $minLong) {
			$minLong = $tempLong;
		}
	}
	# If there are stations with missing data that come after the last station with data,
	# get that data too
	while ($mIndex < scalar(@missingStations)) {
		my $stationName = $missingStations[$mIndex];
		my $cityState = $cityStateInfo{$stationName};
		my @cityStateList = split(/,/, $cityState);
		my $city = shift(@cityStateList);
		my $state = shift(@cityStateList);
		push(@htmlInventoryData, $stationName);
		push(@htmlInventoryData, $city);
		push(@htmlInventoryData, $state);
		push(@htmlInventoryData, "0" );
		push(@htmlInventoryData, "NA" );
		$mIndex++;
	}
	#tar the files up so that they can be added to the database
	my $skipMonthlyInventory = 0;
	if ($julDay ne "")
	{
		# The Arhive::Tar perl module is not being used because it is not available
		# on tsunami. For now, the tar functionality is being handled by a system
		# call. 
		my $tar = Archive::Tar->new();				# Uncomment when using the Archive::Tar module
		$tar->add_files(@saveFiles);				# Uncomment when using the Archive::Tar module
		my $destination = sprintf("hpcn.%s.%s",$julDay, $year . ".tar");
		#system("tar -cvf $destination *.$julDay.$year.Z");	# Comment out when using the Archive::Tar module
		$tar->write($destination);				# Uncomment when using the Archive::Tar module
		# Remove the .Z files that were copied to this directory
		system("rm *.Z");
		# Gzip the tar file and place it in the archive directory
		system("gzip $destination");
		$destination = $destination . ".gz";
		my $archiveFile = $destination;
		$archiveFile = $binary_archive . "/" . $archiveFile;
		if (-e $binary_archive . "/" . $destination) {
			# If there is already an archive file for this month and year in the binary_archive directory
			# check the text file that lists the contents of the previous archive file with the text file
			# that lists the contents of the new archive file. If they are different, then either a new file
			# has been added or a previous file has been replaced with a file that has a different size.
			my $oldTar = "$work_dir/tarInventories/$year.$month.old.txt";
			my $newTar = "$work_dir/tarInventories/$year.$month.new.txt";
			#my $oldTar = "$work_dir/tarInventories/test.$year.$month.old.txt"
			#my $newTar = "$work_dir/tarInventories/test.$year.$month.new.txt";
			if (-e $newTar) {
				my $diff = system("diff $oldTar $newTar");
				if ($diff == 0) {
					system("rm $destination");
					$skipMonthlyInventory = 1;
				}
				else {
					my $curVersion = 1;
					while (-e $archiveFile . "." . $curVersion) { # If current version of the $archiveFile exists
						$curVersion += 1;
					}
					my $newTarFile = $destination . "." . $curVersion;
					system("mv $destination $newTarFile");
					system("mv $newTarFile $binary_archive");
					sendEmail("[HPCN] New tar file for $month/$year", "A new tar file has been created for $month/$year that is different" .
					" than the existing file $archiveFile. The previous file has the same name and the and the new file is named" .
					" $binary_archive/$destination.$curVersion.");
				}
			}
		}
		else {
			system("mv $destination $binary_archive");
			# insert the tar file into the database
			if (insertFile($destination, $binary_archive, $year, $month, $endDay, $maxLat, $minLat, $maxLong, $minLong) == 0) {
				#print "$destination was not inserted.\n";
				printAndLog("$destination was not inserted.\n");
			}
		}
	}
	# Create the monthly inventory page even if there are no readings for this month and year.
	if ($skipMonthlyInventory == 1) {   
		# This sub-routine creates the monthly inventory HTML file. This is called 3 times, each time rebuilding the HTML file and relying on the content of the file that was previously created.
		createMonthlyInventoryFile($month, $year, $tempDir,\@htmlInventoryData, \@newStations, \@missingStations);
	} else {
		#print "skipping monthly inventory (monthly inventory already exists and has not changed)\n";
		printAndLog("skipping monthly inventory (monthly inventory already exists and has not changed)\n");
	}
	databaseDisconnect();
	$tempDir =~ s/\/$month//g;
	# Remove the temporary directory that was created
	system("rm -rf $tempDir");
	#print "processing complete\n";
	printAndLog("processing complete\n");
}

##------------------------------------------------------------------------
# @signature String  getJulianDay
# Returns the first Julian day for the given month and year
#
# @input $month the calendar month
# @input $year the calendar year
##------------------------------------------------------------------------
sub getJulianDay {
	my $month = shift;
	my $year = shift;
	my $leapYear = isLeapYear($year);
	my %julianDays = ("01", 1, "02", 32, "03", 60, "04",91, "05", 122, "06", 152, 
			"07", 182, "08", 213, "09", 244, "10", 274, "11", 305, "12", 335);
	if ($leapYear and $month > 2) {
		return $julianDays{$month} + 1;
	}
	else {
		return $julianDays{$month};
	}
}

##------------------------------------------------------------------------
# @signature int isLeapYear
# Returns 1 if the year is a leap year, 0 otherwise.
#
# @input $year the year
##------------------------------------------------------------------------
sub isLeapYear {
	my $year = shift;
	if ($year % 400 == 0) {
		return 1;
	}
	elsif ($year % 4 == 0 and $year % 100 != 0)
	{
		return 1;
	}
	else {
		return 0;
	}
}

##------------------------------------------------------------------------
# @signature String[]  createFileInventory( int $year, int $month )
# <p>based on the file year, return the directory where the file should go, 
# if the directory doesn't exist then create it</p>
#
# @input $file, name of file to be archived.
# @input $year, the year of the file to be archived.
# @input $binary_archive, directory of the archive location, where a new
# @input @actualFiles, the list of files that are in the ingest directory.
# folder maybe created, or where the correct folder exists.
##------------------------------------------------------------------------
sub createFileInventory{
 	my $year = shift;
 	my $month = shift;
	my ($f, $n) = &readDirectoryForBinary($month, $year);
	my @actualFiles = @$f;
 	
### 	#remove "." and ".." from the list of files
### 	shift(@actualFiles);
### 	shift(@actualFiles);

 	open (STATIONS, "<$stationsFile") or die ("could not open $stationsFile\n");	
 	my @stationsList = <STATIONS>;
	@stationsList = sort(@stationsList);
 	my @filesForStation;
 	my $station;
 	$stationFound = 0;
    	my @unmatchedFiles;
    	my $actualFile;
    
    
 	foreach my $s (@stationsList) {
		chomp($s);
		my @line = split(/,/,$s);
		$station = $line[0];
		$station =~ s/A//g;
 		$stationFound = 0;
 		if(toString($station) ne "") { #avoid processing empty strings
	
	 		#build array of files for current station	
	 		foreach $actualFile (@actualFiles) {
	 			if (expectedStationFound($station, $actualFile, $month, $year)==1) {
	 				if ($actualFile ne "") {	
	 				push(@filesForStation, $actualFile);
	 				$stationFound =1;
	 				}
	 				 
	 			}
 		}
			if($stationFound != 1) { 
				#add the station to the list of missing files
				push(@unmatchedFiles,$station);
			}
 		} 
 	}
# 	close (INVFILE);
 	close (STATIONS);
	return @unmatchedFiles;
 }
 
##------------------------------------------------------------------------
# @signature file createFileInventory(,String $binary_archive, )
# <p>Based on the station, current month, and current year, this will return true
# if the filename matches the station, and the date of the file is within 3 months
# of the current date.</p>
#
# @input $file, name of file to be archived.
# @input $year, the year of the file to be archived.
# @input $binary_archive, directory of the archive location, where a new
# @input @actualFiles, the list of files that are in the ingest directory.
# folder maybe created, or where the correct folder exists.
##------------------------------------------------------------------------ 
 
 
sub expectedStationFound{
 	my ($stationPrefix) = shift; 
 	$stationPrefix =~ s/\\n//g;
 	$stationPrefix =~ s/"//g;
 	my $currentFile = shift;
 	my $stationTest = $currentFile;
 	my $cmonth = shift;
 	my $cyear = shift;
 	if ($stationTest =~ /^A(\d+)\.\d+\.\d+\.Z$/) {
 		 		
 		if (($1 - $stationPrefix) == 0) {
 		
	 		if ($currentFile =~ /\w+\.(\d\d?\d?)\.(\d\d\d\d)\.Z/) {
	 			my $fDay = $1;
	 			my $fYear = $2;

	 			if ((convertJulianDay($currentFile) eq $cmonth) && ($fYear == $cyear)) {
	 				
	 				$stationFound = 1;
	 				return 1;
	 				} 
	 			}
 			}
 		}
 	return 0;
 }

##------------------------------------------------------------------------
# @signature void databaseConnect()
# <p>opens a databse connection</p>
##------------------------------------------------------------------------
sub databaseConnect {
# use the global variable #database to establish a database connection
    
    $database->connect();
    $database->{AutoCommit}=0;
	print("Connected to database\n");
	
	
}
sub databaseDisconnect {
	#disconnect the global variable $database
    $database->disconnect();
}
##------------------------------------------------------------------------
# @signature int getMaxlongFile(String file)
# <p>return the long from the file, or 0</p>
#
# @input $fileToOpen, a file name.
# @output maxlong
##------------------------------------------------------------------------
sub getMaxlongFile {
	my $fileToOpen = shift;
	$fileToOpen =~ s/\.Z//g;
	my $err = "";
	open (DATA, $fileToOpen) or $err.= "Could not open $fileToOpen\n";
	if ($err eq "") {
		my @data = <DATA>;
		close (DATA);
		foreach my $line (@data) {
			if($line =~ /.*Long\.\(deg\)=\s?(\d+\.\d+).*Elev/) {
				return $1;
			}
			
		}
	}
	return 0;
}
##------------------------------------------------------------------------
# @signature int getMaxlatFile(String file)
# <p>return the lat from the file, or 0</p>
#
# @input $fileToOpen, a file name.
# @output maxlat
##------------------------------------------------------------------------
sub getMaxlatFile {
	my $fileToOpen = shift;
	$fileToOpen =~ s/\.Z//g;
	my $err = "";
	open (DATA, $fileToOpen) or $err.= "Could not open $fileToOpen\n";
	if ($err eq "") {
		my @data = <DATA>;
		close (DATA);
		foreach my $line (@data) {
			if($line =~ /.*Lat\.\(deg\)=\s?(\d+\.\d+).*Long/) {
				return $1;
				
			}
			
		}
	}
	return 0;
}
##-----------------------------------------------------------------------------
# @signature String insertFile(String file)
# <p>Insert the specified file into the database.</p>
#
# @input $file The name of the file being inserted.
# @input $fileDir The directory where $file is located.
# @input $fileYear The year of the file being inserted.
# @input $month The month of the file to be inserted.
# @input $endDay The last day of the month for $month.
# @input $maxLat The maximum latitude from the file being inserted.
# @input $minLat The minimum latitude from the file being inserted.
# @input $maxLong The maximum longitude from the file being inserted.
# @input $minLong The minimum longitude from the file being inserted.
# @output $msg An error message that was generated from the insert or the empty
# String if the insert completed successfully.
##-----------------------------------------------------------------------------
sub insertFile {
    my ($file) = shift;
    my $fileDir = shift;
    my $fileYear = shift;
    my $month = shift;
    my $endDay = shift;
    my $maxLat = shift;
    my $minLat = shift;
    my $maxLong = shift;
    my $minLong = shift;
    # Create the file information.
    my $mysql = MySqlFile->new();
    $mysql->setDatasetArchiveIdent($archive_ident);
    $mysql->setDirectory($fileDir);
    #print("my file is: $file");
    my $directory = sprintf("%s",$fileDir);
    #print("my dir is: $directory\n");
    $mysql->setFile($directory,$file);
    $mysql->setFormatId(57);
    $mysql->setBeginDate($fileYear, $month, 1, 0, 0, 0);
    $mysql->setEndDate($fileYear, $month, $endDay, 23, 59, 59);

    $mysql->setMaxlat($maxLat);
    $mysql->setMaxlon($maxLong);
    $mysql->setMinlat($minLat);
    $mysql->setMinlon($minLong);

    #Insert the file
    my $msg = $mysql->insert($database);

    # Only worry about the dataset changes if the file insert was successful
    if ($msg eq "") {
	my $commitMsg = $database->commit();
	if ($commitMsg eq "") {
		print ("insert was successful\n");
	}
	else
	{
		#print "Error inserting file: $commitMsg\n"; 
		printAndLog("Error inserting file: $commitMsg\n"); 
	}
	# Create and load the dataset.
	#my $dataset = MySqlTestDataset->new($archive_ident); # For testing
	my $dataset = MySqlDataset->new($archive_ident); # For production
	$msg = $dataset->selectDataset($database);

	# Only continue if the dataset was retreived successfully.
	if ($msg eq "") {

	    # Load and format dates
	    my $file_begin = $mysql->getBeginDate(); $file_begin =~ s/[\s:-]//g;
	    my $file_end = $mysql->getEndDate(); $file_end =~ s/[\s:-]//g;
	    my $begin = $dataset->getBeginDate(); $begin =~ s/[\s:-]//g;
	    my $end = $dataset->getEndDate(); $end =~ s/[\s:-]//g;
	    
	    # Compare the dates and change as necessary.
	    if ($file_begin < $begin) { 
		$dataset->setBeginDate(split(/[\s:-]/,$mysql->getBeginDate())); }
	    if ($file_end > $end) { 
		$dataset->setEndDate(split(/[\s:-]/,$mysql->getEndDate())); }
		
            # Load and format lats and longs from the file
	    my $file_maxlat = $mysql->getMaxlat(); #$file_maxlat =~ s/[\s:-]//g;
	    my $file_maxlong = $mysql->getMaxlon(); #$file_maxlong =~ s/[\s:-]//g;
	    my $file_minlat = $mysql->getMinlat(); #$file_minlat =~ s/[\s:-]//g;
	    my $file_minlong = $mysql->getMinlon(); #$file_minlong =~ s/[\s:-]//g;
	    #print "File Data:\n\tMax Lat" . "\tMax long". "\tMin lat" . "\tMin long\n";
	    #print "\t$file_maxlat\t$file_maxlong\t$file_minlat\t$file_minlong\n";
		
	    # Load and format lats and longs from the dataset
	    my $Dmaxlat = $dataset->getMaxlat(); #$Dmaxlat =~ s/[\s:-]//g;
	    my $Dmaxlong = $dataset->getMaxlon(); #$Dmaxlong =~ s/[\s:-]//g;
	    my $Dminlat = $dataset->getMinlat(); #$Dminlat =~ s/[\s:-]//g;
	    my $Dminlong = $dataset->getMinlon(); #$Dminlong =~ s/[\s:-]//g;
	    #print "Dataset data:\n\tMax Lat \tMax long\tMin lat \tMin long\n";
            #print "\t$Dmaxlat\t$Dmaxlong\t$Dminlat\t$Dminlong\n";	    

	    #compare the lats and longs and change as necessary
	     if ($Dminlat < $file_minlat) { 
		$dataset->setMinlat(split(/[\s:-]/,$mysql->getMinlat())); }
	    if ($Dmaxlat > $file_maxlat) { 
		$dataset->setMaxlat(split(/[\s:-]/,$mysql->getMaxlat())); }
	    if ($Dminlong < $file_minlong) { 
		$dataset->setMinlon(split(/[\s:-]/,$mysql->getMinlon())); }
	    if ($Dmaxlong > $file_maxlong) { 
		$dataset->setMaxlon(split(/[\s:-]/,$mysql->getMaxlon())); }
	    
	    
	    # Update the dataset with the new dates.
	    $msg = $dataset->updateDataset($database);
	}
    }
    #check for a problem inserting the file into the database
    if($msg ne "") {
    	print($msg . "\n"); 
    	$database->disconnect();
    	return 0;
    }
    else {
	$database->commit();
    }
	
#     Always disconnect cleanly.
#    $database->disconnect();

    return 1;
}

sub toString{
	my $string = shift;
	
	if($string =~ /(.*)/) {
		return $1;
	}
	return 0;

}


sub fileMonthJulian{
	my $fileMonth = shift;
	
	if($fileMonth =~ /^A\d+\.(\d+)\.\d+\.Z$/) {
		return $1;
	}
	return 0;

}

##------------------------------------------------------------------------
# @signature int fileYear(String $file)
# <p>uses regex to pull out the year of the file</p>
#
# @input $file, a file name.
# @output $fileYear, the year of the file.
##------------------------------------------------------------------------
sub fileYear{
	my $fileYear = shift;
	
	if($fileYear =~ /^A\d+\.\d+\.(\d+)\.Z$/) {
		return $1;
	}
	return 0;

}

##------------------------------------------------------------------------
# @signature int endDay(String $month, String $year)
# return the end day of the file based on the month 
# of the file.
#
# @input $month the monthto find the end day for
# @input $year the year for the month
# @output an int representing the last day of $month
##------------------------------------------------------------------------
sub endDay{
	my $month = shift;
	my $year = shift;
	if ($month eq "01") {
		return 31;
	}
	elsif ($month eq "02" && isLeapYear($year)) {
		return 29;
	}
	elsif ($month eq "02") {
		return 28;
	}
	elsif ($month eq "03") {
		return 31;
	}
	elsif ($month eq "04") {
		return 30;
	}
	elsif ($month eq "05") {
		return 31;
	}
	elsif ($month eq "06") {
		return 30;
	}
	elsif ($month eq "07") {
		return 31;
	}
	elsif ($month eq "08") {
		return 31;
	}
	elsif ($month eq "09") {
		return 30;
	}
	elsif ($month eq "10") {
		return 31;
	}
	elsif ($month eq "11") {
		return 30;
	}
	elsif ($month eq "12") {
		return 31;
	}
	return 0;

}	

##------------------------------------------------------------------------
# @signature void convertJulianDay(String $file)
# <p>convert the julian day in the file name to a month
# since all files are recieved on the first of the month the 
# day is not needed.</p>
#
# @input $file, a file name.
# @output $fileMonth, the month of the file.
# @warning 
##------------------------------------------------------------------------
sub convertJulianDay{
	my $fileMonth = shift;
	
	if($fileMonth =~ m/^A\d+\.(\d+)\.\d+\.Z$/) {
	my $month = "0";
	if ($1 ne "")
	{
	$month = $1;
	}
	if ($month eq 1) {
		return "01";
	}
	elsif($month eq 32) {
		return "02";
	}
	elsif($month eq 61 || $month eq 60) {
		return "03";	
	}
	elsif($month eq 92 || $month eq 91) {
		return "04";	
	}		
	elsif($month eq 122 || $month eq 121) {
		return "05";	
	}
	elsif($month eq 153 || $month eq 152) {
		return "06";	
	}
	elsif($month == 183 || $month == 182) {
		return "07";	
	}
	elsif($month == 214 || $month == 213) {
		return "08";	
	}
	elsif($month == 245 ||$month== 244) {
		return "09";	
	}
	elsif($month == 275 ||$month== 274) {
		return "10";	
	}
	elsif($month == 306 || $month == 305) {
		return "11";	
	}
	elsif($month == 336 || $month == 335) {
		return "12";	
	}
}
	return "0";

}

##------------------------------------------------------------------------
# @signature String[] readDirectoryForBinary(int month, int year)
# <p>Find all of the files in the ingest directory that are binary files.</p>
#
# @input $month The month to be found.
# @input $year The year of the month.
# @output files[] The names of the binary files in the ingest directory.
# @output newStations[] the names of new stations in the ingest directory
##------------------------------------------------------------------------
sub readDirectoryForBinary {
    my $month = shift;
    my $year = shift;
    my $INGEST;
    my @files;
    my $subject;
    my $body;
    my @newStations;
    open (STATIONS, "$stationsFile") or die ("could not open $stationsFile\n");
    my @stationsList = <STATIONS> ; 
    @stationsList = sort(@stationsList);
    my @newStationsFile;

    # This $count variable is not used anywhere. -Brooks
#    my $count =0;

    # This section commented out below has no use in this script. The first line is attempting
	# to use split on the first element of the stations array. 
	# The result is that the first station is removed from the stations
	# list and added to the new stations list several times. This code has been found to create
	# problems when new stations are added and has therefore been commented out. - Brooks

#    my @line = split(/,/, shift(@stationsList));
#    my $station = $line[0];
#    $station =~ s/"//g;
#    $station =~ s/A//g;
#    my $place = 0;

    my $fStation;
    opendir($INGEST, $ingest_dir) || die("Cannot read $ingest_dir\n");
    
    
    foreach my $file (sort(readdir($INGEST))) {
		# If the file matches the format A######.###.####.Z (number of # can vary) then add it to @files
		if($file =~ /^(A\d+)\.\d+\.\d+\.Z$/) {
			$fStation = $1;
		        if (not (grep /^$fStation/, @stationsList)) {
                        	push(@newStations, $fStation);
			}
			push(@files,$file);
		}
   }
###
###print "\n";
###foreach my $ff (@files) {
###  print "$ff\n";
###}
###
    closedir($INGEST);
    close($stationsFile);
    return (\@files, \@newStations);
}
sub searchForStation{
	my $searchFor= shift;

	foreach my $station (@stations) {
		if ($searchFor == $station) {
			return 1;
		}
	
	}
	return 0;
}
##------------------------------------------------------------------------
# @signature int fileYear(String $file)
# <p>uses regex to pull out the station of the file</p>
#
# @input $file, a file name.
# @output $fileStation, the station of the file.
##------------------------------------------------------------------------
sub fileStation{
	my $fileStation = shift;
	
	if($fileStation =~ /^A(\d\d\d\d\d\d)\.\d\d?\d?\.\d\d\d\d\.Z$/) {
		if (!searchForStation($1)) {
			push(@stations,$1);
		}
		return $1;
	}
	return " ";

}
##-----------------------------------------------------------------------------
# @signature void sendEmail(String subject, String body)
#
# @input $subject The subject of the email.
# @input $body The message of the email.
##-----------------------------------------------------------------------------
sub sendEmail {
    my ($subject,$body) = @_;
    my $sender = "joss\@eol.ucar.edu";
    my $reply_to = "stott\@eol.ucar.edu";
    
    my @parts = (Email::MIME->create(attributes => { content_type => "text/plain" },
				     body => $body."\n\n--\nFor full details, please see ".$logFile."\n"));

    my $email = Email::MIME->create(parts => [ @parts ]);

    $email->header_set("From" => $sender);
    $email->header_set("Reply-To" => $reply_to);
    $email->header_set("To" => $recipients);
    $email->header_set("Subject" => $subject);

    open(my $SENDMAIL,"|/usr/lib/sendmail -t") || die("Unable to open sendmail\n");
    printf($SENDMAIL $email->as_string());
    close($SENDMAIL);
}

##-----------------------------------------------------------------------------
# @signature String[]
#
# @input $fileName the name of the file to retrieve the city and state from
# @output @cityState an array containing the city as the first element and the state
# as the second element
##-----------------------------------------------------------------------------
sub getCityState {
	my $fileName = shift;
	my $file = open(DATA, $fileName) or die ("Couldn't open $fileName\n");
	my @data = <DATA>;
	close(DATA);
	my @cityState;
	foreach my $line(@data) {
		my $city = "";                                        
		my $state = "";
		if($line =~ /Lat\.\(deg\)=\s?\d+\.\d+\s+Long\.\(deg\)=\s?\d+\.\d+\s+Elev\.\(m\)=\s?\d+\.$/) {
			my $infoLine = $line;
			$infoLine =~ s/Lat\.\(deg\)=\s?\d+\.\d+\s+Long\.\(deg\)=\s?\d+\.\d+\s+Elev\.\(m\)=\s?\d+\.//g;
			chomp($infoLine);
			$infoLine = trim($infoLine);
			my @words = split(/ /, $infoLine);
			$state = pop(@words);
			$city = join(" ", @words);
			$city = trim($city);
			push(@cityState, $city);
			push(@cityState, $state);
        	}
	}
	return \@cityState;
}

##-----------------------------------------------------------------------------
# @signature string
#
# This function takes a string as an argument and returns the same string with
# all leading and trailing whitespace removed

# @input $ the string to have spaces trimmed
# @output the input string with leading and trailing spaces removed
##-----------------------------------------------------------------------------
sub trim($) {
	my $string = shift;
	$string =~ s/^\s+//;
	$string =~ s/\s+$//;
	return $string;
}


##-----------------------------------------------------------------------------
# @signature %stations getInactiveStationsFromFile
#
# This function creates an array inactive stations from the inactive stations
# file.
##-----------------------------------------------------------------------------
sub getInactiveStationsFromFile {
	my $f = open(DATA, $inactiveStationsFile);
	my @data = <DATA>;
	close(DATA);
	my @inactiveStations = ();
	for my $st(@data) {
		chomp($st);
		$st =~ m/(A\d+),(.*)/; 
		my $station = $1;
		push(@inactiveStations, $station);
	}
	return (@inactiveStations);	
}


##-----------------------------------------------------------------------------
# @signature %stations getCityStateFromFile
#
# This function creates a hash of where the key is a station and the value is
# the city and state for that station.
##-----------------------------------------------------------------------------
sub getCityStateFromFile {
	my $f = open(DATA, $stationsFile);
	my @data = <DATA>;
	close(DATA);
	my %stations = ();
	for my $st(@data) {
		chomp($st);
		$st =~ m/(A\d+),(.*)/; 
		my $station = $1;
		my $cityState = $2;
		$stations{$station} = $cityState;
	}
	return (%stations);	
}


##-----------------------------------------------------------------------------
# @signature void addStationInfo
#
# This function adds a new station to the HPCNstations.txt file, which contains
# each station ID along with the city,state for that station
# 
# @input $station the station ID
# @input $city the city where the station is located
# @input $state the state where the station is located 
##-----------------------------------------------------------------------------
sub addStationInfo {
	my $station = shift;
	my $city = shift;
	my $state = shift;
	open(DATA, ">>$stationsFile");
	print DATA "$station,$city,$state\n";
	close(DATA);
}
##-----------------------------------------------------------------------------
# @signature void createMonthlyInventoryFile
#
# @input $month the month of the data
# @input $year the year of the data
# @input $tempDir the directory where the station files are stored
# @input @inventoryData an array containing the data for each station for this
# this month. The data contained is the station ID, city, state, uncompressed
# file size and the compression ratio
# @input @newStations an array of the newstations
# @input @missingStations an array of the missing stations
##-----------------------------------------------------------------------------
sub createMonthlyInventoryFile {
	my $month = shift;
	my $year = shift;
	my $tempDir = shift;
	my $chgStnDataList = ""; # Used to send out an e-mail if inventory's current month changes.
	# Dereference the arrays
	my ($i, $n, $m) = @_;
	# Convert the array references to arrays
	my @inventoryData = @$i;
	my @newStations = @$n;
	my @missingStations = @$m;

	# Retrieve the list of inactive stations
 	my @inactiveStations = ();
	@inactiveStations = getInactiveStationsFromFile(); 
	# --------------------------------------

	my $inventoryDir = "/net/web/dmg/html/projects/operational/surface/hpcn/docs/inventory";
	#my $inventoryDir = "/net/web/dmg/html/projects/operational/surface/hpcn/docs/inventory/test";
	# The beginning and end month for this 6 month period
	my $begMonth = "07";
	my $endMonth = "12";
	# The beginning and end month for the next 6 month period
	my $nextBegMonth = "01";
	my $nextEndMonth = "06";
	# The next and previous year
	my $nextYear = int($year) + 1;
	my $prevYear = $year;
	my @monthList = ("July", "August", "September", "October", "November", "December");
	if ($month < 7) {
		$begMonth = "01";
		$endMonth = "06";
		$nextBegMonth = "07";
		$nextEndMonth = "12";
		$nextYear = $year;
		$prevYear = int($year) - 1;
		@monthList = ("January", "February", "March", "April", "May", "June");
	}
	# In order to make sure that each station listed in the new stations array will match the
	# station data, append an "A" to the beginning of the station number
    #
    # In fact, the result of the below for loop was to add 2 'A's' to the front of station 
	# numbers for new stations which cased problems for new stations. So this has been
	# commented out. -Brooks
   #
   # for my $s(@newStations) {
   # 	$s = "A" . $s;
   # }

	# The beginning and end month for the previous six month period
	my $prevBegMonth = $nextBegMonth;	
	my $prevEndMonth = $nextEndMonth;
	my $fileName = sprintf("%s/HPCN_inventory_%s%s_%s%s.html",$inventoryDir, $year, $begMonth, $year, $endMonth);

	# The file name for the previous and next six month period, so links can be displayed at the top of this page
	my $prevFileName = sprintf("HPCN_inventory_%s%s_%s%s.html",$prevYear, $prevBegMonth, $prevYear, $prevEndMonth);
	my $nextFileName = sprintf("HPCN_inventory_%s%s_%s%s.html",$nextYear, $nextBegMonth, $nextYear, $nextEndMonth);
	my %monthNames = ("01", "January", "02", "February", "03", "March", "04", "April", "05", "May",
			"06", "June","07", "July", "08", "August", "09", "September", "10", "October", 
			"11", "November", "12", "December");
	# Convert the month numbers into month names
	my $monthName = $monthNames{$month};
	#open(LOGFILE, ">>htmllog.txt");
	#print LOGFILE "Creating HTML inventory for $monthName\n";
	my $begMonthName = $monthNames{$begMonth};
	my $endMonthName = $monthNames{$endMonth};
	my $firstMonth = 1; # I think this means the first month of the 6 months in $fileName - AO
	my @previousData = ();
	# Check if the file already exists. If it does, we need to create an array of the previous file data so we can
	# have all the data prior to this month
	# 
	# When no previous HTML inventory file is present the script does not enter into this section and firstMonth is
	# not set to 0. This is related to the case when the script produces bad inventory files for Jan - April because
	# @previousData is populated on the first run through the subroutine. -Brooks
	if (-e $fileName) {
		$firstMonth = 0;
		open(PREVFILE, "<$fileName") or die ("could not open $fileName\n");
		# Creating an array of the previous file, where each element of the array is a String ending with a newline character
		@previousData = <PREVFILE>;
		close(PREVFILE);
		# Remove the ending </body> and </html> lines
		pop(@previousData);
		pop(@previousData); 
		# Now, reopen the file, replacing its contents with the previous data followed by the new data
		open(INVENTORY, ">$fileName") or die "Unable to open $fileName.";
		for(my $i = 0; $i < 109; $i++) { # Originally 111 (added 4 then minus 6) to cover the extra 4 lines in the header and the 6 extra empty lines.
			my $line = shift(@previousData);
			# The first 35 lines contain the header information (including style definitions), the page title, and the legend
			# The first 35 are now the first 39 due to four added lines (css and li).
			if ($i < 39)
			{
				print INVENTORY $line;
			}
		}
		# Close the file
		close(INVENTORY);
	}
	else {
		# If the file does not already exist, which is the case for the first month of a six month period, we need to create the
		# file and write the header and the legend
		open(INVENTORY, ">$fileName") or die "Unable to open $fileName.";	
		print INVENTORY "<html>\n<head>\n\t<title>HPCN Inventory: $begMonthName to $endMonthName $year</title>\n";
		print INVENTORY "\t<style>\n\t\t<!--\n\t\ta { font-weight: bold; padding: 0 1em; }\n";
		print INVENTORY "\t\tbody { font-family: sans-serif; }\n\t\tth { background-color:#333399; color: white; }\n";
		print INVENTORY "\t\ttr.oddstation {background-color: #FFFFCC;}\n";
		print INVENTORY	"\t\ttd.newstation {background-color: #00CC66; color: white;}\n";
		print INVENTORY	"\t\ttd.missingstation {background-color: #FF3300; color: white;}\n";
		print INVENTORY	"\t\ttd.outlierfilesize {background-color: #EEAA00; color: white;}\n";
		print INVENTORY "\t\ttd.highcompression {background-color: purple; color: white;}\n";
		print INVENTORY "\t\t.nouncompression {background-color: #666666; color: white;}\n";
		print INVENTORY "\t\ttr.inactive {background-color: #CCCCCC;}\n";
		print INVENTORY "\t\t-->\n\t</style>\n</head>\n";
		print INVENTORY "<body>\n<h1 align=\"center\">High Plains Climate Network (HPCN) <br />$year Data: $begMonthName to $endMonthName</h1>\n";
		print INVENTORY "<h2>Legend</h2>\n<ul>\n";
		print INVENTORY "\t<li> Cells with a <font color=\"#FF3300\"><b>red</b></font> background in the file size column indicate a station with no reading that month. </li>\n";
		print INVENTORY "\t<li> Cells with a <font color=\"#00CC66\"><b>green</b></font> background in the file size column indicate a new station</li>\n";
		print INVENTORY	"\t<li> Cells with a <font color=\"#EEAA00\"><b>yellow</b></font> background in the file size column indicate an unusual file size, defined by the following criteria:\n";
		print INVENTORY "\t\t<ul>\n\t\t\t<li> The file size is less than 15 Kb</li>\n\t\t\t<li> The file size is greater than 250 Kb</li>\n\t\t</ul>\n\t</li>\n";
		print INVENTORY	 "\t<li> Cells with a <font color=\"purple\"><b>purple</b></font> background in the ratio column have a compression ratio (from the compressed and uncompressed file sizes) greater than 5</li>\n";	
		print INVENTORY	 "\t<li> Cells with a <font color=\"#666666\"><b>dark gray</b></font> background in the ratio column have a compression ratio of 0 from a failed uncompression </li>\n";	
		print INVENTORY "\t<li> Rows with a <font color=\"gray\"><b>gray</b></font> background indicate a station that is no longer active.</li>\n</ul>\n";
		close(INVENTORY);
	}
	open(INVENTORY, ">>$fileName") or die "Unable to open $fileName.";
	if ($firstMonth == 1) { # If this is the first month...
		# For the first month of a six month period, we need to set up the table that will contain all the data, including the links to the
		# previous and next six month period
		print INVENTORY "<table align=\"center\" border=2 width=\"1460\">\n";
		print INVENTORY "\t<tr>\n\t\t<td align=\"left\" colspan=4> <a href=\"$prevFileName\">Previous six months</a></td>\n";
		print INVENTORY "\t\t<td align=\"right\" colspan=4> <a href=\"$nextFileName\">Next six months</a></td>\n\t</tr>\n";
	}
	# The 0-based index of the current month. Ranges from 0-5, with 0 being the first month of a six month period and 5 being the last
	my $monthIndex = ($month - 1) % 6; 
	my $l = scalar(@inventoryData);
	my $index = 0;
	# Keep track of how much data has been removed from the inventoryData array so we don't have an error
	while ($index < $l) {
		if ($index % 50 == 0) {
			# The table header that is repeated every 10 rows. Since each row contains 5 data items from the array, this will need
			# to repeat any time $index is a multiple of 50 
			print INVENTORY "\t<tr>\n\t\t<th align=\"center\">Station ID</th>\n\t\t<th align=\"center\">Location</th>\n";
			for (my $i = 0;$i < 6; $i++) {
				print INVENTORY "\t\t<th align=\"center\" width=\"180\">\n\t\t\t<table align=\"center\" width=\"180\">\n";
				print INVENTORY "\t\t\t\t<tr>\n\t\t\t\t\t<th align=\"center\" width=\"180\" colspan=2>$monthList[$i]</th>\n\t\t\t\t</tr>\n";
				print INVENTORY "\t\t\t\t<tr>\n\t\t\t\t\t<th align=\"center\" width=\"90\">File Size</th>\n\t\t\t\t\t<th align=\"center\" width=\"90\">Ratio</th>\n";
				print INVENTORY "\t\t\t\t</tr>\n\t\t\t</table>\n\t\t</th>\n";
			}
			print INVENTORY "\t</tr>\n";
		}
		# Since we're writing the table header every 10 rows each time a month is added, we will need to remove previous table header lines
		my $line = "";
		if (@previousData) {
			$line = $previousData[1];
                	chomp($line);
		}
                if ($line =~ /\t\t<th align=\"center\">Station ID<\/th>/)
                {
			for (my $j = 0; $j < 70; $j++) # Originally 76 (subtracted 6 for the 6 empty lines that were removed from table header).
                        {
				shift(@previousData);
                        }
                }
		my $lastIndex = 0;
		# Get the data for this station
		my $station = shift(@inventoryData);
                my $city = shift(@inventoryData);
                my $state = shift(@inventoryData);
                my $fileSize = shift(@inventoryData);
                my $ratio = shift(@inventoryData);
		my $shiftline = "";

		my $cmpFileSize;  # Current month's file size to compare to the newer file size.
		my $cmpRatio;  # Current month's compression ratio to compare to the newer compression ratio.
		my @trailingCells; # Trailing cell's file sizes and ratios to check for data.

		if ($firstMonth != 1)
		{	
		# If this is not the first month, then there may be previous data that needs to be written before this month's data
		# In the case of a new station, we need to add the station ID and the city, state for this station's row
		if ((grep /^$station$/, @newStations )) {
			if ($index % 10 == 0) {
				if ( !(grep /^$station$/, @inactiveStations) ) {
					print INVENTORY"\t<tr class=\"oddstation\">\n";
				} else {
					print INVENTORY"\t<tr class=\"oddstation inactive\">\n";
				}
 			} else {
				if ( !(grep /^$station$/, @inactiveStations) ) {
					print INVENTORY"\t<tr>\n";
				} else {
					print INVENTORY"\t<tr class=\"inactive\">\n";
				}
			}
				print INVENTORY "\t\t<td align=\"center\">$station</td>\n\t\t<td align=\"center\">$city, $state</td>\n";
			}
			# If this is not a new station, we need to get the data from the previous months
			else {
				# Since the row of this station may change if a new station is added, remove the previous <tr> line and
				# replace it with one that will be correctly identified as even or odd
				if (@previousData) {
					$shiftline = shift(@previousData);
					if ($index % 10 == 0) {
						if ( !(grep /^$station$/, @inactiveStations) ) {
							print INVENTORY"\t<tr class=\"oddstation\">\n";
							#print "---\n$shiftline\t<tr class=\"oddstation\">\n---\n";
						} else {
							print INVENTORY"\t<tr class=\"oddstation inactive\">\n";
							#print "---\n$shiftline\t<tr class=\"oddstation inactive\">\n---\n";
						}
					} else {
						if ( !(grep /^$station$/, @inactiveStations) ) {
							print INVENTORY"\t<tr>\n";
							#print "---\n$shiftline\t<tr>\n---\n";
						} else {
							print INVENTORY"\t<tr class=\"inactive\">\n";
							#print "---\n$shiftline\t<tr class=\"inactive\">\n---\n";
						}
					}
				}
				# Get the station ID and city, state from the previous file
				for (my $i = 0; $i < 2; $i++)
				{
					my $line = "";
					if (@previousData) {
						$line = shift(@previousData);
						print INVENTORY $line;
						#print "STATION: $line";
					}
				}
			}
		}
		# For new stations, fill in the cells before this month with blanks
		if ( grep /^$station$/, @newStations ) {
			for (my $i = 0; $i < $monthIndex; $i++) {
				print INVENTORY "\t\t<td align=\"center\" width=\"180\">\n\t\t\t<table width=\"180\">\n\t\t\t\t<tr>\n";
				print INVENTORY "\t\t\t\t\t<td width=\"90\" align=\"center\"></td>\n";
				print INVENTORY "\t\t\t\t\t<td width=\"90\" align=\"center\"></td>\n";
				print INVENTORY "\t\t\t\t</tr>\n";
				print INVENTORY "\t\t\t</table>\n\t\t</td>\n";
			}
			# Last index is used to keep track of any trailing cells that may need to be added after this cell
			$lastIndex = $monthIndex - 1;
		}
		# If this station is not a new station, get the previous data from the file
		# 
		# For the period of months when the script produces bad inventory files (particularly in April
		# during my test-runs) $monthIndex is zero (or negative) here and so we never enter in to 
		# this loop during the first (and it turns out only run) through this subroutine when the 
		# script messes up. This may be related to the bad inventory file problem. -Brooks
		else {
			for (my $i = 0; $i < $monthIndex; $i++) {
				for (my $j = 0; $j < 8; $j++) {
					my $line = "";
					if (@previousData) {
						$line = shift(@previousData);
						print INVENTORY $line;
					}
				}
				$lastIndex = $i;
			}
		}

		# If this station is not a new station, remove the trailing cell data
		if (!(grep /^$station$/, @newStations)) {
			if (@previousData) {
				for (my $k = $lastIndex + 1; $k < 6; $k++) {
					for (my $r = 0; $r < 8; $r++) {


					  # Hang on to the "current" month's file size and compression ratio to compare later.
					  if ($k == $lastIndex + 1 && $r == 3) {
						$cmpFileSize = "" . shift(@previousData);
						#print "k: $k\t r: $r\t " . shift(@previousData);
					  } elsif ($k == $lastIndex + 1 && $r == 4) {
						$cmpRatio =  "" . shift(@previousData);
						#print "k: $k\t r: $r\t " . shift(@previousData);
					  } else {
						shift(@previousData);
					  }


					}
				}
				shift(@previousData);
			}
		}		
		if ($firstMonth == 1) # This may be the first month, or there may be no previous data.
		{
		# If this is the "first month", then there is NO previous data before this month's data
		# Write the station ID, city and state for this station if it's the first month
			if ($index % 10 == 0) {
				if ( grep /^$station$/, @inactiveStations ) {
					# Add the inactive station class if this station is in the inactive stations list.
					print INVENTORY"\t<tr class=\"oddstation inactive\">\n";
				} else {
					print INVENTORY"\t<tr class=\"oddstation\">\n";
				}
			} else {
				if ( grep /^$station$/, @inactiveStations ) {
					# Add the inactive station class if this station is in the inactive stations list.
					print INVENTORY"\t<tr class=\"inactive\">\n";
				} else {
					print INVENTORY"\t<tr>\n";
				}
			}

            # For months Jan - Apr when bad inventory files were genearted, the script starts missing stations
			# on the second station. It is this line that it misses and it is because $firstMonth is set to 0
			# due to the presence of a previous HTML inventory file. So the script never makes it into this 
			# section below the conditional statement $firstMonth == 1 -Brooks
			print INVENTORY "\t\t<td align=\"center\">$station</td>\n\t\t<td align=\"center\">$city, $state</td>\n";

			if ($firstMonth != $monthIndex + 1) {
			# If this is not actually the first month, but there is no previous data.
			# Fill earlier months in with missing station data cells (if not a new station).
			# Fill earlier months in with blank cells (if a new station).
				my $tmpFileSize = "\t\t\t\t\t<td width=\"90\" align=\"center\"></td>\n";
				my $tmpRatio = "";

				if ( !( grep /^$station$/, @newStations ) ) {
					$tmpFileSize = "\t\t\t\t\t<td width=\"90\" align=\"center\" class=\"missingstation\">No File</td>\n";
					$tmpRatio = "NA";
				}

				for (my $r = 0; $r < $monthIndex; $r++) {
					print INVENTORY "\t\t<td align=\"center\" name=\"month_$r\" width=\"180\">\n\t\t\t<table width=\"180\">\n\t\t\t\t<tr>\n";
					print INVENTORY "$tmpFileSize";
					print INVENTORY "\t\t\t\t\t<td width=\"90\" align=\"center\">$tmpRatio</td>\n";
					print INVENTORY "\t\t\t\t</tr>\n";
					print INVENTORY "\t\t\t</table>\n\t\t</td>\n";
				}
			}
		}



		# Before adding the data for this month's reading, check if the "current month" from previousData is different from the new data.
		if (@previousData) {
			$chgStnDataList .=  "" . checkForChangedStationData($station, $cmpFileSize, $fileSize, $cmpRatio, $ratio);
		}

		# Now add the data for this month's reading	
		print INVENTORY "\t\t<td align=\"center\" name=\"month_$monthIndex\" width=\"180\">\n";
		if ($ratio eq "Broken") {
			print INVENTORY "\t\t\t<table width=\"180\" class=\"nouncompression\">\n\t\t\t\t<tr>\n";
		} else {
			print INVENTORY "\t\t\t<table width=\"180\">\n\t\t\t\t<tr>\n";
		}
		# Check for conditions such as a missing station, a new station, or an unusual file size (either too large or too small)
		# and use the appropriate style class
		if (grep /^$station$/, @missingStations ) {
			$fileSize = "No File";
			print INVENTORY "\t\t\t\t\t<td width=\"90\" align=\"center\" class=\"missingstation\">$fileSize</td>\n";
		}
		elsif ( grep /^$station$/, @newStations ) {
			print INVENTORY "\t\t\t\t\t<td width=\"90\" align=\"center\" class=\"newstation\">$fileSize</td>\n";
		}
		elsif ($fileSize > 250 or $fileSize < 15) {
			print INVENTORY "\t\t\t\t\t<td width=\"90\" align=\"center\" class=\"outlierfilesize\">$fileSize</td>\n";
		}
		else{
			print INVENTORY "\t\t\t\t\t<td width=\"90\" align=\"center\">$fileSize</td>\n";
		}
		# Check if the compression ratio is too high or had a failed uncompression
		if ($ratio ne "NA") {
			if ($ratio > 5.0) {
				print INVENTORY "\t\t\t\t\t<td width=\"90\" align=\"center\" class=\"highcompression\">$ratio</td>\n";
			} elsif ($ratio eq "Broken") {
				print INVENTORY "\t\t\t\t\t<td width=\"90\" align=\"center\">0</td>\n";
			} else {
				print INVENTORY "\t\t\t\t\t<td width=\"90\" align=\"center\">$ratio</td>\n";
			}
		}
		else{
			print INVENTORY "\t\t\t\t\t<td width=\"90\" align=\"center\">$ratio</td>\n";
		}
		$lastIndex++;
		print INVENTORY "\t\t\t\t</tr>\n";      
		print INVENTORY "\t\t\t</table>\n\t\t</td>\n";
	

		# Fill in blank cells for the remaining months in this six month period
		for (my $r = $monthIndex + 1; $r <= 5; $r++) {
			print INVENTORY "\t\t<td align=\"center\" name=\"month_$r\" width=\"180\">\n\t\t\t<table width=\"180\">\n\t\t\t\t<tr>\n";
			print INVENTORY "\t\t\t\t\t<td width=\"90\" align=\"center\"></td>\n";
			print INVENTORY "\t\t\t\t\t<td width=\"90\" align=\"center\"></td>\n";
			print INVENTORY "\t\t\t\t</tr>\n";
			print INVENTORY "\t\t\t</table>\n\t\t</td>\n";
		}
		print INVENTORY "\t</tr>\n";
		$index += 5;
	}
	# Add the previous/next file links at the bottom of the table.
	print INVENTORY "\t<tr>\n\t\t<td align=\"left\" colspan=4> <a href=\"$prevFileName\">Previous six months</a></td>\n";
	print INVENTORY "\t\t<td align=\"right\" colspan=4> <a href=\"$nextFileName\">Next six months</a></td>\n\t</tr>\n";
	# When all the data has been written, write the closing tags for the table, body and html
	print INVENTORY "</table>\n</body>\n</html>";
	close INVENTORY;

	# Send an e-mail out if there was any change in station data.
	if ($chgStnDataList ne "") {
		sendEmail("[HPCN] Change in Existing Monthly Inventory for $month/$year", "A change in file size and/or compression ratio " .
			" for $month/$year has been detected while creating the latest version of the monthly inventory.\n\n" .
			"$chgStnDataList\n" .
			"If this change in station data is unusual for $month/$year, please check your created TAR file.");
	}
}

##-----------------------------------------------------------------------------
# @signature checkForChangedStationData
#
# @input $cmpFileSize the "current" month's file size from previously existing HTML inventory file
# @input $fileSize the "current" month's file size
# @input $cmpRatio the "current" month's compression ratio from previously existing HTML inventory file
# @input $ratio the "current" month's compression ratio
#
# @output $errStr the string containing the station, file size and ratio changes
##-----------------------------------------------------------------------------
sub checkForChangedStationData{
	my $cur_stn = shift;
	my $old_fs = shift;
	my $cur_fs = shift;
	my $old_r = shift;
	my $cur_r = shift;
	my $statsChanged = 0; # false
	my $errStr = "";

	if ($old_fs =~ m/^.*?<td.*?>/) {
		$old_fs =~ s/^.*?<td.*?>//g; # Remove the leading space(s) and <td>.
		$old_fs =~ s/<\/td>.*?\n//g;  # Remove the </td>.
		if ($old_fs ne $cur_fs && $old_fs ne "") {
			if (!($old_fs eq "No File" && $cur_fs eq "0")) {
				$statsChanged = 1;
			}
		}
	}

	if ($old_r =~ m/^.*?<td.*?>/) {
		$old_r =~ s/^.*?<td.*?>//g; # Remove the leading space(s) and <td>.
		$old_r =~ s/<\/td>.*?\n//g;  # Remove the </td>.
		if ($old_r ne $cur_r && $old_r ne "") {
			$statsChanged = 1;
		}
	}

	if ($statsChanged == 1) {
		# Create a string for an e-mail indicating a change in file size.
		$errStr = "\t Station: $cur_stn\n" .
			"\t Previous File Size: $old_fs\t Latest File Size: $cur_fs\n" .
			"\t Previous Compression Ratio: $old_r\t Latest Compression Ratio: $cur_r\n\n";
	}

	return $errStr;
}

##-----------------------------------------------------------------------------
# @signature void backUpHTMLInventory
#
# @input $halfYear the first (1) or the second (2) half of the year
# @input $curYear the current year
##-----------------------------------------------------------------------------
sub backUpHTMLInventory {
	my $halfYear = shift;
	my $year = shift;
	my $inventoryDir = "/net/web/dmg/html/projects/operational/surface/hpcn/docs/inventory";
	my $filename = "";
	my $begMonth = "01";
	my $endMonth = "06";

	if ($halfYear == 1) { } 
	elsif ($halfYear == 2) {
		$begMonth = "07";
		$endMonth = "12";
	} else {
		#print " Incorrect half year.  No backup has been performed... ";
		printAndLog(" Incorrect half year.  No backup has been performed... ");
		return;
	}

	$filename = sprintf("HPCN_inventory_%s%s_%s%s.html", $year, $begMonth, $year, $endMonth);

	if (-e $inventoryDir."/".$filename) {
		# File exists.  Back it up!
		#print " File $filename has been found - ";
		printAndLog(" File $filename has been found - ");
		# Back up the file
		my $start = $inventoryDir."/".$filename;
		my $end = $start.".bak";
		if (!copy($start, $end)) {
			#print "error copying file :" . $filename . "\n";
			printAndLog("error copying file :" . $filename . "\n");
			return 0;
		}
		#print "and has been backed up - ";
		printAndLog("and has been backed up - ");
	} else {
		#print " File doesn't exist.  No backup has been performed. ";
		printAndLog(" File doesn't exist.  No backup has been performed. ");
	}

	return;
}

##-----------------------------------------------------------------------------
# @signature void sendProcessedEmail
#
# @input $year the four-digit year
# @input $begMonth the two-digit beginning month
# @input $endMonth the two-digit end month
##-----------------------------------------------------------------------------
sub sendProcessedEmail{
	# Send an e-mail out notifying that the HTML inventory is complete.
	my $year = shift;
	my $halfYear = shift;
	my $inventory_url = $inventoryDir;
	my $begMonth = "01";
	my $endMonth = "06";

	if ($halfYear == 1) { } 
	elsif ($halfYear == 2) {
		$begMonth = "07";
		$endMonth = "12";
	} else {
		printAndLog(" Incorrect half year.  No email will be sent... ");
		return;
	}
	
	$inventory_url =~ s/^\/net\/web\/dmg\/html/http:\/\/dmg.eol.ucar.edu/;
	$inventory_url = sprintf("%s/HPCN_inventory_%s%s_%s%s.html",$inventory_url, $year, $begMonth, $year, $endMonth);
	
	sendEmail("[HPCN] Processing Complete - View Inventory", "This month's HPCN ". 
			"inventory processing has been completed.\n\n" .
			"To view the latest inventory changes, please visit the following URL:\n\t" .
			$inventory_url . "\n\n" .
			"- Sent from HPCN Processing Script\n");
	
	printAndLog("\nMonthly inventory e-mail sent.\n");
}

##-----------------------------------------------------------------------------
# @signature String getMonthName
# 
# @input $month the two-digit month
# @output monthName
##-----------------------------------------------------------------------------
sub getMonthName{
	my $month = shift;
	if ($month eq "01") {
		return "January";
	}
	elsif ($month eq "02") {
		return "February";
	}
	elsif ($month eq "03") {
		return "March";
	}
	elsif ($month eq "04") {
		return "April";
	}
	elsif ($month eq "05") {
		return "May";
	}
	elsif ($month eq "06") {
		return "June";
	}
	elsif ($month eq "07") {
		return "July";
	}
	elsif ($month eq "08") {
		return "August";
	}
	elsif ($month eq "09") {
		return "September";
	}
	elsif ($month eq "10") {
		return "October";
	}
	elsif ($month eq "11") {
		return "November";
	}
	elsif ($month eq "12") {
		return "December";
	}
}


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Print/Log Function(s) -------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

##-----------------------------------------------------------------------------
# @signature void printAndLog(String text)
# <p>Prints given text to the screen and appends it to the bottom of the 
# current execution of this script's log file (see createLogFile()).</p>
#
# @input $text The string of text to be printed to the terminal screen and 
#        added to the log file.
##-----------------------------------------------------------------------------
sub printAndLog
{
    my($text) = @_;

    #print $text;

    #open(FH,">>$logFile") or die "$!";
    #print FH $text;
    #close (FH);
    
    print OLD_STDOUT $text; # Try to also print to the screen!
    print $text;
}

##-----------------------------------------------------------------------------
# @signature String createLogFile()
# <p>Create a log file for this run of the script in the log-file directory.</p>
#
# @output $logName The full path to the create log file (including its 
#         filename).
##-----------------------------------------------------------------------------
sub createLogFile
{
    my ($sec,$min,$hour,$day,$mon,$year) = gmtime(time());
    $year += 1900;
    $mon++;
        
    if ( $mon < 10 ) {
        $mon = "0$mon";
    }
  
    my $logName = $logDir."hpcn_run_$year$mon$day$hour$min.log";

    open(FH,">$logName") or die "unable to open file $logName\n";
    print FH "FILENAME: \t $logName \n";
    print FH "RUN TIME: \t ".gmtime(time())."\n";
    print FH "________________________________________________________________________________\n\n";
    close (FH);

    print "\tCreated logfile $logName.\n\tAll further output will be in this file.\n";
    return $logName;
}
