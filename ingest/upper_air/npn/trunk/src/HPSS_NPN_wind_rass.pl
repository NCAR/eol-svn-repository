#! /usr/bin/perl -w

#-About this File -------------------------------------------------------------
#
# Filename:  HPSS_NPN_wind_rass.pl (formerly HPSS_NPN_6min_wind.pl)
#
# Author(s):  Sean Stroble, Amanda Orin
#
# Last Updated:  June 7, 2012
#
#
# The purpose of this script is to create daily tar files of the NPN station
# readings from the LDM feeds (/net/ldm/data/) and move them to the
# corresponding HPSS directories (/EOL/operational/upper_air/profiler/npn/)
# directories. The tar daily files are also inserted into the database for
# corresponding dataset ids: 100.019, 100.020, 100.021.
#
# This script is to be run as a cron job and automatically gets the current 
# date from the system. The script then searches for recent readings from the 
# previous week (date range between end date on dataset and current date).
#
# If there are any issues during the tarring process, the move of files up to
# HPSS, or insertion of tarred files into the database, the script will send 
# an e-mail to Scot Loehrer and Linda Cully informing them of the situation.
#
# Update: The script will also compare the number of files in each TAR file
#   to the expected number of files as each TAR file is created.  If the 
#   number of files is less than the expected number, an inventory file 
#   for that TAR file is created in $work_dir/tarInventories with a filename 
#   which indicates its associated archive ident and TAR file.  An e-mail will 
#   then be sent to Scot Loehrer and Linda Cully informing them of the new 
#   file inventory created.
#
# Testing can be conducted on sferic-dev using the MySqlTestDatabase and 
# MySqlTestDataset perl modules. In this case, be sure to comment out the
# MySqlDatabase and MySqlDataset perl modules. In addition, the directories 
# referred to by the variables $ingest, $work_dir, and $hpss can be changed 
# to a local directory for testing.
#
#-About this File -------------------------------------------------------------


##Module-----------------------------------------------------------------------
# 
##Module-----------------------------------------------------------------------
use strict;
#use lib "/h/eol/dmg/HPSS_cronjobs/lib";
use lib "/net/work/lib/perl/hpss";

use lib "/net/work/lib/perl/mysql";	# For production
#use lib "lib";				# For testing only


#------------------------------------------------------------------------------
# Libraries to send e-mails and update the database
#------------------------------------------------------------------------------
#use MySqlTestDatabase;			# The test database (For testing)
#use MySqlTestDataset;			# The test dataset Module (For testing)
use MySqlDatabase;
use MySqlDataset;
use MySqlMSSFile;
use HPSS;

use Email::MIME;
use Email::MIME::Creator;

#------------------------------------------------------------------------------
# Global Variables
#------------------------------------------------------------------------------
#my $database = MySqlTestDatabase->new(("zithupdate"), ("change-999"));	# For testing
my $database = MySqlDatabase->new(("zithupdate"), ("change-999"));	# For production

# Ingest      "/net/ldm/data/profiler";            # 100.019
#             "/net/ldm/data/rass";                # 100.020 - 2011
#             "/net/ingest/ldm/data/rass";         # 100.020 - 2002-2011
#             "/export/ldm/data/profiler/hourly";  # 100.021

#my $work_dir = "/net/work/tmp/orin/NPN/tarred/";		# For testing
my $work_dir = "/net/work/operational/sounding/NPN/";	# For production

# HPSS      "/EOL/operational/upper_air/profiler/npn/winds_6min";   # 100.019
#           "/EOL/operational/upper_air/profiler/npn/rass";         # 100.020
#           "/EOL/operational/upper_air/profiler/npn/winds_hourly"; # 100.021

my $recipients = "cully\@ucar.edu,loehrer\@ucar.edu";	# For production
#my $recipients = "orin\@ucar.edu";			# For testing

my $report = "";

my $filereport = "";

my $logDir = "/net/work/operational/sounding/NPN/logs/"; # For production
#my $logDir = "/net/work/tmp/orin/NPN/logs/"; # For testing
my $logFile = createLogFile();

# Run the script
&main();

##-----------------------------------------------------------------------------
# @signature void main()
# <p>Run the script.</p>
##-----------------------------------------------------------------------------
sub main {
    open(LOGFILE, ">>$logFile") or die "unable to open file $logFile\n";
    
    # save original settings. You could also use lexical typeglobs.
    *OLD_STDOUT = *STDOUT;
    *OLD_STDERR = *STDERR;

    # reassign STDOUT, STDERR
    *STDOUT = *LOGFILE;
    *STDERR = *LOGFILE;
    
    
    # Process data for archive_ident 100.019.
    my $archive_ident = "100.019";
    my $ingest = "/export/ldm/data/profiler";
#    my $ingest = "/net/work/tmp/orin/NPN/ingest/tar_profiler"; # For testing
    my $hpss = "/EOL/operational/upper_air/profiler/npn/winds_6min";
    my $title = "NOAA Profiler Network (NPN) Six Minute Wind Profile Data (NOAA/ESRL)";
    process_data($archive_ident, $ingest, $hpss, $title);
    $report = "";
    
    # Process data for archive_ident 100.020.
    $archive_ident = "100.020";
    $ingest = "/export/ldm/data/rass";
#    $ingest = "/net/work/tmp/orin/NPN/ingest/tar_rass"; # For testing
    $hpss = "/EOL/operational/upper_air/profiler/npn/rass";
    $title = "NOAA Profiler Network (NPN) Hourly RASS Data (NOAA/ESRL)";
    process_data($archive_ident, $ingest, $hpss, $title);
    $report = "";
    
    # Process data for archive_ident 100.021.
    $archive_ident = "100.021";
    $ingest = "/export/ldm/data/profiler/hourly";
#    $ingest = "/net/work/tmp/orin/NPN/ingest/tar_profiler"; # For testing
    $hpss = "/EOL/operational/upper_air/profiler/npn/winds_hourly";
    $title = "NOAA Profiler Network (NPN) Hourly Wind Profile Data (NOAA/ESRL)";
    process_data($archive_ident, $ingest, $hpss, $title);
    $report = "";

    # If $filereport is not empty, then send out an e-mail with the file count differences
    if ($filereport ne "") {
	printAndLog("\n__________________________________________________\n-- HPSS/CODIAC Tar File Report for Data Sets --\n__________________________________________________\n\n".$filereport."\n\n\n");
        sendEmail("[NPN Warning] HPSS/CODIAC Tar File Report for Data Sets",$filereport);
    } else {
        #print "\n\tThere were no tar file issues found.\n";
        printAndLog("\n\tThere were no TAR file issues found.\n");
    }
    
    
    # done, restore STDOUT/STDERR
    *STDOUT = *OLD_STDOUT;
    *STDERR = *OLD_STDERR;
    
    close(OLD_STDOUT);
    close(OLD_STDERR);
    close(LOGFILE);
}


##-----------------------------------------------------------------------------
# @signature void process_data()
# <p>Process the data for the given Archive ID from the given ingest space
# and place it on the HPSS in the given directory.</p>
#
# @input $archive_ident The archive ID for the given dataset.
# @input $ingest The ingest directory.
# @input $hpss The HPSS directory where files are placed.
##-----------------------------------------------------------------------------
sub process_data {
    my $archive_ident = shift;
    my $ingest = shift;
    my $hpss = shift;
    my $title = shift;

    # Get the latitude/longitude bounding box for the dataset
    my ($minlat, $maxlat, $minlon, $maxlon) = getDatasetBounds($archive_ident);

    # Create the new tar files.
    create_tar_files($archive_ident, $ingest, $hpss, $title); 
    if ($report ne "") { sendMailAndDie($archive_ident,"For dataset $archive_ident:\n\n".$report); return; }    

    # Read in all of the tar files in the working directory ($work_dir)
    opendir(my $TARS,$work_dir) or $report.="Cannot open working directory $work_dir for dataset $archive_ident.\n";
    if ($report ne "") { sendMailAndDie($archive_ident,"For dataset $archive_ident:\n\n".$report); return; }
    my @tar_files = grep(/\.tar$/,readdir($TARS));
    closedir($TARS);

    printAndLog("\tPlacing TAR files on HPSS and inserting into database for $archive_ident... \n");
    #print "\tPlacing TAR files on HPSS and inserting into database for $archive_ident... ";
    # Put the tar files on the mass store and insert them into the database.
    foreach my $file (sort(@tar_files)) {
        my $msg = place_on_hpss($file, $hpss);
        if ($msg eq "") {
            $report .= insert_file($archive_ident, $hpss, $file, $minlat, $maxlat, $minlon, $maxlon);
            unlink(sprintf("%s/%s",$work_dir,$file)) or $report.="Unable to unlink $work_dir/$file.\n";
        } else {
            $report .= $msg;
        }
    }

    # Send out an email that the script has finished.
    if ($report ne "") {
#        print "\tthere was an error. Sending report now for $archive_ident.\n";
        printAndLog("\tthere was an error.\n");
        sendMailAndDie($archive_ident,"For dataset $archive_ident:\n\n".$report);
    } else { 
#        print "\tdone.\n\tThere are no errors to report for dataset $archive_ident.";
        printAndLog("\tThere are no errors to report for dataset $archive_ident.\n");
    }
    
    #print "\n\tProcessing complete for $archive_ident.\n";
    printAndLog("\tProcessing complete for $archive_ident.\n\n");
}

##-----------------------------------------------------------------------------
# @signature String[] create_tar_files(String $archive_ident, String $ingest)
# <p>Read the data in the ingest directory and create the tar balls that are
# to be placed on the HPSS.</p>
#
# @input $archive_ident The archive_ident of the dataset being processed.
# @input $ingest The ingest directory of the dataset being processed.
##-----------------------------------------------------------------------------
sub create_tar_files {
    # Sample file format: 20113431954_6min_pro.nc
    # Year (2011), Julian day (343), hour (19), minute (54)
    my $archive_ident = shift;
    my $ingest = shift;
    my $hpss = shift;
    my $title = shift;
    #my ($sec,$min,$hour,$day,$mon,$year) = localtime(time());
    # gmtime = second, minute, hour, day of month, month, year offset, day of 
    #          week, day of year, daylight savings
    my ($sec,$min,$hour,$day,$mon,$year) = gmtime(time());
    my $today = sprintf("%04d%03d",$year+1900,getJulianDay($mon+1,$year,$day)); # Sample: 2012090 for March 30, 2012
    my @files = ();
    my $startDay; # Calculated from dataset end date.

    #print "\tCreating TAR files for $archive_ident... ";
    printAndLog("\tCreating TAR files for $archive_ident... \n");

    # Get the end_date from the dataset to figure out where to start from.
    $startDay = getEndDateFromDataset($archive_ident);	# For production
#    $startDay = "2012150";	# For testing

    # Get the list of files that will be used to generate the tar files.
    # It is important to omit today's files because they may not have yet finished being ingested for the day.
    opendir(my $INGEST,$ingest) or $report.="Cannot open ingest directory $ingest for dataset $archive_ident.\n";
    if ($report ne "") { printAndLog("\t\t-- ERROR REPORT [ DATASET $archive_ident ] --\n\t\t$report\n\n"); return; } #print "\tthere was an error. Sending report now for $archive_ident.\n"; return; }

	printAndLog("\t\tLooking for files that are between ".$startDay." and ".$today."...");
    foreach my $file (sort(readdir($INGEST))) {
        if ($archive_ident == "100.019") {
            if ($file =~ m/^\d{11}_6min_pro\.nc$/  &&  substr($file,0,7) > $startDay  &&  substr($file,0,7) < $today) {
                push(@files,$file);
            }
        } elsif ($archive_ident == "100.020") {
            if ($file =~ m/^\d{11}$/  &&  substr($file,0,7) > $startDay  &&  substr($file,0,7) < $today) {
                push(@files,$file);
            }
        } elsif ($archive_ident == "100.021") {
            if ($file =~ m/^\d{11}_hourly_pro\.nc$/  &&  substr($file,0,7) > $startDay  &&  substr($file,0,7) < $today) {
                push(@files,$file);
            }
        }
    }
    printAndLog("  done.\n");
    closedir($INGEST);

    # Confirm that there are new files to archive. If not, warn user
    if (@files == 0) {
        $report .= "No files found that are more recent than ".formatJulianDay($startDay)." and created before ".formatJulianDay($today)." in the ingest location: $ingest.\n";
        #print "\tthere was an error. Sending report now for $archive_ident.\n";
        printAndLog("\n\t\t-- ERROR REPORT [ DATASET $archive_ident ] --\n\t\t$report\n\n");
        return;
    }
#    if(@files==0){printAndLog("\t\t--------------------------------------------------\n\t\tWARNING [ DATASET $archive_ident ]:\n\t\tNo files found that are more recent than ".formatJulianDay($startDay)." and created before ".formatJulianDay($today)." in the ingest location: $ingest.\n\t\t--------------------------------------------------\n"); return;} # For testing
#    if(@files==0){print "\t$archive_ident:\n\tNo files found that are more recent than ".formatJulianDay($startDay)." and created before ".formatJulianDay($today)." in the ingest location: $ingest.\n"; return;} # For testing

    # Split the files into lists by their date
    my %tar_hash;
    foreach my $f (sort(@files)) {
        push(@{ $tar_hash{substr($f,0,7)}},$f);
    }

    # Create the tar files for each date
    my @tar_files = ();
    chdir($ingest) or $report.="Cannot change directory to ingest location $ingest for dataset $archive_ident.\n";
    if ($report ne "") { printAndLog("\t\t-- ERROR REPORT [ DATASET $archive_ident ] --\n\t\t$report\n\n"); return; } #print "\tthere was an error. Sending report now for $archive_ident.\n"; return; }

    # Set up the expected number of files in a TAR
    my $expectedFileTotal = 0;
    my $codiacID = 0;
    if ($archive_ident == "100.019") { $expectedFileTotal = 240; $codiacID = 9460; }	# For production
    elsif ($archive_ident == "100.020") { $expectedFileTotal = 24; $codiacID = 9461; }	# For production
    elsif ($archive_ident == "100.021") { $expectedFileTotal = 24; $codiacID = 9462; }	# For production
#     if ($archive_ident == "100.019") { $expectedFileTotal = 240; $codiacID = 9399; }	# For testing
#     elsif ($archive_ident == "100.020") { $expectedFileTotal = 24; $codiacID = 9400; }	# For testing
#     elsif ($archive_ident == "100.021") { $expectedFileTotal = 24; $codiacID = 9401; }	# For testing

    my $filereportcount = 0;
    foreach my $date (sort(keys(%tar_hash))) {
    
        if (system(sprintf("tar -cf %s/%s_npn.tar %s",$work_dir,$date,
                   join(" ", @{$tar_hash{$date}}) ))) {
            $report .= "$date\_npn.tar was not able to be created.\n";
        } else {
            # Check if the number of files in the TAR is the expected number
            my $actualFileTotal = scalar(@{$tar_hash{$date}});
            if ($actualFileTotal != $expectedFileTotal) {
                if ($filereportcount == 0 && $filereport eq "") { 
			$filereport .= "(CODIAC ID: $codiacID)\t$archive_ident\t$title\n"; 
			$filereport .= "--------------------------------------------------------------------------------\n";
		}
                elsif ($filereportcount == 0 && $filereport ne "") { 
			$filereport .= "\n(CODIAC ID: $codiacID)\t$archive_ident\t$title\n";
 			$filereport .= "--------------------------------------------------------------------------------\n";
		}

		# Create the TAR inventory for the affected TAR file
		my $tarInventory = $work_dir."tarInventories/$archive_ident\_npn\_$date.txt";
		my $destFile = "$hpss/".substr($date,0,4)."/$date\_npn.tar";
		open(TARINVENTORY, ">$tarInventory") or $report.="Could not open $tarInventory for dataset $archive_ident file report.\n";
		if ($report ne "") { printAndLog("\n\t\t-- ERROR REPORT [ DATASET $archive_ident ] --\n\t\t$report\n\n"); return; } #print "\tthere was an error. Sending report now for $archive_ident.\n"; return; }
		print TARINVENTORY "$archive_ident - \t$title\n\n";
		print TARINVENTORY "Location: \t$destFile\n";
		print TARINVENTORY "--------------------------------------------------------------------------------\n";
		for my $s (@{$tar_hash{$date}}) {
			print TARINVENTORY "\t$s\n";
		}


        $filereport .= "\t$destFile lists $actualFileTotal files out of $expectedFileTotal expected files.\n";
		$filereport .= "\tPlease see the inventory file located at $tarInventory for further details.\n\n";
        $filereportcount += 1;
    }

    foreach my $rfile (@{$tar_hash{$date}}) {
# ##            $report .= "$rfile was not able to be removed.\n" if (!unlink($rfile));
    }
#            push(@tar_files,sprintf("%s_npn.tar",$date));
        }
    }

    if ($report ne "") { 
        #print "\tthere was an error. Sending report now for $archive_ident.\n";
        printAndLog("\t\tThere was an error. Sending report now for $archive_ident.\n");
    } else {
        #print "\tdone.\n";
        #printAndLog("\n");
        # Do nothing.
    }
}

##-----------------------------------------------------------------------------
# @signature Double getDatasetBounds(String archive_ident)
#
# @input $archive_ident The archive identification of the dataset.
# @output $minlat Minimum latitude
# @output $maxlat Maximum latitude
# @output $minlon Minimum longitude
# @output $maxlon Maxiumum longitude
##-----------------------------------------------------------------------------
sub getDatasetBounds {
    my $archive_ident = shift;
    my $minlat = -90;
    my $maxlat = 90;
    my $minlon = -180;
    my $maxlon = 180;

    $database->connect();

    #my $dataset = MySqlTestDataset->new($archive_ident);	# For testing
    my $dataset = MySqlDataset->new($archive_ident);		# For production
    my $msg = $dataset->selectDataset($database);

    # Only continue if the dataset was retrieved successfully.
    if ($msg eq "") {
        $minlat = $dataset->getMinlat();
        $maxlat = $dataset->getMaxlat();
        $minlon = $dataset->getMinlon();
        $maxlon = $dataset->getMaxlon();
    }

    $database->disconnect();

    return ($minlat, $maxlat, $minlon, $maxlon);
}

##-----------------------------------------------------------------------------
# @signature String getEndDateFromDataset(String archive_ident)
# <p>Retrieve the end date from the specified dataset in the database.</p>
#
# @input $archive_ident The archive identification of the dataset.
# @output $end The end date of the specified dataset.  This will be empty if
#   there was an issue retrieving the end date and parsing it as a string.
##-----------------------------------------------------------------------------
sub getEndDateFromDataset {
    my $archive_ident = shift;
    my $end;
    my $dataset;
    my $msg = "";
    my $mon;
    my $julEnd;

    # Open the database.
    $database->connect();

    # Create and load the dataset.
    #$dataset = MySqlTestDataset->new($archive_ident);	# For testing
    $dataset = MySqlDataset->new($archive_ident);	# For production
    $msg = $dataset->selectDataset($database);

    if ($msg eq "") {
        $end = $dataset->getEndDate();
        $end =~ s/[\s:-]//g;

        $mon = substr($end,4,2);
#        $julEnd = getJulianDay($mon,substr($end,0,4),substr($end,6,2));
        
	$julEnd = sprintf( "%04d%03d", substr($end,0,4), getJulianDay($mon,substr($end,0,4),substr($end,6,2)) ); # This will format the day with all the appropriate leading zero's (e.g. YYYYDDD).

#        # If the computed Julian day is less than 100 but larger than 9 (so only 2 places instead of 3), add a 0.
#        if ($julEnd < 100 && $julEnd >= 10) {
#        	$julEnd = "0" . $julEnd;
#        } else if ($julEnd < 10) {
#		# If the computed Julian day is less than 10 (so only 1 place instead of 3), add 00.
#		$julEnd = "00" . $julEnd;
#	}
#
#        $julEnd = substr($end,0,4) . $julEnd;
    } else {
        $database->disconnect();

        sendMailAndDie($archive_ident, "Unable to select dataset $archive_ident to retrieve end date.\n\n".$msg);
        return;
    }

    # Always disconnect cleanly.
    $database->disconnect();

    return $julEnd;
}

##-----------------------------------------------------------------------------
# @signature String insert_file(String file)
# <p>Insert the specified file into the database.</p>
#
# @input $file The name of the file being inserted.
# @output $msg An error message that was generated from the insert or the empty
# String if the insert completed successfully.
##-----------------------------------------------------------------------------
sub insert_file {
    my $archive_ident = shift;
    my $hpss = shift;
    my $file = shift;
    my ($minlat, $maxlat, $minlon, $maxlon) = @_;

    # Get the begin and end date for the given file.
    my ($fmonth, $fday) = convertJulianDay($file);

    # For production
    # Create the file information.
    my $mysql = MySqlMSSFile->new();
    $mysql->setDatasetArchiveIdent($archive_ident);
    $mysql->setFile(sprintf("%s/%04d",$hpss,substr($file,0,4)),$file);
    $mysql->setFormatId(53); # Format ID for: Unix Tape ARchive (TAR) Format File
    $mysql->setBeginDate(substr($file,0,4),$fmonth,$fday,0,0,0);
    $mysql->setEndDate(substr($file,0,4),$fmonth,$fday,23,59,59);

    # Set the min/max lat/lon coordinates for the file before insertion.
    $mysql->setMinlat($minlat);
    $mysql->setMaxlat($maxlat);
    $mysql->setMinlon($minlon);
    $mysql->setMaxlon($maxlon);

    # Open the database
    $database->connect();

    # Insert the file
    my $msg = $mysql->insert($database);	# For production
    #my $msg = "";	# For testing

    # Commit if no errors have occurred to this point otherwise rollback.
    if ($msg eq "") { $msg .= $database->commit(); }
    else { $msg .= "Database rolled back.\n".$database->rollback(); }

    # Always disconnect cleanly.
    $database->disconnect();

    return $msg;
}

##-----------------------------------------------------------------------------
# @signature String place_on_hpss(String file)
# <p>Copy the specified file to the mass store.</p>
#
# @input $file The file to be copied to the mass store.
# @output $msg Any error messages that occured during the copy or the empty
# String if it was copied successfully.
##-----------------------------------------------------------------------------
sub place_on_hpss {
#    my ($file) = @_;
    my $file = shift;
    my $hpss = shift;
    my $year = substr($file,0,4);
    
    return HPSS::put(\"$work_dir/$file",\"$hpss/$year/$file");	# For production
#    return "";	# For testing
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
        
#    if ( $mon < 10 ) {
#        $mon = "0$mon";
#    }
    my $formattedDate = sprintf("%04d%02d%02d%02d%02d", $year, $mon, $day, $hour, $min);
  
#    my $logName = $logDir."npn_run_$year$mon$day$hour$min.log";
    my $logName = $logDir."npn_run_$formattedDate.log";	

    open(FH,">$logName") or die "unable to open file $logName\n";
    print FH "FILENAME: \t $logName \n";
    print FH "RUN TIME: \t ".gmtime(time())."\n";
    print FH "________________________________________________________________________________\n\n";
    close (FH);

    print "\tCreated logfile $logName.\n\n";
    return $logName;
}


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Julian Day Conversion Functions ---------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

##-----------------------------------------------------------------------------
# @signature String getJulianDay(int $month, int $year, int $day)
# Returns the Julian day for the given month, year, and day
#
# @input $month the calendar month
# @input $year the calendar year
# @input $day the calendar day of the month
#
# @output $julianDay the Julian day for the given month, year, and day
##-----------------------------------------------------------------------------
sub getJulianDay {
    my $month = shift;
    my $year = shift;
    my $day = shift;
    my $leapYear = isLeapYear($year);
    my @julianDays = (1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335);
    my $firstJulDay = $julianDays[$month-1];

    # Convert the first Julian day for the given month and year and return the
    # addition of the day of the month
    if ($leapYear and $month > 2) {
#        return $julianDays[$month-1] + $day;
        return $firstJulDay + $day;
    }
    else {
#        return $julianDays[$month-1] + $day - 1;
        return $firstJulDay + $day - 1;
    }
}

##-----------------------------------------------------------------------------
# @signature int isLeapYear
# Returns 1 if the year is a leap year, 0 otherwise.
#
# @input $year the year
##-----------------------------------------------------------------------------
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

##-----------------------------------------------------------------------------
# @signature int endDay(String $month, String $year)
# return the end day of the file based on the month 
# of the file.
#
# @input $month the monthto find the end day for
# @input $year the year for the month
# @output an int representing the last day of $month
##-----------------------------------------------------------------------------
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

##-----------------------------------------------------------------------------
# @signature void convertJulianDay(String $file)
# <p>convert the julian day in the file name to a month and day.</p>
#
# @input $file, a file name.
# @output $fileMonth, the month of the file.
# @output $fileDay, the day of the file.
##-----------------------------------------------------------------------------
sub convertJulianDay{
    my $file = shift;
    my $year = "";
    my $month = 0;
    my $day = 0;
    my $fileDay = "";
    my $fileMonth = "";
    my @julianDay = (1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335);
    my $firstDay = $julianDay[$month];
    
    if($file =~ m/^\d{7}/) {
        $year = substr($file,0,4);
        $fileDay = substr($file,4,3);
       
        # Iterate over the first Julian days of every month...
        while ($fileDay > $firstDay && $month < scalar(@julianDay)) {
            # If the given file's Julian day is greater than the 1st of the next
            # month and $month does not exceed the julianDays array, add 1 to the month.
            # Also, calculate the difference between the two days (if the next 
            # while-comparison is false, then this is the actual day of the $month.
            $day = $fileDay - $firstDay;
            $month += 1;
            
            # If $month does not currently equal or exceed 12...
            if ($month < scalar(@julianDay)) {
                # Recalculate the first day of the month for the next pass-through.
                $firstDay = $julianDay[$month];
                if (isLeapYear($year) && $month > 2) {
                    $firstDay += 1; # Add 1 if it is a leap year and the month is past February.
                }
            }
        }

        # If $day was actually calculated out to be 0, then it is the first day of that month
        # and must be set to 1 and actually up the $month by 1 as well because it never went
        # through the while-loop.
        if ($fileDay - $firstDay == 0) {
            $day = 1;
            if ($month < 12) {
                $month += 1;
            }   
        } else {
            $day += 1; # To account for the subtraction between days.
        }
        
        # The $day was calculated before the $month was iterated too far.
        # The $month is the actual month (as it doubles as the index + 1 for $julianDays).
        $fileDay = "$day";
        $fileMonth = "$month";
    }

    return ($fileMonth, $fileDay);
}

##-----------------------------------------------------------------------------
# @signature String formatJulianDay
# 
# @input $file The file to convert the Julian day from
# @output $formattedDate The formatted date
##-----------------------------------------------------------------------------
sub formatJulianDay {
    my $file = shift;

    if($file =~ m/^\d{7}/) {
        my ($fMonth, $fDay) = convertJulianDay($file);
        
        if ($fMonth < 10) { $fMonth = "0".$fMonth; }
        if ($fDay < 10) { $fDay = "0".$fDay; }

        return "$fDay-".getMonthName($fMonth)."-".substr($file,0,4);
    } else {
        return $file;
    }
}

##-----------------------------------------------------------------------------
# @signature String getMonthName(String $month)
# 
# @input $month the two-digit month
# @output $monthName the name of the month
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
    my $copied = "orin\@eol.ucar.edu"; # Comment out if sendEmail breaks.
    
    my @parts = (Email::MIME->create(attributes => { content_type => "text/plain" },
                     body => $body."\n\n--\nFor full details, please see ".$logFile."\n"));

    my $email = Email::MIME->create(parts => [ @parts ]);

    $email->header_set("From" => $sender);
    $email->header_set("Reply-To" => $reply_to);
    $email->header_set("To" => $recipients);
    $email->header_set("Cc" => $copied); # Comment out if sendEmail breaks.
    $email->header_set("Subject" => $subject);

    open(my $SENDMAIL,"|/usr/lib/sendmail -t") || die("Unable to open sendmail\n");
    printf($SENDMAIL $email->as_string());
    close($SENDMAIL);
}

##-----------------------------------------------------------------------------
# @signature void sendMailAndDie(String body)
#
# @input $archive_ident The archive_ident of the dataset being processed when called.
# @input $body The message of the email.
##-----------------------------------------------------------------------------
sub sendMailAndDie {
    my $archive_ident = shift;
    my ($body) = @_;
#    sendEmail("LDM MADIS HDW1h Satellite Winds MSS/CODIAC Error",$body);
# #    sendEmail("[NPN] LDM MADIS HDW1h Satellite Winds HPSS/CODIAC Error",$body);
   
    printAndLog("\n\n________________________________________\n-- HPSS/CODIAC Error Report for Data Set $archive_ident --\n________________________________________\n\n".$body."\n\n\n"); 
    sendEmail("[NPN Error] HPSS/CODIAC Error Report for Data Set $archive_ident",$body);

#    exit(1);
    return;
}
