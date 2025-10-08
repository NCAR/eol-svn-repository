#! /usr/bin/perl -w

## hpcn_archive_data.pl
##
## @Daniel Choi
## Added checksum funcion. - February 2025
##
## @author J. Scannell
## @version 1.0 - March 2023
##
## This script runs once each month and downloads HPCN data from the AWDN web site, 
## tars/gzips the data files,
## puts the gz files in /net/archive/data/operational/hpcn, and adds the files to the 
## dataset. 
## Affected dataset: 100.015
##
use strict;
use lib "/net/work/lib/perl/mysql";
use lib "/net/work/lib/perl/mail";
use MySqlDatabase;
use MySqlFile;
use DateTime;
use MAIL;

my $DOWNLOAD_DIR = "/scr/tmp/joss/hpcn";
my $SCRIPT_DIR = "/h/eol/dmg/HPSS_cronjobs/ingest/surface/hpcn";
my $STATION_LIST = "$SCRIPT_DIR/stationList.csv";
my $ARCHIVE_DIR = "/net/archive/data/operational/hpcn";
my $DATASET_ID = "100.015";
my @monitors = ("eol-cds-ingest\@ucar.edu");


#
# Download data for the 3rd previous month, ie download January 2023
# data in April 2023.  This is because the data on the HPCN site 
# can be updated for 50 days.
# Find the 3rd previous month and the last day in that month to download
# the data for.
#
my ($year, $month, $last_day_in_month) = find_download_month();
#
# Download all the data files for the selected month 
#
my @newStations = `$SCRIPT_DIR/hpcn_download_data.py $year $month $last_day_in_month $DOWNLOAD_DIR $STATION_LIST`;
#
# Create email message for the new stations
#
my $stations_closed = 0;
my $stations_new = 0;
my $complete_station_message = "";
if ($#newStations >= 0) {
   my $stations_closed_message = "HPCN Possible Closed Stations as of $month/$year\n\nStation ID    Close Down Date/Time     StartUp Date/Time\n\n";
   my $stations_new_message = "HPCN Possible New Stations as of $month/$year\n\nStation ID    Close Down Date/Time     StartUp Date/Time\n\n";
   foreach my $station (@newStations) {
      my ($name, $closedown, $startup) = split(/,/, $station);
      if ($closedown ne "None") {
         $stations_closed_message .= "$name     $closedown      $startup\n";
	 $stations_closed += 1;
      } else {
         $stations_new_message .= "$name     $closedown      $startup\n";
	 $stations_new += 1;
      } 
   }
   $stations_new_message .= "Data for the possibly new stations may need to be backfilled if the start date of the station is before $month/01/$year.  See instructions for backfilling data.\n\n";
   $stations_closed_message .= "Data for the now closed stations should be included in the archive up until the closed date.\n\n";
   if ($stations_new > 0) {
      $complete_station_message .= $stations_new_message . "\n\n\n";
   }
   if ($stations_closed > 0) {
      $complete_station_message .= $stations_closed_message;
   }
   sendMailAndContinue($complete_station_message);
}
#
# Create the tar.gz file
#
my $tarfile = create_tar_file($year, $month);
#
# Copy the data file to the $ARCHIVE_DIR and chmod the file.
#
my $result = copy_and_chmod_file($tarfile, $ARCHIVE_DIR);
#
# Insert the data file into the database.
#
my $msg = insert_into_database($tarfile, $last_day_in_month);
if ($msg ne "") {
   sendMailAndDie("Couldn't insert $tarfile into database:\n $msg\n");
}


# Update the checksum.

$result = system("python3 /h/eol/eoldata/eoldata-utils/cksum/checksum_utility.py -n -d $ARCHIVE_DIR >/dev/null");
if ($result != 0) {
   sendMailAndDie(sprintf("Error with checksum command.\n", $result/256));
}


#} else { 
#   sendMailAndDie("No errors to report\n"); 
#}

#
# Create the tar files from the downloaded data
#
sub create_tar_file {
   my ($year, $month) = @_;

   my $tar_file = "hpcn.$year$month.tar.gz";
   chdir("$DOWNLOAD_DIR") or sendMailAndDie("Couldn't chdir to $DOWNLOAD_DIR, $!");
   my $result = system("/bin/tar -czf $DOWNLOAD_DIR/$tar_file *.csv");
   if ($result != 0) {
      sendMailAndDie("$DOWNLOAD_DIR/$tar_file could not be created, $!\n");
   }
   unlink glob("$DOWNLOAD_DIR/*.csv");
   return($tar_file);
}

#
# Find the previous month for which the files will be downloaded.
# Also determine the last day of the month for that month to use
# for the ending date in the database.
#

sub find_download_month {

   my $dt = DateTime->now;
   my $month_to_download = $dt - DateTime::Duration->new( months => 3 );
   my $year = sprintf("%04d", $month_to_download->year);
   my $month = sprintf("%02d",$month_to_download->month);
   my $dt2 = DateTime->last_day_of_month(
      year	=> $year,
      month    	=> $month
   );
   my $last_day_in_month = $dt2->day; 

   return ($year, $month, $last_day_in_month);    
}

#
# Copy the files to the $ARCHIVE_DIR and chmod on the files so they 
# can not be easily deleted or written over.
# Remove the file from the temp directory after it has been successfully 
# copied.
#

sub copy_and_chmod_file {
   my ($file, $directory) = @_;

# Copy files to archive location 
   my $result = system("cp -p $DOWNLOAD_DIR/$file $directory/$file");
   if ($result ne "0") {
      sendMailAndDie(sprintf("File: %s/%s could not be copied to %s\n%s\n", $DOWNLOAD_DIR, $file, $directory, $result));
   }
# Chmod file in directory 
   $result = system("chmod 444 $directory/$file");
   if ($result ne "0") {
      sendMailAndDie(sprintf("Error %d for chmod for file: $directory/$file\n", $result/256)); 
   }
# Remove tar file from temp directory 
   unlink ("$DOWNLOAD_DIR/$file");
   return;
}

#
# Insert the data file into the database.
#

sub insert_into_database {
   my ($filename, $last_day_in_month) = @_;

   my $msg = "";

# Create the file information.
   my $mysqlFile = MySqlFile->new();
   $mysqlFile->setDatasetArchiveIdent($DATASET_ID);
   my $file_details = `ls -s $ARCHIVE_DIR/$filename`;
   my @filesize = split(' ', $file_details);
   $mysqlFile->setFile("$ARCHIVE_DIR", "$filename", $filesize[0]);
   $mysqlFile->setFormatId(57);
   $mysqlFile->setHost("localhost");
   $mysqlFile->setBeginDate(substr($filename,5,4), substr($filename,9,2), 1, 0, 0, 0);
   $mysqlFile->setEndDate(substr($filename,5,4), substr($filename,9,2), $last_day_in_month,23,59,59);
# Create and open the database
   my $database = MySqlDatabase->new(); # use ~/.my.cnf
   $database->connect();
# Insert the file
   $msg = $mysqlFile->insert($database);
# Commit if no errors have occurred to this point otherwise rollback.
   if ($msg eq "") { 
      $msg .= $database->commit();
   } else { 
      $msg .= "Database rolled back.\n".$database->rollback(); 
   }
# Always disconnect cleanly.
   $database->disconnect();
   return $msg;
}

#
# Send an email and quit the program if an error has occurred.
#
sub sendMailAndDie {
   my ($body) = @_;
   MAIL::send_mail("HPCN ERROR", $0."\n\n".$body, @monitors);
   exit(1);
}

#
# Send an email and continue.  This is for sending an email
# about possibly new stations.
#
sub sendMailAndContinue {
   my ($body) = @_;
   MAIL::send_mail("HPCN New or Recently Closed Stations", $0."\n\n".$body, @monitors);
}
