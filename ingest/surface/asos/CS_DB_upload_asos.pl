#! /usr/bin/perl -w

## CS_download_asos.pl
##
## @author J. Scannell
## @version 1.0 - June 2022
## @version 1.1 - Sept 2024
##
## Set up to use the download tar files to send to CS and DB
##
## This script runs once each month and downloads ASOS data from the NCEI web site, 
## tars/zips the data files,
## puts the zip files on the campaign store, and adds the files to the 
## appropriate datasets. 
## Affected datasets: 100.035 for two one-minute data files
##                    100.036 for one five-minute data file
##
## October 2022 - JNS
## 
## Data was moved to a new location at NCEI.  Rewrote script to grab from new location
## and reloaded all 2022 data, since the datasets started with 2022 data.
##
use strict;
use lib "/net/work/lib/perl/mysql";
use lib "/net/work/lib/perl/mail";
use MySqlDatabase;
use MySqlFile;
use DateTime;
use MAIL;

my $FTP_SITE = "https://www.ncei.noaa.gov";
my @FTP_DIR;
$FTP_DIR[0] = "$FTP_SITE/data/automated-surface-observing-system-one-minute-pg1/access";
$FTP_DIR[1] = "$FTP_SITE/data/automated-surface-observing-system-one-minute-pg2/access";
$FTP_DIR[2] = "$FTP_SITE/data/automated-surface-observing-system-five-minute/access";
my $TEMP_DIR = "/scr/tmp/joss/asos";
my $DOWNLOAD_DIR = "$TEMP_DIR/temp";
my $CS_HOST = "data-access.ucar.edu";
my $CS_USER = "eoldata\@$CS_HOST";
my $CS_DIR = "/glade/campaign/eol/archive/operational/surface/asos/ncei";
my @CS_SUBDIR;
$CS_SUBDIR[0] = "$CS_DIR/onemin";
$CS_SUBDIR[1] = "$CS_DIR/onemin";
$CS_SUBDIR[2] = "$CS_DIR/fivemin";
my @INFILENAME;
$INFILENAME[0] = "asos-1min-pg1";
$INFILENAME[1] = "asos-1min-pg2";
$INFILENAME[2] = "asos-5min";
my @OUTFILENAME;
$OUTFILENAME[0] = "ASOS_1min_pg1";
$OUTFILENAME[1] = "ASOS_1min_pg2";
$OUTFILENAME[2] = "ASOS_5min";
my @BEGIN_DATE_POSITION;
$BEGIN_DATE_POSITION[0] = 14;
$BEGIN_DATE_POSITION[1] = 14;
$BEGIN_DATE_POSITION[2] = 10;
my @DATASET_ID;
$DATASET_ID[0] = "100.035";
$DATASET_ID[1] = "100.035";
$DATASET_ID[2] = "100.036";
my @monitors = ("eol-cds-ingest\@ucar.edu");

my @tarfiles = ();
#
# Find the previous month and the last day in the month to download
# the data for.
#
my ($year, $month, $last_day_in_month) = find_download_month();
#
for (my $i = 0; $i < 3; $i++) {
# Create the tar/zip files for each data type
#
   $tarfiles[$i] = "$OUTFILENAME[$i].$year$month.tar.gz";
}
#
# Verify campaign store is up and running before continuing
#
my $result = system("/bin/ping -c 1 -W 1 $CS_HOST >/dev/null 2>&1");
if ($result ne "0") {
   sendMailAndDie("The Campaign Store is not up and running.\n");
}
for (my $i = 0; $i < 3; $i++) {
#
# Copy the data files to the campaign store and chmod the files.
#
   put_on_campaign_store($tarfiles[$i], $CS_SUBDIR[$i]);
#
# Insert each data file into the database.
#
   my $msg = insert_into_database($tarfiles[$i], $CS_SUBDIR[$i], $DATASET_ID[$i], $BEGIN_DATE_POSITION[$i], $last_day_in_month);
   if ($msg ne "") {
      sendMailAndDie("Couldn't insert $tarfiles[$i] into database:\n $msg\n");
   }
#   } else { 
#      sendMailAndDie("No errors to report\n"); 
#   }
}

#
# Create the tar files from the downloaded data
#

sub create_tar_file {
   my ($year, $month, $infilename, $outfilename) = @_;

   my $tar_file = "$outfilename.$year$month.tar.gz";
   chdir("$DOWNLOAD_DIR") or sendMailAndDie("Couldn't chdir to $DOWNLOAD_DIR, $!");
   my $result = system("/bin/tar -czf $TEMP_DIR/$tar_file $infilename*.dat");
   if ($result != 0) {
      sendMailAndDie("$TEMP_DIR/$tar_file could not be created, $!\n");
   }
   unlink glob("$DOWNLOAD_DIR/$infilename*.dat");
   return($tar_file);
}

#
# Donwload the data files to the temp directory using wget.
# wget options used:
#    -N - timestamp checking, if the file has been previously downloaded,
#         and the file is not newer on the server, do not download again.
#    -w 1 - wait between each request so the server is not overloaded
#           with data requests
#    -nd - Do not create a hierarchy of directories (Download files to current directory)
#    -nH - Do not create a host directory
#    -np - Don't ever ascend to the parent directory
#    -r - Recursive retrieving
#    -l 1 - recursion maximum depth level (Don't traverse any directories)
#    -A - file names to accept (filetypes are 6405, 6406 and 6401)
#    -a - append to log file (create log file if it doesn't exist)
#

sub download_files {
   my ($year, $month, $ftpdir) = @_;

   chdir("$DOWNLOAD_DIR") or sendMailAndDie("Couldn't chdir to $DOWNLOAD_DIR, $!");
   my $logfile = sprintf("%s/logs/log-%s-%02d", $DOWNLOAD_DIR, $year, $month);
   my $wgetsite = "$ftpdir/$year/$month";
   my $result = system("wget -N -w 1 -nd -nH -np -r -l 1 -A '*.dat' -a $logfile $wgetsite");
   if ($result != 0) {
      sendMailAndDie(sprintf("There was an error downloading the ASOS data for month: %s%s.\n  Dataset being downloaded: %s.\n Return code from wget: %d\n\n\n",
         $year,$month,$wgetsite,$result/256));
   }
   return;
}

#
# Find the previous month for which the files will be downloaded.
# Also determine the last day of the month for that month to use
# for the ending date in the database.
#

sub find_download_month {

   my $dt = DateTime->now;
   my $month_to_download = $dt - DateTime::Duration->new( months => 1 );
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
# Copy the files to the campaign store and chmod on the files so they 
# can not be easily deleted.
# Remove the file from the temp directory after it has been successfully 
# copied.
#

sub put_on_campaign_store {
   my ($file, $directory) = @_;

# Copy files to archive location 
   my $result = `scp $TEMP_DIR/$file $CS_USER:$directory/$file 2>&1`;
   if ($result ne "") {
      sendMailAndDie(sprintf("File: %s/%s could not be copied to %s\n%s\n", $TEMP_DIR, $file, $directory, $result));
   }
# Give the scp command time to finish before continuing
   sleep(60);
# Chmod files in Campaign Storage directory 
   $result = system("ssh $CS_USER chmod 440 $directory/$file >/dev/null");
   if ($result != 0) {
      sendMailAndDie(sprintf("Error %d for chmod for file: $directory/$file\n", $result/256)); 
   }
# Remove tar file from temp directory 
   unlink ("$TEMP_DIR/$file");
   return;
}

#
# Insert the data file into the database.
#

sub insert_into_database {
   my ($filename, $directory, $dataset_id, $date_position, $last_day_in_month) = @_;

   my $msg = "";

# Create the file information.
   my $mysqlFile = MySqlFile->new();
   $mysqlFile->setDatasetArchiveIdent($dataset_id);
   my $file_details = `ssh $CS_USER ls -s $directory/$filename`;
   my @filesize = split(' ', $file_details);
   $mysqlFile->setFile("$directory", "$filename", $filesize[0]);
   $mysqlFile->setFormatId(57);
   $mysqlFile->setHost("campaign");
   $mysqlFile->setBeginDate(substr($filename,$date_position,4), substr($filename,$date_position+4,2), 1, 0, 0, 0);
   $mysqlFile->setEndDate(substr($filename,$date_position,4), substr($filename,$date_position+4,2), $last_day_in_month,23,59,59);
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
   MAIL::send_mail("NCEI ASOS ERROR", $0."\n\n".$body, @monitors);
   exit(1);
}
