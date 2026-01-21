#! /usr/bin/perl -w

##Module-----------------------------------------------------------------------
# <p>The GTS_BUFR_tar_to_archive.pl script is a daily cron script 
# that does the following tasks:</p>
# <ol>
#   <li>Creates a daily tar file for 100.030: GTS BUFR Radiosonde Data</li>  
##   <li>Archives tar file to the /net/archive location.</li>
##   <li>Inserts the tar file into the database.</li>
## </ol>
##
#  @author Linda Echo-Hawk
#  @version 1.1 11 March 2021
#  Revised the MySqlDatabase constructor to pass in the  ~/.my.cnf file 
#  containing the dbase and password rather than having these hard-coded 
#  in the script. This solved a security issue.
#
#
## @author Linda Echo-Hawk
## @version 1.0 October 2019
##
###Module-----------------------------------------------------------------------
#
use strict;
use lib "/net/work/lib/perl/hpss";
use lib "/net/work/lib/perl/mail";
use lib "/net/work/lib/perl/mysql";
use MySqlDatabase;
use MySqlMSSFile;
use HPSS;
use DateTime;
use MAIL;

my $dataset_id = "100.030";
my $ingest = "/data/ldm/data/bufr_sndg";
my $temp = "/scr/tmp/joss/operational/GTS_BUFR";
my $archive = "/net/archive/data/operational/upperair/radiosonde/GTS_BUFR";
my @monitors = ('eol-cds-ingest@ucar.edu');

&main();

##-----------------------------------------------------------------------------
# @signature void main()
# <p>Run the script.</p>
##-----------------------------------------------------------------------------
sub main {

#  Create the tar file for one day ago
   my $dt = DateTime->now;
   my $day_to_download = $dt - DateTime::Duration->new( days => 1 );
   my $year = $day_to_download->year;
   my $month = sprintf("%02d", $day_to_download->month);
   my $day = sprintf("%02d", $day_to_download->day);
   chdir("$ingest") or sendMailAndDie("Cannot change to directory $ingest\n");

# Verify that there are files to tar
   opendir(PDIR,"$ingest");
   my @files = grep(/$year$month$day.+_sounding.bufr$/,readdir(PDIR));
   closedir(PDIR);
   if ($#files < 0) {
      sendMailAndDie("There are no data files to tar for $month/$day/$year.\n");
   }
   
   my $tar_file = "GTS_BUFR_sonde_$year$month$day.tar";
   my $result = system("tar cf $temp/$tar_file *_$year$month$day*_sounding.bufr");
   if ($result != 0) {
      sendMailAndDie(sprintf("There was an error creating the tar file for %02d %02d %04d.\n  Return code from tar: %d\n",
         $month,$day,$year,$result/256));
   }
#   else
#   {
#       my $rmfiles_result = system("rm $ingest/*_$year$month$day*_sounding.bufr");
#       if $rmfiles_result != 0) {
#       sendMailAndDie(sprintf("There was an error removing the ingest file for %02d %02d %04d.\n  Return code from rm: %d\n", $month,$day,$year,$result/256));
#   }

   my $size = -s "$temp/$tar_file";
# Round to nearest integer.  Adding .5 takes care of rounding, since the int function will return the integer part.
   $size = int(($size / 1024) + .5);

   my $move_result = system("mv $temp/$tar_file $archive/");
   if ($move_result != 0) {
      sendMailAndDie(sprintf("There was an error moving the tar file to the archive area for %02d %02d %04d.\n  Return code from move: %d\n",
         $month,$day,$year,$move_result/256));
   }
    
    # Run checksum utility

    my $checksum_res = system( "python3 ~eoldata/eoldata-utils/cksum/checksum_utility.py -n -d $archive >/dev/null");
    if ($checksum_res != 0) {
      my $check_out = $checksum_res/256;
      sendMailAndDie(sprintf("Error with checksum command. %03d \n", $check_out));
    }

#  Copy the tar file to the HPSS
#   $result = HPSS::put(\"$temp/$tar_file",\"$hpss/$tar_file");
#   if ($result ne "") {
#      sendMailAndDie($result);
#   }

#  Insert the tar file into the database
   (my $msg, my $archive_size) = insert_file($tar_file, $year, $month, $day);
   if ($msg ne "") {
      sendMailAndDie($msg);
   }

   if ($archive_size != $size) { 
       # unlink "$temp/$tar_file"; 
       sendMailAndDie(sprintf("The archive size and size of tar file differ for %02d %02d %04d.\n", $month,$day,$year));
   }
#   my $size = -s "$temp/$tar_file" == TOO LATE - has already been moved to
#   /net/archive!
# Round to nearest integer.  Adding .5 takes care of rounding, since the int function will return the integer part.
#   $size = int(($size / 1024) + .5);
   # if ($hpss_size == $size) { 
   # if ($archive_size == $size) { 
   # if ($archive_size != $size) { 
       # unlink "$temp/$tar_file"; 
       #sendMailAndDie(sprintf("The archive size and size of tar file differ for %02d %02d %04d.\n", $month,$day,$year));
       #}

#   sendMailAndDie("The tar file for $month/$day/$year was created, copied to HPSS, and loaded to the database successfully.\n");
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
   my ($file, $year, $month, $day) = @_;
    
   # Create the file information.
   my $mysql = MySqlFile->new();
   $mysql->setDatasetArchiveIdent($dataset_id);
   $mysql->setFile($archive, $file);
   $mysql->setFormatId(53);
   $mysql->setBeginDate($year, $month, $day, 0, 0, 0);
   $mysql->setEndDate($year, $month, $day, 23, 59, 59);

   # Create and open the database
   my $database = MySqlDatabase->new(); # use ~/.my.cnf
   $database->connect();

   # Insert the file
   my $msg = $mysql->insert($database);

   # Commit if no errors have occurred to this point otherwise rollback.
   if ($msg eq "") { 
      $msg .= $database->commit(); 
   }
   else { 
      $msg .= "Database rolled back.\n" . $database->rollback(); 
   }

   # Always disconnect cleanly.
   $database->disconnect();

   return ($msg, $mysql->getSize());
}

##-----------------------------------------------------------------------------
# @signature sendMailAndDie()
# <p>Send email to monitors if there was an error during the cron job.</p>
##-----------------------------------------------------------------------------
sub sendMailAndDie
{
   my ($body) = @_;
   MAIL::send_mail("GTS BUFR soundings Output- ERROR",$0."\n\n".$body, @monitors);
   exit(1);
}
