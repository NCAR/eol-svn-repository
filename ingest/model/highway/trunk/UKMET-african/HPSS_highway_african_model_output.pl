#! /usr/bin/perl -w

##Module-----------------------------------------------------------------------
## <p>The HPSS_highway_african_model_output.pl script is a daily cron script that does the following
## tasks:</p>
## <ol>
##   <li>Create a daily tar.gz file for 572.008: UK Met Office Tropical African Model Output 
##      over the Lake Victoria Region.</li>  
##   <li>Archive tar.gz file to the HPSS.</li>
##   <li>Insert the tar.gz file into the database.</li>
## </ol>
##
#  @author Linda Echo-Hawk
#  @version 1.1 22 March 2021
#  Revised the MySqlDatabase constructor to pass in the  ~/.my.cnf file
#  containing the dbase and password rather than having these hard-coded
#  in the script. This solved a security issue.
#
## @author Janet Scannell 
## @version 1.0 April 2019
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

my $dataset_id = "572.008";
my $ingest = "/net/work/Projects/HIGHWAY/data_ingest/model/UKMET/data/FieldCampaign";
my $temp = "/scr/tmp/joss/highway/african";
my $hpss = "/FS/EOL/2019/highway/model/ukmet/african";
my @monitors = ('eol-cds-ingest@ucar.edu');

&main();

##-----------------------------------------------------------------------------
# @signature void main()
# <p>Run the script.</p>
##-----------------------------------------------------------------------------
sub main {

#  Create the tar file for two days ago
   my $dt = DateTime->now;
   my $day_to_download = $dt - DateTime::Duration->new( days => 2 );
   my $year = $day_to_download->year;
   my $month = sprintf("%02d", $day_to_download->month);
   my $day = sprintf("%02d", $day_to_download->day);
   chdir("$ingest") or sendMailAndDie("Cannot change to directory $ingest\n");

# Verify that there are files to tar
   opendir(PDIR,"$ingest");
   my @files = grep(/^$year$month$day.+_takm4p4/,readdir(PDIR));
   closedir(PDIR);
   if ($#files < 0) {
      sendMailAndDie("There are no data files to tar for $month/$day/$year.\n");
   }
   my $tar_file = "HIGHWAY_UKMET_takm4p4_$year$month$day.tar.gz";
   my $result = system("tar czf $temp/$tar_file $year$month$day*_takm4p4_*");
   if ($result != 0) {
      sendMailAndDie(sprintf("There was an error creating the tar file for %02d %02d %04d.\n  Return code from tar: %d\n",
         $month,$day,$year,$result/256));
   } 
    
#  Copy the tar file to the HPSS
   $result = HPSS::put(\"$temp/$tar_file",\"$hpss/$tar_file");
   if ($result ne "") {
      sendMailAndDie($result);
   }

#  Insert the tar file into the database
   (my $msg, my $hpss_size) = insert_file($tar_file, $year, $month, $day);
   if ($msg ne "") {
      sendMailAndDie($msg);
   }
   my $size = -s "$temp/$tar_file";
# Round to nearest integer.  Adding .5 takes care of rounding, since the int function will return the integer part.
   $size = int(($size / 1024) + .5);
   if ($hpss_size == $size) { 
      unlink "$temp/$tar_file"; 
   }

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
   my $mysql = MySqlMSSFile->new();
   $mysql->setDatasetArchiveIdent($dataset_id);
   $mysql->setFile($hpss, $file);
   $mysql->setFormatId(57);
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
   MAIL::send_mail("HIGHWAY UKMET Tropical African Model Output- ERROR",$0."\n\n".$body, @monitors);
   exit(1);
}
