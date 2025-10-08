#! /usr/bin/perl -w

##Module-----------------------------------------------------------------------
# <p>The MadisStageToMSS script is a daily cron script that does the following
# tasks:</p>
# <ol>
#   <li>Create daily tar files for all previous days to the day the script
# is run.  (This removes all of the files used to create the tar file.)</li>
#   <li>Copy the tar file to the mass store.  (This removes the tar file from
# the local system if it was copied correctly.</li>
#   <li>Insert the mass store file into the database.</li>
# </ol>
#  
#  revised 2025 Feb 6 Daniel Choi
#  Added in ability to updated checksum.
#
#  @author Linda Echo-Hawk
#  @version 3.0 22 March 2021
#  Revised the MySqlDatabase constructor to pass in the  ~/.my.cnf file
#  containing the dbase and password rather than having these hard-coded
#  in the script. This solved a security issue.
#
# revised 2020 Dec 02 (cbsnyder):
# <p>Add functions to copy the data directly to Campaign Storage archive location.</p>
#
# revised 2010 Dec 01:
# <p>Temp location moved from /ingest to /net/ingest.  Updated script to reflect this.</p>
#
# revised 2008 Mar 07:
# <p>Ingest location moved from /ingest to /scr.  Updated script to reflect this.</p>
#
# @author Joel Clawson 2006/07/19
# @version 2.2 Updated from the joss user for jedi on hurricane.joss to the
# ingest user for zedi on tsunami.eol.  Changed the email header to send
# from the joss user and reply to me (Joel).
#
# @author Joel Clawson 2006/05/11
# @version 2.1 Transfered from hurricane onto tsunami.  Updated all JOSS email
#    addresses to EOL.  Changed program references to their locations on tsunami.
#
# @author Joel Clawson
# @version 2.0 Updates the original stage_to_mss script from empress to mysql.
#
##Module-----------------------------------------------------------------------
package MadisStageToMSS;
use strict;
use lib "/net/work/lib/perl/mysql";
use lib "/net/work/lib/perl/mail";
use MySqlDatabase;
use MySqlDataset;
use MySqlFile;
use MAIL;
use DateTime;

# Constants
# Campaign Storage user, host, archive path
my $cs_host = "eoldata\@data-access.ucar.edu";
my $cs_name = "data-access.ucar.edu";
my @monitors = ("eol-cds-ingest\@ucar.edu");
my $report = "";
my $cs_archive = "";

# TO ADD A NEW MESONET FEED THAT IS IN THE SAME INGEST DIR, 
# CREATE A CODIAC DATASET FOR IT AND ADD TO THE HASH BELOW.
# Be sure to create feed subdir in temp dir.
my %datasets;
$datasets{"mesonet1"}{"dataset_id"} = "100.001";
$datasets{"crn"}{"dataset_id"} = "100.023";
$datasets{"urbanet"}{"dataset_id"} = "100.024";
$datasets{"hydro2"}{"dataset_id"} = "100.025";
$datasets{"snow"}{"dataset_id"} = "100.026";
$datasets{"maritime"}{"dataset_id"} = "100.027";
$datasets{"metar"}{"dataset_id"} = "100.028";

foreach my $dataset (sort(keys %datasets)) {
    my $dataset_id = $datasets{$dataset}{"dataset_id"};
    my $ingest = "/data/ldm/data/madis/$dataset";
    my $temp = "/scr/tmp/joss/madis/$dataset";
# The archive will be /.../surface/MADIS/<CRN>/<year>
    $cs_archive = "/glade/campaign/eol/archive/operational/surface/MADIS/" . uc($dataset);

    &main($dataset_id,$ingest,$temp,$cs_archive);

# Send out an email that the script has finished if on the last dataset.
   if (($report ne "") && ($dataset eq "urbanet")) { sendMailAndDie($report); }
#      else { sendMailAndDie("There are no errors to report"); }

    CONTINUE:
}

##-----------------------------------------------------------------------------
# @signature void main()
# <p>Run the script.</p>
##-----------------------------------------------------------------------------
sub main {
    my $dataset_id = shift;
    my $ingest = shift;
    my $temp = shift;
    my $cs_archive = shift;
    my $dt = DateTime->now;
    my $dt_to_tar = $dt - DateTime::Duration->new( days => 1 );
    my $days_to_tar = 1;

# Verify that the campaign store is up
   my $result = system("/bin/ping -c 1 -W 1 $cs_name >/dev/null 2>&1");
   if ($result ne "0") {
      sendMailAndDie("The Campaign Store is not up and running.\n");
   }

# Check how many days need to be tarred.
# If the day already exists on the campaign store, then stop looking
    my $check_date = $dt_to_tar - DateTime::Duration->new( days => 1 );
    my $check_year = $check_date->year;
    my $check_file = sprintf("%04d%02d%02d.tar", $check_year, $check_date->month, $check_date->day);
    $result = system("ssh $cs_host [ -f $cs_archive/$check_year/$check_file ] 2>/dev/null");
    while ($result ne "0") {
       $days_to_tar = $days_to_tar + 1;
       $check_date = $check_date - DateTime::Duration->new( days => 1 );
       $check_year = $check_date->year;
       $check_file = sprintf("%04d%02d%02d.tar", $check_year, $check_date->month, $check_date->day);
       $result = system("ssh $cs_host [ -f $cs_archive/$check_year/$check_file ] 2>/dev/null");
    }

# Create the new tar files.
    create_tar_files($ingest,$temp,$days_to_tar);
    
# Read in all of the tar files in the temp directory
    opendir(my $TARS,$temp) or sendMailAndContinue("Cannot open $temp\n");
    my @tar_files = grep(/\.tar$/,readdir($TARS));
    closedir($TARS);

    if ($#tar_files != -1) { # -1 means an empty array
# get file year from first file
       my $cs_year = substr($tar_files[0],0,4);

# check if YYYY dir exists on Campaign Storage, if not, create it
       $result = system("ssh $cs_host [ -d $cs_archive/$cs_year ] 2>/dev/null");
# If command does not return 0, check for year directory again, because possibly received
# an ssh error.
       if ($result ne "0") {
          $result = system("ssh $cs_host [ -d $cs_archive/$cs_year ] 2>/dev/null");
	  if ($result ne "0") {
             $result = `ssh $cs_host  mkdir $cs_archive/$cs_year 2>&1`;
             if ($result ne "") {
	        $report .= "Could not create directory $cs_archive/$cs_year on campaign store.\n";
	     }
          }
       }

# Put the tar files on the mass store and insert them into the database.
       foreach my $file (sort(@tar_files)) {

# place data on Campaign Store
	   my $msg = copy_to_CS($temp,$cs_archive,$cs_host,$file);

	   if ($msg eq "") {
               my $insert_result = insert_file($dataset_id, $file);
# Remove tar file if file size was not zero
	       if (($insert_result eq "") || (substr($insert_result, 1, 9) ne "File size")) {
	          unlink(sprintf("%s/%s",$temp,$file));
	       }
           $report .= $insert_result;
#	   } elsif ($msg =~ /CS file already exists:/ && 
#	       $dataset_id == "100.026") {
	    # The snow network keeps a month of files. If I already loaded
	    # it, go ahead and delete it.
#	       unlink(sprintf("%s/%s",$temp,$file));
	   } else {
	       $report .= $msg;
	   }
       }

# Chmod files in Campaign Storage directory
       $result = `ssh $cs_host chmod 440 $cs_archive/$cs_year/*.tar 2>&1`;
       $report .= $result;


       # Update checksum file.

       $result = system("ssh $cs_host python3 eoldata-utils/cksum/checksum_utility.py -n -d $cs_archive/$cs_year >/dev/null");
       if ($result != 0) {
           sendMailAndDie(sprintf("Error with checksum command.\n", $result/256));
       }



    }

}

##-----------------------------------------------------------------------------
# @signature String[] create_tar_files()
# <p>Read the data in the ingest directory and create the tar balls that are
# to be placed on the mass store.</p>
##-----------------------------------------------------------------------------
sub create_tar_files {
    my $ingest = shift;
    my $temp = shift;
    my $days_to_tar = shift;

    my $dt = DateTime->now;
    my $dt_to_tar = $dt - DateTime::Duration->new( days => 1 );

    my $today = sprintf("%04d%02d%02d", $dt_to_tar->year, $dt_to_tar->month, $dt_to_tar->day);

    chdir($ingest) or sendMailAndContinue("Cannot change to $ingest\n");
    for (my $i = 1; $i <= $days_to_tar; $i++) {
# Get the list of files that will be used to generate the tar files.
       opendir(my $INGEST,$ingest) or sendMailAndContinue("Cannot open $ingest\n");
       my @files = sort(grep(/^$today.*$/, readdir($INGEST)));
       closedir($INGEST);

# Make sure there are files to archive
       if ($#files <= 0) {
          $report .= "No files exist in the ingest location $ingest for $today.\n";
       } else {
# Create the tar file 
          my $result = system("/bin/tar -cf $temp/$today.tar $today*");
          if ($result ne "0") {
	     $report .= "$temp/$today.tar was not able to be created. Return code - $result\n";
          } else {
# Remove data files from the ldm directory
             $result = unlink glob("$ingest/$today*");
	  } 
       }
       $dt_to_tar = $dt_to_tar - DateTime::Duration->new( days => 1 );
       $today = sprintf("%04d%02d%02d", $dt_to_tar->year, $dt_to_tar->month, $dt_to_tar->day);
    }
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
    my $dataset_id = shift;
    my $filename = shift;

    my $msg = "";

# Create the file information.
    my $mysqlFile = MySqlFile->new();
    $mysqlFile->setDatasetArchiveIdent($dataset_id);

    my $cs_year = substr($filename,0,4);
    my $file_details = `ssh $cs_host ls -s $cs_archive/$cs_year/$filename`;
    my @filesize = split(' ', $file_details);
# If file size is 0, try to get size again.
    if (($filesize[0] eq "") or ($filesize[0] == 0)) {
       $file_details = `ssh $cs_host ls -s $cs_archive/$cs_year/$filename`;
       @filesize = split(' ', $file_details);
       $report .= "Second time file size for $cs_archive/$cs_year/$filename - size $filesize[0]\n";
    }
# If file size is still 0, don't try and insert into database.
    if ($filesize[0] == 0) {
       return "File size is 0 for $cs_archive/$cs_year/$filename\n";
    }
    $mysqlFile->setFile("$cs_archive/$cs_year", "$filename", $filesize[0]);
    $mysqlFile->setFormatId(53);
    $mysqlFile->setHost("campaign");
    $mysqlFile->setBeginDate(substr($filename,0,4),substr($filename,4,2),substr($filename,6,2),0,0,0);
    $mysqlFile->setEndDate(substr($filename,0,4),substr($filename,4,2),substr($filename,6,2),23,59,59);

# Create and open the database
    my $database = MySqlDatabase->new(); # use ~/.my.cnf
    $database->connect();
   
# Insert the file
    $msg = $mysqlFile->insert($database);

# Commit if no errors have occurred to this point otherwise rollback.
    if ($msg eq "") { $msg .= $database->commit(); }
    else { $msg .= "Database rolled back.\n".$database->rollback(); }

# Always disconnect cleanly.
    $database->disconnect();

    return $msg;
}

###################################################
## Copy file to Campaign Storage archive location #
###################################################
sub copy_to_CS {
    my $temp = shift;
    my $cs_archive = shift;
    my $cs_host = shift;
    my ($file) = @_;

    my $year = substr($file,0,4);

    my $rcode = `scp $temp/$file $cs_host:$cs_archive/$year/$file`;
    sleep(60);
    return($rcode);
}

##-----------------------------------------------------------------------------
# @signature void sendMailAndContinue(String body)
# <p>Send an email that a terminal error has been found and continue to the next
# product.</p>
#
# @input $body The message for the email.
##-----------------------------------------------------------------------------
sub sendMailAndContinue {
    my ($body) = @_;
    MAIL::send_mail("LDM MADIS CS/CODIAC Error\n",$0."\n\n".$body, @monitors);
    goto CONTINUE;
}

##-----------------------------------------------------------------------------
# @signature void sendMailAndDie(String body)
# <p>Send an email that a terminal error has been found and quit running the 
# script.</p>
#
# @input $body The message for the email.
##-----------------------------------------------------------------------------
sub sendMailAndDie {
    my ($body) = @_;
    MAIL::send_mail("LDM MADIS CS/CODIAC Error\n",$0."\n\n".$body, @monitors);
    exit(1);
}
