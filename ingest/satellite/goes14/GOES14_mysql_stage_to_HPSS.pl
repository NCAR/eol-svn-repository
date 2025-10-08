#! /usr/bin/perl -w

##Module-----------------------------------------------------------------------
# <p>The GOES14_mysql_stage_to_HPSS script is a daily cron script that does the following
# tasks:</p>
# <ol>
#   <li>Create hourly tar files for all previous days to the day the script
# is run.  (This removes all of the files used to create the tar file.)</li>
#   <li>Copy the tar file to the HPSS.  (This removes the tar file from
# the local system if it was copied correctly.</li>
#   <li>Insert the HPSS file into the database.</li>
# </ol>
#
#  @author Linda Echo-Hawk
#  @version 1.1 22 March 2021
#  Revised the MySqlDatabase constructor to pass in the  ~/.my.cnf file
#  containing the dbase and password rather than having these hard-coded
#  in the script. This solved a security issue.
#
#
# @author Janine Aquino May 29, 2015
# Modified from HPSS_HDW_mysql_stage_to_mss.pl
##Module-----------------------------------------------------------------------
package HDWStageToMSS;
use strict;
use lib "/net/work/lib/perl/mysql";
use lib "/net/work/lib/perl/hpss";
use lib "/net/work/lib/perl/mail";
use Date::Manip;
use MySqlDatabase;
use MySqlDataset;
use MySqlMSSFile;
use HPSS;
use MAIL;

#Constants
my @monitors = ("eol-cds-ingest\@ucar.edu");

# TO ADD A NEW FEED THAT IS IN THE SAME INGEST DIR, 
# # CREATE A CODIAC DATASET FOR IT AND ADD TO THE HASH BELOW.
# # Be sure to create the feed subdir in temp dir.
my %datasets;
$datasets{"srso"}{"c01"}{"dataset_id"} = "485.002";
$datasets{"srso"}{"c02"}{"dataset_id"} = "485.003";
$datasets{"srso"}{"c03"}{"dataset_id"} = "485.004";
$datasets{"srso"}{"c04"}{"dataset_id"} = "485.005";
$datasets{"srso"}{"c06"}{"dataset_id"} = "485.006";

foreach my $dataset (keys %datasets) {
    foreach my $channel (sort keys % { $datasets{$dataset}}) {
        my $ingest = "/export/ldm/data/$dataset";
        my $temp = "/scr/tmp/joss/$dataset";
        my $mss = "/EOL/2015/pecan/satellite/$dataset";

	&main($datasets{$dataset}{$channel}{"dataset_id"},$channel,$ingest,$temp,$mss);
  }
}

##-----------------------------------------------------------------------------
# @signature void main()
# <p>Run the script.</p>
##-----------------------------------------------------------------------------
sub main {
    my $dataset_id = shift;
    my $channel = shift;
    my $ingest = shift;
    my $temp = shift;
    my $mss = shift;


    # Create the new tar files.
    my $report = &create_tar_files($ingest,$temp,$channel);
    
    # Read in all of the tar files in the temp directory
    opendir(my $TARS,$temp) or sendMailAndDie("Cannot open $temp\n");
    my @tar_files = grep(/\.tar.gz$/,readdir($TARS));
    closedir($TARS);

    # Put the tar files on the mass store and insert them into the database.
    foreach my $file (sort @tar_files ) {

	my $msg = place_on_mss($temp,$mss,$file);

	if ($msg eq "") {
	    $report .= insert_file($dataset_id,$mss,$file);
	    unlink(sprintf("%s/%s",$temp,$file));
	} else {
	    $report .= $msg;
	}
    }

    # Send out an email that the script has finished.
    if ($report ne "") { sendMailAndDie($report); }
    #else { sendMailAndDie("There are no errors to report"); }
}

##-----------------------------------------------------------------------------
# @signature String[] create_tar_files()
# <p>Read the data in the ingest directory and create the tar balls that are
# to be placed on the mass store.</p>
##-----------------------------------------------------------------------------
sub create_tar_files {
    my $ingest = shift;
    my $temp = shift;
    my $channel = shift;

    my $report = "";
    my ($sec,$min,$hour,$day,$mon,$year,$wday,$doy,$isdst) = localtime(time());
    my $today = sprintf("%04d%03d",$year+1900,$doy+1);
    my @files = ();

    # Get the list of files that will be used to generate the tar files.
    opendir(my $INGEST,$ingest) or sendMailAndDie("Cannot open $ingest\n");
    foreach my $file (sort(readdir($INGEST))) {
	push(@files,$file) if ($file =~ /^CIRA_\d+i14\.$channel$/ && $file !~ /^CIRA_$today/);
    }
    closedir($INGEST);

    # Confirm that there are new files to archive. If not, warn user
    if(@files==0){sendMailAndDie("No files exist in the ingest location: $ingest.\n");}

    # Split the files into lists by date, hour, and band
    my %tar_hash;
    foreach my $file (sort(@files)) {
	push(@{ $tar_hash{substr($file,5,9)}{substr($file,-3,3)}},$file);
    }

    # Create the tar files for each date, hour, and band
    my @tar_files = ();
    chdir($ingest) or sendMailAndDie("Cannot change to $ingest\n");
    foreach my $date (sort(keys %tar_hash )) {
      foreach my $band (sort(keys %{ $tar_hash{$date}}) ) {
	if (system(sprintf("/bin/tar -zcf %s/%s.%s.tar.gz %s",$temp,$date,$band,
			   join(" ",@{ $tar_hash{$date}{$band}})))) {
	    $report .= "$date.$band.tar.gz was not able to be created.\n";
	} else {
	    foreach my $file (@{ $tar_hash{$date}{$band}}) {
	    	$report .= "$file was not able to be removed.\n" if (!unlink($file));
	    }
	    push(@tar_files,sprintf("%s.%s.tar.gz",$date,$band));
	}
      }
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
    my $mss = shift;
    my ($file) = @_;

    # Create the file information.
    my $mysql = MySqlMSSFile->new();
    $mysql->setDatasetArchiveIdent($dataset_id);
    $mysql->setFile(sprintf("%s/%04d",$mss,substr($file,0,4)),$file);
    $mysql->setFormatId(53);
    my ($year, $mon, $day) = &Date_NthDayOfYear (substr($file,0,4),substr($file,4,3));
    $mysql->setBeginDate($year,$mon,$day,substr($file,7,2),0,0);
    $mysql->setEndDate($year,$mon,$day,substr($file,7,2),59,59);

    # Create and open the database
    my $database = MySqlDatabase->new(); # use ~/.my.cnf
    $database->connect();

    # Insert the file
    my $msg = $mysql->insert($database);

    # Commit if no errors have occurred to this point otherwise rollback.
    if ($msg eq "") { $msg .= $database->commit(); }
    else { $msg .= "Database rolled back.\n".$database->rollback(); }

    # Always disconnect cleanly.
    $database->disconnect();

    return $msg;
}

##-----------------------------------------------------------------------------
# @signature String place_on_mss(String file)
# <p>Copy the specified file to the mass store.</p>
#
# @input $file The file to be copied to the mass store.
# @output $msg Any error messages that occured during the copy or the empty
# String if it was copied successfully.
##-----------------------------------------------------------------------------
sub place_on_mss {
    my $temp = shift;
    my $mss = shift;
    my ($file) = @_;
    my $year = substr($file,0,4);

    return HPSS::put(\"$temp/$file",\"$mss/$year/$file");
}

sub sendMailAndDie {
    my ($body) = @_;
    MAIL::send_mail("LDM GOES-14 SRSO Satellite McIDAS data MSS/CODIAC Script Error\n",$0."\n\n".$body, @monitors);
    exit(1);
}
