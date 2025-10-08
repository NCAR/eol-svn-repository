#! /usr/bin/perl -w

##Module-----------------------------------------------------------------------
# <p>The okmeso_stage_to_hpss script is a daily cron script that does the 
# following tasks:</p>
# <ol>
#   <li>Create daily tar files for all previous days to the day the script
# is run.  (This removes all of the files used to create the tar file.)</li>
#   <li>Copy the tar file to the HPSS (This removes the tar file from
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
# @author Janine Aquino 2015/05/29 - copied from HPSS_madis script.
#
##Module-----------------------------------------------------------------------
package MadisStageToMSS;
use strict;
use lib "/net/work/lib/perl/mysql";
use lib "/net/work/lib/perl/hpss";
use lib "/net/work/lib/perl/mail";
use MySqlDatabase;
use MySqlDataset;
use MySqlMSSFile;
use MAIL;
use HPSS;

# Constants
my @monitors = ("eol-cds-ingest\@ucar.edu");
my $report = "";

# TO ADD A NEW FEED THAT IS IN THE SAME INGEST DIR, 
# CREATE A CODIAC DATASET FOR IT AND ADD TO THE HASH BELOW.
# Be sure to create the feed subdir in temp dir.
my %datasets;
$datasets{"okmeso"}{"dataset_id"} = "485.001";

foreach my $dataset (keys %datasets) {
    my $dataset_id = $datasets{$dataset}{"dataset_id"};
    my $ingest = "/export/ldm/data/$dataset";
    my $temp = "/scr/tmp/joss/$dataset";
    my $mss = "/EOL/2015/pecan/surface/".$dataset;

    &main($dataset_id,$ingest,$temp,$mss);
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
    my $mss = shift;

    # Create the new tar files.
    create_tar_files($ingest,$temp);
    
    # Read in all of the tar files in the temp directory
    opendir(my $TARS,$temp) or sendMailAndDie("Cannot open $temp\n");
    my @tar_files = grep(/\.tar$/,readdir($TARS));
    closedir($TARS);

    # Put the tar files on the mass store and insert them into the database.
    foreach my $file (sort(@tar_files)) {

	my $msg = place_on_mss($temp,$mss,$file);

	if ($msg eq "") {
	    $report .= insert_file($mss,$dataset_id,$file);
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
    my ($sec,$min,$hour,$day,$mon,$year) = localtime(time());
    my $today = sprintf("%04d%02d%02d",$year+1900,$mon+1,$day);
    my @files = ();

    # Get the list of files that will be used to generate the tar files.
    opendir(my $INGEST,$ingest) or sendMailAndDie("Cannot open $ingest\n");
    foreach my $file (sort(readdir($INGEST))) {
	push(@files,$file) if ($file =~ /^mesonet.realtime.*.[mdf|mts]$/ && $file !~ /mesonet.realtime.$today/);
    }
    closedir($INGEST);

    # Make sure there are files to archive
    if(@files==0){sendMailAndDie("No files exist in the ingest location $ingest.\n");}

    # Split the files into lists by their date
    my %tar_hash;
    foreach my $file (sort(@files)) {
	push(@{ $tar_hash{substr($file,17,8)}},$file);
    }

    # Create the tar files for each date
    my @tar_files = ();
    chdir($ingest) or sendMailAndDie("Cannot change to $ingest\n");
    foreach my $date (sort(keys(%tar_hash))) {
	if (system(sprintf("/bin/tar -cf %s/%s.tar %s",$temp,$date,
			   join(" ",@{ $tar_hash{$date}})))) {
	    $report .= "$date.tar was not able to be created.\n";
	} else {
	    foreach my $file (@{ $tar_hash{$date}}) {
		$report .= "$file was not able to be removed.\n" if (!unlink($file));
	    }
	    push(@tar_files,sprintf("%s.tar",$date));
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
    my $mss = shift;
    my $dataset_id = shift;
    my ($file) = @_;

    # Create the file information.
    my $mysql = MySqlMSSFile->new();
    $mysql->setDatasetArchiveIdent($dataset_id);
    $mysql->setFile(sprintf("%s/%04d",$mss,substr($file,0,4)),$file);
    $mysql->setFormatId(53);
    $mysql->setBeginDate(substr($file,0,4),substr($file,4,2),substr($file,6,2),0,0,0);
    $mysql->setEndDate(substr($file,0,4),substr($file,4,2),substr($file,6,2),23,59,59);

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

##-----------------------------------------------------------------------------
# @signature void sendMailAndDie(String body)
# <p>Send an email that a terminal error has been found and quit running the 
# script.</p>
#
# @input $body The message for the email.
##-----------------------------------------------------------------------------
sub sendMailAndDie {
    my ($body) = @_;
    MAIL::send_mail("LDM PECAN OKMESO HPSS/CODIAC Error",$0."\n\n".$body, @monitors);
    goto CONTINUE;
}

