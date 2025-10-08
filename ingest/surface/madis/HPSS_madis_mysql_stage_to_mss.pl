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
use lib "/net/work/lib/perl/hpss";
use lib "/net/work/lib/perl/mail";
#use lib "/h/eol/dmg/HPSS_cronjobs/lib";
use MySqlDatabase;
use MySqlDataset;
use MySqlMSSFile;
use MAIL;
use HPSS;

# Constants
my @monitors = ("eol-cds-ingest\@ucar.edu");
my $report = "";

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

foreach my $dataset (keys %datasets) {
    my $dataset_id = $datasets{$dataset}{"dataset_id"};
    my $ingest = "/export/ldm/data/madis/$dataset";
    my $temp = "/scr/tmp/joss/madis/$dataset";
    my $mss = "/FS/EOL/operational/surface/MADIS/".uc($dataset);
    my $cs = "/glade/campaign/eol/archive/operational/surface/MADIS/".uc($dataset);

    &main($dataset_id,$ingest,$temp,$mss,$cs);
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
    my $cs = shift;

    # Create the new tar files.
    create_tar_files($ingest,$temp);
    
    # Read in all of the tar files in the temp directory
    opendir(my $TARS,$temp) or sendMailAndDie("Cannot open $temp\n");
    my @tar_files = grep(/\.tar$/,readdir($TARS));
    closedir($TARS);

    # get file year from first file
    my $year_cs = substr($tar_files[0],0,4);
    my $host_cs = 'eoldata@data-access.ucar.edu';
    # check if YYYY dir exists on Campaign Storage, if not, create it
    my $mkdir_out = mkdir_CS($year_cs,$cs,$host_cs);

    # Put the tar files on the mass store and insert them into the database.
    foreach my $file (sort(@tar_files)) {

	my $msg = place_on_mss($temp,$mss,$file);
	# place data on Campaign Store
	$msg .= place_on_cs($temp,$cs,$host_cs,$file);

	if ($msg eq "") {
	    $report .= insert_file($mss,$dataset_id,$file);
	    unlink(sprintf("%s/%s",$temp,$file));
	} elsif ($msg =~ /HPSS file already exists:/ && 
	    $dataset_id == "100.026") {
	    # The snow network keeps a month of files. If I already loaded
	    # it, go ahead and delete it.
	    unlink(sprintf("%s/%s",$temp,$file));
	} else {
	    $report .= $msg;
	}
    }

    # Chmod files in Campaign Storage directory
    my $my_cs_chmod = `ssh $host_cs chmod 440 $cs/$year_cs/*.tar 2>&1`;
    $report .= $my_cs_chmod;

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
	push(@files,$file) if ($file =~ /^\d+\_\d+\.gz$/ && $file !~ /^$today/);
    }
    closedir($INGEST);

    # Make sure there are files to archive
    if(@files==0){sendMailAndDie("No files exist in the ingest location $ingest.\n");}

    # Split the files into lists by their date
    my %tar_hash;
    foreach my $file (sort(@files)) {
	push(@{ $tar_hash{substr($file,0,8)}},$file);
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

##################################################
# If YYYY directory does not exist in Campaign   #
# Storage archive location, create it            #
# ################################################
sub mkdir_CS {
    my $yr = shift;
    my $subdir = shift;
    my $cs_host = shift;
    ## check if year directory exists, if not create it
    my $exists = 0;
    my @date_dirs = `ssh $cs_host ls $subdir 2>&1`;
    my $mkdir;
    chomp @date_dirs;
    foreach my $dir (sort(@date_dirs)) {
        if ($dir eq $yr) {
            $exists = 1;
        }
    }
    if (!$exists) {
       $mkdir = `ssh $cs_host  mkdir $subdir/$yr 2>&1`;
    }
    return $mkdir;
}

###################################################
## Copy file to Campaign Storage archive location #
###################################################
sub place_on_cs {
    my $temp = shift;
    my $cs = shift;
    my $cshost = shift;
    my ($file) = @_;

    my $year = substr($file,0,4);

    return `scp $temp/$file $cshost:$cs/$year/$file`;
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
    MAIL::send_mail("LDM MADIS MSS/CODIAC Error",$0."\n\n".$body, @monitors);
    goto CONTINUE;
}

