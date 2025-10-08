#! /usr/bin/perl -w

##Module-----------------------------------------------------------------------
# <p>The HDW1hStageToMSS script is a daily cron script that does the following
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
#  @version 3.1 21 September 2022
#  Added the "$mysql->setHost("campaign");" line to avoid setting the
#  default to localhost. 
#
#  @author Linda Echo-Hawk
#  @version 3.0 22 March 2021
#  Revised the MySqlDatabase constructor to pass in the  ~/.my.cnf file
#  containing the dbase and password rather than having these hard-coded
#  in the script. This solved a security issue.
#
# revised 2010 Dec 01:
# <p>Temp location moved from /ingest to /net/ingest.  Updated script to reflect this.</p>
#
# revised 2008 Mar 07:
# <p>Ingest location moved from /ingest to /scr.  Updated script to reflect this.</p>
#
# revisited 2005 Mar 14:
# <p>As of today, this data has not been used in any project.  It is
# strictly operational.  Jose believes the VAMOS community might be interested
# in this project, so will will keep it for now.  "You know that as soon as we
# cancel and delete someone will want it!" - SFW
# </p>
#
# @author Joel Clawson July 19, 2006
# @version 2.1 Updated to use the ingest user for zedi on tsunami.eol from the
# joss user for jedi on hurricane.joss.  Updated the email header to send from
# the joss user and reply-to me (Joel).
#
# @author Joel Clawson (Janine made minor Madis -> HDW1h changes)
# @version 2.0  May 26, 2005
# Updates the original HDW1h_stage_to_mss script from empress to 
# mysql. Stolen shamelessly from MADIS_mysql_stage_to_mss. Only changed 
# constants below, and  sendmail subjects from MADIS to MADIS Satellite Winds
# throughout code (3 places).
#
# <H2>Data Collection and Usage Notes</H2>
#
# added to pqact.conf for SFW by Janine, 2004 Feb 5:
# <p> operational (3-hour) and experimental (1-hour) satellite wind data
# products created by NESDIS from GOES satellite imagery and sounder data.
# For more info see the Satellite Wind link under the MADIS home page at
# <a href="http://www-sdd.fsl.noaa.gov/MADIS/madis_satwnd.html">
# http://www-sdd.fsl.noaa.gov/MADIS/madis_satwnd.html</a>
# </p>
#
##Module-----------------------------------------------------------------------
package HDW1hStageToMSS;
use strict;
#use lib "/h/eol/dmg/HPSS_cronjobs/lib";
use lib "/net/work/lib/perl/hpss";
use lib "/net/work/lib/perl/mail";
use lib "/net/work/lib/perl/mysql";
use MySqlDatabase;
use MySqlDataset;
use MySqlMSSFile;
use HPSS;
use MAIL;

# Constants
my $dataset_id = "100.008";
my $ingest = "/export/ldm/data/madis/point/HDW1h/netcdf";
my $temp = "/scr/tmp/joss/madis/HDW1h";
my $mss = "/FS/EOL/operational/satellite/hdw1h";
my $cs = "/glade/campaign/eol/archive/operational/satellite/hdw";
my $cshost = 'eoldata@data-access.ucar.edu';
my @monitors = ("eol-cds-ingest\@ucar.edu");
my $report = "";

&main();

##-----------------------------------------------------------------------------
# @signature void main()
# <p>Run the script.</p>
##-----------------------------------------------------------------------------
sub main {

    # Create the new tar files.
    create_tar_files();
    
    # Read in all of the tar files in the temp directory
    opendir(my $TARS,$temp) or sendMailAndDie("Cannot open $temp\n");
    my @tar_files = grep(/\.tar$/,readdir($TARS));
    closedir($TARS);

    # check if YYYY dir exists on Campaign Storage, if not, create it
    # get year from first file
    my $year = substr($tar_files[0],0,4);
    my $mkdir_out = mkdir_CS($year);
    $report .= $mkdir_out;

    # Put the tar files on the mass store and insert them into the database.
    foreach my $file (sort(@tar_files)) {

	my $msg = place_on_mss($file);
        # place files on Campaign Store
        $msg .= place_on_cs($file);

	if ($msg eq "") {
	    $report .= insert_file($file);
	    unlink(sprintf("%s/%s",$temp,$file));
	} else {
	    $report .= $msg;
	}
    }

    # Chmod files in Campaign Storage directory
    my $my_cs_chmod = `ssh $cshost chmod 440 $cs/$year/* 2>&1`;
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
    my ($sec,$min,$hour,$day,$mon,$year) = localtime(time());
    my $today = sprintf("%04d%02d%02d",$year+1900,$mon+1,$day);
    my @files = ();

    # Get the list of files that will be used to generate the tar files.
    opendir(my $INGEST,$ingest) or sendMailAndDie("Cannot open $ingest\n");
    foreach my $file (sort(readdir($INGEST))) {
	push(@files,$file) if ($file =~ /^\d+\_\d+\.gz$/ && substr($file,0,8) < $today);
    }
    closedir($INGEST);

    # Confirm that there are new files to archive. If not, warn user
    if(@files==0){sendMailAndDie("No files exist in the ingest location.\n");}

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
#	    push(@tar_files,sprintf("%s.tar",$date));
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
    my ($file) = @_;

    # Create the file information.
    my $mysql = MySqlMSSFile->new();
    $mysql->setDatasetArchiveIdent($dataset_id);
    $mysql->setFile(sprintf("%s/%04d",$mss,substr($file,0,4)),$file);
    $mysql->setFormatId(53);
    $mysql->setHost("campaign");
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
    my ($file) = @_;
    my $year = substr($file,0,4);
    
    return HPSS::put(\"$temp/$file",\"$mss/$year/$file");
}

##-----------------------------------------------------------------------------
# @signature String place_on_cs(String file)
# <p>Copy the specified file to the campaign store.</p>
#
# @input $file The file to be copied to the campaign store.
# @output $msg Any error messages that occured during the copy or the empty
# String if it was copied successfully.
##-----------------------------------------------------------------------------
sub place_on_cs {
    my ($file) = @_;
    my $year = substr($file,0,4);

    return `scp $temp/$file $cshost:$cs/$year/$file`;
}

##################################################
# If YYYY directory does not exist in Campaign   #
# Storage archive location, create it            #
# ################################################
sub mkdir_CS {
    my $yr = shift;
    ## check if year directory exists, if not create it
    my $exists = 0;
    my @date_dirs = `ssh $cshost ls $cs 2>&1`;
    my $mkdir='';
    chomp @date_dirs;
    foreach my $dir (sort(@date_dirs)) {
        if ($dir eq $yr) {
            $exists = 1;
        }
    }
    if (!$exists) {
       $mkdir = `ssh $cshost  mkdir $cs/$yr 2>&1`;
    }
    return $mkdir;
}

sub sendMailAndDie {
    my ($body) = @_;
    MAIL::send_mail("LDM MADIS HDW1h Satellite Winds MSS/CODIAC Error",$0."\n\n".$body, @monitors);
    exit(1);
}
