#! /usr/bin/perl -w

##Module-----------------------------------------------------------------------
# <p>The HPSS_pacs_berbery_mysql_update.pl script is the HPSS upgrade for the
# pacs_berbery_mysql_update.pl script.  It copies files to the archive,
# creates monthly tar files and puts the tar files on the HPSS.</p>

# @author Janine Aquino
# Now (2011) the CFSR archive is better than this. Turned off this 
# data archival process at the request of Hugo Berbery, the
# data provider 2011/06/28 - JAA
#
# @author Janine Aquino
# @version 3.0 Ported to work with HPSS (from MSS).
#
# @author Joel Clawson
# @version 2.0 Updated to MySQL and cleaned up.
#
# @author Phil Dressen
# @version 1.0 Original EmpSQL version.
##Module-----------------------------------------------------------------------
package PacsBerbery;
use strict;
use lib "/net/work/lib/perl/mysql";
use lib "/net/work/lib/perl/mail";
use lib "/net/work/lib/perl/hpss";
#use lib "/h/eol/dmg/HPSS_cronjobs/lib";
use File::Copy;
use MySqlDatabase;
use MySqlDataset;
use MySqlFile;
use MAIL;
use HPSS;

# Constants
my $FTP_DIR = "/net/iftp/pub/incoming/pacs/berbery";
my $WEB_DIR = "/net/archive/data/pacs/model/ETA_Berbery";
my $HPSS_DIR = "/EOL/operational/model/eta";
my $DATASET_ID = "15.900";
my @monitors = ();

my $report = "";
&main();

##---------------------------------------------------------------------------
# @signature void main()
# <p>Run the script.</p>
##---------------------------------------------------------------------------
sub main {
    
    my @files = load_data_files();
    my $this_month = find_today_month();

    # Copy the files and define which tar balls should be created.
    my %tar_file_list;
    my $deleteFileReport = "";
    foreach my $file (@files) {

	if ($file =~ /^pp_(18|24|30|36)\.gif$/) {
            $deleteFileReport .= sprintf("File %s was found.  It will be deleted.\n",$file);
	    unlink(sprintf("%s/%s",$FTP_DIR,$file));
	    next;
	}
	
	if (substr($file,2,6) < $this_month) {
	    my $directory = sprintf("%s/%s",$WEB_DIR,substr($file,2,6));
	    if (copy_file($directory,$file)) {
		# Only insert if the file copied successfully.
		$report .= insert_file($directory,$file);
		$tar_file_list{substr($file,2,6)} = 1;
	    } else {
	    	$report .= sprintf("File %s was not copied.  Insert into database not attempted.\n",$file);
	    }
	}
    }

    # Create the tar balls.
    my @tar_files = (); 
    if ($report eq "") { @tar_files = create_tar_balls(sort(keys(%tar_file_list))); }
    else { $report .= "Tar balls were not created due to previous errors.\n"; }

    # Put the tar balls on the HPSS.
    if ($report eq "") { foreach my $tar (@tar_files) { copy_to_hpss($tar); } }
    else { $report .= "Tar balls were not copied to the HPSS due to previous errors.\n";}

    $report .= $deleteFileReport;
    
    # Send out notification emails.
    if ($report ne "") { sendMailAndDie($report); }
    #else { MAIL::send_mail("PACS Berbery Script Notification","There were no errors to report.\n"); }
}

##---------------------------------------------------------------------------
# @signature boolean copy_file(String directory, String file)
# <p>Copy the file to the specified directory.</p>
#
# @input $directory The destination directory.
# @input $file The name of the file to be copied.
# @output $success A boolean flag if the copy was successful.
##---------------------------------------------------------------------------
sub copy_file {
    my ($directory,$file) = @_;

    # Create the destination directory if it doesn't exist
    mkdir($directory) unless (-e $directory);

    # Make sure the file doesn't already exist.
    if (-e sprintf("%s/%s",$directory,$file)) {
	$report .= sprintf("File %s/%s already exists.  Copy cancelled.\n",$directory,$file);
	return 0;
    }

    # Copy the file.
    if (copy(sprintf("%s/%s",$FTP_DIR,$file),
	     sprintf("%s/%s",$directory,$file))) {

	# Delete the original file after a successful copy.
	if (!unlink(sprintf("%s/%s",$FTP_DIR,$file))) {
	    $report .= sprintf("Unable to delete file %s/%s\n",$FTP_DIR,$file);
	}
	return 1;
    } else {
	$report .= sprintf("Could not copy %s/%s to %s/%s\n",$FTP_DIR,$file,$directory,$file);
	return 0;
    }
}

##---------------------------------------------------------------------------
# @signature void copy_to_hpss(String file)
# <p>Copy the specified tar file to the HPSS.</p>
#
# @input $file The file to copy to the HPSS.
##---------------------------------------------------------------------------
sub copy_to_hpss {
    my ($file) = @_;

     $report .= HPSS::put(\"$WEB_DIR/$file", \"$HPSS_DIR/$file");
}

##---------------------------------------------------------------------------
# @signature void create_tar_balls(String[] dates)
# <p>Create a tar ball for each date in the list.</p>
#
# @input dates[] The list of dates to create tar balls.
##---------------------------------------------------------------------------
sub create_tar_balls {
    my @dates = @_;
    my @tars = ();

    foreach my $date (@dates) {

	# Change to the directory containing the files.
	chdir(sprintf("%s/%s",$WEB_DIR,$date)) or 
	    sendMailAndDie("Cannot change directories.\n");
	
	# Define the tar ball name.
	my $filename = sprintf("%s/%s.tar",$WEB_DIR,$date);

	# Remove an existing tar file so it can be recreated.
	if (-e $filename || -e sprintf("%s.gz",$filename)) {
	    $report .= sprintf("Tar file %s already exists.  Removing to recreate it.\n",
			       $filename);
		unlink($filename) if (-e $filename);
		unlink(sprintf("%s.gz",$filename)) if (-e sprintf("%s.gz",$filename));
	}

	# Make the tar ball.
	if (system(sprintf("/bin/tar -cf %s *.gif",$filename)) == 0) {
	    if (system(sprintf("/bin/gzip %s",$filename)) == 0) {
		push(@tars,sprintf("%s.tar.gz",$date));
	    } else {
		$report .= sprintf("Unable to gzip file: %s\n",$filename);
	    }
	} else {
	    $report .= sprintf("Unable to create tar ball: %s\n",$filename);
	}
    }

    return @tars;
}

##---------------------------------------------------------------------------
# @signature String find_today_month()
# <p>Find the current year and month.</p>
#
# @output $today Today's year and month.
##---------------------------------------------------------------------------
sub find_today_month {
    my @today = localtime(time());
    return sprintf("%04d%02d",$today[5]+1900,$today[4]+1);
}

##---------------------------------------------------------------------------
# @signature void insert_file(String directory, String file)
# <p>Insert the file into the database and update it's dataset.</p>
#
# @input $database The database connection to be inserted upon.
# @input $directory The directory where the file is located.
# @input $file The file to be inserted.
##---------------------------------------------------------------------------
sub insert_file {
    my ($directory,$file) = @_;

    # Create the file object to be inserted.
    my $mysql = MySqlFile->new();
    $mysql->setDatasetId($DATASET_ID);
    $mysql->setFile($directory,$file);
    $mysql->setFormatId(34);
    $mysql->setBeginDate(substr($file,2,4),substr($file,6,2),substr($file,8,2),0,0,0);
    $mysql->setEndDate(substr($file,2,4),substr($file,6,2),substr($file,8,2),23,59,59);

    # Create the database connection.
    my $database = MySqlDatabase->new("zediupdate","change-456");
    $database->connect();

    # Insert the file.
    my $result = $mysql->insert($database);

    # An error occured so roll back the database.
    if ($result eq "") { $result .= $database->commit(); }
    else {
	$result .= sprintf("Rolling back the database.  %s\n",$database->rollback());
    }

    # Always disconnect from the database.
    $database->disconnect();

    return $result;
}

##---------------------------------------------------------------------------
# @signature String[] load_data_files()
# <p>Get the list of files to be processed.</p>
# 
# @output files[] The list of files to process.
##---------------------------------------------------------------------------
sub load_data_files {
    opendir(my $FTP,$FTP_DIR) or sendMailAndDie("Unable to open $FTP_DIR\n");
    my @files = sort(grep(/\.gif$/,readdir($FTP)));
    closedir($FTP);
    return @files;
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
    MAIL::send_mail("PACS Berbery Script Error",$body, @monitors);
    exit(1);
}

