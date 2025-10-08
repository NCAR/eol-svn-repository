#! /usr/bin/perl -w

##Module-----------------------------------------------------------------------
# <p>The CS_NLDN_lighning script is a daily cron script that does the following
# tasks:</p>
# <ol>
#   <li>Copy the local NLDN to the campaign store (CS).  (This removes the 
#   tar file from the local system if it was copied correctly.)</li>
#   <li>Insert the CS files into the database.</li>
# </ol>
#
#  Revision 2025 February 05
#  Added function to update checksum.
#
#  @author Linda Echo-Hawk
#  @version 4.2 27 Sept 2022
#  Added check to confirm Campaign Store is up and running before trying
#  to call copy_to_CS.
#
#  author Linda Echo-Hawk
#  @version 4.1 23 May 2022
#  Added return code variable ($rcode) to the copy_to_CS routine to solve
#  the zero file size error. Janet suspected that the scp command did not
#  finish completely before doing the return in the old version.  The new
#  version forces the scp command to finish first.
#
#  @author Linda Echo-Hawk
#  @version 3.0 21 Oct 2021
#  Rewrote the script to remove the HPSS references since the HPSS was decommissioned 
#  on 30 Sep 2021. The script will now copy the file to the Campaign Storage and load
#  the file to the database from there.
#
#  @author Linda Echo-Hawk
#  @version 2.0 22 March 2021
#  Revised the MySqlDatabase constructor to pass in the  ~/.my.cnf file
#  containing the dbase and password rather than having these hard-coded
#  in the script. This solved a security issue.
#
# @author Linda Echo-Hawk
# @version 1.0 29 Jan 2019
# Based off of HPSS_USPLN_lightning.pl by Sean Stroble, 2011
#      - This script expects to find NLDN data at
#        /data/ldm/data/lightning/nldn. Daily tar files 
#        are created, copied to the HPSS (aka mass store), 
#        and inserted into the CODIAC database.
#      - This script is run by the crontab found on zephyr.
#      - NOTE that the year is hard-coded and will need to
#        be changed for Jan 1 2020. DEPRECATED: This was 
#        changed to use the year from the first file name.
#
#
##Module-----------------------------------------------------------------------
use strict;
#use lib "/h/eol/dmg/HPSS_cronjobs/lib";
use lib "/net/work/lib/perl/Utilities";
# use lib "/net/work/lib/perl/hpss";
use lib "/net/work/lib/perl/mail";
use lib "/net/work/lib/perl/mysql";
# need DpgDate to convert Julian days
use DpgDate;
use MySqlDatabase;
use MySqlDataset;
# use MySqlMSSFile;
use MySqlFile;
# use HPSS;
use MAIL;

# Constants
my $dataset_id = "100.029";
my $ingest = "/data/ldm/data/lightning/nldn";
my $temp = "/scr/tmp/joss/NLDN/lightning";
# my $hpss = "/FS/EOL/operational/surface/nldn";
# Campaign Storage user, host, archive path
my $cs_name="data-access.ucar.edu";
my $cs_host='eoldata@data-access.ucar.edu';
my $cs_archive="/glade/campaign/eol/archive/operational/surface/nldn";
# my $recipients = "echohawk\@ucar.edu";
my @monitors = ('eol-cds-ingest\@ucar.edu');
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

    ############################
    # Verify that the campaign store is up and running
    ############################
    my $result = system("/bin/ping -c 1 -W 1 $cs_name >/dev/null 2>&1");
    if ($result ne "0") {
	sendMailAndDie("The Campaign Store is not up and running.\n");
    }
    ###########################
  
    # Check that year directory is on Campaign Storage, get year from first file
    my $cs_year = substr($tar_files[0],5,4);
    my $mkdir_out = mkdir_CS($cs_year);

    # Put the tar files on the campaign store and insert them into the database.
    foreach my $file (sort(@tar_files)) {
	
	my $report = "";

    # Copy files to Campaign Storage archive
    my $cs_msg = copy_to_CS($file);
    # Chmod files in Campaign Storage directory
    my $my_cs_chmod = `ssh $cs_host chmod 440 $cs_archive/$cs_year/*.tar 2>&1`;
    $report .= $cs_msg;
    $report .= $my_cs_chmod;

 	if ($cs_msg eq "") {
	    ($cs_msg, my $cs_size) = insert_file($file);
	    # ($msg, my $hpss_size) = insert_file($file);
	    $report .= $cs_msg;

	    if ($cs_msg eq "") {
		my $size = -s "$temp/$file";
		$report .= "CS filesize for $file: $cs_size eq? ".($size/1024)."\n";
		if ($cs_size == (-s "$temp/$file")/1024) { 
		    unlink "$temp/$file"; 
		}
	    }

	}
    }


    # Update the checksum.

    $result = system("ssh $cs_host python3 eoldata-utils/cksum/checksum_utility.py -n -d $cs_archive/$cs_year >/dev/null");
    if ($result != 0) {
         sendMailAndDie(sprintf("Error with checksum command.\n", $result/256));
    }

    # Send out an email that the script has finished.
    if ($report ne "") { sendMailAndDie($report); }

	# can comment out after things are working
    # else { sendMailAndDie("There are no errors to report "); }
}

##-----------------------------------------------------------------------------
# @signature String[] create_tar_files()
# <p>Read the data in the ingest directory and create the tar files that are
# to be placed on the campaign store.</p>
# Example NLDN data file: nldn_2019029192239.bin
# Example USPLN file: 2019012919_report.txt
##-----------------------------------------------------------------------------
sub create_tar_files {
    my ($sec,$min,$hour,$day,$mon,$year) = localtime(time());
    my $today = sprintf("%04d%02d%02d",$year+1900,$mon+1,$day);
# Include the year with the julian date, so files for the last day in the year will be archived as well.    
    my $julian_today = sprintf("%02d%03d", $year + 1900, convertJulian($year+1900,$day,$mon+1));

    my @files = ();
    # Get the list of files that will be used to generate the tar files.
    opendir(my $INGEST,$ingest) or sendMailAndDie("Cannot open $ingest\n");

    foreach my $file (sort(readdir($INGEST))) {
	# need to determine julian "today" 
	push(@files,$file) if ($file =~ /^nldn_\d{13}.bin$/ && substr($file,5,7) < $julian_today);
    }
    closedir($INGEST);
    # get year from first filename
    my $fyear = substr($files[0],5,4);
     
    # Confirm that there are new files to archive. If not, warn user
    if(@files==0){
	sendMailAndDie("No files exist in the ingest location for date < $today (jd = $julian_today).\n");}

    # Split the files into lists by their date
    my %tar_hash;
    foreach my $file (sort(@files)) {
	# the hash key for the file will be the julian day, e.g., 037
	push(@{ $tar_hash{substr($file,9,3)}},$file);
    }

    # Create the tar files for each date
    my @tar_files = ();
    chdir($ingest) or sendMailAndDie("Cannot change to $ingest\n");

    # $date is the 3-digit julian day
    foreach my $date (sort(keys(%tar_hash))) {

	
    # rename the tar file to include month and day (nldn_yyyymmdd_JJJ.tar)
	# # @signature (int, int) convertJulian(int year, int julian)
	# use the year from the first file name
	my ($file_month, $file_day) = convertJulian($fyear, $date);
	$file_month = sprintf("%02d", $file_month);
	$file_day = sprintf("%02d", $file_day);
	my $new_file_name = sprintf ("nldn_%s%s%s_%s.tar", $fyear,$file_month,
	    $file_day,$date);

	my $report;

	if (system(sprintf('/bin/tar -cf %s/%s %s',$temp,$new_file_name,
			   join(" ",@{ $tar_hash{$date}})))) {
	    $report .= "$date.tar was not able to be created.\n";
	} else {
	    foreach my $file (@{ $tar_hash{$date}}) {
		$report .= "$file was not able to be removed.\n" if (!unlink($file));
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
# @output mysql size
# return ($msg, $mysql->getSize());
#
#
# Example NLDN data file: nldn_20190118_018.tar (nldn_yyyymmdd_JJJ.tar)
# Example USPLN file: 2019012919_report.txt
##-----------------------------------------------------------------------------
sub insert_file {
    my ($file) = @_;
    
    # Create the file information.
    my $mysql = MySqlFile->new();
    $mysql->setDatasetArchiveIdent($dataset_id);

    my $cs_year = substr($file,5,4);
    my $cs_output = `ssh $cs_host  ls -l $cs_archive/$cs_year/$file 2>&1`;
    my @inputs = (split(' ',$cs_output));
    my $cs_size = $inputs[4];
    my $cs_size_in_kb = $cs_size/1024;
    # print "CS output: $cs_output\n";
    # print "size of file is $cs_size\n";

    $mysql->setFile(sprintf("%s/%04d",$cs_archive,$cs_year),$file,$cs_size_in_kb);
    $mysql->setFormatId(53);
    $mysql->setHost("campaign");
    $mysql->setBeginDate(substr($file,5,4),substr($file,9,2),substr($file,11,2),0,0,0);
    $mysql->setEndDate(substr($file,5,4),substr($file,9,2),substr($file,11,2),23,59,59);

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

    return ($msg, $mysql->getSize());
}

##-----------------------------------------------------------------------------
# @signature String place_on_mss(String file)
# <p>Copy the specified file to the mass store.</p>
#
# @input $file The file to be copied to the mass store, e.g., nldn_20190204_035.tar
# @output $msg Any error messages that occured during the copy or the empty
# String if it was copied successfully.
##-----------------------------------------------------------------------------
sub place_on_mss {
    my ($file) = @_;
    my $year = substr($file,5,4);
    
    # return HPSS::put(\"$temp/$file",\"$hpss/$year/$file");
    # HPSS decommissioned 9/30/2021
}

################################################
# If YYYY directory does not exist in Campaign #
# Storage archive location, create it  NLDN    #
################################################
# See USPLN notes - This gets run every time and
# tries to create directories that already exist
## ##############################################
sub mkdir_CS {
    my $year = shift;
    ## check if year directory exists, if not create it
    my $exists = 0;
    my @date_dirs = `ssh $cs_host ls $cs_archive 2>&1`;
    my $mkdir;
    chomp @date_dirs;
    foreach my $dir (sort(@date_dirs)) {
        if ($dir eq $year) {
            $exists = 1;
        }
    }
    if (!$exists) {
       print "Creating YYYY $year directory on Campaign Storage\n";
       $mkdir = `ssh $cs_host  mkdir $cs_archive/$year 2>&1`;
    }
    return $mkdir;
}

##################################################
# Copy file to Campaign Storage archive location #
##################################################
sub copy_to_CS {
    my ($cs_file) = @_;
    my $year = substr($cs_file,5,4);

    # return `scp $temp/$cs_file $cs_host:$cs_archive/$year/$cs_file 2>&1`;
    my $rcode = `scp $temp/$cs_file $cs_host:$cs_archive/$year/$cs_file`;
    sleep(120);
    return($rcode);
}

sub sendMailAndDie {
    my ($body) = @_;
    MAIL::send_mail("LDM NLDN Lightning HPSS/CODIAC Error",$0."\n\n".$body, @monitors);
    exit(1);
}
