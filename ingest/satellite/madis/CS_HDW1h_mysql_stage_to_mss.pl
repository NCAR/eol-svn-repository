#! /usr/bin/perl -w

#
##  THIS SCRIPT IS NOT RUNNING IN PRODUCTION AND HASN'T BEEN TESTED!!!
##
#
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
#  @version 1.2 27 Sept 2022
#  Added check to confirm Campaign Store is up and running before trying
#  to call copy_to_CS.
#
#  @author Linda Echo-Hawk
#  @version 1.1 22 March 2021
#  Revised the MySqlDatabase constructor to pass in the  ~/.my.cnf file
#  containing the dbase and password rather than having these hard-coded
#  in the script. This solved a security issue.
#
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
# revisited 2005 Mar 14:
# <p>As of today, this data has not been used in any project.  It is
# strictly operational.  Jose believes the VAMOS community might be interested
# in this project, so will will keep it for now.  "You know that as soon as we
# cancel and delete someone will want it!" - SFW
# </p>
# revised 2008 Mar 07:
# <p>Ingest location moved from /ingest to /scr.  Updated script to reflect this.</p>
# revised 2010 Dec 01:
# <p>Temp location moved from /ingest to /net/ingest.  Updated script to reflect this.</p>
#
##Module-----------------------------------------------------------------------
package HDW1hStageToMSS;
use strict;
use lib "/net/work/lib/perl/mysql";
use lib "/net/work/lib/perl/mail";
use MySqlDatabase;
use MySqlDataset;
use MySqlFile;
use MAIL;

# Constants
my $dataset_id = "100.008";
#my $ingest = "/ingest/ldm/data/madis/point/HDW1h/netcdf";
#my $ingest = "/scr/satellite/ldm/data/madis/point/HDW1h/netcdf";
#my $temp = "/net/ingest/tmp/madis/HDW1h";
#my $mss = "/JOSS/DATA/RAW/SATELLITE/HDW1h";
my $ingest = "/export/ldm/data/madis/point/HDW1h/netcdf";
my $temp = "/scr/tmp/joss/madis/HDW1h";
my $cs_name = "data-access.ucar.edu";
my $cs_archive = "/glade/campaign/eol/archive/operational/satellite/hdw1h";
my $cs_host = 'eoldata@data-access.ucar.edu';
# my @monitors = ("eol-cds-ingest\@ucar.edu"); ## from HDW script
my $receipients = "eol-cds-ingest\@ucar.edu";
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
    
    # ##############################
    # Verify that the campaign store is up and running
    # ##############################
    my $result = system("/bin/ping -c 1 -W 1 $cs_name >/dev/null 2>&1");
    if ($result ne "0") {
	sendMailAndDie("The Campaign Store is not up and running.\n");
    }

    # ##############################
    # # check if YYYY dir exists on Campaign Storage, if not, create it
    # get year from first file
    my $cs_year = substr($tar_files[0],0,4); 
    my $mkdir_out = mkdir_CS($cs_year);
    $report .= $mkdir_out;





    # Put the tar files on the campaign store and insert them into the database.
    foreach my $file (sort(@tar_files)) {

	#my $msg = place_on_mss($file);
	my $msg .= copy_to_CS($file);

	if ($msg eq "") {
	    $report .= insert_file($file);
	    unlink(sprintf("%s/%s",$temp,$file));
	} else {
	    $report .= $msg;
	}
    }


    # Chmod files in Campaign Storage directory
    my $my_cs_chmod = `ssh $cs_host chmod 440 $cs_archive/$cs_year/* 2>&1`;
    $report .= $my_cs_chmod;


    # Send out an email that the script has finished.
    if ($report ne "") { sendMailAndDie("LDM MADIS HDW1h Satellite Winds MSS/CODIAC Script Error\n",$report); }
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
    my ($file) = @_;

    # Create the file information.
    my $mysql = MySqlFile->new();
    $mysql->setDatasetArchiveIdent($dataset_id);

    my $cs_year = substr($file,0,4);
    my $cs_output = `ssh $cs_host  ls -l $cs_archive/$cs_year/$file 2>&1`;
    my @inputs = (split(' ',$cs_output));
    my $cs_size = $inputs[4];
    my $cs_size_in_kb = $cs_size/1024;
    $mysql->setFile(sprintf("%s/%04d",$cs_archive,$cs_year),$file,$cs_size_in_kb);
    #$mysql->setFile(sprintf("%s/%04d",$mss,substr($file,0,4)),$file);
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
    # Only worry about the dataset changes if the file insert was successful
    if ($msg eq "") {

	# Create and load the dataset.
	# ##################
	# NOTE This is very different than the HDW script
	# are you sure you want to create a new dataset again?
	# FROM HERE ON DOWN NEEDS TO BE CHECKED
	# #################################
	#
	# TODO
	#
	my $dataset = MySqlDataset->new($dataset_id);
	$msg = $dataset->selectDataset($database);

	# Only continue if the dataset was retreived successfully.
	if ($msg eq "") {

	    # Load and format dates
	    my $file_begin = $mysql->getBeginDate(); $file_begin =~ s/[\s:-]//g;
	    my $file_end = $mysql->getEndDate(); $file_end =~ s/[\s:-]//g;
	    my $begin = $dataset->getBeginDate(); $begin =~ s/[\s:-]//g;
	    my $end = $dataset->getEndDate(); $end =~ s/[\s:-]//g;
	    
	    # Compare the dates and change as necessary.
	    if ($file_begin < $begin) { 
		$dataset->setBeginDate(split(/[\s:-]/,$mysql->getBeginDate())); }
	    if ($file_end > $end) { 
		$dataset->setEndDate(split(/[\s:-]/,$mysql->getEndDate())); }
	    
	    # Update the dataset with the new dates.
	    $msg = $dataset->updateDataset($database);
	}
    }

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
    
    my $result = `/net/local_lnx/dcs/bin/msls $mss`;
    if ($result =~ /$year/) {
        # Get the listing of the directory
        my $path = sprintf("%s/%04d",$mss,substr($file,0,4));
        $result = `/net/local_lnx/dcs/bin/msls $path`;
    } else {
	$result = "";
    }
	
    # Only put the file on the MSS if the file does not exist.
    if ($result =~ /$file/) { 
	return sprintf("%s already exists on the mass store.\n",$file);
    } elsif (system(sprintf("/net/local_lnx/dcs/bin/msrcp -wpwd jossdata -pe 32767 %s/%s mss:%s/%04d/%s",$temp,$file,$mss,substr($file,0,4),$file))) {
	return sprintf("%s may not have been copied to the mass store.\n",$file);
    }
    return "";
}

##-----------------------------------------------------------------------------
# @signature void send_mail(String subject, String body)
# <p>Send an email to the group of receipients with the specified subject and
# text.</p>
#
# @input $subject The subject of the email.
# @input $body The message of the email.
##-----------------------------------------------------------------------------
sub send_mail {
    my ($subject,$body) = @_;
    my $sender = "eol-cds-ingest\@ucar.edu";
    my $reply_to = "eol-cds-ingest\@ucar.edu";
    
    my @parts = (Email::MIME->create(attributes => { content_type => "text/plain" },
				     body => $body));

    my $email = Email::MIME->create(parts => [ @parts ]);

    $email->header_set("From" => $sender);
    $email->header_set("Reply-To" => $reply_to);
    $email->header_set("To" => $receipients);
    $email->header_set("Subject" => $subject);

    open(my $SENDMAIL,"|/usr/lib/sendmail -t") || die("Unable to open sendmail\n");
    printf($SENDMAIL $email->as_string());
    close($SENDMAIL);
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
    send_mail("LDM MADIS HDW1h Satellite Winds MSS/CODIAC Error",$body);
    exit(1);
}




