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
# revised 2008 Mar 07:
# <p>Ingest location moved from /ingest to /scr.  Updated script to reflect this.</p>
#
# revised 2010 Dec 01:
# <p>Temp location moved from /ingest to /net/ingest.  Updated script to reflect this.</p>
##Module-----------------------------------------------------------------------
package MadisStageToMSS;
use strict;
use lib "/net/work/lib/perl/mysql";
use Email::MIME;
use Email::MIME::Creator;
use MySqlDatabase;
use MySqlDataset;
use MySqlMSSFile;

# Constants
my $dataset_id = "100.001";
#my $ingest = "/ingest/ldm/data/madis/mesonet1";
my $ingest = "/scr/satellite/ldm/data/madis/mesonet1";
my $temp = "/net/ingest/tmp/madis";
my $mss = "/JOSS/DATA/RAW/SURFACE/MADIS/MESONET1";
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

    # Put the tar files on the mass store and insert them into the database.
    foreach my $file (sort(@tar_files)) {

	my $msg = place_on_mss($file);

	if ($msg eq "") {
	    $report .= insert_file($file);
	    ###unlink(sprintf("%s/%s",$temp,$file));
	} else {
	    $report .= $msg;
	}
    }

    # Send out an email that the script has finished.
    if ($report ne "") { send_mail("LDM MADIS MSS/CODIAC Script Error\n",$report); }
    #else { send_mail("LDM MADIS MSS/CODIAC Notification\n","There are no errors to report"); }
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
	push(@files,$file) if ($file =~ /^\d+\_\d+\.gz$/ && $file !~ /^$today/);
    }
    closedir($INGEST);

    # Make sure there are files to archive
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
		###$report .= "$file was not able to be removed.\n" if (!unlink($file));
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
    my $mysql = MySqlMSSFile->new();
    $mysql->setDatasetArchiveIdent($dataset_id);
    $mysql->setFile(sprintf("%s/%04d",$mss,substr($file,0,4)),$file);
    $mysql->setFormatId(53);
    $mysql->setBeginDate(substr($file,0,4),substr($file,4,2),substr($file,6,2),0,0,0);
    $mysql->setEndDate(substr($file,0,4),substr($file,4,2),substr($file,6,2),23,59,59);

    # Create and open the database
    #my $database = MySqlDatabase->new("joss","ofps-345");
    my $database = MySqlDatabase->new("ingest","gob-ble");
    $database->connect();

    # Insert the file
    my $msg = $mysql->insert($database);

    # Only worry about the dataset changes if the file insert was successful
    if ($msg eq "") {

	# Create and load the dataset.
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

    die "MSS is dead. $0 must be fixed.";
    
    my $result = `/net/local_lnx/dcs/bin/msls $mss`;
    if ($result =~ /$year/) {
        # Get the listing of the directory
        my $path = sprintf("%s/%04d",$mss,$year);
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
    send_mail("LDM MADIS MSS/CODIAC Error",$body);
    exit(1);
}




