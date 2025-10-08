#! /usr/bin/perl -w

##Module-----------------------------------------------------------------------
# <p>The MadisMapTarMysql script is a daily cron script that does the following
# tasks:</p>
# <ol>
#   <li>Create daily tar files for all previous days to the day the script
# is run.  (This removes all of the files used to create the tar file.)</li>
#   <li>Copy the tar file to the archive directory.</li>
#   <li>Insert the archived tar file into the database.</li>
# </ol>
# <p>This script is a modified version of the madis_mysql_stage_to_mss.pl script written 
# by Joel Clawson.</p>
#
#
#  @author Linda Echo-Hawk
#  @version 2.0 22 March 2021
#  Revised the MySqlDatabase constructor to pass in the  ~/.my.cnf file
#  containing the dbase and password rather than having these hard-coded
#  in the script. This solved a security issue.
#
# revised 2008 Mar 20:
# <p>Updated to check for existence of tar file and not overwrite it if it is  already there.
# Just warn user.</p>
#
# revised 2008 Mar 07:
# <p>Ingest location moved from /ingest to /scr.  Updated script to reflect this.</p>
#
# created by Pierce Martin Mar 2008:
#
##Module-----------------------------------------------------------------------
use strict;
use lib "/net/work/lib/perl/mysql";
use Email::MIME;
use Email::MIME::Creator;
use MySqlDatabase;
use MySqlDataset;
use MySqlFile;

# Constants
my $dataset_id = "100.014";
# Directory where ingest data is stored.
#my $ingest = "/ingest/ldm/data/madis/map";
my $ingest = "/data/ldm/data/madis/map";
# Final directory for tar files.
my $archive = "/net/archive/data/operational/profiler";

#EXAMPLE: my $recipients = "USER1\@eol.ucar.edu,USER2\@eol.ucar.edu";
my $recipients = "eol-cds-ingest\@ucar.edu";

my $report = "";

&main();

##-----------------------------------------------------------------------------
# @signature void main()
# <p>Run the script.</p>
##-----------------------------------------------------------------------------
sub main {

    my @tyears = ();

    # Create the new tar files.
    create_tar_files();
    
    # Read in files that need to be loaded from the log file
    open(LOGFILE, "$archive/log.txt"); 
    my(@tar_files) = <LOGFILE>;
    foreach my $file (sort(@tar_files)) {
	# remove new line character
	chomp($file);

        # Grab file year (in case there are multiple, load in an non-duplicative array)
        my $fyear = substr($file,0,4);
        unless (@tyears) {
           push(@tyears,$fyear);
        }
	if (@tyears > 1 && $fyear != $tyears[$#tyears]) {
           push(@tyears,$fyear);
        }

	# insert the tars into the database
        if($file =~ /.tar$/){
	    $report .= insert_file($file);
	}
    }
    close(LOGFILE);


    # Run checksum utility for relevant year subdirs
    foreach my $subdir (@tyears) {

        my $checksum_res = system( "python3 ~eoldata/eoldata-utils/cksum/checksum_utility.py -n -d $archive/$subdir >/dev/null");
        if ($checksum_res != 0) {
          send_mail("Error with checksum command.\n", $checksum_res/256);
        }
    }

    # Send out an email that the script has finished.
    if ($report ne "") { send_mail("LDM MADIS MAP CODIAC Script Error\n",$report); }
    else { 	
		# clear log file if succesful
		open(LOGFILE, ">$archive/log.txt"); 
		close(LOGFILE);

		### send_mail("LDM MADIS MAP CODIAC Notification\n","There are no errors to report"); 
	}


}

##-----------------------------------------------------------------------------
# @signature String[] create_tar_files()
# <p>Read the data in the ingest directory and create the tar balls that are
# to be placed on the mass store.</p>
##-----------------------------------------------------------------------------
sub create_tar_files {
    #my ($sec,$min,$hour,$day,$mon,$year) = localtime(time());
    #my $today = sprintf("%04d%02d%02d",$year+1900,$mon+1,$day);
    my $delay_days = 3;	# delay archival of data for 3 days.
    my ($sec,$min,$hour,$day,$mon,$year) = 
    	localtime(time()-((3600*24)*$delay_days));
    my $procdate = sprintf("%04d%02d%02d", $year+1900,$mon+1,$day);
    my @files = ();

    # Get the list of files that will be used to generate the tar files.
    opendir(my $INGEST,$ingest) or sendMailAndDie("Cannot open $ingest\n");
    foreach my $file (sort(readdir($INGEST))) {
	push(@files,$file) if ($file =~ /^\d+\_\d+\.gz$/ &&
	    substr($file,0,8) < $procdate);
    }
    
    closedir($INGEST);
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
	my $tar_year = substr($date,0,4);
	# Each Jan 1 need to create a new year subdir
	if (!(-e $archive."/".$tar_year)) {
	    mkdir($archive."/".$tar_year,0755) || die("Cannot create $archive/$tar_year\n");
	}
	# Only create the tar file if it doesn't already exist, else warn user.
	if (-e $archive."/".$tar_year."/".$date.".tar") {
	    $report .= "${archive}/${tar_year}/${date}.tar already exists.  New tar file not created from data in ${ingest}.\n";
	    # For some reason, we regularly get spoardic ancient files on this
	    # feed. These are generally one to two files on a given day. Assume
	    # the files we got in real-time are correct, and just delete these if
	    # they are more than a month old.
	    my $old_data_buffer = 30; # 30 days
            my ($sec,$min,$hour,$day,$mon,$year) = 
    	       localtime(time()-((3600*24)*$old_data_buffer));
            my $buffer_date = sprintf("%04d%02d%02d", $year+1900,$mon+1,$day);
	    if ($date < $buffer_date) {
	        # remove original file
	        foreach my $file (@{ $tar_hash{$date}}) {
	            $report .= "$file collection date older than 30 days - delete and don't rearchive\n";
	            $report .= "$file was not able to be removed.\n" if (!unlink($file));
	        }
	    }
        } else {
	    if (system(sprintf("/bin/tar -cf %s/%s/%s.tar %s",$archive,$tar_year,$date,
			   join(" ",@{ $tar_hash{$date}})))) {
	        $report .= "$date.tar was not able to be created.\n";
	    sendMailAndDie(sprintf("/bin/tar -cf %s/%s/%s.tar %s",$archive,$tar_year,$date,
			   join(" ",@{ $tar_hash{$date}})));
	    } else {
	        # remove original file
	        foreach my $file (@{ $tar_hash{$date}}) {
		    $report .= "$file was not able to be removed.\n" if (!unlink($file));
	        }
	        push(@tar_files,sprintf("%s.tar",$date));
	    }
	}
    } 
    #create a log file to tell which new tars need to be uploaded
    my $loc = $archive."/log.txt";
    open (OUTFILE, ">$loc");
    print OUTFILE "Log File used to show files needing to be uploaded.\n";
    foreach my $tar (sort(@tar_files)){
        print OUTFILE "$tar\n";
    }
    close (OUTFILE);
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
    $mysql->setFile(sprintf("%s/%04d",$archive,substr($file,0,4)),$file);
    # .tar format
    $mysql->setFormatId(53);
    $mysql->setBeginDate(substr($file,0,4),substr($file,4,2),substr($file,6,2),0,0,0);
    $mysql->setEndDate(substr($file,0,4),substr($file,4,2),substr($file,6,2),23,59,59);

    # Create and open the database
    my $database = MySqlDatabase->new(); # use ~/.my.cnf
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
#sub place_on_mss {
#    my ($file) = @_;
#    my $year = substr($file,0,4);
#    
#    my $result = `/net/local_lnx/dcs/bin/msls $mss`;
#    if ($result =~ /$year/) {
#        # Get the listing of the directory
#        my $path = sprintf("%s/%04d",$mss,$year);
#        $result = `/net/local_lnx/dcs/bin/msls $path`;
#    } else {
#	$result = "";
#    }
#	
#    # Only put the file on the MSS if the file does not exist.
#    if ($result =~ /$file/) { 
#	return sprintf("%s already exists on the mass store.\n",$file);
#    } elsif (system(sprintf("/net/local_lnx/dcs/bin/msrcp -wpwd jossdata -pe 32767 %s/%s mss:%s/%04d/%s",$archive,$file,$mss,substr($file,0,4),$file))) {
#	return sprintf("%s may not have been copied to the mass store.\n",$file);
#    }
#    return "";
#}

##--------------------r---------------------------------------------------------
# @signature void send_mail(String subject, String body)
# <p>Send an email to the group of recipients with the specified subject and
# text.</p>
#
# @input $subject The subject of the email.
# @input $body The message of the email.
##-----------------------------------------------------------------------------
sub send_mail {
    my ($subject,$body) = @_;
    my $sender = "eol-cds-ingest\@ucar.edu";
    my $reply_to = "eol-cds-ingest\@ucar.edu";

    $body = $0."\n\n".$body;
    
    my @parts = (Email::MIME->create(attributes => { content_type => "text/plain" },
				     body => $body));

    my $email = Email::MIME->create(parts => [ @parts ]);

    $email->header_set("From" => $sender);
    $email->header_set("Reply-To" => $reply_to);
    $email->header_set("To" => $recipients);
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
    send_mail("LDM MADIS MAP CODIAC Error",$0."\n\n".$body);
    exit(1);
}




