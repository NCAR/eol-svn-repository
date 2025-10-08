#! /usr/bin/perl -w

##Module-----------------------------------------------------------------------
# <p>The HIGHWAY 3D-PAWS_tar_to_archive.pl script is a daily cron script 
# that does the following tasks:</p>
# <ol>
#   <li>Tars files from their /net/ingest location
#   <li>Moves the tar file to the /net/archive location
#   <li>Inserts files from their /net/archive location to the CODIAC database.
# </ol>
#
#  @author Linda Echo-Hawk
#  @version 1.1 22 March 2021
#  Revised the MySqlDatabase constructor to pass in the  ~/.my.cnf file
#  containing the dbase and password rather than having these hard-coded
#  in the script. This solved a security issue.
#
# @author Linda Echo-Hawk
# @version 1.0 20 March 2019
# Based off of HPSS_USPLN_lightning script
#
##Module-----------------------------------------------------------------------
use strict;
use lib "/net/work/lib/perl/mysql";
use lib "/net/work/lib/perl/hpss";
use lib "/net/work/lib/perl/mail";
use MySqlDatabase;
use MySqlDataset;
use MySqlFile;
use HPSS;
use MAIL;
use FindBin qw($Script $Bin); # What/where script running.

# Constants
my $dataset_id = "572.005";
my $archive = "/net/archive/data/highway/surface/3D-PAWS/";

#EXAMPLE: my $recipients = "USER1\@eol.ucar.edu,USER2\@eol.ucar.edu";
my $recipients = "eol-cds-ingest\@ucar.edu";
# my $recipients = "echohawk\@ucar.edu";

my $report = "";
my $report_header = "";
my @tar_files;


&main();

##-----------------------------------------------------------------------------
# @signature void main()
# <p>Run the script.</p>
##-----------------------------------------------------------------------------
sub main {

    # Clearly identify which script is running 
	$report_header .= "I am $Bin/$Script running on ".
        `/bin/hostname`."\n";
    
	create_tar_files();
                                               
    # Insert the tar files into the database.
    foreach my $file (sort(@tar_files)) {

		my $msg = "";
		$report .= $msg;

		# insert the file into the database
		if ($msg eq "") {
			# insert_file returns ($msg, $mysql->getSize());
			($msg, my $hpss_size) = insert_file($file);
			# $msg .= "File $file with size $hpss_size inserted into DB $dataset_id\n";
			$report .= $msg;
		}
		else {
			$report .= $msg;
		}
	}

    # Send out an email that the script has finished.
    if ($report ne "") { 
		sendMailAndDie($report_header.$report); 
	}
    # else { sendMailAndDie("$report_header.$report. \nThere are no errors to report"); }
}

##-----------------------------------------------------------------------------
# @signature String[] create_tar_files()
# <p>Read the data in the ingest directory and create the tar balls that are
# to be placed in the archive location.</p>
##-----------------------------------------------------------------------------
sub create_tar_files {

    # from SCOT'S SCRIPT
    my ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = gmtime(time-(24*60*60));
    my ($osec,$omin,$ohour,$omday,$omon,$oyear,$owday,$oyday,$oisdst) = gmtime(time-(3*24*60*60));

    $mon++; # zero-based
    $omon++; # zero-based
    $year = $year + 1900;
    $oyear = $oyear + 1900;


    # yesterday for 572.001-003
    my $date=sprintf("%04d%02d%02d",$year,$mon,$mday);
    # three days previous for 572.004-005
    my $odate=sprintf("%04d%02d%02d",$oyear,$omon,$omday);

    my $work_dir = "/net/work/Projects/HIGHWAY/data_ingest/surface/";

    chdir($work_dir) || die "Couldn't reach $work_dir: $!";

    #3DPAWS data directories for 572.005
    my $paws_dir = "3D-PAWS/data/$odate/";
    
	#3DPAWS archive directory
    my $paws_arch_dir = "/net/archive/data/highway/surface/3D-PAWS";

    # -----------------------------------------------
    # Create and move 3DPAWS file to archive location
    # -----------------------------------------------
    # print "tar'ing 3DPAWS directories\n";
    system("/bin/tar cf HIGHWAY_3D-PAWS_$odate.tar $paws_dir");
    # system("/bin/tar cvf HIGHWAY_3D-PAWS_$odate.TEST.tar $paws_dir");
    system("/bin/mv HIGHWAY_3D-PAWS*tar $paws_arch_dir");

    my $file = "HIGHWAY_3D-PAWS_$odate.tar";
    # my $file = "HIGHWAY_3D-PAWS_$odate.TEST.tar";
    push(@tar_files, $file);
    # print "FILES FOUND: @tar_files\n";

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
	# HIGHWAY_3D-PAWS_20190408.tar
    my $mysql = MySqlFile->new();
    $mysql->setDatasetArchiveIdent($dataset_id);
    # $mysql->setFile(sprintf("%s/%04d",$archive,substr($file,0,4)),$file);
    $mysql->setFile($archive,$file);
	# set the format ID for TAR files (Unix Tape ARchive (TAR) Format File)
    $mysql->setFormatId(53);
    $mysql->setBeginDate(substr($file,16,4),substr($file,20,2),substr($file,22,2),0,0,0);
    $mysql->setEndDate(substr($file,16,4),substr($file,20,2),substr($file,22,2),23,59,59);

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
# @signature void send_mail(String subject, String body)
# <p>Send an email to the group of recipients with the specified subject and
# text.</p>
#
# @input $subject The subject of the email.
# @input $body The message of the email.
##-----------------------------------------------------------------------------
sub send_mail {
    my ($subject,$body) = @_;
    # my $sender = "eol-cds-ingest\@ucar.edu";
    # my $reply_to = "eol-cds-ingest\@ucar.edu";
    my $sender = "echohawk\@ucar.edu";
    my $reply_to = "echohawk\@ucar.edu";

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
    send_mail("HIGHWAY 3D-PAWS Error Report",$0."\n\n".$body);
    exit(1);
}

