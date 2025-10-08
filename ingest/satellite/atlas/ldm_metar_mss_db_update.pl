#! /usr/bin/perl -w

##Module-----------------------------------------------------------------------
# <p>The ldm_metar_mss_db_update.pl script is used for creating monthly tar
# files of the LDM Surface METAR data, putting them on the mass store once 
# they have been compressed and inserting them into the database.</p>
#
# @author Joel Clawson 7/19/2006
# @version 1.2 Updated the script to use the ingest user on the zedi database 
# on tsunami.eol instead of the joss user on the jedi database on hurricane.joss.
# The email section was updated to send the mail using the joss@eol user with
# the Reply-To field being me (Joel).
#
# @author Joel Clawson 5/15/2006
# @version 1.1 Migrated the script from hurricane.joss to tsunami.eol.
#
# @author Joel Clawson 6-21-2005
# @version 1.0 The original creation of the script.
##Module-----------------------------------------------------------------------
use strict;
use lib "/net/work/lib/perl/mysql";
use Email::MIME;
use Email::MIME::Creator;
use MySqlDatabase;
use MySqlDataset;
use MySqlMSSFile;

# Constants used by the script.
my $DATASET_ID = "100.013";
#my $INGEST = "/ingest/ldm/data/atlas";
my $INGEST = "/scr/satellite/ldm/data/atlas";
my $MSS_HOME = "/JOSS/DATA/RAW/BY_PROJECT/ATLAS/METAR";

#EXAMPLE: my $recipients = "USER1\@eol.ucar.edu,USER2\@eol.ucar.edu";
my $recipients = "eol-cds-ingest\@ucar.edu";

my $report = "";

&main();

##-----------------------------------------------------------------------------
# @signature void main()
# <p>Run the script.</p>
##-----------------------------------------------------------------------------
sub main {
    my $proc_date = determine_processing_date();

    my @mss_files = create_month_files($proc_date);
    my @db_files = copy_to_mss(@mss_files);
    update_database(@db_files);

    if ($report ne "") { send_mail("LDM Surface METAR Script Error",$report); }
    #else { send_mail("LDM Surface METAR Script Notification","There are no errors to report.\n"); }
}

##-----------------------------------------------------------------------------
# @signature MySqlMSSFile[] db_files copy_to_mss(String[] mss_files)
# <p>Copy the specified files to the mass store and created the database
# entries for the files.</p>
#
# @input mss_files The files to be copied to the mass store.
# @output db_files The entries for the files in the database.
##-----------------------------------------------------------------------------
sub copy_to_mss {
    my @mss_files = @_;

    my @db_files = ();
    foreach my $file (sort(@mss_files)) {
	my @mss = grep(/$file/,`/net/local_lnx/dcs/bin/msls -l $MSS_HOME`);

	if (@mss) {
	    $report .= "Mass Store already contains $MSS_HOME/$file\n";
	    next;
	}

	if (system("/net/local_lnx/dcs/bin/msrcp -wpwd jossdata -pe 32767 $file mss:$MSS_HOME/$file") == 0) {
	    my $mysql = MySqlMSSFile->new();
	    $mysql->setDatasetArchiveIdent($DATASET_ID);
	    $mysql->setFile($MSS_HOME,$file);
	    $mysql->setFormatId(53);
	    $mysql->setBeginDate(substr($file,12,4),substr($file,16,2),1,0,0,0);
	    $mysql->setEndDate(substr($file,12,4),substr($file,16,2),
			       days_in_month(substr($file,12,4),substr($file,16,2)),
			       23,59,59);

	    push(@db_files,$mysql);

	    ###unlink($file);
	} else {
	    $report .= "Could not copy $file to the mass store.\n";
	}
    }

    return @db_files;
}

##-----------------------------------------------------------------------------
# @signature String[] files create_month_files(int date)
# <p>Create the bzipped tar files for the months up to and including the date.</p>
#
# @input $date The last date to be archived in YYYYMM format.
# @output mss_files The list of files to be put on the mass store.
##-----------------------------------------------------------------------------
sub create_month_files {
    my ($date) = @_;

    opendir(my $RAW,$INGEST) or sendMailAndDie("Cannot open $INGEST to be read.\n");
    my @files = grep(/^[^\.]+$/,readdir($RAW));
    if(@files==0){sendMailAndDie("No files exist in the ingest location\n");}
    closedir($RAW);
    
    chdir($INGEST) or sendMailAndDie("Cannot change to $INGEST\n");

    my @mss_files = ();
    foreach my $file (sort(@files)) {
	if ($file <= $date) {
	    if (system("/bin/tar -cf atlas.metar.$file.tar $file") == 0) {
		if (system("/usr/bin/bzip2 atlas.metar.$file.tar") == 0) {
		    push(@mss_files,"atlas.metar.$file.tar.bz2");
		} else {
		    $report .= "Could not bzip2 atlas.metar.$file.tar\n";
		}

		###if (system("/bin/rm -rf $file") != 0) {
		###    $report .= "Could not remove $file\n";
		###}
	    } else {
		$report .= "Could not create the tar file: atlas.metar.$file.tar\n";
	    }
	}
    }
    return @mss_files;
}

##-----------------------------------------------------------------------------
# @signature int days_in_month(int year, int month)
# <p>Determine the number of days in a month for a year.</p>
#
# @input $year The year of the month.
# @input $month The month to determine the number of days for.
# @output $days The number of days in the month
##-----------------------------------------------------------------------------
sub days_in_month {
    my ($year,$month) = @_;
    
    return (31,(($year % 4 == 0) && ($year % 100 != 0)) || ($year % 400 == 0) ?
	    29 : 28,31,30,31,30,31,31,30,31,30,31)[$month - 1];
}

##----------------------------------------------------------------------------------
# @signature int determine_processing_date()
# <p>Get the year and month that is the latest that should be processed by
# the script.</p>
#
# @output $date The year and month that is the latest to be processed in YYYYMM
# format.
##----------------------------------------------------------------------------------
sub determine_processing_date {
    my @today = localtime();
    my $month = $today[4] + 1;
    my $year = $today[5] + 1900;

    ($year,$month) = get_previous_month(get_previous_month($year,$month));

    return sprintf("%04d%02d",$year,$month);
}

##----------------------------------------------------------------------------------
# @signature (int year, int month) get_previous_month(int year, int month)
# <p>Get the previous month from the specified month and year.</p>
#
# @input $year The year to use to find the previous month.
# @input $month The month to use to find the previous month.
# @output $year The year for the previous month.
# @output $month The previous month.
##----------------------------------------------------------------------------------
sub get_previous_month {
    my ($cur_year,$cur_month) = @_;

    if ($cur_month == 1) { return ($cur_year - 1,12); }
    else { return ($cur_year,$cur_month - 1); }
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
    my $sender = "eol-cds-ingest\@ucar.edu";
    my $reply_to = "eol-cds-ingest\@ucar.edu";
    
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
# @signature void update_database(MySqlMSSFile[] files)
# <p>Insert the files into the database.</p>
#
# @input files The list of MySqlMSSFiles to be inserted into the database.
##-----------------------------------------------------------------------------
sub update_database {
    my @files = @_;

    # Establish the connection to the database.
    #my $database = MySqlDatabase->new("joss","ofps-345");
    my $database = MySqlDatabase->new("ingest","gob-ble");
    $database->connect();

    foreach my $mysql (@files) {
	my $msg = $mysql->insert($database);

	# Update the dataset description on a successful file insert.
	if ($msg eq "") {
	    my $dataset = MySqlDataset->new($DATASET_ID);
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
		    $dataset->setBeginDate(split(/[\s:-]/,$mysql->getBeginDate()));
		}
		if ($file_end > $end) {
		    $dataset->setEndDate(split(/[\s:-]/,$mysql->getEndDate()));
		}
		
		# Update the dataset with the new dates.
		$msg = $dataset->updateDataset($database);
	    }
	}

	# Only commit if an error has not occured, otherwise roll back.
	if ($msg eq "") { $report .= $database->commit(); }
	else { $report .= $msg."  Database rolled back.\n".$database->rollback(); }
    }

    $database->disconnect();
}








