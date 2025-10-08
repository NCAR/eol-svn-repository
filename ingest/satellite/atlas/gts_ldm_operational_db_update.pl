#! /usr/bin/perl -w

##Module---------------------------------------------------------------------------
# <p>The gts_ldm_operational_db_update.pl script runs once a day at 6:00 PM (18:00)
# to update the four GTS LDM operational datasets (100.009 - 100.012) with the 
# information from the files that were put up earlier in the day by the <b>savefiles</b>
# script.  The script reads all of the files in the directory for the current month
# and the previous month and inserts any files into the database that were not
# previously inserted.</p>
#
# @author Joel Clawson 7/19/2006
# @version 1.2 Updated the user to be the ingest user to the zedi database on
# tsunami.eol from the joss user on hurricane.joss.  Changed the email to come
# from the joss@eol user and fixed the Reply-To to come to me (Joel).
#
# @author Joel Clawson 5/15/2006
# @version 1.1 Migrated from hurricane.joss to tsunami.eol
#
# @author Joel Clawson 6/20/2005
# @version 1.0 The creation of the script.
##Module---------------------------------------------------------------------------
use strict;
use lib "/net/work/lib/perl/mysql";
use Email::MIME;
use Email::MIME::Creator;
use MySqlDatabase;
use MySqlDataset;
use MySqlMSSFile;

my $MSS_HOME = "/JOSS/DATA/RAW/BY_PROJECT/ATLAS/OBS";

#EXAMPLE: my $recipients = "USER1\@eol.ucar.edu,USER2\@eol.ucar.edu";
my $recipients = "eol-cds-ingest\@ucar.edu";

&main();

##----------------------------------------------------------------------------------
# @signature void main()
# <p>Read the files on the mass store and insert the files into the database that
# have not yet been inserted.</p>
##----------------------------------------------------------------------------------
sub main {
    my %datasets = load_dataset_defs();

    # Get the two months to be checked for files
    my ($cur_year,$cur_month) = get_current_date();
    my ($last_year,$last_month) = get_previous_month($cur_year,$cur_month);

    # Update the database with for the current and last month
    my $report = update_database($last_year,$last_month,\%datasets);
    $report .= update_database($cur_year,$cur_month,\%datasets);

    # Send out emails if there is a problem.
    if ($report ne "") { send_mail("GTS LDM MSS Script ERROR",$report); }
    #else { send_mail("GTS LDM MSS Script Notification","There are no errors to report."); }
}

##----------------------------------------------------------------------------------
# @signature (int year, int month) get_current_date()
# <p>Get the current month and year for the current day.</p>
#
# @output $year The current year.
# @output $month The current month.
##----------------------------------------------------------------------------------
sub get_current_date {
    my @time = localtime();
    return ($time[5]+1900,$time[4]+1);
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

##----------------------------------------------------------------------------------
# @signature Hash load_dataset_defs()
# <p>Load the attributes for the datasets into a hash to be returned.</p>
#
# @output $datasets The attributes for each of the datasets to be updated.
##----------------------------------------------------------------------------------
sub load_dataset_defs {
    my %datasets;

    $datasets{"sa"}{"dataset_id"} = "100.011";
    $datasets{"sh"}{"dataset_id"} = "100.009";
    $datasets{"sy"}{"dataset_id"} = "100.012";
    $datasets{"ua"}{"dataset_id"} = "100.010";

    return %datasets;
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

##----------------------------------------------------------------------------------
# @signature String update_database(int year, int month, Hash* datasets)
# <p>Update the database for the the files in the dataset hash reference for the
# specified month and year.</p>
#
# @input $year The year of the files to be updated.
# @input $month The month of the files to be updated.
# @input $datasets A reference to the hash containing the attributes for the datasets.
# @output $msg Any error messages that were generated when updating the database.
##----------------------------------------------------------------------------------
sub update_database {
    my ($year,$month,$datasets) = @_;
    my $msg = "";

    # Create the database and make the connection.
    #my $database = MySqlDatabase->new("joss","ofps-345");
    my $database = MySqlDatabase->new("ingest","gob-ble");
    $database->connect();

    my $result = `/net/local_lnx/dcs/bin/msls $MSS_HOME`;
    my $monthdir = sprintf("%04d%02d",$year,$month);
    if ($result !~ /$monthdir/) { 
        $database->disconnect();
	return $msg; 
    }
    
    # Define the directory where the files can be found.
    my $dir = sprintf("%s/%04d%02d",$MSS_HOME,$year,$month);
    
    # Get the list of GEMPAK files in the directory
    my @files = grep(/\.gem/,`/net/local_lnx/dcs/bin/msls -l $dir`);

    foreach my $file (sort(@files)) {
	chomp($file);
	my @file_props = split(' ',$file);
	my $dataset_id = $datasets->{substr($file_props[8],0,2)}->{"dataset_id"};

	# Look up the file in the database.
	my $inserted = $database->select("file","id",sprintf("directory='%s' and filename='%s' and dataset_id=(select id from dataset where archive_ident='%s')",$dir,$file_props[8],$dataset_id));

	# Only try to insert the file if it has not been put in already.
	if (!$inserted) {
	    # Define the file
	    my $mysql = MySqlMSSFile->new();
	    $mysql->setDatasetArchiveIdent($dataset_id);
	    $mysql->setFile($dir,$file_props[8]);
	    $mysql->setFormatId(48); # GEMPAK Format ID
	    $mysql->setBeginDate($year,$month,substr($file_props[8],8,2),0,0,0);
	    $mysql->setEndDate($year,$month,substr($file_props[8],8,2),23,59,59);

	    # Insert the file into the database.
	    $msg = $mysql->insert($database);

	    # Only update the dataset dates if the file was inserted properly.
	    if ($msg eq "") {

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
	    if ($msg eq "") { $msg = $database->commit(); }
	    else { $msg .= $msg."  Database rolled back.\n".$database->rollback(); }
	}
    }
    
    $database->disconnect();
    return $msg;
}
