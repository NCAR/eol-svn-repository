#! /usr/bin/perl -w

##Module---------------------------------------------------------------------------
# <p>The gts_ldm_operational_db_update.pl script runs once a day at 6:00 PM (18:00)
# to update the four GTS LDM operational datasets (100.009 - 100.012) with the 
# information from the files that were put up earlier in the day by the <b>savefiles</b>
# script.  The script reads all of the files in the directory for the current month
# and the previous month and inserts any files into the database that were not
# previously inserted.</p>
#
#  @author Linda Echo-Hawk
#  @version 2.0 22 March 2021
#  Revised the MySqlDatabase constructor to pass in the  ~/.my.cnf file
#  containing the dbase and password rather than having these hard-coded
#  in the script. This solved a security issue.
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
use lib "/net/work/lib/perl/mail";
use lib "/net/work/lib/perl/hpss";
#use lib "/h/eol/dmg/HPSS_cronjobs/lib";
use MySqlDatabase;
use MySqlDataset;
use MySqlMSSFile;
use HPSS;
use MAIL;

my $MSS_HOME = "/FS/EOL/operational/atlas/OBS";
my @monitors = ("eol-cds-ingest\@ucar.edu");

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
    if ($report ne "") { MAIL::send_mail("GTS LDM MSS Script ERROR",$0."\n\n".$report, @monitors); }
    #else { MAIL::send_mail("GTS LDM MSS Script Notification",$0."\n\n"."There are no errors to report.", @monitors); }
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

    my $update_report = "";

    # Create the database and make the connection.
    my $database = MySqlDatabase->new(); # use ~/.my.cnf
    $database->connect();

    my $monthdir = sprintf("%04d%02d",$year,$month);
    #my @result = grep(/$monthdir/,HPSS::ls($MSS_HOME));
    #print join("\n", @result);
    unless (grep(/$monthdir/,HPSS::ls($MSS_HOME))) {
        $database->disconnect();
	return $update_report; 
    }
    
    # Define the directory where the files can be found.
    my $dir = sprintf("%s/%04d%02d",$MSS_HOME,$year,$month);
    
    # Get the list of GEMPAK files in the directory
    #print "$dir\n";
    #print join("\n", HPSS::ls($dir, "-l"));
    #print "\n";
    my @files = grep(/\.gem/,HPSS::ls($dir, "-l"));

    foreach my $file (sort(@files)) {
	chomp($file);
	if ($file !~ /^-/) { next; } ####

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
	    my $msg = $mysql->insert($database);

	    # Only commit if an error has not occured, otherwise roll back.
	    if ($msg eq "") { $msg = $database->commit(); }
	    else { $update_report .= $msg."  Database rolled back.\n".$database->rollback(); }
	}
    }
    
    $database->disconnect();
    return $update_report;
}
