#! /usr/bin/perl -w

##Module-----------------------------------------------------------------------
# <p>The ldm_metar_mss_db_update.pl script is used for creating monthly tar
# files of the LDM Surface METAR data, putting them on the mass store once 
# they have been compressed and inserting them into the database.</p>
#
#  @author Linda Echo-Hawk
#  @version 2.0  22 March 2021
#  Revised the MySqlDatabase constructor to pass in the  ~/.my.cnf file
#  containing the dbase and password rather than having these hard-coded
#  in the script. This solved a security issue.
#
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
use lib "/net/work/lib/perl/mail";
use lib "/net/work/lib/perl/hpss";
#use lib "/h/eol/dmg/HPSS_cronjobs/lib";
use MySqlDatabase;
use MySqlDataset;
use MySqlMSSFile;
use HPSS;
use MAIL;
use File::Basename;

# Constants used by the script.
my $DATASET_ID = "100.013";
my $INGEST = "/export/ldm/data/atlas";
my $TEMP = "/scr/tmp/joss/atlas/METAR";
my $MSS_HOME = "/FS/EOL/operational/atlas/METAR";
my $CS_HOST='eoldata@data-access.ucar.edu';
my $CS_ARCHIVE="/glade/campaign/eol/archive/operational/atlas/METAR";
my @monitors = ("eol-cds-ingest\@ucar.edu");

my $report = "";

&main();

##-----------------------------------------------------------------------------
# @signature void main()
# <p>Run the script.</p>
##-----------------------------------------------------------------------------
sub main {
    my $proc_date = determine_processing_date();

    my @mss_files = create_month_files($proc_date);
    #
    # This script does not recover nicely if it can't write to HPSS in the
    # next step, because at this point in the code, the month files are in
    # the tarfile in $TEMP, and have been removed from the $INGEST dir, so
    # @mss_files will be empty if you try to rerun. The next line fixes
    # this.
    opendir(my $RAW,$TEMP) or sendMailAndDie("Cannot open $TEMP to be read.\n");
    if(@mss_files == 0){my @mss_files = grep(/^[^\.]+$/,readdir($RAW));}
    closedir($RAW);
    
    my @db_files = copy_to_mss(@mss_files);
    update_database(@db_files);

    if ($report ne "") { sendMailAndDie($report); }
    #else { sendMailAndDie("There are no errors to report.\n"); }
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

	my ($filename,$dir) = fileparse($file);
	my $msg;
	my $msg = HPSS::put(\$file, \"$MSS_HOME/$filename");
    my $cs_msg = `scp $file $CS_HOST:$CS_ARCHIVE/$filename 2>&1`;
    my $cs_chmod = `ssh $CS_HOST chmod 440 $CS_ARCHIVE/$filename 2>&1`;
    $msg .= $cs_msg . "\n" . $cs_chmod . "\n";
	if ($msg eq "") { #, "-d"
        print "no errors\n";
	    my $mysql = MySqlMSSFile->new();
	    $mysql->setDatasetArchiveIdent($DATASET_ID);
	    $mysql->setFile($MSS_HOME,$filename);
	    $mysql->setFormatId(53);
	    my @date = ($file =~ /\.(\d{4})(\d{2})\./);
	    $mysql->setBeginDate($date[0],$date[1],1,0,0,0);
	    $mysql->setEndDate($date[0],$date[1],
			       days_in_month($date[0],$date[1]),
			       23,59,59);

	    push(@db_files,$mysql);
	    if (system("/bin/rm -rf $file") != 0) {
	        $report .= "Could not remove $file\n";
	    }
	} else {
	    $report .= "Could not copy $file to the mass store.\n$msg\n";
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
	    if (system("/bin/tar -cf $TEMP/atlas.metar.$file.tar $file") == 0) {
		if (system("/usr/bin/bzip2 $TEMP/atlas.metar.$file.tar") == 0) {
		    push(@mss_files,"$TEMP/atlas.metar.$file.tar.bz2");
		} else {
		    $report .= "Could not bzip2 atlas.metar.$file.tar\n";
		}

		if (system("/bin/rm -rf $file") != 0) {
		    $report .= "Could not remove $file\n";
		}
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
# @signature void update_database(MySqlMSSFile[] files)
# <p>Insert the files into the database.</p>
#
# @input files The list of MySqlMSSFiles to be inserted into the database.
##-----------------------------------------------------------------------------
sub update_database {
    my @files = @_;

    # Establish the connection to the database.
    my $database = MySqlDatabase->new(); # use ~/.my.cnf
    $database->connect();

    foreach my $mysql (@files) {
	my $msg = $mysql->insert($database);

	# Only commit if an error has not occured, otherwise roll back.
	if ($msg eq "") { $report .= $database->commit(); }
	else { $report .= $msg."  Database rolled back.\n".$database->rollback(); }
    }

    $database->disconnect();
}


sub sendMailAndDie {
    my ($body) = @_;
    MAIL::send_mail("LDM Surface METAR Script Error",$0."\n\n".$body, @monitors);
    exit(1);
}
