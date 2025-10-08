#! /usr/bin/perl -w

##Module----------------------------------------------------------------------------------
# <p>The process_ncep.pl script is the script that controls the downloading and archiving
# of the NCEP precipitation data from Sid Katz.  The process the script follows.  If any
# step in the processing produces an error, an email message will be sent to the addresses
# listed in the <code>$addresses</code> variable and the remaining steps will not be
# executed.</p><ol>
#   <li>Download the data from the server.</li>
#   <li>Create tar balls of the data.</li>
#   <li>Put the tar balls on the mass store.</li>
#   <li>Email Chi-Fan that the tar balls are on the mass store.</li>
#   <li>Uncompress the data so it can be put into the database.</li>
#   <li>Move the data to the final archive location and insert the files into the database.</li>
# </ol>
#
# @author Joel Clawson
##Module----------------------------------------------------------------------------------
package NcepExec;
use strict;
use lib "../lib";
use NcepArchive;
use NcepFTP;
use NcepMySql;
use NcepUtil;

&main();

##-----------------------------------------------------------------------------------------
# @signature void main(int month, int year)
# <p>Download, archive, and insert the NCEP precipitation data from Sid Katz for the
# specified month.</p>
#
# @input $month The month to process.
# @input $year The year of the month.
# @input $address The email of the person running the script.
# @warning The month must be between 1 and 12 and the year must be four digits.
##-----------------------------------------------------------------------------------------
sub main {
    my $month;
    my $year;
    my $address;

    # Make sure the parameters are valid
    if (scalar(@ARGV) != 3 || $ARGV[0] < 1 || $ARGV[0] > 12 ||
	$ARGV[1] !~ /^\d{4}$/) {
	printf("Usage: process_ncep.pl MM YYYY address\n");
	exit(1);
    } else {
	($month,$year,$address) = @ARGV;
    }

    # Download the data from the server.
    my $cleared = download_data($month,$year,$address);

    # Archive the data on the mass store and prepare for the database.
    $cleared = archive_data($month,$year,$address) if ($cleared);

    # Insert the data into the database.
    $cleared = insert_data($month,$year,$address) if ($cleared);

    if ($cleared) {
      NcepUtil::send_mail(sprintf("Sid,\n\nThe data from %02d %04d has been downloaded and is now available on codiac.  Until next month...\n\nJanine\n\n",$month,$year),sprintf("Data for %02d %04d",$month,$year),$address,NcepUtil::final_email_addresses());
    }
}

##-----------------------------------------------------------------------------------------
# @signature int archive_data(int month, int year)
# <p>Archive the data using the NcepArchive module.  This creates tar balls and puts
# them on the mass store, emails Chi-Fan that the data is on the mass store, and 
# uncompresses/untars the files so they can be put into the database.</p>
#
# @input $month The month of the data being archvied.
# @input $year The year of the month.
# @output $success A true value if the process was successful, false if there were errors.
##-----------------------------------------------------------------------------------------
sub archive_data {
    my $month = shift;
    my $year = shift;
    my $address = shift;

    my $archive = NcepArchive->new($month,$year);
    my $report = $archive->archive_data();

    # Only uncompress the data if there are not any errors in the archiving process.
    if ($report eq "") {
	$report = $archive->uncompress_data();
	$archive->close();
	if ($report eq "") {
	    return 1;
	} else {
	  NcepUtil::send_mail(sprintf("There was a problem that occured during the uncompressing of the data.  Details follow.\n\n%s",$report),"NCEP Script Uncompress Error",$address,NcepUtil::addresses());
	    return 0;
	}
    } else {
      NcepUtil::send_mail(sprintf("There was a problem that occured during the archiving to the mass store.  Details follow.\n\n%s",$report),"NCEP Script Archvie Error",$address,NcepUtil::addresses());
	return 0;
    }
}

##-----------------------------------------------------------------------------------------
# @signature int download_data(int month, int year)
# <p>Download the data from the NCEP server into a local temporary ingest directory.</p>
#
# @input $month The month of the data to download.
# @input $year The year of the month.
# @output $success A true value if the data was downloaded, false if there was an error.
##-----------------------------------------------------------------------------------------
sub download_data {
    my $month = shift;
    my $year = shift;
    my $address = shift;

    my $ftp = NcepFTP->new($month, $year);
    my $report = $ftp->download_data();

    if ($report eq "") {
	return 1;
    } else {
      NcepUtil::send_mail(sprintf("There was a problem that occured during the FTP download.  Details follow.\n\n%s\n",$report),"NCEP Script FTP Error",$address,NcepUtil::addresses());
	return 0;
    }
}

##-----------------------------------------------------------------------------------------
# @signature (int,int) get_date()
# <p>Get the month and year of the data to process the data.  It returns the previous month
# and year pair.</p>
#
# @output $month The month to process.
# @output $year The year of the month.
##-----------------------------------------------------------------------------------------
sub get_date {
    my @today = localtime();

    my $month = $today[4];
    my $year = $today[5] + 1900;

    if ($month == 0) {
	$month = 12;
	$year--;
    }

    return ($month,$year);
}

##-----------------------------------------------------------------------------------------
# @signature int insert_data(int month, int year)
# <p>Insert the data into the database.</p>
#
# @input $month The month of the data being inserted.
# @input $year The year of the month.
# @output $success A true value if the database was updated successfully, false if there
# was an error.
##-----------------------------------------------------------------------------------------
sub insert_data {
    my $month = shift;
    my $year = shift;
    my $address = shift;


    chdir("/work/operational/surface/NCEP_EMC/software");

    my ($month,$year,$address) = @ARGV;

    my $db = NcepMySql->new($month,$year);
    $db->open();
    my $report = $db->insert_ncep();
    $db->disconnect();
    $db->close();

    if ($report ne "") {
        NcepUtil::send_mail(sprintf("There was a problem that occured during the inserting of the data into the database.  Details follow.\n\n%s\n",$report),"NCEP Script MySQL Database Error",$address,NcepUtil::addresses());
        return 0;
    } else { return 1; }


#    return (system(sprintf("/usr/local/bin/ssh -l joss hurricane %s/software/ncep_empsql.pl %d %04d %s",
#			 NcepUtil::ncep_home(),$month,$year,$address)) == 0);

#    return (system(sprintf("/usr/local/bin/ssh -l joss hurricane %s/software/ncep_mysql.pl %d %04d %s",
#			 NcepUtil::ncep_home(),$month,$year,$address)) == 0);
}
