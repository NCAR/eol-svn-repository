#! /usr/bin/perl -w

##Module----------------------------------------------------------------------------
# <p>The ncep_aux_ftp.pl script is an auxillary script that can be used to download
# the NCEP precipitation data.  It allows the user the ability to choose the type(s)
# of files to be downloaded.  The options are:</p><ul>
#  <li>hrly- Hourly Precipitation Tar Ball</li>
#  <li>dly- Daily Precipitation Tar Ball</li>
#  <li>snapshot- Snapshot GIF Imagery Tar Ball</li>
#  <li>st2_4km- Daily GRIB Data Tar Balls</li>
#  <li>stage4- Stage IV Tar Ball</li>
#  <li>ceopavn- 12 Hourly Ceopavn Tar Balls</li>
# </ul>
#
# @author Joel Clawson
##Module----------------------------------------------------------------------------
use strict;
use lib "../lib";
use NcepFTP;

&main();

##-----------------------------------------------------------------------------------
# @signature void main(int month, int year <i>,types</i>)
# <p>Download data from the FTP server.</p>
#
# @input $month The month to be downloaded.
# @input $year The year for the month.
# @input $types <b>Optional</b> A list of types of data to be downloaded.  Can be
# all or a combination of hrly, dly, snapshot, stage4, st2_4km, ceopavn.
##-----------------------------------------------------------------------------------
sub main {
    if (scalar(@ARGV) < 2) { print_usage(); }

    # Make sure the month and year are in the correct format and exist.
    my $month = $ARGV[0];
    my $year = $ARGV[1];
    if ($month !~ /^\d{1,2}$/ || $month < 1 || $month > 12 || $year !~ /^\d{4}$/) {
	print_usage();
    }

    # Define the types to be downloaded.
    my $types = "hrly dly snapshot stage4 st2_4km ceopavn";
    if (defined($ARGV[2]) && ($ARGV[2] ne "all")) {
	$types = $ARGV[2];
	for (my $i = 3; $i < scalar(@ARGV); $i++) {
	    $types .= sprintf(" %s\n",$ARGV[$i]);
	}
    }
    
    # Actually download the data.
    download_data($month,$year,$types);
}

##-----------------------------------------------------------------------------------
# @signature void download_data(int month, int year, String types)
# <p>Download the specified data types for the month and year.</p>
#
# @input $month The month of the data.
# @input $year The year of the data.
# @input $types A space delimited list of types to be downloaded.
# @warning All warnings will be printed to the screen.
##-----------------------------------------------------------------------------------
sub download_data {
    my $month = shift;
    my $year = shift;
    my $type = shift;

    my $ftp = NcepFTP->new($month,$year);

    my $report = $ftp->open();
    if ($report ne "") { printf("%s\n",$report);die(); }

    if ($type =~ /hrly/) {
	printf("%s\n",$ftp->download_hourly_precip());
    }

    if ($type =~ /dly/) {
	printf("%s\n",$ftp->download_daily_precip());
    }

    if ($type =~ /snapshot/) {
	printf("%s\n",$ftp->download_snapshot());
    }

    if ($type =~ /stage4/) {
	printf("%s\n",$ftp->download_stage4());
    }

    if ($type =~ /st2_4km/) {
	printf("%s\n",$ftp->download_st2_4km());
    }

    if ($type =~ /ceopavn/) {
	printf("%s\n",$ftp->download_ceopavn());
    }

    $ftp->close();
}

##-----------------------------------------------------------------------------------
# @signature void print_usage()
# <p>Display the usage of the script to the screen.</p>
##-----------------------------------------------------------------------------------
sub print_usage {
    printf("Usage: ncep_aux_ftp.pl MM YYYY\n");
    printf("       ncep_aux_ftp.pl MM YYYY all\n");
    printf("       ncep_aux_ftp.pl MM YYYY hrly dly snapshot stage4 st2_4km ceopavn\n");
    exit(1);
}

