#! /usr/bin/perl -w

##Module----------------------------------------------------------------------------
# <p>The ncep_aux_mysql.pl script is an auxillary script to insert the NCEP
# precipitation data into the MySQL database.  It gives the user the ability to
# choose the type(s) of data to be inserted into the database.</p>
#
# @use //work/./NCEP_EMC/software/ncep_aux_mysql.pl MM YYYY <i>options</i>
# <p>The <i>options</i> allowed are:</p><ul>
#   <li>hrly - Hourly Gage Files</li>
#   <li>dly - Daily Gage Files</li>
#   <li>snapshot - Preview GIF Imagery</li>
#   <li>st2_4km - 4 KM GRIB Data Files (All Datasets)</li>
#   <li>stage4 - Stage IV GRIB Data and Imagery</li>
# </ul>
#
# @author Joel Clawson
##Module----------------------------------------------------------------------------
use strict;
use lib "../lib";
use NcepMySql;

&main();

##-----------------------------------------------------------------------------------
# @signature void main(int month, int year, String[] types)
# <p>Put the downloaded data into the database.</p>
#
# @input $month The month to be inserted.
# @input $year The year for the month.
# @input $types <b>Optional</b> A list of types of data to be inserted.  Can be
# all or a combination of hrly dly snapshot stage4 st2_4km.
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
    my $types = "hrly dly snapshot stage4 st2_4km";
    if (defined($ARGV[2]) && ($ARGV[2] ne "all")) {
	$types = $ARGV[2];
	for (my $i = 3; $i < scalar(@ARGV); $i++) {
	    $types .= sprintf(" %s\n",$ARGV[$i]);
	}
    }
    
    foreach my $type (split(' ',$types)) {
	insert_data($month,$year,$type);
    }
}

##-----------------------------------------------------------------------------------
# @signature void insert_data(int month, int year, String type)
# <p>Insert the specified type of data into the database.</p>
#
# @input $month The month to be inserted.
# @input $year The year of the month.
# @input $type The type of data to be inserted.
##-----------------------------------------------------------------------------------
sub insert_data {
    my $month = shift;
    my $year = shift;
    my $type = shift;

    my $db = NcepMySql->new($month,$year);
    $db->open();

    if ($type eq "hrly") {
	my $msg = $db->insert_hourly_precip();
	printf("%s\n",$msg) if ($msg ne "");
    } elsif ($type eq "dly") {
	my $msg = $db->insert_daily_precip();
	printf("%s\n",$msg) if ($msg ne "");
    } elsif ($type eq "snapshot") {
	my $msg = $db->insert_snapshot();
	printf("%s\n",$msg) if ($msg ne "");
    } elsif ($type eq "stage4") {
	my $msg = $db->insert_stage4();
	printf("%s\n",$msg) if ($msg ne "");
    } elsif ($type eq "st2_4km") {
	my $msg = $db->insert_st2_4km();
	printf("%s\n",$msg) if ($msg ne "");
    }

    $db->disconnect();
    $db->close();
}

##-----------------------------------------------------------------------------------
# @signature void print_usage()
# <p>Display the usage of the script to the screen.</p>
##-----------------------------------------------------------------------------------
sub print_usage {
    printf("Usage: ncep_aux_mysql.pl MM YYYY\n");
    printf("       ncep_aux_mysql.pl MM YYYY all\n");
    printf("       ncep_aux_mysql.pl MM YYYY hrly dly snapshot stage4 st2_4km\n");
    exit(1);
}

