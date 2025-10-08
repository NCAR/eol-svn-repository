#! /usr/bin/perl -w

##Module----------------------------------------------------------------------------
# <p>The ncep_aux_uncompress.pl script is an auxillary script that can be used
# to uncompress the downloaded data and put the files into a directory structure
# that will be recognized by the database modules.  It gives the user the ability
# to choose which collection of files is to be uncompressed.</p>
#
# @use //work/./NCEP_EMC/software/ncep_aux_uncompress.pl MM YYYY <i>options</i>
# <p>The <i>options</i> recognized are:<ul>
#   <li>dly - Daily Precipitation</li>
#   <li>hrly - Hourly Precipitation</li>
#   <li>snapshot - Snapshot GIF Imagery</li>
#   <li>st2_4km - 4 KM GRIB Data</li>
#   <li>stage4 - Stage IV GRIB Data and GIF Imagery</li>
# </ul></p>
#
# @author Joel Clawson
##Module----------------------------------------------------------------------------
use strict;
use lib "../lib";
use NcepArchive;

&main();

##-----------------------------------------------------------------------------------
# @signature void main(int month, int year, String[] types)
# <p>Uncompress the data for preparation for the database.</p>
#
# @input $month The month to be uncompressed.
# @input $year The year for the month.
# @input $types <b>Optional</b> A list of types of data to be uncompressed.  Can be
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
	uncompress_data($month,$year,$type);
    }
}

##-----------------------------------------------------------------------------------
# @signature void uncompress_data(int month, int year, String type)
# <p>Uncompress the specified type of data for the month and year.</p>
#
# @input $month The month to be uncompressed.
# @input $year The year of the month.
# @input $type The type of data to be uncompressed.
##-----------------------------------------------------------------------------------
sub uncompress_data {
    my $month = shift;
    my $year = shift;
    my $type = shift;

    my $archive = NcepArchive->new($month,$year);
    my $msg = "";

    if ($type eq "hrly") {
	$msg = $archive->uncompress_hrly_prcp();
    } elsif ($type eq "dly") {
	$msg = $archive->uncompress_dly_prcp();
    } elsif ($type eq "stage4") {
	$msg = $archive->uncompress_stage4();
    } elsif ($type eq "st2_4km") {
	$msg = $archive->uncompress_st2_4km();
    } elsif ($type eq "snapshot") {
	$msg = $archive->uncompress_snapshot();
    }
	
    printf("%s\n",$msg) if ($msg ne "");
}

##-----------------------------------------------------------------------------------
# @signature void print_usage()
# <p>Display the usage of the script to the screen.</p>
##-----------------------------------------------------------------------------------
sub print_usage {
    printf("Usage: ncep_aux_uncompress.pl MM YYYY\n");
    printf("       ncep_aux_uncompress.pl MM YYYY all\n");
    printf("       ncep_aux_uncompress.pl MM YYYY dly hrly snapshot st2_4km stage4\n");
    exit(1);
}

