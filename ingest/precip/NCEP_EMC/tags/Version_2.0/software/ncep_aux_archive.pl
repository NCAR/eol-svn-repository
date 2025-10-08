#! /usr/bin/perl -w

##Module----------------------------------------------------------------------------
# <p>The ncep_aux_archive.pl script is an auxillary script for the NCEP data
# processing.  It is only to be used if there is a problem running the 
# process_ncep.pl script and the archiving needs to be done.</p>
# <p>This provides the user the ability to have more control over the actual
# steps needed in the archiving process.  The script is used as follows:</p>
# <div>ncep_aux_archive.pl MM YYYY</div>
# <div>ncep_aux_archive.pl MM YYYY all</div>
# <div>ncep_aux_archive.pl MM YYYY ceopavn ncep</div>
# <p>The <b>MM</b> is the month to be processed and <b>YYYY</b> is the year.</p>
#
# @author Joel Clawson
##Module----------------------------------------------------------------------------
use strict;
use lib "../lib";
use NcepArchive;

&main();

##-----------------------------------------------------------------------------------
# @signature void main(int month, int year <i>, String[] types</i>)
# <p>Archive the data into tar balls and put them on the mass store.</p>
#
# @input $month The month to be downloaded.
# @input $year The year for the month.
# @input $types <b>Optional</b> A list of types of data to be archived.  Can be
# all or a combination of ceopavn ncep.
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
    my $types = "ceopavn ncep";
    if (defined($ARGV[2]) && ($ARGV[2] ne "all")) {
	$types = $ARGV[2];
	for (my $i = 3; $i < scalar(@ARGV); $i++) {
	    $types .= sprintf(" %s\n",$ARGV[$i]);
	}
    }
    
    # Archive the data for each type
    foreach my $type (split(' ',$types)) {
	archive_data($month,$year,$type);
    }
}

##-----------------------------------------------------------------------------------
# @signature void archive_data(int month, int year, String type)
# <p>Archive the specified type of data for the month and year.</p>
#
# @input $month The month to be archived.
# @input $year The year of the month.
# @input $type The type of data to be archived.
##-----------------------------------------------------------------------------------
sub archive_data {
    my $month = shift;
    my $year = shift;
    my $type = shift;

    my $archive = NcepArchive->new($month,$year);

    # Archive the ceopavn data
    if ($type eq "ceopavn") {
	printf("%s\n",$archive->archive_ceopavn());
	printf("%s\n",$archive->mass_store_ceopavn());

    # Archive the NCEP data
    } elsif ($type eq "ncep") {
	my $report = "";
	printf("%s\n",$report .= $archive->archive_ncep());
	printf("%s\n",$report .= $archive->mass_store_ncep());

	# Only should send the email if the tar ball was created and put on the mass store.
	if ($report eq "") {
	    $archive->email_chifan();
	} else {
	    printf("An error occured.  Email to Chi-Fan was not sent.\n");
	}
    }
}

##-----------------------------------------------------------------------------------
# @signature void print_usage()
# <p>Display the usage of the script to the screen.</p>
##-----------------------------------------------------------------------------------
sub print_usage {
    printf("Usage: ncep_aux_archive.pl MM YYYY\n");
    printf("       ncep_aux_archive.pl MM YYYY all\n");
    printf("       ncep_aux_archive.pl MM YYYY ceopavn ncep\n");
    exit(1);
}







