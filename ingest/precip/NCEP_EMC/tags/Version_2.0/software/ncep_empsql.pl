#! /usr/bin/perl -w

##Module----------------------------------------------------------------------------
# <p>The ncep_empsql.pl script is a piece of the process_ncep.pl script that 
# performs the database activities for the EmpSQL database.  It is needed 
# because the rest of the processing is done on tornado, while the database
# stuff needs to be done on hurricane.  The process_ncep.pl script will make
# a call to hurricane with this script to make it run.  <b>It should never be
# run by itself.</b></p>
#
# @author Joel Clawson
##Module----------------------------------------------------------------------------
package NcepDB;
use strict;
use lib "/work/operational/surface/NCEP_EMC/lib";
use NcepEmpsql;
use NcepUtil;

&main();


##----------------------------------------------------------------------------------
# @signature int main(int month, int year, String address)
#
# @input $month The month to be processed.
# @input $year The year of the month.
# @output $success 1 if the process was a success, 0 if it failed.
##----------------------------------------------------------------------------------
sub main {

    # Make sure the parameters are valid
    if (scalar(@ARGV) != 3 || $ARGV[0] < 1 || $ARGV[0] > 12 ||
	$ARGV[1] !~ /^\d{4}$/) {
	printf("Usage: ncep_empsql.pl MM YYYY addresses\n");
	exit(1);
    }

    chdir("/work/operational/surface/NCEP_EMC/software");

    my ($month,$year,$address) = @ARGV;

    my $db = NcepEmpSql->new($month,$year);
    my $report = $db->insert_ncep();
    $db->close();

    if ($report ne "") {
      NcepUtil::send_mail(sprintf("There was a problem that occured during the inserting of the data into the database.  Details follow.\n\n%s\n",$report),"NCEP Script EmpSQL Database Error",$address,NcepUtil::addresses());
	return 0;
    } else { return 1; }
}
