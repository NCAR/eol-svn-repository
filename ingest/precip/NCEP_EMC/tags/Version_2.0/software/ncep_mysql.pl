#! /usr/bin/perl -w

##Module----------------------------------------------------------------------------
# <p>The ncep_mysql.pl script is a piece of the process_ncep.pl script that 
# performs the database activities for the MySQL database.  It is needed 
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
use NcepMySql;
use NcepUtil;

&main();

##----------------------------------------------------------------------------------
# @signature int main(int month, int year, String addresses)
# <p>Insert the NCEP precipitation data for the specified month into the MySQL
# database.</p>
#
# @input $month The month of the data.
# @input $year The year of the data.
# @input $address The email addressses of people to send error messages to.
# @output 1 if there were no errors, 0 otherwise.
##----------------------------------------------------------------------------------
sub main {

    # Make sure the parameters are valid
    if (scalar(@ARGV) != 3 || $ARGV[0] < 1 || $ARGV[0] > 12 ||
	$ARGV[1] !~ /^\d{4}$/) {
	printf("Usage: ncep_mysql.pl MM YYYY addresses\n");
	exit(1);
    }

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
}





