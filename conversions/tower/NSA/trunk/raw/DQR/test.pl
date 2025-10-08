#!/usr/bin/perl

use Getopt::Std;
use DQR::Flag;
use DQR::FlagList;
use strict;

exit();
#my $year = "2005";
my $year = "1969";
$year = int($year);
my @current = gmtime(time()); 
my $current_year = $current[5] + 1900;
$current_year = int($current_year);
print "year: $year  current: $current_year\n";

if ( $year < 1970) {
   print "xx: $year\n";
}
