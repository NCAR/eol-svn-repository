#!/usr/bin/perl

use Getopt::Std;

# Script to check .dat files for duplicate
# records.  The files must have the same
# date
if ( $#ARGV < 0 ) {
  print "check_for_duplicates.pl\n";
  print "\t-l: list of filenames\n";
  exit();
}
