#!/usr/bin/perl

use Getopt::Std;
use Time::gmtime;
use strict;

if ( $#ARGV < 0 ) {
   # no arguments are specified so show the options
   print "check_length.pl\n";
   print "\t-i: name of input file (.sfc)\n";
   exit();
}

# fetch the options
my %option = {};
getopt("i:", \%option);
my $in_fname = $option{i};

# make sure that the user specified the input file
if ( !$in_fname) {
   print "Error: input file option (-i) not set..\n";
   exit();
} # endif


# make sure the input file exists
if ( -e $in_fname ) {
   open(IN, $in_fname) || die "cannot open $in_fname";
} else {
   die "ERROR: $in_fname does not exist!";
} # endif

my ($line, @values);
my $line_number = 0;
my $expected_number_of_values = 48;
my $num_values;
while ( $line = <IN> ) {
   if ( $line =~ /^\d{4}\/\d{2}\/\d{2}/ ) {
      $line_number++;
      @values = split(/\s+/, $line);
      $num_values = $#values+1;
      if ( $num_values != $expected_number_of_values ) {
         print "line number: $line_number has incorrect number of values..\n";
	 exit();
      } # endif
   } # endif
} # end while
print "there are no lines in $in_fname that have the incorrect length :=)\n";
close(IN);
#******************************
