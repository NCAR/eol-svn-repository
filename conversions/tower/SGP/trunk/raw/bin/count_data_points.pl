#!/usr/bin/perl

use Getopt::Std;
use strict;

#***************************
# count the number of datapoints
# in a raw (.dat) data file
#***************************

if ( $#ARGV < 0 ) {
   print "count_data_points.pl\n";
   print "\t-i: name of data file (.dat)\n";
   exit();
}

our($opt_i);
&getopt('i');
my $in_fname = $opt_i;

# die if the input file doesn't exist
die "error: $in_fname doesn't exist!!\n" if ( !-e $in_fname );


my ($line, $var_name, $value_str, @values, $num_data_points);
open(DATA, "$in_fname") || die "cannot open $in_fname";
while ( $line=<DATA> ) {
  chop($line);
  $line =~ s/\s+//g;
  if ( $line =~ /time_offset/ ) {
     ($var_name, $value_str) = split(/:/, $line);
     my @values = split(/,/, $value_str);
     my $num_data_points = $#values+1;
     print "there are $num_data_points data points for $in_fname\n";
     exit();
  } # endif
} # end while
close(DATA);
