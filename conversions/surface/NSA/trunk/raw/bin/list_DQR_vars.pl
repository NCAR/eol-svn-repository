#!/usr/bin/perl

use Getopt::Std;

#**************************************
# list and sort the variable names from a DQR file
# (ie: NSA_GNDRAD_flagging.txt)
# -i: name of input (DQR) file
# SJS 06/05/2008
#**************************************

if ( $#ARGV < 0 ) {
   print "USAGE: list_DQR_vars.pl\n";
   print "\t-i: input file name\n";
   exit();
} else {
   getopt(i);
} # endif

# the name of the input file
my $in_fname = $opt_i;

my %vars;
open(IN, "$in_fname") || die "cannot open $in_fname";

# first, create a hash where the key is the variable name
# so that we can get a list of unique variables names
while (<IN>) {
  chop;
  my ($station, $name, $flag, $date1, $time1,
      $date2, $time2, $id) = split(/\s+/);
  $vars{$name} = "" if $date1 =~ /\d+/;
} # end while
close(IN);

# now, print out the sorted variable names 
my $i=0;
my @var_names;
foreach $key (sort (keys %vars)) {
  print "$key\n";
} # end foreach
