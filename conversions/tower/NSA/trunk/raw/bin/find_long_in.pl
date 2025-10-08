#!/usr/bin/perl

# script to find the incoming longwave radiation
# name from the netcdf file
my $in_file = "2007.vars.out";

open(IN, $in_file) || die "cannot open $in_file";

while ( <IN> ) {
  chop;
  print "$_\n" if (/cdf$/);
  print "\t$_\n" if (/down_long_hemisp_shaded1\b/);
  print "\t$_\n" if (/down_long_hemisp1\b/);
}
close(IN);
