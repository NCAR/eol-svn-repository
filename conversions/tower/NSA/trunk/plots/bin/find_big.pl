#!/usr/bin/perl

# find the big value 
if ( $#ARGV < 0 ) {
  print "file argument not specified..\n";
  exit();
}
my $in_file = $ARGV[0];

open(IN, $in_file) || die "cannot open $in_file";
while ($line=<IN>) {
  chomp($line);
  my @tmp = split(/\s+/, $line);
  my $ws = $tmp[6];
  if ( $ws >= 2000 || $ws < -999.99 ) {
    print "$line\n";
  }
}
close(IN);
