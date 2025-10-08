#!/usr/bin/perl

use strict;

my $dir;
my $subdir;
my @files = `msls -1 $ARGV[0]`;

foreach (@files)
{
    chomp $_;
    print "Downloading $_\n";
    system("msrcp", "mss:$ARGV[0]/$_", "$ARGV[1]");
}
