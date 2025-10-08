#!/usr/bin/perl

use strict;

my $dir="/RAF/1993/770";

open FILE,"files.txt" or die $!;

while (my $file = <FILE>) {
    chomp($file);
   

    print "Downloading: $dir/$file  To  ./$file";
    system("msrcp", "mss:$dir/$file", "./$file");
    
    print "Splitting a joining\n";
    system("cossplit", "$file");
    system("cosconvert", "-b", "f001");
    system("cosconvert", "-b", "f002");
    system("cosconvert", "-b", "f003");
    system("cat f001 f002 f003 > temp");
    print "Cleaning temp files\n";
    system("rm", "-f", "f001");
    system("rm", "-f", "f002");
    system("rm", "-f", "f003");
    print "Converting\n";
    system("g2n", "temp", "$file.nc");
    system("rm", "-f", "temp");
    print "\n";
}

