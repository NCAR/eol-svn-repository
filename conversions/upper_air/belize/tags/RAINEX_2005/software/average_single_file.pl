#! /usr/bin/perl -w


package AverageSounding;
use strict;
use Averager;

&main();

sub main {

    if (scalar(@ARGV) != 4) {
	die("Usage:  average.pl input_file output_file points frequency\n");
    }
    my ($infile,$outfile,$points,$freq) = @ARGV;


    my $averager = Averager->new(sprintf("%s_point_averager.log",$points));
    $averager->average($infile,$outfile,$points,$freq);
}
