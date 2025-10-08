#!/bin/perl

open (INFILE, "new.lat") || die "Problem opening the in file\n";
open (OUTFILE, ">files.out") || die "Problem opening the out file\n";
$i = 0;
while ($line = <INFILE>) {
	$i++;
	if (!($i % 30)) {
		print OUTFILE $line;
	}
}
