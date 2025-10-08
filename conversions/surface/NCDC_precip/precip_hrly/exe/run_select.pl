#!/bin/perl

# to reformat the select.sh script

open (INFILE, "select.sh") || die "Can't open the file for reading";
open (OUTFILE, ">run_select.sh") || die "Can't open the file for writing";
while ($line = <INFILE>) {
	@fields = split(/-time.+59:59"/, $line);
	print "@fields\n";
	print OUTFILE @fields;
}
print "\nAll done!\n";
