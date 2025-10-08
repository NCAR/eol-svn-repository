#!/usr/bin/perl

use strict;


for my $file (@ARGV) {
	#cos split GENPRO file
	print "Splitting: $file \n";
	system("cossplit", "$file");
	
	#read dir $Bin
	opendir(DIR, ".");
	my @files = readdir(DIR);
	closedir(DIR);

	#cos splitted files are of format f###
	@files = grep(m/f\d\d\d/, @files);
	#IMPORTANT: if the files are not sorted they will cat'ed together in the  wrong order
	@files = sort(@files);

	#cat the split pieces
	my $catline = "cat ";
	foreach my $ffile (@files) {
		system("cosconvert", "-b", "$ffile");
		$catline .= "$ffile ";
	}
	$catline .= "> temp";
	print "Joining: $catline\n";
	system($catline);

	#cleanup the cos split files so we dont reuse them..
	print "Cleaning up splited files\n";
	foreach my $ffile (@files) {
		system("rm", "-f", "$ffile");
	}
	
	#CONVERT!
	print "Converting..\n";
	system("g2n", "temp", "$file.nc");
	system("rm", "-f", "temp");
	print "\n";
	
	#reorder
	print "Reordering..\n";
	system("reorder", "$file.nc");
	my $output = `rm -f s$file.nc`;
	print $output;
	print "======================================================================\n";
}
