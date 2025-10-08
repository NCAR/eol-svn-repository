#!/usr/bin/perl

use strict;

my $dir;
my $subdir;
my $file;
open FILE,"files.txt" or die $!;

while (my $path = <FILE>) {
	chomp($path);
	if ($path =~ m/(\/RAF\/\d\d\d\d\/\d\d\d)(\/\S+)\/(\S+)$/) { $dir = $1; $subdir = $2; $file = $3; }
	else {die "Unkown path: $path\n";}
	
	unless (-d ".$subdir") {print "Making dir .$subdir\n"; mkdir ".$subdir"; }

	print "Downloading: $path  To  .$subdir/$file\n";
	system("msrcp", "mss:$path", ".$subdir/$file");

	print "Splitting: .$subdir/$file \n";
	system("cossplit", ".$subdir/$file");
	
	opendir(DIR, ".");
	my @files = readdir(DIR);
	closedir(DIR);
	@files = grep(m/f\d\d\d/, @files);
	#IMPORTANT: if the files are not soreted they will cat'ed together wrong
	@files = sort(@files);

	my $catline = "cat ";
	foreach my $ffile (@files) {
		system("cosconvert", "-b", "$ffile");
		$catline .= "$ffile ";
	}
	$catline .= "> temp";
	print "Joining: $catline\n";
	system($catline);

	print "Cleaning up splited files\n";
	foreach my $ffile (@files) {
		system("rm", "-f", "$ffile");
	}
	
	print "Converting\n";
	system("g2n", "temp", ".$subdir/$file.nc");
	system("rm", "-f", "temp");
	print "\n";
}
print "Reordering";
system("reorder", ".$subdir/*.nc");
#system("rm", "-f", ".$subdir/s*.nc");
