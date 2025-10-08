#!/usr/bin/perl

use strict;

my $dir;
my $subdir;
my $file;
my $fileList = "filesnc.txt";
unless (-e $fileList) { system "vim $fileList"; }
open FILE,$fileList or die $!;

while (my $path = <FILE>) {
	chomp($path);
	if ($path =~ m/(\/RAF\/\d\d\d\d\/\d\d\d)(\/\S+)\/(\S+)$/) { $dir = $1; $subdir = $2; $file = $3; }
	else {die "Unkown path: $path\n";}
	
	unless (-d ".$subdir") {print "Making dir .$subdir\n"; mkdir ".$subdir"; }

	print "Downloading: $path  To  .$subdir/$file\n";
	system("msrcp", "mss:$path", ".$subdir/$file");
}
chdir "./$subdir" or die "Unable to change dir";
print "Reordering\n";
system("reorder", "*.nc");
system("rm", "-f", "s*.nc");
