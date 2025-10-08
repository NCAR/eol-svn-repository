#!/usr/bin/perl

use strict;
use FindBin qw($Bin);

chdir "$Bin" or die "Unable to set Bin Directory";

my $dir;
my $subdir;
my $file;
my $fileList = "filesnc.txt";
unless (-e $fileList) { system "vim $fileList"; }
open FILE,$fileList or die $!;

while (my $path = <FILE>) {
	chomp($path);
	if ($path =~ /^\#/) { next; }

	if ($path =~ m/(\/RAF\/\d\d\d\d\/\d\d\d)(\/\S+)\/(\S+)$/) { $dir = $1; $subdir = $2; $file = $3; }
	else {die "Unkown path: $path\n";}
	
	print `msls $path 2>&1`;
	if (`msls $path 2>&1` =~ m/No such file or directory/) { 
	    open (LOGFILE, ">>ErrorLog.txt");
	    print "Error Downloading $path File does not exist.\n";
	    print LOGFILE "Error Downloading $path File does not exist.\n";
	    close (LOGFILE);
	    next; 
	}

	unless (-d ".$subdir") {print "Making dir .$subdir\n"; mkdir ".$subdir"; }

	print "Downloading: $path  To  .$subdir/$file\n";
	system("msrcp", "mss:$path", ".$subdir/$file");
}
chdir "./$subdir" or die "Unable to change dir";
print "Reordering\n";
system("reorder", "*.nc");
system("rm", "-f", "s*.nc");
