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

while (my $line = <FILE>) {
	#clean and split
	if ($line =~ /^\#/) { next; }
	chomp($line);
	my @split = split(',', $line);
	
	#first column is the fill mass_store path
	my $path = $split[0];

	#set subdir (dir where files will be stores locally) as the dir that files are in on the mass store
        #Also set the GENPRO and NetCDF file names equal to the file names on the mass store
        if ($path =~ m/(\/RAF.*)\/([^\/]+)\/([^\/]+)$/) { $subdir = $2; $file = $3}
        else {die "Unkown path: $path\n";}

        #if $NCfile is not a valid flight number, search for one in the path
        unless (uc($file) =~ /[A-Za-z]F\d\d\w?/) {
                    if (uc($path) =~ /([A-Za-z]F\d\d\w?)/) { $file = $1; }
	        }

        #if a second column is present an alternate NetCDF filename was specified, set NCfile to it
        if ($#split == 1) { $file = $split[1] }

	print `msls $path 2>&1`;
	if (`msls $path 2>&1` =~ m/No such file or directory/) { 
	    open (LOGFILE, ">>ErrorLog.txt");
	    print "Error Downloading $path File does not exist.\n";
	    print LOGFILE "Error Downloading $path File does not exist.\n";
	    close (LOGFILE);
	    next; 
	}

	unless (-d ".$subdir") {print "Making dir .$subdir\n"; mkdir ".$subdir"; }

	print "Downloading: $path  To  ./$subdir/$file\n";
	system("msrcp", "mss:$path", "./$subdir/$file");
}
chdir "./$subdir" or die "Unable to change dir";
print "Reordering\n";
system("reorder", "*.nc");
system("rm", "-f", "s*.nc");
