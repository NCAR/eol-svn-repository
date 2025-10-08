#!/usr/bin/perl

use strict;
use FindBin qw($Bin);

chdir "$Bin" or die "Unable to set Bin Directory";

my $dir;
my $subdir;
my $GPfile;
my $NCfile;
unless (-e "files.txt") { system("vim files.txt"); }
open FILE,"files.txt" or die "Could not open files.txt: $!";

while (my $line = <FILE>) {
	#clean and split
	chomp($line);
	my @split = split(',', $line);
	if ($line =~ /^\#/) { next; }
	
	#first column is the fill mass_store path
	my $path = $split[0];
	
	#Skip Missing Files, Log it
	print `msls $path 2>&1`;
	if (`msls $path 2>&1` =~ m/No such file or directory/) { 
	    open (LOGFILE, ">>ErrorLog.txt");
	    print "Error Downloading $path File does not exist.\n";
	    print LOGFILE "Error Downloading $path File does not exist.\n";
	    close (LOGFILE);
	    next; 
	}
	#set subdir (dir where files will be stores locally) as the dir that files are in on the mass store
	#Also set the GENPRO and NetCDF file names equal to the file names on the mass store
	if ($path =~ m/(\/RAF.*)\/([^\/]+)\/([^\/]+)$/) { $subdir = $2; $GPfile = $3; $NCfile = $3 }
	else {die "Unkown path: $path\n";}

	#if $NCfile is not a valid flight number, search for one in the path
	unless (uc($NCfile) =~ /[A-Za-z]F\d\d\w?/) {
		if (uc($path) =~ /([A-Za-z]F\d\d\w?)/) { $NCfile = $1; }
	}

	#if a second column is present an alternate NetCDF filename was specified, set NCfile to it
	if ($#split == 1) { $NCfile = $split[1] }

	#Make subdir if needed
	unless (-d "$Bin/$subdir") {print "Making dir $Bin/$subdir\n"; mkdir "$Bin/$subdir"; }
	
	#if end file already exists don't bother
	#do it anyway, dosen't take that long
	#if (-e "$Bin/$subdir/$NCfile.nc") { next; }

	#don't bother downling the GP file if it exists
	unless (-e "$Bin/$subdir/$GPfile") 
	{   
	    #Download GENPRO file
	    print "Downloading: $path  To  $Bin/$subdir/$GPfile\n";
	    system("msrcp", "mss:$path", "$Bin/$subdir/$GPfile");
	}

	#cos split GENPRO file
	print "Splitting: $Bin/$subdir/$GPfile \n";
	system("cossplit", "$Bin/$subdir/$GPfile");
	
	#read dir $Bin
	opendir(DIR, "$Bin");
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
	system("/h/eol/stroble/scripts/g2n_dev/g2n/g2n", "temp", "$Bin/$subdir/$NCfile.nc");
	system("rm", "-f", "temp");
	print "\n";
	
	#reorder
	print "Reordering..\n";
	chdir "$Bin/$subdir" or die "Unable to change dir";
	system("reorder", "$NCfile.nc");
	my $output = `rm -f s$NCfile.nc`;
	print $output;
	chdir "$Bin" or die "Unable to set Bin Directory";
	print "======================================================================\n";
}
