#!/usr/bin/perl

use strict;

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
	
	#first column is the fill mass_store directory
	my $path = $split[0];
	
	#set subdir (dir where files will be stores locally) as the dir that files are in on the mass store
	#Also set the GENPRO and NetCDF file names equal to the file names on the mass store
	if ($path =~ m/(\/RAF.*)(\/\S+)\/(\S+)$/) { $subdir = $2; $GPfile = $3; $NCfile = $3 }
	else {die "Unkown path: $path\n";}
	
	#if $NCfile is not a valid flight number, search for one in the path
	unless (uc($NCfile) =~ /\wF\d\d\w?/) {
		if (uc($path) =~ /(\wF\d\d\w?)/) { $NCfile = $1; }
	}

	#if a second column is present an alternate NetCDF filename was specified, set it
	if ($#split == 1) { $NCfile = $split[1] }

	unless (-d ".$subdir") {print "Making dir .$subdir\n"; mkdir ".$subdir"; }

	#Download GENPRO file
	print "Downloading: $path  To  .$subdir/$GPfile\n";
	system("msrcp", "mss:$path", ".$subdir/$GPfile");

	#cos split GENPRO file
	print "Splitting: .$subdir/$GPfile \n";
	system("cossplit", ".$subdir/$GPfile");
	
	#read dir .
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
	print "Converting\n";
	system("g2n", "temp", ".$subdir/$NCfile.nc");
	system("rm", "-f", "temp");
	print "\n";
}
chdir "./$subdir" or die "Unable to change dir";
print "Reordering\n";
system("reorder", "*.nc");
system("rm", "-f", "s*.nc");
