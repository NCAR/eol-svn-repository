#!/usr/bin/perl
# generate_arctic_manifest.pl
# Generates a list of directories and files in the 
# Arctic cloud staging area specified in the $base_path
# variable near the end of this file. Also determines
# the size of files in human-readable format and prints
# the result next to each file.
#
# Created by Morgan Garske 05/27/2015

use File::Find;
use strict;

my $base_path = '/scr/ctm/cloud/s3_staging';

sub byteToHuman {
	my $size = shift;
	if ($size < 2**10) {
		"$size B";
	}
	elsif ($size < 2**20) {
		$size /= 2**10;
		sprintf("%.2f KB", $size);
	}
	elsif ($size < 2**30) {
		$size /= 2**20;
		sprintf("%.2f MB", $size);
	}
	else {
		$size /= 2**30;
		sprintf("%.2f GB", $size);
	}
}

sub expandDirs {
	my $n = 29;
	my $dir = shift;
	my ($dirname) = $dir =~ /.*\/(.*)$/;
	my $depth = ($dir =~ s/\//\//g) - 4;
	print "   " x $depth . "$dirname/\n";
	my @contents = <$dir/*>;	
	my @files;
	foreach(<$dir/*>) {
#		&expandDirs($_) if -d;
		push(@files, $_) if -f;
	}
	if (scalar @files <= $n) {
		foreach (@files) {
			my $size = &byteToHuman(-s $_);
			my ($name) = $_ =~ /.*\/(.*)$/;
			print "   " x ($depth + 1);
			print "$name\t$size\n";
		}
	}
	else {
		my $size;
		foreach (@files) {
			$size += -s $_;
		}
		my $hsize = &byteToHuman($size);
		print "   " x ($depth + 1);
		print "(" . scalar @files . " files\t$hsize)\n";
	}
	foreach(<$dir/*>) {
		&expandDirs($_) if -d;
	}

}

print "acadis-field-projects/\n";
foreach (<$base_path/*>) {
	&expandDirs($_);
}
