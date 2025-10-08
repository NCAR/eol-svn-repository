#! /usr/bin/perl
use strict;

my $ext = shift(@ARGV);
my $extnew = shift(@ARGV);

my @files = <*.*>;
for my $file (@files) {
	print "$file\n";
    	my @split = split(/\./, $file);
	if ($#split == 1) {
		if ($split[1] eq $ext) {	
		    rename $file, "$split[0].$extnew";
	    	}
	}
}
