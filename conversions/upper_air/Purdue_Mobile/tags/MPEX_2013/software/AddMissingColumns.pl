#!/usr/bin/perl -w

##Module-------------------------------------------------------------------------- 
# @author Linda Echo-Hawk
# @version Created for MPEX Purdue sounding conversion.  One file has a missing
#           column that needs to be found and have a missing value (-4316020) inserted.
#           The columns are separated by tabs in the original file.  Where the RH 
#           column should appear, there is an extra tab.  Insert the "missing" value
#           and write out the line.
#           NOTE: The problem file is: research.Purdue_sonde.20130604011707.skewT.txt
#
# Usage:    AddMissingColumns.pl <research.Purdue_sonde.20130604011707.skewT.txt> <*.new>
#
# Example:  AddMissingColumns.pl old.txt new.txt
#
##Module--------------------------------------------------------------------------
use strict;
use warnings; 

open (IN, "$ARGV[0]") || die "Can't open for reading\n";
open (OUT, ">$ARGV[1]") || die "Can't open file for writing\n";

# my $fileName = $ARGV[0];
# print "Checking File $fileName\n";        


while (<IN>)
{
	my $line = "$_";
	chomp($line);
	my (@cols) = (split("\t",$line));

    # if the column is empty, insert the "missing" value
	if ($cols[3] == "")
	{
		$cols[3] = -4316020;
		my $newLine = join " ", @cols;
		print OUT "$newLine\n";
	}
	else
	{
		print OUT "$line\n";
	}
}

close IN;


