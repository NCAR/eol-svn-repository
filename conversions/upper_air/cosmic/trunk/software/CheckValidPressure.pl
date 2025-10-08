#!/usr/bin/perl -w

##Module-------------------------------------------------------------------------- 
# @author Linda Echo-Hawk
# 
# @version This script was created for the DEEPWAVE COSMIC sounding processing.
#          The purpose is to remove the initial data lines that do not have a
#          valid pressure value per Scot's instructions. The number of lines
#          varied for each raw data file. This script is to be run as a post-
#          processing step after running the COSMIC_NetCDF_Converter.pl script.
#          It reads in the *.cls file, checks for valid pressure and writes out a 
#          new file.
#
# Usage:    CheckValidPressure.pl <*.cls> <*.cls.new>
#
# Example:  CheckValidPressure.pl COSMIC_201406060611.cls COSMIC_201406060611.cls.new
#
#           Run this script in a foreach loop, then remove the original *.cls file
#           after confirming the changes are what you want, and rename the *.new
#           file to remove the ".new" extension.
#
##Module--------------------------------------------------------------------------
use strict;
use warnings; 

                             
open (IN, "$ARGV[0]") || die "Can't open for reading\n";
open (OUT, ">$ARGV[1]") || die "Can't open file for writing\n";

my $fileName = $ARGV[0];
print "Checking File $fileName\n";        

my $index = 0;
my $found_good_pressure = 0;
while (<IN>)
{
	my $line = "$_";
	chomp($line);

	
	my (@text) = (split(/ /,$line));
    my $myText = $text[1];

	if ((!$found_good_pressure) && ($myText =~ /9999.0/))
	{   
   		print "Skipping $myText\n";
	}
    else
	{
		print OUT "$line\n";
		if ($index > 15)
		{
			$found_good_pressure = 1;
		}
	}
	$index++;

}

close IN;


