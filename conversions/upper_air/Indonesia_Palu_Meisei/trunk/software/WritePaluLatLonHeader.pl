#! /usr/bin/perl -w
#
##Module--------------------------------------------------------------------------
# WritePaluLatLonHeader.pl
#
# <p>Use this script to write out ESC header Release Location line (lat/lon 
#  information) which was incorrect due to perl libraries incorrectly displaying
#  negative zero latitude values as "N" instead of "S".</p>
#
# Release Location (lon,lat,alt):    119 54.30'E, 00 54.95'S, 119.905, 0.916, 84.0
# SHOULD BE:
# Release Location (lon,lat,alt):    119 54.30'E, 00 54.95'S, 119.905, -0.916, 84.0
#
# @author L. Cully
# @version Updated by L. Cully to correct Indonesia Palu site's latitude.
#          The dayfiles and autoqc software incorrectly changes lat/lons
#          that are negative but very close to the equator or 0 longitude
#          lines. Those programs currently "convert" any negative value
#          between -1.0 and 0 to be a positive number. They "drop" the 
#          negative sign. This code adds back in the negative sign by
#          rewriting the whole header line.  The code was modified to
#          to set the output file name to have the "corrLat" suffix, to
#          output start and stop times for the processing.
#
# @author Linda Echo-Hawk
# @version Created for DYNAMO ARM Nauru sounding conversion
#
# Usage:    WritePaluLatLon.pl <ESC formatted output file> <corrected ESC file>
#
# Example:  WritePaluLatLon.pl Indonesia_Palu_201204012348.cls $i.corrLoc
#           use in a foreach loop.  After verifying correctness,
#           mv *.corrLoc into *.cls (the original files should be
#           replaced with the corrected files)
#
##Module--------------------------------------------------------------------------
use strict;
use warnings;                              

printf "\nWritePaluLatLonHeader.pl began on ";print scalar localtime;printf "\n";

open (IN, "$ARGV[0]") || die "Can't open for reading: $ARGV[0]\n";
open (OUT, ">$ARGV[0].corrLoc") || die "Can't open file for writing: $ARGV[0].corrLoc\n";    

print "Processing File $ARGV[0]. Output file is $ARGV[0].corrLoc\n";

while (<IN>)
{
	my $line = "$_";
	chomp ($line);
	my $corrected_release = sprintf("Release Location (lon,lat,alt):    119 54.30'E, 00 54.95'S, 119.905, -0.916, 84.0");

	if (/Release Location \(lon,lat,alt\):/)
	{
		print OUT "$corrected_release\n";
	}
	else 
	{
		print OUT "$line\n";
	}
}
close IN;

printf "\nWritePaluLatLonHeader.pl end on ";print scalar localtime;printf "\n";
