#! /usr/bin/perl -w
#
##Module--------------------------------------------------------------------------
# <p>Use this script to update and write out ESC header Release Location line 
# (lat/lon information). Only this line is modified. </p>
#
# @author LE Cully
# @version 1    Updated to correct Colombo, Sri Lanka Longitude ESC output -
#               release point to be 79.872 = 79 deg 52.32 minutes. Set code
#               to determine output file name automatically based on input file name.
#  Bad  Long:: "Release Location (lon,lat,alt):    088 12.33'E, 06 54.35'N, 88.206, 6.906, 15.0"
#  Good Long:: "Release Location (lon,lat,alt):    079 52.32'E, 06 54.35'N, 79.872, 6.906, 15.0"
#
# @author Linda Echo-Hawk
# @version 0   Created for DYNAMO ARM Gan sounding conversion. Originally used to
#    correct where lat/lon info was incorrectly displaying negative zero latitude
#    values as "N" instead of "S". 
#    
#    Release Location (lon,lat,alt):    073 09.00'E, 00 41.40'N, 73.150, 0.690, 1.0
#    SHOULD BE:
#    Release Location (lon,lat,alt):    073 09.00'E, 00 41.40'S, 73.150, -0.690, 1.0
#
# Originally required 2 inputs: input and output file names:
#    Input ($fileName): D20080915_235426_PQC.eol.cls
#    Output ($outfile): D20080915_235426_PQC.eol.cls.new
#
# Usage:    WriteLatLon.pl <ESC formatted output file> 
#
# Example:  WriteLatLon.pl D20080924_221515.PQC.eol.cls 
#           use in a foreach loop.  After verifying correctness,
#           mv *.new into *.cls (the original files should be
#           replaced with the correct files)
#
##Module--------------------------------------------------------------------------
use strict;
use warnings;                              


my $fileName = $ARGV[0];
my $outfile = $ARGV[0].".corrLon";

print "Checking File $fileName. Output file name: $outfile\n";

open (IN, "$ARGV[0]") || die "Can't open for reading\n";
open (OUT, ">$ARGV[0].corrLon") || die "Can't open file for writing\n";    

while (<IN>)
{
	my $line = "$_";
	chomp ($line);
	my $corrected_release = sprintf("Release Location (lon,lat,alt):    079 52.32'E, 06 54.35'N, 79.872, 6.906, 15.0");

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


