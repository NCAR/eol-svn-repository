#! /usr/bin/perl -w
#
##Module--------------------------------------------------------------------------
# <p>Use this script to write out ESC header Release Location line (lat/lon 
#  information) which was incorrect due to perl libraries incorrectly displaying
#  negative zero latitude values as "N" instead of "S".</p>
#
# Release Location (lon,lat,alt):    073 09.00'E, 00 41.40'N, 73.150, 0.690, 1.0
# SHOULD BE:
# Release Location (lon,lat,alt):    073 09.00'E, 00 41.40'S, 73.150, -0.690, 1.0
#
# @author Linda Echo-Hawk
# @version Created for DYNAMO ARM Gan sounding conversion
#
# Usage:    WriteLatLon.pl <ESC formatted output file> <corrected ESC file>
#
# Example:  WriteLatLon.pl D20080924_221515.PQC.eol.cls $i.new
#           use in a foreach loop.  After verifying correctness,
#           mv *.new into *.cls (the original files should be
#           replaced with the correct files)
#
##Module--------------------------------------------------------------------------
use strict;
use warnings;                              


my $fileName = $ARGV[0];
my $outfile = $ARGV[1];
# save the original input file
print "Executing: /bin/cp $fileName $fileName.orig \n";
system "/bin/cp -f $fileName $fileName.orig";

# Input ($fileName): D20080915_235426_PQC.eol.cls  
# Output ($outfile): D20080915_235426_PQC.eol.cls.new
# cp -f D20080915_235426_PQC.eol.cls D20080915_235426_PQC.eol.cls.orig

open (IN, "$ARGV[0]") || die "Can't open for reading\n";
open (OUT, ">$ARGV[1]") || die "Can't open file for writing\n";    

# print "Checking File $fileName\n";

while (<IN>)
{
	my $line = "$_";
	chomp ($line);
	my $corrected_release = sprintf("Release Location (lon,lat,alt):    073 09.00'E, 00 41.40'S, 73.150, -0.690, 1.0");

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


