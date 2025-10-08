#! /usr/bin/perl -w
#
##Module--------------------------------------------------------------------------
# <p>Use this script to find ISF EOL formatted sounding data files with missing
# Header latitude/longitude information, and replace this data with the highest-
# available altitude data from the file.  Write out a new raw data file.
#
#
# Launch Location (lon,lat,alt):             -999 -999, -999 -999,    32.90
#
# @author Linda Echo-Hawk  17 August 2012
# @version Revised for DYNAMO NOAA P-3 dropsondes
#
# @author Linda Echo-Hawk
# @version Created for T-PARC DOTSTAR Astra dropsonde data conversion
#
# Usage:    ReplaceMissingLatLonHeaderData.pl <ISF EOL formatted raw data file> <output.new>
#
# Example:  ReplaceMissingLatLonHeaderData.pl D20080924_221515.PQC.eol.cls D20080924_221515.PQC.eol.cls.new
#           useful in a foreach loop, then redirect output >> results.txt
#
##Module--------------------------------------------------------------------------
use strict;
use warnings;

if (-e "/net/work") {
    use lib "/net/work/lib/perl/Utilities";
    use lib "/net/work/lib/perl/UpperAir";
    use lib "/net/work/lib/perl/Station";
} else {
    use lib "/work/lib/perl/Utilities";
    use lib "/work/lib/perl/UpperAir";
    use lib "/work/lib/perl/Station";
}
use DpgConversions;     


my $fileName = $ARGV[0];
my $outfile = $ARGV[1];

open (IN, "$ARGV[0]") || die "Can't open for reading\n";
open (OUT, ">$ARGV[1]") || die "Can't open file for writing\n";    

my $lat = 888;
my $lon = 888;
my $alt = -999.0;

my $foundData = 0;
my $gotInfo = 0;
my $gotDegMin = 0;
my $outputRecord;

	
my @datalines = <IN>;
my @lines = @datalines;
foreach my $line (@lines)
{
	# print $line;
	if (($line =~ /Launch Location \(lon,lat,alt\):             -999 -999, -999 -999,/) ||
         ($line =~ /Launch Location \(lon,lat,alt\):             0/)) 
	{
		print "$fileName:  $line\n";
		# create $outputRecord
		foreach my $dataline (@datalines)
		{
			my @data = split (' ', $dataline);
			if ($data[0] =~ /^.?\d+/)
			{
				$foundData = 1;
			}
			if (($foundData) && (!$gotInfo))
			{
   		 		if (($data[14] > -999.000000) && ($data[15] > -999.000000))
   				{
					$lon = $data[14];
   			 		$lat = $data[15];
   			 		print "Header lon = $lon  Header lat = $lat  ";
   				 	$alt = $data[13];
   				 	print "Header alt = $alt \n";
					$gotInfo = 1;
				}
			}
			if (($gotInfo) && (!$gotDegMin))
			{
				# print "GotInfo!\n";
				# -----------------------------------------------------
				# Determine the degrees and minutes for lat/lon header
				# -----------------------------------------------------
				# format length must be the same as the value length or
				# convertLatLong will complain (see example below)
				# base lat = 36.6100006103516 base lon = -97.4899978637695
				# Lat format = DDDDDDDDDDDDDDDD  Lon format = -DDDDDDDDDDDDDDDD  
				my $lat_fmt = $lat < 0 ? "-" : "";  
				while (length($lat_fmt) < length($lat)) { $lat_fmt .= "D"; } 
				my $lon_fmt = $lon < 0 ? "-" : "";  
				while (length($lon_fmt) < length($lon)) { $lon_fmt .= "D"; } 
				# print "Lat format = $lat_fmt  Lon format = $lon_fmt\n";
				# get the degrees and minutes values and directions
				my ($lat_deg,$lat_min,undef()) = convertLatLong($lat,$lat_fmt,"DM");
				my ($lon_deg,$lon_min,undef()) = convertLatLong($lon,$lon_fmt,"DM"); 
				my $lat_dir = $lat_deg < 0 ? "S" : "N";
				my $lon_dir = $lon_deg < 0 ? "W" : "E";        

				$outputRecord = sprintf "Launch Location (lon,lat,alt):    %03d %05.2f'%s %.6f, %02d %05.2f'%s %.6f, %.2f\n", 
					abs($lon_deg),$lon_min,$lon_dir,$lon,abs($lat_deg),$lat_min,$lat_dir,$lat,$alt; 
				$gotDegMin = 1;
			}
		}
		print (OUT $outputRecord);
	}
	else
	{
		print (OUT $line);
	}                  
}
close IN;
close OUT;

                  




