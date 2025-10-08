#! /usr/bin/perl -w
#
##Module--------------------------------------------------------------------------
# <p>Use this script to correct lat/lon data for DYNAMO Manus soundings. This 
#    problem was discovered after the files were processed, so this script is 
#    a post-processing step and will write out a new data file.</p>
#
# @author Linda Echo-Hawk
# @version Created for DYNAMO 2010 Manus data conversion
#           One file had lat/lon of 2.060/0.000
#           Three files had lat/lon of 0.000/0.000
#           Many files had lat/lon of 2.060/147.430 
#           Some files had lat/lon of -2.060/147.430 (correct)
#           - This script fixes each of the three conditions
#           - Use in a foreach loop
#
# Use:      Adjust_Manus_LatLon.pl <Manus_ARM_201202140230.cls> <output.new>
#
# Example:  Adjust_Manus_LatLon.pl Manus_ARM_201202140230.cls Manus_ARM_201202140230.cls.new
#
# -------------------------------------------------------------------------------------
use strict;
use warnings;


my $fileName = $ARGV[0];
my $outfile = $ARGV[1];

open (IN, "$ARGV[0]") || die "Can't open for reading\n";
open (OUT, ">$ARGV[1]") || die "Can't open file for writing\n";    

my $lat_cor = -2.060;
my $lon_cor = 147.430;
my $corr_type = 0;
my $index = 0;

my @lines = <IN>;
foreach my $line (@lines)
{
    # blank line at end of file will cause warnings without this
	if ($line =~ /^\s*$/)
	{
		last;
	}
	if ($index >= 15)
	{
		chomp ($line);
	    my @data = split (' ', $line);
        # $data[10] = longitude, $data[11] = latitude
		my $current_lon = $data[10];
		my $current_lat = $data[11];
		if ($corr_type == 1) # lat_pos
		{
			$data[11] = ($current_lat - 2.060) + $lat_cor unless $data[11] =~ /999.000/;
			# $data[10] unchanged
		}
		elsif ($corr_type == 2) # both_zero
		{
			$data[11] = ($current_lat + $lat_cor) unless $data[11] =~ /999.000/;
			$data[10] = ($current_lon + $lon_cor) unless $data[10] =~ /9999.000/;
		}
		elsif ($corr_type == 3) # lon_zero_lat_pos;
		{
			$data[11] = ($current_lat - 2.060) + $lat_cor unless $data[11] =~ /999.000/;
			$data[10] = ($current_lon + $lon_cor) unless $data[10] =~ /9999.000/;
		}
		else
		{
            # do nothing 
	    }

        my $outputRecord = sprintf "%6.1f %6.1f %5.1f %5.1f %5.1f %6.1f %6.1f %5.1f %5.1f %5.1f %8.3f %7.3f %5.1f %5.1f %7.1f %4.1f %4.1f %4.1f %4.1f %4.1f %4.1f\n", $data[0], $data[1], $data[2], $data[3], $data[4], $data[5], $data[6], $data[7], $data[8], $data[9], $data[10], $data[11], $data[12], $data[13], $data[14], $data[15], $data[16], $data[17], $data[18], $data[19], $data[20];

		print OUT "$outputRecord";
	}
	else  # index less than 15 - fix the header info
	{
		# print "Not yet at data section\n";

	    chomp ($line);
	    my $corrected_release = sprintf("Release Location (lon,lat,alt):    147 25.80'E, 02 03.60'S, 147.430, -2.060, 4.0");
        

	    if ($line =~ /Release Location \(lon,lat,alt\):/)
	    {
			my @release_value = split (' ', $line);
			$release_value[7] =~ s/,//g;
			$release_value[8] =~ s/,//g;

            print "LON: $release_value[7]   LAT: $release_value[8]\n";

			if (($release_value[8] == 2.060) && ($release_value[7] == 147.430))
			{
				$corr_type = 1; # lat_pos
			}
			elsif (($release_value[8] == 0.000) && ($release_value[7] == 0.000))
			{
				$corr_type = 2; # both_zero
			}
			elsif (($release_value[7] == 0.000) && ($release_value[8] == 2.060))
			{
				$corr_type = 3; # lon_zero_lat_pos;
			}

		    print OUT "$corrected_release\n";
	    }
	    else 
	    {  
		    print OUT "$line\n";
	    }
		
	}
	$index++;
}
close IN;
close OUT;

