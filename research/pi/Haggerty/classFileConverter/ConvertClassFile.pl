#!/usr/bin/perl -w

##Module-------------------------------------------------------------------------- 
# @author Linda Echo-Hawk
# secondary author: C. Brooks Snyder
#          
#
# Usage:  reformat .cls files for Julie Haggerty's research
#
# 
#
##Module--------------------------------------------------------------------------
use strict;    # no auto-correct done by perl
use warnings;  # give warnings

#Usage
if(scalar(@ARGV) != 2)
{
	print "usage: ConvertClassFiles.pl <inputfile> <outputfile>\n";
	exit(1);
}

# open the input file and output file or give error if not possible
open (IN, "$ARGV[0]") || die "Can't open $ARGV[0] for reading: $!!\n";
open (OUT, ">$ARGV[1]") || die "Can't open $ARGV[1] for writing: $!\n";

#assign input file to filename and let user know that it is being checked
my $fileName = $ARGV[0];
print "Checking File $fileName\n";        

#define global variables
my $index = 0;
my $lat;
my $lon;
my $alt;

#print out HTML stuff and header
print (OUT "<HTML>\n");
print (OUT "<H2>99999 Howard University Raob at Barbados</H2>\n");
print (OUT "<PRE>\n");
print (OUT "-----------------------------------------------------------------------------\n");
print (OUT "   PRES   HGHT   TEMP   DWPT   RELH   MIXR   DRCT   SKNT   THTA   THTE   THTV\n");
print (OUT "    hPa     m      C      C      %    g/kg    deg   knot     K      K      K \n");
print (OUT "-----------------------------------------------------------------------------\n");

#define array lines and open for loop to populate arrays
my @lines = <IN>;
foreach my $line (@lines)
{
	if ($line =~ /Release Location/)	#go to the line that starts with "Release Location"
	{
		chomp ($line);			#remove \n from end of line

	    my @header = split (' ', $line); 	#fill array with elements separated by space on each line
       	        $lat = $header[8];		#designate which element of array is assigned to lat,lon,etc
		$lat =~ s/,//;			#replace any commas with nothing (space)
		$lon = $header[7];
		$lon =~ s/,//;
		$alt = $header[9];
#		print "$lat $lon $alt";
	}

	
	if ($index > 15)			#go to the 15th line where the data columns start
	{
	    chomp ($line);			#remove \n from line
	    my @data = split (' ', $line);	#split by empty space

        	my $pres = $data[1];		#assign variable names to specific array entry numbers
		my $height = $data[14];
		my $temp = $data[2];
		my $dewpt = $data[3];
		my $RH = $data[4];
	#print floating point values with proper column spacing
        my $newline = sprintf " %6.1f  %5.0f  %5.1f  %5.1f     %2.0f   -999   -999   -999   -999   -999   -999\n", $pres, $height, $temp, $dewpt, $RH;
        print (OUT $newline); 			#print each line to output file
	}
	

	$index++;				#increment counter variable

} #end of for loop
 
#print out footer info with lat,lon,alt data taken from the top of the input file
print (OUT "    Station information and sounding indices\n");
print (OUT "                         Station identifier: BAR\n");
print (OUT "                             Station number: 99999\n");
print (OUT "                           Observation time:\n"); 
print (OUT "                           Station latitude: $lat\n");
print (OUT "                          Station longitude: $lon\n");
print (OUT "                          Station elevation: $alt\n");
print (OUT "                            Showalter index: \n");
print (OUT "                               Lifted index: \n");
print (OUT "    LIFT computed using virtual temperature: \n");
print (OUT "                                SWEAT index: \n");
print (OUT "                                    K index: \n"); 
print (OUT "                         Cross totals index: \n"); 
print (OUT "                      Vertical totals index: \n"); 
print (OUT "                        Totals totals index: \n");
print (OUT "      Convective Available Potential Energy: \n");
print (OUT "             CAPE using virtual temperature: \n");
print (OUT "                      Convective Inhibition: \n");
print (OUT "             CINS using virtual temperature: \n");
print (OUT "                           Equilibrum Level: \n");
print (OUT " Equilibrum Level using virtual temperature: \n");
print (OUT "                   Level of Free Convection: \n");
print (OUT "             LFCT using virtual temperature:\n"); 
print (OUT "                     Bulk Richardson Number:\n"); 
print (OUT "          Bulk Richardson Number using CAPV: \n");
print (OUT "  Temp [K] of the Lifted Condensation Level: \n");
print (OUT "Pres [hPa] of the Lifted Condensation Level: \n");
print (OUT "     Mean mixed layer potential temperature: \n");
print (OUT "              Mean mixed layer mixing ratio: \n");
print (OUT "              1000 hPa to 500 hPa thickness: \n");
print (OUT "Precipitable water [mm] for entire sounding: \n");

close IN;		#close input and output files
close OUT;
               


