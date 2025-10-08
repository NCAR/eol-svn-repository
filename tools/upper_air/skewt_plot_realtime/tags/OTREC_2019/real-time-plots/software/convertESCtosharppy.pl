#!/usr/bin/perl -w

##Module--------------------------------------------------------------------------------
## <p>The convertESCtosharppy.pl script takes an ESC formatted sounding with one
## sounding per file, and converts it to a format that the SHARPpy code will
## read. This was used for the OTREC project to plot skew t's for the field
## catalog.</p>
## The argument is:
## <ol>
##   <li> Input file: The ESC sounding file to convert.
##
## The following file is created:
##   <li> Output file: The converted ESC sounding file written in SHARPpy format.
## The name is the same as the input file with .sharppy added to the end.
##
## @author Janet Scannell
## @version 1.0 June 2019
##
###Module-------------------------------------------------------------------------------

use strict;

die "Usage: convertESCtosharppy.pl input-ESC-file\n" if ($#ARGV != 0);

my $infile = $ARGV[0];

my $outfile = $infile . ".sharppy";

my @data;
my ($pressure, $height, $temp, $dwpt, $wdir, $wspd, $i);
my ($location, $releasedate);
my ($foundLocation, $foundReleasedate) = (0, 0);
my ($year, $month, $day, $hour, $minute) = ("00", "01", "01", "00", "00");

# Read the entire file

if (! open(FILE1,"<$infile"))
{
   die "ERROR: Could not open $infile for reading.  Terminating...\n";
}
my @infile = <FILE1>;
chop(@infile);
close(FILE1);

if (! open(FILE2,">$outfile"))
{
   die "ERROR: Could not open $outfile for writing.  Terminating...\n";
}

# Get the location and the release date and time.
# Remove white space from location name and reformat date/time.

for ($i=0; $i<15; $i++) {
   if (index($infile[$i], "Release Site") >= 0) {
      $location = substr($infile[$i], 35);
      $foundLocation = 1;
   }
   if (index($infile[$i], "UTC Release Time") >= 0) {
      $releasedate = substr($infile[$i], 35);
      $foundReleasedate = 1;
   }
}
if ($foundLocation == 1) {
   $location =~ tr/ //ds;
} else {
   $location = "Unknown";
}
if ($foundReleasedate == 1) {
   ($year, $month, $day, $hour, $minute) = $releasedate =~ /^\d\d(\d\d), (\d\d), (\d\d), (\d\d):(\d\d)/;
}

print(FILE2 "%TITLE%\n");
print(FILE2 " $location  $year$month$day/$hour$minute\n\n");
print(FILE2 "   LEVEL       HGHT       TEMP       DWPT       WDIR       WSPD\n");
print(FILE2 "-------------------------------------------------------------------\n");
print(FILE2 "%RAW%\n");

# Read all the data records and output to file.
# Change the missing value to what SHARPpy recognizes.

for ($i=15; $i<=$#infile; $i++) {
   @data = split(' ', $infile[$i]);
   $pressure = $data[1];
   $height = $data[14];
   $temp = $data[2];
   $dwpt = $data[3];
   $wdir = $data[8];
   $wspd = $data[7];
   if ($pressure == 9999.0) {
      $pressure = -9999.00;
   }
   if ($height == 99999.0) {
      $height = -9999.00;
   }
   if ($temp == 999.0) {
      $temp = -9999.00;
   }
   if ($dwpt == 999.0) {
      $dwpt = -9999.00;
   }
   if ($wdir == 999.0) {
      $wdir = -9999.00;
   }
   if ($wspd == 999.0) {
      $wspd = -9999.00;
   } else {
# Convert wind speed from m/s to kts
      $wspd = $data[7] * 1.94384;
   }
   printf(FILE2 "%10.2f,%10.2f,%10.2f,%10.2f,%10.2f,%10.2f\n", $pressure, $height, $temp, $dwpt, $wdir, $wspd);
}
print(FILE2 "%END%\n");
close(FILE2);
