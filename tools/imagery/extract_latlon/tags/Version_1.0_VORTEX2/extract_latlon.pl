#!/usr/bin/perl 

#-------------------------------------------------------------------
# extract_latlon.pl - This perl program extracts the lat/lon from
#   a jpeg image using jhead and then converts these to decimal degrees.
#
#   Execute: extract_latlon.pl [file_name] [info_level]
#
#   Required Inputs: Two input parameters are required as shown below:
#
#   [file_name] is the name of the image file to extract lat/lon from
#   and [info_level] is either 0 or 1. Level 0 produces minimal
#   information to be printed to screen. Level 1 produces more detailed
#   information to be printed to screen. The default info_level is 0.
#
#   Required Inputs: Two input parameters are required as shown above.
#
#   Output:
#     This s/w prints Informational Messages and the converted lat/lon
#     to the screen.  
#--------------------------------------------------------------------
# Jan 2011 lec
#   Created Version 1.0. For VORTEX2 Photogrammetry datasets.
#------------------------------------------------------------------------
printf "\nextract_latlon.pl $ARGV[0] began on ";print scalar localtime; printf "\n\n";

#----------------------------------
# Set the output information level.
#----------------------------------
$info = 0; # default

if ($ARGV[1] == 0) {$info = 0;}
if ($ARGV[1] == 1) {$info = 1;}

$image_file = $ARGV[0];

if ($info == 1) 
    { 
    printf "\nProcessing file:: $image_file\n";
    print "Image File $image_file Info.\n"; 
    system "/usr/local/codiac/extra/bin/jhead $image_file";
    }

#-------------------------------------------------
# Use the jhead tool to  pull out and convert
# the GPS lat/lons to decimal degrees.
#
#lat string:: GPS Latitude : N 41d 36m 52.2s
#lon string:: GPS Longitude: W 104d 15m  7.3s
#-------------------------------------------------
# Grab the output from the system command.
# Convert to decimal degrees and write to screen.
#-------------------------------------------------
my $lat_string = `/usr/local/codiac/extra/bin/jhead $image_file | grep "GPS Latitude"`;
my $lon_string = `/usr/local/codiac/extra/bin/jhead $image_file | grep "GPS Longitude"`;

# Remove excess whitespace
$lat_string =~ s/\s+$//; 
$lat_string =~ s/\s+/ /g; 

$lon_string =~ s/\s+$//; 
$lon_string =~ s/\s+/ /g;

if ($info ==1) {print "lat_string:: xxx $lat_string xxx\nlon_string:: xxx $lon_string xxx\n\n";}

if ($lat_string ne '' && $lat_string ne "GPS Latitude : ? ?")
   {
   #---------
   # Latitude
   #---------
   @parts = split(/ /, $lat_string);
   chop($parts[4]); chop($parts[5]); chop($parts[6]);                # chop chars from end of each element.
   $lat_degrees = $parts[4] + (($parts[5] + ($parts[6]/60.0))/60.0); # Convert to decimal degrees.
   if ($parts[3] eq 'S') {$lat_degrees = $lat_degrees * -1.0;}       # Southern Hemisphere 
    }
else
   { $lat_degrees = -999.99999; } # Null or missing GPS Latitude 


if ($lon_string ne '' && $lon_string ne "GPS Longitude: ? ?")
   {
   #-----------
   # Longitude
   #-----------
   @parts = split(/ /, $lon_string);
   chop($parts[3]); chop($parts[4]); chop($parts[5]);                # chop chars from end of each element.
   $lon_degrees = $parts[3] + (($parts[4] + ($parts[5]/60.0))/60.0); # Convert to decimal degrees.
   if ($parts[2] eq 'W') {$lon_degrees = $lon_degrees * -1.0;}       # Western Hemisphere 
   }
else
   { $lon_degrees = -999.99999;} # Null or missing GPS Longitude 

printf "lat_degrees = %10.5f\nlon_degrees = %10.5f\n",$lat_degrees, $lon_degrees;

printf "\nextract_latlon.pl of file $ARGV[0] ended on ";print scalar localtime;
