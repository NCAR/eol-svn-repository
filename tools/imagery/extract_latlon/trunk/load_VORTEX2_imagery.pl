#! /usr/bin/perl -w

##Module---------------------------------------------------------------------
# <p>load_VORTEX2_imagry.pl This perl program inserts jpg images into CODIAC with dates/times and lats/lons.
# image date and time is extracted from the image filename: "*YYYYMMDDhhmmss*.jpg".
# lats/lons are extracted from the jpeg images using jhead and converted to decimal</p>
#
# @author Sean Stroble
# @version This was originally created for Linda Echo-Hawk.
#
# Required inputs: an existing dataset_id and a valid directory conatining jpg images
# Optional inputs: mysqlhost: defaults to tsunami.eol.ucar.edu (production CODIAC database)
#
# Output: List of files and information to be inserted into codiac, confimation is required before the insert is committed
#
# Usage:    load_VORTEX2_imagry.pl dataset_id directory [mysqlhost]
#
# Example:  load_VORTEX2_imagry.pl 170.001 /net/work/Projects/VORTEX2/camera/load_test/test merlot.eol.ucar.edu
#
##Module---------------------------------------------------------------------

use strict;

use lib "/work/lib/perl/mysql";
use lib "/net/work/lib/perl/mysql";
use Cwd 'abs_path';
use MySqlDatabase;
use MySqlFile;

if ($#ARGV < 1){
	print "Usage: load_VORTEX2_imagery.pl dataset_id directory\n";
	exit(1);
}
die "$ARGV[1] is not a directory!\n" unless -d $ARGV[1];

my $purpose = "preview";
my $host = "tsunami.eol.ucar.edu";
my $dataset_id = $ARGV[0];
my $dir = abs_path($ARGV[1]); #Change dir to absolute path since CODIAC wont know what do do with relative paths
if ($#ARGV >= 2){ $host = $ARGV[2]};

#Get a list of all *.jpg files in specified directory
my @files = <$dir/*.jpg>;

#Connect to CODIAC MySql Database
my $database = MySqlDatabase->new("zediupdate", "change-456");
$database->setHost($host);
$database->connect();

my $msg = "";
my $count = 0;
my @MySqlFiles;
print "Processing Directory: $dir\n";
print "\ndataset_id date time lat lon type format_id size host path\n";
for (my $i = 0; $i <= $#files; $i++) {

	#Get Date/time info from filename
        if ($files[$i] =~ /(\d\d\d\d)(\d\d)(\d\d)(\d\d)(\d\d)(\d\d)/) {
		$count++;

		my @split = split(/\//,$files[$i]);
		
		#Create new mysql file
		my $mysqlFile = MySqlFile->new();
                $mysqlFile->setHost("localhost");
                $mysqlFile->setDatasetId($dataset_id);
                $mysqlFile->setFile($dir, $split[$#split]);
                $mysqlFile->setFormatId(60); #JPEG
                $mysqlFile->setBeginDate($1, $2, $3, $4, $5, $6);
                $mysqlFile->setEndDate($1, $2, $3, $4, $5, $6);
		$mysqlFile->setPurpose($purpose);
		
		#Get lat/lon from jhead
                my @coords = GetCoords($files[$i]);
		
		$mysqlFile->setMinlat($coords[0]);
		$mysqlFile->setMaxlat($coords[0]);
		$mysqlFile->setMinlon($coords[1]);
		$mysqlFile->setMaxlon($coords[1]);
		
		#Print info about file to be inserted
		print "$dataset_id $1-$2-$3 $4:$5:$6 $coords[0] $coords[1] $purpose 60 " . $mysqlFile->getSize() . " localhost $dir/$split[$#split]\n";
		
		#insert the file (Note: this is not yet final untill $database->commit() is called
                $msg = $mysqlFile->insert($database);
		#if something went wrong exit loop
                unless ($msg eq "") { last; }
        }
}
if ($msg eq "") {
	print "\n\n$count files are ready to be inserted.\n\nTo insert the files, press Enter.\nTo cancel the insert, enter any value and press Enter.\n\n>> "; 
	my $ans = <STDIN>;
	chomp ($ans);
	print "\n";
	if ($ans eq "") {
		$msg .= $database->commit(); 
	}
	else {
		print "User selected to cancel. Rolling back previous commands\n";
		$msg .= $database->rollback();
	}
}
if ($msg ne "") {
	#Something went wrong..
	$msg .= "Database rolled back.\n" . $database->rollback(); 
}
$database->disconnect();
print "$msg\n";

sub GetCoords
{
	my $image_file = shift;
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

	#Init lat/lon to -999.99999 (which will be returned if Lat/Lon cannot be found)
	my $lat_degrees = -99.99999;
	my $lon_degrees = -999.99999;
	if ($lat_string ne '' && $lat_string ne "GPS Latitude : ? ?")
	{
	   #---------
	   # Latitude
	   #---------
	   my @parts = split(/ /, $lat_string);
	   chop($parts[4]); chop($parts[5]); chop($parts[6]);                # chop chars from end of each element.
	   $lat_degrees = $parts[4] + (($parts[5] + ($parts[6]/60.0))/60.0); # Convert to decimal degrees.
	   if ($parts[3] eq 'S') {$lat_degrees = $lat_degrees * -1.0;}       # Southern Hemisphere 
	}

	if ($lon_string ne '' && $lon_string ne "GPS Longitude: ? ?")
	{
	   #-----------
	   # Longitude
	   #-----------
	   my @parts = split(/ /, $lon_string);
	   chop($parts[3]); chop($parts[4]); chop($parts[5]);                # chop chars from end of each element.
	   $lon_degrees = $parts[3] + (($parts[4] + ($parts[5]/60.0))/60.0); # Convert to decimal degrees.
	   if ($parts[2] eq 'W') {$lon_degrees = $lon_degrees * -1.0;}       # Western Hemisphere 
	}

	return ($lat_degrees, $lon_degrees);
}
