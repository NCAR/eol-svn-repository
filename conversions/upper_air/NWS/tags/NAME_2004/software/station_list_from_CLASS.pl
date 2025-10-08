#! /usr/bin/perl -w

use strict;
use lib "/work/software/conversion_modules/Version4";
use Station::Station;
use Station::ElevatedStationMap;

my $CLASS_DIR = "../final";
my $NETWORK = "NWS";
my $STN_FILE = "../final/NWS_NAME_stationCD.out";

&main();

sub main {
    opendir(my $DIR,$CLASS_DIR) or die("Can't read directory $CLASS_DIR\n");
    my @files = grep(/\.cls$/,readdir($DIR));
    closedir($DIR);

    my $stations = Station::ElevatedStationMap->new();

    printf("Files:\n");
    foreach my $file (@files) {
	open(my $FILE,sprintf("%s/%s",$CLASS_DIR,$file)) or die("Can't open file $file\n");
	my @header = (<$FILE>)[0..11];
	close($FILE);

	my $id = (split(/_/,$file))[0];
	my ($lon,$lat,$elev) = (split(' ',$header[3]))[7..9];
	my $date = substr($header[11],35,12);

	chomp($header[2]);
	$lat =~ s/,//g;
	$lon =~ s/,//g;
	$elev =~ s/,//g;

	my $station = $stations->getStation($id,$NETWORK,$lat,$lon,$elev);

	if (!defined($station)) {
	    $station = Station::Station->new($id,$NETWORK);
	    $station->setStationName(trim((split(/:/,$header[2]))[1]));
	    
	    my $lat_fmt = $lat < 0 ? "-" : "";
	    while (length($lat_fmt) < length($lat)) { $lat_fmt .= "D"; }
	    $station->setLatitude($lat,$lat_fmt);

	    my $lon_fmt = $lon < 0 ? "-" : "";
	    while (length($lon_fmt) < length($lon)) { $lon_fmt .= "D"; }
	    $station->setLongitude($lon,$lon_fmt);

	    $station->setElevation($elev,"m");

	    $station->setNetworkIdNumber(7);
	    $station->setPlatformIdNumber(54);
	    $station->setReportingFrequency("12 hourly");
	    $station->setStateCode((split(' ',$header[2]))[-1]);

	    printf("%s",$station->toString());

	    $stations->addStation($station);
	}
	
	$station->insertDate($date,"YYYY, MM, DD");
#	printf("%s: %s %s %s - %s\n",$id,$lat,$lon,$elev,$date);
    }

    open(my $STN,">$STN_FILE") or die("Can't open file: $STN_FILE\n");
    foreach my $station ($stations->getAllStations()) {
	print($STN $station->toString());
    }
    close($STN);
}

sub trim {
   my ($line) = @_;
   $line =~ s/^\s+//;
   $line =~ s/\s+$//;
   return $line;
}
