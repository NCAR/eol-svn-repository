#! /usr/bin/perl -w

##Module---------------------------------------------------------------------
# The StationMap module is a data structure for holding a list of 
# Station values.
#
# <p><font color=red><b>Warning: </b>This is only able to handle fixed 
# stations that never change location.</font></p>
#
# <p><b>See: </b><a href="http://www.joss.ucar.edu/cgi-bin/dpg/doc/docperl.cgi?file=/work/DPG_HTML/BEST_SW/conversion_modules/Version1/Station.pm">Station.pm</a></p>
# 
# <p><b>Intended Use: </b>The intended use, as seen by the author of the 
# module, is to use the StationMap to keep track of every station seen in the 
# conversion.  It is to be used to create the <code>stationCD.out</code>
# file by getting all of the Station instances in the map and printing them 
# out to the file.</p>
#
# @use  use lib "work/DPG_HTML/BEST_SW/conversion_modules/Version1";
#      use StationMap;
# @author Joel Clawson
# @version 0.01 Original Version
##Module---------------------------------------------------------------------

package StationMap;
use strict;
use lib ".";
use Station;

#&main();

##-------------------------------------------------------------------
# @signature void clear()
# Remove all of the Station values from the StationMap.
##-------------------------------------------------------------------
sub clear {
    my $self = shift;
    delete $self->{"station_list"};
}

##-------------------------------------------------------------------
# @signature @Station getAllStations()
# Get a list of all of the stations that are in the StationMap
#
# @output stations Returns the list of stations known by the StationMap.
##-------------------------------------------------------------------
sub getAllStations {
    my $self = shift;
    my @stations = ();
    foreach my $network (keys %{ $self->{"station_list"} }) {
	foreach my $station (keys %{ $self->{"station_list"}->{$network} }) {
	    @stations[scalar(@stations)] = $self->getStation($station,
							     $network);
	}
    }
    return @stations;
}

##-------------------------------------------------------------------
# @signature Station getStation(String station_id, String network_id)
# Get a station that is in the network.
#
# @input  $station_id The identifier of the station.
# @input  $network_id The name of the network.
#
# @output Returns the specified Station.
##-------------------------------------------------------------------
sub getStation {
    my $self = shift;
    my $station_id = shift;
    my $network_id = shift;
    return $self->{"station_list"}->{$network_id}->{$station_id};
}

##--------------------------------------------------------------------
# @signature String getStationSummary()
# Get a summary of the number of networks in the StationMap, the number of
# stations in each network, and the hemispheres where the stations are
# located.
# 
# @output Returns a summary of station information.  It includes the
#         number of stations in the list per network, along with which
#         hemispheres the stations exist in.
##--------------------------------------------------------------------
sub getStationSummary {
    my $self = shift;
    my $ne = 0;
    my $se = 0;
    my $sw = 0;
    my $nw = 0;
    my %networks; # A hash of networks to station counts
    my $network_count = 0;

    # Loop through all of the stations in the StationMap
    foreach my $station ($self->getAllStations()) {
	
	# Check to see if the station has valid dates
	# Only need to check begin date because end date can't be an invalid
	# date if the begin date is not. (Property of insertDate in Station.pm)
	if ($station->getBeginDate() ne "99999999") {
	    my $net = $station->getNetworkName();
	    if (defined($networks{$net})) { # add station to known network
		$networks{$net}++;
	    } else { # add a new network
		$networks{$net} = 1;
		$network_count++;
	    }
	    my $lat = $station->getLatitude();
	    my $long = $station->getLongitude();

	    # Determine which hemisphere the station is located
	    if ($lat >=0 && $long >= 0) { $ne = 1; }
	    elsif ($lat <=0 && $long >=0) { $se = 1; }
	    elsif ($lat <=0 && $long <=0) { $sw = 1; }
	    elsif ($lat >=0 && $long <=0) { $nw = 1; }
	}
    }

    # Create the summary
    my $summary = "There were ".$network_count." networks found:\n";
    foreach my $network (keys %networks) {
	$summary .= "\t".$network.": ".$networks{$network}." stations.\n";
    }
    $summary .= "The following hemispheres were seen in the stations:\n\t";
    if ($ne) { $summary.="north-east: Asia, N. Africa, Europe\n"; }
    if ($se) { $summary.="south-east: Australia, S. Africa\n"; }
    if ($sw) { $summary.="south-west: S. America\n"; }
    if ($nw) { $summary.="north-west: N. America\n"; }
    return $summary;
}

##-------------------------------------------------------------------
# @signature int hasStation(String station_id, String network_id)
# Check to see if a station is in the StationMap
#
# @input  $station_id The identifier of the station.
# @input  $network_id The name of the network.
# 
# @output $val Returns a true value if the station exist, 
#              otherwise a false value.
##-------------------------------------------------------------------
sub hasStation {
    my $self = shift;
    my $station_id = shift;
    my $network_id = shift;
    return (defined($self->getStation($station_id, $network_id)));
}

##-------------------------------------------------------------------
# @signature void addStation(Station station)
# Add a station to the known station list.
# @warning  This will replace a station with the same station identifier
#           and network name if it already exists in the network.
#
# @input  $station The Station to be added to the known stations.
##-------------------------------------------------------------------
sub addStation {
    my $self = shift;
    my $station = shift;
    $self->{"station_list"}->{$station->getNetworkName()}->{$station->getStationId()} = $station;
}

#--- Just a method used for testing.
sub main {
    my $stationA = Station->new();
    my $stationB = Station->new();
    my $stationC = Station->new();

    $stationA->setStationId("BOO");
    $stationA->setNetworkName("KVII");
    $stationB->setStationId("BEA");
    $stationB->setNetworkName("KVII");
    $stationC->setStationId("ALT01");
    $stationC->setNetworkName("COAGMET");

    my $converter = new StationMap->new();
    $converter->insertStation($stationA);
    $converter->insertStation($stationB);
    $converter->insertStation($stationC);

    my $retStation = $converter->getStation("BEA", "KVII");
    print $converter->hasStation("BOO", "KVII")."\n";
    print $converter->hasStation("BOO", "COAGMET")."\n";
    print $retStation->toQCF_String();
}

##------------------------------------------------------------------
# @signature StationMap new()
# Create a new StationMap.
#
# @output Returns a new StationMap.
##------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self, $class);
    return $self;
}
1; # Need for returning from being called by another module.










