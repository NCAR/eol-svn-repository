#! /usr/bin/perl -w

##Module---------------------------------------------------------------------
# <p>The StationMap module is a data structure for holding a list of 
# Station values.</p>
#
# <p><font color=red><b>Warning: </b>This is equivalent to an abstract class
# in Java.  It cannot be called by itself, you <b>MUST</b> use a subclass
# of the StationMap module.</font></p>
#
# <p><b>See: </b><a href="http://www.joss.ucar.edu/cgi-bin/dpg/doc/docperl.cgi?file=/work/DPG_HTML/BEST_SW/conversion_modules/Version3/Station/Station.pm">Station.pm</a></p>
# 
# <p><b>Intended Use: </b>The intended use, as seen by the author of the 
# module, is to use the StationMap to keep track of every station seen in the 
# conversion.  It is to be used to create the <code>stationCD.out</code>
# file by getting all of the Station instances in the map and printing them 
# out to the file.</p>
#
# @use  use lib "work/DPG_HTML/BEST_SW/conversion_modules/Version3";
#      use Station::StationMap;
#
# @author Joel Clawson
# @version 3.0 <p>Checks on the parameter lists were added. This is to try to
# help find errors quicker by preventing the wrong arguements from being 
# passed to a function.  It still does not check the values, only ensures a
# correct number are passed to it.</p>
# @author Joel Clawson
# <p>It was also update to handle the new packaging structure for the
# conversion modules.</p>
#
# @author Joel Clawson
# @version 2.00 Updated to handle the strange issues with Perl hashes and 
# undefined values.
#
# @author Joel Clawson
# @version 1.0 Original Version
##Module---------------------------------------------------------------------

package Station::StationMap;
use strict;
use lib "..";
use Station::Station;

##-------------------------------------------------------------------
# @signature void addStation(Station station)
# <p>Add a station to the known station list.</p>
#
# @input  $station The Station to be added to the known stations.
##-------------------------------------------------------------------
sub addStation {
    die("Subclass of StationMap must define addStation\n");
}

##-------------------------------------------------------------------
# @signature void clear()
# Remove all of the Station values from the StationMap.
##-------------------------------------------------------------------
sub clear {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to clear\n"); }
    delete $self->{"station_list"};
}

##-------------------------------------------------------------------
# @signature @Station getAllStations()
# Get a list of all of the stations that are in the StationMap
#
# @output stations Returns the list of stations known by the StationMap.
##-------------------------------------------------------------------
sub getAllStations {
    die("Subclass of StationMap must define getAllStations\n");
}

##-------------------------------------------------------------------
# @signature Station getStation(String station_id, String network_id)
# Get a station that is in the network.
#
# @input  $station_id The identifier of the station.
# @input  $network_id The name of the network.
# @output Returns the specified Station.
##-------------------------------------------------------------------
sub getStation {
    die("Subclass of StationMap must define getStation\n");
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
    if (scalar(@_) != 0) { die("Invalid parameters to getStationSummary\n"); }

    my ($ne,$se,$sw,$nw) = (0,0,0,0);
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
    $summary .= "The following hemispheres were seen in the stations:\n";
    if ($ne) { $summary.="\tnorth-east: Asia, N. Africa, Europe\n"; }
    if ($se) { $summary.="\tsouth-east: Australia, S. Africa\n"; }
    if ($sw) { $summary.="\tsouth-west: S. America\n"; }
    if ($nw) { $summary.="\tnorth-west: N. America\n"; }
    return $summary;
}

##-------------------------------------------------------------------
# @signature int hasStation(String station_id, String network_id, float lat, float long)
# Check to see if a station is in the StationMap
#
# @input $station_id The identifier of the station.
# @input $network_id The name of the network.
# @input $lat The latitude of the the station.
# @output $val Returns a true value if the station exist, 
#              otherwise a false value.
##-------------------------------------------------------------------
sub hasStation {
    die("Subclass of StationMap must define hasStation\n");
}

##------------------------------------------------------------------
# @signature StationMap new()
# <p>Create a new StationMap.</p>
#
# @output Returns a new StationMap.
##------------------------------------------------------------------
sub new {
    my $invocant = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to StationMap->new\n"); }
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self, $class);
    return $self;
}

1; # Need for returning from being called by another module.










