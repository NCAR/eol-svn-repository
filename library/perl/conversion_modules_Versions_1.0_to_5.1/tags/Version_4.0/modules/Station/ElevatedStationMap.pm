#! /usr/bin/perl -w

##Module----------------------------------------------------------------------
# The <code>ElevatedStationMap</code> is a <code>StationMap</code> that holds
# <code>Station</code> instances that are accessed by their station id, 
# network name, latitude, longitude, and elevation.
#
# @author Joel Clawson
# @version 3.0 <p>Checks on the parameter lists were added. This is to try to
# help find errors quicker by preventing the wrong arguements from being 
# passed to a function.  It still does not check the values, only ensures a
# correct number are passed to it.</p>
# <p>It was also update to handle the new packaging structure for the
# conversion modules.</p>
#
# @author Joel Clawson
# @version 1.00  This is a new class for Version2 of the modules to be able
# to handle data that reports lats/longs/elevations with the data so that 
# the map may catch instances when the station has moved.
##Module----------------------------------------------------------------------
package Station::ElevatedStationMap;
use strict;
use lib "..";
use Station::Station;
use Station::StationMap;
our @ISA = ("Station::StationMap");

##----------------------------------------------------------------------------
# @signature void addStation(Station station)
# <p>Insert a <code>Station</code> into the <code>StationMap</code>.</p>
# 
# @input $station The <code>Station</code> to insert into the StationMap.
# @warning This function will die if the <code>Station</code> is already in
# the <code>StationMap</code>.
##----------------------------------------------------------------------------
sub addStation {
    my $self = shift;
    if (scalar(@_) != 1) { die("Invalid parameters to addStation\n"); }
    
    # Die if the station is already in the StationMap
    if ($self->hasStation($_[0]->getStationId(), $_[0]->getNetworkName(),
			  $_[0]->getLatitude(), $_[0]->getLongitude(),
			  $_[0]->getElevation())) {
	die(sprintf("Station %s at %s %s at height %s in network %s is already in the StationMap\n", $_[0]->getStationId(), $_[0]->getLatitude(), $_[0]->getLongitude(), $_[0]->getElevation(), $_[0]->getNetworkName()));
    }

    $self->{"station_list"}->{$_[0]->getNetworkName()}->{$_[0]->getStationId()}->{sprintf("%10.5f", $_[0]->getLatitude())}->{sprintf("%11.5f", $_[0]->getLongitude())}->{sprintf("%7.2f", $_[0]->getElevation())} = $_[0];
}

##----------------------------------------------------------------------------
# @signature Station[] getAllStations()
# <p>Get a list of all of the <code>Stations</code> in the StationMap.</p>
#
# @output @list The list of the <code>Stations</code> in the StationMap.
##----------------------------------------------------------------------------
sub getAllStations {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getAllStations\n"); }
    my @list = ();

    foreach my $net (sort(keys(%{$self->{"station_list"}}))) {
	foreach my $stn (sort(keys(%{$self->{"station_list"}->{$net}}))) {
	    foreach my $lat (sort(keys(%{$self->{"station_list"}->{$net}->{$stn}}))) {
		foreach my $long (sort(keys(%{$self->{"station_list"}->{$net}->{$stn}->{$lat}}))) {
		    foreach my $elev (sort(keys(%{$self->{"station_list"}->{$net}->{$stn}->{$lat}->{$long}}))) {
			push(@list, $self->{"station_list"}->{$net}->{$stn}->{$lat}->{$long}->{$elev});
		    }
		}
	    }
	}
    }

    return @list;
}

##----------------------------------------------------------------------------
# @signature Station getStation(String stn_id, String net_id, float lat, float long, float elev)
# <p>Get the specified <code>Station</code> from the <code>StationMap</code>.
# </p>
#
# @input $stn_id The station id for the <code>Station</code>.
# @input $net_id The network id for the <code>Station</code>.
# @input $lat The latitude of the <code>Station</code>.
# @input $long The longitude of the <code>Station</code>.
# @input $elev The elevation of the <code>Station</code>.
# @output $station The specified <code>Station</code> if it exists, otherwise
# <code>NULL</code>.
##----------------------------------------------------------------------------
sub getStation {
    my $self = shift;
    if (scalar(@_) != 5) { die("Invalid parameters to getStation\n"); }

    return $self->{"station_list"}->{$_[1]}->{$_[0]}->
    {sprintf("%10.5f", $_[2])}->{sprintf("%11.5f", $_[3])}->
    {sprintf("%7.2f", $_[4])};
}

##----------------------------------------------------------------------------
# @signature int hasStation(String stn_id, String net_id, float lat, float long, float elev)
# <p>Check to see if a <code>Station</code> is in the <code>StationMap</code>.
# </p>
#
# @input $stn_id The station id for the <code>Station</code>.
# @input $net_id The network id for the <code>Station</code>.
# @input $lat The latitude of the <code>Station</code>.
# @input $long The longitude of the <code>Station</code>.
# @input $elev The elevation of the <code>Station</code>.
# @output $val A true value if the <code>Station</code> is in the map, false
# otherwise.
##----------------------------------------------------------------------------
sub hasStation {
    my $self = shift;
    if (scalar(@_) != 5) { die("Invalid parameters to hasStation\n"); }

    return defined($self->{"station_list"}->{$_[1]}->{$_[0]}->
		   {sprintf("%10.5f", $_[2])}->
		   {sprintf("%11.5f", $_[3])}->
		   {sprintf("%7.2f", $_[4])});
}

1;

