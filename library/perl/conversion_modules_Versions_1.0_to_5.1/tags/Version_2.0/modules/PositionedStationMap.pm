#! /usr/bin/perl -w

##Module----------------------------------------------------------------------
# The <code>PositionedStationMap</code> is a <code>StationMap</code> that holds
# <code>Station</code> instances that are accessed by their station id, 
# network name, latitude and longitude.
#
# @author Joel Clawson
# @version 1.00  This is a new class for Version2 of the modules to be able
# to handle data that reports lats/longs with the data so that the map may
# catch instances when the station has moved.
##Module----------------------------------------------------------------------
package PositionedStationMap;
use strict;
use lib ".";
use Station;
use StationMap;
our @ISA = ("StationMap");

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
    
    # Die if the station is already in the StationMap
    if ($self->hasStation($_[0]->getStationId(), $_[0]->getNetworkName(),
			  $_[0]->getLatitude(), $_[0]->getLongitude())) {
	die(sprintf("Station %s at %s %s in network %s is already in the StationMap\n", $_[0]->getStationId(), $_[0]->getLatitude(), $_[0]->getLongitude(), $_[0]->getNetworkName()));
    }

    $self->{"station_list"}->{$_[0]->getNetworkName()}->{$_[0]->getStationId()}->{sprintf("%10.5f",$_[0]->getLatitude())}->{sprintf("%11.5f",$_[0]->getLongitude())} = $_[0];
}

##----------------------------------------------------------------------------
# @signature Station[] getAllStations()
# <p>Get a list of all of the <code>Stations</code> in the StationMap.</p>
#
# @output @list The list of the <code>Stations</code> in the StationMap.
##----------------------------------------------------------------------------
sub getAllStations {
    my $self = shift;
    my @list = ();

    foreach my $net (sort(keys(%{$self->{"station_list"}}))) {
	foreach my $stn (sort(keys(%{$self->{"station_list"}->{$net}}))) {
	    foreach my $lat (sort(keys(%{$self->{"station_list"}->{$net}->{$stn}}))) {
		foreach my $long (sort(keys(%{$self->{"station_list"}->{$net}->{$stn}->{$lat}}))) {
		    push(@list, $self->{"station_list"}->{$net}->{$stn}->{$lat}->{$long});
		}
	    }
	}
    }
    
    return @list;
}

##----------------------------------------------------------------------------
# @signature Station getStation(String stn_id, String net_id, float lat, float long)
# <p>Get the specified <code>Station</code> from the <code>StationMap</code>.
# </p>
#
# @input $stn_id The station id for the <code>Station</code>.
# @input $net_id The network id for the <code>Station</code>.
# @input $lat The latitude of the <code>Station</code>.
# @input $long The longitude of the <code>Station</code>.
# @output $station The specified <code>Station</code> if it exists, otherwise
# <code>NULL</code>.
##----------------------------------------------------------------------------
sub getStation {
    my $self = shift;
    return $self->{"station_list"}->{$_[1]}->{$_[0]}->
    {sprintf("%10.5f",$_[2])}->{sprintf("%11.5f",$_[3])};
}

##----------------------------------------------------------------------------
# @signature int hasStation(String stn_id, String net_id, float lat, float long)
# <p>Check to see if a <code>Station</code> is in the <code>StationMap</code>.
# </p>
#
# @input $stn_id The station id for the <code>Station</code>.
# @input $net_id The network id for the <code>Station</code>.
# @input $lat The latitude of the <code>Station</code>.
# @input $long The longitude of the <code>Station</code>.
# @output $val A true value if the <code>Station</code> is in the map, false
# otherwise.
##----------------------------------------------------------------------------
sub hasStation {
    my $self = shift;
    return defined($self->{"station_list"}->{$_[1]}->{$_[0]}->
		   {sprintf("%10.5f",$_[2])}->{sprintf("%11.5f",$_[3])});
}

1;
