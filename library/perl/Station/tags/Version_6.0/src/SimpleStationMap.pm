#! /usr/bin/perl -w

##Module----------------------------------------------------------------------
# The <code>SimpleStationMap</code> is a <code>StationMap</code> that holds
# <code>Station</code> instances that are only accessed by the network name
# and the station's id.  <font color=red>It is not capable of handling 
# <code>Stations</code> that are mobile or have moved during the times data
# is gathered.</font>
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
# @version 2.00  This is a new class for Version2 of the modules.  It is 
# mostly the original <code>StationMap</code> from Version1, but has been
# refactored to this class and the <code>StationMap</code> superclass to 
# better deal with data that has lats/longs in the data.  See the 
# <code>PositionedStationMap</code> class for the use of lats/longs.
##Module----------------------------------------------------------------------
package SimpleStationMap;
use strict;
use Station;
use StationMap;
our @ISA = ("StationMap");

##----------------------------------------------------------------------------
# @signature void addStation(Station station)
# <p>Insert a <code>Station</code> into the <code>StationMap</code>.
# 
# @input $station The <code>Station</code> to be inserted.
# @warning This function will die if the <code>Station</code> is already in
# the <code>StationMap</code>.
##----------------------------------------------------------------------------
sub addStation {
    my $self = shift;
    if (scalar(@_) != 1) { die("Invalid parameters to addStation\n"); }

    # Die if the station is already in the StationMap
    if ($self->hasStation($_[0]->getStationId(), $_[0]->getNetworkName())) {
	die(sprintf("Station %s for network %s is already in the StationMap\n",
		    $_[0]->getStationId(), $_[0]->getNetworkName()));
    }

    $self->{"station_list"}->{$_[0]->getNetworkName()}->{$_[0]->getStationId()}= $_[0];
}

##----------------------------------------------------------------------------
# @signature int hasStation(String stn_id, String net_id)
# <p>Check to see if a <code>Station</code> is in the <code>StationMap</code>.
# </p>
#
# @input $stn_id The station id for the <code>Station</code>.
# @input $net_id The network id for the <code>Station</code>.
# @output $val A true value if the <code>Station</code> is in the map, false
# otherwise.
##----------------------------------------------------------------------------
sub hasStation {
    my $self = shift;
    if (scalar(@_) != 2) { die("Invalid parameters to hasStation\n"); }
    return defined($self->{"station_list"}->{$_[1]}->{$_[0]});
}

##----------------------------------------------------------------------------
# @signature Station[] getAllStations()
# <p>Get a list of all of the <code>Stations</code> in the <code>StationMap
# </code>.</p>
#
# @output @list The list of <code>Stations</code> in the StationMap.
##----------------------------------------------------------------------------
sub getAllStations {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getAllStations\n"); }
    my @list = ();
    
    foreach my $net (sort(keys(%{$self->{"station_list"}}))) {
	foreach my $stn (sort(keys(%{$self->{"station_list"}->{$net}}))) {
	    push(@list, $self->{"station_list"}->{$net}->{$stn});
	}
    }

    return @list;
}

##----------------------------------------------------------------------------
# @signature Station getStation(String stn_id, String net_id)
# <p>Get the specified <code>Station</code> from the <code>StationMap</code>.
# </p>
# 
# @input $stn_id The station id for the <code>Station</code>.
# @input $net_id The network id for the <code>Station</code>.
# @output $station The specified <code>Station</code> if it exists, otherwise
# <code>NULL</code>.
##----------------------------------------------------------------------------
sub getStation {
    my $self = shift;
    if (scalar(@_) != 2) { die("Invalid parameters to getStation\n"); }
    return $self->{"station_list"}->{$_[1]}->{$_[0]};
}
1;















