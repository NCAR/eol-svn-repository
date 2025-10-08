#! /usr/bin/perl -w

##Module----------------------------------------------------------------------
# <p>The RecordMap is a data structure for holding a set of Record values.</p>
#
# <p><font color=RED><b>Warning: </b>Storing every Record from the raw data
# into a single RecordMap may drasticly slow down the conversion if there is
# a lot of data.</font></p>
# <p><b>See:</b><a href=http://www.joss.ucar.edu/cgi-bin/dpg/doc/docperl.cgi?file=/work/DPG_HTML/BEST_SW/conversion_modules/Version1/Record.pm>Record.pm</a></p>
#
# <p><b>Intended Use: </b>The intended use, as seen by the author of the
# module, is to use the RecordMap to hold data to assist in sequence checking,
# duplicate Record checking, and accumulative precipitation.  It defines a
# unique Record by having a unique station id, network id, nominal date and
# nominal time.</p>
#
# @use  use lib "work/DPG_HTML/BEST_SW/conversion_modules/Version1";
#       use RecordMap;
#
# @author Joel Clawson
# @version 0.01 Original Version
##Module----------------------------------------------------------------------
package RecordMap;
use strict;
use lib ".";
use Record;

#----------------------------------------------------------------------
# Add a duplicate record to the list.
#
# @input $record The duplicate Record.
#----------------------------------------------------------------------
our $addDupeRecord = sub {
    my $self = shift;
    my $record = shift;
    push @{ $self->{"dupes"} }, $record;
};

##----------------------------------------------------------------------
# @signature void addRecord(Record record)
# <p>Add a Record to the map of Records.  If the Record is already in the
# RecordMap (same station, network, nominal date, and nominal time) the 
# most recently added Record goes to the duplicate Record list.</p>
#
# @input $record The Record to be added.
##---------------------------------------------------------------------
sub addRecord {
    my $self = shift;
    my $record = shift;
    if (defined($self->getRecord($record->getStationId(), 
				 $record->getNetworkId(),
				 $record->getNominalDate(), 
				 $record->getNominalTime()))) {
#	printf("Duplicate Record Found\n");
	$self->$addDupeRecord($record);
    } else {
	$self->{"data"}->{$record->getNetworkId()}->{$record->getStationId()}->{$record->getNominalDate()}->{$record->getNominalTime()} = $record;
    }
}

##-------------------------------------------------------------------------
# @signature String check1minuteSequence(String begin_date, String end_date)
# <p>Check the Records in the list to see if there are any Records missing.</p>
# 
# @input  $begin_date The first day to start the sequence search in YYYY/MM/DD
#                     format
# @input  $end_date The last day to end the sequence search in YYYY/MM/DD 
#                     format
# @output $summary Returns a string with the missing records, 1 per line.
##--------------------------------------------------------------------------
sub check1minuteSequence(String begin_date, String end_date) {
    my $self = shift;
    my $begin_date = shift;
    my $end_date = shift;
    my $sequence_summary = "";

    foreach my $network (keys %{ $self->{"data"}}) {
	foreach my $station (keys %{ $self->{"data"}->{$network}}) {
	    my $currentDate = $begin_date;
	    my $currentTime = "00:00";

	    while (Conversions::compareDates($currentDate, $end_date) <= 0) {
		if (!$self->hasRecord($station, $network, $currentDate, 
				      $currentTime)) {
		    $sequence_summary .= "$station in network $network does not have a record for nominal time $currentTime on $currentDate\n";
		}
		
		my $hour = substr($currentTime, 0, 2);
		my $min = substr($currentTime, 3, 2);
		$min++;
		if ($min > 59) {
		    $min = 0;
		    $hour++;
		}
		if ($hour > 23) {
		    $hour = 0;
		    $currentDate = Conversions::getNextDay($currentDate);
		}
		$currentTime = sprintf("%02d:%02d", $hour, $min);
	    }
	}
    }
    return $sequence_summary;
}

##-------------------------------------------------------------------------
# @signature String checkHourlySequence(String begin_date, String end_date)
# <p>Check the Records in the list to see if there are any Records missing.</p>
# 
# @input  $begin_date The first day to start the sequence search in YYYY/MM/DD
#                     format
# @input  $end_date The last day to end the sequence search in YYYY/MM/DD 
#                     format
# @output $summary Returns a string with the missing records, 1 per line.
##--------------------------------------------------------------------------
sub checkHourlySequence {
    my $self = shift;
    my $begin_date = shift;
    my $end_date = shift;
    my $sequence_summary = "";

    foreach my $network (keys %{ $self->{"data"}}) {
	foreach my $station (keys %{ $self->{"data"}->{$network}}) {
	    my $currentDate = $begin_date;
	    my $currentTime = "00:00";

	    while (Conversions::compareDates($currentDate, $end_date) <= 0) {
		if (!$self->hasRecord($station, $network, $currentDate, 
				      $currentTime)) {
		    $sequence_summary .= "$station in network $network does not have a record for nominal time $currentTime on $currentDate\n";
		}
		
		my $hour = substr($currentTime, 0, 2);
		$hour++;
		while (length($hour) < 2) { $hour = "0".$hour; }
		if ($hour > 23) {
		    $hour = "00";
		    $currentDate = Conversions::getNextDay($currentDate);
		}
		$currentTime = $hour.substr($currentTime, 2);
	    }
	}
    }
    return $sequence_summary;
}

##----------------------------------------------------------------------
# @signature void clear()
# <p>Remove all of the Records from the map.</p>
##----------------------------------------------------------------------
sub clear() {
    my $self = shift;
    $self->{"data"} = undef();
}

##----------------------------------------------------------------------
# @signature @Record getAllRecords()
# <p>Get a list of all of the Records that are in the map.</p>
#
# @output record_list Returns a list of Records in the map.
##----------------------------------------------------------------------
sub getAllRecords {
    my $self = shift;
    my @record_list;
    foreach my $network (keys %{ $self->{"data"}}) {
	foreach my $station (keys %{ $self->{"data"}->{$network}}) {
	    foreach my $date (keys %{ $self->{"data"}->{$network}->{$station}}) {
		foreach my $time (keys %{ $self->{"data"}->{$network}->{$station}->{$date}}) {
		    @record_list[scalar(@record_list)] = $self->getRecord($station, $network, $date, $time);
		}
	    }
	}
    }
    return @record_list;
}

##------------------------------------------------------------------------
# @signature @Record getDuplicateRecords()
# <p>Get the list of duplicate Records in the RecordMap.  A duplicate record
# is a record that has the same nominal date and time for a station that is
# already in the current RecordMap.</p>
#
# @output dupes Returns the list of duplicate Records.
##------------------------------------------------------------------------
sub getDuplicateRecords {
    my $self = shift;
    if (defined($self->{"dupes"})) {
	return @{ $self->{"dupes"} };
    } else {
	return ();
    }
}

##------------------------------------------------------------------------
# @signature $Record getRecord(String station_id, String network_id, String date, String time)
# <p>Get a Record from the RecordMap.</p>
#
# @input  $station_id The station of the Record.
# @input  $network_id The network of the Record.
# @input  $date The nominal date of the Record.
# @input  $time The nominal time of the Record.
# @output $record Returns the specified Record.
##------------------------------------------------------------------------
sub getRecord {
    my $self = shift;
    my $station_id = shift;
    my $network_id = shift;
    my $date = shift;
    my $time = shift;
    return $self->{"data"}->{$network_id}->{$station_id}->{$date}->{$time};
}

##-----------------------------------------------------------------------
# @signature int hasRecord(String station_id, String network_id, String date, String time)
# <p>Check to see if a Record is in the RecordMap.</p>
#
# @input  $station_id The station of the Record.
# @input  $network_id The network of the Record.
# @input  $date The nominal date of the Record.
# @input  $time The nominal time of the Record.
# @output $val Returns a true value if the Record is in the RecordMap.
##-----------------------------------------------------------------------
sub hasRecord {
    my $self = shift;
    my $station_id = shift;
    my $network_id = shift;
    my $date = shift;
    my $time = shift;
    return (defined($self->getRecord($station_id, $network_id, $date, $time)));
}

##-----------------------------------------------------------------------
# @signature RecordMap new()
# <p>Create a new RecordMap.</p>
#
# @output $recordMap Returns the new RecordMap.
##-----------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self, $class);
    return $self;
}

##------------------------------------------------------------------------
# @signature void removeRecord(String station_id, String network_id, String date, String time)
# <p>Remove the <code>Record</code> in the <code>RecordMap</code> that is 
# for the specified date and time for the station_id in the network.</p>
#
# @input $station_id The station identifier.
# @input $network_id The network identifier.
# @input $date The date of the record to be removed.
# @input $time The time of the record to be removed.
##-------------------------------------------------------------------------
sub removeRecord {
    my $self = shift;
    my $station_id = shift;
    my $network_id = shift;
    my $date = shift;
    my $time = shift;
    $self->{"data"}->{$network_id}->{$station_id}->{$date}->{$time} = undef();
}

1;
