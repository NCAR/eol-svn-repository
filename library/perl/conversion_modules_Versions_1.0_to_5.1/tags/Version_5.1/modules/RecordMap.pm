#! /usr/bin/perl -w

##Module----------------------------------------------------------------------
# <p>The RecordMap is a data structure for holding a set of Record values.</p>
#
# <p><font color=RED><b>Warning: </b>Storing every Record from the raw data
# into a single RecordMap may drasticly slow down the conversion if there is
# a lot of data.</font></p>
#
# <p><b>Intended Use: </b>The intended use, as seen by the author of the
# module, is to use the RecordMap to hold data to assist in sequence checking,
# duplicate Record checking, and accumulative precipitation.  It defines a
# unique Record by having a unique station id, network id, nominal date and
# nominal time.</p>
#
# @use  use lib "work/DPG_HTML/BEST_SW/conversion_modules/Version3";
#       use RecordMap;
#
# @author Joel Clawson
# @version 3.00 No changes made.  Version updated to match the rest of the
# modules.
#
# @author Joel Clawson
# @version 2.00 Updated to handle the strange issues with Perl hashes and 
# undefined values.
#
# @author Joel Clawson
# @version 1.00 Original Version
##Module----------------------------------------------------------------------
package RecordMap;
use strict;
use lib ".";
use DpgDate qw(:DEFAULT);
use Record;

#----------------------------------------------------------------------
# Add a duplicate record to the list.
#
# @input $record The duplicate Record.
#----------------------------------------------------------------------
our $addDupeRecord = sub {
    my $self = shift;
    push(@{ $self->{"dupes"} }, $_[0]);
};

our $buildSummary = sub {
    my $self = shift;
    my $summary = "";

    foreach my $net (sort(keys(%{$self->{"data"}}))) {
	foreach my $stn (sort(keys(%{$self->{"data"}->{$net}}))) {
	    my $date = $_[0];
	    my $time = "00:00";

	    while (compareDates($date,"YYYY/MM/DD", $_[1],"YYYY/MM/DD") <= 0) {
		if (!$self->hasRecord($stn,$net,$date,$time)) {
		    $summary .= sprintf("%s in network %s does not have a record at %s %s\n", $stn, $net, $date, $time);
		}
		($date, $time) = adjustDateTime($date,"YYYY/MM/DD",$time,"HH:MM",$_[2],
							     $_[3],$_[4]);
	    }
	}
    }

    return $summary;
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
    if ($self->hasRecord($_[0]->getStationId(), $_[0]->getNetworkId(),
			 $_[0]->getNominalDate(), $_[0]->getNominalTime())) {
	$self->$addDupeRecord($_[0]);
    } else {
	$self->{"data"}->{$_[0]->getNetworkId()}->{$_[0]->getStationId()}->
	{$_[0]->getNominalDate()}->{$_[0]->getNominalTime()} = $_[0];
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
sub check1minuteSequence {
    my $self = shift;
    return $self->$buildSummary($_[0], $_[1], 0, 0, 1);
}

##-------------------------------------------------------------------------
# @signature String check5minuteSequence(String begin_date, String end_date)
# <p>Check the Records in the list ot see if there are any 5 minute records missing.</p>
#
# @input  $begin_date The first day to start the sequence search in YYYY/MM/DD
#                     format
# @input  $end_date The last day to end the sequence search in YYYY/MM/DD
#                     format
# @output $summary Returns a string with the missing records, 1 per line.
##--------------------------------------------------------------------------
sub check5minuteSequence {
    my $self = shift;
    return $self->$buildSummary($_[0],$_[1],0,0,5);
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
    return $self->$buildSummary($_[0], $_[1], 0, 1, 0);
}

##----------------------------------------------------------------------
# @signature void clear()
# <p>Remove all of the Records from the map.</p>
##----------------------------------------------------------------------
sub clear() {
    my $self = shift;
    delete($self->{"data"});
    delete($self->{"dupes"});
}

##----------------------------------------------------------------------
# @signature Record[] getAllRecords()
# <p>Get a list of all of the Records that are in the map.</p>
#
# @output record_list Returns a list of Records in the map.
##----------------------------------------------------------------------
sub getAllRecords {
    my $self = shift;
    my @record_list = ();
    foreach my $net (sort keys %{ $self->{"data"}}) {
	foreach my $stn (sort keys %{ $self->{"data"}->{$net}}) {
	    foreach my $date (sort keys %{ $self->{"data"}->{$net}->{$stn}}) {
		foreach my $time (sort keys %{ $self->{"data"}->{$net}->{$stn}
					       ->{$date}}) {
		    push(@record_list,$self->getRecord($stn,$net,$date,$time));
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
    if (defined($self->{"dupes"})) { return @{ $self->{"dupes"} }; } 
    else { my @list = (); return @list; }
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
    return $self->{"data"}->{$_[1]}->{$_[0]}->{$_[2]}->{$_[3]};
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
    return (defined($self->{"data"}->{$_[1]}->{$_[0]}->{$_[2]}->{$_[3]}));
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
    delete($self->{"data"}->{$_[1]}->{$_[0]}->{$_[2]}->{$_[3]});
}

1;

