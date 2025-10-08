#! /usr/bin/perl -w

##Module--------------------------------------------------------------------------
# <p>The PrecipRecord module is a data structure for holding the meta data for
# a Precipication Quality Control Format (PQCF) data record.  It only contains
# the data about the station and network for the data.  It does <b>NOT</b> contain
# any precipitation data.  See one of the sub modules for the actual data 
# containers.</p>
#
# @since Version 4.
# 
# @author Joel Clawson
# @version 4.0 The original creation of the PrecipRecord Module.
##Module--------------------------------------------------------------------------
package Precip::PrecipRecord;
use strict;
use lib "..";
use DpgConstants qw(:DEFAULT);
use DpgConversions qw(:DEFAULT);
use Station::Station;
use Surface::QCFConstants;

##--------------------------------------------------------------------------------
# @signature String checkDefined(String value, String missing)
# <p>Determine which if a value or a missing value should be used.</p>
#
# @input $value The value to use if defined.
# @input $missing The missing value to use if the value is not defined.
# @output $val The value or missing value.
##--------------------------------------------------------------------------------
sub checkDefined {
    if (@_ != 3) { die("Invalid parameters to PrecipRecord->checkDefined\n"); }
    my ($self,$value,$missing) = @_;
    return defined($value) ? $value : $missing;
}

##--------------------------------------------------------------------------------
# @signature float getLatitude()
# <p>Get the latitude of the station where this record was taken.</p>
#
# @output $lat The latitude of the station.
##--------------------------------------------------------------------------------
sub getLatitude {
    if (@_ != 1) { die("Invalid parameters to PrecipRecord->getLatitude\n"); }
    my ($self) = @_;
    return $self->checkDefined($self->{"latitude"},$self->{"station"}->getLatitude());
}

##--------------------------------------------------------------------------------
# @signature float getLongitude()
# <p>Get the longitude of the station where this record was taken.</p>
#
# @output $lon The longitude of the station.
##--------------------------------------------------------------------------------
sub getLongitude {
    if (@_ != 1) { die("Invalid parameters to PrecipRecord->getLongitude\n"); }
    my ($self) = @_;
    return $self->checkDefined($self->{"longitude"},$self->{"station"}->getLongitude());
}

##--------------------------------------------------------------------------------
# @signature String getNetworkId()
# <p>Get the identifier for the network of the station where the record was taken.</p>
#
# @output $id The network id for the station.
##--------------------------------------------------------------------------------
sub getNetworkId {
    if (@_ != 1) { die("Invalid parameters to PrecipRecord->getNetworkId\n"); }
    my ($self) = @_;
    return $self->checkDefined($self->{"network"},$self->{"station"}->getNetworkName());
}

##--------------------------------------------------------------------------------
# @signature int getOccurence()
# <p>Get the occurence number for the record.</p>
#
# @output $occurence The occurence value for the record.
##--------------------------------------------------------------------------------
sub getOccurence {
    if (@_ != 1) { die("Invalid parameters to PrecipRecord->getOccurence\n"); }
    my ($self) = @_;
    return $self->checkDefined($self->{"occurence"},0);
}

##--------------------------------------------------------------------------------
# @signature String getStationId()
# <p>Get the identifier for the station where the record was taken.</p>
#
# @output $id The station id for the station.
##--------------------------------------------------------------------------------
sub getStationId {
    if (@_ != 1) { die("Invalid parameters to PrecipRecord->getStationId\n"); }
    my ($self) = @_;
    return $self->checkDefined($self->{"stn_id"},$self->{"station"}->getStationId());
}

##--------------------------------------------------------------------------------
# @signature int isValidPrecipFlag(int flag)
# <p>Determine if the specified flag is a valid precipitation flag.</p>
#
# @input $flag The flag to be tested.
# @output $pass If the flag passed the valid precip flag test.
##--------------------------------------------------------------------------------
sub isValidPrecipFlag {
    my ($self,$flag) = @_;
    return (defined($flag) && ($flag == $PRECIP_ACCUM_CODE ||
			       $flag == $PRECIP_ACCUM_END_CODE ||
			       $flag == $PRECIP_DELETE_CODE ||
			       $flag == $PRECIP_MISSING_CODE ||
			       $flag == $PRECIP_NO_QUALIFIER_CODE ||
			       $flag == $PRECIP_PROB_AMT_CODE ||
			       $flag == $PRECIP_SUSPECT_AMT_CODE ||
			       $flag == $PRECIP_TRACE_CODE));
}

##--------------------------------------------------------------------------------
# @signature int isValidQCFFlag(String flag)
# <p>Determine if the specified flag is a valid QCF flag.</p>
#
# @input $flag The $flag to be tested.
# @output $pass If the flag passed the valid QCF flag test.
##--------------------------------------------------------------------------------
sub isValidQCFFlag {
    my ($self,$flag) = @_;
    return (defined($flag) && ($flag eq $BAD_FLAG ||
			       $flag eq $DUBIOUS_FLAG ||
			       $flag eq $ESTIMATE_FLAG ||
			       $flag eq $GOOD_FLAG ||
			       $flag eq $GLITCH_FLAG ||
			       $flag eq $INCALCUABLE_FLAG ||
			       $flag eq $MISSING_FLAG ||
			       $flag eq $NEG_PRECIP_FLAG ||
			       $flag eq $NO_READING_FLAG ||
			       $flag eq $TRACE_PRECIP_FLAG ||
			       $flag eq $UNCHECKED_FLAG ||
			       $flag eq $VALUE_DOES_NOT_FIT_FLAG));
}

##--------------------------------------------------------------------------------
# @signature SELF_TYPE new(FileHandle WARN, --Station station--)
# <p>Create a new PrecipRecord instance.</p>
#
# @input $WARN The FileHandle where the warnings are to be written.
# @input $station <b>Optional</b> The station where the record was taken.
##--------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    if (@_ < 1 || @_ > 2) { die("Invalid parameters to PrecipRecord->new\n"); }
    my $self = {};
    my $class = $invocant || ref($invocant);
    bless($self,$class);

    $self->{"warn"} = $_[0];
    $self->{"station"} = defined($_[1]) ? $_[1] : Station::Station->new();

    return $self;
}

##--------------------------------------------------------------------------------
# @signature void setLatitude(float lat, String fmt)
# <p>Set the latitude where the record was taken.</p>
#
# @input $lat The latitude value.
# @input $fmt The format of the latitude.
##--------------------------------------------------------------------------------
sub setLatitude {
    if (@_ != 3) { die("Invalid parameters to PrecipRecord->setLatitude\n"); }
    my ($self,$lat,$lat_fmt) = @_;
    $self->{"latitude"} = (convertLatLong($lat,$lat_fmt,"D"))[0];
}

##--------------------------------------------------------------------------------
# @signature void setLongitude(float lon, String fmt)
# <p>Set the longitude where the record was taken.</p>
# 
# @input $lon The longitude value.
# @input $fmt The format of the longitude.
##--------------------------------------------------------------------------------
sub setLongitude {
    if (@_ != 3) { die("Invalid parameters to PrecipRecord->setLongitude\n"); }
    my ($self,$lon,$lon_fmt) = @_;
    $self->{"longitude"} = (convertLatLong($lon,$lon_fmt,"D"))[0];
}

##--------------------------------------------------------------------------------
# @signature void setNetworkId(String network)
# <p>Set the identifier for the network of the station where the record was taken.</p>
#
# @input $network The name of the network.
##--------------------------------------------------------------------------------
sub setNetworkId {
    if (@_ != 2) { die("Invalid parameters to PrecipRecord->setNetworkId\n"); }
    my ($self,$network) = @_;
    $self->{"network"} = $network;
}

##--------------------------------------------------------------------------------
# @signature void setOccurence(int occurence)
# <p>Set the occurence of the record.</p>
#
# @input $occurence The new occurence value.
##--------------------------------------------------------------------------------
sub setOccurence {
    if (@_ != 2) { die("Invalid parameters to PrecipRecord->setOccurence\n"); }
    my ($self,$occurence) = @_;
    $self->{"occurence"} = $occurence;
}

##--------------------------------------------------------------------------------
# @signature void setStationId(String station)
# <p>Set the identifer for the station where the record was taken.</p>
#
# @input $station The id for the station.
##--------------------------------------------------------------------------------
sub setStationId {
    if (@_ != 2) { die("Invalid parameters to PrecipRecord->setStationId\n"); }
    my ($self,$station) = @_;
    $self->{"stn_id"} = $station;
}

1;











