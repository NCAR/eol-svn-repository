#! /usr/bin/perl -w

##Module--------------------------------------------------------------------------
# <p>The PrecipDailyRecord is a PrecipRecord that contains daily precipitation
# data for a month as defined for the Precipitation Quality Control Format defined
# by UCAR/JOSS.</p>
#
# @since Version 3.
#
# @author Joel Clawson
# @version 4.0 The creation of the class.
##Module--------------------------------------------------------------------------
package Precip::PrecipDailyRecord;
use strict;
use lib "..";
use DpgDate qw(:DEFAULT);
use DpgConversions qw(:DEFAULT);
use Precip::PrecipRecord;
use Surface::QCFConstants qw(:DEFAULT);
our @ISA = ("Precip::PrecipRecord");

##--------------------------------------------------------------------------------
# @signature String getDate()
# <p>Get the date of the data in the record.</p>
#
# @output $date The date of the data in YYYY/MM/DD format.
##--------------------------------------------------------------------------------
sub getDate {
    if (@_ != 1) { die("Invalid parameters to PrecipDailyRecord->getDate\n"); }
    my ($self) = @_;
    return $self->checkDefined($self->{"date"},"9999/99");
}

##--------------------------------------------------------------------------------
##--------------------------------------------------------------------------------
sub getObservationHour {
    if (@_ != 2) { die("Invalid parameters to PrecipDailyRecord->getObservationHour\n"); }
    my ($self,$day) = @_;
    return $self->checkDefined($self->{sprintf("%02d",$day)}->{"hour"},99);
}

##--------------------------------------------------------------------------------
# @signature float getPrecip(int day)
# <p>Get the precipitation value for the specified day.</p>
#
# @input $day The day of the precipitation value to be retreived.
# @output $value The precipitation amount in millimeters.  Default MISSING.
##--------------------------------------------------------------------------------
sub getPrecip {
    if (@_ != 2) { die("Invalid parameters to PrecipDailyRecord->getPrecip\n"); }
    my ($self,$day) = @_;
    return $self->checkDefined($self->{sprintf("%02d",$day)}->{"precip"},$MISSING);
}

##--------------------------------------------------------------------------------
# @signature int getPrecipFlag(int day)
# <p>Get the precipitation flag that is associated with a value for the specified
# day.</p>
#
# @input $day The day of the precipitation flag to be retreived.
# @output $flat The precipitation flag at the specified day.  Default 
# PRECIP_MISSING_CODE.
##--------------------------------------------------------------------------------
sub getPrecipFlag {
    if (@_ != 2) { die("Invalid parameters to PrecipDailyRecord->getPrecipFlag\n"); }
    my ($self,$day) = @_;
    return $self->checkDefined($self->{sprintf("%02d",$day)}->{"pcp_flag"},
			       $self->getPrecip($day) == $MISSING ? 
			       $PRECIP_MISSING_CODE : $PRECIP_NO_QUALIFIER_CODE);
}

##--------------------------------------------------------------------------------
# @signature String getQCFFlag(int day)
# <p>Get the QCF flag that is associated with a precipitation day.</p>
#
# @input $day The day of the QCF flag to be retreived.
# @output $flag The QCF flag at the specified time.  Default: MISSING_FLAG
##--------------------------------------------------------------------------------
sub getQCFFlag {
    if (@_ != 2) { die("Invalid parameters to PrecipDailyRecord->getQCFFlag\n"); }
    my ($self,$day) = @_;
    return $self->checkDefined($self->{sprintf("%02d",$day)}->{"qcf_flag"},
			       $self->getPrecip($day) == $MISSING ? $MISSING_FLAG : 
			       $UNCHECKED_FLAG);
}

##--------------------------------------------------------------------------------
# @signature void setDate(String date, String fmt)
# <p>Set the date of the record.</p>
#
# @input $date The date for the metadata.
# @input $fmt The format of the date.
##--------------------------------------------------------------------------------
sub setDate {
    if (@_ != 3) { die("Invalid Parameters to PrecipDailyRecord->setDate\n"); }
    my ($self,$date,$fmt) = @_;
    $self->{"date"} = formatDate($date,$fmt,"YYYY/MM");
}

##--------------------------------------------------------------------------------
# @signature void setPrecip(int day, int hour, float precip, String unit)
# <p>Set the precipitation value for a specified day.</p>
#
# @input $day The day the precipitation is to be set.
# @input $hour The observation hour of the reading.
# @input $precip The amount of precipitation that fell during the time.
# @input $unit The unit of length for the precipitation.
##--------------------------------------------------------------------------------
sub setPrecip {
    if (@_ != 5) { die("Invalid parameters to PrecipDailyRecord->setPrecip\n"); }
    my ($self,$day,$hour,$precip,$unit) = @_;
    $self->{sprintf("%02d",$day)}->{"precip"} = convertLength($precip,$unit,"mm");
    $self->{sprintf("%02d",$day)}->{"hour"} = $hour;
}

##--------------------------------------------------------------------------------
# @signature void setPrecipFlag(int day, int flag)
# <p>Set the precipitation flag for the value at the specified day.</p>
#
# @input $day The day the flag is to be set.
# @input $flag The precipitation flag to be set.
# @warning The flag will not be set if the flag is not a valid precipitation flag.  
# A warning will be generated.
##--------------------------------------------------------------------------------
sub setPrecipFlag {
    if (@_ != 3) { die("Invalid parameters to PrecipDailyRecord->setPrecip\n"); }
    my ($self,$day,$flag) = @_;

    if (!$self->isValidPrecipFlag($flag)) {
	printf({ $self->{"warn"}} "Precip flag %s for day %s/%s cannot be set.  The flag is not a valid precip flag.\n",$flag,$self->getDate(),$day);
    } else {
	$self->{sprintf("%02d",$day)}->{"pcp_flag"} = $flag;
    }
}

##--------------------------------------------------------------------------------
# @signature void setQCFFlag(int day, String flag)
# <p>Set the QCF flag for the value at the specified day.</p>
#
# @input $day The day the flag is to be set.
# @input $flag The QCF flag to be set.
# @warning The flag will not be set if the flag is not a valid QCF flag.  A warning 
# will be generated.
##--------------------------------------------------------------------------------
sub setQCFFlag {
    if (@_ != 3) { die("Invalid parameters to PrecipDailyRecord->setPrecip\n"); }
    my ($self,$day,$flag) = @_;

    if (!$self->isValidQCFFlag($flag)) {
	printf({$self->{"warn"}} "QCF Flag %s for time %s/%02d cannot be set.  The flag is not a valid QCF flag.\n",$flag,$self->getDate(),$day);
    } else {
	$self->{sprintf("%02d",$day)}->{"qcf_flag"} = $flag;
    }
}

##--------------------------------------------------------------------------------
# @signature String toString()
# <p>Create the String PQCF record line for the record.</p>
#
# @output $str The PQCF formatted string for the record.
##--------------------------------------------------------------------------------
sub toString {
    if (@_ != 1) { die("Invalid parameters to PrecipDailyRecord->toString\n"); }
    my ($self) = @_;

    my $buffer = sprintf("%7s %-10s %-15s %10.5f %11.5f %3d",
			 $self->getDate(),$self->getNetworkId(),$self->getStationId(),
			 $self->getLatitude(),$self->getLongitude(),$self->getOccurence());

    for (my $i = 1; $i <= 31; $i++) {
	$buffer .= sprintf(" %7.2f %d %s %2d",
			   $self->getPrecip($i),$self->getPrecipFlag($i),
			   $self->getQCFFlag($i),$self->getObservationHour($i));
    }

    return sprintf("%s\n",$buffer);
}

1;

