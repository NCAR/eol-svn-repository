#! /usr/bin/perl -w

##Module--------------------------------------------------------------------------
# <p>The Precip15minRecord is a PrecipRecord that contains 15 minute precipitation
# data for a day as defined for the Precipitation Quality Control Format defined
# by UCAR/JOSS.</p>
#
# @since Version 3.
#
# @author Joel Clawson
# @version 4.0 The creation of the class.
##Module--------------------------------------------------------------------------
package Precip15minRecord;
use strict;
use lib "/net/work/lib/perl/Utilities";
use DpgDate qw(:DEFAULT);
use DpgConversions qw(:DEFAULT);
use PrecipRecord;
use QCFConstants qw(:DEFAULT);
our @ISA = ("PrecipRecord");

##--------------------------------------------------------------------------------
# @signature String getDate()
# <p>Get the date of the data in the record.</p>
#
# @output $date The date of the data in YYYY/MM/DD format.
##--------------------------------------------------------------------------------
sub getDate {
    if (@_ != 1) { die("Invalid parameters to Precip15minRecord->getDate\n"); }
    my ($self) = @_;
    return $self->checkDefined($self->{"date"},"9999/99/99");
}

##--------------------------------------------------------------------------------
# @signature float getPrecip(String time, String fmt)
# <p>Get the precipitation value for the specified time.</p>
#
# @input $time The time of the precipitation to be retreived.
# @input $fmt The format of the time parameter.
# @output $value The precipitation amount in millimeters.  Default MISSING.
##--------------------------------------------------------------------------------
sub getPrecip {
    if (@_ != 3) { die("Invalid parameters to Precip15minRecord->getPrecip\n"); }
    my ($self,$time,$fmt) = @_;
    return $self->checkDefined($self->{formatTime($time,$fmt,"HH:MM")}->{"precip"},$MISSING);
}

##--------------------------------------------------------------------------------
# @signature int getPrecipFlag(String time, String fmt)
# <p>Get the precipitation flag that is associated with a value for the specified
# time.</p>
#
# @input $time The time of the precipitation flat to be retreived.
# @input $fmt The format of the time parameter.
# @output $flat The precipitation flag at the specified time.  Default 
# PRECIP_MISSING_CODE.
##--------------------------------------------------------------------------------
sub getPrecipFlag {
    if (@_ != 3) { die("Invalid parameters to Precip15minRecord->getPrecipFlag\n"); }
    my ($self,$time,$fmt) = @_;
    return $self->checkDefined($self->{formatTime($time,$fmt,"HH:MM")}->{"pcp_flag"},
			       $self->getPrecip($time,$fmt) == $MISSING ? 
			       $PRECIP_MISSING_CODE : $PRECIP_NO_QUALIFIER_CODE);
}

##--------------------------------------------------------------------------------
# @signature Strin getQCFFlag(String time, String fmt)
# <p>Get the QCF flag that is associated with a precipitation value.</p>
#
# @input $time The time of the QCF flag to be retreived.
# @input $fmt The format of the time parameter.
# @output $flag The QCF flag at the specified time.  Default: MISSING_FLAG
##--------------------------------------------------------------------------------
sub getQCFFlag {
    if (@_ != 3) { die("Invalid parameters to Precip15minRecord->getQCFFlag\n"); }
    my ($self,$time,$fmt) = @_;
    return $self->checkDefined($self->{formatTime($time,$fmt,"HH:MM")}->{"qcf_flag"},
			       $self->getPrecip($time,$fmt) == $MISSING ? $MISSING_FLAG : 
			       $UNCHECKED_FLAG);
}

##--------------------------------------------------------------------------------
# @signature String getTime()
# <p>Get the metadata time for the record.</p>
#
# @output $time The time in HH:MM:SS format.
##--------------------------------------------------------------------------------
sub getTime {
    if (@_ != 1) { die("Invalid parameters to Precip15minRecord->getTime\n"); }
    my ($self) = @_;
    return $self->checkDefined($self->{"time"},"00:00:00");
}

##--------------------------------------------------------------------------------
# @signature int isNominalTime(String time, String format)
# <p>Determine if the specified time is a nominal 15 minute time.</p>
#
# @input $time The time to be checked.
# @input $fmt The format of the time.
# @output $pass A boolean value if the time is nominal.
##--------------------------------------------------------------------------------
sub isNominalTime {
    if (@_ != 3) { die("Invalid parameters to Precip15minRecord->isNominal\n"); }
    my ($self,$time,$fmt) = @_;
    my ($hour,$min) = split(/:/,formatTime($time,$fmt,"HH:MM"));
    return ($min % 15 == 0);
}

##--------------------------------------------------------------------------------
# @signature void setReadingTime(String date, String fmt, --int hour_off--, --int min_off--, --int sec_off--)
# <p>Set the reading time of the record.</p>
#
# @input $date The date for the metadata.
# @input $fmt The format of the date.
# @input $hour_off <b>Optional</b>  The number of hours offset from the 0 hour 
# for the time.
# @input $min_off <b>Optional</b> The number of minutes offset from the 0 hour
# for the time.
# @input $sec_off <b>Optional</b> The number of seconds offset from the 0 hour
# for the time.
##--------------------------------------------------------------------------------
sub setReadingTime {
    if (@_ != 3 && @_ != 6) { die("Invalid Parameters to Precip15minRecord->setReadingTime\n"); }
    my ($self,$date,$fmt,$hour_off,$min_off,$sec_off) = (@_,0,0,0,0);
    ($date,$self->{"time"}) = adjustDateTime($date,$fmt,"00:00:00","HH:MM:SS",0,
				   $hour_off,$min_off,$sec_off);
    $self->{"date"} = formatDate($date,$fmt,"YYYY/MM/DD");
}

##--------------------------------------------------------------------------------
# @signature void setPrecip(String time, String fmt, float precip, String unit)
# <p>Set the precipitation value for a specified time.</p>
#
# @input $time The time of the value to be set.
# @input $fmt The format for the time parameter.
# @input $precip The amount of precipitation that fell during the time.
# @input $unit The unit of length for the precipitation.
# @warning The value will not be set if the time is not a nominal 15 minute time.
# A warning will be generated.
##--------------------------------------------------------------------------------
sub setPrecip {
    if (@_ != 5) { die("Invalid parameters to Precip15minRecord->setPrecip\n"); }
    my ($self,$time,$fmt,$precip,$unit) = @_;
    
    if ($self->isNominalTime($time,$fmt)) {
	$self->{formatTime($time,$fmt,"HH:MM")}->{"precip"} = convertLength($precip,$unit,"mm");
    } else {
	printf({$self->{"warn"}} "Precip %s for time %s %s cannot be set.  Time is not a nominal 15 minutes.\n",$precip,$self->getDate(),$time);
    }
}

##--------------------------------------------------------------------------------
# @signature void setPrecipFlag(String time, String fmt, int flag)
# <p>Set the precipitation flag for the value at the specified time.</p>
#
# @input $time The time of the flag to be set.
# @input $fmt The format for the time parameter.
# @input $flag The precipitation flag to be set.
# @warning The flag will not be set if the time is not a nominal 15 minute time
# or if the flag is not a valid precipitation flag.  A warning will be generated.
##--------------------------------------------------------------------------------
sub setPrecipFlag {
    if (@_ != 4) { die("Invalid parameters to Precip15minRecord->setPrecip\n"); }
    my ($self,$time,$fmt,$flag) = @_;

    if (!$self->isNominalTime($time,$fmt)) {
	printf({ $self->{"warn"}} "Precip flag %s for time %s %s cannot be set.  Time is not a nominal 15 minutes.\n",$flag,$self->getDate(),$time);
    } elsif (!$self->isValidPrecipFlag($flag)) {
	printf({ $self->{"warn"}} "Precip flag %s for time %s %s cannto be set.  The flag is not a valid precip flag.\n",$flag,$self->getDate(),$time);
    } else {
	$self->{formatTime($time,$fmt,"HH:MM")}->{"pcp_flag"} = $flag;
    }
}

##--------------------------------------------------------------------------------
# @signature void setQCFFlag(String time, String fmt, String flag)
# <p>Set the QCF flag for the value at the specified time.</p>
#
# @input $time The time of the flag to be set.
# @input $fmt The format for the time parameter.
# @input $flag The QCF flag to be set.
# @warning The flag will not be set if the time is not a nominal 15 minute time
# or if the flag is not a valid QCF flag.  A warning will be generated.
##--------------------------------------------------------------------------------
sub setQCFFlag {
    if (@_ != 4) { die("Invalid parameters to Precip15minRecord->setPrecip\n"); }
    my ($self,$time,$fmt,$flag) = @_;

    if (!$self->isNominalTime($time,$fmt)) {
	printf({$self->{"warn"}} "QCF Flag %s for time %s %s cannot be set.  Time is not a nomianl 15 minutes.\n",$flag,$self->getDate(),$time);
    } elsif (!$self->isValidQCFFlag($flag)) {
	printf({$self->{"warn"}} "QCF Flag %s for time %s %s cannot be set.  The flag is not a valid QCF flag.\n",$flag,$self->getDate(),$time);
    } else {
	$self->{formatTime($time,$fmt,"HH:MM")}->{"qcf_flag"} = $flag;
    }
}

##--------------------------------------------------------------------------------
# @signature String toString()
# <p>Create the String PQCF record line for the record.</p>
#
# @output $str The PQCF formatted string for the record.
##--------------------------------------------------------------------------------
sub toString {
    if (@_ != 1) { die("Invalid parameters to Precip15minRecord->toString\n"); }
    my ($self) = @_;

    my $buffer = sprintf("%10s %8s %-10s %-15s %10.5f %11.5f %3d",
			 $self->getDate(),$self->getTime(),$self->getNetworkId(),
			 $self->getStationId(),$self->getLatitude(),$self->getLongitude(),
			 $self->getOccurence());

    # Need to reset the date if it hasn't been set to prevent an error in 
    # adjustDateTime
    my $date = $self->getDate() eq "9999/99/99" ? "0000/01/01" : $self->getDate();
    my $time = "00:00";

    while ($date eq $self->getDate() || $date eq "0000/01/01") {
	$buffer .= sprintf(" %7.2f %d %s",
			  $self->getPrecip($time,"HH:MM"),
			  $self->getPrecipFlag($time,"HH:MM"),
			  $self->getQCFFlag($time,"HH:MM"));
	($date,$time) = adjustDateTime($date,"YYYY/MM/DD",$time,"HH:MM",0,0,15,0);
    }

    return sprintf("%s\n",$buffer);
}

1;

