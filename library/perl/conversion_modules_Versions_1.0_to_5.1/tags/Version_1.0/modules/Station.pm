#! /usr/bin/perl

##Module--------------------------------------------------------------
# <p>This is a data structure for a Station for QCF conversions.</p>
#
# <p><b>Intended Use: </b>The intended use, as seen by the author of the
# module, is to make creating a Station for the <code>stationCD.out</code>
# file quickly with relative simplicity and flexibility.  It tries to allow
# the person writing a conversion to minimize the number of tedious details
# that are common to most conversions.</p>
#
# @use  use lib "work/DPG_HTML/BEST_SW/conversion_modules/Version1";
#       use Station;
# @author Joel Clawosn
# @version 0.01 Original Version
##Module--------------------------------------------------------------

package Station;

use strict;
use lib ".";
use Conversions;

#-------------------------------------------------------------
# This is a private function used to set the begin date.  The
# station automatically finds the start date through the
# insertDate function.
#
# Input:  $date The date to be set as the begin date.
#         $format The format of $date 
# 
# See:    Conversions::convertDate
#-------------------------------------------------------------
our $setBeginDate = sub {
    my $self = shift;
    my $date = shift;
    my $format = shift;
    my $begin = Conversions::convertDate($date, $format);
    $begin =~ s/\///g;
    $self->{"begin_date"} = $begin;
};

#-------------------------------------------------------------
# This is a private function used to set the end date.  The
# station automatically finds the end date through the
# insertDate function.
#
# Input:  $date The date to be set as the begin date.
#         $format The format of $date 
# 
# See:    Conversions::convertDate
#-------------------------------------------------------------
our $setEndDate = sub {
    my $self = shift;
    my $date = shift;
    my $format = shift;
    my $end = Conversions::convertDate($date, $format);
    $end =~ s/\///g;
    $self->{"end_date"} = $end;
};

#-------------------------------------------------------------
# This is a private function used to set the verbose flag.  It
# is used to print warnings to STDERR on calculations.
#
# Input:  $verbose A true/false value to have verbosity on/off.
#-------------------------------------------------------------
our $setVerbose = sub {
    my $self = shift;
    my $verbose = shift;
    $self->{"verbose"} = $verbose;
};

#&main();

##------------------------------------------------------------
# @signature String getBeginDate()
# <p>Get the date that the station started collecting data.</p>
#
# @output $begin_date The date the station began collecting data.
##------------------------------------------------------------
sub getBeginDate {
    my $self = shift;
    return $self->{"begin_date"};
}

##------------------------------------------------------------
# @signature String getCountry()
# <p>Get the country the station is located in.</p>
#
# @output $country The country of the station location.
##------------------------------------------------------------
sub getCountry {
    my $self = shift;
    return $self->{"country"};
}

##------------------------------------------------------------
# @signature String getCounryCode()
# <p>Get the code that represents the county of the station.</p>
# 
# @output $county Returns the code for the county of the station.
##------------------------------------------------------------
sub getCountyCode {
    my $self = shift;
    return $self->{"county_code"};
}

##------------------------------------------------------------
# @signature String getDaylightSavingsSwitch()
# <p>Get the flag that specifies if the station switches to DST.</p>
#
# @output $switch Returns the flag for DST switching.
##------------------------------------------------------------
sub getDaylightSavingsSwitch {
    my $self = shift;
    return $self->{"dst_switch"};
}

##------------------------------------------------------------
# @signature float getElevation()
# <p>Get the elevation of the station.</p>
#
# @output $elevation Returns the elevation of the station.
##------------------------------------------------------------
sub getElevation {
    my $self = shift;
    my $elev = $self->{"elevation"};
    if ($elev eq Conversions::getMissing()) {
	$elev *= 10;
    }
    return $elev;
}

##------------------------------------------------------------
# @signature String getEndDate()
# <p>Get the last date the station collects data.</p>
#
# @output $end_date Returns the last date the station collects data.
##------------------------------------------------------------
sub getEndDate {
    my $self = shift;
    return $self->{"end_date"};
}

##------------------------------------------------------------
# @signature String getIsCommissioned()
# <p>Get the flag that specifies if the station is commissioned.</p>
#
# @output $commissioned Returns the flag specifying the stations commission 
# status.
##------------------------------------------------------------
sub getIsCommissioned {
    my $self = shift;
    return $self->{"commissioned"};
}

##------------------------------------------------------------
# @signature float getLatitude()
# <p>Get the latitude of the station.</p>
#
# @output $latitude Returns the latitude of the station.
##-----------------------------------------------------------
sub getLatitude {
    my $self = shift;
    return $self->{"latitude"};
}

##-------------------------------------------------------------
# @signature int getLatLongAccuracy()
# <p>Get the accuracy of the latitude and longitude readings.</p>
#
# @output: $accuracy Returns the accuracy of the lat and long values.
##-------------------------------------------------------------
sub getLatLongAccuracy {
    my $self = shift;
    return $self->{"accuracy"};
}

##-------------------------------------------------------------
# @signature float getLongitude()
# <p>Get the longitude of the station.</p>
#
# @output $longitude Returns the longitude of the station.
##-------------------------------------------------------------
sub getLongitude {
    my $self = shift;
    return $self->{"longitude"};
}

##-------------------------------------------------------------
# @signature String getMobilityFlag()
# <p>Get the flag that specifies if the station is a mobile station
# or a fixed station.</p>
#
# @output $flag Returns the mobility flag of the station.
##--------------------------------------------------------------
sub getMobilityFlag {
    my $self = shift;
    return $self->{"mobility_flag"};
}

##--------------------------------------------------------------
# @signature int getNetworkIdNumber()
# <p>Get the network id number that the station is a part of.</p>
#
# @output $network_id Returns the network id number for the station.
##--------------------------------------------------------------
sub getNetworkIdNumber {
    my $self = shift;
    return $self->{"network_id_number"};
}

##--------------------------------------------------------------
# @signature String getNetworkName()
# <p>Get the name of the network that station is a part of.</p>
#
# @output $network_name Returns the name of the network for the station.
##--------------------------------------------------------------
sub getNetworkName {
    my $self = shift;
    return $self->{"network_name"};
}

##--------------------------------------------------------------
# @signature int getOccurence()
# <p>Get the occurence value of the station.</p>
#
# @output $occurence Returns the occurence value of the station.
##--------------------------------------------------------------
sub getOccurence {
    my $self = shift;
    return $self->{"occurence"};
}

##--------------------------------------------------------------
# @signature int getPlatformIdNumber()
# <p>Get the platform id number that the station is a part of.</p>
#
# @output $platform_id Returns the platform id number for the station.
##--------------------------------------------------------------
sub getPlatformIdNumber {
    my $self = shift;
    return $self->{"platform"};
}

##--------------------------------------------------------------
# @signature String getReportingFrequency()
# <p>Get the frequency that data is reported by the station.</p>
#
# @output $frequency Returns the station's data reporting frequency.
##--------------------------------------------------------------
sub getReportingFrequency {
    my $self = shift;
    return $self->{"frequency"};
}

##--------------------------------------------------------------
# @signature int getStateCode()
# <p>Get the state code for the state the station is located in.</p>
#
# @output: $state_code Returns the station's state code.
##--------------------------------------------------------------
sub getStateCode {
    my $self = shift;
    return $self->{"state_code"};
}

##--------------------------------------------------------------
# @signature String getStationId()
# <p>Get the identifier of the station.</p>
#
# @output $station_id Returns the station's identifier.
##--------------------------------------------------------------
sub getStationId {
    my $self = shift;
    return $self->{"station_id"};
}

##-------------------------------------------------------------
# @signature String getStationName()
# <p>Get the name/description of the station.</p>
#
# @output $station_name Returns the station's name.
##-------------------------------------------------------------
sub getStationName {
    my $self = shift;
    return $self->{"station_name"};
}

##-------------------------------------------------------------
# @signature int getUTC_Offset()
# <p>Get the number of hours the data is offset from UTC time.</p>
#
# @output $offset Returns the number of hours the readings are from UTC time.
##-------------------------------------------------------------
sub getUTC_Offset {
    my $self = shift;
    return $self->{"utc_offset"};
}

##-------------------------------------------------------------
# @signature int getVerbose()
# <p>Get the verbose flag.  It is used to print warnings to STDERR 
# on calculations.</p>
#
# @output $verbose Returns a true/false value to have verbosity on/off.
##-------------------------------------------------------------
sub getVerbose {
    my $self = shift;
    if (defined($self->{"verbose"})) {
	return $self->{"verbose"};
    } else {
	return 0;
    }
}

##-------------------------------------------------------------
# @signature void insertDate(String date)
# <p>Insert a date a reading occured for the station.</p>
# 
# @input  $date The date of the reading in YYYY/MM/DD format.
##-------------------------------------------------------------
##-------------------------------------------------------------
# @signature void insertDate(String date, String format)
# <p>Insert a date a reading occured for the station.</p>
# 
# @input  $date The date of the reading.
# @input  $format The format of the date.
# @link <a href=http://www.joss.ucar.edu/cgi-bin/dpg/doc/docperl.cgi?file=/work/DPG_HTML/BEST_SW/conversion_modules/Version1/Conversions.pm#convertDate(String date, String date_format)>Conversions::convertDate(String date, String date_format)</a>
##-------------------------------------------------------------
sub insertDate {
    my $self = shift;
    my $date = shift;
    my $format = shift;

    if (!defined($format)) { $format = "YYYY/MM/DD"; }

    my $current_date = Conversions::convertDate($date, $format);
    if (Conversions::compareDates($current_date, $self->getBeginDate()) == -1) {
	$self->$setBeginDate($current_date, "YYYY/MM/DD");
    }
    if (Conversions::compareDates($current_date, $self->getEndDate()) == 1) {
	$self->$setEndDate($current_date, "YYYY/MM/DD");
    }
}

# Just for testing.
sub main {
    my $station = Station->new();
    $station->insertDate("2003/06/25", "YYYY/MM/DD");
    $station->insertDate("2003/05/13", "YYYY/MM/DD");
    $station->setStationId("FL4");
    $station->setNetworkIdNumber(42);
    $station->setLatitude(40);
    $station->setLongitude(-100);
    $station->setStationName("Foothills Lab 4");
    $station->setIsCommissioned("n");
    $station->setCountry("US");
    $station->setStateCode("CO");
    $station->setDaylightSavingsSwitch("n");
    $station->setPlatformIdNumber(420);
    $station->setReportingFrequency("hourly");
    $station->setElevation(6000, "ft");
    $station->setMobilityFlag("f");
    print $station->toQCF_String();
}

##---------------------------------------------------------------
# @signature Station new(int verbose)
# <p>Create a new station with the default values.</p>
#
# @input  $verbose A true/false verbose flag for printing warnings.
##---------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $verbose = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self, $class);

    $self->$setVerbose($verbose);

    $self->setNetworkName("NetName");
    $self->setStationId("StnId");
    $self->setNetworkIdNumber(0);
    $self->setLatitude(-999.99999, "-DDDDDDDDD");
    $self->setLongitude(-999.99999, "-DDDDDDDDD");
    $self->setOccurence(0);
    $self->setLatLongAccuracy(4);
    $self->setStationName("Station Name");
    $self->setIsCommissioned("N");
    $self->$setBeginDate("9999/99/99", "YYYY/MM/DD");
    $self->$setEndDate("0000/00/00", "YYYY/MM/DD");
    $self->setCountry("US");
    $self->setStateCode("XX");
    $self->setCountyCode("???");
    $self->setUTC_Offset(0);
    $self->setDaylightSavingsSwitch("n");
    $self->setPlatformIdNumber(0);
    $self->setReportingFrequency("xxxxx");
    $self->setElevation(Conversions::getMissing(), "m");
    $self->setMobilityFlag("f");

    return $self;
}

##-----------------------------------------------------------------
# @signature void setCountry(String country)
# <p>Set the country the station is located in.</p>
#
# @input $country The abbreviation of the country
##-----------------------------------------------------------------
sub setCountry {
    my $self = shift;
    my $country = shift;
    $self->{"country"} = $country;
}

##-----------------------------------------------------------------
# @signature void setCountyCode(String code)
# <p>Set the county code for the station's location.</p>
#
# @input  $code The code of the county.
##-----------------------------------------------------------------
sub setCountyCode {
    my $self = shift;
    my $code = shift;
    $self->{"county_code"} = $code;
}

##-----------------------------------------------------------------
# @signature void setDaylightSavingsSwitch(String switch)
# <p>Set the flag for the station if it switches to DST or not.</p>
# <p>Legal values are "y" or "n".</p>
#
# @input  $switch A legal flag value.
# @warning This function terminates if the flag is not a 'y' or 'n'.
##-----------------------------------------------------------------
sub setDaylightSavingsSwitch {
    my $self = shift;
    my $switch = shift;
    $switch = lc($switch); # change switch to lower case
    if ($switch ne "y" && $switch ne "n") {
	die "Daylight savings switch must be an 'y' or 'n'\n";
    }
    $self->{"dst_switch"} = $switch;
}

##------------------------------------------------------------------
# @signature void setElevation(float elev, String measure)
# <p>Set the elevation for the station.</p>
#
# @input  $elev The measurement of the elevation.
# @input  $measure The units of length used to measure the elevation.
# @link <a href=http://www.joss.ucar.edu/cgi-bin/dpg/doc/docperl.cgi?file=/work/DPG_HTML/BEST_SW/conversion_modules/Version1/Conversions.pm#convertLength(float len, String in, String out)>Conversions::convertLength(float len, String in, String out)</a>
##------------------------------------------------------------------
sub setElevation {
    my $self = shift;
    my $elev = shift;
    my $measure = shift;
    $self->{"elevation"} = Conversions::convertLength($elev, $measure, "m");
}

##------------------------------------------------------------------
# @signature void setIsCommissioned(String comm)
# <p>Set the flag for the station if is is commissioned or not.</p>
# <p>Legals values are "Y" or "N".</p>
#
# @input  $comm A legal flag value
# @warning This function terminates if the flag is not a 'Y' or 'N'.
##------------------------------------------------------------------
sub setIsCommissioned {
    my $self = shift;
    my $comm = shift;
    $comm = uc($comm); # Change to upper case
    if ($comm ne "Y" && $comm ne "N") {
	die "Commissioned value must be 'Y' or 'N'\n";
    }
    $self->{"commissioned"} = "($comm)";
}

##------------------------------------------------------------------
# @signature void setLatitude(float lat, String format)
# <p>Set the latitude of the station.</p>
#
# @input  $lat The latitude of the station.
# @input  $format The format of $lat using "-DMS" characters
# @link <a href=http://www.joss.ucar.edu/cgi-bin/dpg/doc/docperl.cgi?file=/work/DPG_HTML/BEST_SW/conversion_modules/Version1/Conversions.pm#convertLatLong(String value, String format)>Conversions::convertLatLong(String value, String format)</a>
##------------------------------------------------------------------
sub setLatitude {
    my $self = shift;
    my $lat = shift;
    my $format = shift;
    $self->{"latitude"} = Conversions::convertLatLong($lat, $format);
}

##-----------------------------------------------------------------
# @signature void setLatLongAccuracy(int acc)
# ,p>Set the accuracy of the lat and long measurements.</p>
#
# @input  $acc The accuracy of the measurements.
##-----------------------------------------------------------------
sub setLatLongAccuracy {
    my $self = shift;
    my $acc = shift;
    $self->{"accuracy"} = $acc;
}

##------------------------------------------------------------------
# @signature void setLongitude(float long, String format)
# <p>Set the longitude of the station.</p>
#
# @input  $long The longitude of the station.
# @input  $format The format of $long using "-DMS" characters
# @link <a href=http://www.joss.ucar.edu/cgi-bin/dpg/doc/docperl.cgi?file=/work/DPG_HTML/BEST_SW/conversion_modules/Version1/Conversions.pm#convertLatLong(String value, String format)>Conversions::convertLatLong(String value, String format)</a>
##------------------------------------------------------------------
sub setLongitude {
    my $self = shift;
    my $long = shift;
    my $format = shift;
    $self->{"longitude"} = Conversions::convertLatLong($long, $format);
}

##------------------------------------------------------------------
# @signature void setMobilityFlag(String flag)
# <p>Set the flag if the station is a mobile station of a fixed one.</p>
# <p>Legal values are "f" or "m"</p>
# 
# @input $flag A legal flag value.
# @warning This function terminates if the specified flag is not an 'f' or 'm'.
##------------------------------------------------------------------
sub setMobilityFlag {
    my $self = shift;
    my $flag = shift;
    $flag = lc($flag);
    if ($flag ne "f" && $flag ne "m") {
	die "Mobility flag must be 'f' or 'm'\n";
    }
    $self->{"mobility_flag"} = $flag;
}

##------------------------------------------------------------------
# @signature void setNetworkIdNumber(int id)
# <p>Set the network id number that the station is a part of.</p>
#
# @input $id The network id number of the station's network.
##------------------------------------------------------------------
sub setNetworkIdNumber {
    my $self = shift;
    my $id = shift;
    $self->{"network_id_number"} = $id;
}

##-----------------------------------------------------------------
# @signature void setNetworkName(String name)
# <p>Set the name of the network that the station is a part of.</p>
#
# @input $name The name of the network.
##-----------------------------------------------------------------
sub setNetworkName {
    my $self = shift;
    my $name = shift;
    $self->{"network_name"} = $name;
}

##-----------------------------------------------------------------
# @signature void setOccurence(int occ)
# <p>Set the occurence value of the station.  This should be set to 0 in
# almost every case.</p>
#
# @input $occ The occurence value of the station.
##-----------------------------------------------------------------
sub setOccurence {
    my $self = shift;
    my $occ = shift;
    $self->{"occurence"} = $occ;
}

##-----------------------------------------------------------------
# @signature void setPlatformIdNumber(int id)
# <p>Set the platform id number the station is a part of.</p>
#
# @input  $id The platform id number of the station's platform.
##-----------------------------------------------------------------
sub setPlatformIdNumber {
    my $self = shift;
    my $id = shift;
    $self->{"platform"} = $id;
}

##-----------------------------------------------------------------
# @signature void setReportingFrequecy(String freq)
# <p>Set the frequency which the station reports data.</p>
#
# @input $freq The frequency the station reports data.
##-----------------------------------------------------------------
sub setReportingFrequency {
    my $self = shift;
    my $freq = shift;
    $self->{"frequency"} = $freq;
}

##----------------------------------------------------------------
# @signature void setStateCode(String state)
# <p>Set the state code for the station's location.</p>
#
# @input $state The state's abbreviation, "XX" if unknown
# @link <a href=http://www.joss.ucar.edu/cgi-bin/dpg/doc/docperl.cgi?file=/work/DPG_HTML/BEST_SW/conversion_modules/Version1/Conversions.pm#getStateCode(String state)>Conversions::getStateCode(String state)</a>
##----------------------------------------------------------------
sub setStateCode {
    my $self = shift;
    my $state = shift;
    $self->{"state_code"} = Conversions::getStateCode($state);
}

##----------------------------------------------------------------
# @signature void setStationId(String id)
# <p>Set the station's identifier.</p>
#
# @input  $id The station's identifier.
##----------------------------------------------------------------
sub setStationId {
    my $self = shift;
    my $id = shift;
    $self->{"station_id"} = $id;
}

##----------------------------------------------------------------
# @signature void setStationName(String name)
# <p>Set the name/description of the station.</p>
# 
# @input $name The name of the station.
# @warning This function will die if the name is longer than 46 characters.
##----------------------------------------------------------------
sub setStationName {
    my $self = shift;
    my $name = shift;
    if (length($name) > 46) {
	printf("Name of %s is longer that 46 characters for station %s\n", 
	       $name, $self->getStationId());
	die();
    }
    $self->{"station_name"} = $name;
}

##----------------------------------------------------------------
# @signature void setUTC_Offset(int offset)
# <p>Set the number of hours the readings are from UTC time.  This should
# be set to 0 in almost every case.</p>
#
# @input $offset The number of hours off of UTC time of the readings.
##-----------------------------------------------------------------
sub setUTC_Offset {
    my $self = shift;
    my $offset = shift;
    $self->{"utc_offset"} = $offset;
}

##------------------------------------------------------------------
# @signature String toQCF_String()
# <p>Create a QCF station string from the data in the station.</p>
#
# @output Returns a string of the station's data in QCF format.
##------------------------------------------------------------------
sub toQCF_String {
    my $self = shift;
    my $format = "%-15s %4d %10.5f %11.5f %3d %5d %-46s %-3s %-8s %-8s %-2s %-2s %-3s %6.2f %-1s %4d %-15s %9.1f %1s\n";

    return sprintf($format,
		   $self->getStationId(),
		   $self->getNetworkIdNumber(),
		   $self->getLatitude(),
		   $self->getLongitude(),
		   $self->getOccurence(),
		   $self->getLatLongAccuracy(),
		   $self->getStationName(),
		   $self->getIsCommissioned(),
		   $self->getBeginDate(),
		   $self->getEndDate(),
		   $self->getCountry(),
		   $self->getStateCode(),
		   $self->getCountyCode(),
		   $self->getUTC_Offset(),
		   $self->getDaylightSavingsSwitch(),
		   $self->getPlatformIdNumber(),
		   $self->getReportingFrequency(),
		   $self->getElevation(),
		   $self->getMobilityFlag());
}










