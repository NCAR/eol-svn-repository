#! /usr/bin/perl -w

##Module--------------------------------------------------------------
# <p>This is a data structure for a Station for QCF conversions.</p>
#
# <p><b>Intended Use: </b>The intended use, as seen by the author of the
# module, is to make creating a Station for the <code>stationCD.out</code>
# file quickly with relative simplicity and flexibility.  It tries to allow
# the person writing a conversion to minimize the number of tedious details
# that are common to most conversions.</p>
#
# @use  use lib "work/DPG_HTML/BEST_SW/conversion_modules/Version2";
#       use Station;
# @author Joel Clawson
# @version 1.00 Original Version
# @author Joel Clawson
# @version 2.00 Code was cleaned up and set up the default values in a
# better fashion.
##Module--------------------------------------------------------------
package Station;

use strict;
use lib ".";
use Conversions;

#-------------------------------------------------------------------
# This is a private function used to check to see if a value has
# been defined in the station.
#
# Input: $value The value to be checked.
# Input: $default The default value to return if the value is not defined.
# Output: The value if defined, otherwise the default value.
#-------------------------------------------------------------------
our $checkDefined = sub {
  my $self = shift;
  if (defined($_[0])) { return $_[0]; } else { return $_[1]; }
};

#-------------------------------------------------------------
# This is a private function used to set the verbose flag.  It
# is used to print warnings to STDERR on calculations.
#
# Input:  $verbose A true/false value to have verbosity on/off.
#-------------------------------------------------------------
our $setVerbose = sub {
    my $self = shift;
    $self->{"verbose"} = shift;
};

##------------------------------------------------------------
# @signature String getBeginDate()
# <p>Get the date that the station started collecting data.</p>
#
# @output $begin_date The date the station began collecting data.
##------------------------------------------------------------
sub getBeginDate {
    my $self = shift;
    return $self->$checkDefined($self->{"begin_date"}, "99999999");
}

##------------------------------------------------------------
# @signature String getCountry()
# <p>Get the country the station is located in.</p>
#
# @output $country The country of the station location.
##------------------------------------------------------------
sub getCountry {
    my $self = shift;
    return $self->$checkDefined($self->{"country"}, "US");
}

##-----------------------------------------------------------------------
# @signature String getCSEId()
# <p>Get the CSE for the Record.</p>
#
# @output $cse_id The identifier for the CSE, "CSE" default.
##-----------------------------------------------------------------------
sub getCSEId {
    my $self = shift;
    $self->checkDefined($self->{"cse_id"}, "CSE");
}

##------------------------------------------------------------
# @signature String getCounryCode()
# <p>Get the code that represents the county of the station.</p>
# 
# @output $county Returns the code for the county of the station.
##------------------------------------------------------------
sub getCountyCode {
    my $self = shift;
    return $self->$checkDefined($self->{"county_code"}, "???");
}

##------------------------------------------------------------
# @signature String getDaylightSavingsSwitch()
# <p>Get the flag that specifies if the station switches to DST.</p>
#
# @output $switch Returns the flag for DST switching.
##------------------------------------------------------------
sub getDaylightSavingsSwitch {
    my $self = shift;
    return $self->$checkDefined($self->{"dst_switch"}, "n");
}

##------------------------------------------------------------
# @signature float getElevation()
# <p>Get the elevation of the station.</p>
#
# @output $elevation Returns the elevation of the station.
##------------------------------------------------------------
sub getElevation {
    my $self = shift;
    my $elev = $self->$checkDefined($self->{"elevation"}, -9999.9);
    if ($elev eq Conversions::getMissing()) { $elev *= 10; }
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
    return $self->$checkDefined($self->{"end_date"}, "00000000");
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
    return $self->$checkDefined($self->{"commissioned"}, "(N)");
}

##------------------------------------------------------------
# @signature float getLatitude()
# <p>Get the latitude of the station.</p>
#
# @output $latitude Returns the latitude of the station.
##-----------------------------------------------------------
sub getLatitude {
    my $self = shift;
    return $self->$checkDefined($self->{"latitude"}, "-999.99999");
}

##-------------------------------------------------------------
# @signature int getLatLongAccuracy()
# <p>Get the accuracy of the latitude and longitude readings.</p>
#
# @output: $accuracy Returns the accuracy of the lat and long values.
##-------------------------------------------------------------
sub getLatLongAccuracy {
    my $self = shift;
    return $self->$checkDefined($self->{"accuracy"}, 1);
}

##-------------------------------------------------------------
# @signature float getLongitude()
# <p>Get the longitude of the station.</p>
#
# @output $longitude Returns the longitude of the station.
##-------------------------------------------------------------
sub getLongitude {
    my $self = shift;
    return $self->$checkDefined($self->{"longitude"}, "-999.99999");
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
    return $self->$checkDefined($self->{"mobility_flag"}, "f");
}

##--------------------------------------------------------------
# @signature int getNetworkIdNumber()
# <p>Get the network id number that the station is a part of.</p>
#
# @output $network_id Returns the network id number for the station.
##--------------------------------------------------------------
sub getNetworkIdNumber {
    my $self = shift;
    return $self->$checkDefined($self->{"network_id_number"}, -1);
}

##--------------------------------------------------------------
# @signature String getNetworkName()
# <p>Get the name of the network that station is a part of.</p>
#
# @output $network_name Returns the name of the network for the station.
##--------------------------------------------------------------
sub getNetworkName {
    my $self = shift;
    return $self->$checkDefined($self->{"network_name"}, "Network");
}

##--------------------------------------------------------------
# @signature int getOccurence()
# <p>Get the occurence value of the station.</p>
#
# @output $occurence Returns the occurence value of the station.
##--------------------------------------------------------------
sub getOccurence {
    my $self = shift;
    return $self->$checkDefined($self->{"occurence"}, 0);
}

##--------------------------------------------------------------
# @signature int getPlatformIdNumber()
# <p>Get the platform id number that the station is a part of.</p>
#
# @output $platform_id Returns the platform id number for the station.
##--------------------------------------------------------------
sub getPlatformIdNumber {
    my $self = shift;
    return $self->$checkDefined($self->{"platform"}, -1);
}

##-----------------------------------------------------------------------
# @signature String getReferenceSiteId()
# <p>Get the identifier for the reference site.
#
# @output $ref_site_id The identifier for the reference site, "Ref_Site"
# default.
##-----------------------------------------------------------------------
sub getReferenceSiteId {
    my $self = shift;
    $self->checkDefined($self->{"ref_site_id"}, "Ref_Site");
}

##--------------------------------------------------------------
# @signature String getReportingFrequency()
# <p>Get the frequency that data is reported by the station.</p>
#
# @output $frequency Returns the station's data reporting frequency.
##--------------------------------------------------------------
sub getReportingFrequency {
    my $self = shift;
    return $self->$checkDefined($self->{"frequency"}, "xxxxxx");
}

##--------------------------------------------------------------
# @signature int getStateCode()
# <p>Get the state code for the state the station is located in.</p>
#
# @output: $state_code Returns the station's state code.
##--------------------------------------------------------------
sub getStateCode {
    my $self = shift;
    return $self->$checkDefined($self->{"state_code"}, 99);
}

##--------------------------------------------------------------
# @signature String getStationId()
# <p>Get the identifier of the station.</p>
#
# @output $station_id Returns the station's identifier.
##--------------------------------------------------------------
sub getStationId {
    my $self = shift;
    return $self->$checkDefined($self->{"station_id"}, "Station");
}

##-------------------------------------------------------------
# @signature String getStationName()
# <p>Get the name/description of the station.</p>
#
# @output $station_name Returns the station's name.
##-------------------------------------------------------------
sub getStationName {
    my $self = shift;
    return $self->$checkDefined($self->{"station_name"}, "Description");
}

##-------------------------------------------------------------
# @signature int getUTC_Offset()
# <p>Get the number of hours the data is offset from UTC time.</p>
#
# @output $offset Returns the number of hours the readings are from UTC time.
##-------------------------------------------------------------
sub getUTC_Offset {
    my $self = shift;
    return $self->$checkDefined($self->{"utc_offset"}, 0);
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
    return $self->$checkDefined($self->{"verbose"}, 1);
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
# @link <a href=http://www.joss.ucar.edu/cgi-bin/dpg/doc/docperl.cgi?file=/work/DPG_HTML/BEST_SW/conversion_modules/Version2/Conversions.pm#formatDate(String date, String date_format)>Conversions::formatDate(String date, String date_format)</a>
##-------------------------------------------------------------
sub insertDate {
    my $self = shift;
    my $date = $_[0];
    my $format = $_[1];
    
    if (!defined($format)) { $format = "YYYY/MM/DD"; }
    
    my $current_date = Conversions::formatDate($date, $format);
    my $begin_date = Conversions::formatDate($self->getBeginDate(),"YYYYMMDD");
    if (Conversions::compareDates($current_date, $begin_date) == -1)
      {
	$self->setBeginDate($current_date, "YYYY/MM/DD");
      }
    my $end_date = Conversions::formatDate($self->getEndDate(), "YYYYMMDD");
    if (Conversions::compareDates($current_date, $end_date) == 1) {
	$self->setEndDate($current_date, "YYYY/MM/DD");
    }
}

##---------------------------------------------------------------
# @signature Station new()
# <p>Create a new <code>Station</code>.</p>
##---------------------------------------------------------------
##---------------------------------------------------------------
# @signature Station new(int verbose)
# <p>Create a new station with the default values.</p>
#
# @input  $verbose A true/false verbose flag for printing warnings.
##---------------------------------------------------------------
##---------------------------------------------------------------
# @signature Station new(String station_id, String network_name)
# <p>Create a new <code>Station</code> for the specified id and network.</p>
#
# @input $station_id The unique identifer for the station.
# @input $network_name The name of the network the station belongs to.
##---------------------------------------------------------------
##---------------------------------------------------------------
# @signature Station new(String station_id, String network_name, int verbose)
# <p>Create a new <code>Station</code> for the specified id and network.</p>
#
# @input $station_id The unique identifer for the station.
# @input $network_name The name of the network the station belongs to.
# @input $verbose A true/false verbose flag for printing warnings.
##---------------------------------------------------------------
##---------------------------------------------------------------
# @signature Station new(String station_id, String ref_id, String cse_id)
# <p>Create a new <code>Station</code> for the specified id and network.</p>
#
# @input $station_id The unique identifer for the station.
# @input $ref_id The unique identifier for the reference site.
# @input $cse_id The unique identifier for the CSE.
##---------------------------------------------------------------
##---------------------------------------------------------------
# @signature Station new(String station_id, String ref_id, String cse_id, int verbose)
# <p>Create a new <code>Station</code> for the specified id and network.</p>
#
# @input $station_id The unique identifer for the station.
# @input $ref_id The unique identifier for the reference site.
# @input $cse_id The unique identifier for the CSE.
# @input  $verbose A true/false verbose flag for printing warnings.
##---------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self, $class);
    
    if (scalar(@_) == 0) { $self->$setVerbose(1); }
    elsif (scalar(@_) == 1) { $self->$setVerbose($_[0]); }
    elsif (scalar(@_) == 2) {
	$self->$setVerbose(1);
	$self->setStationId($_[0]);
	$self->setNetworkName($_[1]);
    } elsif (scalar(@_) == 3 && $_[2] =~ /^(0|1)$/) {
	$self->setStationId($_[0]);
	$self->setNetworkName($_[1]);
	$self->$setVerbose($_[2]);
    } elsif (scalar(@_) == 3) {
	$self->$setVerbose(1);
	$self->setStationId($_[0]);
	$self->setReferenceSite($_[1]);
	$self->setCSEId($_[2]);
    } elsif (scalar(@_) == 4) {
	$self->setStationId($_[0]);
	$self->setReferenceSiteId($_[1]);
	$self->setCSEId($_[2]);
	$self->$setVerbose($_[4]);
    }

    
  return $self;
}

##-------------------------------------------------------------
# @signature void setBeginDate(String date, String format)
# <p>Set the begin date for the <code>Station</code>.  In most cases, 
# you should use the insertDate function instead.</p>
#
# @input $date The date to be set as the begin date.
# @input $format The format of the date.
##-------------------------------------------------------------
sub setBeginDate {
    my $self = shift;
    my $begin = Conversions::formatDate($_[0], $_[1]);
    $begin =~ s/\///g;
    $self->{"begin_date"} = $begin;
}

##-----------------------------------------------------------------
# @signature void setCountry(String country)
# <p>Set the country the station is located in.</p>
#
# @input $country The abbreviation of the country
##-----------------------------------------------------------------
sub setCountry {
    my $self = shift;
     $self->{"country"} = $_[0];
}

##-----------------------------------------------------------------------
# @signature void setCSEId(String cse_id)
# <p>Set the identifier for the CSE of the Record.</p>
#
# @input $cse_id The CSE's identifier.
##-----------------------------------------------------------------------
sub setCSEId {
    my $self = shift;
    $self->{"cse_id"} = $_[0];
}

##-----------------------------------------------------------------
# @signature void setCountyCode(String code)
# <p>Set the county code for the station's location.</p>
#
# @input  $code The code of the county.
##-----------------------------------------------------------------
sub setCountyCode {
    my $self = shift;
    $self->{"county_code"} = $_[0];
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
    my $switch = $_[0];
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
# @link <a href=http://www.joss.ucar.edu/cgi-bin/dpg/doc/docperl.cgi?file=/work/DPG_HTML/BEST_SW/conversion_modules/Version2/Conversions.pm#convertLength(float len, String in, String out)>Conversions::convertLength(float len, String in, String out)</a>
##------------------------------------------------------------------
sub setElevation {
    my $self = shift;
    $self->{"elevation"} = Conversions::convertLength($_[0], $_[1], "m");
}

##-------------------------------------------------------------
# @signature void setEndDate(String date, String format)
# <p>Set the end date for the <code>Station</code>.  In most cases you
# should use the insertDate function instead.</p>
#
# @input $date The end date for the <code>Station</code>.
# @input $format The format of the date.
##-------------------------------------------------------------
sub setEndDate {
    my $self = shift;
    my $end = Conversions::formatDate($_[0], $_[1]);
    $end =~ s/\///g;
    $self->{"end_date"} = $end;
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
    my $comm = $_[0];
    $comm = uc($comm); # Change to upper case
    if ($comm ne "Y" && $comm ne "N") {
	die "Commissioned value must be 'Y' or 'N'\n";
    }
    $self->{"commissioned"} = sprintf("(%s)", $comm);
}

##------------------------------------------------------------------
# @signature void setLatitude(float lat, String format)
# <p>Set the latitude of the station.</p>
#
# @input  $lat The latitude of the station.
# @input  $format The format of $lat using "-DMS" characters
# @link <a href=http://www.joss.ucar.edu/cgi-bin/dpg/doc/docperl.cgi?file=/work/DPG_HTML/BEST_SW/conversion_modules/Version2/Conversions.pm#convertLatLong(String value, String format)>Conversions::convertLatLong(String value, String format)</a>
##------------------------------------------------------------------
sub setLatitude {
    my $self = shift;
    $self->{"latitude"} = Conversions::convertLatLong($_[0], $_[1]);
}

##-----------------------------------------------------------------
# @signature void setLatLongAccuracy(int acc)
# ,p>Set the accuracy of the lat and long measurements.</p>
#
# @input  $acc The accuracy of the measurements.
##-----------------------------------------------------------------
sub setLatLongAccuracy {
    my $self = shift;
    $self->{"accuracy"} = $_[0];
}

##------------------------------------------------------------------
# @signature void setLongitude(float long, String format)
# <p>Set the longitude of the station.</p>
#
# @input  $long The longitude of the station.
# @input  $format The format of $long using "-DMS" characters
# @link <a href=http://www.joss.ucar.edu/cgi-bin/dpg/doc/docperl.cgi?file=/work/DPG_HTML/BEST_SW/conversion_modules/Version2/Conversions.pm#convertLatLong(String value, String format)>Conversions::convertLatLong(String value, String format)</a>
##------------------------------------------------------------------
sub setLongitude {
    my $self = shift;
    $self->{"longitude"} = Conversions::convertLatLong($_[0], $_[1]);
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
    my $flag = lc($_[0]);
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
    $self->{"network_id_number"} = $_[0];
}

##-----------------------------------------------------------------
# @signature void setNetworkName(String name)
# <p>Set the name of the network that the station is a part of.</p>
#
# @input $name The name of the network.
##-----------------------------------------------------------------
sub setNetworkName {
    my $self = shift;
    $self->{"network_name"} = $_[0];
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
    $self->{"occurence"} = $_[0];
}

##-----------------------------------------------------------------
# @signature void setPlatformIdNumber(int id)
# <p>Set the platform id number the station is a part of.</p>
#
# @input  $id The platform id number of the station's platform.
##-----------------------------------------------------------------
sub setPlatformIdNumber {
    my $self = shift;
    $self->{"platform"} = $_[0];
}

##-----------------------------------------------------------------------
# @signature void setReferenceSiteId(String ref_site_id)
# <p>Set the identifier for the reference site of the Record.</p>
#
# @input $ref_site_id The reference site's identifier.
##-----------------------------------------------------------------------
sub setReferenceSiteId {
    my $self = shift;
    $self->{"ref_site_id"} = $_[0];
}

##-----------------------------------------------------------------
# @signature void setReportingFrequecy(String freq)
# <p>Set the frequency which the station reports data.</p>
#
# @input $freq The frequency the station reports data.
##-----------------------------------------------------------------
sub setReportingFrequency {
    my $self = shift;
    $self->{"frequency"} = $_[0];
}

##----------------------------------------------------------------
# @signature void setStateCode(String state)
# <p>Set the state code for the station's location.</p>
#
# @input $state The state's abbreviation, "XX" if unknown
# @link <a href=http://www.joss.ucar.edu/cgi-bin/dpg/doc/docperl.cgi?file=/work/DPG_HTML/BEST_SW/conversion_modules/Version2/Conversions.pm#getStateCode(String state)>Conversions::getStateCode(String state)</a>
##----------------------------------------------------------------
sub setStateCode {
    my $self = shift;
    $self->{"state_code"} = Conversions::getStateCode($_[0]);
}

##----------------------------------------------------------------
# @signature void setStationId(String id)
# <p>Set the station's identifier.</p>
#
# @input  $id The station's identifier.
##----------------------------------------------------------------
sub setStationId {
    my $self = shift;
    $self->{"station_id"} = $_[0];
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
    $self->{"utc_offset"} = $_[0];
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

1;
