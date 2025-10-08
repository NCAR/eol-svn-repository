#! /usr/bin/perl -w

##Module---------------------------------------------------------------
# The <code>SurfaceRecord</code> is a <code>Record</code> that contains
# general surface data.  It only contains values and does not have flags
# associated with them (since flags are specific to the type of record).
#
# @author Joel Clawson
# @version 2.0 This is part of the original version 1 record that holds
# weather data values.
##Module---------------------------------------------------------------
package SurfaceRecord;
use strict;
use lib ".";
use Conversions;
use Station;
use Record;
our @ISA = ("Record");

sub getMissing { return "-999.99"; }

##------------------------------------------------------------------------
# @signature float getAltimeter()
# <p>Get the altimeter value for the record.</p>
#
# @output $altimeter The altimeter value in mbar.
##------------------------------------------------------------------------
sub getAltimeter {
    my $self = shift;
    return $self->checkDefined($self->{"altimeter"}, $self->getMissing());
}

##------------------------------------------------------------------------
# @signature float getCalcSeaLevelPressure()
# <p>Calculate the sea level pressure from the station pressure, elevation,
# dew point, and temperature.</p>
#
# @output $cslp The calculated sea level pressure, -999.99 default.
##------------------------------------------------------------------------
sub getCalcSeaLevelPressure {
    my $self = shift;
    
    if (!defined($self->{"csl_pressure"})) {
	return Conversions::calculateSeaLevelPressure($self->getPressure(), $self->getElevation(), $self->getDewPoint(), $self->getTemperature(), $self->getVerbose(), $self->{"warn"});
    }

    return $self->{"csl_pressure"};
}

##-----------------------------------------------------------------------
# @signature float getDewPoint()
# <p>Get the dew point of the record, or try to calculate it if it has
# not been set.</p>
#
# @output $dew_pt The dew point value, default -999.99
##-----------------------------------------------------------------------
sub getDewPoint {
    my $self = shift;
    my $dew_pt = $self->checkDefined($self->{"dew_pt"}, $self->getMissing());

    if ($dew_pt eq getMissing() && defined($self->{"rel_humid"}) &&
	$self->{"rel_humid"} ne $self->getMissing()) {
	return Conversions::calculateDewPoint($self->getTemperature(),
					      $self->getRelativeHumidity(),
					      $self->getVerbose(),
					      $self->{"warn"});
    }
    return $dew_pt;
}

##----------------------------------------------------------------------
# @signature String getGustMarker()
# <p>Get the gust marker for the record.</p>
#
# @output $mark The gust marker, default " "
##----------------------------------------------------------------------
sub getGustMarker {
    my $self = shift;
    if ($self->getGustSpeed() ne getMissing()) { return "G"; }
    else { return " "; }
}

##----------------------------------------------------------------------
# @signature float getGustSpeed()
# <p>Get the gust speed of the record.</p>
#
# @output $spd The gust speed, default -999.99
##----------------------------------------------------------------------
sub getGustSpeed {
    my $self = shift;
    return $self->checkDefined($self->{"gust_spd"}, getMissing());
}

##-----------------------------------------------------------------------
# @signature float getIncomingLongwave()
# <p>Get the incoming longwave radiation value.</p>
#
# @output $long The incoming longwave value in W/m<sup2>2</sup2>.
##-----------------------------------------------------------------------
sub getIncomingLongwave {
    my $self = shift;
    return $self->checkDefined($self->{"in_long"}, $self->getMissing());
}

##-----------------------------------------------------------------------
# @signature float getIncomingPAR()
# <p>Get the incoming PAR value.</p>
#
# @output $par The incoming PAR value in umol/m<sup2>2</sup2>/s.
##-----------------------------------------------------------------------
sub getIncomingPAR {
    my $self = shift;
    return $self->checkDefined($self->{"in_par"}, $self->getMissing());
}

##-----------------------------------------------------------------------
# @signature float getIncomingShortwave()
# <p>Get the incoming shortwave radiation value.</p>
#
# @output $long The incoming shortwave value in W/m<sup2>2</sup2>.
##-----------------------------------------------------------------------
sub getIncomingShortwave {
    my $self = shift;
    return $self->checkDefined($self->{"in_short"}, $self->getMissing());
}

##-----------------------------------------------------------------------
# @signature float getNetRadiation()
# <p>Get the net radiation value of the record.</p>
#
# @output $net The net radiation value in W/m<sup>2</sup>
##-----------------------------------------------------------------------
sub getNetRadiation {
    my $self = shift;
    my $net = $self->checkDefined($self->{"net_rad"}, $self->getMissing());
    
    if ($net eq $self->getMissing()) {
       return Conversions::calculateNetRadiation($self->getIncomingShortwave(),
						 $self->getOutgoingShortwave(),
						 $self->getIncomingLongwave(),
						 $self->getOutgoingLongwave(),
						 $self->getVerbose(),
						 $self->{"warn"});
    }
    return $net;
}

##-----------------------------------------------------------------------
# @signature float getOutgoingLongwave()
# <p>Get the outgoing longwave radiation value.</p>
#
# @output $long The outgoing longwave value in W/m<sup2>2</sup2>.
##-----------------------------------------------------------------------
sub getOutgoingLongwave {
    my $self = shift;
    return $self->checkDefined($self->{"out_long"}, $self->getMissing());
}

##-----------------------------------------------------------------------
# @signature float getOutgoingPAR()
# <p>Get the outgoing PAR value.</p>
#
# @output $par The outgoing PAR value in umol/m<sup2>2</sup2>/s.
##-----------------------------------------------------------------------
sub getOutgoingPAR {
    my $self = shift;
    return $self->checkDefined($self->{"out_par"}, $self->getMissing());
}

##-----------------------------------------------------------------------
# @signature float getOutgoingShortwave()
# <p>Get the outgoing shortwave radiation value.</p>
#
# @output $long The outgoing shortwave value in W/m<sup2>2</sup2>.
##-----------------------------------------------------------------------
sub getOutgoingShortwave {
    my $self = shift;
    return $self->checkDefined($self->{"out_short"}, $self->getMissing());
}

##-----------------------------------------------------------------------
# @signature float getPrecip()
# <p>Get the amount of precipitation of the record.</p>
#
# @output $precip The precipitation value, default -999.99
##-----------------------------------------------------------------------
sub getPrecip {
    my $self = shift;
    return $self->checkDefined($self->{"precip"}, getMissing());
}

##-----------------------------------------------------------------------
# @signature int getPresentWeather()
# <p>Get the code for the present weather conditions</p>
#
# @output $code The present weather code, default -999
##-----------------------------------------------------------------------
sub getPresentWeather {
    my $self = shift;
    return $self->checkDefined($self->{"present_wx"}, -999);
}

##-----------------------------------------------------------------------
# @signature float getPressure()
# <p>Get the value of the station pressure of the record.</p>
#
# @output $press The station pressure, default -999.99
##-----------------------------------------------------------------------
sub getPressure {
    my $self = shift;
    my $press = $self->checkDefined($self->{"pressure"}, $self->getMissing());

    if ($press eq $self->getMissing() && defined($self->{"altimeter"})) {
	return Conversions::calculateStationPressure($self->getAltimeter(),
						     $self->getElevation(),
						     $self->getVerbose(),
						     $self->{"warn"});
    }
    return $press;
}

##-----------------------------------------------------------------------
# @signature float getRelativeHumidity()
# <p>Get the value of the relative humidity of the record.</p>
#
# @output $rh The relative humidity, default -999.99
##-----------------------------------------------------------------------
sub getRelativeHumidity {
    my $self = shift;
    my $rh = $self->checkDefined($self->{"rel_humid"}, getMissing());

    if ($rh eq getMissing() && defined($self->{"dew_pt"}) &&
	$self->{"dew_pt"} ne $self->getMissing()) {
	return Conversions::calculateRelativeHumidity($self->getTemperature(),
						      $self->getDewPoint(),
						      $self->getVerbose(),
						      $self->{"warn"});
    }
    return $rh;
}

##-----------------------------------------------------------------------
# @signature float getSeaLevelPressure()
# <p>Get the value of the sea level pressure of the record.</p>
#
# @output $slp The sea level pressure, default -999.99
##-----------------------------------------------------------------------
sub getSeaLevelPressure {
    my $self = shift;
    return $self->checkDefined($self->{"sl_pressure"}, getMissing());
}

##-----------------------------------------------------------------------
# @signature float getSkinTemperature()
# <p>Get the skin temperature of the record.</p>
#
# @output $skin The skin temperture of the record in degrees C.
##-----------------------------------------------------------------------
sub getSkinTemperature {
    my $self = shift;
    return $self->checkDefined($self->{"skin_temp"}, $self->getMissing());
}

##-----------------------------------------------------------------------
# @signature float getSnowDepth()
# <p>Get the value of the snow depth in centimeters.</p>
#
# @output $snow The snow depth in centimeters.
##-----------------------------------------------------------------------
sub getSnowDepth {
    my $self = shift;
    return $self->checkDefined($self->{"snow_depth"}, $self->getMissing());
}

##-----------------------------------------------------------------------
# @signature float getSpecificHumidity()
# <p>Get the specific humidity value for the record.</p>
#
# @output $sh The specific humidity in g/kg.
##-----------------------------------------------------------------------
sub getSpecificHumidity {
    my $self = shift;
    my $sh = $self->checkDefined($self->{"spec_humid"}, $self->getMissing());

    if ($sh eq $self->getMissing()) {
	return Conversions::calculateSpecificHumidity($self->getPressure(),
						      $self->getDewPoint(),
						      $self->getVerbose(),
						      $self->{"warn"});
    }
    return $sh;
}

##-----------------------------------------------------------------------
# @signature float getTemperature()
# <p>Get the value of the temperature of the record.</p>
#
# @output $temp The temperature, default -999.99
##-----------------------------------------------------------------------
sub getTemperature {
    my $self = shift;
    return $self->checkDefined($self->{"temp"}, getMissing());
}

##-----------------------------------------------------------------------
# @signature float getVisibility()
# <p>Get the value of the visibility of the record.</p>
#
# @output $vis The visibility, default -999.99
##-----------------------------------------------------------------------
sub getVisibility {
    my $self = shift;
    return $self->checkDefined($self->{"visibility"}, getMissing());
}

##-----------------------------------------------------------------------
# @signature float getWindDirection()
# <p>Get the value of the wind direction of the record in degrees.</p>
#
# @output $wind_dir The wind direction, default -999.99
##-----------------------------------------------------------------------
sub getWindDirection {
    my $self = shift;
    my $wind_dir = $self->checkDefined($self->{"wind_dir"}, getMissing());

    if ($wind_dir eq $self->getMissing() &&
	defined($self->{"u_wind"}) && defined($self->{"v_wind"})) {
	return (Conversions::calculateWindFromUV($self->getWindUComponent(),
						 $self->getWindVComponent(),
						 $self->getVerbose(),
						 $self->{"warn"}))[1];
    }
    return $wind_dir;
}

##-----------------------------------------------------------------------
# @signature float getWindSpeed()
# <p>Get the value of the wind speed of the record in m/s.</p>
#
# @output $wind_spd The wind speed, default -999.99
##-----------------------------------------------------------------------
sub getWindSpeed {
    my $self = shift;
    my $wind_spd = $self->checkDefined($self->{"wind_spd"}, getMissing());

    if ($wind_spd eq $self->getMissing() &&
	defined($self->{"u_wind"}) && defined($self->{"v_wind"})) {
	return (Conversions::calculateWindFromUV($self->getWindUComponent(),
						 $self->getWindVComponent(),
						 $self->getVerbose(),
						 $self->{"warn"}))[0];
    }
    return $wind_spd;
}

##-----------------------------------------------------------------------
# @signature float getWindUComponent()
# <p>Get the U wind component for the record.</p>
#
# @output $comp The U wind component in m/s.
##-----------------------------------------------------------------------
sub getWindUComponent {
    my $self = shift;
    my $comp = $self->checkDefined($self->{"u_wind"}, $self->getMissing());

    if ($comp eq $self->getMissing() && 
	defined($self->{"wind_spd"}) && defined($self->{"wind_dir"})) {
	return (Conversions::calculateUVfromWind($self->getWindSpeed(),
						 $self->getWindDirection(),
						 $self->getVerbose(),
						 $self->{"warn"}))[0];
    }
    return $comp;
}

##-----------------------------------------------------------------------
# @signature float getWindVComponent()
# <p>Get the V wind component for the record.</p>
#
# @output $comp The V wind component in m/s.
##-----------------------------------------------------------------------
sub getWindVComponent {
    my $self = shift;
    my $comp = $self->checkDefined($self->{"v_wind"}, $self->getMissing());

    if ($comp eq $self->getMissing() && 
	defined($self->{"wind_spd"}) && defined($self->{"wind_dir"})) {
	return (Conversions::calculateUVfromWind($self->getWindSpeed(),
						 $self->getWindDirection(),
						 $self->getVerbose(),
						 $self->{"warn"}))[1];
    }
    return $comp;
}

##------------------------------------------------------------------------
# @signature void setAltimeter(float alt, String unit)
# <p>Set the altimeter value for the record.</p>
#
# @input $alt The altimeter value.
# @input $unit The pressure unit of messurement for the altimeter value.
##------------------------------------------------------------------------
sub setAltimeter {
    my $self = shift;
    $self->{"altimeter"} = Conversions::convertPressure($_[0], $_[1], "mbar");
}

##-----------------------------------------------------------------------
# @signature void setCalcSeaLevelPressure(float cslp, String unit)
# <p>Set the calculated sea level pressure value for the record.</p>
# 
# @input $dew_pt The calculated sea level pressure value.
# @input $unit The unit of measurement of the calculated sea level pressure.
##-----------------------------------------------------------------------
sub setCalcSeaLevelPressure {
    my $self = shift;
    $self->{"csl_pressure"} = Conversions::convertPressure($_[0],$_[1],"mb");
}

##-----------------------------------------------------------------------
# @signature void setDewPoint(float dew_pt, String unit)
# <p>Set the dew point value for the record.</p>
# 
# @input $dew_pt The dew point value
# @input $unit The unit of measurement of the dew point.
##-----------------------------------------------------------------------
sub setDewPoint {
    my $self = shift;
    $self->{"dew_pt"} = Conversions::convertTemperature($_[0], $_[1], "C");
}

##-----------------------------------------------------------------------
# @signature void setGustSpeed(float gust, String unit)
# <p>Set the gust speed value for the record.</p>
#
# @input $gust The gust speed value.
# @input $unit The unit of measurement of the gust speed.
##-----------------------------------------------------------------------
sub setGustSpeed {
    my $self = shift;
    $self->{"gust_spd"} = Conversions::convertVelocity($_[0], $_[1], "m/s");
}

##-----------------------------------------------------------------------
# @signature void setIncomingLongwave(float long, String unit)
# <p>Set the incoming longwave radiation for the record.</p>
#
# @input $long The incoming longwave radiation value.
# @input $unit The radiation unit of measurement of the incoming longwave 
# value.
##-----------------------------------------------------------------------
sub setIncomingLongwave {
    my $self = shift;
    $self->{"in_long"} = Conversions::convertRadiation($_[0],$_[1],"W/m2");
}

##-----------------------------------------------------------------------
# @signature void setIncomingShortwave(float short, String unit)
# <p>Set the incoming shortwave radiation for the record.</p>
#
# @input $short The incoming shortwave radiation value.
# @input $unit The radiation unit of measurement of the incoming shortwave 
# value.
##-----------------------------------------------------------------------
sub setIncomingShortwave {
    my $self = shift;
    $self->{"in_short"} = Conversions::convertRadiation($_[0],$_[1],"W/m2");
}

##-----------------------------------------------------------------------
# @signature void setIncomingPAR(float par)
# <p>Set the incoming par value for the record.</p>
#
# @input $par The incoming PAR value.
# @input $unit The radiation unit of measurement of the incoming PAR value.
##-----------------------------------------------------------------------
sub setIncomingPAR {
    my $self = shift;
    $self->{"in_par"} = $_[0]
}

##-----------------------------------------------------------------------
# @signature void setNetRadiation(float net, String unit)
# <p>Set the net radiation value of the record.</p>
#
# @input $net The net radiation value.
# @input $unit The radiation unit of measurement of the net radiation.
##-----------------------------------------------------------------------
sub setNetRadiation {
    my $self = shift;
    $self->{"net_rad"} = Conversions::convertRadiation($_[0],$_[1],"W/m2");
}

##-----------------------------------------------------------------------
# @signature void setOutgoingLongwave(float long, String unit)
# <p>Set the outgoing longwave radiation for the record.</p>
#
# @input $long The outgoing longwave radiation value.
# @input $unit The radiation unit of measurement of the outgoing longwave 
# value.
##-----------------------------------------------------------------------
sub setOutgoingLongwave {
    my $self = shift;
    $self->{"out_long"} = Conversions::convertRadiation($_[0],$_[1],"W/m2");
}

##-----------------------------------------------------------------------
# @signature void setOutgoingShortwave(float short, String unit)
# <p>Set the outgoing shortwave radiation for the record.</p>
#
# @input $short The outgoing shortwave radiation value.
# @input $unit The radiation unit of measurement of the outgoing shortwave 
# value.
##-----------------------------------------------------------------------
sub setOutgoingShortwave {
    my $self = shift;
    $self->{"out_short"} = Conversions::convertRadiation($_[0],$_[1],"W/m2");
}

##-----------------------------------------------------------------------
# @signature void setOutgoingPAR(float par)
# <p>Set the outgoing par value for the record.</p>
#
# @input $par The outgoing PAR value.
# @input $unit The radiation unit of measurement of the outgoing PAR value.
##-----------------------------------------------------------------------
sub setOutgoingPAR {
    my $self = shift;
    $self->{"out_par"} = $_[0]
}

##-----------------------------------------------------------------------
# @signature void setPrecip(float precip, String unit)
# <p>Set the precipitation value for the record.</p>
# 
# @input $precip The precip value.
# @input $unit The unit of measurement of the precipitation.
##-----------------------------------------------------------------------
sub setPrecip {
    my $self = shift;
    $self->{"precip"} = Conversions::convertLength($_[0], $_[1], "mm");
}

##-----------------------------------------------------------------------
# @signature void setPresentWeather(int wx)
# <p>Set the present weather conditions for the reading.</p>
#
# @input $wx The present weather conditions.
##-----------------------------------------------------------------------
sub setPresentWeather {
    my $self = shift;
    $self->{"present_wx"} = $_[0];
}

##-----------------------------------------------------------------------
# @signature void setPressure(float press, String unit)
# <p>Set the station pressure for the reading.</p>
# 
# @input $press The station pressure value
# @input $unit The unit of measurement of the pressure value.
##-----------------------------------------------------------------------
sub setPressure {
    my $self = shift;
    $self->{"pressure"} = Conversions::convertPressure($_[0], $_[1], "mbar");
}

##-----------------------------------------------------------------------
# @signature void setRelativeHumidity(float rh, String unit)
# <p>Set the relative humidity for the reading.</p>
#
# @input $rh The relative humidity value.
# @input $unit 1 if the rh is in percent, 0 otherwise.
##-----------------------------------------------------------------------
sub setRelativeHumidity {
    my $self = shift;
    if ($_[1] || $_[0] eq "-999.99") { $self->{"rel_humid"} = $_[0]; }
    else { $self->{"rel_humid"} = 100 * $_[0]; }
}

##-----------------------------------------------------------------------
# @signature void setSeaLevelPressure(float slp, String unit)
# <p>Set the sea level pressure of the record.</p>
#
# @input $slp The sea level pressure value.
# @input $unit The unit of measurement of the sea level pressure.
##-----------------------------------------------------------------------
sub setSeaLevelPressure {
    my $self = shift;
    $self->{"sl_pressure"} = Conversions::convertPressure($_[0],$_[1],"mbar");
}

##-----------------------------------------------------------------------
# @signature void setSkinTemperature(float temp, String unit)
# <p>Set the skin temperature of the record.</p>
#
# @input $temp The skin temperature value.
# @input $unit The temperature unit of the skin temperature.
##-----------------------------------------------------------------------
sub setSkinTemperature {
    my $self = shift;
    $self->{"skin_temp"} = Conversions::convertTemperature($_[0],$_[1],"C");
}

##-----------------------------------------------------------------------
# @signature void setSnowDepth(float depth, String unit)
# <p>Set the snow depth of the record.</p>
# 
# @input $depth The depth of the snow of the record.
# @input $unit The length unit of the snow depth.
##-----------------------------------------------------------------------
sub setSnowDepth {
    my $self = shift;
    $self->{"snow_depth"} = Conversions::convertLenth($_[0], $_[1], "cm");
}

##-----------------------------------------------------------------------
# @signature void setSpecificHumidity(float sh, String unit)
# <p>Set the specific humidity value of the record.</p>
#
# @input $sh The specific humidity value.
# @input $unit The unit of measurement of the specific humidity.
##-----------------------------------------------------------------------
sub setSpecificHumidity {
    my $self = shift;
    if ($_[1] eq "kg/kg") { $self->{"spec_humid"} = $_[0] * 1000; }
    elsif ($_[1] eq "g/kg") { $self->{"spec_humid"} = $_[0]; }
    else { die("Unknown units $_[1] for specific humidity.\n"); }
}

##-----------------------------------------------------------------------
# @signature void setTemperature(float temp, String unit)
# <p>Set the temperature of the record.</p>
# 
# @input $temp The temperature value.
# @input $unit The unit of measurement of the temperature.
##-----------------------------------------------------------------------
sub setTemperature {
    my $self = shift;
    $self->{"temp"} = Conversions::convertTemperature($_[0], $_[1], "C");
}

##-----------------------------------------------------------------------
# @signature void setVisibility(float vis, String unit)
# <p>Set the visibility of the record.</p>
#
# @input $vis The visibility value.
# @input $unit The unit of measurement of the visibility.
##-----------------------------------------------------------------------
sub setVisibility {
    my $self = shift;
    $self->{"visibility"} = Conversions::convertLength($_[0], $_[1], "m");
}

##-----------------------------------------------------------------------
# @signature void setWindDirection(float wind_dir)
# <p>Set the wind direction of the record</p>
#
# @input $wind_dir The wind direction in degrees.
##-----------------------------------------------------------------------
sub setWindDirection {
    my $self = shift;
    $self->{"wind_dir"} = $_[0];
}

##-----------------------------------------------------------------------
# @signature void setWindSpeed(float spd, String unit)
# <p>Set the wind speed of the record.</p>
#
# @input $spd The wind speed value.
# @input $unit The unit of measurement of the wind speed.
##-----------------------------------------------------------------------
sub setWindSpeed {
    my $self = shift;
    $self->{"wind_spd"} = Conversions::convertVelocity($_[0], $_[1], "m/s");
}

##-----------------------------------------------------------------------
# @signature void setWindUComponent(float comp, String unit)
# <p>Set the U wind component of the record.</p>
#
# @input $comp The U wind component.
# @input $unit The velocity unit of measurement of the U component.
##-----------------------------------------------------------------------
sub setWindUComponent {
    my $self = shift;
    $self->{"u_wind"} = Conversions::convertVelocity($_[0], $_[1], "m/s");
}

##-----------------------------------------------------------------------
# @signature void setWindVComponent(float comp, String unit)
# <p>Set the V wind component of the record.</p>
#
# @input $comp The V wind component.
# @input $unit The velocity unit of measurement of the V component.
##-----------------------------------------------------------------------
sub setWindVComponent {
    my $self = shift;
    $self->{"v_wind"} = Conversions::convertVelocity($_[0], $_[1], "m/s");
}

1;
