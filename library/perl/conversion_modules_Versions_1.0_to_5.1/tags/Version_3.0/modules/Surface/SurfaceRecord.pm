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
package Surface::SurfaceRecord;
use strict;
use lib "..";
use DpgCalculations;
use DpgConversions;
use Record;
use Surface::SurfaceConstants qw(:DEFAULT);
our @ISA = ("Record");

##------------------------------------------------------------------------
# @signature float getAltimeter()
# <p>Get the altimeter value for the record.</p>
#
# @output $altimeter The altimeter value in mbar.
##------------------------------------------------------------------------
sub getAltimeter {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getAltimeter\n"); }

    return $self->checkDefined($self->{"altimeter"}, $MISSING);
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
    if (scalar(@_) != 0) { die("Invalid parameters to getCalcSeaLevelPressure\n"); }

    return $self->{"csl_pressure"} if (defined($self->{"csl_pressure"}));

    if ($self->getTemperature() != $MISSING &&
	$self->getElevation()   != $MISSING &&
	$self->getDewPoint()    != $MISSING &&
	$self->getPressure()    != $MISSING) {
	my $cslp = calculateSeaLevelPressure($self->getPressure(), 
					     $self->getElevation(), 
					     $self->getDewPoint(), 
					     $self->getTemperature(),
					     $self->getVerbose(), 
					     $self->{"warn"});
	return $cslp if (defined($cslp));
    }
    return $MISSING;
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
    if (scalar(@_) != 0) { die("Invalid parameters to getDewPoint\n"); }

    return $self->{"dew_pt"} if (defined($self->{"dew_pt"}));

    if ($self->getTemperature() != $MISSING && 
	defined($self->{"rel_humid"})) {
	my $dewpt = calculateDewPoint($self->getTemperature(),
				      $self->getRelativeHumidity(),
				      $self->getVerbose(),
				      $self->{"warn"});
	return $dewpt if (defined($dewpt));
    } 
    return $MISSING;
}

##----------------------------------------------------------------------
# @signature float getGustSpeed()
# <p>Get the gust speed of the record.</p>
#
# @output $spd The gust speed, default -999.99
##----------------------------------------------------------------------
sub getGustSpeed {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getGustSpeed\n"); }

    return $self->checkDefined($self->{"gust_spd"}, $MISSING);
}

##-----------------------------------------------------------------------
# @signature float getIncomingLongwave()
# <p>Get the incoming longwave radiation value.</p>
#
# @output $long The incoming longwave value in W/m<sup2>2</sup2>.
##-----------------------------------------------------------------------
sub getIncomingLongwave {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getIncomingLongwave\n"); }

    return $self->checkDefined($self->{"in_long"}, $MISSING);
}

##-----------------------------------------------------------------------
# @signature float getIncomingPAR()
# <p>Get the incoming PAR value.</p>
#
# @output $par The incoming PAR value in umol/m<sup2>2</sup2>/s.
##-----------------------------------------------------------------------
sub getIncomingPAR {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getIncomingPAR\n"); }

    return $self->checkDefined($self->{"in_par"}, $MISSING);
}

##-----------------------------------------------------------------------
# @signature float getIncomingShortwave()
# <p>Get the incoming shortwave radiation value.</p>
#
# @output $long The incoming shortwave value in W/m<sup2>2</sup2>.
##-----------------------------------------------------------------------
sub getIncomingShortwave {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getIncomingShortwave\n"); }

    return $self->checkDefined($self->{"in_short"}, $MISSING);
}

##-----------------------------------------------------------------------
# @signature float getNetRadiation()
# <p>Get the net radiation value of the record.</p>
#
# @output $net The net radiation value in W/m<sup>2</sup>
##-----------------------------------------------------------------------
sub getNetRadiation {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getNetRadiation\n"); }

    return $self->{"net_rad"} if (defined($self->{"net_rad"}));

    if ($self->getIncomingShortwave() != $MISSING &&
	$self->getIncomingLongwave()  != $MISSING &&
	$self->getOutgoingShortwave() != $MISSING &&
	$self->getOutgoingLongwave()  != $MISSING) {

	return ($self->getIncomingShortwave() + $self->getIncomingLongwave()) -
	    ($self->getOutgoingShortwave() + $self->getOutgoingLongwave());
    }
    return $MISSING;
}

##-----------------------------------------------------------------------
# @signature float getOutgoingLongwave()
# <p>Get the outgoing longwave radiation value.</p>
#
# @output $long The outgoing longwave value in W/m<sup2>2</sup2>.
##-----------------------------------------------------------------------
sub getOutgoingLongwave {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getOutgoingLongwave\n"); }

    return $self->checkDefined($self->{"out_long"}, $MISSING);
}

##-----------------------------------------------------------------------
# @signature float getOutgoingPAR()
# <p>Get the outgoing PAR value.</p>
#
# @output $par The outgoing PAR value in umol/m<sup2>2</sup2>/s.
##-----------------------------------------------------------------------
sub getOutgoingPAR {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getOutgoingPAR\n"); }

    return $self->checkDefined($self->{"out_par"}, $MISSING);
}

##-----------------------------------------------------------------------
# @signature float getOutgoingShortwave()
# <p>Get the outgoing shortwave radiation value.</p>
#
# @output $long The outgoing shortwave value in W/m<sup2>2</sup2>.
##-----------------------------------------------------------------------
sub getOutgoingShortwave {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getOutgoingShortwave\n"); }

    return $self->checkDefined($self->{"out_short"}, $MISSING);
}

##-----------------------------------------------------------------------
# @signature float getPrecip()
# <p>Get the amount of precipitation of the record.</p>
#
# @output $precip The precipitation value, default -999.99
##-----------------------------------------------------------------------
sub getPrecip {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getPrecip\n"); }

    return $self->checkDefined($self->{"precip"}, $MISSING);
}

##-----------------------------------------------------------------------
# @signature int getPresentWeather()
# <p>Get the code for the present weather conditions</p>
#
# @output $code The present weather code, default -999
##-----------------------------------------------------------------------
sub getPresentWeather {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getPresentWeather\n"); }

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
    if (scalar(@_) != 0) { die("Invalid parameters to getPressure\n"); }

    return $self->{"pressure"} if defined($self->{"pressure"});

    if ($self->getAltimeter() != $MISSING &&
	$self->getElevation() != $MISSING) {
	my $press = calculatePressureFromAltimeter($self->getAltimeter(),
						   $self->getElevation(),
						   $self->getVerbose(),
						   $self->{"warn"});
	return $press if (defined($press));
    }
    return $MISSING;
}

##-----------------------------------------------------------------------
# @signature float getRelativeHumidity()
# <p>Get the value of the relative humidity of the record.</p>
#
# @output $rh The relative humidity, default -999.99
##-----------------------------------------------------------------------
sub getRelativeHumidity {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getRelativeHumidity\n"); }

    return $self->{"rel_humid"} if (defined($self->{"rel_humid"}));

    if ($self->getTemperature() != $MISSING && 
	defined($self->{"dew_pt"})) {
	my $rh = calculateRelativeHumidity($self->getTemperature(),
					   $self->getDewPoint(),
					   $self->getVerbose(),
					   $self->{"warn"});
	return $rh if (defined($rh));
    }
    return $MISSING;
}

##-----------------------------------------------------------------------
# @signature float getSeaLevelPressure()
# <p>Get the value of the sea level pressure of the record.</p>
#
# @output $slp The sea level pressure, default -999.99
##-----------------------------------------------------------------------
sub getSeaLevelPressure {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getSeaLevelPressure\n"); }

    return $self->checkDefined($self->{"sl_pressure"}, $MISSING);
}

##-----------------------------------------------------------------------
# @signature float getSkinTemperature()
# <p>Get the skin temperature of the record.</p>
#
# @output $skin The skin temperture of the record in degrees C.
##-----------------------------------------------------------------------
sub getSkinTemperature {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getSkinTemperature\n"); }

    return $self->checkDefined($self->{"skin_temp"}, $MISSING);
}

##-----------------------------------------------------------------------
# @signature float getSnowDepth()
# <p>Get the value of the snow depth in centimeters.</p>
#
# @output $snow The snow depth in centimeters.
##-----------------------------------------------------------------------
sub getSnowDepth {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getSnowDepth\n"); }

    return $self->checkDefined($self->{"snow_depth"}, $MISSING);
}

##-----------------------------------------------------------------------
# @signature float getSpecificHumidity()
# <p>Get the specific humidity value for the record.</p>
#
# @output $sh The specific humidity in g/kg.
##-----------------------------------------------------------------------
sub getSpecificHumidity {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getSpecificHumidity\n"); }

    return $self->{"spec_humid"} if (defined($self->{"spec_humid"}));

    if ($self->getPressure() != $MISSING &&
	$self->getDewPoint() != $MISSING) {
	my $sh = calculateSpecificHumidity($self->getPressure(),
					   $self->getDewPoint(),
					   $self->getVerbose(),
					   $self->{"warn"});
	return $sh if (defined($sh));
    }
    return $MISSING;
}

##-----------------------------------------------------------------------
# @signature float getTemperature()
# <p>Get the value of the temperature of the record.</p>
#
# @output $temp The temperature, default -999.99
##-----------------------------------------------------------------------
sub getTemperature {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getTemperature\n"); }

    return $self->checkDefined($self->{"temp"}, $MISSING);
}

##-----------------------------------------------------------------------
# @signature float getVisibility()
# <p>Get the value of the visibility of the record.</p>
#
# @output $vis The visibility, default -999.99
##-----------------------------------------------------------------------
sub getVisibility {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getVisibility\n"); }

    return $self->checkDefined($self->{"visibility"}, $MISSING);
}

##-----------------------------------------------------------------------
# @signature float getWindDirection()
# <p>Get the value of the wind direction of the record in degrees.</p>
#
# @output $wind_dir The wind direction, default -999.99
##-----------------------------------------------------------------------
sub getWindDirection {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getWindDirection\n"); }

    return $self->{"wind_dir"} if (defined($self->{"wind_dir"}));

    if (defined($self->{"u_wind"}) && defined($self->{"v_wind"})) {
	my $dir = (calculateWindFromUV($self->getWindUComponent(),
				       $self->getWindVComponent(),
				       $self->getVerbose(),
				       $self->{"warn"}))[1];
	return $dir if (defined($dir));
    }
    return $MISSING;
}

##-----------------------------------------------------------------------
# @signature float getWindSpeed()
# <p>Get the value of the wind speed of the record in m/s.</p>
#
# @output $wind_spd The wind speed, default -999.99
##-----------------------------------------------------------------------
sub getWindSpeed {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getWindSpeed\n"); }

    return $self->{"wind_spd"} if (defined($self->{"wind_spd"}));

    if (defined($self->{"u_wind"}) && defined($self->{"v_wind"})) {
	my $spd = (calculateWindFromUV($self->getWindUComponent(),
				       $self->getWindVComponent(),
				       $self->getVerbose(),
				       $self->{"warn"}))[0];
	return $spd if (defined($spd));
    }
    return $MISSING;
}

##-----------------------------------------------------------------------
# @signature float getWindUComponent()
# <p>Get the U wind component for the record.</p>
#
# @output $comp The U wind component in m/s.
##-----------------------------------------------------------------------
sub getWindUComponent {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getWindUComponent\n"); }

    return $self->{"u_wind"} if (defined($self->{"u_wind"}));

    if (defined($self->{"wind_spd"}) && defined($self->{"wind_dir"})) {
	my $uwind = (calculateUVfromWind($self->getWindSpeed(),
					 $self->getWindDirection(),
					 $self->getVerbose(),
					 $self->{"warn"}))[0];
	return $uwind if (defined($uwind));
    }
    return $MISSING;
}

##-----------------------------------------------------------------------
# @signature float getWindVComponent()
# <p>Get the V wind component for the record.</p>
#
# @output $comp The V wind component in m/s.
##-----------------------------------------------------------------------
sub getWindVComponent {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getWindVComponent\n"); }

    return $self->{"v_wind"} if (defined($self->{"v_wind"}));

    if (defined($self->{"wind_spd"}) && defined($self->{"wind_dir"})) {
	my $vwind = (calculateUVfromWind($self->getWindSpeed(),
					 $self->getWindDirection(),
					 $self->getVerbose(),
					 $self->{"warn"}))[1];
	return $vwind if (defined($vwind));
    }
    return $MISSING;
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
    if (scalar(@_) != 2) { die("Invalid parameters to setAltimeter\n"); }

    if (defined($_[0]) && $_[0] != $MISSING) {
	$self->{"altimeter"} = convertPressure(@_,"mbar");
    } else {
	undef($self->{"altimeter"});
    }
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
    if (scalar(@_) != 2) { die("Invalid parameters to setCalcSeaLevelPressure\n"); }

    if (defined($_[0]) && $_[0] != $MISSING) {
	$self->{"csl_pressure"} = convertPressure(@_,"mb");
    } else {
	undef($self->{"csl_pressure"});
    }
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
    if (scalar(@_) != 2) { die("Invalid parameters to setDewPoint\n"); }

    if (defined($_[0]) && $_[0] != $MISSING) {
	$self->{"dew_pt"} = convertTemperature(@_,"C");
    } else {
	undef($self->{"dew_pt"});
    }
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
    if (scalar(@_) != 2) { die("Invalid parameters to setGustSpeed\n"); }

    if (defined($_[0]) && $_[0] != $MISSING) {
	$self->{"gust_spd"} = convertVelocity(@_,"m/s");
    } else {
	undef($self->{"gust_spd"});
    }
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
    if (scalar(@_) != 2) { die("Invalid parameters to setIncomingLongwave\n"); }

    if (defined($_[0]) && $_[0] != $MISSING) {
	$self->{"in_long"} = convertRadiation(@_,"w/m2");
    } else {
	undef($self->{"in_long"});
    }
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
    if (scalar(@_) != 2) { die("Invalid parameters to setIncomingShortwave\n"); }

    if (defined($_[0]) && $_[0] != $MISSING) {
	$self->{"in_short"} = convertRadiation(@_,"w/m2");
    } else {
	undef($self->{"in_short"});
    }
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
    if (scalar(@_) != 2) { die("Invalid parameters to setIncomingPAR\n"); }

    if (defined($_[0])) { $self->{"in_par"} = $_[0]; }
    else { undef($self->{"in_par"}); }
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
    if (scalar(@_) != 2) { die("Invalid parameters to setNetRadiation\n"); }

    if (defined($_[0]) && $_[0] != $MISSING) {
	$self->{"net_rad"} = convertRadiation(@_,"w/m2");
    } else {
	undef($self->{"net_rad"});
    }
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
    if (scalar(@_) != 2) { die("Invalid parameters to setOutgoingLongwave\n"); }

    if (defined($_[0]) && $_[0] != $MISSING) {
	$self->{"out_long"} = convertRadiation(@_,"w/m2");
    } else {
	undef($self->{"out_long"});
    }
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
    if (scalar(@_) != 2) { die("Invalid parameters to setOutgoingShortwave\n"); }

    if (defined($_[0]) && $_[0] != $MISSING) {
	$self->{"out_short"} = convertRadiation(@_,"w/m2");
    } else {
	undef($self->{"out_short"});
    }
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
    if (scalar(@_) != 2) { die("Invalid parameters to setOutgoingPAR\n"); }

    if (defined($_[0])) { $self->{"out_par"} = $_[0]; }
    else { undef($self->{"out_par"}); }
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
    if (scalar(@_) != 2) { die("Invalid parameters to setPrecip\n"); }

    if (defined($_[0]) && $_[0] != $MISSING) {
	$self->{"precip"} = convertLength(@_,"mm");
    } else {
	undef($self->{"precip"});
    }
}

##-----------------------------------------------------------------------
# @signature void setPresentWeather(int wx)
# <p>Set the present weather conditions for the reading.</p>
#
# @input $wx The present weather conditions.
##-----------------------------------------------------------------------
sub setPresentWeather {
    my $self = shift;
    if (scalar(@_) != 1) { die("Invalid parameters to setPresentWeather\n"); }

    if (defined($_[0]) && $_[0] != -999) { $self->{"present_wx"} = $_[0]; }
    else { undef($self->{"present_wx"}); }
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
    if (scalar(@_) != 2) { die("Invalid parameters to setPressure\n"); }

    if (defined($_[0]) && $_[0] != $MISSING) {
	$self->{"pressure"} = convertPressure(@_,"mbar");
    } else { undef($self->{"pressure"}); }
}

##-----------------------------------------------------------------------
# @signature void setRelativeHumidity(float rh)
# <p>Set the relative humidity for the reading.</p>
#
# @input $rh The relative humidity value in percent.
##-----------------------------------------------------------------------
sub setRelativeHumidity {
    my $self = shift;
    if (scalar(@_) != 1) { die("Invalid parameters to setRelativeHumidity\n"); }

    if (defined($_[0]) && $_[0] != $MISSING) {
	$self->{"rel_humid"} = $_[0];
    } else { undef($self->{"rel_humid"}); }
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
    if (scalar(@_) != 2) { die("Invalid parameters to setSeaLevelPressure\n"); }

    if (defined($_[0]) && $_[0] != $MISSING) {
	$self->{"sl_pressure"} = convertPressure(@_,"mbar");
    } else { undef($self->{"sl_pressure"}); }
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
    if (scalar(@_) != 2) { die("Invalid parameters to setSkinTemperature\n"); }

    if (defined($_[0]) && $_[0] != $MISSING) {
	$self->{"skin_temp"} = convertTemperature(@_,"C");
    } else { undef($self->{"skin_temp"}); }
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
    if (scalar(@_) != 2) { die("Invalid parameters to setSnowDepth\n"); }

    if (defined($_[0]) && $_[0] != $MISSING) {
	$self->{"snow_depth"} = convertLength(@_,"cm");
    } else { undef($self->{"snow_depth"}); }
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
    if (scalar(@_) != 2) { die("Invalid parameters to setSpecificHumidity\n"); }

    if (defined($_[0]) && $_[0] != $MISSING) {
	if ($_[1] eq "kg/kg") { $self->{"spec_humid"} = $_[0] * 1000; }
	elsif ($_[1] eq "g/kg") { $self->{"spec_humid"} = $_[0]; }
	else { die("Unknown units $_[1] for specific humidity.\n"); }
    } else { undef($self->{"spec_humid"}); }
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
    if (scalar(@_) != 2) { die("Invalid parameters to setTemperature\n"); }

    if (defined($_[0]) && $_[0] != $MISSING) {
	$self->{"temp"} = convertTemperature(@_,"C");
    } else { undef($self->{"temp"}); }
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
    if (scalar(@_) != 2) { die("Invalid parameters to setVisibility\n"); }

    if (defined($_[0]) && $_[0] != $MISSING) {
	$self->{"visibility"} = convertLength(@_,"m");
    } else { undef($self->{"visibility"}); }
}

##-----------------------------------------------------------------------
# @signature void setWindDirection(float wind_dir)
# <p>Set the wind direction of the record</p>
#
# @input $wind_dir The wind direction in degrees.
##-----------------------------------------------------------------------
sub setWindDirection {
    my $self = shift;
    if (scalar(@_) != 1) { die("Invalid parameters to setWindDirection\n"); }

    if (defined($_[0]) && $_[0] != $MISSING) {
	$self->{"wind_dir"} = $_[0];
    } else { undef($self->{"wind_dir"}); }
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
    if (scalar(@_) != 2) { die("Invalid parameters to setWindSpeed\n"); }

    if (defined($_[0]) && $_[0] != $MISSING) {
	$self->{"wind_spd"} = convertVelocity(@_,"m/s");
    } else { undef($self->{"wind_spd"}); }
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
    if (scalar(@_) != 2) { die("Invalid parameters to setWindUComponent\n"); }

    if (defined($_[0]) && $_[0] != $MISSING) {
	$self->{"u_wind"} = convertVelocity(@_,"m/s");
    } else { undef($self->{"u_wind"}); }
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
    if (scalar(@_) != 2) { die("Invalid parameters to setWindVComponent\n"); }

    if (defined($_[0]) && $_[0] != $MISSING) {
	$self->{"v_wind"} = convertVelocity(@_,"m/s");
    } else { undef($self->{"v_wind"}); }
}

1;
