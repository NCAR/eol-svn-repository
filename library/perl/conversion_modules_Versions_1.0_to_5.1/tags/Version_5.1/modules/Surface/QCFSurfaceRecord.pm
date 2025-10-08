#! /usr/bin/perl -w

##Module---------------------------------------------------------------
# <p>The <code>QCFSurfaceRecord</code> is a <code>SurfaceRecord</code> and
# a <code>QCFConstants</code>.  It defineds a QCF formated surface record.</p>
#
# @author Joel Clawson
# @version 5.0 <p>Janine found that the dubious flag was missing in 
# check_parameter.  Added it in.</p>
#
# @author Joel Clawson
# @version 2.0  This is the original version.  It is the flags and specific
# QCF information for a surface record for the version 1 Record.
##Module---------------------------------------------------------------
package Surface::QCFSurfaceRecord;
use strict;
use lib "..";
use DpgDate;
use Surface::QCFConstants qw(:DEFAULT);
use Surface::SurfaceRecord;
our @ISA = ("Surface::SurfaceRecord");

#---------------------------------------------------------------------------
# @signature String getDewPointFlagPrecedence()
# <p>Get the dew point flag based on the flag precedence of the values
# used to calculate the dew point.</p>
#
# @output $flag The dew point flag.
#---------------------------------------------------------------------------
our $getDewPointFlagPrecedence = sub {
    my $self = shift;
    my %flag_precedence = getFlagPrecedence();
 
    # Get the initial flag for the dew point value.
    my $flag = $self->getDewPoint() eq $self->getMissing() ?
	$INCALCUABLE_FLAG : $UNCHECKED_FLAG;

    # Get the highest precedence flag used in the dew point calculation
    my $dep_flag = $flag_precedence{$self->getTemperatureFlag()} >
	$flag_precedence{$self->getRelativeHumidityFlag()} ?
	    $self->getTemperatureFlag() : $self->getRelativeHumidityFlag();

    # Get the highest precedence flag between the dew point and
    # its dependency flag
    $flag =  $flag_precedence{$dep_flag} > $flag_precedence{$flag} ? 
	$dep_flag : $flag;

    # Max missing flag for dew point is incalcuable so change.
    $flag = $flag_precedence{$flag} > $flag_precedence{$INCALCUABLE_FLAG} ? 
	$INCALCUABLE_FLAG : $flag;

    # Make sure that if there is a value that it has a correct flag.
    return (defined($self->{"dew_pt"}) && $flag eq $INCALCUABLE_FLAG) &&
	$self->{"dew_pt"} ne $self->getMissing() ? $UNCHECKED_FLAG : $flag;
};

#---------------------------------------------------------------------------
# @signature int allValuesMissing()
# <p>Check to see if all of the values in the <code>Record</code> are 
# missing values.</p>
#
# @output $bool Return true if all the values are missing, false otherwise.
#---------------------------------------------------------------------------
our $allValuesMissing = sub {
    my $self = shift;
    if ($self->getPressure()         eq $self->getMissing() &&
	$self->getSeaLevelPressure() eq $self->getMissing() &&
	$self->getTemperature()      eq $self->getMissing() &&
	$self->getDewPoint()         eq $self->getMissing() &&
	$self->getWindSpeed()        eq $self->getMissing() &&
	$self->getWindDirection()    eq $self->getMissing() &&
	$self->getPrecip()           eq $self->getMissing() &&
	$self->getGustSpeed()        eq $self->getMissing() &&
	$self->getPresentWeather()   eq "-999"       &&
	$self->getVisibility()       eq $self->getMissing() &&
	$self->getCeilingHeight(1)   eq $self->getMissing() &&
	$self->getCeilingHeight(2)   eq $self->getMissing() &&
	$self->getCeilingHeight(3)   eq $self->getMissing() &&
	$self->getCloudAmount(1)     == 15           && 
	$self->getCloudAmount(2)     == 15           &&
	$self->getCloudAmount(3)     == 15) 
    { return 1; }
    else { return 0; }
};

#--------------------------------------------------------------------------
# @signature String checkDate(String name, String date)
# <p>Check a date to see if it is a real date.</p>
# 
# @input $name The name of the date being checked.
# @input $date The date in YYYY/MM/DD format.
# @output $warning The warning if the date was invalid, otherwise an empty
# string.
#--------------------------------------------------------------------------
our $checkDate = sub {
    my $self = shift;
    my $warning = "";

    if (!DpgDate::validDate($_[1],"YYYY/MM/DD")) {
	$warning .= sprintf("\t%s (%s) is not a valid date.\n", $_[0], $_[1]);
    }

    return $warning;
};

#--------------------------------------------------------------------------
# @signature String checkTime(String name, String time)
# <p>Check a time to see if it is a real time.</p>
#
# @input $name The name of the time being checked.
# @input $time The time in HH:MM format.
# @output $warning The warning if the time was invalid, otherwise the empty
# string.
#--------------------------------------------------------------------------
our $checkTime = sub {
    my $self = shift;
    my $warning = "";

    if (!DpgDate::validTime($_[1],"HH:MM")) {
	$warning .= sprintf("\t%s (%s) is not a valid time.\n", $_[0], $_[1]);
    }

    return $warning;
};

#--------------------------------------------------------------------------
# @signature String checkStation(String name, String value, String format, int length)
# <p>Check a station value to see if it fits into its specified field.</p>
#
# @input $name The name of the station paramter being checked.
# @input $value The value of the station parameter.
# @input $format The output format for the station parameter.
# @input $length The maximum length of the parameter allowed.
# @output $warning The warning generated by the parameter, otherwise the
# empty string.
#--------------------------------------------------------------------------
our $checkStation = sub {
    my $self = shift;
    my $string = sprintf($_[2], $_[1]);
    my $warning = "";

    # Check to see if the value fits in its provided space.
    if (length($string) > $_[3]) {
	$warning .= sprintf("\t%s is too big %s to fit into its field.\n", 
			    $_[0], $string)
    }

    return $warning;
};

#--------------------------------------------------------------------------
# @signature String limitCheck(String param, float value, float l_bound, float u_bound)
# <p>Check to see if the specified parameter is between the specified 
# upper and lower bounds.</p>
# 
# @input $param The name of the parameter being checked.
# @input $value The value of the parameter.
# @input $l_bound The lower bound for the parameter.
# @input $u_bound The upper bound for the parameter.
# @output $warning The warning generated by the value being out of the bounds,
# otherwise the empty string.
#--------------------------------------------------------------------------
our $limitCheck = sub {
    my $self = shift;
    my $warning = "";

    if (($_[1] < $_[2] || $_[1] > $_[3]) && $_[1] ne $_[4]) {
	$warning .= sprintf("\t%s has value (%s) and was expected to be between %s and %s.\n", $_[0], $_[1], $_[2], $_[3]);
    }
    
    return $warning;
};

#--------------------------------------------------------------------------
# @signature String checkParameter(String name, float value, String flag, String format, int length)
# <p>Check a parameter to see if it fits in the space alloted for it.  Check
# to see if the flag is a valid flag and if it matches the value.</p>
#
# @input $name The name of the parameter being checked.
# @input $value The value of the parameter.
# @input $flag The flag associated with the value for the parameter.
# @input $format The format used for the parameter value.
# @input $length The maximum length of the value in its string format.
# @output $warning Any warnings that were created by the parameter, otherwise
# the empty string.
#--------------------------------------------------------------------------
our $checkParameter = sub {
    my $self = shift;
    my $string = sprintf($_[3], $_[1]);
    my $warning = "";

    # Check to see if the value fits in its provided space.
    if (length($string) > $_[4]) {
	$warning .= sprintf("\t%s is too big %s to fit into its field.\n", 
			    $_[0], $string)
    }

    # Check to see if the flag is an expected flag.
    if (length($_[2]) != 1 ||
	($_[2] ne $MISSING_FLAG         && 
	 $_[2] ne $INCALCUABLE_FLAG     &&
	 $_[2] ne $GLITCH_FLAG          && 
	 $_[2] ne $NEG_PRECIP_FLAG      &&
	 $_[2] ne $NO_READING_FLAG      && 
	 $_[2] ne $VALUE_DOES_NOT_FIT_FLAG &&
	 $_[2] ne $BAD_FLAG             && 
	 $_[2] ne $GOOD_FLAG            &&
	 $_[2] ne $DUBIOUS_FLAG         &&
         $_[2] ne $ESTIMATE_FLAG        && 
	 $_[2] ne $TRACE_PRECIP_FLAG    &&
	 $_[2] ne $UNCHECKED_FLAG)) {
	$warning .= sprintf("\t%s has an invalid flag (%s).\n", $_[0], $_[2]);
    }

    # Check for missing value with non missing flags or vice versa.
    if ($_[1] eq $_[5] &&
	($_[2] ne $MISSING_FLAG         && 
	 $_[2] ne $INCALCUABLE_FLAG     &&
	 $_[2] ne $GLITCH_FLAG          && 
	 $_[2] ne $NEG_PRECIP_FLAG  &&
	 $_[2] ne $NO_READING_FLAG       && 
	 $_[2] ne $VALUE_DOES_NOT_FIT_FLAG)) {
	$warning .= sprintf("\t%s has a missing value (%s) but not a missing flag (%s).\n", $_[0], $string, $_[2]);
    } elsif ($_[1] ne $_[5] &&
	     ($_[2] eq $BAD_FLAG         &&
	      $_[2] eq $TRACE_PRECIP_FLAG &&
	      $_[2] eq $DUBIOUS_FLAG     && 
	      $_[2] eq $UNCHECKED_FLAG   &&
	      $_[2] eq $ESTIMATE_FLAG)) {
	$warning .= sprintf("\t%s has a non-missing value (%s) but has a missing flag (%s).\n", $_[0], $string, $_[2]);
    }

    return $warning;
};

#--------------------------------------------------------------------------
# @signature String checkForErrors()
# <p>Find all of the errors/warnings for the record and print them to
# the warning file.</p>
#--------------------------------------------------------------------------
our $checkForErrors = sub {
    my $self = shift;
    my $warning = "";

    # Check Dates and Times
    $warning .= $self->$checkDate("NOMINAL_DATE", $self->getNominalDate());
    $warning .= $self->$checkTime("NOMINAL_TIME", $self->getNominalTime());
    $warning .= $self->$checkDate("ACTUAL_DATE", $self->getActualDate());
    $warning .= $self->$checkTime("ACTUAL_TIME", $self->getActualTime());

    # Check Network/Station Data
    $warning .= $self->$checkStation("NETWORK_ID", $self->getNetworkId(),
				     "%-10s", 10);
    $warning .= $self->$checkStation("STATION_ID", $self->getStationId(),
				     "%-15s", 15);
    $warning .= $self->$checkStation("LATITUDE", $self->getLatitude(),
				     "%10.5f", 10);
    $warning .= $self->$limitCheck("LATITUDE", $self->getLatitude(),
				   -90, 90, $self->getMissing());
    $warning .= $self->$checkStation("LONGITUDE", $self->getLongitude(),
				     "%11.5f", 11);
    $warning .= $self->$limitCheck("LONGITUDE", $self->getLongitude(),
				   -180, 180, $self->getMissing());
    $warning .= $self->$checkStation("OCCURENCE", $self->getOccurence(),
				     "%3d", 3);
    $warning .= $self->$checkStation("ELEVATION", $self->getElevation(),
				     "%7.2f", 7);
    $warning .= $self->$limitCheck("ELEVATION", $self->getElevation(),
				   -200, 9000, $self->getMissing());
				     
    # Check PRESSURES
    $warning .= $self->$checkParameter("STATION_PRESSURE",$self->getPressure(),
				       $self->getPressureFlag(), "%7.2f", 7,
				       $self->getMissing());
    $warning .= $self->$limitCheck("STATION_PRESSURE", $self->getPressure(),
				   300, 1200, $self->getMissing());
    $warning .= $self->$checkParameter("SEA_LEVEL_PRESSURE",
				       $self->getSeaLevelPressure(), 
				       $self->getSeaLevelPressureFlag(),
				       "%7.2f", 7, $self->getMissing());
    $warning .= $self->$limitCheck("SEA_LEVEL_PRESSURE",
				   $self->getSeaLevelPressure(),
				   800, 1200, $self->getMissing());
    $warning .= $self->$checkParameter("CALCULATED_SEA_LEVEL_PRESSURE",
				       $self->getCalcSeaLevelPressure(),
				       $self->getCalcSeaLevelPressureFlag(),
				       "%7.2f", 7, $self->getMissing());
    $warning .= $self->$limitCheck("CALCULATED_SEA_LEVEL_PRESSURE",
				   $self->getCalcSeaLevelPressure(),
				   800, 1200, $self->getMissing());


    # Check TEMP DEW_PT, RH
    $warning .= $self->$checkParameter("TEMPERATURE", $self->getTemperature(),
				       $self->getTemperatureFlag(),"%7.2f", 7,
				       $self->getMissing());
    $warning .= $self->$limitCheck("TEMPERATURE", $self->getTemperature(),
				   -100, 100, $self->getMissing());
    $warning .= $self->$checkParameter("DEW_POINT", $self->getDewPoint(),
				       $self->getDewPointFlag(), "%7.2f", 7,
				       $self->getMissing());
    $warning .= $self->$limitCheck("DEW_POINT", $self->getDewPoint(),
				   -100, 100, $self->getMissing());
    $warning .= $self->$limitCheck("RELATIVE_HUMIDITY",
				   $self->getRelativeHumidity(),
				   0, 104, $self->getMissing());

    # Check the WIND stuff
    $warning .= $self->$checkParameter("WIND_SPEED", $self->getWindSpeed(),
				       $self->getWindSpeedFlag(), "%7.2f", 7,
				       $self->getMissing());
    $warning .= $self->$limitCheck("WIND_SPEED", $self->getWindSpeed(),
				   0, 200, $self->getMissing());
    $warning .= $self->$checkParameter("WIND_DIRECTION", 
				       $self->getWindDirection(),
				       $self->getWindDirectionFlag(),
				       "%7.2f", 7, $self->getMissing());
    $warning .= $self->$limitCheck("WIND_DIRECTION", $self->getWindDirection(),
				   0, 360, $self->getMissing());
    $warning .= $self->$checkParameter("GUST_SPEED", $self->getGustSpeed(),
				       $self->getGustSpeedFlag(), "%7.2f", 7,
				       $self->getMissing());
    $warning .= $self->$limitCheck("GUST_SPEED", $self->getGustSpeed(),
				    0, 9999.99, $self->getMissing());
    if ($self->getWindSpeed() eq $self->getMissing() &&
	$self->getGustSpeed() ne $self->getMissing()) {
	$warning .= sprintf("GUST_SPEED (%s) is not missing with a missing wind speed.\n", $self->getGustSpeed());
    } elsif ($self->getWindSpeed() ne $self->getMissing() &&
	     $self->getGustSpeed() ne $self->getMissing() &&
	     $self->getWindSpeed() > $self->getGustSpeed()) {
	$warning .= sprintf("GUST_SPEED (%s) is less than the WIND_SPEED (%s).\n", $self->getGustSpeed(), $self->getWindSpeed());
    }

    # Check PRECIP, WX, VIS
    $warning .= $self->$checkParameter("PRECIPITATION",
				       $self->getPrecip(), 
				       $self->getPrecipFlag(), "%7.2f", 7,
				       $self->getMissing());
    $warning .= $self->$checkParameter("PRESENT_WEATHER",
				       $self->getPresentWeather(),
				       $self->getPresentWeatherFlag(),
				       "%4d", 4, -999);
    $warning .= $self->$limitCheck("PRESENT_WEATHER",
				   $self->getPresentWeather(), 0, 999, -999);
    $warning .= $self->$checkParameter("VISIBILITY", $self->getVisibility(),
				       $self->getVisibilityFlag(), "%8.2f", 8,
				       $self->getMissing());
    $warning .= $self->$limitCheck("VISIBILITY", $self->getVisibility(),
				   0, 160000, $self->getMissing());

    # Check the CEILING HEIGHT and CLOUDS
    my $i; for($i = 1; $i < 4; $i++) {
	$warning .= $self->$checkParameter("CEILING_HEIGHT ".$i,
					   $self->getCeilingHeight($i),
					   $self->getCeilingHeightFlag($i),
					   "%7.2f", 7, $self->getMissing());
	$warning .= $self->$limitCheck("CEILING_HEIGHT ".$i,
				       $self->getCeilingHeight($i), 0,
				       9999.99, $self->getMissing());
	if (($self->getCeilingHeight($i) eq $self->getMissing() &&
	     $self->getCeilingHeightCode($i) != 15 &&
	     $self->getCeilingHeightCode($i) != 2) ||
	    ($self->getCeilingHeight($i) ne $self->getMissing() &&
	     $self->getCeilingHeightCode($i) == 15)) {
	    $warning .= sprintf("CEILING_HEIGHT %d, (%s) does not match the CEILING_CODE %d (%d).\n", $i, $self->getCeilingHeight($i), $i, $self->getCeilingHeightCode($i));
	}
	$warning .= $self->$checkParameter("CEILING_CODE ".$i,
					   $self->getCeilingHeightCode($i),
					   $self->getCeilingHeightFlag($i),
					   "%2d", 2, 15);
	$warning .= $self->$limitCheck("CEILING_CODE ".$i,
				       $self->getCeilingHeightCode($i),
				       0, 15, 15);
	$warning .= $self->$checkParameter("CLOUD CODE ".$i,
					   $self->getCloudAmount($i),
					   $self->getCloudAmountFlag($i),
					   "%2d", 2, 15);
	$warning .= $self->$limitCheck("CLOUD CODE ".$i,
				       $self->getCloudAmount($i), 0, 15, 15);
    }

    my $WARN = $self->{"warn"};
    if ($warning ne "") {
	printf($WARN "Station %s in network %s at actual time %s on %s had the following warnings:\n", $self->getStationId(), $self->getNetworkId(), $self->getActualTime(), $self->getActualDate());
	print($WARN $warning);
    }
};

##------------------------------------------------------------------------
# @signature float getCeilingHeight(int level)
# <p>Get the ceiling height for the specified level.</p>
#
# @input $level The ceiling height level
# @output $height The ceiling height value for the level, default -999.99
##-------------------------------------------------------------------------
sub getCeilingHeight {
    my $self = shift;
    return $self->checkDefined($self->{"ceil_ht_".$_[0]}, $self->getMissing());
}

##-----------------------------------------------------------------------
# @signature int getCeilingHeightCode(int level)
# <p>Get the code value for the specified ceiling height.</p>
#
# @input $level The ceiling height level
# @output $code The code value for the specified level, default 15
##-----------------------------------------------------------------------
sub getCeilingHeightCode {
  my $self = shift;
  return $self->checkDefined($self->{"ceil_ht_code_".$_[0]}, 15);
}

##-----------------------------------------------------------------------
# @signature String getCeilingHeightFlag(int level)
# <p>Get the flag for the specified ceiling height.</p>
#
# @input $level The ceiling height level.
# @output $flag The flag for the specified level, default M
##-----------------------------------------------------------------------
sub getCeilingHeightFlag {
    my $self = shift;
    return $self->checkDefined($self->{"ceil_ht_".$_[0]."_flag"},
				$MISSING_FLAG);
}

##-----------------------------------------------------------------------
# @signature int getCloudAmount(int level)
# <p>Get the cloud amount code for the specified level.</p>
#
# @input $level The cloud level
# @output $code The cloud amount for the specified level, default 15
##-----------------------------------------------------------------------
sub getCloudAmount {
  my $self = shift;
  return $self->checkDefined($self->{"cloud_".$_[0]}, 15);
}

##-----------------------------------------------------------------------
# @signature String getCloudAmountFlag(int level)
# <p>Get the flag for the specified cloud level.</p>
#
# @input $level The cloud level
# @output $flag The flag for the specified level, default M
##-----------------------------------------------------------------------
sub getCloudAmountFlag {
    my $self = shift;
    return $self->checkDefined($self->{"cloud_".$_[0]."_flag"},
			       $MISSING_FLAG);
}

##-------------------------------------------------------------------------
# @signature String getFlag(String value, String flag)
# <p>Determine the flag for the value.</p>
#
# @input $value The value to find the flag for.
# @input $flag A flag value if it is already specified.
# @output $flag The flag for the value.
##-------------------------------------------------------------------------
sub getFlag {
    my $self = shift;
    if (defined($_[1])) { return $_[1]; }
    elsif ($_[0] eq $self->getMissing()) { return $MISSING_FLAG; }
    else { return $UNCHECKED_FLAG; }
}

##-------------------------------------------------------------------------
# @signature String getCalcSeaLevelPressureFlag()
# <p>Get the flag for the calculated sea level pressure field.</p>
#
# @output $flag The flag for the calculated sea level pressure.
##-------------------------------------------------------------------------
sub getCalcSeaLevelPressureFlag {
    my $self = shift;
    my %flag_precedence = getFlagPrecedence();

    if (defined($self->{"calc_slp_flag"})) {
	return $self->{"calc_slp_flag"};
    }

    # Get the maximum dependence flag
    my $dep_flag = $flag_precedence{$self->getPressureFlag()} >
	$flag_precedence{$self->getDewPointFlag()} ? $self->getPressureFlag() :
	    $self->getDewPointFlag();
    $dep_flag = $flag_precedence{$dep_flag} > 
	$flag_precedence{$self->getTemperatureFlag()} ? $dep_flag :
	    $self->getTemperatureFlag();

    # Set the default value of the CSLP flag
    my $flag = $self->getCalcSeaLevelPressure() eq $self->getMissing() ?
	$INCALCUABLE_FLAG : $UNCHECKED_FLAG;
    
    # Determine the CSLP flag
    $flag = $flag_precedence{$flag} > $flag_precedence{$dep_flag} ?
	$flag : $dep_flag;

    # Any missing value for CSLP should be the incalcuable flag
    return $flag_precedence{$flag} > 
	$flag_precedence{$INCALCUABLE_FLAG} ?
	    $INCALCUABLE_FLAG : $flag;
}

##-------------------------------------------------------------------------
# @signature String getDewPointFlag()
# <p>Get the flag for the dew point.</p>
#
# @output $flag The flag for the dew point.
##-------------------------------------------------------------------------
sub getDewPointFlag {
    my $self = shift;
    return $self->checkDefined($self->{"dew_pt_flag"},
			       $self->$getDewPointFlagPrecedence());
}

##----------------------------------------------------------------------
# @signature String getGustMarker()
# <p>Get the gust marker for the record.</p>
#
# @output $mark The gust marker, default " "
##----------------------------------------------------------------------
sub getGustMarker {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getGustMarker\n"); }

    if ($self->getGustSpeed() ne $self->getMissing()) { return "G"; }
    else { return " "; }
}

##-------------------------------------------------------------------------
# @signature String getGustSpeedFlag()
# <p>Get the flag for the gust speed.</p>
# 
# @output $flag The flag for the gust wind speed.
##-------------------------------------------------------------------------
sub getGustSpeedFlag {
    my $self = shift;
    return $self->checkDefined($self->{"gust_spd_flag"},
			       ($self->getGustSpeed() eq 
				$self->getMissing()) ?
			       $MISSING_FLAG : 
			       $UNCHECKED_FLAG);
}

##-------------------------------------------------------------------------
# @signature String getPrecipFlag()
# <p>Get the flag for the precipitation.</p>
#
# @output $flag The flag for the precipitation.
##-------------------------------------------------------------------------
sub getPrecipFlag {
    my $self = shift;
    return $self->checkDefined($self->{"precip_flag"},
			       ($self->getPrecip() eq 
				$self->getMissing()) ?
			       $MISSING_FLAG : 
			       $UNCHECKED_FLAG);
}

##-------------------------------------------------------------------------
# @signature String getPresentWeatherFlag()
# <p>Get the flag for the present weather conditions.</p>
#
# @output $flag The flag for the present weather conditions.
##-------------------------------------------------------------------------
sub getPresentWeatherFlag {
    my $self = shift;
    return $self->checkDefined($self->{"present_wx_flag"},
			       ($self->getPresentWeather() eq "-999") ?
				$MISSING_FLAG : 
				$UNCHECKED_FLAG);
}

##-------------------------------------------------------------------------
# @signature String getPressureFlag()
# <p>Get the flag for the station pressure.</p>
#
# @output $flag The flag for the station pressure.
##-------------------------------------------------------------------------
sub getPressureFlag {
    my $self = shift;
    return $self->checkDefined($self->{"pressure_flag"},
			       ($self->getPressure() eq $self->getMissing()) ?
			       $MISSING_FLAG : 
			       $UNCHECKED_FLAG);
}

##-------------------------------------------------------------------------
# @signature String getRelativeHumidityFlag()
# <p>Get the flag for the relative humidity.</p>
#
# @output $flag The flag for the relative humidity.
##-------------------------------------------------------------------------
sub getRelativeHumidityFlag {
    my $self = shift;
    return $self->checkDefined($self->{"rel_humid_flag"},
			       ($self->getRelativeHumidity() eq 
				$self->getMissing()) ?
			       $MISSING_FLAG : 
			       $UNCHECKED_FLAG);
}

##-------------------------------------------------------------------------
# @signature String getSeaLevelPressureFlag()
# <p>Get the flag for the sea level pressure.</p>
#
# @output $flag The flag for the sea level pressure.
##-------------------------------------------------------------------------
sub getSeaLevelPressureFlag {
    my $self = shift;
    return $self->checkDefined($self->{"sl_press_flag"},
			       ($self->getSeaLevelPressure() eq 
				$self->getMissing()) ?
			       $MISSING_FLAG : 
			       $UNCHECKED_FLAG);    
}

##-------------------------------------------------------------------------
# @signature String getTemperatureFlag()
# <p>Get the flag for the temperature.</p>
#
# @output $flag The flag for the temperature.
##-------------------------------------------------------------------------
sub getTemperatureFlag {
    my $self = shift;
    return $self->checkDefined($self->{"temp_flag"},
			       ($self->getTemperature() eq 
				$self->getMissing()) ?
			       $MISSING_FLAG : 
			       $UNCHECKED_FLAG);
}

##-------------------------------------------------------------------------
# @signature String getVisibilityFlag()
# <p>Get the flag for the visibility.</p>
#
# @output $flag The flag for the visibility.
##-------------------------------------------------------------------------
sub getVisibilityFlag {
    my $self = shift;
    return $self->checkDefined($self->{"visibility_flag"},
			       ($self->getVisibility() eq 
				$self->getMissing()) ?
			       $MISSING_FLAG : 
			       $UNCHECKED_FLAG);
}

##-------------------------------------------------------------------------
# @signature String getWindDirectionFlag()
# <p>Get the flag for the wind direction.</p>
#
# @output $flag The flag for the wind direction.
##-------------------------------------------------------------------------
sub getWindDirectionFlag {
    my $self = shift;
    return $self->checkDefined($self->{"wind_dir_flag"},
			       ($self->getWindDirection() eq 
				$self->getMissing()) ?
			       $MISSING_FLAG : 
			       $UNCHECKED_FLAG);
}

##-------------------------------------------------------------------------
# @signature String getWindSpeedFlag()
# <p>Get the flag for the wind speed.</p>
#
# @output $flag The flag for the wind speed.
##-------------------------------------------------------------------------
sub getWindSpeedFlag {
    my $self = shift;
    return $self->checkDefined($self->{"wind_spd_flag"},
			       ($self->getWindSpeed() eq 
				$self->getMissing()) ?
			       $MISSING_FLAG : 
			       $UNCHECKED_FLAG);
}

##-------------------------------------------------------------------------
# @signature void setCalcSeaLevelPressureFlag(String flag)
# <p>Set the flag for the calculated sea level pressure.</p>
#
# @input $flag The flag for the calculated sea level pressure.
##-------------------------------------------------------------------------
sub setCalcSeaLevelPressureFlag {
    my $self = shift;
    $self->{"calc_slp_flag"} = $_[0];
}    

##-------------------------------------------------------------------------
# @signature void setCeilingHeight(float height, int code, int level)
# <p>Set the ceiling height and code for the specified level.</p>
# 
# @input $height The height in hundreds of feet.
# @input $code The associated code for the height.
# @input $level The level of the ceiling height.
##-------------------------------------------------------------------------
sub setCeilingHeight {
    my $self = shift;
    $self->{"ceil_ht_".$_[2]} = $_[0];
    $self->{"ceil_ht_code_".$_[2]} = $_[1];
    $self->setCeilingHeightFlag($self->getFlag($_[0]), $_[2]);
}

##-------------------------------------------------------------------------
# @signature void setCeilingHeightFlag(String flag, int level)
# <p>Set the flag for the ceiling height at the specified level.</p>
# 
# @input $flag The flag for the ceiling height.
# @input $level The level of the ceiling height.
##-------------------------------------------------------------------------
sub setCeilingHeightFlag {
    my $self = shift;
    $self->{"ceil_ht_".$_[1]."_flag"} = $_[0];
}

##-------------------------------------------------------------------------
# @signature void setCloudAmount(int amt, int level)
# <p>Set the cloud amount for the specified level.</p>
#
# @input $amt The cloud amount code of the level.
# @input $level The level of the clouds.
##-------------------------------------------------------------------------
sub setCloudAmount {
    my $self = shift;
    $self->{"cloud_".$_[1]} = $_[0];
    if ($_[0] != 15) { 
	$self->{"cloud_".$_[1]."_flag"} = $UNCHECKED_FLAG;
    }
}

##-------------------------------------------------------------------------
# @signature void setCloudAmountFlag(String flag, int level)
# <p>Set the flag for the cloud amount for the specified level.</p>
#
# @input $flag The flag for the cloud amount.
# @input $level The level of the clouds.
##-------------------------------------------------------------------------
sub setCloudAmountFlag {
    my $self = shift;
    $self->{"cloud_".$_[1]."_flag"} = $_[0];
}

##-------------------------------------------------------------------------
# @signature void setDewPoint(float dew_pt, String in_units)
# <p>Set the dew point value for the record.</p>
#
# @input $dew_pt The dew point value.
# @input $in_units The temperature units of the dew point.
##-------------------------------------------------------------------------
sub setDewPoint {
    my $self = shift;
    $self->{"dew_pt"} = DpgConversions::convertTemperature($_[0], $_[1], "C");
#    $self->setDewPointFlag($_[0] eq $self->getMissing() ? 
#			   $INCALCUABLE_FLAG :
#			   $UNCHECKED_FLAG);
}

##-------------------------------------------------------------------------
# @signature void setDewPointFlag(String flag)
# <p>Set the flag for the dew point.</p>
#
# @input $flag The flag for the dew point.
##-------------------------------------------------------------------------
sub setDewPointFlag {
    my $self = shift;
    $self->{"dew_pt_flag"} = $_[0];
}

##-------------------------------------------------------------------------
# @signature void setGustSpeedFlag(String flag)
# <p>Set the flag for the gust speed.</p>
#
# @input $flag The flag for the gust speed.
##-------------------------------------------------------------------------
sub setGustSpeedFlag {
    my $self = shift;
    $self->{"gust_spd_flag"} = $_[0];
}

##-------------------------------------------------------------------------
# @signature void setPrecipFlag(String flag)
# <p>Set the flag for the precipitation.</p>
# 
# @input $flag The flag for the precipitation.
##-------------------------------------------------------------------------
sub setPrecipFlag {
    my $self = shift;
    $self->{"precip_flag"} = $_[0];
}

##-------------------------------------------------------------------------
# @signature void setPresentWeatherFlag(String flag)
# <p>Set the flag for the present weather conditions.</p>
#
# @input $flag The flag for the present weather conditions.
##-------------------------------------------------------------------------
sub setPresentWeatherFlag {
    my $self = shift;
    $self->{"present_wx_flag"} = $_[0];
}

##-------------------------------------------------------------------------
# @signature void setPressureFlag(String flag)
# <p>Set the flag for the station pressure.</p>
# 
# @input $flag The flag for the station pressure.
##-------------------------------------------------------------------------
sub setPressureFlag {
    my $self = shift;
    $self->{"pressure_flag"} =$_[0];
}

##-------------------------------------------------------------------------
# @signature void setRelativeHumidityFlag(String flag)
# <p>Set the flag for the relative humidity.</p>
#
# @input $flag The flag for the relative humidity.
##-------------------------------------------------------------------------
sub setRelativeHumidityFlag {
    my $self = shift;
    $self->{"rel_humid_flag"} = $_[0];
}

##-------------------------------------------------------------------------
# @signature void setSeaLevelPressureFlag(String flag)
# <p>Set the flag for the sea level pressure.</p>
#
# @input $flag The flag for the sea level pressure.
##-------------------------------------------------------------------------
sub setSeaLevelPressureFlag {
    my $self = shift;
    $self->{"sl_press_flag"} = $_[0];
}    

##-------------------------------------------------------------------------
# @signature void setTemperatureFlag(String flag)
# <p>Set the flag for the temperature.</p>
#
# @input $flag The flag for the temperature.
##-------------------------------------------------------------------------
sub setTemperatureFlag {
    my $self = shift;
    $self->{"temp_flag"} = $_[0];
}

##-------------------------------------------------------------------------
# @signature void setVisibilityFlag(String flag)
# <p>Set the flag for the visibility.</p>
# 
# @input $flag The flag for the visibility.
##-------------------------------------------------------------------------
sub setVisibilityFlag {
    my $self = shift;
    $self->{"visibility_flag"} = $_[0];
}

##-------------------------------------------------------------------------
# @signature void setWindDirectionFlag(String flag)
# <p>Set the flag for the wind direction.</p>
# 
# @input $flag The flag for the wind direction.
##-------------------------------------------------------------------------
sub setWindDirectionFlag {
    my $self = shift;
    $self->{"wind_dir_flag"} = $_[0];
}

##-------------------------------------------------------------------------
# @signature void setWindSpeedFlag(String flag)
# <p>Set the flag for the wind speed.</p>
#
# @input $flag The flag for the wind speed.
##-------------------------------------------------------------------------
sub setWindSpeedFlag {
    my $self = shift;
    $self->{"wind_spd_flag"} = $_[0];
}

##-------------------------------------------------------------------------
# @signature String toQCF_String(int warn)
# <p>Get the QCF string of the record.</p>
#
# @input $warn A boolean int to check for record warnings.
# @output $qcf The record in QCF format or a blank line if all of the values
# are missing.
# @warning This will automatically check for errors and warnings and print
# them to the warning file.
##-------------------------------------------------------------------------
sub toQCF_String {
    my $self = shift;
    my $warn = shift;
    $warn = 1 if (!defined($warn));
    
    if (!$self->$allValuesMissing()) {
	$self->$checkForErrors() if($warn);

	return sprintf ("%-8s %-5s %-8s %-5s %-10s %-15s %10.5f %11.5f %3d %7.2f %7.2f %s %7.2f %s %7.2f %s %7.2f %s %7.2f %s %7.2f %s %7.2f %s %7.2f %s %s %7.2f %s %4d %s %8.2f %s %7.2f %2d %s %2d %s %7.2f %2d %s %2d %s %7.2f %2d %s %2d %s\n",
			$self->getNominalDate(),
			$self->getNominalTime(),
			$self->getActualDate(),
			$self->getActualTime(),
			$self->getNetworkId(),
			$self->getStationId(),
			$self->getLatitude(),
			$self->getLongitude(),
			$self->getOccurence(),
			$self->getElevation(),
			$self->getPressure(),
			$self->getPressureFlag(),
			$self->getSeaLevelPressure(),
			$self->getSeaLevelPressureFlag(),
			$self->getCalcSeaLevelPressure(),
			$self->getCalcSeaLevelPressureFlag(),
			$self->getTemperature(),
			$self->getTemperatureFlag(),
			$self->getDewPoint(),
			$self->getDewPointFlag(),
			$self->getWindSpeed(),
			$self->getWindSpeedFlag(),
			$self->getWindDirection(),
			$self->getWindDirectionFlag(),
			$self->getPrecip(),
			$self->getPrecipFlag(),
			$self->getGustMarker(),
			$self->getGustSpeed(),
			$self->getGustSpeedFlag(),
			$self->getPresentWeather(),
			$self->getPresentWeatherFlag(),
			$self->getVisibility(),
			$self->getVisibilityFlag(),
			$self->getCeilingHeight(1),
			$self->getCeilingHeightCode(1),
			$self->getCeilingHeightFlag(1),
			$self->getCloudAmount(1),
			$self->getCloudAmountFlag(1),
			$self->getCeilingHeight(2),
			$self->getCeilingHeightCode(2),
			$self->getCeilingHeightFlag(2),
			$self->getCloudAmount(2),
			$self->getCloudAmountFlag(2),
			$self->getCeilingHeight(3),
			$self->getCeilingHeightCode(3),
			$self->getCeilingHeightFlag(3),
			$self->getCloudAmount(3),
			$self->getCloudAmountFlag(3));
    } else {
	my $WARN = $self->{"warn"};
	printf($WARN "Record for %s %s at %s %s was dropped because all values were missing.\n", $self->getStationId(), $self->getNetworkId(), $self->getNominalDate(), $self->getNominalTime());
	return "";
    }
}






















