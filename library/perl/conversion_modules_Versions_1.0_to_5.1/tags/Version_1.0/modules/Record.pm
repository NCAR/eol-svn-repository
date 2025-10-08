#! /usr/bin/perl -w

##Module---------------------------------------------------------------
# <p>This is a class that holds a reading from a <code>Station</code>.</p>
#
# <p><b>Intended Use: </b>The intended use, as seen by the author of the
# module, is to make converting a raw data record to a QCF record quickly with
# relative simplicity and flexibility.  It tries to allow the person writing
# the conversion to minimize the number of tedious details that are common
# to most conversions.</p>
#
# @use  use lib "work/DPG_HTML/BEST_SW/conversion_modules/Version1";
#       use Record;
# @author Joel Clawson
# @version 0.01 Original Version
##Module---------------------------------------------------------------

package Record;

use strict;
use lib ".";
use Conversions;
use Station;

#------------------------------------------------------------------------
# This is a private function used to add a warning to the warning log for
# the Record.
#
# @input $id What the warning is for (temperature, wind speed, etc)
# @input $value The value that is causing the warning.
# @input $why What is being done about the value.
# @input $reason The explanation for the $why.
#------------------------------------------------------------------------
our $appendWarning = sub {
    my $self = shift;
    my $id = shift;
    my $value = shift;
    my $reason = shift;

    my $actTime = $self->getActualDate()." ".$self->getActualTime();
    my $nomTime = $self->getNominalDate()." ".$self->getNominalTime();
    my $recTime = "";
    if (defined($self->{"record_date"})) {
	$recTime = $self->{"record_date"}." ".$self->{"record_time"};
    }

    my $warning = $id." value of ".$value." in station: ".$self->getStationId()." in network: ".$self->getNetworkId()." at actual time ".$actTime.", nominal time ".$nomTime.", recorded time ".$recTime." has caused a warning because ".$reason.".\n";

    if (defined($self->{"warning"})) { 
	$self->{"warning"} = $self->{"warning"}.$warning;
    } else {
	$self->{"warning"} = $warning;
    }
};

##------------------------------------------------------------------------
# @signature void checkGustSpeed()
# <p>Check to see if the gust speed has a value when there is no wind speed
# value and check to see if the wind speed is greater than the gust speed.</p>
##------------------------------------------------------------------------
our $checkGustSpeed = sub {
    my $self = shift;
    my $gust = $self->getGustSpeed();
    my $wind = $self->getWindSpeed();
    my $missing = Conversions::getMissing();
    
    if ($wind eq $missing && $gust ne $missing) {
	$self->$appendWarning("Gust Speed", $gust, 
			      "is not missing with wind speed value missing");
    } elsif ($gust ne $missing && $wind ne $missing && $wind > $gust) {
	$self->$appendWarning("Wind Speed", $wind, "wind speed is greater " .
			      "than the gust speed ($gust)");
    }
};

#-------------------------------------------------------------------------
# Print a fatal error to STDERR and then kill the conversion.
#
# @input $stars A string of stars to print around ERROR
# @input $id The id type of the value (temperature, wind_speed, etc)
# @input $value The value that is causing the error.
# @input $reason Why the value is a fatal error.
# @warning This function terminates the conversion after printing an error.
#--------------------------------------------------------------------------
our $printErrorAndDie = sub {
    my $self = shift;
    my $stars = shift;
    my $id = shift;
    my $value = shift;
    my $reason = shift;

    my $actTime = $self->getActualDate()." ".$self->getActualTime();
    my $nomTime = $self->getNominalDate()." ".$self->getNominalTime();
    my $recTime = $self->{"record_date"}." ".$self->{"record_time"};

    my $warning = $stars." ERROR ".$stars." ".$id." value of ".$value." in station: ".$self->getStationId()." in network: ".$self->getNetworkId()." at actual time ".$actTime.", nominal time ".$nomTime.", recorded time ".$recTime." has caused an error because ".$reason.".\n";

    print(STDERR $warning);
    die();
};

#---------------------------------------------------------------------------
# Check to see if the flag is one character and it is one of the recognized
# QCF flags.
#
# @input $id The type of the value (temperature, wind_speed, etc)
# @input $flag The flag to be checked.
# @warning The function will die if the tests fail.
#---------------------------------------------------------------------------
our $checkFlagSize = sub {
    my $self = shift;
    my $id = shift;
    my $flag = shift;
    if (length($flag) != 1) {
	$self->$printErrorAndDie("**", $id, $flag,
				 "the flag is not one character");
    } elsif ($flag ne Conversions::getBadFlag() && 
	     $flag ne Conversions::getDubiousFlag() &&
	     $flag ne Conversions::getEstimateFlag() &&
	     $flag ne Conversions::getGlitchFlag() &&
	     $flag ne Conversions::getIncalcuableFlag() &&
	     $flag ne Conversions::getMissingFlag() &&
	     $flag ne Conversions::getNegativePrecipFlag() &&
	     $flag ne Conversions::getNoReadingFlag() &&
	     $flag ne Conversions::getTracePrecipFlag() &&
	     $flag ne Conversions::getUncheckedFlag()) {
	$self->$printErrorAndDie("**", $id, $flag,
				 "the flag is not a recognized flag");
    }
};

#--------------------------------------------------------------------------
# Check to see if a value fits in the specified space for the value.
# 
# @input $id What value is being checked (temperature, wind speed, etc.)
# @input $value The value that is being checked.
#--------------------------------------------------------------------------
our $checkIfFits = sub {
    my $self = shift;
    my $id = shift;
    my $value = shift;

    if ($id =~ m/ceiling_height_code/ || $id =~ m/cloud_amount/) {
	if ($value < -9 || $value > 99) {
	    $self->$printErrorAndDie("****", $id, $value,
				    "$value does not fit in the field");
	}
    } elsif ($id =~ m/ceiling_height/ ||
	$id =~ m/gust_speed/ || $id =~ m/wind_speed/ || 
	$id =~ m/wind_direction/ || $id =~ m/precip/ ||
	$id =~ m/dew_point/ || $id =~ m/pressure/ || $id =~ m/temperature/) {
	if ($value > 9999.99 || $value < -999.99) {
	    $self->$printErrorAndDie("****", $id, $value,
				    "$value does not fit in the field");
	}
    } elsif ($id =~ m/visibility/) {
	if ($value > 99999.99 || $value < -9999.99) {
	    $self->$printErrorAndDie("****", $id, $value,
				    "$value does not fit in the field");
	}
    } elsif ($id =~ m/present_weather/) {
	if ($value > 9999 || $value < -999) {
	    $self->$printErrorAndDie("****", $id, $value,
				    "$value does not fit in the field");
	}
    }
};

#----------------------------------------------------------------------
# Compare a value with to its flag.  Ensures that a flag for missing values
# matches the missing value and non-missing values match not missing flags.
# 
# @input $id The identifer of the type (temperature, wind_speed, etc)
# @input $value The value to be compared with the flag.
# @input $flag The flag to be compared with the value.
# @warning The method dies if the value does not match the flag.
#-----------------------------------------------------------------------
our $checkValueWithFlag = sub {
    my $self = shift;
    my $id = shift;
    my $value = shift;
    my $flag = shift;

    # Check for non missing flags
    if (($flag eq Conversions::getBadFlag() ||
	 $flag eq Conversions::getDubiousFlag() ||
	 $flag eq Conversions::getEstimateFlag() ||
	 $flag eq Conversions::getTracePrecipFlag() ||
	 $flag eq Conversions::getUncheckedFlag()) &&
	($value eq Conversions::getMissing() || 
	 (($value == -999 && $id =~ m/present_weather/) ||
	  ($value == 15 && ($id =~ m/ceiling_height_code/ ||
			    $id =~ m/cloud_amount/))))) {
	$self->$printErrorAndDie("***", $id, $value." ".$flag,
			  "the value $value does not match with flag $flag");
    } elsif (($flag eq Conversions::getGlitchFlag() ||
	      $flag eq Conversions::getIncalcuableFlag() ||
	      $flag eq Conversions::getMissingFlag() ||
	      $flag eq Conversions::getNegativePrecipFlag() ||
	      $flag eq Conversions::getNoReadingFlag() ||
	      $flag eq Conversions::getValueDoesNotFitFlag()) &&
	     $value ne Conversions::getMissing()) {
	if (($value eq -999 && $id =~ m/present_weather/) ||
	    ($value == 15 && ($id =~ m/ceiling_height_code/ ||
			      $id =~ m/cloud_amount/))) {
	    return;
	} else {
	    $self->$printErrorAndDie("***", $id, $value." ".$flag,
			  "the value $value does not match with flag $flag");
	}
    }
};

#-------------------------------------------
# Private function to set reporting frequency.
# Used to set nominal date and time.
#
# Input:  $freq The reporting frequency.
#-------------------------------------------
our $setReportingFrequency = sub {
    my $self = shift;
    my $freq = shift;
    $self->{"frequency"} = $freq;
};

#--------------------------------------------
# Get the reporting frequency.
#
# Output: Returns the reporting frequency.
#--------------------------------------------
our $getReportingFrequency = sub {
    my $self = shift;
    return $self->{"frequency"};
};

#&main();

##---------------------------------------------------------------
# @signature String getActualDate()
# <p>Get the actual date of the reading in YYYY/MM/DD format.</p>
#
# @output $act_date Returns the actual date of the reading.
##---------------------------------------------------------------
sub getActualDate {
    my $self = shift;
    if (defined($self->{"act_date"})) {
	return $self->{"act_date"};
    } else {
	return "9999/99/99";
    }
}

##---------------------------------------------------------------
# @signature String getActualTime()
# <p>Get the actual time of the reading in HH:MM format.</p>
#
# @output $act_time Returns the actual time of the reading.
##---------------------------------------------------------------
sub getActualTime {
    my $self = shift;
    if (defined($self->{"act_time"})) {
	return $self->{"act_time"};
    } else {
	return "99:99";
    }
}

##---------------------------------------------------------------
# @signature float getCalcSeaLevelPressure()
# <p>Get the calculated sea level pressure for the reading.  If the calculated
# sea level pressure was not set to be calculated, the function will
# return the QCF missing value.</p>
#
# @link <a href=#setCalculateSeaLevelPressure(int calc)>setCalculateSeaLevelPressure(int calc)</a>
# @output $slp Returns the calculated sea level pressure for the reading.
##---------------------------------------------------------------
sub getCalcSeaLevelPressure {
    my $self = shift;
    if ($self->{"calc_sea_level_pressure"}) {
	my $calc = Conversions::calculateSeaLevelPressure($self->getPressure(),
						     $self->getElevation(),
						     $self->getDewPoint(),
						     $self->getTemperature(),
						     $self->getVerbose());
	if ($calc eq Conversions::getMissing()) {
	    $self->{"calc_sea_level_pressure_flag"} = 
	      Conversions::getIncalcuableFlag();
	} elsif ($calc ne Conversions::getMissing() &&
		 ($self->getPressureFlag() eq Conversions::getBadFlag() ||
		  $self->getDewPointFlag() eq Conversions::getBadFlag() ||
		  $self->getTemperatureFlag() eq Conversions::getBadFlag())) {
	    $self->{"calc_sea_level_pressure_flag"} =
	      Conversions::getBadFlag();
	} elsif ($calc ne Conversions::getMissing() &&
		 ($self->getPressureFlag() eq Conversions::getEstimateFlag() ||
		  $self->getDewPointFlag() eq Conversions::getEstimateFlag() ||
		  $self->getTemperatureFlag() eq 
		Conversions::getEstimateFlag())) {
	    $self->{"calc_sea_level_pressure_flag"} =
	      Conversions::getEstimateFlag();
	} else {
	    $self->{"calc_sea_level_pressure_flag"} = 
	      Conversions::getUncheckedFlag();	    
	}
	$self->{"calc_sea_level_pressure"} = $calc;
    } else {
	$self->{"calc_sea_level_pressure"} = 
	  Conversions::getMissing();
	$self->{"calc_sea_level_pressure_flag"} =
	  Conversions::getIncalcuableFlag();
    }
    return $self->{"calc_sea_level_pressure"};
}

##----------------------------------------------------------------
# @signature String getCalcSeaLevelPressureFlag()
# <p>Get the calculated sea level pressure flag for the reading.  Returns the
# QCF incalcuable flag if the flag has not been set.</p>
#
# @output $flag Returns the flag for the calculated sea level pressure.
##-----------------------------------------------------------------
sub getCalcSeaLevelPressureFlag {
    my $self = shift;
    if (defined($self->{"calc_sea_level_pressure_flag"})) {
	return $self->{"calc_sea_level_pressure_flag"};
    } else {
	return Conversions::getIncalcuableFlag();
    }
}

##-----------------------------------------------------------------
# @signature float getCeilingHeight(int level)
# <p>Get the ceiling height for a specified level for the Record.  Returns the
# QCF missing value if the ceiling height has not been set.</p>
#
# @input  $level The level of the ceiling height.
# @output $height Returns the ceiling height for the specified level.
##-----------------------------------------------------------------
sub getCeilingHeight {
    my $self = shift;
    my $level = shift;
    if (defined($self->{"ceiling_height_$level"})) {
	return $self->{"ceiling_height_$level"};
    } else {
	return Conversions::getMissing();
    }
}

##-----------------------------------------------------------------
# @signature int getCeilingHeightCode(int level)
# <p>Get the ceiling height code for a specified level of the Record.  Returns
# the missing ceiling height code value of 15 if the ceiling height code has
# not been set.</p>
# 
# @input $level The level of the ceiling height code.
# @output $code Returns the ceiling height code for the specified level.
##-----------------------------------------------------------------
sub getCeilingHeightCode {
    my $self = shift;
    my $level = shift;
    if (defined($self->{"ceiling_height_code_$level"})) {
	return $self->{"ceiling_height_code_$level"};
    } else {
	return 15;
    }
}

##------------------------------------------------------------------
# @signature String getCeilingHeightFlag(int level)
# <p>Get the flag for the ceiling height of a specified level.  Returns the
# QCF missing flag if the flag has not been set.</p>
#
# @input $level The level of the ceiling height flag.
# @output $flag Returns the flag for the specified ceiling height.
##------------------------------------------------------------------
sub getCeilingHeightFlag {
    my $self = shift;
    my $level = shift;
    if (defined($self->{"ceiling_height_$level"."_flag"})) {
	return $self->{"ceiling_height_$level"."_flag"};
    } else {
	return Conversions::getMissingFlag();
    }
}

##------------------------------------------------------------------
# @signature int getCloudAmount(int level)
# <p>Get the cloud amount for a specified level.  Returns the missing cloud
# amount value of 15 if the cloud amount has not been set.</p>
#
# @input $level The level of the cloud amount.
# @output $amt The cloud amount value.
##------------------------------------------------------------------
sub getCloudAmount {
    my $self = shift;
    my $level = shift;
    if (defined($self->{"cloud_amount_$level"})) {
	return $self->{"cloud_amount_$level"};
    } else {
	return 15;
    }
}

##------------------------------------------------------------------
# @signature String getCouldAmountFlag()
# <p>Get the flag for the cloud amount.  Returns the QCF missing flag if the
# flag was not set.</p>
#
# @input $level The level of the cloud amount
# @output $flag Returns the flag for the specified level's cloud amount.
##------------------------------------------------------------------
sub getCloudAmountFlag {
    my $self = shift;
    my $level = shift;
    if (defined($self->{"cloud_amount_$level"."_flag"})) {
	return $self->{"cloud_amount_$level"."_flag"};
    } else {
	return Conversions::getMissingFlag();
    }
}

##------------------------------------------------------------------
# @signature float getDewPoint()
# <p>Get the dew point for the reading.  If it was given it returns it, or
# it returns the calculated value if the flag was set.  Returns the QCF
# missing value if the dew point cannot be calculated or was not set.</p>
#
# @link <a href=#setCalculateDewPoint(int calc)>setCalculateDewPoint(int calc)</a>
# @output $dew_point Returns the dew point for the reading.
##------------------------------------------------------------------
sub getDewPoint {
    my $self = shift;
    if ($self->{"calc_dew_point"}) {
	my $calc = Conversions::calculateDewPoint($self->getTemperature(),
						  $self->getRelativeHumidity(),
						  $self->getVerbose());
	$self->{"calc_dew_point_value"} = $calc;
	if ($calc eq Conversions::getMissing()) {
	    $self->{"calc_dew_point_flag"} = Conversions::getIncalcuableFlag();
	} elsif ($calc ne Conversions::getMissing() && 
		 ($self->getTemperatureFlag() eq Conversions::getEstimateFlag()
		  || $self->{"rel_humidity_flag"} eq 
		Conversions::getEstimateFlag())) {
	    $self->{"calc_dew_point_flag"} = Conversions::getEstimateFlag();
	} elsif ($calc ne Conversions::getMissing() && 
		 ($self->getTemperatureFlag() eq Conversions::getBadFlag()
		  || $self->{"rel_humidity_flag"} eq 
		Conversions::getBadFlag())) {
	    $self->{"calc_dew_point_flag"} = Conversions::getBadFlag();
	} else {
	    $self->{"calc_dew_point_flag"} = Conversions::getUncheckedFlag();
	}
	return $calc;
    } elsif (defined($self->{"dew_point"})) {
	return $self->{"dew_point"};
    } else {
	return Conversions::getMissing();
    }
}

##------------------------------------------------------------------
# @signature String getDewPointFlag()
# <p>Get the flag for the dew point for the Record.  Returns the QCF missing
# flag if the flag has not been set unless the dew point is suppose to be
# calculated.  It then returns the QCF incalcuable flag.</p>
#
# @output $flag Returns the dew point flag for the reading.
##------------------------------------------------------------------
sub getDewPointFlag {
    my $self = shift;
    if ($self->{"calc_dew_point"} && defined($self->{"calc_dew_point_value"})){
	if (defined($self->{"calc_dew_point_flag"})) {
	    return $self->{"calc_dew_point_flag"};
	} else {
	    return Conversions::getIncalcuableFlag();
	}
    } else {
	if (defined($self->{"dew_point_flag"})) {
	    return $self->{"dew_point_flag"};
	} else {
	    return Conversions::getMissingFlag();
	}
    }
}

##------------------------------------------------------------------
# @signature float getElevation()
# <p>Get the elevation of the reading.</p>
#
# @output $elevation Returns the elevation of the reading.
##------------------------------------------------------------------
sub getElevation {
    my $self = shift;
    return $self->{"elevation"};
}

##-----------------------------------------------------------------
# @signature String getGustMarker()
# <p>Get the gust marker for the reading.</p>
#
# @output $marker Returns 'G' or ' ' for the gust marker.
##-----------------------------------------------------------------
sub getGustMarker {
    my $self = shift;
    if ($self->getGustSpeed() eq Conversions::getMissing() &&
	$self->getGustSpeedFlag() ne Conversions::getValueDoesNotFitFlag()) {
	return ' ';
    } else {
	return 'G';
    }
}

##----------------------------------------------------------------
# @signature float getGustSpeed()
# <p>Get the speed of the gust for the Record.  Returns the QCF missing value
# if the gust speed has not been set.</p>
#
# @output $gust_speed Returns the gust speed for the reading.
##----------------------------------------------------------------
sub getGustSpeed {
    my $self = shift;
    if (defined($self->{"gust_speed"})) {
	return $self->{"gust_speed"};
    } else {
	return Conversions::getMissing();
    }
}

##----------------------------------------------------------------
# @signature String getGustSpeedFlag()
# <p>Get the flag for the gust speed for the Record.  Returns the QCF 
# missing flag if the flag has not been set.</p>
#
# @output $flag Returns the flag for the gust speed.
##----------------------------------------------------------------
sub getGustSpeedFlag {
    my $self = shift;
    if (defined($self->{"gust_speed_flag"})) {
	return $self->{"gust_speed_flag"};
    } else {
	return Conversions::getMissingFlag();
    }
}

##----------------------------------------------------------------
# @signature float getLatitude()
# <p>Get the latitude of the reading.</p>
#
# @output $latitude Returns the latitude of the reading.
##----------------------------------------------------------------
sub getLatitude {
    my $self = shift;
    return $self->{"latitude"};
}

##---------------------------------------------------------------
# @signature float getLongitude()
# <p>Get the longitude of the reading.</p>
#
# @output $longitude Returns the longitude of the reading.
##---------------------------------------------------------------
sub getLongitude {
    my $self = shift;
    return $self->{"longitude"};
}

##---------------------------------------------------------------
# @signature String getNetworkId()
# <p>Get the identification value of the network for the reading.</p>
#
# @output $network_id Returns the network identifier.
##---------------------------------------------------------------
sub getNetworkId {
    my $self = shift;
    return $self->{"network_id"};
}

##---------------------------------------------------------------
# @signature String getNominalDate()
# <p>Get the nominal date of the reading in YYYY/MM/DD format.  Returns the
# actual date if the nominal date has not been set.</p>
#
# @output $nom_date Returns the nominal date of the reading.
##---------------------------------------------------------------
sub getNominalDate {
    my $self = shift;
    if (!defined($self->{"nom_date"})) {
	$self->{"nom_date"} = $self->getActualDate();
    }
    return $self->{"nom_date"};
}

##---------------------------------------------------------------
# @signature String getNominalTime()
# <p>Get the nominal time of the reading in HH:MM format.  Returns the 
# actual time if the nominal time has not been set.</p>
#
# @output $nom_time Returns the nominal time of the reading.
##---------------------------------------------------------------
sub getNominalTime {
    my $self = shift;
    if (!defined($self->{"nom_time"})) {
	$self->{"nom_time"} = $self->getActualTime();
    }
    return $self->{"nom_time"};
}

##---------------------------------------------------------------
# @signature int getOccurence()
# <p>Get the occurence of the reading.</p>
#
# @output $occurence Returns the occurence of the reading.
##---------------------------------------------------------------
sub getOccurence {
    my $self = shift;
    return $self->{"occurence"};
}

##---------------------------------------------------------------
# @signature float getPrecip()
# <p>Get the precipitation for the Record.  Returns the QCF missing value
# if the precipitation has not been set.</p>
#
# @output $precip Returns the precipitation of the reading.
##---------------------------------------------------------------
sub getPrecip {
    my $self = shift;
    if (defined($self->{"precip"})) {
	return $self->{"precip"};
    } else {
	return Conversions::getMissing();
    }
}

##---------------------------------------------------------------
# @signature String getPrecipFlag()
# <p>Get the flag for the precip value of the Record.  Returns the QCF 
# missing flag if the flag has not been set.</p>
# 
# @output $flag Returns the flag for the precipitation.
##---------------------------------------------------------------
sub getPrecipFlag {
    my $self = shift;
    if (defined($self->{"precip_flag"})) {
	return $self->{"precip_flag"};
    } else {
	return Conversions::getMissingFlag();
    }
}

##---------------------------------------------------------------
# @signature int getPresentWeather()
# <p>Get the present weather conditions for the Record.  Returns the weather
# missing value of -999 if the present weather has not been set.</p>
#
# @output $present_weather Returns the present weather conditions.
##---------------------------------------------------------------
sub getPresentWeather {
    my $self = shift;
    if (defined($self->{"present_weather"})) {
	return $self->{"present_weather"};
    } else {
	return -999;
    }
}

##---------------------------------------------------------------
# @signature String getPresentWeatherFlag()
# <p>Get the flag for the present weather conditions.  Returns the QCF missing
# flag if the flag has not been set.</p>
#
# @output $flag Returns the flag for the present conditions.
##---------------------------------------------------------------
sub getPresentWeatherFlag {
    my $self = shift;
    if (defined($self->{"present_weather_flag"})) {
	return $self->{"present_weather_flag"};
    } else {
	return Conversions::getMissingFlag();
    }
}

##---------------------------------------------------------------
# @signature float getPressure()
# <p>Get the station pressure for the Record.  Returns the QCF missing 
# value if the station pressure has not been set.</p>
#
# @output $pressure Returns the station pressure for the reading.
##---------------------------------------------------------------
sub getPressure {
    my $self = shift;
    if (defined($self->{"pressure"})) {
	return $self->{"pressure"};
    } else {
	return Conversions::getMissing();
    }
}

##---------------------------------------------------------------
# @signature String getPressureFlag()
# <p>Get the flag for the station pressure for the Record.  Returns the QCF
# missing flag if the flag has not been set.</p>
# 
# @output $flag Returns the flag for the station pressure.
##---------------------------------------------------------------
sub getPressureFlag {
    my $self = shift;
    if (defined($self->{"pressure_flag"})) {
	return $self->{"pressure_flag"};
    } else {
	return Conversions::getMissingFlag();
    }
}

##---------------------------------------------------------------
# @signature float getRelativeHumidity()
# <p>Get the relative humidity of the Record.  Returns the QCF missing value
# if the relative humidity has not been set.</p>
#
# @output $rel_humidity Returns the relative humidity of the reading.
##---------------------------------------------------------------
sub getRelativeHumidity {
    my $self = shift;
    if (defined($self->{"rel_humidity"})) {
	return $self->{"rel_humidity"};
    } else {
	return Conversions::getMissing();
    }
}

##---------------------------------------------------------------
# @signature float getSeaLevelPressure()
# <p>Get the sea level pressure of the Record.  Returns the QCF missing
# value if the sea level pressure has not been set.</p>
#
# @output $slpressure Returns the sea level pressure of the reading.
##---------------------------------------------------------------
sub getSeaLevelPressure {
    my $self = shift;
    if (defined($self->{"sea_pressure"})) {
	return $self->{"sea_pressure"};
    } else {
	return Conversions::getMissing();
    }
}

##---------------------------------------------------------------
# @signature String getSeaLevelPressureFlag()
# <p>Get the flag for the sea level pressure for the Record.  Returns the
# QCF missing flag if the flag has not been set.</p>
#
# @output $flag Returns the flag for the sea level pressure.
##---------------------------------------------------------------
sub getSeaLevelPressureFlag {
    my $self = shift;
    if (defined($self->{"sea_pressure_flag"})) {
	return $self->{"sea_pressure_flag"};
    } else {
	return Conversions::getMissingFlag();
    }
}

##----------------------------------------------------------------
# @signature String getStationId()
# <p>Gets the station identifier for the Record.</p>
#
# @output $station_id Returns the station identifier for the reading.
##---------------------------------------------------------------
sub getStationId {
    my $self = shift;
    return $self->{"station_id"};
}

##----------------------------------------------------------------
# @signature float getTemperature()
# <p>Get the temperature of the Record.  Returns the QCF missing value if the
# temperature has not been set.</p>
#
# @output $temperature Returns the temperature of the reading.
##----------------------------------------------------------------
sub getTemperature {
    my $self = shift;
    if (defined($self->{"temperature"})) {
	return $self->{"temperature"};
    } else {
	return Conversions::getMissing();
    }
}

##----------------------------------------------------------------
# @signature String getTemperatureFlag()
# <p>Get the flag for the temperature for the Record.  Returns the QCF missing
# flag if the flag was not set.</p>
#
# @output $flag Returns the flag for the temperature.
##----------------------------------------------------------------
sub getTemperatureFlag {
    my $self = shift;
    if (defined($self->{"temperature_flag"})) {
	return $self->{"temperature_flag"};
    } else {
	return Conversions::getMissingFlag();
    }
}

##---------------------------------------------------------------
# @signature int getVerbose()
# <p>Get the verbose flag.</p>
#
# @output: $verbose True if the verbose flag is set, false otherwise.
##---------------------------------------------------------------
sub getVerbose {
    my $self = shift;
    return $self->{"verbose"};
}

##----------------------------------------------------------------
# @signature float getVisibility()
# <p>Get the visibilty at the station for the Record.  Returns the QCF missing
# value if the visibility has not been set.</p>
#
# @output $visibility Returns the visibility at the time of the reading.
##----------------------------------------------------------------
sub getVisibility {
    my $self = shift;
    if (defined($self->{"visibility"})) {
	return $self->{"visibility"};
    } else {
	return Conversions::getMissing();
    }
}

##----------------------------------------------------------------
# @signature String getVisibilityFlag()
# <p>Get the flag for the visibility for the Record.  Returns the QCF missing
# flag if the visibility has not been set.</p>
#
# @output $flag Returns the flag for the visibility of the reading.
##----------------------------------------------------------------
sub getVisibilityFlag {
    my $self = shift;
    if (defined($self->{"visibility_flag"})) {
	return $self->{"visibility_flag"};
    } else {
	return Conversions::getMissingFlag();
    }
}

##---------------------------------------------------------------
# @signature String getWarnings()
# <p>Get the warnings generated for this record.  The warnings are for all of
# variables that have been set in the Record.  Each warning is on a single 
# line.  It returns an empty string if there are no warnings.</p>
#
# @output $warnings Returns the warnings for the record.
##---------------------------------------------------------------
sub getWarnings {
    my $self = shift;
    $self->$checkGustSpeed();
    if (defined($self->{"warning"})) {
	return $self->{"warning"};
    } else {
	return "";
    }
}

##---------------------------------------------------------------
# @signature float getWindDirection()
# <p>Get the wind direction of the reading if it has been set.  Returns the
# QCF missing value if the wind direction has not been set.</p>
#
# @output $wind_direction Returns the wind direction of the reading.
##---------------------------------------------------------------
sub getWindDirection {
    my $self = shift;
    if (defined($self->{"wind_direction"})) {
	return $self->{"wind_direction"};
    } else {
	return Conversions::getMissing();
    }
}

##---------------------------------------------------------------
# @signature String getWindDirectionFlag()
# <p>Get the flag for the wind direction if it has been set.  Returns the QCF
# missing flag if the wind direction flag has not been set.</p>
#
# @output $flag Returns the flag for the wind direction.
##---------------------------------------------------------------
sub getWindDirectionFlag {
    my $self = shift;
    if (defined($self->{"wind_direction_flag"})) {
	return $self->{"wind_direction_flag"};
    } else {
	return Conversions::getMissingFlag();
    }
}

##----------------------------------------------------------------
# @signature float getWindSpeed()
# <p>Get the wind speed of the reading if it has been set.  Returns the QCF
# missing value if the wind speed has not been set.</p>
#
# @output $wind_speed Returns the wind speed of the reading.
##----------------------------------------------------------------
sub getWindSpeed {
    my $self = shift;
    if (defined($self->{"wind_speed"})) {
	return $self->{"wind_speed"};
    } else {
	return Conversions::getMissing();
    }
}

##----------------------------------------------------------------
# @signature String getWindSpeedFlag()
# <p>Get the flag for the wind speed if it has been set.  Returns the
# QCF missing flag if the flag has not been set.</p>
#
# @output $flag Returns the flag for the wind speed.
##----------------------------------------------------------------
sub getWindSpeedFlag {
    my $self = shift;
    if (defined($self->{"wind_speed_flag"})) {
	return $self->{"wind_speed_flag"};
    } else {
	return Conversions::getMissingFlag();
    }
}

##---------------------------------------------------------------------
# @signature int isSpecial()
# <p>Check to see if the record is a special record (it does not land on
# a time consistant with the reporting frequency).</p>
#
# @limitation This method only knows how to deal with reporting frequencies of
# <ul><li>hourly</li><li>20 minute</li><li>5 minute</li></ul>
# @output $special Returns true if the record is special, false otherwise.
##---------------------------------------------------------------------
sub isSpecial {
    my $self = shift;
    my $freq = $self->$getReportingFrequency();
    my $min = substr($self->getNominalTime(), 3, 2);

    if ($freq eq "hourly") { return ($min != 0); }
    elsif ($freq eq "20 minute") { return (($min % 20) != 0); }
    elsif ($freq eq "5 minute") { return (($min % 5) != 0); }
    else { return 0; }
}

# Just used for testing.
sub main {
    my $data = Record->new();
    $data->setNominalDate("2003/06/24", "YYYY/MM/DD");
    $data->setNominalTime("12:25", "HH:MM");
    $data->setActualDate("2003175", "YYYYDDD");
    $data->setActualTime("12:28", "HH:MM");
    $data->setNetworkId("JOSS");
    $data->setStationId("Kakarott");
    $data->setLatitude(40);
    $data->setLongitude(-100);
    $data->setElevation(6000, "ft");
    $data->setTemperature(85, "F");
    $data->setPrecip(1.0, "in");
    
    print $data->toQCF_String();
}

##------------------------------------------------------------------
# @signature Record new(Station station)
# Create a new Record object with the default values.
# 
# @input $station The Station this reading is from.
# @output $record The new Record created.
##------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $station = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self, $class);

    $self->setVerbose($station->getVerbose());
    
    my $lat_format = ""; # Create a degree format for latitude
    my $count = 0;
    # "-" is a special char in format, so it needs to be handled differently
    if ($station->getLatitude() < 0) {
	$lat_format = "-";
	$count = 1;
    }
    while ($count < length($station->getLatitude())) {
	$lat_format .= "D";
	$count++;
    }
    
    my $long_format = ""; # Create a degree format for longitude
    $count = 0;
    # "-" is a special char in format, so it needs to be handled differently
    if ($station->getLongitude() < 0) { 
	$long_format = "-";
	$count = 1;
    }
    while ($count < length($station->getLongitude())) {
	$long_format .= "D";
	$count++;
    }

    # Set data that is specified by the network.
    $self->setNetworkId($station->getNetworkName());
    $self->setStationId($station->getStationId());
    $self->setLatitude($station->getLatitude(), $lat_format);
    $self->setLongitude($station->getLongitude(), $long_format);
    
    # Stations store missing elevation values as -9999.9 because of 
    # the Station format.  The Record uses the standard missing
    # value.  This converts the station missing to the standard missing.
    if ($station->getElevation() == -9999.9) {
	$self->setElevation(Conversions::getMissing(), "m");
    } else {
	$self->setElevation($station->getElevation(), "m");
    }
    
    # Set the rest of the data to missing values.
    $self->setReadingTime("9999/99/99", "YYYY/MM/DD", "99:99", "HH:MM", 0);
    $self->setOccurence(0);

    # Used to mark if the calculations should be done.
    $self->setCalculateDewPoint(0);
    $self->setCalculateSeaLevelPressure(0);

    # Set values not outputed to the QCF, but are used for conversions.
    $self->setRelativeHumidity(Conversions::getMissing(), 1);
    $self->$setReportingFrequency($station->getReportingFrequency);
    
    return $self;
}

##----------------------------------------------------------------------
# @signature void setActualDate(String date, String format)
# <p>Set the acutal date of the reading.</p>
#
# @input $date The actual date
# @input $date_format The format of the date with YMDJ values
# @output $act_date Returns the date in YYYY/MM/DD format.
# @link <a href=http://www.joss.ucar.edu/cgi-bin/dpg/doc/docperl.cgi?file=/work/DPG_HTML/BEST_SW/conversion_modules/Version1/Conversions.pm#convertDate(String date, String date_format)>Conversions::convertDate(String date, String date_format)</a>
# @warning If the date is not a valid date, the conversion will terminate.
##----------------------------------------------------------------------
sub setActualDate {
    my $self = shift;
    my $date = shift;
    my $date_format = shift;
    $date = Conversions::convertDate($date, $date_format);
    if (!Conversions::validDate($date) && $date ne "9999/99/99") {
	$self->$printErrorAndDie("*****", "Actual Date", $date, 
				 "$date is not a real date");
    }
    return $self->{"act_date"} = $date;
}

##----------------------------------------------------------------------
# @signature void setActualTime(String time, String format)
# <p>Set the actual time of the reading.</p>
#
# @input  $time The actual time.
# @input  $time_format The format of the time with HM values
# @output $act_time Returns the time in HH:MM format.
# @link <a href=http://www.joss.ucar.edu/cgi-bin/dpg/doc/docperl.cgi?file=/work/DPG_HTML/BEST_SW/conversion_modules/Version1/Conversions.pm#convertTime(String time, String time_format)>Conversions::convertTime(String time, String time_format)</a>
# @warning If the time is not a valid time, the conversion will terminate.
##----------------------------------------------------------------------
sub setActualTime {
    my $self = shift;
    my $time = shift;
    my $time_format = shift;
    $time = Conversions::convertTime($time, $time_format);
    if (!Conversions::validTime($time) && $time ne "99:99") {
	$self->$printErrorAndDie("*****", "Actual Time", $time, 
				 "$time is not a real time");
    }
    return $self->{"act_time"} = $time;
}

##----------------------------------------------------------------------
# @signature void setCalculateDewPoint(int calc)
# Set the flag if the dew point should be calculated.
#
# @input  $calc A true value if the dew point should be calculated.
##----------------------------------------------------------------------
sub setCalculateDewPoint {
    my $self = shift;
    my $calc = shift;
    $self->{"calc_dew_point"} = $calc;
}

##----------------------------------------------------------------------
# @signature void setCalculateSeaLevelPressure(int calc)
# <p>Set the flag if the sea pressure should be calculated.</p>
#
# @input $calc A true value if the sea level pressure should be calculated.
##----------------------------------------------------------------------
sub setCalculateSeaLevelPressure {
    my $self = shift;
    my $calc = shift;
    $self->{"calc_sea_level_pressure"} = $calc;
}

##----------------------------------------------------------------------
# @signature void setCeilingHeight(float height, int code, int level)
# <p>Set the ceiling height and code for a specified level.</p>
# <p>Ceiling height values less than 0 are sent to the warning log for the 
# Record.</p>
# <p>Ceiling height code values less than 0 or greater than 15 are sent to the
# warning log for the Record.</p>
#
# @input $height The ceiling height for the record.
# @input $code The ceiling height code for the record.
# @input $level The level of the ceiling height.
# @output $ceiling_height The ceiling height for the record.
# @output $ceiling_height_code The ceiling height code for the record.
# @warning If the ceiling height or ceiling height code do not fit in their
# allocated spaces, the conversion will terminate.
# @warning If the ceiling height and the ceiling height code are not both 
# missing values or if they are not both non-missing values, the conversion
# will terminate.
# @limitation This function will only determine the QCF flags for missing and
# unchecked ceiling height values.
##----------------------------------------------------------------------
##----------------------------------------------------------------------
# @signature void setCeilingHeight(float height, int, code, int level, String flag)
# <p>Set the ceiling height, code and flag for a specified level.</p>
# <p>Ceiling height values less than 0 are sent to the warning log for the 
# Record.</p>
# <p>Ceiling height code values less than 0 or greater than 15 are sent to the
# warning log for the Record.</p>
#
# @input $height The ceiling height for the record.
# @input $code The ceiling height code for the record.
# @input $level The level of the ceiling height.
# @input $flag The flag for the ceiling height.
# @output $ceiling_height The ceiling height for the record.
# @output $ceiling_height_code The ceiling height code for the record.
# @warning If the ceiling height or ceiling height code do not fit in their
# allocated spaces, the conversion will terminate.
# @warning If the flag given for the ceiling height is not one character or if
# the flag is not a recognized flag, the conversion will terminate.
# @warning If the ceiling height and the ceiling height code are not both 
# missing values or if they are not both non-missing values, the conversion
# will terminate.
##----------------------------------------------------------------------
sub setCeilingHeight {
    my $self = shift;
    my $height = shift;
    my $code = shift;
    my $level = shift;
    my $flag = shift;

    # Check to see if the flag was passed in as a parameter.
    if (!defined($flag)) {
	$flag = Conversions::getUncheckedFlag();
	if ($height eq Conversions::getMissing()) {
	    $flag = Conversions::getMissingFlag();
	}
    }

    # Check to see if the value fits in the space set aside for precip.
    $self->$checkIfFits("ceiling_height", $height);
    $self->$checkIfFits("ceiling_height_code", $code);
    $self->$checkFlagSize("ceiling_height", $flag);
    $self->$checkValueWithFlag("ceiling_height", $height, $flag);

    # Make sure both the height and code match missing/non-missing values.
    if (($height eq Conversions::getMissing() && $code != 15 && $code != 2) ||
	($height ne Conversions::getMissing() && $code == 15)){
	die("The ceiling height and the ceiling height code must both be missing values or both not missing values for ".$self->getStationId()." at actual time of ".$self->getActualDate()." ".$self->getActualTime()."\n");
    }

    # Perform range checking on the ceiling height.
    if ($height < 0 && $height ne Conversions::getMissing() || 
	$height > 9999.99) {
	$self->$appendWarning("ceiling_height_$level", $height,
			      "$height cannot must between 0 and 9999.99.");
    }
    if ($code < 0 || $code > 15) {
	$self->$appendWarning("ceiling_height_code_$level", $code,
			      "$code is an illegal value");
    }

    # Store the values to the Record.
    $self->{"ceiling_height_$level"."_flag"} = $flag;
    $self->{"ceiling_height_$level"} = $height;
    $self->{"ceiling_height_code_$level"} = $code;
    return ($height, $code);
}

##----------------------------------------------------------------------
# @signature void setCloudAmount(int amt, int level)
# <p>Set the cloud amount for a specified level.</p>
# <p>Cloud amount values less than 0 or greater than 15 are sent to the 
# warning log for the Record.</p>
# 
# @input  $amt The cloud amount.
# @input  $level The level of the clouds.
# @output $cloud_amount Returns the cloud amount.
# @warning If the cloud amount does not fit in the space allocated, the 
# conversion will terminate.
# @limitation This function will only determine the QCF flags for missing
# and unchecked values for the cloud amount.
##----------------------------------------------------------------------
##----------------------------------------------------------------------
# @signature void setCloudAmount(int amt, int level, String flag)
# <p>Set the cloud amount and its flag for a specified level.</p>
# <p>Cloud amount values less than 0 or greater than 15 are sent to the 
# warning log for the Record.</p>
# 
# @input  $amt The cloud amount.
# @input  $level The level of the clouds.
# @input  $flag The flag for the cloud amount.
# @output $cloud_amount Returns the cloud amount.
# @warning If the cloud amount does not fit in the space allocated, the 
# conversion will terminate.
# @warning If the flag given for the cloud amount is not one character or if
# the flag is not a recognized flag, the conversion will terminate.
##----------------------------------------------------------------------
sub setCloudAmount {
    my $self = shift;
    my $amt = shift;
    my $level = shift;
    my $flag = shift;
    
    # Check to see if the flag was passed in as an arguement.
    if (!defined($flag)) {
	$flag = Conversions::getUncheckedFlag();
	if ($amt == 15) {
	    $flag = Conversions::getMissingFlag();
	}
    }
    
    # Check to see if the value fits in the space set aside for cloud amount.
    $self->$checkIfFits("cloud_amount_$level", $amt);
    $self->$checkFlagSize("cloud_amount_$level", $flag);
    $self->$checkValueWithFlag("cloud_amount_$level", $amt, $flag);

    # Perform range checking on the cloud amount codes.
    if ($amt < 0 || $amt > 15) {
	$self->$appendWarning("cloud_amount_$level", $amt,
			      "$amt is an illegal value");
    }

    # Store the cloud amount and flag to the Record.
    $self->{"cloud_amount_$level"."_flag"} = $flag;
    return $self->{"cloud_amount_$level"} = $amt;
}

##----------------------------------------------------------------------
# @signature void setDewPoint(float dewpoint, String measure)
# <p>Set the dew point for the reading.  The dew point is stored in degrees 
# Celsius</p>
# <p>Dew point values less than -100 &deg;C or greater than 100 &deg;C are sent
# to the warning log for the Record.</p>
#
# @input $dewpoint The dew point for the reading.
# @input $measure The temperature units used to measure the dew point.
# @output $dep_point Returns the dew point in degrees Celsius.
# @link <a href=http://www.joss.ucar.edu/cgi-bin/dpg/doc/docperl.cgi?file=/work/DPG_HTML/BEST_SW/conversion_modules/Version1/Conversions.pm#convertTemperature(float temp, String in, String out)>Conversions::convertTemperature(float temp, String in, String out)</a>
# @warning If the converted dew point does not fit in the space allocated, the
# conversion will terminate.
# @limitation This function will only determine the QCF flags for missing and
# unchecked values for the dew point.
##----------------------------------------------------------------------
##----------------------------------------------------------------------
# @signature void setDewPoint(float dewpoint, String measure, String flag)
# <p>Set the dew point and its flag for the reading.  The dew point is stored
# in degrees Celsius</p>
# <p>Dew point values less than -100 &deg;C or greater than 100 &deg;C are sent
# to the warning log for the Record.</p>
#
# @input $dewpoint The dew point for the reading.
# @input $measure The temperature units used to measure the dew point.
# @input $flag The flag to set for the dew point.
# @output $dep_point Returns the dew point in degrees Celsius.
# @link <a href=http://www.joss.ucar.edu/cgi-bin/dpg/doc/docperl.cgi?file=/work/DPG_HTML/BEST_SW/conversion_modules/Version1/Conversions.pm#convertTemperature(float temp, String in, String out)>Conversions::convertTemperature(float temp, String in, String out)</a>
# @warning If the converted dew point does not fit in the space allocated, the
# conversion will terminate.
# @warning If the flag given for the dew point is not one character or if the
# flag is not a recognized flag, the conversion will terminate.
##----------------------------------------------------------------------
sub setDewPoint {
    my $self = shift;
    my $dewpoint = shift;
    my $measure = shift;
    my $flag = shift;

    # Check to see if the flag was passed in as an arguement.
    if (!defined($flag)) {
	$flag = Conversions::getUncheckedFlag();
	if ($dewpoint eq Conversions::getMissing()) {
	    $flag = Conversions::getMissingFlag();
	}
    }
    $dewpoint = Conversions::convertTemperature($dewpoint, $measure, "C");

    # Check to see if the value fits in the space set aside for dew point.
    $self->$checkIfFits("dewpoint", $dewpoint);
    $self->$checkFlagSize("dewpoint", $flag);
    $self->$checkValueWithFlag("dewpoint", $dewpoint, $flag);

    # Perform range checking on the dew point values.
    if ($dewpoint < -100 && $dewpoint ne Conversions::getMissing() ||
	$dewpoint > 100) {
	$self->$appendWarning("dew_point", $dewpoint,
			      "dewpoint is not between -100 and 100");
    } 
    
    # Store the dew point and its flag to the Record.
    $self->{"dew_point_flag"} = $flag;
    return $self->{"dew_point"} = $dewpoint;
}

##-----------------------------------------------------------------------
# @signature void setElevation(float elev, String measure)
# <p>Set the elevation for the reading.  The elevation is stored in m.</p>
# <p>Elevation values less than -200 m or greater than 9000 m are sent to
# the warning log for the Record.</p>
#
# @input  $elev The elevation of the reading.
# @input  $measure The units of length used to measure the elevation.
# @output $elevation Returns the elevation in m.
# @link <a href=http://www.joss.ucar.edu/cgi-bin/dpg/doc/docperl.cgi?file=/work/DPG_HTML/BEST_SW/conversion_modules/Version1/Conversions.pm#>convertLength(float len, String in, String out)Conversions::convertLength(float len, String in, String out)</a>
##-----------------------------------------------------------------------
sub setElevation {
    my $self = shift;
    my $elev = shift;
    my $measure = shift;
    
    if ($elev ne Conversions::getMissing()) {
	$elev = Conversions::convertLength($elev, $measure, "m");
	
	# Perform range checking for the elevation.
	if ($elev < -200 || $elev > 9000 && 
	    $elev ne Conversions::getMissing()) {
	    $self->$appendWarning("elevation", $elev,
				  "elevation should be between -200 and 9000");
	}
    } 

    # Store the elevation to the Record.
    return $self->{"elevation"} = $elev;
}

##------------------------------------------------------------------------
# @signature void setGustSpeed(float gust, String measure)
# <p>Set the gust speed and its flag for the Record.  The gust speed is stored
# in m/s.</p>
# 
# @input $gust The gust speed value from the raw data.
# @input $measure The units of velocity the gust speed value is measured.
# @output $gust_speed Returns the gust speed measured in m/s.
# @link <a href=http://www.joss.ucar.edu/cgi-bin/dpg/doc/docperl.cgi?file=/work/DPG_HTML/BEST_SW/conversion_modules/Version1/Conversions.pm#convertVelocity(float velocity, String in, String out)>Conversions::convertVelocity(float velocity, String in, String out)</a>
# @warning If the converted gust speed does not fit in the space allocated, the
# conversion will terminate.
# @limitation This function will only determine the QCF flags for missing and
# unchecked values for the gust speed.
##------------------------------------------------------------------------
##------------------------------------------------------------------------
# @signature void setGustSpeed(float gust, String measure, String flag)
# <p>Set the gust speed and its flag for the Record.  The gust speed is stored
# in m/s.</p>
# 
# @input $gust The gust speed value from the raw data.
# @input $measure The units of velocity the gust speed value is measured.
# @input $flag The flag value to set for the gust speed.
# @output $gust_speed Returns the gust speed measured in m/s.
# @link <a href=http://www.joss.ucar.edu/cgi-bin/dpg/doc/docperl.cgi?file=/work/DPG_HTML/BEST_SW/conversion_modules/Version1/Conversions.pm#convertVelocity(float velocity, String in, String out)>Conversions::convertVelocity(float velocity, String in, String out)</a>
# @warning If the converted gust speed does not fit in the space allocated, the
# conversion will terminate.
# @warning If the flag given for the gust speed is not one character or if the
# flag is not a recognized flag, the conversion will terminate.
##------------------------------------------------------------------------
sub setGustSpeed {
    my $self = shift;
    my $gust = shift;
    my $measure = shift;
    my $flag = shift;

    # Check to see if the flag was passed in as an arguement.
    if (!defined($flag)) {
	$flag = Conversions::getUncheckedFlag();
	if ($gust eq Conversions::getMissing()) {
	    $flag = Conversions::getMissingFlag();
	}
    }
    $gust = Conversions::convertVelocity($gust, $measure, "m/s");
    
    # Check to see if the value fits in the space set aside for precip.
    $self->$checkIfFits("gust_speed", $gust);
    $self->$checkFlagSize("gust_speed", $flag);
    $self->$checkValueWithFlag("gust_speed", $gust, $flag);

    # Perform some range checking on the gust speed.
    if ($gust < 0 && $gust ne Conversions::getMissing()) {
	$self->$appendWarning("gust_speed", $gust,
			      "$gust cannot be less than zero.");
    }

    # Store the gust speed and flag to the Record.
    $self->{"gust_speed_flag"} = $flag;
    return $self->{"gust_speed"} = $gust;
}

##------------------------------------------------------------------------
# @signature void setLatitude(float lat, String format)
# <p>Set the latitude of the reading.</p>
# <p>Latitude values less than -90 or greater than 90 are sent to the warning
# log for the Record.</p>
#
# @input  $lat The latitude of the reading.
# @input  $format The format of $lat using "-DMS" values
# @output $latitude Returns the latitude in degrees.
# @link <a href=http://www.joss.ucar.edu/cgi-bin/dpg/doc/docperl.cgi?file=/work/DPG_HTML/BEST_SW/conversion_modules/Version1/Conversions.pm#>convertLatLong(String value, String format)Conversions::convertLatLong(String value, String format)</a>
##------------------------------------------------------------------------
sub setLatitude {
    my $self = shift;
    my $lat = shift;
    my $format = shift;
    $lat = Conversions::convertLatLong($lat, $format);
    
    # Perform range checking on the latitude.
    if ($lat < -90 || $lat > 90) {
	$self->$appendWarning("latitude", $lat,
			      "latitude must be between -90 and 90.");
    }

    return $self->{"latitude"} = $lat;
}

##------------------------------------------------------------------------
# @signature void setLongitude(float long, String format)
# <p>Set the longitude of the reading.</p>
# <p>Longitude values less than -180 or greater than 180 are sent to the
# warning log for the Record</p>
#
# @input  $long The longitude of the reading.
# @input  $format The format of $lat using "-DMS" values
# @output $longitude Returns the longitude in degrees.
# @link <a href=http://www.joss.ucar.edu/cgi-bin/dpg/doc/docperl.cgi?file=/work/DPG_HTML/BEST_SW/conversion_modules/Version1/Conversions.pm#>convertLatLong(String value, String format)Conversions::convertLatLong(String value, String format)</a>
##------------------------------------------------------------------------
sub setLongitude {
    my $self = shift;
    my $long = shift;
    my $format = shift;
    $long = Conversions::convertLatLong($long, $format);

    # Perform range checking on the longitude
    if ($long < -180 || $long > 180) {
	$self->$appendWarning("longitude", $long,
			      "longitude must be between -180 and 180.");
    }

    return $self->{"longitude"} = $long;
}

##------------------------------------------------------------------------
# @signature void setNetworkId(String id)
# <p>Set the network identifier for the reading.</p>
#
# @input  $id The network identifier.
##------------------------------------------------------------------------
sub setNetworkId {
    my $self = shift;
    my $id = shift;
    $self->{"network_id"} = $id;
}

##------------------------------------------------------------------------
# @signature void setNominalDate(String date, String date_format)
# <p>Set the nominal date for the reading.</p>
#
# @input  $date The nominal date.
# @input  $date_format The format of the date using YMDJ values
# @output $nom_date Returns the nominal date in YYYY/MM/DD format.
# @link <a href=http://www.joss.ucar.edu/cgi-bin/dpg/doc/docperl.cgi?file=/work/DPG_HTML/BEST_SW/conversion_modules/Version1/Conversions.pm#convertDate(String date, String date_format)>Conversions::convertDate(String date, String date_format)</a>
# @warning If the date is not a valid date, the conversion will terminate.
##------------------------------------------------------------------------
sub setNominalDate {
    my $self = shift;
    my $date = shift;
    my $date_format = shift;
    $date = Conversions::convertDate($date, $date_format);
    if (!Conversions::validDate($date) && $date ne "9999/99/99") {
	$self->$printErrorAndDie("*****", "Nominal Date", $date, 
				 "$date is not a real date");
    }
    $self->{"nom_date"} = $date;
}

##-----------------------------------------------------------------------
# @signature void setNominalTime(String time, String time_format)
# <p>Set the nominal time of the reading.</p>
#
# @input  $time The nominal time of the reading.
# @input  $time_format The format of the time using HM values
# @output $nom_time Returns the nominal time in HH:MM format.
# @link <a href=http://www.joss.ucar.edu/cgi-bin/dpg/doc/docperl.cgi?file=/work/DPG_HTML/BEST_SW/conversion_modules/Version1/Conversions.pm#convertTime(String time, String time_format)>Conversions::convertTime(String time, String time_format)</a>
# @warning If the time is not a valid time, the conversion will terminate.
##-----------------------------------------------------------------------
sub setNominalTime {
    my $self = shift;
    my $time = shift;
    my $time_format = shift;
    $time = Conversions::convertTime($time, $time_format);
    if (!Conversions::validTime($time) && $time ne "99:99") {
	$self->$printErrorAndDie("*****", "Nominal Time", $time,
				 "$time is not a real time");
    }
    $self->{"nom_time"} = $time;
}

##------------------------------------------------------------------------
# @signature void setOccurence()
# <p>Set the occurence value for the reading to the default value of 0.</p>
##------------------------------------------------------------------------
##------------------------------------------------------------------------
# @signature void setOccurence(int occ)
# <p>Set the occurence value for the reading.</p>
#
# @input $occ The occurence value.
##------------------------------------------------------------------------
sub setOccurence {
    my $self = shift;
    my $occ = shift;

    if (!defined($occ)) { $occ = 0; }
    $self->{"occurence"} = $occ;
}

##------------------------------------------------------------------------
# @signature void setPrecip(float precip, String measure)
# <p>Set the precipitation value and the precipitation flag of the reading.
# The precipitation is stored in mm.</p>
#
# @input $precip The precipitation of the reading.
# @input $measure The units of length of the precipitation reading.
# @output $precip Returns the precipitation in mm.
# @link <a href=http://www.joss.ucar.edu/cgi-bin/dpg/doc/docperl.cgi?file=/work/DPG_HTML/BEST_SW/conversion_modules/Version1/Conversions.pm#convertLength(float len, String in, String out)>Conversions::convertLength(float len, String in, String out)</a>
# @warning If the precipitation does not fit in the space allocated, the
# conversion will terminate.
# @limitation This function will only determine the QCF flags for missing, 
# unchecked, and negative precipitation values.
##------------------------------------------------------------------------
##------------------------------------------------------------------------
# @signature void setPrecip(float precip, String measure, String flag)
# <p>Set the precipitation value and the precipitation flag of the reading.
# The precipitation is stored in mm.</p>
#
# @input $precip The precipitation of the reading.
# @input $measure The units of length of the precipitation reading.
# @input $flag The flag for the precipitation of the reading.
# @output $precip Returns the precipitation in mm.
# @link <a href=http://www.joss.ucar.edu/cgi-bin/dpg/doc/docperl.cgi?file=/work/DPG_HTML/BEST_SW/conversion_modules/Version1/Conversions.pm#convertLength(float len, String in, String out)>Conversions::convertLength(float len, String in, String out)</a>
# @warning If the precipitation does not fit in the space allocated, the
# conversion will terminate.
# @warning If the flag given for the precipitation is not one character or
# if the flag is not a recognized flag, the conversion will terminate.
##------------------------------------------------------------------------
sub setPrecip {
    my $self = shift;
    my $precip = shift;
    my $measure = shift;
    my $flag = shift;

    # Check to see if the flag was passed in as an arguement
    if (!defined($flag)) {
	$flag = Conversions::getUncheckedFlag();
	if ($precip eq Conversions::getMissing()) {
	    $flag = Conversions::getMissingFlag();
	}
	# Perform range checks on the precipitation values.
	if ($precip < 0 && $precip ne Conversions::getMissing()) {
	    $self->$appendWarning("precip", "$precip",
				  "recorded precip was less than zero.");
	    $precip = Conversions::getMissing();
	    $flag = Conversions::getNegativePrecipFlag();
	}
    }
    $precip = Conversions::convertLength($precip, $measure, "mm");

    # Check to see if the value fits in the space set aside for precip.
    $self->$checkIfFits("precip", $precip);
    $self->$checkFlagSize("precip", $flag);
    $self->$checkValueWithFlag("precip", $precip, $flag);

    # Store the precip and flag values to the Record.
    $self->{"precip_flag"} = $flag;
    return $self->{"precip"} = $precip;
}

##------------------------------------------------------------------------
# @signature void setPresentWeather(int wx)
# <p>Set the present weather conditions for the reading.</p>
# <p>Present Weather values less than 0 or greater than 999 are sent to the 
# warning log for the Record.</p>
#
# @input $wx The present weather conditions reading.
# @output $present_weather Returns the present weather conditions.
# @warning If the present weather does not fit in the space allocated, the
# conversion wil terminate.
# @limitation This function will only determine the QCF flags for missing and
# unchecked values for the present weather conditions.
##------------------------------------------------------------------------
##------------------------------------------------------------------------
# @signature void setPresentWeather(int wx, String flag)
# <p>Set the present weather conditions for the reading.</p>
# <p>Present Weather values less than 0 or greater than 999 are sent to the 
# warning log for the Record.</p>
#
# @input $wx The present weather conditions reading.
# @input $flag The flag for the present weather conditions reading.
# @output $present_weather Returns the present weather conditions.
# @warning If the present weather does not fit in the space allocated, the
# conversion wil terminate.
# @warning If the flag given for the present weather conditions is not one
# character or if the flag is not a recognized flat, the conversion will
# terminate.
##------------------------------------------------------------------------
sub setPresentWeather {
    my $self = shift;
    my $wx = shift;
    my $flag = shift;
    
    # Check to see if the flag was passed in as an arguement.
    if (!defined($flag)) {
	$flag = Conversions::getUncheckedFlag();
	if ($wx eq "-999") {
	    $flag = Conversions::getMissingFlag();
	}
    }

    # Check to see if the value fits in the space set aside for pressure.
    $self->$checkIfFits("present_weather", $wx);
    $self->$checkFlagSize("present_weather", $flag);
    $self->$checkValueWithFlag("present_weather", $wx, $flag);

    # Perform range checks on the values for the present weather conditions.
    if (($wx < 0 && $wx != -999) || $wx > 999) {
	$self->$appendWarning("present_weather", $wx,
			      "$wx is an illegal value.");
    }

    # Store the present weather and flag values to the Record.
    $self->{"present_weather_flag"} = $flag;
    return $self->{"present_weather"} = $wx;
}

##-----------------------------------------------------------------------
# @signature void setPressure(float press, String measure)
# <p>Set the station pressure and its flag for the reading.  The pressure is
# stored in mb.</p>
# <p>Pressure values less than 800 mb or greater than 1200 mb are sent to the 
# warning log for the Record.</p>
#
# @input $press The pressure of the reading.
# @input $measure The units of pressure of the pressure reading.
# @output $pressure Returns the pressure in mb.
# @link <a href=http://www.joss.ucar.edu/cgi-bin/dpg/doc/docperl.cgi?file=/work/DPG_HTML/BEST_SW/conversion_modules/Version1/Conversions.pm#convertPressure(float press, String in, String out)>Conversions::convertPressure(float press, String in, String out)</a>
# @warning If the pressure does not fit in the space allocated, the conversion
# will terminate.
# @limitation This function will only determine the QCF flags for missing and
# unchecked values for the pressure.
##-----------------------------------------------------------------------
##-----------------------------------------------------------------------
# @signature void setPressure(float press, String measure, String flag)
# <p>Set the station pressure and its flag for the reading.  The pressure is
# stored in mb.</p>
# <p>Pressure values less than 800 mb or greater than 1200 mb are sent to the 
# warning log for the Record.</p>
#
# @input $press The pressure of the reading.
# @input $measure The units of pressure of the pressure reading.
# @input $flag The flag for the station pressure reading.
# @output $pressure Returns the pressure in mb.
# @link <a href=http://www.joss.ucar.edu/cgi-bin/dpg/doc/docperl.cgi?file=/work/DPG_HTML/BEST_SW/conversion_modules/Version1/Conversions.pm#convertPressure(float press, String in, String out)>Conversions::convertPressure(float press, String in, String out)</a>
# @warning If the pressure does not fit in the space allocated, the conversion
# will terminate.
# @warning If the flag given for the pressure is not one character or if the 
# flag is not a recognized flag, the conversion will terminate.
##-----------------------------------------------------------------------
sub setPressure {
    my $self = shift;
    my $press = shift;
    my $measure = shift;
    my $flag = shift;
   
    # Check to see if the flag was passed in as an arguement.
    if (!defined($flag)) {
	$flag = Conversions::getUncheckedFlag();
	if ($press eq Conversions::getMissing()) {
	    $flag = Conversions::getMissingFlag();
	}
    }
    $press = Conversions::convertPressure($press, $measure, "mb");

    # Check to see if the value fits in the space set aside for pressure.
    $self->$checkIfFits("pressure", $press);
    $self->$checkFlagSize("pressure", $flag);
    $self->$checkValueWithFlag("pressure", $press, $flag);
    
    # Perform limit checks on the pressure.
    if (($press < 800 || $press > 1200) && 
	$press ne Conversions::getMissing()) {
	$self->$appendWarning("pressure", $press,
			      "pressure must be between 800 and 1200");
    }

    # Store the pressure and flag to the Record.
    $self->{"pressure_flag"} = $flag;
    return $self->{"pressure"} = $press;
}

##-----------------------------------------------------------------------
# @signature void setReadingTime(String date, String date_fmt, String time, String time_fmt, float offset)
# Set the date and time of the reading.
#
# @input  $date The date of the reading.
# @input  $date_fmt The format of $date using Y,M,D,J for years, months, days,
# and julian days.
# @input  $time The time of the reading.
# @input  $time_fmt The format of $time using H and M for hours and minutes.
# @input  $offset The number of hours the data is offset from UTC.
# @link <a href=http://www.joss.ucar.edu/cgi-bin/dpg/doc/docperl.cgi?file=/work/DPG_HTML/BEST_SW/conversion_modules/Version1/Conversions.pm#convertDate(String date, String date_format)>Conversions::convertDate(String date, String date_format)</a>
# @link <a href=http://www.joss.ucar.edu/cgi-bin/dpg/doc/docperl.cgi?file=/work/DPG_HTML/BEST_SW/conversion_modules/Version1/Conversions.pm#convertTime(String time, String time_format)>Conversions::convertTime(String time, String time_format)</a>
##-----------------------------------------------------------------------
sub setReadingTime {
    my $self = shift;
    my $date = shift;
    my $date_fmt = shift;
    my $time = shift;
    my $time_fmt = shift;
    my $offset = shift;

    $self->{"record_date"} = $date;
    $self->{"record_time"} = $time;
    
    # If time is "99:99" it is just an initialization and does not need
    # to be converted.
    if ($time ne "99:99") {
	$date = Conversions::convertDate($date, $date_fmt);
	$time = Conversions::convertTime($time, $time_fmt);

	my $hour = substr($time, 0, 2) + $offset;
	my $min = substr($time, 3, 2);
	
	# Check to see if the UTC offset caused a day change
	if ($hour > 23) {
	    $hour -= 24;
	    $date = Conversions::getNextDay($date);
	}

	while (length($hour) < 2) { # Ensure that the hour is in "HH" format
	    $hour = "0".$hour;
	}
	while (length($min) < 2) { # Ensure that the min is in "MM" format
	    $min = "0".$min;
	}

	$time = $hour.":".$min; # Format the time.
    }

    # Store the dates and times to the Record.
    $self->setActualDate($date, "YYYY/MM/DD");
    $self->setActualTime($time, "HH:MM");
    $self->setNominalDate($date, "YYYY/MM/DD");
    $self->setNominalTime($time, "HH:MM");
}

##-----------------------------------------------------------------------
# @signature void setRelativeHumidity(float rh, int percent)
# <p>Set the relative humidity of the reading.  The relative humidity is
# stored as a percent.</p>
#
# @input  $rh The relative humidity value.
# @input  $percent Should be 0 if not given as a percent, 1 otherwise
##------------------------------------------------------------------------
##-----------------------------------------------------------------------
# @signature void setRelativeHumidity(float rh, int percent, String flag)
# <p>Set the relative humidity of the reading and its flag.  The relative 
# humidity is stored as a percent.</p>
#
# @input  $rh The relative humidity value.
# @input  $percent Should be 0 if not given as a percent, 1 otherwise
# @input  $flag The QCF flag for the relative humidity.
##------------------------------------------------------------------------
sub setRelativeHumidity {
    my $self = shift;
    my $rh = shift;
    my $percent = shift;
    my $flag = shift;

    if (!defined($flag)) { $flag = Conversions::getUncheckedFlag(); }

    if (!$percent) {
	$self->{"rel_humidity"} = $rh * 100;
    } else {
	$self->{"rel_humidity"} = $rh;
    }
    $self->{"rel_humidity_flag"} = $flag;
}

##-------------------------------------------------------------------------
# @signature void setSeaLevelPressure(float press, String measure)
# <p>Set the sea level pressure and its flag for the reading.  The sea level
# pressure is stored in mb.</p>
# <p>Pressure values less than 800 mb or greater than 1200 mb are sent to the 
# warning log for the Record.</p>
#
# @input $press The sea level pressure of the reading.
# @input $measure The units of pressure of the sea level pressure reading.
# @output $slpressure Returns the sea level pressure in mb.
# @link <a href=http://www.joss.ucar.edu/cgi-bin/dpg/doc/docperl.cgi?file=/work/DPG_HTML/BEST_SW/conversion_modules/Version1/Conversions.pm#convertPressure(float press, String in, String out)>Conversions::convertPressure(float press, String in, String out)</a>
# @warning If the sea level pressure does not fit in the space allocated, the 
# conversions will terminate.
# @limitation This function will only determine the QCF flags for missing and
# unchecked values for the sea level pressure.
##------------------------------------------------------------------------
##-------------------------------------------------------------------------
# @signature void setSeaLevelPressure(float press, String measure, String flag)
# <p>Set the sea level pressure and its flag for the reading.  The sea level
# pressure is stored in mb.</p>
# <p>Pressure values less than 800 mb or greater than 1200 mb are sent to the 
# warning log for the Record.</p>
#
# @input $press The sea level pressure of thre reading.
# @input $measure The units of pressure of the sea level pressure reading.
# @input $flag The flag for the sea level pressure reading.
# @output $slpressure Returns the sea level pressure in mb.
# @link <a href=http://www.joss.ucar.edu/cgi-bin/dpg/doc/docperl.cgi?file=/work/DPG_HTML/BEST_SW/conversion_modules/Version1/Conversions.pm#convertPressure(float press, String in, String out)>Conversions::convertPressure(float press, String in, String out)</a>
# @warning If the sea level pressure does not fit in the space allocated, the 
# conversions will terminate.
# @warning If the flag given for the sea level pressure is not one character or
# if the flag is not a recognized flag, the conversion will terminate.
##------------------------------------------------------------------------
sub setSeaLevelPressure {
    my $self = shift;
    my $press = shift;
    my $measure = shift;
    my $flag = shift;

    # Check to see if the flag was passed in as an arguement.
    if (!defined($flag)) {
	$flag = Conversions::getUncheckedFlag();
	if ($press eq Conversions::getMissing()) {
	    $flag = Conversions::getMissingFlag();
	}
    }
    $press = Conversions::convertPressure($press, $measure, "mb");

    # Check to see if the value fits in the space set aside for sl pressure.
    $self->$checkIfFits("sea_pressure", $press);
    $self->$checkFlagSize("sea_pressure", $flag);
    $self->$checkValueWithFlag("sea_pressure", $press, $flag);
    
    # Perform limit checks on the sea level pressure.
    if (($press < 800 || $press > 1200) && 
	$press ne Conversions::getMissing()) {
	$self->$appendWarning("sea_level_pressure", $press,
			      "sea level pressure must be between 800 and 1200");
    }

    # Store the sea level pressure and flag to the record.
    $self->{"sea_pressure_flag"} = $flag;
    return $self->{"sea_pressure"} = $press;
}

##-------------------------------------------------------------------------
# @signature void setStationId(String id)
# <p>Set the station identifier for the reading.</p>
# 
# @input $id The station identifier.
##-------------------------------------------------------------------------
sub setStationId {
    my $self = shift;
    my $id = shift;
    $self->{"station_id"} = $id;
}

##-------------------------------------------------------------------------
# @signature void setTemperature(float temp, String deg)
# <p>Set the temperature and temperature flag of the reading.  The temperature
# is stored in degrees Celsius.</p>
# <p>Temperature values less than -100 &deg;C or greater than 100 &deg;C are 
# sent to the warning log for the Record.</p>
# 
# @input $temp The temperature of the reading.
# @input $deg The units of temperature of the temperature reading
# @output $temperature Returns the temperature in degrees Celsius.
# @link <a href=http://www.joss.ucar.edu/cgi-bin/dpg/doc/docperl.cgi?file=/work/DPG_HTML/BEST_SW/conversion_modules/Version1/Conversions.pm#convertTemperature(float temp, String in, String out)>Conversions::convertTemperature(float temp, String in, String out)</a>
# @warning If the temperature does not fit in the space allocated, the 
# conversion will terminate.
# @limitation This function will only determine the QCF flags for missing and
# unchecked values for the temperature.
##-------------------------------------------------------------------------
##-------------------------------------------------------------------------
# @signature void setTemperature(float temp, String deg, String flag)
# <p>Set the temperature and temperature flag of the reading.  The temperature
# is stored in degrees Celsius.</p>
# <p>Temperature values less than -100 &deg;C or greater than 100 &deg;C are 
# sent to the warning log for the Record.</p>
# 
# @input $temp The temperature of the reading.
# @input $deg The units of temperature of the temperature reading
# @input $flag The temperature flag of the reading.
# @output $temperature Returns the temperature in degrees Celsius.
# @link <a href=http://www.joss.ucar.edu/cgi-bin/dpg/doc/docperl.cgi?file=/work/DPG_HTML/BEST_SW/conversion_modules/Version1/Conversions.pm#convertTemperature(float temp, String in, String out)>Conversions::convertTemperature(float temp, String in, String out)</a>
# @warning If the temperature does not fit in the space allocated, the 
# conversion will terminate.
# @warning If the flag given for the temperature is not one character or if 
# the flag is not a recognized flag, the conversion will terminate.
##-------------------------------------------------------------------------
sub setTemperature {
    my $self = shift;
    my $temp = shift;
    my $deg = shift;
    my $flag = shift;

    # Check to see if the flag was passed in as an argument.
    if (!defined($flag)) {
	$flag = Conversions::getUncheckedFlag();
	if ($temp eq Conversions::getMissing()) {
	    $flag = Conversions::getMissingFlag();
	}
    }
    $temp = Conversions::convertTemperature($temp, $deg, "C");

    # Check to see if the value fits in the space set aside for temperature.
    $self->$checkIfFits("temperature", $temp);
    $self->$checkFlagSize("temperature", $flag);
    $self->$checkValueWithFlag("temperature", $temp, $flag);
    
    # Perform limit checks on the visibility
    if ($temp < -100 && $temp ne Conversions::getMissing() || $temp > 100) {
	$self->$appendWarning("temperature", $temp,
			      "$temp is not between -100 and 100");
    }
    
    # Store the temperature and flag to the Record.
    $self->{"temperature_flag"} = $flag;
    return $self->{"temperature"} = $temp;
}      

##-----------------------------------------------------------------------
# @signature void setVerbose()
# Set the verbose flag to print warnings to STDERR on calculations.
##-----------------------------------------------------------------------
##-----------------------------------------------------------------------
# @signature void setVerbose(int flag)
# Set the verbose flag to optionally print warnings to STDERR on calculations.
#
# @input $flag The verbose flag.  1 (True) to print, 0 (False) to not print.
##-----------------------------------------------------------------------
sub setVerbose {
    my $self = shift;
    my $flag = shift;

    # Check to see if the flag was passed in.
    if (!defined($flag)) { $flag = 1; }
    $self->{"verbose"} = $flag;
}

##------------------------------------------------------------------------
# @signature void setVisibility(float vis, String measure)
# <p>Set the visibilty of the reading.  The visibility
# is stored in m.</p>
# <p>Visibility values less than 0 m or greater than 160000 m are sent to
# the warning log for the Record</p>
#
# @input $vis The visibility of the reading.
# @input $measure The units of length of the visibility reading.
# @output $visibility The visibility measured in m.
# @link <a href=http://www.joss.ucar.edu/cgi-bin/dpg/doc/docperl.cgi?file=/work/DPG_HTML/BEST_SW/conversion_modules/Version1/Conversions.pm#convertLength(float len, String in, String out)>Conversions::convertLength(float len, String in, String out)</a>
# @warning If the visibility does not fit in the space allocated, the 
# conversion will terminate.
# @limitation This function will only determine the QCF flags for missing and
# unchecked values for the visibility.
##------------------------------------------------------------------------
##------------------------------------------------------------------------
# @signature void setVisibility(float vis, String measure, String flag)
# <p>Set the visibilty and the visibility flag of the reading.  The visibility
# is stored in m.</p>
# <p>Visibility values less than 0 m or greater than 160000 m are sent to
# the warning log for the Record</p>
#
# @input $vis The visibility of the reading.
# @input $measure The units of length of the visibility reading.
# @input $flag The flag for the visibility of the reading.
# @output $visibility The visibility measured in m.
# @link <a href=http://www.joss.ucar.edu/cgi-bin/dpg/doc/docperl.cgi?file=/work/DPG_HTML/BEST_SW/conversion_modules/Version1/Conversions.pm#convertLength(float len, String in, String out)>Conversions::convertLength(float len, String in, String out)</a>
# @warning If the visibility does not fit in the space allocated, the 
# conversion will terminate.
# @warning If the flag given for the visibility is not one character or if the
# flag is not a recognized flag, the conversion will terminate.
##------------------------------------------------------------------------
sub setVisibility {
    my $self = shift;
    my $vis = shift;
    my $measure = shift;
    my $flag = shift;
    
    # Check to see if the flag was passed in as an argument.
    if (!defined($flag)) {
	$flag = Conversions::getUncheckedFlag();
	if ($vis eq Conversions::getMissing()) {
	    $flag = Conversions::getMissingFlag();
	}
    }
    $vis = Conversions::convertLength($vis, $measure, "m");
    
    # Check to see if the value fits in the space set aside for visibility.
    $self->$checkIfFits("visibility", $vis);
    $self->$checkFlagSize("visibility", $flag);
    $self->$checkValueWithFlag("visibility", $vis, $flag);
    
    # Perform limit checks on the visibility
    if ($vis < 0 && $vis ne Conversions::getMissing() ||
	$vis > 160000) {
	$self->$appendWarning("visibility", $vis,
			      "visibility must be between 0 and 160000.");
    }

    # Store the visibiltiy and flag to the Record
    $self->{"visibility_flag"} = $flag;
    return $self->{"visibility"} = $vis;
}

##-----------------------------------------------------------------------
# @signature void setWindDirection(float dir)
# <p>Set the wind direction of the reading.  The function assumes the 
# direction is in degrees.</p>
# 
# @input $dir The wind direction in degrees.
# @output $wind_direction Returns the wind direction in degrees.
# @warning If the wind direction does not fit in the space allocated, the
# conversion will terminate.
# @limitation The function will only determine the QCF flags for missing
# and unchecked values for the wind direction.
##-----------------------------------------------------------------------
##-----------------------------------------------------------------------
# @signature void setWindDirection(float dir, String flag)
# <p>Set the wind direction and its flag for the reading.  The function
# assumes the direction is in degrees.</p>
#
# @input $dir The wind direction in degrees.
# @input $flag The flag to set for the wind direction.
# @output $wind_direction Returns the wind direction in degrees.
# @warning If the wind direction does not fit in the space allocated, the
# conversion will terminate.
# @warning If the flag given for the wind direction is not one character
# or if the flag is not a recognized flag, the conversion will terminate.
##-----------------------------------------------------------------------
sub setWindDirection {
    my $self = shift;
    my $dir = shift;
    my $flag = shift;
    
    # Check to see if the flag was passed in as an arguement.
    if (!defined($flag)) {
	$flag = Conversions::getUncheckedFlag();
	if ($dir eq Conversions::getMissing()) {
	    $flag = Conversions::getMissingFlag();
	}
    }

    # Check to see if the value fits in the space set aside for wind direction.
    $self->$checkIfFits("wind_direction", $dir);
    $self->$checkFlagSize("wind_direction", $flag);
    $self->$checkValueWithFlag("wind_direction", $dir, $flag);
    
    # Perform limit checks on the wind direction.
    if ($dir < 0 && $dir ne Conversions::getMissing() || $dir > 360) {
	$self->$appendWarning("wind_direction", $dir,
			      "directions must be between 0 and 360.");
    }

    # Store the wind direction and flag to the Record
    $self->{"wind_direction_flag"} = $flag;
    return $self->{"wind_direction"} = $dir;
}

##-----------------------------------------------------------------------
# @signature void setWindSpeed(float speed, String measure)
# <p>Set the wind speed of the Record.  The wind speed is stored in m/s.</p>
# <p>Wind speed values less than 0 m/s or greater that 200 m/s are sent to the
# warning log for the Record.</p>
# 
# @input $speed The wind speed value from the raw data.
# @input $measure The units of velocity the wind speed value is measured.
# @output $wind_speed Returns the wind speed measured in m/s.
# @link <a href=http://www.joss.ucar.edu/cgi-bin/dpg/doc/docperl.cgi?file=/work/DPG_HTML/BEST_SW/conversion_modules/Version1/Conversions.pm#convertVelocity(float velocity, String in, String out)>Conversions::convertVelocity(float velocity, String in, String out)</a>
# @warning If the converted wind speed does not fit in the space allocated, the
# conversion will terminate.
# @limitation This function will only determine the QCF flags for missing and
# unchecked values for the wind speed.
##-----------------------------------------------------------------------
##-----------------------------------------------------------------------
# @signature void setWindSpeed(float speed, String measure, String flag)
# <p>Set the wind speed and its flag for the Record.  The wind speed is stored
# in m/s.</p>
# <p>Wind speed values less than 0 m/s or greater that 200 m/s are sent to the
# warning log for the Record.</p>
# 
# @input $speed The wind speed value from the raw data.
# @input $measure The units of velocity the wind speed value is measured.
# @input $flag The flag value to set for the wind speed.
# @output $wind_speed Returns the wind speed measured in m/s.
# @link <a href=http://www.joss.ucar.edu/cgi-bin/dpg/doc/docperl.cgi?file=/work/DPG_HTML/BEST_SW/conversion_modules/Version1/Conversions.pm#convertVelocity(float velocity, String in, String out)>Conversions::convertVelocity(float velocity, String in, String out)</a>
# @warning If the converted wind speed does not fit in the space allocated, the
# conversion will terminate.
# @warning If the flag given for the wind speed is not one character or if the
# flag is not a recognized flag, the conversion will terminate.
##-----------------------------------------------------------------------
sub setWindSpeed {
    my $self = shift;
    my $speed = shift;
    my $measure = shift;
    my $flag = shift;
    
    # Check to see if the flag was passed in as an arguement.
    if (!defined($flag)) {
	$flag = Conversions::getUncheckedFlag();
	if ($speed eq Conversions::getMissing()) {
	    $flag = Conversions::getMissingFlag();
	}
    }
    $speed = Conversions::convertVelocity($speed, $measure, "m/s");

    # Check to see if the value fits in the space set aside for wind speed.
    $self->$checkIfFits("wind_speed", $speed);
    $self->$checkFlagSize("wind_speed", $flag);
    $self->$checkValueWithFlag("wind_speed", $speed, $flag);
    
    # Perform limit checks on the wind speed.
    if ($speed < 0 && $speed ne Conversions::getMissing() ||
	$speed > 200) {
	$self->$appendWarning("wind_speed", $speed,
			      "wind speed must be between 0 and 200");
    }

    # Store the wind speed and flag to the Record.
    $self->{"wind_speed_flag"} = $flag;
    return $self->{"wind_speed"} = $speed;
}

##-----------------------------------------------------------------------
# @signature String toQCF_String()
# <p>Get the reading in QCF format.</p><p>The format does include the new
# line character at the end of the string.</p>
#
# @output $qcf_string Returns the data in the reading in QCF format.
##-----------------------------------------------------------------------
sub toQCF_String {
    my $self = shift;
    my $qcf_format = "%-8s %-5s %-8s %-5s %-10s %-15s %10.5f %11.5f %3d %7.2f %7.2f %s %7.2f %s %7.2f %s %7.2f %s %7.2f %s %7.2f %s %7.2f %s %7.2f %s %s %7.2f %s %4d %s %8.2f %s %7.2f %2d %s %2d %s %7.2f %2d %s %2d %s %7.2f %2d %s %2d %s\n";
    return sprintf ($qcf_format,
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
}
