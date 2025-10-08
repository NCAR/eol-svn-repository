#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The ClassRecord.pm module is a representation of a single record in a
# CLASS format sounding.</p>
#
# @author Joel Clawson
# @version 3.0 <p>The way missing/undefined values were changed.  There was a 
# bug in the previous version that prevented a value from being set to its
# missing value.</p>
# <p>Fixed a problem in the ascension rate calculation that had the missing
# time values being 999.0 instead of 9999.0.</p>
# <p>Added warnings and flag corrections for (value,flag) pairs that do not
# match.  Any missing value will now output the missing flag and the unchecked
# flag will be output for any not missing value when the flag has been set to
# missing.</p>
# <p>Checks for the number of parameters being to the functions has been added.
# This allows for some minor error checking.  The functions still do not check
# the incoming values, only the number of parameters.</p>
#
# @author Joel Clawson
# @version 2.0 Original Creation
##Module------------------------------------------------------------------------
package ClassRecord;
use strict;
use lib "/net/work/lib/perl/Utilities";
use DpgCalculations qw(:DEFAULT);
use DpgConversions qw(:DEFAULT);
use ClassConstants qw(:DEFAULT);
$| = 1;
    

##------------------------------------------------------------------------------
# @signature void check_size(FileHandle $WARN, float* value_ref, String missing, String name)
# <p>Check the size of the value with the space allocated for it in the line.</p>
#
# @input $WARN The file handle to the open warning file.
# @input $value_ref The pointer to the value being checked.
# @input $missing The missing value for the parameter.
# @input $name The name of the parameter being checked.
# @warning This function will set values to missing that will not fit in the value.
##------------------------------------------------------------------------------
sub check_size {
    my $self = shift;
    if (scalar(@_) != 5) { die("Invalid parameters to check_size\n"); }
    my ($WARN,$value_ref,$missing,$name,$format) = @_;

    if (defined($$value_ref) && 
	length(sprintf($format,$$value_ref)) > length($missing)) {
	printf($WARN 
	       "%s: %s value %s is too big for the field at time %s.  Setting to missing.\n",
	       $self->{"filename"},$name,$$value_ref,$self->getTime());
	$$value_ref = $missing;
    }
}

##------------------------------------------------------------------------------
# @signature void check_values()
# <p>Check all of the values in the ClassRecord.</p>
#
# @warning This will set values to missing that will not fit in the appropriate
# field.
##------------------------------------------------------------------------------
sub check_values {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to check_values\n"); }
    my $prev = $self->{"previous"};
    my $WARN = $self->{"warn"};

    # Check dew point for < -99.9
    if ($self->getDewPoint() < -99.9) {
	printf($WARN 
	       "%s: Dew Point value %s is less than -99.9 at time %s.  Setting to -99.9.\n",
	       $self->{"filename"},$self->getDewPoint(),$self->getTime());
	$self->setDewPoint("-99.9","C");
    }
    
    $self->check_size($WARN,\$self->{"time"},"9999.0","Time","%6.1f");
    $self->check_size($WARN,\$self->{"pressure"},"9999.0","Pressure","%6.1f");
    $self->check_size($WARN,\$self->{"temperature"},"999.0","Temperature","%5.1f");
    $self->check_size($WARN,\$self->{"dewpoint"},"999.0","Dew Point","%5.1f");
    $self->check_size($WARN,\$self->{"rel_humid"},"999.0","Relative Humidity","%5.1f");
    $self->check_size($WARN,\$self->{"uwind"},"9999.0","U Wind Component","%6.1f");
    $self->check_size($WARN,\$self->{"vwind"},"9999.0","V Wind Component","%6.1f");
    $self->check_size($WARN,\$self->{"wind_spd"},"999.0","Wind Speed","%5.1f");
    $self->check_size($WARN,\$self->{"wind_dir"},"999.0","Wind Direction","%5.1f");
    $self->check_size($WARN,\$self->{"asc_rate"},"999.0","Ascension Rate","%5.1f");
    $self->check_size($WARN,\$self->{"longitude"},"9999.000","Longitude","%8.3f");
    $self->check_size($WARN,\$self->{"latitude"},"999.000","Latitude","%7.3f");
    $self->check_size($WARN,\$self->{"var_1"},"999.0","Variable Parameter 1","%5.1f");
    $self->check_size($WARN,\$self->{"var_2"},"999.0","Variable Parameter 2","%5.1f");
    $self->check_size($WARN,\$self->{"altitude"},"99999.0","Altitude","%7.1f");

    $self->check_size($WARN,\$self->{"pressure_flag"},"99.0","Pressure Flag","%4.1f");
    $self->check_size($WARN,\$self->{"temperature_flag"},"99.0","Temperature Flag","%4.1f");
    $self->check_size($WARN,\$self->{"rel_humid_flag"},"99.0","Relative Humidity Flag","%4.1f");
    $self->check_size($WARN,\$self->{"uwind_flag"},"99.0","U Wind Flag","%4.1f");
    $self->check_size($WARN,\$self->{"vwind_flag"},"99.0","V Wind Flag","%4.1f");
    $self->check_size($WARN,\$self->{"asc_rate_flag"},"99.0","Ascension Rate Flag","%4.1f");

    # Do some comparisons between this and the previous record.
    if (defined($prev)) {

	if ($self->getTime() != 999.0 && $prev->getTime() != 999.0 &&
	    $self->getTime() <= $prev->getTime()) {
	    
	    printf($WARN "%s: Time Sequence: Time of %s is after %s\n",
		   $self->{"filename"},$self->getTime(),$prev->getTime());
	}

    }
}

##------------------------------------------------------------------------------
# @signature float getAltitude()
# <p>Get the altitude of the record.</p>
#
# @output $alt The altitude value.  Default: 99999.0
##------------------------------------------------------------------------------
sub getAltitude {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getAltitude\n"); }

    return (defined($self->{"altitude"})) ? $self->{"altitude"} : 99999.0;
}

##------------------------------------------------------------------------------
# @signature float getAscensionRate()
# <p>Get the ascension rate of the record.</p>
#
# @output $asc_rate The ascension rate value.  Default: 999.0
# @warning This function will attempt to calculate the ascension rate from the
# values in the previous record.
##------------------------------------------------------------------------------
sub getAscensionRate {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getAscensionRate\n"); }

    return $self->{"asc_rate"} if (defined($self->{"asc_rate"}));

    if (defined($self->{"previous"}) && 
	$self->getTime() != $self->{"previous"}->getTime() &&
	$self->getTime() != 9999.0 && $self->{"previous"}->getTime() != 9999.0 &&
	$self->getAltitude() != 99999.0 && $self->{"previous"}->getAltitude() != 99999.0) {

	my $rate = (($self->getAltitude() - $self->{"previous"}->getAltitude()) /
		    ($self->getTime() - $self->{"previous"}->getTime()));

	if (length(sprintf("%5.1f",$rate)) <= 5) { return $rate; }

	if (!defined($self->{"asc_rate_warn"})) {
	    my $WARN = $self->{"warn"};
	    printf($WARN "%s: Ascension Rate (%s) calulated to be too big for the field.  Setting to missing.\n",$self->{"filename"},$rate);
	    $self->{"asc_rate_warn"} = 1;
	}
	       
    }

    return 999.0;
}

##------------------------------------------------------------------------------
# @signature float getAscensionRateFlag()
# <p>Get the flag for the ascension rate for the record.  It is dependant on the
# value of the ascension rate if the flag has not been set.</p>
#
# @output $flag The ascension rate flag.
##------------------------------------------------------------------------------
sub getAscensionRateFlag {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getAscensionRateFlag\n"); }

    if (defined($self->{"asc_rate_flag"})) {
	my $WARN = $self->{"warn"};
	my $flag = $self->{"asc_rate_flag"};
	
	# Checks to make sure that flags match the data.
	if ($self->getAscensionRate() == 999.0 && $flag != $MISSING_FLAG) {
	    printf($WARN "%s: Ascension Rate missing at time %s without missing flag %s.  Setting flag to missing.\n",$self->{"filename"},$self->getTime(),$flag);
	    undef($self->{"asc_rate_flag"});
	} elsif ($self->getAscensionRate() != 999.0 && $flag == $MISSING_FLAG){
	    printf($WARN "%s: Ascension Rate flag at time %s missing without missing value %s.  Setting flag to unchecked.\n",$self->{"filename"},$self->getTime(),$self->getAscensionRate());
	    undef($self->{"asc_rate_flag"});
	}
    }

    return (defined($self->{"asc_rate_flag"})) ? $self->{"asc_rate_flag"} :
	($self->getAscensionRate() == 999.0 ? $MISSING_FLAG : 
	 $UNCHECKED_FLAG);
}

##------------------------------------------------------------------------------
# @signature float getDewPoint()
# <p>Get the dew point for the record.</p>
#
# @output $dewpt The dew point value.  Default: 999.0
# @warning This function will attempt to calculate the dew point if it has not
# been set or has been set to missing.
##------------------------------------------------------------------------------
sub getDewPoint {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getDewPoint\n"); }

    return $self->{"dewpoint"} if (defined($self->{"dewpoint"}));

    if ($self->getTemperature() != 999.0 && defined($self->{"rel_humid"})) {
	my $dewpt = calculateDewPoint($self->getTemperature(),$self->getRelativeHumidity(),
				      1,$self->{"warn"});
	return $dewpt if (defined($dewpt));
    }
    return 999.0
}

##------------------------------------------------------------------------------
# @signature float getLatitude()
# <p>Get the latitude of the record.</p>
#
# @output $lat The latitude value.  Default: 999.000
##------------------------------------------------------------------------------
sub getLatitude {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getLatitude\n"); }

    return defined($self->{"latitude"}) ? $self->{"latitude"} : 999.000;
}

##------------------------------------------------------------------------------
# @signature float getLongitude()
# <p>Get the longitude of the record.</p>
# 
# @output $lon The longitude value.  Default: 9999.000
##------------------------------------------------------------------------------
sub getLongitude {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getLongitude\n"); }

    return defined($self->{"longitude"}) ? $self->{"longitude"} : 9999.000;
}

##------------------------------------------------------------------------------
# @signature float getPressure()
# <p>Get the pressure of the record.</p>
#
# @output $press The pressure value.  Default: 9999.0
##------------------------------------------------------------------------------
sub getPressure {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getPressure\n"); }

    return defined($self->{"pressure"}) ? $self->{"pressure"} : 9999.0;
}

##------------------------------------------------------------------------------
# @signature float getPressureFlag()
# <p>Get the flag for the pressure value in the record.  It is dependant on the
# value of the pressure if the flag has not been set.</p>
#
# @output $flag The pressure flag.
##------------------------------------------------------------------------------
sub getPressureFlag {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getPressureFlag\n"); }

    if (defined($self->{"pressure_flag"})) {
	my $WARN = $self->{"warn"};
	my $flag = $self->{"pressure_flag"};
	
	# Checks to make sure that flags match the data.
	if ($self->getPressure() == 9999.0 && $flag != $MISSING_FLAG) {
	    printf($WARN "%s: Pressure missing at time %s without missing flag %s.  Setting flag to missing.\n",$self->{"filename"},$self->getTime(),$flag);
	    undef($self->{"pressure_flag"});
	} elsif ($self->getPressure() != 9999.0 && $flag == $MISSING_FLAG){
	    printf($WARN "%s: Pressure flag at time %s missing without missing value %s.  Setting flag to unchecked.\n",$self->{"filename"},$self->getTime(),$self->getPressure());
	    undef($self->{"pressure_flag"});
	}
    }

    return defined($self->{"pressure_flag"}) ? $self->{"pressure_flag"} : 
	($self->getPressure() == 9999.0 ? $MISSING_FLAG : 
	 $UNCHECKED_FLAG);
}

##------------------------------------------------------------------------------
# @signature float getRelativeHumidity()
# <p>Get the relative humidity for the record.</p>
#
# @output $rh The relative humidity value.  Default: 999.0
# @warning This function will try to calculate the relative humidity if it
# has not been set or if it has been set to missing.
##------------------------------------------------------------------------------
sub getRelativeHumidity {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getRelativeHumidity\n"); }

    return $self->{"rel_humid"} if (defined($self->{"rel_humid"}));

    if ($self->getTemperature() != 999.0 && defined($self->{"dewpoint"})) {
	my $rh = calculateRelativeHumidity($self->getTemperature(),$self->{"dewpoint"},
					   1,$self->{"warn"});
	return $rh if (defined($rh));
    }
    return 999.0;
}

##------------------------------------------------------------------------------
# @signature float getRelativeHumidityFlag()
# <p>Get the flag for the relative humidity value for the sounding.  It is 
# dependant on the value of the relative humidity if the flag has not been set.</p>
#
# @output $flag The relative humidity flag.
##------------------------------------------------------------------------------
sub getRelativeHumidityFlag {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getRelativeHumidityFlag\n"); }

    if (defined($self->{"rel_humid_flag"})) {
	my $WARN = $self->{"warn"};
	my $flag = $self->{"rel_humid_flag"};
	
	# Checks to make sure that flags match the data.
	if ($self->getRelativeHumidity() == 999.0 && $flag != $MISSING_FLAG) {
	    printf($WARN "%s: Relative Humidity missing at time %s without missing flag %s.  Setting flag to missing.\n",$self->{"filename"},$self->getTime(),$flag);
	    undef($self->{"rel_humid_flag"});
	} elsif ($self->getRelativeHumidity() != 999.0 && $flag == $MISSING_FLAG){
	    printf($WARN "%s: Relative Humidity flag at time %s missing without missing value %s.  Setting flag to unchecked.\n",$self->{"filename"},$self->getTime(),$self->getRelativeHumidity());
	    undef($self->{"rel_humid_flag"});
	}
    }

    return (defined($self->{"rel_humid_flag"})) ? $self->{"rel_humid_flag"} :
	($self->getRelativeHumidity() == 999.0 ? $MISSING_FLAG :
	 $UNCHECKED_FLAG);
}

##------------------------------------------------------------------------------
# @signature float getTemperature()
# <p>Get the temperature for the record.</p>
#
# @output $temp The temperature value.
##------------------------------------------------------------------------------
sub getTemperature {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getTemperature\n"); }

    return (defined($self->{"temperature"})) ? $self->{"temperature"} : 999.0;
}

##------------------------------------------------------------------------------
# @signature float getTemperatureFlag()
# <p>Get the flag for the temperature value for the record.  It is dependant 
# on the value of the temperature if the flag has not been set.</p>
#
# @output $flag The temperature flag.
##------------------------------------------------------------------------------
sub getTemperatureFlag {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getTemperatureFlag\n"); }

    if (defined($self->{"temperature_flag"})) {
	my $WARN = $self->{"warn"};
	my $flag = $self->{"temperature_flag"};
	
	# Checks to make sure that flags match the data.
	if ($self->getTemperature() == 999.0 && $flag != $MISSING_FLAG) {
	    printf($WARN "%s: Temperature missing at time %s without missing flag %s.  Setting flag to missing.\n",$self->{"filename"},$self->getTime(),$flag);
	    undef($self->{"temperature_flag"});
	} elsif ($self->getTemperature() != 999.0 && $flag == $MISSING_FLAG){
	    printf($WARN "%s: Temperature flag at time %s missing without missing value %s.  Setting flag to unchecked.\n",$self->{"filename"},$self->getTime(),$self->getTemperature());
	    undef($self->{"temperature_flag"});
	}
    }

    return (defined($self->{"temperature_flag"})) ? $self->{"temperature_flag"} :
	($self->getTemperature() == 999.0 ? $MISSING_FLAG :
	 $UNCHECKED_FLAG);
}

##------------------------------------------------------------------------------
# @signature float getTime()
# <p>Get the time of the record.</p>
#
# @output $time The time in seconds.  Default: 9999.0
##------------------------------------------------------------------------------
sub getTime {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getTime\n"); }

    return (defined($self->{"time"})) ? $self->{"time"} : 9999.0;
}

##------------------------------------------------------------------------------
# @signature float getUWindComponent()
# <p>Get the U wind component value for the record.</p>
#
# @output $uwind The U wind component value. Default: 9999.0
# @warning This function will attempt to calculate the value if it has not
# been set.
##------------------------------------------------------------------------------
sub getUWindComponent {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getUWindComponent\n"); }

    return $self->{"uwind"} if (defined($self->{"uwind"}));

    if (defined($self->{"wind_spd"}) && defined($self->{"wind_dir"})) {
	my $uwind = (calculateUVfromWind($self->{"wind_spd"},$self->{"wind_dir"},
					 1,$self->{"warn"}))[0];
	return $uwind if (defined($uwind));
    }
    return 9999.0;
}

##------------------------------------------------------------------------------
# @signature float getUWindComponentFlag()
# <p>Get the flag for the U wind component for the record.  It is dependant on the
# value of the U wind component if the flag has not been set.</p>
#
# @output $flag The flag for the U wind component.
##------------------------------------------------------------------------------
sub getUWindComponentFlag {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getUWindComponentFlag\n"); }

    if (defined($self->{"uwind_flag"})) {
	my $WARN = $self->{"warn"};
	my $flag = $self->{"uwind_flag"};
	
	# Checks to make sure that flags match the data.
	if ($self->getUWindComponent() == 9999.0 && $flag != $MISSING_FLAG) {
	    printf($WARN "%s: U Wind Component missing at time %s without missing flag %s.  Setting flag to missing.\n",$self->{"filename"},$self->getTime(),$flag);
	    undef($self->{"uwind_flag"});
	} elsif ($self->getUWindComponent() != 9999.0 && $flag == $MISSING_FLAG){
	    printf($WARN "%s: U Wind Component flag at time %s missing without missing value %s.  Setting flag to unchecked.\n",$self->{"filename"},$self->getTime(),$self->getUWindComponent());
	    undef($self->{"uwind_flag"});
	}
    }

    return (defined($self->{"uwind_flag"})) ? $self->{"uwind_flag"} :
	($self->getUWindComponent() == 9999.0 ? $MISSING_FLAG :
	 $UNCHECKED_FLAG);
}

##------------------------------------------------------------------------------
# @signature float getVariableValue(int index)
# <p>Get the value of the specified variable parameter.</p>
#
# @input $index The index of the variable.
# @output $value The value of the specified variable.  Default: 999.0
##------------------------------------------------------------------------------
sub getVariableValue {
    my $self = shift;
    if (scalar(@_) != 1) { die("Invalid parameters to getVariableValue\n"); }

    return (defined($self->{sprintf("var_%d",$_[0])})) ? 
	$self->{sprintf("var_%d",$_[0])} : 999.0;
}

##------------------------------------------------------------------------------
# @signature float getVWindComponent()
# <p>Get the V wind component value for the record.</p>
#
# @output $vwind The V wind component value.  Default: 9999.0
# @warning This function will try to calculate the V wind component if it has
# not been set.
##------------------------------------------------------------------------------
sub getVWindComponent {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getVWindComponent\n"); }

    return $self->{"vwind"} if (defined($self->{"vwind"}));

    if (defined($self->{"wind_spd"}) && defined($self->{"wind_dir"})) {
	my $vwind = (calculateUVfromWind($self->{"wind_spd"},$self->{"wind_dir"},
					 1,$self->{"warn"}))[1];
	return $vwind if (defined($vwind));
    }
    return 9999.0;
}

##------------------------------------------------------------------------------
# @signature float getVWindComponentFlag()
# <p>Get the flag for the V wind component for the record.  It is dependant on the
# value of the V wind component if the flag has not been set.</p>
#
# @output $flag The flag of the V wind component.
##------------------------------------------------------------------------------
sub getVWindComponentFlag {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getVWindComponentFlag\n"); }

    if (defined($self->{"vwind_flag"})) {
	my $WARN = $self->{"warn"};
	my $flag = $self->{"vwind_flag"};
	
	# Checks to make sure that flags match the data.
	if ($self->getVWindComponent() == 9999.0 && $flag != $MISSING_FLAG) {
	    printf($WARN "%s: V Wind Component missing at time %s without missing flag %s.  Setting flag to missing.\n",$self->{"filename"},$self->getTime(),$flag);
	    undef($self->{"vwind_flag"});
	} elsif ($self->getVWindComponent() != 9999.0 && $flag == $MISSING_FLAG){
	    printf($WARN "%s: V Wind Component flag at time %s missing without missing value %s.  Setting flag to unchecked.\n",$self->{"filename"},$self->getTime(),$self->getVWindComponent());
	    undef($self->{"vwind_flag"});
	}
    }

    return (defined($self->{"vwind_flag"})) ? $self->{"vwind_flag"} :
	($self->getVWindComponent() == 9999.0 ? $MISSING_FLAG :
	 $UNCHECKED_FLAG);
}

##------------------------------------------------------------------------------
# @signature float getWindDirection()
# <p>Get the wind direction for the record.</p>
#
# @output $dir The wind direction.  Default: 999.0
# @warning This function will attempt to calculate the wind direction if it
# has not been set.
##------------------------------------------------------------------------------
sub getWindDirection {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getWindDirection\n"); }

    return $self->{"wind_dir"} if (defined($self->{"wind_dir"}));

    if (defined($self->{"uwind"}) && defined($self->{"vwind"})) {
	my $winddir = (calculateWindFromUV($self->{"uwind"},$self->{"vwind"},
					   1,$self->{"warn"}))[1];
	return $winddir if (defined($winddir));
    }
    return 999.0;
}

##------------------------------------------------------------------------------
# @signature float getWindSpeed()
# <p>Get the wind speed for the record.</p>
#
# @output $spd The wind speed.  Default: 999.0
# @warning This function will attempt to calculate the wind speed if it has
# not been set.
##------------------------------------------------------------------------------
sub getWindSpeed {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getWindSpeed\n"); }

    return $self->{"wind_spd"} if (defined($self->{"wind_spd"}));

    if (defined($self->{"uwind"}) && defined($self->{"vwind"})) {
	my $windspd = (calculateWindFromUV($self->{"uwind"},$self->{"vwind"},
					   1,$self->{"warn"}))[0];
	return $windspd if (defined($windspd));
    }
    return 999.0;
}

##------------------------------------------------------------------------------
# @signature ClassRecord new(FILE* warn, String filename, --ClassRecord previous--)
# <p>Create a new ClassRecord object.</p>
#
# @input $warn The file that will contain the warning information.
# @input $filename The name of the file that will contain this data.
# @input $previous <b>Optional</b> This is the ClassRecord that preceded this 
#    record.  It is necessary to calculate ascension rates.
# @output $self The new ClassRecord object.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);

    if (scalar(@_) < 2 || scalar(@_) > 3) { die("Invalid parameters to ClassRecord->new\n"); }

    $self->{"warn"} = $_[0];
    $self->{"filename"} = $_[1];
    $self->{"previous"} = $_[2] if (defined($_[2]));

    return $self;
}

##------------------------------------------------------------------------------
# @signature void setAltitude(float alt, String unit)
# <p>Set the altitude/elevation for the record.</p>
#
# @input $alt The altitude value.
# @input $unit The unit of the altitude.
##------------------------------------------------------------------------------
sub setAltitude {
    my $self = shift;
    if (scalar(@_) != 2) { die("Invalid parameters to setAltitude\n"); }

    if (defined($_[0]) && $_[0] != 99999.0) {
	$self->{"altitude"} = convertLength($_[0],$_[1],"m");
    } else {
	undef($self->{"altitude"});
    }
}

##------------------------------------------------------------------------------
# @signature void setAscensionRate(float rate, String unit)
# <p>Set the ascension rate for the record.</p>
#
# @input $rate The ascenstion rate.
# @input $unit The unit of the ascension rate.
##------------------------------------------------------------------------------
sub setAscensionRate {
    my $self = shift;
    if (scalar(@_) != 2) { die("Invalid parameters to setAscensionRate\n"); }

    if (defined($_[0]) && $_[0] != 999.0) {
	$self->{"asc_rate"} = convertVelocity($_[0],$_[1],"m/s");
    } else {
	undef($self->{"asc_rate"});
    }
}

##------------------------------------------------------------------------------
# @signature void setAscensionRateFlag(float flag)
# <p>Set the flag for the ascension rate for the record.</p>
#
# @input $flag The flag for the ascension rate.
##------------------------------------------------------------------------------
sub setAscensionRateFlag {
    my $self = shift;
    if (scalar(@_) != 1) { die("Invalid parameters to setAscensionRateFlag\n"); }

    if (defined($_[0])) {
	$self->{"asc_rate_flag"} = $_[0];
    } else {
	undef($self->{"asc_rate_flag"});
    }
}

##------------------------------------------------------------------------------
# @signature void setDewPoint(float dewpt, String unit)
# <p>Set the dew point for the record.</p>
#
# @input $dewpt The dew point value.
# @input $unit The unit of the dew point.
##------------------------------------------------------------------------------
sub setDewPoint {
    my $self = shift;
    if (scalar(@_) != 2) { die("Invalid parameters to setDewPoint\n"); }

    if (defined($_[0]) && $_[0] != 999.0) {
	$self->{"dewpoint"} = convertTemperature($_[0],$_[1],"C");
    } else {
	undef($self->{"dewpoint"});
    }
}

##------------------------------------------------------------------------------
# @signature void setLatitude(String lat, String fmt)
# <p>Set the latitude for the record.</p>
#
# @input $lat The latitude value.
# @input $fmt The format of the latitude.
##------------------------------------------------------------------------------
sub setLatitude {
    my $self = shift;
    if (scalar(@_) != 2) { die("Invalid parameters to setLatitude\n"); }

    if (defined($_[0]) && $_[0] ne "999.000") {
	$self->{"latitude"} = (convertLatLong($_[0],$_[1],"D"))[0];
    } else {
	undef($self->{"latitude"});
    }
}

##------------------------------------------------------------------------------
# @signature void setLongitude(String lon, String fmt)
# <p>Set the longitude for the record.</p>
#
# @input $lon The longitude value.
# @input $fmt The format of the longitude.
##------------------------------------------------------------------------------
sub setLongitude {
    my $self = shift;
    if (scalar(@_) != 2) { die("Invalid parameters to setLongitude\n"); }

    if (defined($_[0]) && $_[0] ne "9999.000") {
	$self->{"longitude"} = (convertLatLong($_[0],$_[1],"D"))[0];
    } else {
	undef($self->{"longitude"});
    }
}

##------------------------------------------------------------------------------
# @signature void setPressure(float press, String unit)
# <p>Set the pressure for the record.</p>
#
# @input $press The pressure value.
# @input $unit The unit of the pressure value.
##------------------------------------------------------------------------------
sub setPressure {
    my $self = shift;
    if (scalar(@_) != 2) { die("Invalid parameters to setPressure\n"); }

    if (defined($_[0] && $_[0] != 9999.0)) {
	$self->{"pressure"} = convertPressure($_[0],$_[1],"mb");
    } else {
	undef($self->{"pressure"});
    }
}

##------------------------------------------------------------------------------
# @signature void setPressureFlag(float flag)
# <p>Set the flag for the pressure value of the record.</p>
#
# @input $flag The pressure flag.
##------------------------------------------------------------------------------
sub setPressureFlag {
    my $self = shift;
    if (scalar(@_) != 1) { die("Invalid parameters to setPressureFlag\n"); }

    if (defined($_[0])) {
	$self->{"pressure_flag"} = $_[0];
    } else {
	undef($self->{"pressure_flag"});
    }
}

##------------------------------------------------------------------------------
# @signature void setRelativeHumidity(float rh)
# <p>Set the relative humidity value of the record.</p>
#
# @input $rh The relative humidity value in percent.
##------------------------------------------------------------------------------
sub setRelativeHumidity {
    my $self = shift;
    if (scalar(@_) != 1) { die("Invalid parameters to setRelativeHumidity\n"); }

    if (defined($_[0]) && $_[0] != 999.0) {
	$self->{"rel_humid"} = $_[0];
    } else {
	undef($self->{"rel_humid"});
    }
}

##------------------------------------------------------------------------------
# @signature void setRelativeHumidityFlag(float flag)
# <p>Set the flag for the relative humidity of the record.</p>
#
# @input $flag The relative humidity flag.
##------------------------------------------------------------------------------
sub setRelativeHumidityFlag {
    my $self = shift;
    if (scalar(@_) != 1) { die("Invalid parameters to setRelativeHumidityFlag\n"); }

    if (defined($_[0])) {
	$self->{"rel_humid_flag"} = $_[0];
    } else {
	undef($self->{"rel_humid_flag"});
    }
}

##------------------------------------------------------------------------------
# @signature void setTemperature(float temp, String unit)
# <p>Set the temperature of the record.</p>
#
# @input $temp The temperature value.
# @input $unit The unit of the temperature.
##------------------------------------------------------------------------------
sub setTemperature {
    my $self = shift;
    if (scalar(@_) != 2) { die("Invalid parameters to setTemperature\n"); }

    if (defined($_[0]) && $_[0] != 999.0) {
	$self->{"temperature"} = convertTemperature($_[0],$_[1],"C");
    } else {
	undef($self->{"temperature"});
    }
}

##------------------------------------------------------------------------------
# @signature void setTemperatureFlag(float flag)
# <p>Set the flag for the temperature of the record.</p>
#
# @input $flag The temperature flag.
##------------------------------------------------------------------------------
sub setTemperatureFlag {
    my $self = shift;
    if (scalar(@_) != 1) { die("Invalid parameters to setTemperatureFlag\n"); }

    if (defined($_[0])) {
	$self->{"temperature_flag"} = $_[0];
    } else {
	undef($self->{"temperature_flag"});
    }
}

##------------------------------------------------------------------------------
# @signature void setTime(float secs)
# <p>Set the time of the record.</p>
#
# @input $secs The number of seconds after release.
##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
# @signature void setTime(float mins, float secs)
# <p>Set the time of the record.</p>
#
# @input $mins The number of minutes after release.
# @input $secs The number of seconds after the minute.
##------------------------------------------------------------------------------
sub setTime {
    my $self = shift;
    if (scalar(@_) < 1 || scalar(@_) > 2) { die("Invalid parameters to setTime\n"); }

    if (defined($_[0]) && $_[0] != 9999.0) {
	$self->{"time"} = (scalar(@_) == 2) ? 
	    convertTime($_[0],"min","sec") + $_[1]: $_[0];
    } else {
	undef($self->{"time"});
    }
}

##------------------------------------------------------------------------------
# @signature void setUWindComponent(float uwind, String unit)
# <p>Set the U wind component of the record.</p>
#
# @input $uwind The U wind component value.
# @input $unit The unit of the U wind component.
##------------------------------------------------------------------------------
sub setUWindComponent {
    my $self = shift;
    if (scalar(@_) != 2) { die("Invalid parameters to setUWindComponent\n"); }

    if (defined($_[0]) && $_[0] != 9999.0) {
	$self->{"uwind"} = convertVelocity($_[0],$_[1],"m/s");
    } else {
	undef($self->{"uwind"});
    }
}

##------------------------------------------------------------------------------
# @signature void setUWindComponentFlag(float flag)
# <p>Set the flag for the U wind component for the record.</p>
#
# @input $flag The U wind component flag.
##------------------------------------------------------------------------------
sub setUWindComponentFlag {
    my $self = shift;
    if (scalar(@_) != 1) { die("Invalid parameters to setUWindComponentFlag\n"); }

    if (defined($_[0])) { $self->{"uwind_flag"} = $_[0]; }
    else { undef($self->{"uwind_flag"}); }
}

##------------------------------------------------------------------------------
# @signature void setVariableValue(int index, float value)
# <p>Set the value of the specified variable parameter.</p>
#
# @input $index The index of the variable parameter.
# @input $value The value of the variable parameter.
##------------------------------------------------------------------------------
sub setVariableValue {
    my $self = shift;
    if (scalar(@_) != 2) { die("Invalid parameters to setVariableValue\n"); }

    if (defined($_[1]) && $_[1] != 999.0) {
	$self->{sprintf("var_%d",$_[0])} = $_[1];
    } else {
	undef($self->{sprintf("var_%d",$_[0])});
    }
}

##------------------------------------------------------------------------------
# @signature void setVWindComponent(float uwind, String unit)
# <p>Set the V wind component of the record.</p>
#
# @input $uwind The V wind component value.
# @input $unit The unit of the V wind component.
##------------------------------------------------------------------------------
sub setVWindComponent {
    my $self = shift;
    if (scalar(@_) != 2) { die("Invalid parameters to setVWindComponent\n"); }

    if (defined($_[0]) && $_[0] != 9999.0) {
	$self->{"vwind"} = convertVelocity($_[0],$_[1],"m/s");
    } else {
	undef($self->{"vwind"});
    }
}

##------------------------------------------------------------------------------
# @signature void setVWindComponentFlag(float flag)
# <p>Set the flag for the V wind component for the record.</p>
#
# @input $flag The V wind component flag.
##------------------------------------------------------------------------------
sub setVWindComponentFlag {
    my $self = shift;
    if (scalar(@_) != 1) { die("Invalid parameters to setVWindComponentFlag\n"); }

    if (defined($_[0])) { $self->{"vwind_flag"} = $_[0]; }
    else { undef($self->{"vwind_flag"}); }
}

##------------------------------------------------------------------------------
# @signature void setWindDirection(float dir)
# <p>Set the wind direction of the record.</p>
#
# @input $dir The wind direction in degrees.
##------------------------------------------------------------------------------
sub setWindDirection {
    my $self = shift;
    if (scalar(@_) != 1) { die("Invalid parameters to setWindDirection\n"); }

    if (defined($_[0]) && $_[0] != 999.0) {
	$self->{"wind_dir"} = $_[0];
    } else {
	undef($self->{"wind_dir"});
    }
}

##------------------------------------------------------------------------------
# @signature void setWindSpeed(float spd, String unit)
# <p>Set the wind speed of the record.</p>
#
# @input $spd The wind speed value.
# @input $unit The unit of the wind speed.
##------------------------------------------------------------------------------
sub setWindSpeed {
    my $self = shift;
    if (scalar(@_) != 2) { die("Invalid parameters to setWindSpeed\n"); }

    if (defined($_[0]) && $_[0] != 999.0) {
	$self->{"wind_spd"} = convertVelocity($_[0],$_[1],"m/s");
    } else {
	undef($self->{"wind_spd"});
    }
}

##------------------------------------------------------------------------------
# @signature String toString()
# <p>Convert the ClassRecord into its String representation.
#
# @output The String representation of the ClassRecord.
##------------------------------------------------------------------------------
sub toString {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to toString\n"); }

    $self->check_values();

    my $read_fmt = "%6.1f %6.1f %5.1f %5.1f %5.1f %6.1f %6.1f ".
	"%5.1f %5.1f %5.1f %8.3f %7.3f %5.1f %5.1f %7.1f %4.1f ".
	    "%4.1f %4.1f %4.1f %4.1f %4.1f\n";

    return sprintf($read_fmt,
		   $self->getTime(),
		   $self->getPressure(),
		   $self->getTemperature(),
		   $self->getDewPoint(),
		   $self->getRelativeHumidity(),
		   $self->getUWindComponent(),
		   $self->getVWindComponent(),
		   $self->getWindSpeed(),
		   $self->getWindDirection(),
		   $self->getAscensionRate(),
		   $self->getLongitude(),
		   $self->getLatitude(),
		   $self->getVariableValue(1),
		   $self->getVariableValue(2),
		   $self->getAltitude(),
		   $self->getPressureFlag(),
		   $self->getTemperatureFlag(),
		   $self->getRelativeHumidityFlag(),
		   $self->getUWindComponentFlag(),
		   $self->getVWindComponentFlag(),
		   $self->getAscensionRateFlag()
		   );
}

1;









