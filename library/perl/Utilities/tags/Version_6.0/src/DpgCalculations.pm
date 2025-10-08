#! /usr/bin/perl -w

##Module---------------------------------------------------------------------
# <p>The DpgCalculations.pm module is a collection of functions that calculate
# different values.  In previous versions, this used to be part of the 
# Conversions.pm modules.  It has been seperated from the module since they
# are not really conversions, but calculations.</p>
# <p>There exists a testing suite for the DpgCalculations.pm functions.  They
# are located in teh TestDpgCalculations.pl script.  It should be rerun after 
# changes have been made to the module to ensure that the functions still work
# correctly.  New tests should also be added when new functionality is added
# to the module.</p>
#
# @use   use lib "/work/DPG_HTML/BEST_SW/conversion_modules/Version3";
#        use DpgCalculations qw(:DEFAULT);
# <p>The functions can be called like any other function.</p>
#
# @author Joel Clawson
# @version 3.0 <p>The module was updated to export the functions as global
# variables as long as the qw(:DEFAULT) is included in the use clause.  This
# allows the calculations to be used without having to call DpgCalculations::function.</p>
# <p>More functions were added including the calculateMixingRatio, calculateVaporPressure,
# and calcualteVirtualTemperature.  These were added to provide a single point
# of access for other calculations as well as make it available for the web
# interface.</p>
# <p>The missing value of -999.99 was changed to undef.  This is to allow the
# calculations to be independant of the QCF format.  The users of the 
# calculations must handle the undefined values instead of the calculations
# handling them.</p>
# <p>Checks on the parameter list size were also added.  This is to try to help
# find errors quicker by preventing undefined values from being passed to a 
# function.  It still does not check the values, only ensures that a correct
# number of parameters are passed to the function.</p>
#
# @author Joel Clawson
# @version 2.0 No changes documented.
#
# @author Joel Clawson
# @version 1.0 Original Version.
##Module---------------------------------------------------------------------
package DpgCalculations;
use strict;
use lib ".";
use DpgConstants qw(:DEFAULT);
use DpgConversions qw(:DEFAULT);
use Exporter;
use POSIX;
our @ISA = ("Exporter");
our @EXPORT = qw(calculateAirDensity calculateAltitude calculateDewPoint calculateMixingRatio calculateRelativeHumidity calculateSeaLevelPressure calculatePressureFromAltimeter calculateSpecificHumidity calculateUVfromWind calculateVaporPressure calculateVirtualTemperature calculateWindFromUV);
$| = 1; # Turn on auto flush.

##---------------------------------------------------------------------------
# @signature float calculateAirDensity(float press, float temp, --int verbose--, --FileHandle OUT--)
# <p>Calculate the density of air from the pressure and temperature.  The
# air density will be calculated in mbar/J/kg.  The air density will be
# <i>undef</i> if the value cannot be calculated.</p>
# @warning The pressure must be in millibars and the temperature in &deg;K.
#
# @input $press The pressure value in mbar.
# @input $temp The temperature value in &deg;K.
# @input $verbose <b>Optional</b> A flag if error messages should be generated.
# Default == 1.
# @input $OUT <b>Optional</b> A FileHandle where the errors are to be printed.
# Default == *STDOUT.
# @output $air_dens The air density in mbar/J/kg.
##---------------------------------------------------------------------------
sub calculateAirDensity {
    if (scalar(@_) < 2 || scalar(@_) > 4) {
	die("Invalid parameters to calculateAirDensity\n");
    }
    my ($press,$temp,$verbose,$OUT) = @_;
    $verbose = 1 if (!defined($verbose));
    $OUT = *STDOUT if (!defined($OUT));

    # Make sure that the pressure is defined.
    if (!defined($press)) {
	printf($OUT "calculateAirDensity:  Undefined Pressure.  Air Density not calculated.\n") if ($verbose);
	return undef();
    }

    # Make sure that the temperature is defined.
    if (!defined($temp)) {
	printf($OUT "calculateAirDensity:  Undefined Temperature.  Air Density not calculated.\n") if $verbose;
	return undef();
    }
    
    return $press / ($Rdry * $temp);
}

##---------------------------------------------------------------------------
# @signature float calculateAltitude(float last_press, float last_temp, float last_dewpt, float last_alt, float this_press, float this_temp, float this_dewpt, --int verbose--, --FileHandle OUT-)
# <p>Calculate the altitude from the pressures, temperatures, dew points, and 
# last altitude of the previous known value and the current level.  The altitude
# will be calculated in meters.  The altitude will be <i>undef</i> if the value
# cannot be calculated.</p>
# @warning The pressures must be in millibars, the altitude in meters, and the
# dew points and temperatures in &deg;C.
#
# @input $last_press The last known pressure value in mbars.
# @input $last_temp The last known temperature value in &deg;C.
# @input $last_dewpt The last known dew point value in &deg;C (can be undef).
# @input $last_alt The last known altitude value in m.
# @input $this_press The pressure value at the calculated altitude in mbar.
# @input $this_temp The temperature value at the calculated altitude in &deg;C.
# @input $this_dewpt The dew point value at the calculate altitude in &deg;C.
# (can be undef)
# @input $verbose <b>Optional</b> A flag if error messages should be generated.
# Default == 1
# @input $OUT <b>Optional</b> A FileHandle where the errors are to be printed.
# Default == *STDOUT
# @output $this_alt The altitude value in meters.
##---------------------------------------------------------------------------
sub calculateAltitude {
    if (scalar(@_) < 7 || scalar(@_) > 9) {
	die("Invalid parameters to calculateAltitude\n");
    }
    my ($last_press,$last_temp,$last_dewpt,$last_alt,
	$this_press,$this_temp,$this_dewpt,$verbose,$OUT) = @_;
    $verbose = $TRUE if (!defined($verbose));
    $OUT = *STDOUT if (!defined($OUT));

    # Make sure that the last values are defined.
    if (!defined($last_press)) {
	printf($OUT "calculateAltitude:  Last Pressure not defined.  Altitude not calculated.\n") if ($verbose);
	return undef();
    }
    if (!defined($last_temp)) {
	printf($OUT "calculateAltitude:  Last Temperature not defined.  Altitude not calculated.\n") if ($verbose);
	return undef();
    }
    if (!defined($last_alt)) {
	printf($OUT "calculateAltitude:  Last Altitude not defined.  Altitude not calculated.\n") if ($verbose);
	return undef();
    }

    # Make sure that the this values are defined
    if (!defined($this_press)) {
	printf($OUT "calculateAltitude:  This Pressure not defined.  Altitude not calculated.\n") if ($verbose);
	return undef();
    }
    if (!defined($this_temp)) {
	printf($OUT "calculateAltitude:  This Temperature not defined.  Altitude not calculated.\n") if ($verbose);
	return undef();
    }

    # Convert temperature to K
    $this_temp = convertTemperature($this_temp,"C","K");
    $last_temp = convertTemperature($last_temp,"C","K");

    # Define the average virtual temperature function.
    my $averageVirtualTemperature = sub {
	my ($last_temp,$last_dens,$this_temp,$this_dens) = @_;
	return ((($this_dens * $this_temp) + ($last_dens * $last_temp)) /
		($this_dens + $last_dens));
    };

    # Calculate the average virtual temperature
    my $avg_virt_temp;
    if (defined($last_dewpt) && defined($this_dewpt)) {
	my $last_virt_temp = calculateVirtualTemperature($last_temp,calculateMixingRatio($last_press,calculateVaporPressure(convertTemperature($last_temp,"K","C"),$verbose,$OUT),$verbose,$OUT),$verbose,$OUT);
	my $this_virt_temp = calculateVirtualTemperature($this_temp,calculateMixingRatio($this_press,calculateVaporPressure(convertTemperature($this_temp,"K","C"),$verbose,$OUT),$verbose,$OUT),$verbose,$OUT);
	my $last_dens = calculateAirDensity($last_press,$last_virt_temp,$verbose,$OUT);
	my $this_dens = calculateAirDensity($this_press,$this_virt_temp,$verbose,$OUT);
	$avg_virt_temp = \&$averageVirtualTemperature($last_virt_temp,$last_dens,$this_virt_temp,$this_dens);
    } elsif (defined($this_dewpt)) {
	my $this_virt_temp = calculateVirtualTemperature($this_temp,calculateMixingRatio($this_press,calculateVaporPressure(convertTemperature($this_temp,"K","C"),$verbose,$OUT),$verbose,$OUT),$verbose,$OUT);
	my $this_dens = calculateAirDensity($this_press,$this_virt_temp,$verbose,$OUT);
	my $last_dens = calculateAirDensity($last_press,$last_temp,$verbose,$OUT);
	$avg_virt_temp = \&$averageVirtualTemperature($last_temp,$last_dens,$this_virt_temp,$this_dens);
    } elsif (defined($last_dewpt)) {
	my $last_virt_temp = calculateVirtualTemperature($last_temp,calculateMixingRatio($last_press,calculateVaporPressure(convertTemperature($last_temp,"K","C"),$verbose,$OUT),$verbose,$OUT),$verbose,$OUT);
	my $last_dens = calculateAirDensity($last_press,$last_virt_temp,$verbose,$OUT);
	my $this_dens = calculateAirDensity($this_press,$this_temp,$verbose,$OUT);
	$avg_virt_temp = \&$averageVirtualTemperature($last_virt_temp,$last_dens,$this_temp,$this_dens);
    } else {
	my $this_dens = calculateAirDensity($this_press,$this_temp,$verbose,$OUT);
	my $last_dens = calculateAirDensity($last_press,$last_temp,$verbose,$OUT);
	$avg_virt_temp = \&$averageVirtualTemperature($last_temp,$last_dens,$this_temp,$this_dens);
    }

    # Calculate the altitude.
    return $last_alt + ((($Rdry * $$avg_virt_temp)/$GRAVITY) * 
			(log($last_press/$this_press)));
}

##---------------------------------------------------------------------------
# @signature float calculateDewPoint(float temp, float rh, --int verbose--, --FileHandle OUT--)
# <p>Calculate the dew point from the temperature and relative humidity.  The
# dew point that is returned is in &deg;C.  The function will return <i>undef</i>
# for the dew point if it cannot be calculated.</p>
# @warning The temperature needs to be in &deg;C and the relative humidity in %.
#
# @input $temp The temperature value in &deg;C.
# @input $rh The relative humidity value in %.
# @input $verbose <b>Optional</b> A flag for printing out error messages.
# Default == 1
# @input $OUT <b>Optional</b> A FileHandle where the errors are to be printed.
# Default == *STDOUT
# @output $dewpt The dew point in &deg;C.
##---------------------------------------------------------------------------
sub calculateDewPoint {
    if (scalar(@_) < 2 || scalar(@_) > 4) {
	die("Invalid parameters to calculateDewPoint\n");
    }
    my ($temp,$rh,$verbose,$OUT) = @_;
    $verbose = 1 if (!defined($verbose));
    $OUT = *STDOUT if (!defined($OUT));

    # Make sure that the temperature is defined.
    if (!defined($temp)) {
	printf($OUT "calculateDewPoint:  Undefined Temperature.  Dew point not calculated.\n") if $verbose;
	return undef();
    }
    
    # Make sure that the relative humidity is defined.
    if (!defined($rh)) {
	printf($OUT "calculateDewPoint:  Undefined Relative Humidity.  Dew point not calculated.\n") if ($verbose);
	return undef();
    }

    # Make sure that the relative humidity is valid (positive rh).
    if ($rh <= 0) {
	printf($OUT "calculateDewPoint:  Invalid Relative Humidity (%s) is <= 0.  Dew point not calculated.\n",$rh) if ($verbose);
	return undef();
    }

    # Calculate the dew point.
    my $sat_vapor_pressure = calculateVaporPressure($temp,$verbose,$OUT);
    my $vapor_pressure = $sat_vapor_pressure * ($rh / 100.0);
    my $log_value = log($vapor_pressure / $ES0);
    return ($log_value * 243.5 / (17.67 - $log_value));
}

##---------------------------------------------------------------------------
# @signature float calculateMixingRatio(float press, float vapor_press, --int verbose--, --FileHandle OUT--)
# <p>Calculate the mixing ratio from the pressure and vapor pressure.  The 
# mixing ratio will be in kg/kg.  The mixing ratio will be <i>undef</i> if the
# value cannot be calculated.</p>
# @warning The pressure and vapor pressure must be in millibars.
#
# @input $press The pressure value in millibars.
# @input $vapor_press The vapor pressure value in millibars.
# @input $verbose <b>Optional</b> A flag if error messages should be generated.
# Default == 1
# @input $OUT <b>Optional</b> A FileHandle where the errors should be printed.
# Default == *STDOUT
# @ouptut $mixr The mixing ratio in kg/kg.
##---------------------------------------------------------------------------
sub calculateMixingRatio {
    if (scalar(@_) < 2 || scalar(@_) > 4) {
	die("Invalid parameters to calculateMixingRatio\n");
    }
    my ($press,$vapor_pressure,$verbose,$OUT) = @_;
    $verbose = 1 if (!defined($verbose));
    $OUT = *STDOUT if (!defined($OUT));

    # Make sure that the pressure is defined.
    if (!defined($press)) {
	printf($OUT "calculateMixingRatio:  Undefined Pressure.  Mixing Ratio not calculated.\n") if ($verbose);
	return undef();
    }

    # Make sure that the vapor pressure is defined.
    if (!defined($vapor_pressure)) {
	printf($OUT "calculateMixingRatio:  Undefined Vapor Pressure.  Mixing Ratio not calculated.\n") if ($verbose);
	return undef();
    }

    # Calculate the mixing ratio
    my $e = $vapor_pressure * (1.001 + ($press - 100.0) / 900.0 * 0.0034);
    return $EPS * ($e / ($press - $e));
}

##---------------------------------------------------------------------------
# @signature float calculatePressureFromAltimeter(float alt, float elev, --int verbose--, --FileHandle OUT--)
# <p>Calculate the pressure from the altimeter and elevation.  The pressure
# will be in millibars.  The pressure will be <i>undef</i> if the pressure
# value cannot be calculated.</p>
# @warning The altimeter must be in millibars and the elevation in meters.
#
# @input $alt The altimeter value in millibars.
# @input $elev The elevation value is meters.
# @input $verbose <b>Optional</b> A flag if error messages should be generated.
# Default == 1
# @input $OUT <b>Optional</b> A FileHandle where the errors should be printed.
# Default == *STDOUT
# @output $press The pressure in millibars.
##---------------------------------------------------------------------------
sub calculatePressureFromAltimeter {
    if (scalar(@_) < 2 || scalar(@_) > 4) {
	die("Invalid parameters to calculatePressureFromAltimeter\n");
    }
    my ($alt,$elev,$verbose,$OUT) = @_;
    $verbose = 1 if (!defined($verbose));
    $OUT = *STDOUT if (!defined($OUT));

    # Make sure the altimeter is defined
    if (!defined($alt)) {
	printf($OUT "calculatePressureFromAltimeter:  Altimeter not defined.  Pressure not calculated.\n") if ($verbose);
	return undef();
    }

    # Make sure the elevation is defined.
    if (!defined($elev)) {
	printf($OUT "calculatePressureFromAltimeter:  Elevation not defined.  Pressure not calculated.\n") if ($verbose);
	return undef();
    }

    # Define the exponent constant.
    my $n = $PRESS_LAPSE * $Rdry / $GRAVITY;

    # Make sure a real value can be calculated.
    if (($alt ** $n) < ((($StdSLPress ** $n) * $PRESS_LAPSE) / 
			convertTemperature($StdSLTemp,"C","K"))) {
	printf($OUT "calculatePressureFromAltimeter:  Altimeter value (%f) caused an imaginary number.  Pressure not calculated.\n",$alt) if ($verbose);
	return undef();
    }

    return ((($alt**$n)-(((($StdSLPress**$n)*$PRESS_LAPSE)/
			  convertTemperature($StdSLTemp,"C","K"))*$elev))**(1/$n));
}

##---------------------------------------------------------------------------
# @signature float calculateRelativeHumidity(float temp, flaot dewpt, --int verbose--, --FileHandle OUT--)
# <p>Calculate the relative humidity from the temperature and dew point.  The
# relative humidity will be calculated in percent.  The relative humidity will
# be <i>undef</i> if the value cannot be calculated.</p>
# @warning The temperature and dew point must be in &deg;C.
#
# @input $temp The temperature value in &deg;C.
# @input $dewpt The dew point value in &deg;C.
# @input $verbose <b>Optional</b> A flag if error messages should be generated.
# Default == 1
# @input $OUT <b>Optional</b> A FileHandle where the errors are to be printed.
# Default == *STDOUT
# @output $rh The relative humidity in %.
##---------------------------------------------------------------------------
sub calculateRelativeHumidity {
    if (scalar(@_) < 2 || scalar(@_) > 4) {
	die("Invalid parameters to calculateRelativeHumidity\n");
    }
    my ($temp,$dewpt,$verbose,$OUT) = @_;
    $verbose = 1 if (!defined($verbose));
    $OUT = *STDOUT if (!defined($OUT));

    # Make sure that the temperature is defined
    if (!defined($temp)) {
	printf($OUT "calculateRelativeHumidity:  Temperature not defined.  Relative Humidity not calculated.\n") if ($verbose);
	return undef();
    }

    # Make sure that the dew point is defined.
    if (!defined($dewpt)) {
	printf($OUT "calculateRelativeHumidity:  Dew Point not defined.  Relative Humidity not calculated.\n") if ($verbose);
	return undef();
    }

    # Calculate the relative humidity
    my $sat_vapor_pressure = calculateVaporPressure($temp,$verbose,$OUT);
    my $vapor_pressure = calculateVaporPressure($dewpt,$verbose,$OUT);
    return (100.0 * $vapor_pressure / $sat_vapor_pressure);
}

##---------------------------------------------------------------------------
# @signature float calculateSeaLevelPressure(float press, float elev, float dewpt, float temp, --int verbose--, --FileHandle OUT--)
# <p>Calculate the sea level pressure from the pressure, elevation, dew point,
# and temperature.  The sea level pressure will be in millibars.  The sea 
# level pressure will be <i>undef</i> if the value cannot be calculated.</p>
# @warning The pressure must be in millibars, the elevation in meters, and the
# dew point and temperature in &deg;C.
#
# @input $press The pressure value in millibars.
# @input $elev The elevation value in meters.
# @input $dewpt The dew point value in &deg;C.
# @input $temp The temperature value in &deg;C.
# @input $verbose <b>Optional</b> A flag if error messages should be generated.
# Default == 1
# @input $OUT <b>Optional</b> A FileHandle where the errors should be printed.
# Default == *STDOUT
# @output $slp The sea level pressure in millibars.
##---------------------------------------------------------------------------
sub calculateSeaLevelPressure {
    if (scalar(@_) < 4 || scalar(@_) > 6) {
	die("Invalid parameters to calculateSeaLevelPressure\n");
    }
    my ($press,$elev,$dewpt,$temp,$verbose,$OUT) = @_;
    $verbose = 1 if (!defined($verbose));
    $OUT = *STDOUT if (!defined($OUT));

    # Make sure that the pressure is defined.
    if (!defined($press)) {
	printf($OUT "calcluateSeaLevelPressure:  Pressure not defined.  Sea Level Pressure not calculated.\n") if ($verbose);
	return undef();
    }

    # Make sure that the elevation is defined.
    if (!defined($elev)) {
	printf($OUT "calculateSeaLevelPressure:  Elevation not defined.  Sea Level Pressure not calculated.\n") if ($verbose);
	return undef();
    }

    # Make sure that the temperature is defined.
    if (!defined($temp)) {
	printf($OUT "calculateSeaLevelPressure:  Temperature not defined.  Sea Level Pressure not calculated.\n") if ($verbose);
	return undef();
    }

    # Make sure that the dew point is defined.
    if (!defined($dewpt)) {
	printf($OUT "calculateSeaLevelPressure:  Dew Point not defined.  Sea Level Pressure not calculated.\n") if ($verbose);
	return undef();
    }

    my $vapor_pressure = calculateVaporPressure($dewpt,$verbose,$OUT);
    my $mix_ratio = calculateMixingRatio($press,$vapor_pressure,$verbose,$OUT);
    my $virt_temp_k = calculateVirtualTemperature(convertTemperature($temp,"C","K"),
						  $mix_ratio,$verbose,$OUT);
    my $delta_vtk = $PRESS_LAPSE * $elev;
    my $avg_virt_temp_k = $virt_temp_k - $delta_vtk / 2.0;
    return $press * exp(($GRAVITY * $elev) / ($Rdry * $avg_virt_temp_k));
}

##---------------------------------------------------------------------------
# @signature float calculateSpecificHumidity(float press, float dewpt, --int verbose--, --FileHandle OUT--)
# <p>Calculate the specific humidity from the pressure and dew point.  The specific
# humidity will be returned in %.  The value will be <i>undef</i> if the specific
# humidity cannot be calculated.</p>
# @warning The pressure must be in millibars and the dew point in &deg;C.
#
# @input $press The pressure value in millibars.
# @input $dewpt The dew point value in &deg;C.
# @input $verbose <b>Optional</b> A flag if error messages should be generated.
# Default == 1
# @input $OUT <b>Optional</b> A FileHandle where errors should be printed.
# Default == *STDOUT
# @output $sh The specific humidity in %
##---------------------------------------------------------------------------
sub calculateSpecificHumidity {
    if (scalar(@_) < 2 || scalar(@_) > 4) {
	die("Invalid parameters to calculateSpecificHumidity\n");
    }
    my ($press,$dewpt,$verbose,$OUT) = @_;
    $verbose = 1 if (!defined($verbose));
    $OUT = *STDOUT if (!defined($OUT));

    # Make sure that the pressure is defined.
    if (!defined($press)) {
	printf($OUT "calculateSpecificHumidity:  Pressure not defined.  Specific Humidity not calculated.\n") if ($verbose);
	return undef();
    }

    # Make sure that the dew point is defined.
    if (!defined($dewpt)) {
	printf($OUT "calculateSpecificHumidity:  Dew Point not defined.  Specific Humidity not calculated.\n") if ($verbose);
	return undef();
    }

    my $vapor_press = calculateVaporPressure($dewpt,$verbose,$OUT);
    return ($EPS * $vapor_press)/($press - (.378 * $vapor_press));

    return undef();
}

##---------------------------------------------------------------------------
# @signature (float,float) calculateUVfromWind(float windspd, float winddir, --int verbose--, --FileHandle OUT--)
# <p>Calculate the U and V wind components from the wind speed and wind 
# direction.  The component values will be calculated in m/s.  The values will
# be <i>undef</i> if they cannot be calculated.</p>
# @warning The wind speed must be in m/s and the wind direction in degrees.
#
# @input $windspd The wind speed value in m/s.
# @input $winddir The wind direction value in degrees.
# @input $verbose <b>Optional</b> A flag if error messages should be generated.
# Default == 1
# @input $OUT <b>Optional</b> A FileHandler where the errors should be printed.
# Default == *STDOUT
# @output $uwind The U wind component in m/s.
# @ouptut $vwind The V wind component in m/s.
##---------------------------------------------------------------------------
sub calculateUVfromWind {
    if (scalar(@_) < 2 || scalar(@_) > 4) {
	die("Invalid parameters to calculateUVfromWind\n");
    }
    my ($windspd,$winddir,$verbose,$OUT) = @_;
    $verbose = 1 if (!defined($verbose));
    $OUT = *STDOUT if (!defined($OUT));

    # Make sure wind speed is defined.
    if (!defined($windspd)) {
	printf($OUT "calculateUVfromWind:  Wind Speed not defind.  U and V not calculated.\n") if ($verbose);
	return (undef(),undef);
    }

    # Make sure wind direction is defined.
    if (!defined($winddir)) {
	printf($OUT "calculateUVfromWind:  Wind Direction not defined.  U and V not calculated.\n") if ($verbose);
	return (undef(),undef());
    }

    # Make sure wind speed is valid (non-negative)
    if ($windspd < 0) {
	printf($OUT "calculateUVfromWind:  Wind Speed (%f) is negative.  U and V not calculated.\n",$windspd) if ($verbose);
	return (undef(),undef());
    }

    $winddir = 0 if ($winddir == 360);

    # Make sure wind direction is valid
    if ($winddir < 0 || $winddir >= 360) {
	printf($OUT "calculateUVfromWind:  Wind Direction (%f) is not valid.  U and V not calculated.\n",$winddir) if ($verbose);
	return (undef(),undef());
    }

    return (-sin(convertAngle($winddir,"deg","rad")) * $windspd,
	    -cos(convertAngle($winddir,"deg","rad")) * $windspd);
}

##---------------------------------------------------------------------------
# @signature float calculateVaporPressure(float temp, --int verbose--, --FileHandle OUT--)
# <p>Calculate the vapor pressure from the temperature.  The vapor pressure
# that is returned is in millibars.  The function will return <i>undef</i>
# for the vapor pressure if it cannot be calculated.</p>
# @warning The temperature needs to be in &deg;C.
#
# @input $temp The temperature or dew point in &deg;C.
# @input $verbose <b>Optional</b> A flag for printing out error messages.
# Default == 1
# @input $OUT <b>Optional</b> A FileHandle where the errors are to be printed.
# Default == *STDOUT
# @output $vapor_press The vapor pressure value in millibars.
##---------------------------------------------------------------------------
sub calculateVaporPressure {
    if (scalar(@_) < 1 || scalar(@_) > 3) {
	die("Invalid parameters to calculateVaporPressure\n");
    }
    my ($temp,$verbose,$OUT) = @_;
    $verbose = 1 if (!defined($verbose));
    $OUT = *STDOUT if (!defined($OUT));

    # Make sure that the temperature is defined.
    if (!defined($temp)) {
	printf($OUT "calculateVaporPressure:  Temperature not defined.  Vapor Pressure not calculated.\n") if ($verbose);
	return undef();
    }

    # Prevent a divide by zero
    if ($temp == -243.5) {
	printf($OUT "calcluateVaporPressure: Invalid Temperature (%f) will cause a divide by zero.  Vapor Pressure not calculated\n") if ($verbose);
	return undef();
    }
    
    return ($ES0 * exp((17.67 * $temp) / ($temp + 243.5)));
}

##---------------------------------------------------------------------------
# @signature float calculateVirtualTemperature(float temp, float mixr, --int verbose--, --FileHandle OUT--)
# <p>Calculate the virtual temperature from the temperature and mixing ratio.
# The virtual temperature that is returned is in &deg;K.  The function will
# return <i>undef</i> if the virtual temperature cannot be calculated.</p>
# @warning The temperature must be in &deg;K and the mixing ratio in kg/kg.
#
# @input $temp The temperature value in &deg;K.
# @input $mixr The mixing ratio value in kg/kg.
# @input $verbose <b>Optional</b> A flag for prining out error messages.
# Default == 1
# @input $OUT <b>Optional</b> A FileHandle where the errors are to be printed.
# Default == *STDOUT
# @output virt_temp The virtual temperature value in &deg;K.
##---------------------------------------------------------------------------
sub calculateVirtualTemperature {
    if (scalar(@_) < 2 || scalar(@_) > 4) {
	die("Invalid parameters to calculateVirtualTemperature.\n");
    }
    my ($temp,$mixr,$verbose,$OUT) = @_;
    $verbose = $TRUE if (!defined($verbose));
    $OUT = *STDOUT if (!defined($OUT));

    # Make sure the temperature is defined.
    if (!defined($temp)) {
	printf($OUT "calculateVirtualTemperature:  Temperature undefined.  Virtual Temperature not calculated.\n") if ($verbose);
	return undef();
    }

    # Make sure the mixing ratio is defined.
    if (!defined($mixr)) {
	printf($OUT "calculateVirtualTemperature:  Mixing Ratio undefined.  Virtual Temperature not calculated.\n") if ($verbose);
	return undef();
    }

    # Make sure the mixing ratio won't cause a divide by zero error.
    if ($mixr == -1) {
	printf($OUT "calculateVirtualTemperature:  Mixing ratio is -1.  Virtual Temperature not calculated because it will divide by zero.\n") if ($verbose);
	return undef();
    }

    return $temp * ((1.0 + $mixr/$EPS) / (1.0 + $mixr));
}

##---------------------------------------------------------------------------
# @signature (float,float) calculateWindFromUV(float uwind, float vwind, --int verbose--, --FileHandle OUT--)
# <p>Calculate the wind speed and wind direction from the U and V wind components.
# The returned wind speed is in m/s and the wind direction in degrees.  The function
# will return <i>undef</i> values if they cannot be calculated.</p>
# @warning The components neeed to be in m/s.
#
# @input $uwind The U wind component value in m/s.
# @input $vwind The V wind component value in m/s.
# @input $verbose <b>Optional</b> A flag for printing out error messages.
# Default == 1
# @input $OUT <b>Optional</b> A FileHandle where the errors are to be printed.
# Default == *STDOUT
# @output $windspd The wind speed value in m/s.
# @output $winddir The wind direction in degrees.
##---------------------------------------------------------------------------
sub calculateWindFromUV {
    if (scalar(@_) < 2 || scalar(@_) > 4) {
	die("Invalid parameters to calculateWindFromUV\n");
    }
    my ($uwind,$vwind,$verbose,$OUT) = @_;
    $verbose = 1 if (!defined($verbose));
    $OUT = *STDOUT if (!defined($OUT));

    # Make sure that the u component is defined
    if (!defined($uwind)) {
	printf($OUT "calculateWindFromUV:  U Component not defined.  Speed and Direction not calculated.\n") if ($verbose);
	return (undef(),undef());
    }

    # Make sure that the v component is defined
    if (!defined($vwind)) {
	printf($OUT "calculateWindFromUV:  V Component not defined.  Speed and direction not calculated.\n") if ($verbose);
	return (undef(),undef());
    }

    # Special cases for atan
    if ($vwind == 0) {
	if ($uwind == 0) { return (0,0); }
	elsif ($uwind > 0) { return ($uwind,270); }
	else { return (-$uwind,90); }
    }

    my $dir = convertAngle(atan($uwind/$vwind),"rad","deg");
    if ($vwind >= 0) { $dir += 180; }
    if ($dir < 0) { $dir += 360; }

    return (sqrt($uwind**2 + $vwind**2),$dir);
}

1;







