#! /usr/bin/perl -w

##Module----------------------------------------------------------------
# <p>The <code>Conversions.pm</code> module is is a collection of functions 
# that  do conversions to be used with converting QCF data.  It performs
# a number of different calculations and conversions for different
# measurements that are typically seen in data.  It also contains a
# number of constants used by the QCF data format.</p>
#
# <p>There exists a testing suite for the functions in this module.
# They are located in the test_conversions.pl scripts.  The script
# should be rerun after any change has been made to this module.
# Documentation about using the test script is with the test script.</p>
#
# @use  use lib "work/software/conversion_modules/Version1";
#       use Conversions;
# @use  $return_value = Conversions::functionName(params);
#
# @author Joel Clawson
# @version 0.01 Original Version
#
# @author Janine Goldstein
# @version 0.02 Fixed bug in getNextTime function.
##Module----------------------------------------------------------------

package Conversions;
use strict;
use lib ".";
use POSIX;

##----------------------------------------------------------------------------
# @signature float calculateDewPoint(float temperature, float rel_humitity, int verbose)
# <p>Calculates the dew point using temperature and relative humidity.</p>
# <p>The equation used for calculating the dew point is:<pre>
# sat_vapor_pressure = 6.112 * exp((17.67 * temperature) / temperature + 243.5)
# vapor_pressure = sat_vapor_pressure * (rel_humidity / 100.0)
# log_value = log(vapor_pressure / 6.112)
# dew_point = log_value * 243.5 / (17.67 - log_value)
# </pre></p>
#
# @input  $temperature The temperature in degrees Celsius
# @input  $rel_humidity The relative humidity in percent.
# @input  $verbose True value if warnings are to be printed to STDERR
# @output $dew_point Returns the calculated dew point or the missing value if
#         either input value is missing or out of range.
# @warning The temperature must be in &deg;C and the relative humidity must
#          be in % for this function to work correctly.
##----------------------------------------------------------------------------
sub calculateDewPoint {
    my $temperature = shift;
    my $rel_humidity = shift;
    my $verbose = shift;

    # Check to see if the incoming values are valid.
    if ($temperature eq getMissing() || $rel_humidity eq getMissing()) {
	return getMissing();
    } elsif ($temperature > 100.0 || $temperature < -100.0) {
	if ($verbose) {
	    printf STDERR "Invalid temperature %7.2f for dew point calculation.\n", $temperature;
	}
	return getMissing();
    } elsif ($rel_humidity <= .5 || $rel_humidity > 100.0) {
	if ($verbose) {
	    printf STDERR "Invalid relative humidity %7.2f for dew point calculation.\n", $rel_humidity;
	}
	return getMissing();
    } elsif ($rel_humidity == 100.0) { 
        return handleZero($temperature); 
    } else { # Calculate the dew point.
	my $sat_vapor_pressure = 6.112 * exp((17.67 * $temperature) /
					     ($temperature + 243.5));
	my $vapor_pressure = $sat_vapor_pressure * ($rel_humidity / 100.0);
	my $log_value = log($vapor_pressure / 6.112);
	my $dew_point = $log_value * 243.5 / (17.67 - $log_value);
	if ($dew_point < 0.00 && $dew_point > -0.005) {$dew_point = 0.0;}
	return handleZero($dew_point);
    }
}

##--------------------------------------------------------------------------
# @signature float calculateRelativeHumidity(float temperature, float dewpoint, int verbose)
# <p>Calculate the relative humidity from the temperature and dew point.</p>
# <p>The equation used for calculating the relative humidity is:<pre>
# sat_vapor_pressure = 6.112 * exp((17.67 * temperature)/(temperature + 243.5))
# E_sat_vapor_pressure = 6.112 * expt((17.67 * dewpoint) / (dewpoint + 243.5))
# rel_humidity = 100.0 * (E_sat_vapor_pressure / sat_vapor_pressure)
# </pre></p>
#
# @input $temperature The temperature in degrees Celsius.
# @input $dewpoint The dewpoint in degrees Celsius.
# @input $verbose True value if warnings are tho be printed to STDERR.
# @output $rel_humidity Returns the calculated relative humidity value or the
#         missing value if one of the inputs is missing or out of range.
# @warning The temperature and dewpoint must be in &deg;C for this function
# to work correctly.
##--------------------------------------------------------------------------
sub calculateRelativeHumidity {
    my $temperature = shift;
    my $dewpoint = shift;
    my $verbose = shift;
    
    # Ensure the values are valid for the calculation
    if ($temperature eq getMissing() || $dewpoint eq getMissing()) {
	return getMissing();
    } elsif ($temperature > 100.0 || $temperature < -100.0) {
	if ($verbose) {
	    printf STDERR "Invalid temperature %7.2f for relative humidity calculation.\n", $temperature;
	}
	return getMissing();
    } elsif ($dewpoint > 100.0 || $dewpoint < -100.0) {
	if ($verbose) {
	    printf STDERR "Invalid dewpoint %7.2f for relative humidity calculation.\n", $dewpoint;
	}
	return getMissing();
    } elsif ($dewpoint > $temperature) {
	if ($verbose) {
	    printf STDERR "The dewpoint %7.2f cannot be greater than the temperature %7.2f for the relative humidity calculation.", $dewpoint, $temperature;
	}
	return getMissing();
    } else { # Calculate the relative humidity.
	my $sat_vapor_pressure = 6.112 * exp((17.67 * $temperature) /
					     ($temperature + 243.5));
	my $E_sat_vapor_pressure = 6.112 * exp((17.67 * $dewpoint) /
					       ($dewpoint + 243.5));
	my $rel_humidity = 100.0 * ($E_sat_vapor_pressure/$sat_vapor_pressure);
	if ($rel_humidity < 0.00 && $rel_humidity > -0.005) {
	    $rel_humidity = 0.0;
	}
	return handleZero($rel_humidity);
    }
}

##----------------------------------------------------------------
# @signature float calculateSeaLevelPressure(float pressure, float elevation, float dewpoint, float temperature, int verbose)
# <p>Calculate the sea level pressure using the station pressure, elevation,
# dewpoint, and temperature.</p>
# <p>The equation used for calculating the sea level pressure is:<pre>
# gamma = 6.5
# gravity = 9.80616
# r = 287.04
# kelvin = temperature - 273.15
# vapr = 6.112 * exp ((17.67 * dewpoint) / (dewpoint + 243.5))
# e = vapr * (1.001 + (pressure - 100.0) / 900.0 * 0.0034)
# mixr = 0.62197 * (e / (pressure - e)) * 1000.0
# virt_temp = kelvin * ((1.0 + (0.001 * mixr) / 0.62197) / (1.0 + 0.001 * mixr))
# deltv = gamma * elevation / 1000.0
# avg_virt_temp = (virt_temp + (virt_temp - deltv)) / 2.0
# slpressure = pressure * exp((gravity * elevation) / (r * avg_virt_temp)))
# </pre></p>
#
# @input  $pressure The station pressure in millibars
# @input  $elevation The elevation of the station in meters
# @input  $dewpoint The dewpoint at the station in degrees Celcius
# @input  $temperature The temperature at the station in degrees Celcius
# @input  $verbose True value if warnings should be printed to STDERR
#
# @output $slpresssure Returns the calculated sea level pressure.
#
# @warning The pressure must be in <code>mb</code>, the elevation in <code>m
# </code>, and the temperature and dewpoint in <code>&deg;C</code>.
##----------------------------------------------------------------
sub calculateSeaLevelPressure {
    my $pressure = shift;
    my $elevation = shift;
    my $dewpoint = shift;
    my $temperature = shift;
    my $verbose = shift;

    # Check to see if the incoming parameters are valid for the calculation
    if ($pressure eq getMissing() || $elevation eq getMissing() ||
	$dewpoint eq getMissing() || $temperature eq getMissing()) {
	return getMissing();
    } elsif ($temperature > 100.0 || $temperature < -100.0) {
	if ($verbose) {
	    printf STDERR "Invalid temperature %7.2f for sea level pressure calculation.\n", $temperature;
	}
	return getMissing();
    } elsif ($pressure < 300.0 || $pressure > 1200.0) {
	if ($verbose) {
	    printf STDERR "Invalid pressure %7.2f for sea level pressure calculation.\n", $pressure;
	}
	return getMissing();
    } elsif ($dewpoint < -100.0 || $dewpoint > 100.0) {
	if ($verbose) {
	    printf STDERR "Invalid dew point %7.2f for sea level pressure calculation.\n", $dewpoint;
	}
	return getMissing();
    } elsif ($elevation < 0) {
	if ($verbose) {
	    printf STDERR "Invalid elevation %7.2f for sea level pressure calculation.\n", $elevation;
	}
	return getMissing();
    }

    # Constants
    my $gamma = 6.5;
    my $gravity = 9.80616;
    my $r = 287.04;

    # Calculated Values
    my $kelvin = convertTemperature($temperature, "C", "K");
    my $vapr = 6.112 * exp ((17.67 * $dewpoint) / ($dewpoint + 243.5));
    my $e = $vapr * (1.001 + ($pressure - 100.0) / 900.0 * 0.0034);
    my $mixr = 0.62197 * ($e / ($pressure - $e)) * 1000.0;
    my $virt_temp = $kelvin * ((1.0 + (0.001 * $mixr) / 0.62197) / 
			       (1.0 + 0.001 * $mixr));
    my $deltv = $gamma * $elevation / 1000.0;
    my $avg_virt_temp = ($virt_temp + ($virt_temp - $deltv)) / 2.0;
    return handleZero($pressure * exp(($gravity * $elevation) / ($r * $avg_virt_temp)));
}

##---------------------------------------------------------------------------
# @signature float calculateSpecificHumidity(float pressure, float dewpoint, int verbose)
# <p>Calculate the specific humidity at a station given its station pressure
# and dewpoint.</p>
# <p>This function will return the QCF missing value if the dewpoint is not
# between -100 and 100 or if the station pressure is not between 100 and 1500.
# </p>
# <p>The equation used to calculate the specific humidity is:<pre>
# e = 6.112 * exp((17.67 * dewpoint)/(243.5 + dewpoint))
# specific_humidity = (0.622 * e)/(pressure - (0.378 * e))
# </pre></p>
#
# @input $pressure The station pressure in mb
# @input $dewpoint The dewpoint in degrees Celsius.
# @input $verbose A 1 of warnings are to be printed to STDERR, 0 otherwise.
# @output $specific_humidity The specific humidity of the station in kg/kg.
# @warning The station pressure must be in mb and the dewpoint in &deg;C for
# the function to work correctly.
##---------------------------------------------------------------------------
sub calculateSpecificHumidity {
    my $pressure = shift;
    my $dewpoint = shift;
    my $verbose = shift;

    # Define verbose.
    if (!defined($verbose)) { $verbose = 1; }

    # Check for invalid values.
    if ($pressure eq getMissing() || $dewpoint eq getMissing()) {
	return getMissing();
    } elsif ($pressure < 100 || $pressure > 1500) {
	if ($verbose) {
	    printf(STDERR "Invalid pressure %f. Unable to calculate specific humidity.\n", $pressure);
	} 
	return getMissing();
    } elsif ($dewpoint < -100 || $dewpoint > 100) {
	if ($verbose) {
	    printf(STDERR "Invalid dewpoint %f. Unable to calculate specific humidity.\n", $dewpoint);
	}
	return getMissing();
    }

    # Calculate the Specific Humidity
    my $e = 6.112 * exp((17.67 * $dewpoint)/(243.5 + $dewpoint));
    my $specific_humidity = (0.622 * $e)/($pressure - (0.378 * $e));

    # Sanity Check
    if ($specific_humidity < 0 || $specific_humidity > .04) {
	if ($verbose) {
	    print(STDERR "Specific humidity is out of range: %f\n",
		  $specific_humidity);
	}
    }
    return $specific_humidity;
}

##---------------------------------------------------------------------------
# @signature float calculateStationPressure(float altimeter, float elevation)
# <p>Calculate the station pressure from the altimeter reading and elevation.
# </p>
# <p>The equation used for calculating the station pressure is:<pre>
# p0 = 1013.25
# a = .0065
# t0 = 288
# n = .190284
# stn_pressure = (((altimeter ** n) - ((((p0 ** n) * a) / t0) * elevation)) ** (1 / n)) + 0.01
# </pre></p>
# 
# @input $altimeter The altimeter reading in mb.
# @input $elevation The elevation of the station in m.
# @output $pressure The pressure at the station in mb.
# @warning The altimeter reading must be in mb and the elevation must be in
# m for the function to work correctly.
##---------------------------------------------------------------------------
sub calculateStationPressure {
    my $altimeter = shift;
    my $elevation = shift;

    if ($altimeter eq getMissing() || $elevation eq getMissing()) {
	return getMissing();
    }
    
    # Assign Constants
    # Std Sea Level Pressure
    my $p0 = 1013.25; 
    # lapse rate in NACA std atmosphere below the isothermal layer (.0065 C/m)
    my $a = .0065;
    # Std Sea Level Temperature (288 degrees A)
    my $t0 = 288;
    # n = aR = .190284, where R is the gas constant for dry air
    my $n = .190284;

    if (($altimeter ** $n) < ((($p0 ** $n) * $a) / $t0)) {
	printf(STDERR "Calculation calculated an imaginary number in calculateStationPressure.  Elevation: %s Altimeter %s\n", $elevation, $altimeter);
	return getMissing();
    }

    # Calculate Station Pressure
    return (((($altimeter ** $n) - (((($p0 ** $n) * $a) / $t0) * $elevation))
	     ** (1 / $n)) + .01);
}

##---------------------------------------------------------------------------
# @signature (float ucomp, float vcomp) calculateUVfromWind(float wind_speed, float wind_dir, int verbose)
# <p>Calculate the U and V vector components from the wind speed and wind
# direction.</p>
# <p>The equation used for calculating the UV components is:<pre>
# ucomp = -1 * sin(PI / 180 * (wind_dir)) * wind_speed
# vcomp = -1 * cos(PI / 180 * (wind_dir)) * wind_speed 
# </pre></p>
#
# @warning The wind speed must be in m/s and the wind direction in degrees.
# @input $wind_speed The wind speed in m/s.
# @input $wind_dir The wind direction in degrees.
# @input $verbose 1 if the function should print warnings to STDERR, 0 otherwise.
# @output $ucomp The U vector component of the wind.
# @output $vcomp The V vector component of the wind.
##----------------------------------------------------------------------------
sub calculateUVfromWind {
    my $wind_speed = shift;
    my $wind_dir = shift;
    my $verbose = shift;

    # Ensure that the wind speed and direction are valid.
    if ($wind_speed < 0 || $wind_dir < 0 || $wind_dir > 360) {
	return (getMissing(), getMissing());
    }

    my $ucomp = -1 * sin(degToRad($wind_dir)) * $wind_speed;
    my $vcomp = -1 * cos(degToRad($wind_dir)) * $wind_speed;

    # Get rid of negative zeros when rounded.
    if ($ucomp > -.005 && $ucomp < .005) { $ucomp = 0; }
    if ($vcomp > -.005 && $vcomp < .005) { $vcomp = 0; }

    if ($verbose && ($ucomp < -200 || $ucomp > 200)) {
	printf STDERR "WARNING: Calculated U wind component is out of expected range: %f\n", $ucomp;
    }
    if ($verbose && ($vcomp < -200 || $vcomp > 200)) {
	printf STDERR "WARNING: Calculated V wind component is out of expected range: %f\n", $vcomp;
    }

    return ($ucomp, $vcomp);
}

##---------------------------------------------------------------------------
# @signature (float wind_speed, float wind_dir) calculateWindFromUV(float ucomp, float vcomp)
# <p>Calculate the wind speed and wind direction from U and V vector components.</p>
# <p>The equation used for calculating the wind speed and direction is:<pre>
# if (vcomp == 0) {
#   if (ucomp == 0) { return (0, 0); }
#   elsif (ucomp > 0) { return (ucomp, 270); }
#   else { return ((-1 * ucomp), 90); }
# }
# wind_speed = sqrt(ucomp ** 2 + vcomp ** 2);
# wind_dir = (atan(ucomp / vcomp)) * 180 / PI;
# if (vcomp >= 0) { wind_dir += 180; }
# </pre></p>
#
# @warning The U and V components must be in m/s.
# @input $ucomp The U vector component in m/s.
# @input $vcomp The V vector component in m/s.
# @output $wind_speed The wind speed in m/s.
# @output $wind_dir The wind direction in degrees.
##---------------------------------------------------------------------------
sub calculateWindFromUV {
    my $ucomp = shift;
    my $vcomp = shift;

    # Special Case to prevent a divide by zero later.
    if ($vcomp == 0) {
	if ($ucomp == 0) { return (0, 0); }
	elsif ($ucomp > 0) { return ($ucomp, 270); }
	else { return ((-1 * $ucomp), 90); }
    }

    # Calculate wind direction and wind speed
    my $wind_speed = sqrt($ucomp ** 2 + $vcomp ** 2);
    my $wind_dir = radToDeg(atan($ucomp / $vcomp));
    if ($vcomp >= 0) { $wind_dir += 180; } # Gets to other half of unit circle
    
    # Force wind direction to be between 0 and 360
    if ($wind_dir < 0) { $wind_dir += 360; }

    return ($wind_speed, $wind_dir);
}

##----------------------------------------------------------------
# @signature int compareDates(String date1, String date2)
# Compare two dates to see if the first date occurred before, is the same day
# as, or after the second date.
#
# @input  $date1 The first date to compare
# @input  $date2 The second date to compare
#
# @output $compare Returns -1, 0, or 1 if $date1 is less than, equal to
#            or greater than $date2.
##----------------------------------------------------------------
sub compareDates {
    my $date1 = shift;
    my $date2 = shift;
    
    # Change the string date to a number date to standard comparison
    # operators on.
    $date1 =~ s/\///g; # Change format YYYY/MM/DD to YYYYMMDD
    $date2 =~ s/\///g; # Change format YYYY/MM/DD to YYYYMMDD
    
    if ($date1 < $date2) {
	return -1;
    } elsif ($date1 == $date2) {
	return 0;
    } else {
	return 1;
    }
}

##--------------------------------------------------------------------------
# @signature float convertArea(float area, String inUnits, String outUnits)
# <p>Convert an area from one unit of area to another.</p><p>This function
# converts every input area to m<sup>2</sup> and then converts it to the
# output units.</p>
#
# @input $area The area value to convert.
# @input $inUnits The input units of the area.
# @input $outUnits The units to convert the area to.
# @output $value The converted area to the output units.
# @warning This function will <code>die</code> if it is given an unknown
# area unit.
# @limitation The conversion only recognizes the units:<ul>
# <li>acre = acres</li>
# <li>cm2 = cm<sup>2</sup> = square centimeters</li>
# <li>ft2 = ft<sup>2</sup> = square feet</li>
# <li>in2 = in<sup>2</sup> = square inches</li>
# <li>km2 = km<sup>2</sup> = square kilometers</li>
# <li>m2 = m<sup>2</sup> = square meters</li>
# <li>mile2 = mile<sup>2</sup> = square miles</li>
# <li>mm2 = mm<sup>2</sup> = square millimeters</li>
# <li>yd2 = yd<sup>2</sup> = square yards</li></ul>
##--------------------------------------------------------------------------
sub convertArea {
    my $area = shift;
    my $inUnits = shift;
    my $outUnits = shift;
    my $value;

    # Convert the area to square meters
    if ($inUnits eq $outUnits || $area eq Conversions::getMissing()) {
	return handleZero($area);
    } elsif ($inUnits eq "m2") {
	$value = $area;
    } elsif ($inUnits eq "ft2") {
	$value = $area * convertLength(1, "ft", "m") ** 2;
    } elsif ($inUnits eq "in2") {
	$value = $area * convertLength(1, "in", "m") ** 2;
    } elsif ($inUnits eq "cm2") {
	$value = $area * convertLength(1, "cm", "m") ** 2;
    } elsif ($inUnits eq "km2") {
	$value = $area * convertLength(1, "km", "m") ** 2;
    } elsif ($inUnits eq "mile2") {
	$value = $area * convertLength(1, "mile", "m") ** 2;
    } elsif ($inUnits eq "mm2") {
	$value = $area * convertLength(1, "mm", "m") ** 2;
    } elsif ($inUnits eq "yd2") {
	$value = $area * convertLength(1, "yd", "m") ** 2;
    } elsif ($inUnits eq "acre") {
	$value = ($area / 640) * convertLength(1, "mile", "m") ** 2;
    } else {
	die("Unknown area units of $inUnits for input units.\n");
    }

    # Convert the square meters to output units
    if ($outUnits eq "cm2") {
	$value = $value * convertLength(1, "m", "cm") ** 2;
    } elsif ($outUnits eq "ft2") {
	$value = $value * convertLength(1, "m", "ft") ** 2;
    } elsif ($outUnits eq "in2") {
	$value = $value * convertLength(1, "m", "in") ** 2;
    } elsif ($outUnits eq "km2") {
	$value *= convertLength(1, "m", "km") ** 2;
    } elsif ($outUnits eq "m2") {
	# Already in square meters.
    } elsif ($outUnits eq "mile2") {
	$value *= convertLength(1, "m", "mile") ** 2;
    } elsif ($outUnits eq "mm2") {
	$value *= convertLength(1, "m", "mm") ** 2;
    } elsif ($outUnits eq "yd2") {
	$value *= convertLength(1, "m", "yd") ** 2;
    } elsif ($outUnits eq "acre") {
	$value = ($value * 640) * convertLength(1, "m", "mile") ** 2;
    } else {
	die("Unknown area unit $outUnits for output units.\n");
    }
    return handleZero($value);
}

##---------------------------------------------------------------
# @signature String convertDate(String date, String date_format)
# <p>Convert a date from a specified format to the YYYY/MM/DD format.
# The conversion recognizes Y for year, M for month, D for day,
# and J for julian days.</p>
#
# @input  $date The date to be converted.
# @input  $date_format The format of $date
# @output $new_date Returns $date in YYYY/MM/DD format.
# @warning The function will <code>die</code> if the number of characters in
# the date and the number of characters in the date format are not the same.
# @warning The function will <code>die</code> if the year is not given in
# a four digit format.
##---------------------------------------------------------------
sub convertDate {
    my $date = shift;
    my $date_format = shift;
    my $year = "";
    my $month = "";
    my $day = "";
    my $julian = "";

    my $count = 0;
    if (length($date) != length($date_format)) {
	die "Length of $date and $date_format do not match in Conversions::convertDate\n";
    }

    # Loop through all of the chars in the date
    while ($count < length($date)) { 
	my $char = substr(uc($date_format), $count, 1);
	my $date_char = substr($date, $count, 1);

	if ($char eq "Y") { # Look for year characters
	    $year .= $date_char;
	} elsif ($char eq "M") { # Look for month characters
	    $month .= $date_char;
	} elsif ($char eq "D") { # Look for day characters
	    $day .= $date_char;
	} elsif ($char eq "J") { # Look for julian day characters
	    $julian .= $date_char;
	}
	$count++;
    }
    
    if (length($year) < 4) { # Check for short digit year
	die "Year: $year needs to have four digits.\n";
    }

    if ($julian ne "") { # Day was given in Julian
	($month, $day) = convertJulian($year, $julian);
    }

    # Format date to "YYYY/MM/DD"
    return sprintf("%04d/%02d/%02d", $year, $month, $day);
}

##---------------------------------------------------------------------------
# @signature float convertFlow(float flow, String in, String out)
# <p>Convert a flow value from one unit of flow to another.</p>
# 
# @input $flow The flow value to be converted.
# @input $in The input units of the flow value.
# @input $out The output units to convert the flow value to.
# @output $value The converted flow in the output units.
# @warning This function will <code>die</code> if an unknown flow unit is
# passed in.
# @limitation This function recognizes the flow units of:<ul>
# <li>"ft3/s" cubic feet per second</li>
# <li>"m3/s" cubic meters per second</li></ul>
##---------------------------------------------------------------------------
sub convertFlow {
    my $flow = shift;
    my $in = shift;
    my $out = shift;
    my $value;

    # Convert to m3/s
    if ($in eq $out || $flow eq getMissing()) {
	return $flow;
    } elsif ($in eq "ft3/s") {
	$value = $flow * convertVolume(1, "ft3", "m3");
    } elsif ($in eq "m3/s") {
	$value = $flow;
    } else {
	die("Unknown flow units of $in for input measurement.\n");
    }

    # Convert to output units
    if ($out eq "ft3/s") {
	$value = $value * convertVolume(1, "m3", "ft3");
    } elsif ($out eq "m3/s") {
	# Already in m3/s
    } else {
	die("Unknown flow units of $out for output measurement.\n");
    }

    return $value;
}

##------------------------------------------------------------------
# @signature (int month, int day) convertJulian(int year, int julian)
# Convert a Julian day to a standard month and day.
# 
# @input  $year The year of the day to be converted.
# @input  $julian The Julian day value.
#
# @output $month The month specified by the julian day.
# @output $day The day specified by the julian day.
##------------------------------------------------------------------
##------------------------------------------------------------------
# @signature int convertJulian(int year, int day, int month)
# Convert a standard month and day to a julian day for a given year.
#
# @input $year The year of the day to be converted.
# @input $day The day of the month
# @input $month The month to be converted.
# @output $julian The julian day of the specified day and month.
##------------------------------------------------------------------
sub convertJulian {
    my $year = shift;
    my $julian = shift;
    my $month = shift;
    my @days_per_month = (31, days_in_feb($year), 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
    
    if (!defined($month)) {
	$month = 1;
	$julian += 0; # This is for $julian to be recognized as a number

	foreach my $mnth (@days_per_month) {
	    if ($julian > $mnth) {
		$month++;
		$julian = $julian - $mnth;
	    } else {
		last;
	    }
	}
	return ($month, $julian);
    } else {
	$month--; # Get to zero count for months
	while ($month > 0) {
	    if (defined($days_per_month[$month - 1])) {
		$julian += $days_per_month[$month - 1];
	    }
	    $month--;
	}
	return $julian;
    }
}

##--------------------------------------------------------------------
# @signature String convertLatLong(String value, String format)
# <p>Convert a latitude or longitude value from a DMS format to just
# degrees.  The conversion only recognizes D for degrees, M for minutes
# and S for seconds.  The "-" character is used for negativity of the 
# value and should not be included as part of the DMS in the format.  It
# should not be used in the format except for setting negativity.  The 
# "." character should be included as part of DMS in the formatting.  If
# it is put into the format, it will be ignored.</p>
#
# @input  $value The latitude or longitude value.
# @input  $format The format of $value using DMS characters.
#
# @output $converted Returns $value in degree format.
#
# @limitiation This only converts from DMS to degrees and not the other
# way.
##---------------------------------------------------------------------
sub convertLatLong {
    my $value = shift;
    my $format = shift;
    my $out_format = shift;

    if (!defined($out_format)) { $out_format = "D"; }
    
    my $multiplier = 1;
    my $degree = "";
    my $minute = "";
    my $second ="";

    my $count = 0;

    if (length($value) != length($format)) {
	die "Length of $value is not the same as the length in $format in Conversions::convertLatLong.\n";
    }

    # Loop through all of the characters in the format
    while ($count < length($format)) {
	my $char = substr($format, $count, 1);
	my $val_char = substr($value, $count, 1);
	
	if ($char eq "D") { 
	    $degree .= $val_char; 
	} elsif ($char eq "M") { 
	    $minute .= $val_char; 
	} elsif ($char eq "S") { 
	    $second .= $val_char;
	} elsif ($char eq "-") {
	    $multiplier = -1;
	}
	$count++;
    }

    # Must have a value for degrees
    if ($degree eq "") { 
	$degree = 0; 
    }
    
    if ($minute ne "") {
	if ($second ne "") {
	    $minute += $second / 60; # 60 seconds in a minute
	}
	$degree += $minute / 60; # 60 minutes in a degree
    }

    if ($out_format eq "D") {
	return $multiplier * $degree;
    } elsif ($out_format eq "DMS") {
	my $deg = int($degree);
	my $min = int(($degree - $deg) * 60);
	my $sec = ($degree - $deg - ($min / 60)) * 3600;
	return ($deg, $min, $sec);
    }
}

##--------------------------------------------------------------------
# @signature float convertLength(float len, String in, String out)
# <p>Convert a length from one unit of measurement to another.</p><p>The
# function converts every input units to meters then to the output units.</p>
# @limitation The conversion currently only recognizes the units of:<ul>
#    <li>"cm"      centimeters</li>
#    <li>"dm"      decimeters</li>
#    <li>"ft"      feet</li>
#    <li>"hi"      hundredths of inches</li>
#    <li>"in"      inches</li>
#    <li>"km"      kilometers</li>
#    <li>"m"       meters</li>
#    <li>"mile"    miles</li>
#    <li>"mm"      millimeters</li>
#    <li>"nmile"   nautical miles</li>
#    <li>"st-mile" statute miles</li>
#    <li>"ti"     tenths of inches</li>
#    <li>"yd"      yards</li></ul>
#
# @input  $len The length to be converted.
# @input  $in The unit of length of the initial length
# @input  $out The desired unit of length
# @output $val Return the value of $len in the desired unit of length.
# @warning The function will <code>die</code> if it receives an unknown
# unit of measurement in either the <code>$in</code> or <code>$out</code>
# input parameters.
##--------------------------------------------------------------------
sub convertLength {
    my $len = shift;
    my $in = shift;
    my $out = shift;
    my $val;

    # Convert $len into meters from $in
    if ($in eq $out || $len eq Conversions::getMissing()) {
	return handleZero($len);
    } elsif ($in eq "mm") {
	$val = $len / 1000;
    } elsif ($in eq "cm") {
	$val = $len / 100;
    } elsif ($in eq "dm") {
	$val = $len / 10;
    } elsif ($in eq "m") {
	$val = $len;
    } elsif ($in eq "hi") {
	$val = $len * .000254;
    } elsif ($in eq "ti") {
	$val = $len * .00254;
    } elsif ($in eq "in") {
	$val = $len * .0254;
    } elsif ($in eq "ft") {
	$val = $len * .3048;
    } elsif ($in eq "yd") {
	$val = $len * .9144;
    } elsif ($in eq "km") {
	$val = $len * 1000;
    } elsif ($in eq "mile" || $in eq "st-mile") {
	$val = $len * 1609.344;
    } elsif ($in eq "nmile") {
	$val = $len * 1852;
    } else {
	die "Unknown length of unit $in for input measurement.\n";
    }

    # Convert from meters to $out
    if ($out eq "mm") {
	$val *= 1000;
    } elsif ($out eq "cm") {
	$val *= 100;
    } elsif ($out eq "dm") {
	$val *= 10;
    } elsif ($out eq "m") {
	# $val is already in meters
    } elsif ($out eq "hi") {
	$val *= (10000 / 2.54);
    } elsif ($out eq "ti") {
	$val *= (1000 / 2.54);
    } elsif ($out eq "in") {
	$val *= (100 / 2.54);
    } elsif ($out eq "ft") {
	$val = $val / .3048;
    } elsif ($out eq "yd") {
	$val = $val / .9144;
    } elsif ($out eq "km") {
	$val /= 1000;
    } elsif ($out eq "mile" || $out eq "st-mile") {
	$val /= 1609.344;
    } elsif ($out eq "nmile") {
	$val /= 1852;
    } else {
	die "Unknown length unit of $out for output measurement.\n";
    }

    return handleZero($val);
}

##-----------------------------------------------------------------
# @signature float convertPressure(float press, String in, String out)
# <p>Convert a pressure from one unit of measurement to another.</p><p>The
# function converts all input units to bars then converts it to the output
# units.</p>
# @limitation Currently only recognizes the units of:<ul>
#    <li>"atm"        atmospheres</li>
#    <li>"bar"        bars</li>
#    <li>"dyne/cm2"   dynes per square centimeter</li>
#    <li>"hPa"        hectopascals</li>
#    <li>"inHg"       inches of mercury</li>
#    <li>"kPa"        kilopascals</li>
#    <li>"mb"         millibars</li>
#    <li>"mbar"       millibars</li>
#    <li>"mmHg"       millimeters of mercury</li>
#    <li>"Pa"         pascals (N/m^2)</li>
# @input  $press The value of pressure to be converted.
# @input  $in The unit of pressure of the input value.
# @input  $out The unit of pressure to be outputed.
# @output $val Return the value of $press in the units of $out.
# @warning The function will <code>die</code> if it receives an unknown
# unit of measurement in either the <code>$in</code> or <code>$out</code>
# input parameters.
##-----------------------------------------------------------------
sub convertPressure {
    my $press = shift;
    my $in = shift;
    my $out = shift;
    my $val;

    # Convert $in to bar
    if ($in eq $out || $press eq Conversions::getMissing()) {
	return handleZero($press);
    } elsif ($in eq "atm") {
	$val = $press * 1.01325;
    } elsif ($in eq "inHg") {
	$val = $press * ((1.01325 * 25.4) / 760);
    } elsif ($in eq "mmHg") {
	$val = $press * 1.01325 / 760;
    } elsif ($in eq "Pa") {
	$val = $press * 1.01325 / 101325;
    } elsif ($in eq "kPa") {
	$val = $press * 1.01325 / 101.325;
    } elsif ($in eq "hPa") {
	$val = $press * 1.01325 / 1013.25
    } elsif ($in eq "mbar" || $in eq "mb") {
	$val = $press / 1000;
    } elsif ($in eq "dyne/cm2") {
	$val = $press / 1000000;
    } elsif ($in eq "bar") {
	$val = $press;
    } else {
	die "Unknown pressure unit of $in for input measurement.\n";
    }

    # Convert bar to $out units
    if ($out eq "atm") {
	$val = $val / 1.01325;
    } elsif ($out eq "inHg") {
	$val *= (760 / (1.01325 * 25.4));
    } elsif ($out eq "mmHg") {
	$val *= (760 / 1.01325);
    } elsif ($out eq "Pa") {
	$val *= (101325 / 1.01325);
    } elsif ($out eq "kPa") {
	$val *= (101.325 / 1.01325);
    } elsif ($out eq "hPa") {
	$val *= (1013.25 / 1.01325);
    } elsif ($out eq "mbar" || $out eq "mb") {
	$val *= 1000;
    } elsif ($out eq "dyne/cm2") {
	$val *= 1000000;
    } elsif ($out eq "bar") {
	# $val already in bar units.
    } else {
	die "Unknown pressure unit of $out for output measurement.\n";
    }

    return handleZero($val);
}

##---------------------------------------------------------------------------
# @signature float convertRadiation(float radiation, String in, String out)
# <p>Convert a radiation value from one unit of radiation to another.</p>
#
# @input $radiation The radiation value to be converted.
# @input $in The input units of the radiation.
# @input $out The units to convert the radiation value to.
# @output $value The converted radiation value in the output units.
# @warning This function will <code>die</code> if an unknown radiation unit
# is passed in.
# @limitation This function recognizes the radiation units of:<ul>
# <li>"langly"</li>
# <li>"w/m2"</li></ul>
##---------------------------------------------------------------------------
sub convertRadiation {
    my $radiation = shift;
    my $in = shift;
    my $out = shift;
    my $value;

    # Convert to w/m2
    if ($in eq $out || $radiation eq getMissing()) {
	return $radiation;
    } elsif ($in eq "langly") {
	$value = $radiation * 10;
    } elsif ($in eq "w/m2") {
	$value = $radiation;
    } else {
	die("Unknown radiation units of $in for input measurement.\n");
    }

    # Convert to output units
    if ($out eq "langly") {
	$value /= 10;
    } elsif ($out eq "w/m2") {
	# Don't do anything already in w/m2
    } else {
	die("Unknown radiation units of $out for output measurement.\n");
    }

    return $value;
}

##----------------------------------------------------------------
# @signature float convertTemperature(float temp, String in, String out)
# <p>Convert a temperature from one unit of measurement to another.</p><p>The
# function converts all input temperatures to C then converts it to the
# output unit of measurement.</p>
#
# @limitation The conversions recoginzes the units of:<ul>
#     <li>"C"      degrees Celsius</li>
#     <li>"F"      degress Fahrenheit</li>
#     <li>"K"      Kelvin</li></ul>
# @input  $temp The temperature to be converted.
# @input  $in The units of measurement of $temp.
# @input  $out The units of measurement to covert $temp to.
# @output $val Return $temp converted to $out units
# @warning The function will <code>die</code> if it receives an unknown
# unit of measurement in either the <code>$in</code> or <code>$out</code>
# input parameters.
##----------------------------------------------------------------
sub convertTemperature {
    my $temp = shift;
    my $in = shift;
    my $out = shift;
    my $val;
    
    if ($in eq $out || $temp eq Conversions::getMissing()) {
	return handleZero($temp);
    }
    
    # Convert $in to C
    if ($in eq "F") {
	$val = ($temp - 32.0) * (5 / 9);
    } elsif ($in eq "K") {
	$val = $temp - 273.15;
    } elsif ($in eq "C") {
	$val = $temp;
    } else {
	die "Unknown temp unit of $in for input measurement.\n";
    }
    
    # Convert C to $out
    if ($out eq "K") {
	$val += 273.15;
    } elsif ($out eq "F") {
	$val = ((9 / 5) * $val) + 32.0;
    } elsif ($out eq "C") {
	# $val already in celcius
    } else {
	die "Unknow temp unit of $out for output measurement.\n";
    }
    
    return handleZero($val);
}

##----------------------------------------------------------------
# @signature String convertTime(String time, String time_format)
# Convert a time in a specified format to the format "HH:MM".
# The conversion expects H for hours and M for minutes.
# 
# @input  $time The time to be converted.
# @input  $time_format The format of $time.
# @output $new_time Returns $time in "HH:MM" format.
# @warning The function will <code>die</code> if the number of characters in
# the time are not the same as the number of characters in the format.
##----------------------------------------------------------------
sub convertTime {
    my $time = shift;
    my $time_format = shift;
    my $hour = "";
    my $min = "";

    my $count = 0;

    if (length($time) ne length($time_format)) {
	die "Length of $time is not the same as the length of $time_format in Conversions::convertTime\n";
    }

    # Loop through the characters in the time
    while ($count < length($time)) {
	my $time_char = substr(uc($time), $count, 1);
	my $char = substr($time_format, $count, 1);
	if ($char eq "H") {
	    $hour .= $time_char;
	} elsif ($char eq "M") {
	    $min .= $time_char;
	}
	$count++;
    }

    if ($hour eq "") { $hour = 0; }
    if ($min eq "") { $min = 0; }

    # Format the time to HH:MM
    return sprintf("%02d:%02d", $hour, $min);
}

##---------------------------------------------------------------------------
# @signature float convertTimePeriod(float time String inUnits, String outUnits)
# <p>Convert a time period from one unit of time to another.</p><p>This 
# function converts all input units to hr and then converts it to the 
# specified output units.</p>
#
# @input $time The time value to be converted.
# @input $inUnits The input units of the time value.
# @input $outUnits The output units to convert the time value to.
# @output $value The time value converted to the output units.
# @warning This function will <code>die</code> if an unknown unit of time 
# is given.
# @limitation The conversion currently recognizes the units of:
# <ul><li>"day" days</li><li>"hr" hours</li><li>"min" minutes</li>
# <li>"s" seconds</li><li>"sec" seconds</li></ul>
##----------------------------------------------------------------------------
sub convertTimePeriod {
    my $time = shift;
    my $inUnits = shift;
    my $outUnits = shift;
    my $value;

    # Convert input units to hr
    if ($inUnits eq $outUnits || $time eq Conversions::getMissing()) {
	return handleZero($time);
    } elsif ($inUnits eq "sec" || $inUnits eq "s") {
	$value = $time / 3600; # 3600 seconds in an hour
    } elsif ($inUnits eq "min") {
	$value = $time / 60; # 60 minutes in an hour
    } elsif ($inUnits eq "hr") {
	$value = $time;
    } elsif ($inUnits eq "day") {
	$value = $time * 24; # 24 hours in a day.
    } else {
	die("Unknown time units $inUnits for input units.\n");
    }
    
    # $value is now in hr so convert it to output units
    if ($outUnits eq "sec" || $outUnits eq "s") {
	$value = $value * 3600;
    } elsif ($outUnits eq "min") {
	$value = $value * 60;
    } elsif ($outUnits eq "hr") {
	$value = $value;
    } elsif ($outUnits eq "day") {
	$value = $value / 24;
    } else {
	die("Unknown time units $outUnits for output units.\n");
    }
    return handleZero($value);
}

##-----------------------------------------------------------------
# @signature float convertVelocity(float velocity, String in, String out)
# <p>Convert a velocity from one unit of velocity to another.</p><p>This
# function converts all input units to m/s and then converts it to the
# specified output units.</p>
#
# @limitation The conversion currently recognizes the units of:<ul>
#     <li>"ft/s"      feet per second</li>
#     <li>"km/hr"     kilometers per hour</li>
#     <li>"knot"      nautical mile per hour</li>
#     <li>"m/s"       meters per second</li>
#     <li>"mi/hr"     miles per hour</li>
#     <li>"mph"       miles per hour</li></ul>
# @input  $velocity The velocity value to be converted.
# @input  $in The unit of measurement of $velocity
# @input  $out The unit of measurement to be outputed.
# @output $val Returns $velocity converted to $out units.
# @warning The function will <code>die</code> if it receives an unknown
# unit of measurement in either the <code>$in</code> or <code>$out</code>
# input parameters.
##----------------------------------------------------------------
sub convertVelocity {
    my $velocity = shift;
    my $in = shift;
    my $out = shift;
    my $val;

    #Convert $in to m/s
    if ($in eq $out || $velocity eq Conversions::getMissing()) {
	return handleZero($velocity);
    } elsif ($in eq "mph" || $in eq "mi/hr") {
	$val = $velocity * (1609.344 / 3600);
    } elsif ($in eq "m/s") {
	$val = $velocity;
    } elsif ($in eq "km/hr") {
	$val = $velocity / 3.6;
    } elsif ($in eq "ft/s") {
	$val = $velocity * .3048;
    } elsif ($in eq "knot") {
	$val = $velocity * (463/900);
    } else {
	die "Unknown unit of velocity $in for input measurement.\n";
    }
    
    #Convert m/s to $out
    if ($out eq "mph" || $out eq "mi/hr") {
	$val *= (3600 / 1609.344);
    } elsif ($out eq "m/s") {
	# $val is in m/s
    } elsif ($out eq "km/hr") {
	$val *= 3.6;
    } elsif ($out eq "ft/s") {
	$val /= .3048;
    } elsif ($out eq "knot") {
	$val *= (900 / 463);
    } else {
	die "Unknown unit of velocity $out for output measurement.\n";
    }

    return handleZero($val);
}

##--------------------------------------------------------------------------
# @signature float convertVolume(float volume, String in, String out)
# <p>Convert a volume from one unit to another.</p><p>This function converts
# every volume to cm<sup>3</sup> and then converts it to the output units.</p>
#
# @input $volume The volume to be converted.
# @input $in The input units of the volume.
# @input $out The output units to convert the volume to.
# @output $value The converted volume in the output units.
# @warning The function with <code>die</code> if a given unit is not known.
# @limitation The conversion only recognizes the units:<ul>
# <li>af = acre-feet</li>
# <li>cm3 = cm<sup>3</sup> = cubic centimeters</li>
# <li>ft3 = ft<sup>3</sup> = cubic feet</li>
# <li>gal = gallons</li>
# <li>in3 = in<sup>3</sup> = cubic inches</li>
# <li>liter = liters</li>
# <li>m3 = m<sup>3</sup> = cubic meters</li>
# <li>ml = milliliters</li>
# <li>oz = ounces</li>
# <li>pt = pints</li>
# <li>qt = quarts</li></ul>
##----------------------------------------------------------------------------
sub convertVolume {
    my $volume = shift;
    my $in = shift;
    my $out = shift;
    my $value = shift;

    # Convert the volume to cubic centimeters
    if ($in eq $out || $volume eq Conversions::getMissing()) {
	return handleZero($volume);
    } elsif ($in eq "cm3" || $in eq "ml") {
	$value = $volume;
    } elsif ($in eq "liter") {
	$value = $volume * 1000;
    } elsif ($in eq "gal") {
	$value = $volume * (34628947 / 9148);
    } elsif ($in eq "qt") {
	$value = $volume * (34628947 / 36592);
    } elsif ($in eq "pt") {
	$value = $volume * (34628947 / 73184);
    } elsif ($in eq "oz") {
	$value = $volume * (34628947 / 1170944);
    } elsif ($in eq "in3") {
	$value = $volume * convertLength(1, "in", "cm") ** 3;
    } elsif ($in eq "ft3") {
	$value = $volume * convertLength(1, "ft", "cm") ** 3;
    } elsif ($in eq "m3") {
	$value = $volume * convertLength(1, "m", "cm") ** 3;
    } elsif ($in eq "af") {
	$value = $volume * 43560 * convertLength(1, "ft", "cm") ** 3;
    } else {
	die("Unknown volume units $in for input units.\n");
    }

    # Convert from cubic centimeters to output units.
    if ($out eq "cm3" || $out eq "ml") {
	# Already in cm3 and ml
    } elsif ($out eq "liter") {
	$value = $value / 1000;
    } elsif ($out eq "gal") {
	$value *= (9148 / 34628947);
    } elsif ($out eq "qt") {
	$value *= (36592 / 34628947);
    } elsif ($out eq "pt") {
	$value *= (73184 / 34628947);
    } elsif ($out eq "oz") {
	$value *= (1170944 / 34628947);
    } elsif ($out eq "in3") {
	$value *= convertLength(1, "cm", "in") ** 3;
    } elsif ($out eq "ft3") {
	$value *= convertLength(1, "cm", "ft") ** 3;
    } elsif ($out eq "m3") {
	$value *= convertLength(1, "cm", "m") ** 3;
    } elsif ($out eq "af") {
	$value *= convertLength(1, "cm", "ft") ** 3 / 43560;
    } else {
	die("Unknown volume units $out for output units.\n");
    }
    return handleZero($value);
}

##------------------------------------------------------------------
# @signature int days_in_feb(int year)
# Find the number of days in February for a given year.
#
# @input  $year The current year to find the day in Feb for.
# @output $days Returns the number of days in February.
##------------------------------------------------------------------
sub days_in_feb {
    my $year = shift;
    if ((($year % 4 == 0) && ($year % 100 != 0)) || ($year % 400 == 0)) {
	return 29;
    } else {
	return 28;
    }
}

##--------------------------------------------------------------------------
# @signature float degToRad(float deg)
# <p>Convert a direction in degrees to radians.</p>
#
# @input $deg The number of degrees to convert to radians.
# @output $rad The degrees converted to radians.
##--------------------------------------------------------------------------
sub degToRad {
    my $deg = shift;
    return $deg * pi() / 180;
}

##-----------------------------------------------------------------
# @signature String getBadFlag()
#
# Get the QCF flag for a bad value: 'B'
#
# @warning This should only be used if the raw data is marked as bad and
# then only if it has been cleared through the proper channels.
# @output $flag Returns the flag for a bad value.
##-----------------------------------------------------------------
sub getBadFlag { return 'B'; }

##--------------------------------------------------------------------------
# @signature (String date, String time, int sec) getDateFromSeconds(int seconds)
# <p>Get the date, time and seconds from the number of seconds after January
# 1st, 1970 in UTC time.</p>
# <p>This function returns seconds and can be used as a check because 0 seconds
# is going to be a false value in perl and all other values will be true.</p>
#
# @input $seconds The number of seconds after 1970/01/01.
# @output $date The date in YYYY/MM/DD format.
# @output $time The time in HH:MM format.
# @output $sec The seconds in SS format.
##--------------------------------------------------------------------------
sub getDateFromSeconds {
    my $seconds = shift;

    # Convert seconds after 1970/01/01 to a standard date and time.
    (my $day_of_week, my $month, my $day, my $hour, my $min, my $sec, my $year)
	= split(' ', strftime("%a %b %e %H %M %S %Y", gmtime($seconds)));
    
    # Convert month abbrev to number
    if ($month =~ /Jan/) { $month = 1; }
    elsif ($month =~ /Feb/) { $month = 2; }
    elsif ($month =~ /Mar/) { $month = 3; }
    elsif ($month =~ /Apr/) { $month = 4; }
    elsif ($month =~ /May/) { $month = 5; }
    elsif ($month =~ /Jun/) { $month = 6; }
    elsif ($month =~ /Jul/) { $month = 7; }
    elsif ($month =~ /Aug/) { $month = 8; }
    elsif ($month =~ /Sep/) { $month = 9; }
    elsif ($month =~ /Oct/) { $month = 10; }
    elsif ($month =~ /Nov/) { $month = 11; }
    elsif ($month =~ /Dec/) { $month = 12; }

    # Format and return date, time, sec
    return (sprintf("%04d/%02d/%02d", $year, $month, $day),
	    sprintf("%02d:%02d", $hour, $min), sprintf("%02d", $sec));
}

##-----------------------------------------------------------------
# @signature String getDubiousFlag()
# Get the QCF flag for a dubious/questionable value: 'D'
#
# @warning This should only be used if the raw data is marked as 
# dubious/questionable and then only if it has been cleared through the 
# proper channels.
# @output $flag Returns the flag for a dubious value.
##-----------------------------------------------------------------
sub getDubiousFlag { return 'D'; }

##-----------------------------------------------------------------
# @signature String getEstimateFlag()
# Get the QCF flag for an estimated value: 'E'
# 
# @output $flag Returns the flag for an estimated value.
# @warning This flag should only be used when the raw data specifies that
# the measurement is estimated.
##----------------------------------------------------------------
sub getEstimateFlag { return 'E'; }

##----------------------------------------------------------------
# @signature String getGlitchFlag()
# Get the QCF flag for a glitch value: 'X'
#
# @output $flag Returns the flag for a glitch value.
# @warning This flag should only be used when the raw data specifies that
# there was a glitch with the measurement.
##----------------------------------------------------------------
sub getGlitchFlag { return 'X'; }

##----------------------------------------------------------------
# @signature String getIncalcuableFlag()
# Get the QCF flag for an incalcuable value: 'I'
#
# @output $flag Returns the QCF incalcuable value flag.
##----------------------------------------------------------------
sub getIncalcuableFlag { return 'I'; }

##-----------------------------------------------------------------
# @signature String getMissing()
# Get the QCF missing value: -999.99
#
# @output $missing Returns the QCF missing value.
##-----------------------------------------------------------------
sub getMissing { return "-999.99"; }

##-----------------------------------------------------------------
# @signature String getMissingFlag()
# Get the QCF missing value flag: 'M'
#
# @output $flag Returns the QCF missing value flag.
##-----------------------------------------------------------------
sub getMissingFlag { return 'M'; }

##----------------------------------------------------------------
# @signature String getNegativePrecipFlag()
# Get the QCF flag for a recorded negative precip value: 'C'
#
# @output: $flag Returns the QCF negative precip flag.
##----------------------------------------------------------------
sub getNegativePrecipFlag() { return 'C'; }

##-----------------------------------------------------------------
# @signature String getNextDay(String date)
# Determine the next day from a given date.  Assumes the date is 
# in the format "YYYY/MM/DD".
#
# @input  $date  The date to use to get the next day.
# @output $next_day The day after $date in "YYYY/MM/DD" format.
##-----------------------------------------------------------------
sub getNextDay {
    my $date = shift;
    my $year = substr($date, 0, 4);
    my $month = substr($date, 5, 2);
    my $day = substr($date, 8, 2) + 1; # Increase day to be next day.

    my @days_per_month = (31, days_in_feb($year), 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);

    if ($day > $days_per_month[$month - 1]) { # Day increase changed month
	$day = 1;
	$month++;
	if ($month > scalar(@days_per_month)) { # Month increase changed year
	    $month = 1;
	    $year++;
	}
    }

    # format return date to "YYYY/MM/DD"
    return sprintf("%04d/%02d/%02d", $year, $month, $day);
}

##-----------------------------------------------------------------
# @signature (String new_date, String new_time) getNextTime(String date, String time, Integer offset)
# Calculate a new time from a given date, time, and offset.  Assumes
# the date is in the format "YYYY/MM/DD", the time is in the format "HH:MM",
# the range of hours is 0-23, and the offset is in minutes.  The offset can
# be negative.
#
# Modified April 6, 2005 to fix bug which did not decrement the hour correctly.
#
# @input  $date     The original date
# @input  $time     The original time
# @input  $offset   The offset to be added to (subtracted from) the
#                   original date and time
# @output $new_date The calculated $date in "YYYY/MM/DD" format.
# @output $new_time The calculated $time in "HH:MM" format (range = 0-23 hours).
##-----------------------------------------------------------------
sub getNextTime{
    my $date = shift;
    my $time = shift;
    my $offset = shift;

    (my $hour, my $minute) = split ":",$time;
    $minute = $minute + $offset;
    if ($minute >= 60) {
        $minute = $minute - 60;
        $hour +=1;
        if ($hour == 24) {
            $hour = 0;
            $date = Conversions::getNextDay($date);
	}
    } elsif ($minute < 0) {
	$minute = $minute + 60;
	$hour -=1;
	if ($hour == -1) {
            ## Should be $hour = 23; else subracting one hour from 
            ## 2003/05/20 00:00 gives 2003/05/19 00:00!!! Fixed 4/6/05 JAG
	    ###$hour = 0;  ## Old erroneous line.
            $hour = 23;    ## New corrected line.
	    $date = Conversions::getPreviousDay($date);
	}
    }
    return($date,sprintf("%02d:%02d", $hour, $minute));
}

##-----------------------------------------------------------------
# @signature String getNoReadingFlag()
# Get the QCF flag for a measurement that is not collected from a
# station: 'N'
#
# @output $flag Returns the no reading flag.
# @warning This flag should only be used if the station is known to not
# have an instrument for reading a measurement.
##----------------------------------------------------------------
sub getNoReadingFlag() { return 'N'; }

##-----------------------------------------------------------------
# @signature String getPreviousDay(String date)
# Determine the previous day from a given date.  Assumes the date is
# in the format "YYYY/MM/DD".
#
# @input  $date  The date to use to get the previous day.
# @output $next_day The day before $date in "YYYY/MM/DD" format.
##-----------------------------------------------------------------
sub getPreviousDay {
    my $date = shift;
    my $year = substr($date, 0, 4);
    my $month = substr($date, 5, 2);
    my $day = substr($date, 8, 2) - 1; # Decrease day to be previous day.

    my @days_per_month = (31, days_in_feb($year), 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);

    if ($day < 1) { # Day decrease changed month
        if ($month == 1) {
            $day = $days_per_month[scalar(@days_per_month)-1];
	} else {
            $day = $days_per_month[$month - 2];
	}
        $month--;
        if ($month == 0) { # Month decrease changed year
            $month = scalar(@days_per_month);
            $year--;
        }
    }

    # format return date to "YYYY/MM/DD"
    return sprintf("%04d/%02d/%02d", $year, $month, $day);
}

##-----------------------------------------------------------------
# @signature int getStateCode(String state)
# Get the state code number for a specified state.
#
# @input  $state The state abbreviation XX if unknown.
# @output $code The state code number of <code>$state</code>.
##----------------------------------------------------------------
##-----------------------------------------------------------------
# @signature int getStateCode(String state, int web)
# Get the state code number for a specified state.
#
# @input $state The state abbreviation XX if unknown.
# @input $web True if the call is from the web, false otherwise.
# @output $code The state code number of <code>$state</code>.
##----------------------------------------------------------------
sub getStateCode {
    my $state = shift;
    my $web = shift;
    if (!defined($web)) { $web = 0; }
    my %codes = ();
    if ($state =~ m/^[0-9]{1,2}$/) {
	$state = sprintf("%02d", $state);
	%codes = loadStateCodes(0);
    } else {
	%codes = loadStateCodes(1);
    }
    if (defined($codes{$state})) {
	return $codes{$state};
    } else {
	if ($web) {
	    print STDOUT "The state/code of $state is not known.";
	    return "";
	} else {
	    die ("The state/code of $state is not known.");
	}
    }
}

##-----------------------------------------------------------------
# @signature String getTracePrecipFlag()
# Get the QCF flag for a trace amount of precip recorded: 'T'
#
# @output $flag Returns the QCF flag for trace precip recordings.
##-----------------------------------------------------------------
sub getTracePrecipFlag { return 'T'; }

##-----------------------------------------------------------------
# @signature String getValueDoesNotFitFlag()
# Get the QCF flag for when the value is too big to fit in the 
# space provided for it: 'C'
#
# @output $flag Returns the flag for the value not fitting.
##-----------------------------------------------------------------
sub getValueDoesNotFitFlag { return 'C'; }

##-----------------------------------------------------------------
# @signature String getUncheckedFlag()
# Get the QCF flag for values that are unchecked: 'U'
#
# @output $flag Returns the unchecked flag.
##-----------------------------------------------------------------
sub getUncheckedFlag { return 'U'; }

##------------------------------------------------------------------
# @signature float handleZero(float value)
# Handle a negative zero value so it does not display as a negative.
#
# @input  $value The value to be checked.
##------------------------------------------------------------------
sub handleZero {
    my $value = shift;
    if (sprintf("%7.2f", $value) == 0.0) {
	$value = 0.0;
    }
    return $value;
}

##----------------------------------------------------------------
# @signature %String loadStateCodes()
# Load the list of state codes from the file <code>states.list</code>.
#
# @input $state_order True if the keys should be the states, false for the
# code values.
# @output %codes Returns a hash of state abbrevations to state code numbers.
##----------------------------------------------------------------
sub loadStateCodes {
    my $state_order = shift;
    my %codes;
    my $state_file = "/work/software/conversion_modules/Version1/states.list";
    open(CODEFILE, $state_file) || die "Cannot open states.list file.\n";
    while (<CODEFILE>) {
	$_ =~ m/^(.*),(.*)$/;
	if ($state_order) {
	    $codes{"$2"} = $1;
	} else {
	    $codes{"$1"} = $2;
	}
    }
    close(CODEFILE);
    return %codes;
}

##-------------------------------------------------------------------------
# @signature float pi()
# <p>Get the value of PI ~ 3.14159265</p>
#
# @output $pi The value of pi.
##-------------------------------------------------------------------------
sub pi { return 4 * atan2(1, 1); }

##-------------------------------------------------------------------------
# @signature float radToDeg(float rad)
# <p>Convert a radian value to degrees.</p>
#
# @input $rad The angle in radians.
# @output $deg The angle converted to degrees.
##------------------------------------------------------------------------
sub radToDeg {
    my $rad = shift;
    return $rad * 180 / pi();
}

##-------------------------------------------------------------------------
# @signature int validDate(String date)
# <p>Determine if a date in YYYY/MM/DD format is an actual date of the 
# year.</p>
#
# @input $date The date to check to see if it is a valid date.
# @output $valid Returns true if the date is a valid date, false otherwise.
# @limitation The date must be in YYYY/MM/DD format.
##-------------------------------------------------------------------------
sub validDate {
    my $date = shift;
    my $year = substr($date, 0, 4);
    my $month = substr($date, 5, 2);
    my $day = substr($date, 8, 2);

    if ($day <= 0) {
	return 0;
    } elsif ($month == 2 && $day <= days_in_feb($year)) { # 28,29 day months
	return 1;
    } elsif (($month == 4 || $month == 6 || $month == 9 || $month == 11) &&
	     $day <= 30) {
	return 1;
    } elsif (($month == 1 || $month == 3 || $month == 5 || $month == 7 ||
	      $month == 8 || $month == 10 || $month == 12) && $day <= 31) {
	return 1;
    } else {
	return 0;
    }
}

##-------------------------------------------------------------------------
# @signature int validTime(String time)
# <p>Determine if a time in HH:MM format is an actual time of day.</p>
# 
# @input $time The time to check to see if it is a valid time.
# @output $valid Returns true if the time is a valid time, false otherwise.
# @limitation The time must be in HH:MM format.
##-------------------------------------------------------------------------
sub validTime {
    my $time = shift;
    my $hour = substr($time, 0, 2);
    my $min = substr($time, 3, 2);

    if ($hour >= 0 && $hour <= 23 && $min >= 0 && $min <= 59) {
	return 1;
    } else {
	return 0;
    }
}
1; # Needed to return from a script call

