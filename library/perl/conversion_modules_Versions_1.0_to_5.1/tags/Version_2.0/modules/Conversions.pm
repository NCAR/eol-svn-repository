#! /usr/bin/perl -w

package Conversions;
use strict;
use lib ".";
use POSIX;

##---------------------------------------------------------------------------
# @signature (String, String) adjustDateTime(String date, String time, int day_offset, int hour_offset, int min_offset)
# <p>Shift the specified date and time by the amount specified in either 
# positive or negative values.  A combination of negative and positive value
# will subtract and add the values given and does not imply all one or the
# other.
# 
# @input $date The date in YYYY/MM/DD format.
# @input $time The time in HH:MM format.
# @input $day_offset The number of days to increase/decrease the date by.
# @input $hour_offset The number of hours to increase/decrease the hour by.
# @input $min_offset The number of hours to increase/decrease the minutes by.
##---------------------------------------------------------------------------
sub adjustDateTime {
  my $date = shift;
  my $time = shift;
  my $day_offset = shift;
  my $hour_offset = shift;
  my $min_offset = shift;

  my ($year, $month, $day) = split('/', $date);
  $day = convertJulian($year, $day, $month);
  my ($hour, $min) = split(':', $time);

  $min += $min_offset; 
  while ($min > 59) { $hour_offset++; $min -= 60; }
  while ($min < 00) { $hour_offset--; $min += 60; }

  $hour += $hour_offset;
  while ($hour > 23) { $day_offset++; $hour -= 24; }
  while ($hour < 00) { $day_offset--; $hour += 24; }

  $day += $day_offset;
  my $days_in_year = (daysInFeb($year) == 28) ? 365 : 366;
  while ($day > $days_in_year) { 
    $year++;
    $day -= $days_in_year;
    $days_in_year = (daysInFeb($year) == 28) ? 365 : 366;
  }
  while ($day < 1) {
    $year--;
    $day += (daysInFeb($year - 1) == 28) ? 365 : 366;
  }

  return (sprintf("%04d/%02d/%02d", $year, convertJulian($year, $day)),
	  sprintf("%02d:%02d", $hour, $min));
}

##---------------------------------------------------------------------------
# @signature float calculateDewPoint(String temp, String rh, --int verbose--, --FILE OUT--)
# <p>Calculate the dew point from the provided temperature and relative 
# humidity.  The formula used for the calculation is:<pre>
# sat_vapor_pressure = 6.112 * exp((17.67 * temp) / temp + 243.5)
# vapor_pressure = sat_vapor_pressure * (rh / 100.0)
# log_value = log(vapor_pressure / 6.112)
# dew_point = log_value * 243.5 / (17.67 - log_value)
# </pre></p>
#
# @input $temp The temperature value in degrees C.
# @input $rh The relative humidity value in percent.
# @input $verbose <b>Optional</b> A flag to print out warnings.  Default = 1
# @input $OUT <b>Optional</b> The file to print the warnings to.  
# Default = STDOUT.  Needs the verbose flag manually set to 1.
# @output $dew The calculated dew point value in degrees C.
# @warning This will return the missing value if the temperature is less than 
# -100&deg;C or greater than 100&deg;C, the relative humidity is less than 
# 0.05% or greater than 100%, or if either value is missing.
##---------------------------------------------------------------------------
sub calculateDewPoint {
    my $temp = $_[0];
    my $rh = $_[1];
    my $verbose = $_[2];
    my $OUT = $_[3];
    
    # Assign default values to the variables if they were not assigned.
    if (!defined($verbose)) { $verbose = 1 }
    if (!defined($OUT)) { $OUT = *STDOUT; }
    
    if ($temp eq getMissing() || $rh eq getMissing()) {
	return getMissing();
    } elsif ($rh <= 0) {
	if ($verbose) {
	    printf($OUT "Cannot calculate dew point with a relative humidity (%7.2f) at or below 0.\n", $rh);
	}
	return getMissing();
    } else { # Calculate the dew point
	my $sat_vapor_pressure = 6.112 * exp((17.67 * $temp)/($temp + 243.5));
	my $vapor_pressure = $sat_vapor_pressure * ($rh / 100.0);
	my $log_value = log($vapor_pressure / 6.112);
	my $dew_point = $log_value * 243.5 / (17.67 - $log_value);
	return handleZero($dew_point);
    }
}

##--------------------------------------------------------------------------
# @signature float calculateNetRadiation(float short_in, short_out, long_in, long_out, --int verbose--, --FILE OUT--)
# <p>Calculate the net radiation from teh provided incoming and outgoing 
# shortwave and longwave values.  The equation used for the calculation is:
# <pre>
# NET_Radiation = short_in + long_in - short_out - long_out</pre></p>
#
# @input $short_in The incoming shortwave radiation value in W/m2.
# @input $short_out The outgoing shortwave radiation value in W/m2.
# @input $long_in The incoming longwave radiation value in W/m2.
# @input $long_out The outgoing longwave radiation value in W/m2.
# @input $verbose <b>Optional</b> A flag to print out warnings.  Default = 1
# @input $OUT <b>Optional</b> The file where the warnings should be printed.
# Default = STDOUT.  Needs the verbose flag manually set to 1.
# @output $net_rad The net radiation value in W/m2.
##--------------------------------------------------------------------------
sub calculateNetRadiation {
    my ($short_in, $short_out, $long_in, $long_out, $verbose, $OUT) = @_;

    if (!defined($verbose)) { $verbose = 1; }
    if (!defeind($OUT)) { $OUT = *STDOUT; }

    if ($short_in eq getMissing() || $short_out eq getMissing() ||
	$long_in eq getMissing() || $long_out eq getMissing()) {
	return getMissing();
    } else {
	return $short_in + $long_in - $short_out - $long_out;
    }
}

##--------------------------------------------------------------------------
# @signature float calculateRelativeHumidity(float temp, float dewpt, --int verbose--, --FILE OUT--)
# <p>Calculate the relative humidity from the provided temperature and dew 
# point values.  The equations used for the calculation are:<pre>
# sat_vapor_pressure = 6.112 * exp((17.67 * temp)/(temp + 243.5))
# E_sat_vapor_pressure = 6.112 * exp((17.67 * dewpt) / (dewpt + 243.5))
# rel_humidity = 100.0 * (E_sat_vapor_pressure / sat_vapor_pressure)
# </pre></p>
#
# @input $temp The temperature value in degrees C.
# @input $dewpt The dew point value in degress C.
# @input $verbose <b>Optional</b> A flag to print out warnings.  Default = 1
# @input $OUT <b>Optional</b> The file where the warnings should be printed.
# Default = STDOUT.  Needs the verbose flag manually set to 1.
# @output $rh The calculated relative humidity value in percent.
# @warning This will return the QCF missing value if the temperature is less
# than -100&deg;C or greater than 100&deg;C, the dew point is less than 
# -100&deg;C or greater than 100&deg;C, the dew point is greater than the 
# temperature, or if either value is missing.
##--------------------------------------------------------------------------
sub calculateRelativeHumidity {
    my $temp = $_[0];
    my $dewpt = $_[1];
    my $verbose = $_[2];
    my $OUT = $_[3];
    
    if (!defined($verbose)) { $verbose = 1; };
    if (!defined($OUT)) { $OUT = *STDOUT; }
    
    if ($temp eq getMissing() || $dewpt eq getMissing()) { 
	return getMissing();
	# Calculate the Relative Humidity
    } else {
	my $sat_vapor_pressure = 6.112 * exp((17.67 * $temp)/($temp + 243.5));
	my $E_sat_vapor_pressure=6.112*exp((17.67 * $dewpt)/($dewpt + 243.5));
	my $rel_humidity = 100.0 * ($E_sat_vapor_pressure/$sat_vapor_pressure);
	return handleZero($rel_humidity);
    }
}

##--------------------------------------------------------------------------
# @signature float calculateSeaLevelPressure(float press, float elev, float dewpt, float temp, --int verbose--, --FILE OUT--)
# <p>Calculate the sea level pressure from the provided station pressure,
# elevation, dew point, and temperature.  The equations used for the 
# calculation are:<pre>
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
# @input $press The station pressure in millibars.
# @input $elev The elevation in meters.
# @input $dewpt The dew point in degrees Celcius.
# @input $temp The temperature in degrees Celcius.
# @input $verbose <b>Optional</b> A flag to print out warnings.  Default = 1.
# @input $OUT <b>Optional</b> The file where warnings should be printed.
# Default = STDOUT.
# @output $slp The calculated sea level pressure in millibars.
# @warning This function will return the QCF if the pressure is less than 
# 300 mbars or greater than 1200 mbars, the elevation is less than 0, the 
# dew point is less than -100&deg;C or greater than 100&deg;C, the temperature
# is less than -100&deg;C or greater than 100&deg;C, or any of the four values
# is missing.
##--------------------------------------------------------------------------
sub calculateSeaLevelPressure {
    my $press = $_[0];
    my $elev = $_[1];
    my $dewpt = $_[2];
    my $temp = $_[3];
    my $verbose = $_[4];
    my $OUT = $_[5];

    if (!defined($verbose)) { $verbose = 1; }
    if (!defined($OUT)) { $OUT = *STDOUT; }
    
    if ($press eq getMissing() || $elev eq getMissing() ||
	$dewpt eq getMissing() || $temp eq getMissing()) {
	return getMissing();
	
	# Calculate the sea level pressure
    } else {
	# Constants
	my $gamma = 6.5; my $gravity = 9.80616; my $r = 287.04;
	
	# Calculations
	my $kelvin = convertTemperature($temp, "C", "K");
	my $vapr = 6.112 * exp ((17.67 * $dewpt) / ($dewpt + 243.5));
	my $e = $vapr * (1.001 + ($press - 100.0) / 900.0 * 0.0034);
	my $mixr = 0.62197 * ($e / ($press - $e)) * 1000.0;
	my $virt_temp = $kelvin * ((1.0 + (0.001 * $mixr) / 0.62197) / 
				   (1.0 + 0.001 * $mixr));
	my $deltv = $gamma * $elev / 1000.0;
	my $avg_virt_temp = ($virt_temp + ($virt_temp - $deltv)) / 2.0;
	my $value = handleZero($press*exp(($gravity*$elev)/($r*$avg_virt_temp)));

	return $value > 9999.99 ? getMissing() : $value;
    }
}

##--------------------------------------------------------------------------
# @signature float calculateSpecificHumidity(float press, float dewpt, --int verbose--, --FILE OUT--)
# <p>Calculate the specific humidity from the pressure and the dew point.
# The equations used for the calculation are:<pre>
#    e = 6.112 * exp((17.67 * dewpt)/(243.5 + dewpt));
#    sh = (0.622 * e)/(press - (0.378 * e));
# </pre>
#
# @input $press The pressure in millibars.
# @input $dewpt The dew point in &deg;C.
# @input $verbose <b>Optional</b> A flag to print out warnings.  Default = 1.
# @input $OUT <b>Optional</b> The file where warnings should be printed.
# Default = STDOUT.
# @output $sh The calculated specific humidity value.
# @warning This function will return the missing value if either the pressure
# or the dewpoint are missing, if the pressure is not between 100 and 1500 
# mbars or the dewpoint is not between -100 and 100 &deg;C.
##--------------------------------------------------------------------------
sub calculateSpecificHumidity {
    my $press = $_[0];
    my $dewpt = $_[1];
    my $verbose = $_[2];
    my $OUT = $_[3];
    
    if (!defined($verbose)) { $verbose = 1; }
    if (!defined($OUT)) { $OUT = *STDOUT; }
    
    if ($press eq getMissing() || $dewpt eq getMissing()) {
	return getMissing(); 
    } else {
	# Calculate the Specific Humidity
	my $e = 6.112 * exp((17.67 * $dewpt)/(243.5 + $dewpt));
	my $sh = (0.622 * $e)/($press - (0.378 * $e));

	# Sanity Check
	if ($sh < 0 || $sh > .04) {
	    if ($verbose) {
		printf($OUT "Specific humidity is out of range: %f\n", $sh);
	    }
	}
	return $sh;
    }
}

##--------------------------------------------------------------------------
# @signature float calculateStationPressure(float alt, float elev, --int verbose--, --FILE OUT--)
# <p>Calculate the station pressure from an altimeter reading and the 
# elevation of the station.  The equations used for the calculation are<pre>
#  p0 = 1013.25;
#  a = .0065;
#  t0 = 288;
#  n = .190284;
#  press =  ((((alt ** $n) - ((((p0 ** n) * a) / t0) * elev)) ** (1/n)) + .01);
# </pre>
#
# @input $alt The altimeter reading in millibars.
# @input $elev The elevation of the station in meters.
# @input $verbose <b>Optional</b> A flag to print out warnings.  Default = 1.
# @input $OUT <b>Optional</b> The file where warnings should be printed.
# Default = STDOUT.
# @output $press The calculated station pressure in millibars.
# @warning This function will return the missing value if either the altimeter
# value or the elevation are missing or if the calculation would create an
# imaginary number because of a bad altimeter value.
##--------------------------------------------------------------------------
sub calculateStationPressure {
    my $alt = $_[0];
    my $elev = $_[1];
    my $verbose = $_[2];
    my $OUT = $_[3];
    
    if (!defined($verbose)) { $verbose = 1; }
    if (!defined($OUT)) { $OUT = *STDOUT; }
    
    if ($alt eq getMissing() || $elev eq getMissing()) { return getMissing(); }
    
    # Set Constants
    my $p0 = 1013.25;
    my $a = .0065;
    my $t0 = 288;
    my $n = .190284;
    
    # Calculations
    if (($alt ** $n) < ((($p0 ** $n) * $a) / $t0)) {
	if ($verbose) {
	    printf($OUT "Altimeter value (%7.2f) caused an imaginary number in the station pressure calculation.\n", $alt);
	}
	return getMissing();
    }

    # Calculate Station Pressure
    return (((($alt ** $n)-(((($p0 ** $n) * $a)/$t0) * $elev)) ** (1/$n))+.01);
}

##--------------------------------------------------------------------------
# @signature (float ucomp, float vcomp) calculateUVfromWind(float spd, float dir)
# <p>Calculate the U and V wind components from the wind speed and wind
# direction. The equations used for the calculation are:<pre>
# ucomp = -1 * sin(degToRad(dir)) * spd;
# vcomp = -1 * cos(degToRad(dir)) * spd;
# </pre></p>
#
# @input $spd The wind speed in m/s.
# @input $dir The wind direction in degrees.
# @output $ucomp The U component of the wind.
# @output $vcomp The V component of the wind.
# @warning This function will return the missing value if either the wind speed
# or the wind direction are missing, if the wind speed is below zero, or if
# the wind direction is not between 0 and 360 degrees.
##--------------------------------------------------------------------------
sub calculateUVfromWind {
    my $spd = $_[0];
    my $dir = $_[1];
    my $verbose = $_[2];
    my $OUT = $_[3];
    
    if (!defined($verbose)) { $verbose = 1; }
    if (!defined($OUT)) { $OUT = *STDOUT; }
    
    if ($spd < 0 || $dir < 0 || $dir > 360) {
	return(getMissing(),getMissing());
    }
    
    my $ucomp = -1 * sin(degToRad($dir)) * $spd;
    my $vcomp = -1 * cos(degToRad($dir)) * $spd;
    
#    if ($verbose && ($ucomp < -200 || $ucomp > 200)) {
#	printf($OUT "Calculated U wind component (%7.2f) is out of the expected range.\n", $ucomp);
#    }
#    if ($verbose && ($vcomp < -200 || $vcomp > 200)) {
#	printf($OUT "Calculated V wind component (%7.2f) is out of the expected range.\n", $vcomp);
#    }

    return (handleZero($ucomp), handleZero($vcomp));
}

##--------------------------------------------------------------------------
# @signature (float spd, float dir) calculateWindFromUV(float ucomp, float vcomp)
# <p>Calculate the wind speed and wind direction from the U and V wind
# components.  The equations used for the calculations are:<pre>
# spd = sqrt(ucomp ** 2 + vcomp ** 2);
# dir = radToDeg(atan(ucomp / vcomp));
# </pre></p>
#
# @input $ucomp The U wind component.
# @input $vcomp The V wind component.
# @output $spd The wind speed in m/s.
# @output $dir The wind direction in degrees.
# @warning This function will return the missing value if either the U or the
# V wind component is missing.
##--------------------------------------------------------------------------
sub calculateWindFromUV {
    my $ucomp = $_[0];
    my $vcomp = $_[1];
    
    if ($ucomp eq getMissing() || $vcomp eq getMissing()) { 
	return getMissing();
    }

    # Special Case to prevent a divide by zero later.
    if ($vcomp == 0) {
	if ($ucomp == 0) { return (0, 0); }
	elsif ($ucomp > 0) { return ($ucomp, 270); }
	else { return ((-1 * $ucomp), 90); }
    }

    # Calculate wind direction and wind speed
    my $spd = sqrt($ucomp ** 2 + $vcomp ** 2);
    my $dir = radToDeg(atan($ucomp / $vcomp));
    if ($vcomp >= 0) { $dir += 180; } # Gets to other half of unit circle
    
    # Force wind direction to be between 0 and 360
    if ($dir < 0) { $dir += 360; }
    
    return ($spd, $dir);
}

##--------------------------------------------------------------------------
# @signature int compareDates(String first, String second)
# <p>Compare two dates to determine if the first date is before, the same, or
# after the second.</p>
#
# @input $first The first date to compare.
# @input $second The second date to compare.
# @output $value -1, 0, or 1 if the first date is less than, equal to, or
# greater than the second date, or undef() if either date is not in 
# YYYY/MM/DD format.
# @limitation Both dates must be in YYYY/MM/DD format.
##--------------------------------------------------------------------------
sub compareDates {
    if ($_[0] !~ /^[0-9]{4}\/[0-9]{2}\/[0-9]{2}$/) { return undef(); }
    if ($_[1] !~ /^[0-9]{4}\/[0-9]{2}\/[0-9]{2}$/) { return undef(); }
    
    # Remove the / marks to make each date an 8 digit integer
    my $first = $_[0]; $first =~ s/\///g;
    my $second = $_[1]; $second =~ s/\///g;
    
    if ($first < $second) { return -1; }
    elsif ($first == $second) { return 0; }
    else { return 1; }
}

##--------------------------------------------------------------------------
# @signature float convertArea(float value, String in, String out)
# <p>Convert an area value from one unit of measurement to another.  This
# conversion currently recognizes the units of:<ul>
#   <li>acres - Acres</li>
#   <li>cm2   - Square Centimeters</li>
#   <li>ft2   - Square Feet</li>
#   <li>in2   - Square Inches</li>
#   <li>km2   - Square Kilometers</li>
#   <li>m2    - Square Meters</li>
#   <li>mile2 - Square Miles</li>
#   <li>mm2   - Square Millimeters</li>
#   <li>yd2   - Square Yards</li>
# </ul></p>
#
# @input $value The initial value of the area to be converted.
# @input $in The initial units of the value.
# @input $out The target units of the value.
# @output $area The area in the output units.
# @warning This function will die if it receives an area unit that it does
# not recognize.
##--------------------------------------------------------------------------
sub convertArea {
  if ($_[0] eq getMissing()) { return getMissing(); }
  my %ins = ("acres" => sub { 
	       return ($_[0] / 640) * convertLength(1, "mile", "m") ** 2; },
	     "cm2"   => sub { return $_[0] * convertLength(1,"cm","m") ** 2; },
	     "ft2"   => sub { return $_[0] * convertLength(1,"ft","m") ** 2; },
	     "in2"   => sub { return $_[0] * convertLength(1,"in","m") ** 2; },
	     "km2"   => sub { return $_[0] * convertLength(1,"km","m") ** 2; },
	     "m2"    => sub { return $_[0]; },
	     "mile2" => sub { return $_[0] * convertLength(1,"mile","m")**2; },
	     "mm2"   => sub { return $_[0] * convertLength(1,"mm","m") ** 2; },
	     "yd2"   => sub { return $_[0] * convertLength(1,"yd","m") ** 2;});
  my %outs =("acres" => sub {
	       return ($_[0] * 640) * convertLength(1, "m", "mile") ** 2; },
	     "cm2"   => sub { return $_[0] * convertLength(1,"m","cm") ** 2; },
	     "ft2"   => sub { return $_[0] * convertLength(1,"m","ft") ** 2; },
	     "in2"   => sub { return $_[0] * convertLength(1,"m","in") ** 2; },
	     "km2"   => sub { return $_[0] * convertLength(1,"m","km") ** 2; },
	     "m2"    => sub { return $_[0]; },
	     "mile2" => sub { return $_[0] * convertLength(1,"m","mile")**2; },
	     "mm2"   => sub { return $_[0] * convertLength(1,"m","mm") ** 2; },
	     "yd2"   => sub { return $_[0] * convertLength(1,"m","yd") ** 2;});
  if (!defined($ins{$_[1]})) {
    die("Area input unit of $_[1] not known.\n");
  }
  if (!defined($outs{$_[2]})) {
    die("Area output unit of $_[2] not known.\n");
  }
  return handleZero($outs{$_[2]}->($ins{$_[1]}->($_[0])));
}

##--------------------------------------------------------------------------
# @signature float convertFlow(float value, String in, String out)
# <p>Convert a flow value from one unit of measurement to another.  This
# function currently recognizes the following units:<ul>
#   <li>ft3/s - Cubic Feet per Second</li>
#   <li>m3/s  - Cubic Meters per Second</li>
# </ul></p>
#
# @input $value The initial flow value.
# @input $in The initial flow units.
# @input $out The target flow units.
# @output $flow The flow value converted to the target units.
# @warning This function will die if it receives a unit of flow that it does
# not recognize.
##--------------------------------------------------------------------------
sub convertFlow {
  if ($_[0] eq getMissing()) { return getMissing(); }
  my %ins = ("ft3/s" => sub { return $_[0] * convertVolume(1,"ft3","m3"); },
	     "m3/s"  => sub { return $_[0]; });
  my %outs= ("ft3/s" => sub { return $_[0] * convertVolume(1,"m3","ft3"); },
	     "m3/s"  => sub { return $_[0]; });
  if (!defined($ins{$_[1]})) {
    die("Flow input unit of $_[1] not known.\n");
  }
  if (!defined($outs{$_[2]})) {
    die("Flow output unit of $_[2] not known.\n");
  }
  return handleZero($outs{$_[2]}->($ins{$_[1]}->($_[0])));
}

##--------------------------------------------------------------------------
# @signature (int, int) convertJulian(int year, int julian)
# <p>Convert a julian date into a month and day.</p>
# @input $year The year of the julian date.
# @input $julian The julian date.
# @output $month The month of the julian date.
# @output $day The day in the month of the julian date.
##--------------------------------------------------------------------------
##--------------------------------------------------------------------------
# @signature int convertJulian(int year, int days, int month)
# <p>Convert a month and day into a julian date.</p>
# @input $year The year of the date.
# @input $days The day of the month of the date.
# @input $month The month of the date.
# @output $julian The julian date from the month and day for the year.
##--------------------------------------------------------------------------
sub convertJulian {
  my $days = $_[1] + 0;
  my $month = $_[2];

  my @daysInMonth = (31,daysInFeb($_[0]),31,30,31,30,31,31,30,31,30,31);

  if (defined($month)) {
    while ($month > 1) {
      $days += $daysInMonth[$month-2] if defined($daysInMonth[$month-2]);
      $month--;
    }
    return $days;
  } else {
    $month = 1;
    while ($days > $daysInMonth[$month - 1]) {
      $days = $days - $daysInMonth[$month - 1];
      $month++;
    }
    return ($month, $days);
  }
}

##--------------------------------------------------------------------------
# @signature (Number degs, int mins, int secs) convertLatLong(String value, String format, --String out_format--)
# <p>Convert a latitude/longitude value in the specified format to the output
# format or, if unspecified, to degrees.  The function recognizes the following
# format characters and ignores the rest:<ul>
#   <li>D: Degrees</li>
#   <li>M: Minutes</li>
#   <li>S: Seconds</li>
#   <li>-: For negative values</li></ul>
# Decimal points '.' should have a format value of D,M, or S.</p>
# <p>The output format only recognizes<ul>
#   <li>D: Degrees</li>
#   <li>DMS: Degrees Minutes Seconds</li></ul></p>
#
# @input $value The latitude/longitude being converted.
# @input $format The original format of the latitude/longitude value.
# @input $out_format <b>Optional</b> A 'D' or 'DMS' value of the output format
# of the latitude/longitude.  If omitted, it will use the default of 'D'.
# @output $degs The number of degrees of the latitude/longitude value.
# @output $mins The number of minutes of the latitude/longitude value.  Only
# used if 'DMS' is out_format.
# @output $secs The number of seconds of the latitued/longitude value.  Only
# used if 'DMS' is out_format.
# @warning This function will die if the length of the value is not the same
# as the length of the format or if the out_format is not one of 'D' or 'DMS'.
##--------------------------------------------------------------------------
sub convertLatLong {
  # Ensure that the format matches the value
  if (length($_[0]) != length($_[1])) {
    die("Length of $_[0] is not the same as the length of $_[1] to convert ".
	"latitude/longitude\n");
  }

  # Check to see if the output format is defined and define it if it is not.
  if (!defined($_[2])) { $_[2] = "D"; }

  # Make sure the output format is an expected value.
  if ($_[2] ne "D" && $_[2] ne "DMS") {
    die("Output format of $_[2] is not recognized to convert lat/long\n");
  }

  # Split the value into its component pieces based on the format.
  my $degs = "0"; my $mins = "0"; my $secs = "0"; my $mult = 1;
  for(my $index = 0; $index < length($_[0]); $index++) {
    my $fmt_char = substr($_[1], $index, 1);
    if ($fmt_char eq "D") { $degs .= substr($_[0], $index, 1); }
    elsif ($fmt_char eq "M") { $mins .= substr($_[0], $index, 1); }
    elsif ($fmt_char eq "S") { $secs .= substr($_[0], $index, 1); }
    elsif ($fmt_char eq "-") { $mult = -1; }
  }

  my $total = ($degs + $mins / 60 + $secs / 3600);

  # Convert the value to the specified output format.
  if ($_[2] eq "D") {
    return $mult * $total
  } else {
    if ($mins == 0 && $secs == 0) {
      $degs = int($total);
      $mins = int(($total - $degs) * 60);
      $secs = ($total - $degs - ($mins / 60)) * 3600;
    }
    return ($degs * $mult, $mins, $secs);	    
  }
}

##-------------------------------------------------------------------------
# @signature float convertLength(float value, String in, String out)
# <p>Convert a length value from one unit of measurement to another.  This
# conversion currently recognizes the units of:<ul>
#   <li>cm    - Centimeters</li>
#   <li>dm    - Decimeters</li>
#   <li>ft    - Feet</li>
#   <li>Hft   - Hundreds of Feet</li>
#   <li>hi    - Hundredths of an Inch</li>
#   <li>in    - Inches</li>
#   <li>km    - Kilometers</li>
#   <li>m     - Meters</li>
#   <li>mile  - (Statute) Miles</li>
#   <li>mm    - Millimeters</li>
#   <li>nmile - Nautical Mile</li>
#   <li>ti    - Tenths of an Inch</li>
#   <li>yd    - Yards</li>
#   </ul></p>
#
# @input $value The initial value of the length to be converted.
# @input $in The initial units of the length.
# @input $out The output units for the length.
# @output $value The length in the output units.
# @warning This method will di if it does not receive a unit of length that
# it recognizes.
##-------------------------------------------------------------------------
sub convertLength {
  if ($_[0] eq getMissing()) { return getMissing(); }
  my %ins = ("cm"    => sub { return $_[0] * .01; },
	     "dm"    => sub { return $_[0] * .1; },
	     "ft"    => sub { return $_[0] * .3048; },
	     "Hft"   => sub { return $_[0] * 30.48; },
	     "hi"    => sub { return $_[0] * .000254; },
	     "in"    => sub { return $_[0] * .0254; },
	     "km"    => sub { return $_[0] * 1000; },
	     "m"     => sub { return $_[0]; },
	     "mile"  => sub { return $_[0] * 1609.344; },
	     "mm"    => sub { return $_[0] * .001; },
	     "nmile" => sub { return $_[0] * 1852; },
	     "ti"    => sub { return $_[0] * .00254; },
	     "yd"    => sub { return $_[0] * .9144; });
  my %outs = ("cm"    => sub { return $_[0] / .01; },
	      "dm"    => sub { return $_[0] / .1; },
	      "ft"    => sub { return $_[0] / .3048; },
	      "Hft"   => sub { return $_[0] / 30.48; },
	      "hi"    => sub { return $_[0] / .000254; },
	      "in"    => sub { return $_[0] / .0254; },
	      "km"    => sub { return $_[0] / 1000; },
	      "m"     => sub { return $_[0]; },
	      "mile"  => sub { return $_[0] / 1609.344; },
	      "mm"    => sub { return $_[0] / .001; },
	      "nmile" => sub { return $_[0] / 1852; },
	      "ti"    => sub { return $_[0] / .00254; },
	      "yd"    => sub { return $_[0] / .9144; });

  if (!defined($ins{$_[1]})) {
    die("Length input unit of $_[1] not known.\n");
  }
  if (!defined($outs{$_[2]})) {
    die("Length output unit of $_[2] not known.\n");
  }
  return handleZero($outs{$_[2]}->($ins{$_[1]}->($_[0])));
}

##--------------------------------------------------------------------------
# @signature float convertPressure(float value, String in, String out)
# <p>Convert a pressure value from one unit of measurement to another.  This
# conversion currently recognizes the following units:<ul>
#    <li>atm      - Atmosphere</li>
#    <li>bar      - Bar</li>
#    <li>dyne/cm2 - Dyne per Square Centimeter</li>
#    <li>hPa      - Hectopascals</li>
#    <li>inHg     - Inches of Mercury</li>
#    <li>kPa      - Kilopascals</li>
#    <li>mb       - Millibars</li>
#    <li>mbar     - Millibars</li>
#    <li>mmHg     - Millimeters of Mercury</li>
#    <li>Pa       - Pascals</li>
# </ul></p>
#
# @input $value The initial pressure value.
# @input $in The initial pressure units.
# @input $out The target pressure units.
# @output $val The pressure value in the target units.
# @warning This function will die if it receives a unit of pressure is does
# not recognize.
##--------------------------------------------------------------------------
sub convertPressure {
  if ($_[0] eq getMissing()) { return getMissing(); }
  my %ins = ("atm"  => sub { return $_[0] * 1.01325; },
	     "bar"  => sub { return $_[0]; },
	     "dyne/cm2"=> sub { return $_[0] / 1000000; },
	     "hPa"  => sub { return $_[0] / 1000; }, 
	     "inHg" => sub { return $_[0] * (25.73655 / 760); },
	     "kPa"  => sub { return $_[0] / 100; }, 
	     "mb"   => sub { return $_[0] / 1000; },
	     "mbar" => sub { return $_[0] / 1000; },
	     "mmHg" => sub { return $_[0] * 1.01325 / 760; },
	     "Pa"   => sub { return $_[0] / 100000; });
  my %outs= ("atm"  => sub { return $_[0] / 1.01325; },
	     "bar"  => sub { return $_[0]; },
	     "dyne/cm2" => sub { return $_[0] * 1000000; },
	     "hPa"  => sub { return $_[0] * 1000; },
	     "inHg" => sub { return $_[0] * (760 / 25.73655); },
	     "kPa"  => sub { return $_[0] * 100; },
	     "mb"   => sub { return $_[0] * 1000; },
	     "mbar" => sub { return $_[0] * 1000; },
	     "mmHg" => sub { return $_[0] * 760 / 1.01325; },
	     "Pa"   => sub { return $_[0] * 100000; });
  
  if (!defined($ins{$_[1]})) {
    die("Pressure input unit of $_[1] not known.\n");
  }
  if (!defined($outs{$_[2]})) {
    die("Pressure output unit of $_[2] not known.\n");
  }
  return handleZero($outs{$_[2]}->($ins{$_[1]}->($_[0])));
}

##--------------------------------------------------------------------------
# @signature float convertRadiation(float value, String in, String out)
# <p>Convert a radiation value from one unit of measurement to another.  This
# conversion currently recognizes the following units:<ul>
#   <li>langly - Langly</li>
#   <li>w/m2   - Something per Square Meter</li>
# </ul></p>
#
# @input $value The initial radiation value.
# @input $in The initial units of measurements.
# @input $out The target units of measurements.
# @output $rad The converted radiation to the output units.
# @warning The function will die if it receives a unit of measurement that
# it does not recognize.
##--------------------------------------------------------------------------
sub convertRadiation {
  if ($_[0] eq getMissing()) { return getMissing(); }
  my %ins = ("langly" => sub { return $_[0] * 10; },
	     "w/m2"   => sub { return $_[0]; });
  my %outs= ("langly" => sub { return $_[0] / 10; },
	     "w/m2"   => sub { return $_[0]; });

  if (!defined($ins{$_[1]})) {
    die("Radiation input unit of $_[1] not known.\n");
  }
  if (!defined($outs{$_[2]})) {
    die("Radiation output unit of $_[2] not known.\n");
  }
  return handleZero($outs{$_[2]}->($ins{$_[1]}->($_[0])));
}

##--------------------------------------------------------------------------
# @signature float convertTemperature(float value, String in, String out)
# <p>Convert a temperature value from one unit of measurement to another. This
# conversions currently recognizes the units of:<ul>
# <li>C - Celcius</li>
# <li>F - Farenheit</li>
# <li>K - Kelvin</li>
# </ul></p>
#
# @input $value The initial value of the temperature to be converted.
# @input $in The initial units of the temperature.
# @input $out The output units for the temperature.
# @output $value The temperature in the output units.
# @warning This method will die if it does not receive a unit of temperature
# that it recognizes.
##--------------------------------------------------------------------------
sub convertTemperature {
  if ($_[0] eq getMissing()) { return getMissing(); }
  my %ins = ("C" => sub { return $_[0]; },
	     "F" => sub { return ($_[0] - 32) * (5 / 9); },
	     "K" => sub { return $_[0] - 273.15; });
  my %outs = ("C" => sub { return $_[0]; },
	      "F" => sub { return 32 + ($_[0] * (9 / 5)); },
	      "K" => sub { return $_[0] + 273.15; });
  if (!defined($ins{$_[1]})) { 
    die("Temperature input unit of $_[1] not known.\n"); 
  }
  if (!defined($outs{$_[2]})) { 
    die("Temperature output unit of $_[2] not known.\n");
  }
  return handleZero($outs{$_[2]}->($ins{$_[1]}->($_[0])));
}

##--------------------------------------------------------------------------
# @signature float convertTime(float value, String in, String out)
# <p>Convert a time value from one unit of measurement to another.  This
# conversion currently recognizes the following units:<ul>
#   <li>day - Days</li>
#   <li>hr  - Hours</li>
#   <li>min - Minutes</li>
#   <li>sec - Seconds</li>
# </ul></p>
# 
# @input $value The initial value of time.
# @input $in The initial units of measurement.
# @input $out The target units of measurement.
# @output $time The value converted to the target units.
# @warning This function will die if it receives a unit of measurement that
# it does not recognize.
##--------------------------------------------------------------------------
sub convertTime {
  if ($_[0] eq getMissing()) { return getMissing(); }
  my %ins = ("day" => sub { return $_[0] * 24; },
	     "hr"  => sub { return $_[0]; },
	     "min" => sub { return $_[0] / 60; },
	     "sec" => sub { return $_[0] / 3600; });
  my %outs= ("day" => sub { return $_[0] / 24; },
	     "hr"  => sub { return $_[0]; },
	     "min" => sub { return $_[0] * 60; },
	     "sec" => sub { return $_[0] * 3600; });

  if (!defined($ins{$_[1]})) {
    die("Time input unit of $_[1] not known.\n");
  }
  if (!defined($outs{$_[2]})) {
    die("Time output unit of $_[2] not known.\n");
  }
  return handleZero($outs{$_[2]}->($ins{$_[1]}->($_[0])));
}

##--------------------------------------------------------------------------
# @signature float convertVelocity(float value, String in, String out)
# <p>Convert a velocity value from one unit of measurement to another.  This
# conversion currently recognizes the following units of measurement:<ul>
#   <li>ft/s  - Feet per Second</li>
#   <li>km/hr - Kilometers per Hour</li>
#   <li>knot  - Knots</li>
#   <li>m/s   - Meters per Second</li>
#   <li>mi/hr - Miles per Hour</li>
#   <li>mph   - Miles per Hour</li>
# </ul></p>
#
# @input $value The initial velocity value.
# @input $in The initial velocity units.
# @input $out The target velocity units.
# @output $vel The velocity value in the output units.
# @warning This function will die if it receives a velocity unit that it
# does not recognize.
##--------------------------------------------------------------------------
sub convertVelocity {
  if ($_[0] eq getMissing()) { return getMissing(); }
  my %ins = ("ft/s"  => sub { return $_[0] * .3048; }, 
	     "km/hr" => sub { return $_[0] / 3.6; },
	     "knot"  => sub { return $_[0] * 463 / 900; },
	     "m/s"   => sub { return $_[0]; },
	     "mi/hr" => sub { return $_[0] * (1609.344 / 3600); },
	     "mph"   => sub { return $_[0] * (1609.344 / 3600); });
  my %outs= ("ft/s"  => sub { return $_[0] / .3048; },
	     "km/hr" => sub { return $_[0] * 3.6; },
	     "knot"  => sub { return $_[0] * 900 / 463; },
	     "m/s"   => sub { return $_[0]; },
	     "mi/hr" => sub { return $_[0] * (3600 / 1609.344); },
	     "mph"   => sub { return $_[0] * (3600 / 1609.344); });

  if (!defined($ins{$_[1]})) {
    die("Velocity input unit of $_[1] not known.\n");
  }
  if (!defined($outs{$_[2]})) {
    die("Velocity output unit of $_[2] not known.\n");
  }
  return handleZero($outs{$_[2]}->($ins{$_[1]}->($_[0])));
}

##--------------------------------------------------------------------------
# @signature float convertVolume(float value, String in, String out)
# <p>Convert a volume value from on unit of measurement to another.  This
# conversion currently recognizes the following units:<ul>
#   <li>af    - Acre-Feet</li>
#   <li>cm3   - Cubic Centimeters</li>
#   <li>ft3   - Cubic Feet</li>
#   <li>gal   - Gallons</li>
#   <li>in3   - Cubic Inches</li>
#   <li>liter - Liters</li>
#   <li>m3    - Cubic Meters</li>
#   <li>ml    - Milliliters</li>
#   <li>oz    - Ounces</li>
#   <li>pt    - Pints</li>
#   <li>qt    - Quarts</li>
# </ul></p>
#
# @input $value The initial volume to be converted.
# @input $in The initial units of the value.
# @input $out The target units of the value.
# @output $volume The converted value to the target units.
# @warning This function will die if it receives a volume unit it does not
# recognize.
##--------------------------------------------------------------------------
sub convertVolume {
  if ($_[0] eq getMissing()) { return getMissing(); }
  my %ins = ("af"    => sub { 
	       return $_[0] * 43560 * convertLength(1,"ft","cm") ** 3; },
	     "cm3"   => sub { return $_[0]; },
	     "ft3"   => sub { return $_[0] * convertLength(1,"ft","cm") ** 3;},
	     "gal"   => sub { return $_[0] * (34628947 / 9148); },
	     "in3"   => sub { return $_[0] * convertLength(1,"in","cm") ** 3;},
	     "liter" => sub { return $_[0] * 1000; },
	     "m3"    => sub { return $_[0] * convertLength(1,"m","cm") ** 3; },
	     "ml"    => sub { return $_[0]; },
	     "oz"    => sub { return $_[0] * (34628947 / 1170944); },
	     "pt"    => sub { return $_[0] * (34628947 / 73184); },
	     "qt"    => sub { return $_[0] * (34628947 / 36592); });
  my %outs= ("af"    => sub { 
	       return $_[0] * convertLength(1,"cm","ft") ** 3 / 43560; },
	     "cm3"   => sub { return $_[0]; },
	     "ft3"   => sub { return $_[0] * convertLength(1,"cm","ft") ** 3;},
	     "gal"   => sub { return $_[0] * (9148 / 34628947); },
	     "in3"   => sub { return $_[0] * convertLength(1,"cm","in") ** 3;},
	     "liter" => sub { return $_[0] / 1000; },
	     "m3"    => sub { return $_[0] * convertLength(1,"cm","m") ** 3; },
	     "ml"    => sub { return $_[0]; },
	     "oz"    => sub { return $_[0] * (1170944 / 34628947); },
	     "pt"    => sub { return $_[0] * (73184 / 34628947); },
	     "qt"    => sub { return $_[0] * (36592 / 34628947); });
  if (!defined($ins{$_[1]})) {
    die("Volume input unit of $_[1] not known.\n");
  }
  if (!defined($outs{$_[2]})) {
    die("Volume output unit of $_[2] not known.\n");
  }
  return handleZero($outs{$_[2]}->($ins{$_[1]}->($_[0])));
}

##--------------------------------------------------------------------------
# @signature int daysInFeb(int year)
# <p>Get the number of days in February for the specified year.</p>
#
# @input $year The year to find the number of days in February.
# @output $days The number of days in February for the specified year.
##--------------------------------------------------------------------------
sub daysInFeb {
  if ((($_[0] % 4 == 0) && ($_[0] % 100 != 0)) || ($_[0] % 400 == 0)) {
    return 29;
  } else {
    return 28;
  }
}

##--------------------------------------------------------------------------
# @signature float degToRad(float deg)
# <p>Convert an angle measured in degrees to radians.</p>
# @input $deg The angle measured in degrees.
# @output $rad The angle converted to radians.
##--------------------------------------------------------------------------
sub degToRad { return $_[0] * pi() / 180; }

##--------------------------------------------------------------------------
# @signature String formatDate(String date, String fmt)
# <p>Convert a date in the specified format to the "YYYY/MM/DD" format.</p>
# @input $date The date to be formated.
# @input $fmt The format of the specified date.
# @output $date The date formated in the "YYYY/MM/DD" format.
##--------------------------------------------------------------------------
sub formatDate {
  my $date = $_[0];
  my $fmt = $_[1];

  if (length($date) != length($fmt)) {
    die(sprintf("Length of date (%s) is not the same as the length of the format (%s) in formating the date.\n", $date, $fmt));
  }

  my $year = "0"; my $month = "0"; my $day = "0"; my $julian = "0";

  for (my $index = 0; $index < length($fmt); $index++) {
    my $fmt_char = uc(substr($fmt, $index, 1));
    if ($fmt_char eq "Y") { $year .= substr($date, $index, 1); }
    elsif ($fmt_char eq "M") { $month .= substr($date, $index, 1); }
    elsif ($fmt_char eq "D") { $day .= substr($date, $index, 1); }
    elsif ($fmt_char eq "J") { $julian .= substr($date, $index, 1); }
  }

# if ($year < 1000) { die(sprintf("Year (%d) must be four digits\n", $year)); }

  if ($julian != 0) { ($month, $day) = convertJulian($year, $julian); }

  return sprintf("%04d/%02d/%02d", $year, $month, $day);
}

##--------------------------------------------------------------------------
# @signature String formatTime(String time, String fmt)
# <p>Format a time in the specified format to the "HH:MM" format.</p>
# @input $time The time to be formatted.
# @input $fmt The format of the time.
# @output $time The time formatted in the "HH:MM" format.
##--------------------------------------------------------------------------
sub formatTime {
  my $time = $_[0];
  my $fmt = $_[1];
  
  if (length($time) != length($fmt)) {
    die(sprintf("Length of time (%s) is not the same as the length of the format (%s) in formating the time.\n", $time, $fmt));
  }

  my $hour = "0"; my $min = "0";

  for(my $index = 0; $index < length($fmt); $index++) {
    my $fmt_char = uc(substr($fmt, $index, 1));
    if ($fmt_char eq "H") { $hour .= substr($time, $index, 1); }
    elsif ($fmt_char eq "M") { $min .= substr($time, $index, 1); }
  }

  return sprintf("%02d:%02d", $hour, $min);
}

##--------------------------------------------------------------------------
# @signature (String date, String time) getDateFromSeconds(int seconds)
# <p>Convert a Unix time in seconds past Jan 1 1970 to a date and time.</p>
#
# @input seconds The Unix time in seconds past Jan 1 1970.
# @output date The date of the Unix time in YYYY/MM/DD format.
# @output time The time of the Unix time in HH:MM format.
##--------------------------------------------------------------------------
sub getDateFromSeconds {
  # Parse out the time pieces from the provided seconds.
  my ($day_of_week, $month, $day, $hour, $min, $sec, $year) =
    split(' ', strftime("%a %b %e %H %M %S %Y", gmtime($_[0])));

  # Get the number of the month
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

  return (sprintf("%04d/%02d/%02d", $year, $month, $day),
	  sprintf("%02d:%02d", $hour, $min));
}

##--------------------------------------------------------------------------
# @signature String getMissing()
# <p>Get the QCF missing value.</p>
#
# @output Return the QCF missing value of "-999.99"
##--------------------------------------------------------------------------
sub getMissing { return "-999.99"; }

##--------------------------------------------------------------------------
# @signature int getStateCode(String abbrev)
# <p>Get the state code for the specified state.</p>
# @input $abbrev The state's abbreviation.
# @output $code The specified state's code.
# @warning This function will die if it receives an abbreviation it does not
# recognize.
##--------------------------------------------------------------------------
##--------------------------------------------------------------------------
# @signature String getStateCode(int code)
# <p>Get the state abbreviation for the specified code.</p>
# @input $code The state code.
# @output $abbrev The state's abbreviation for the code.
# @warning This function will die if it receives a code it does not recognize.
##--------------------------------------------------------------------------
##--------------------------------------------------------------------------
# @signature int getStateCode(String abbrev, int web)
# <p>Get the state code for the specified state.</p>
# @input $abbrev The state's abbreviation.
# @input $web A flag saying if the function call came from a web script.  It
# prevents the function call from dying if it does not recognize the input 
# value.
# @output $code The specified state's code.
##--------------------------------------------------------------------------
##--------------------------------------------------------------------------
# @signature String getStateCode(int code, int web)
# <p>Get the state abbreviation for the specified code.</p>
# @input $code The state code.
# @input $web A flag saying if the function call came from a web script.  It
# prevents the function call from dying if it does not recognize the input 
# value.
# @output $abbrev The state's abbreviation for the code.
##--------------------------------------------------------------------------
sub getStateCode {
  my %codes;
  if (!defined($_[1])) { $_[1] = 0; }

  # Open the file and get the correct mappings
  open(FILE, "/work/DPG_HTML/BEST_SW/conversion_modules/Version2/states.list")
      || die("Cannot open the states.list file\n");
  while(<FILE>) {
    $_ =~ m/^(.*),(.*)$/;
    my $code = $1;
    my $abbr = $2;

    if ($_[0] =~ m/\d+/) { $codes{sprintf("%02d", $code)} = $abbr; }
    else { $codes{$abbr} = sprintf("%02d", $code); }
  }
  close(FILE);
  
  # Return the correct value.
  if (defined($codes{$_[0]})) { return $codes{$_[0]}; }
  else {
    if ($_[1]) { 
      printf(STDOUT "The state/code of %s is not known\n", $_[0]); 
      return "";
    } else { die(sprintf("The state/code of %s is not known\n", $_[0])); }
  }
}

##--------------------------------------------------------------------------
# @signature float handleZero(float value)
# <p>Handle rounding problems for values near zero.</p>
#
# @input The value to be checked.
# @output The adjusted value.
##--------------------------------------------------------------------------
sub handleZero {
  if ($_[0] <= 0 && $_[0] > -0.005) { return 0; }
  return $_[0];
}

##--------------------------------------------------------------------------
# @signature float pi()
# <p>Get the value of PI ~ 3.14159265.</p>
# @output $pi The value of pi.
##--------------------------------------------------------------------------
sub pi { return 4 * atan2(1, 1); }

##--------------------------------------------------------------------------
# @signature float radToDeg(float rad)
# <p>Convert an angle in radians to degrees.</p>
# @input $rad The angle measured in radians.
# @output $deg The angle converted to degrees.
##--------------------------------------------------------------------------
sub radToDeg { return $_[0] * 180 / pi(); }

##--------------------------------------------------------------------------
# @signature int validDate(String date)
# <p>Determine if a date in YYYY/MM/DD format is a real date.</p>
#
# @input $date The date to check in YYYY/MM/DD format.
# @output $valid 1 if the specified date is a valid date, 0 otherwise.
# @limitation The format of $date must be in YYYY/MM/DD format.
##--------------------------------------------------------------------------
sub validDate {
  if ($_[0] !~ /^[0-9]{4}\/[0-9]{2}\/[0-9]{2}$/) { return 0; }

  my ($year, $month, $day) = split('/', $_[0]);

  # Check if valid month
  if ($month < 1 || $month > 12) { return 0; }

  # Check if valid day for the given month
  my @daysInMonth = (31,daysInFeb($year),31,30,31,30,31,31,30,31,30,31);
  if ($day < 1 || $day > $daysInMonth[$month - 1]) { return 0; }

  return 1;
}

##--------------------------------------------------------------------------
# @signature int validTime(String time)
# <p>Determine if a time in HH:MM format is a real time.</p>
#
# @input $time The time to check in HH:MM format.
# @output $valid 1 if the specified time is a valid time, 0 otherwise.
# @limitation The format of $time must be in HH:MM format.
##--------------------------------------------------------------------------
sub validTime {
  if ($_[0] !~ /^[0-9]{2}:[0-9]{2}$/) { return 0; }

  my $hour = substr($_[0], 0, 2);
  my $min = substr($_[0], 3, 2);

  if (0 <= $hour && $hour <= 23 && 0 <= $min && $min <= 59) {
    return 1;
  } else {
    return 0;
  }
}

1; # Needed to return from a script call
