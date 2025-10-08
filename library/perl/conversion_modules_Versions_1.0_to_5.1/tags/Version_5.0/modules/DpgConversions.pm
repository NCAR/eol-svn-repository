#! /usr/bin/perl -w

##Module--------------------------------------------------------------------
# <p>The DpgConversions.pm module is a collection of functions that convert
# values from one unit of measurement to another.</p>
#
# <p>In previous versions, this was part of the Conversions.pm module.  It has
# had the date functions, the constants, and the calculations removed from it
# to maintain a level of encapsulation among the functions.</p>
#
# <p>There exists a testing suite for the DpgConversions.pm functions.  They
# are located in the TestDpgConversions.pl script.  It should be rerun after
# changes to the module have been made to ensure that the functions still work
# correctly.  New test should also be added when new functionality is added
# to the module</p>
#
# @use   use lib "/work/DPG_HTML/BEST_SW/conversion_modules/Version3";
#        use DpgConversions;
# @use   $return_value = DpgConversions::functionsName(param_list);
#
# @author Joel Clawson
# @version 5.0 <p>Added in the country for the state code.  This is a mandatory
# parameter to the function so it knows which file it should look in to find
# the state.</p>
#
# @author Joel Clawson
# @version 3.0 <p>The convertLatLong function was given the abiltiy to 
# convert to a degree minute output.  This is because the sounding data has
# output for latitudes and longitudes in degrees and minutes.</p>
# <p>The missing value checks on the conversions were removed.  This is due
# to different formats have different missing values.  It will now attempt to
# convert any value being passed to the function.  Any missing checks should
# be handled by the caller.  Undefined values will be the universal missing and
# will return the undefined value.</p>
# <p>Checks on the parameter lists were also added.  This is to try to help
# find errors quicker by preventing undefined values being passed to a function.
# It still does not check the values, only ensures that a correct number of 
# parameters are passed to the function.</p>
#
# @author Joel Clawson
# @version 2.0 No changes documented.
#
# @author Joel Clawson
# @version 0.01 Original Version
##Module--------------------------------------------------------------------
package DpgConversions;
use strict;
use lib ".";
use Exporter;
use DpgConstants qw(:DEFAULT);
our @ISA = ("Exporter");
our @EXPORT = qw(convertArea convertAngle convertFlow convertLatLong convertLength convertPressure convertRadiation convertTemperature convertTime convertVelocity convertVolume getStateCodeFromFile);

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
    if (scalar(@_) != 3) {
	die("Invalid parameters passed to convertArea\n");
    }

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
    return ($outs{$_[2]}->($ins{$_[1]}->($_[0])));
}

##--------------------------------------------------------------------------
# @signature float convertAngles(float value, String in, String out)
# <p>Convert an angle value from one unit of measurement to another.  This
# function currently recognizes the following units:<ul>
#   <li>deg - degrees</li>
#   <li>rad - radians</li>
# </ul></p>
#
# @input $value The initial angle value.
# @input $in The initial angle units.
# @input $out The target angle units.
# @output $angle The angle value converted to the target units.
# @warning This function will die if it receives a unit of angle that it does
# not recognize.
##--------------------------------------------------------------------------
sub convertAngle {
    if (scalar(@_) != 3) {
	die("Invalid parameters passed to convertAngle\n");
    }

    my %ins = ("deg" => sub { return $_[0] * $PI / 180; },
	       "rad"  => sub { return $_[0]; });
    my %outs= ("deg" => sub { return $_[0] * 180 / $PI; },
	       "rad"  => sub { return $_[0]; });
    if (!defined($ins{$_[1]})) {
	die("Angle input unit of $_[1] not known.\n");
    }
    if (!defined($outs{$_[2]})) {
	die("Angle output unit of $_[2] not known.\n");
    }
    return ($outs{$_[2]}->($ins{$_[1]}->($_[0])));
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
    if (scalar(@_) != 3) {
	die("Invalid parameters passed to convertFlow\n");
    }

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
    return ($outs{$_[2]}->($ins{$_[1]}->($_[0])));
}

##--------------------------------------------------------------------------
# @signature (Number degs, int mins, int secs) convertLatLong(String value, String format, String out_format)
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
#   <li>DM: Degrees Minutes</li>
#   <li>DMS: Degrees Minutes Seconds</li></ul></p>
#
# @input $value The latitude/longitude being converted.
# @input $format The original format of the latitude/longitude value.
# @input $out_format A 'D' or 'DMS' value of the output format
# of the latitude/longitude.  If omitted, it will use the default of 'D'.
# @output $degs The number of degrees of the latitude/longitude value.
# @output $mins The number of minutes of the latitude/longitude value.  Only
# used if 'DM' or 'DMS' is out_format.
# @output $secs The number of seconds of the latitued/longitude value.  Only
# used if 'DMS' is out_format.
# @warning This function will die if the length of the value is not the same
# as the length of the format or if the out_format is not one of 'D', 'DM' or 'DMS'.
##--------------------------------------------------------------------------
sub convertLatLong {
    if (scalar(@_) != 3) {
	die("Invalid parameters to convertLatLong.\n");
    }

    # Ensure that the format matches the value
    if (length($_[0]) != length($_[1])) {
	die("Length of $_[0] is not the same as the length of $_[1] to convert ".
	    "latitude/longitude\n");
    }

    # Make sure the output format is an expected value.
    if ($_[2] ne "D" && $_[2] ne "DM" && $_[2] ne "DMS") {
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

    if ($mins == 0 && $secs == 0) {
	my $total = $degs + $mins / 60 + $secs / 3600;
	$degs = int($total);
	$mins = int(($total - $degs) * 60.0);
	$secs = ($total - $degs - ($mins / 60.0)) * 3600.0;
    }

    if ($_[2] =~ /s/i) {
	return ($mult * $degs,$mins,$secs);
    } elsif ($_[2] =~ /m/i) {
	return ($mult * $degs,$mins + $secs / 60,0);
    } else {
	return ($mult * ($degs + $mins / 60 + $secs / 3600), 0,0);
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
    if (scalar(@_) != 3) {
	die("Invalid parameters passed to convertLength\n");
    }

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
    return ($outs{$_[2]}->($ins{$_[1]}->($_[0])));
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
    if (scalar(@_) != 3) {
	die("Invalid parameters passed to convertPressure\n");
    }

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
    return ($outs{$_[2]}->($ins{$_[1]}->($_[0])));
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
    if (scalar(@_) != 3) {
	die("Invalid parameters passed to convertRadiation\n");
    }

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
    return ($outs{$_[2]}->($ins{$_[1]}->($_[0])));
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
    if (scalar(@_) != 3) {
	die("Invalid parameters passed to convertTemperature\n");
    }
    
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
    return ($outs{$_[2]}->($ins{$_[1]}->($_[0])));
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
    if (scalar(@_) != 3) {
	die("Invalid parameters passed to convertTime\n");
    }

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
    return ($outs{$_[2]}->($ins{$_[1]}->($_[0])));
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
    if (scalar(@_) != 3) {
	die("Invalid parameters passed to convertVelocity\n");
    }

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
    return ($outs{$_[2]}->($ins{$_[1]}->($_[0])));
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
    if (scalar(@_) != 3) {
	die("Invalid parameters passed to convertVolume\n");
    }

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
    return ($outs{$_[2]}->($ins{$_[1]}->($_[0])));
}

##--------------------------------------------------------------------------
# @signature int getStateCodeFromFile(String abbrev, String country)
# <p>Get the state code for the specified state.</p>
# @input $abbrev The state's abbreviation.
# @input $country The country for the code.
# @output $code The specified state's code.
# @warning This function will die if it receives an abbreviation it does not
# recognize.
##--------------------------------------------------------------------------
##--------------------------------------------------------------------------
# @signature String getStateCodeFromFile(int code, String country)
# <p>Get the state abbreviation for the specified code.</p>
# @input $code The state code.
# @input $country The country for the code.
# @output $abbrev The state's abbreviation for the code.
# @warning This function will die if it receives a code it does not recognize.
##--------------------------------------------------------------------------
##--------------------------------------------------------------------------
# @signature int getStateCodeFromFile(String abbrev, String country, int web)
# <p>Get the state code for the specified state.</p>
# @input $abbrev The state's abbreviation.
# @input $country The country for the abbreviation.
# @input $web A flag saying if the function call came from a web script.  It
# prevents the function call from dying if it does not recognize the input 
# value.
# @output $code The specified state's code.
##--------------------------------------------------------------------------
##--------------------------------------------------------------------------
# @signature String getStateCodeFromFile(int code, String country, int web)
# <p>Get the state abbreviation for the specified code.</p>
# @input $code The state code.
# @input $country The country for the code.
# @input $web A flag saying if the function call came from a web script.  It
# prevents the function call from dying if it does not recognize the input 
# value.
# @output $abbrev The state's abbreviation for the code.
##--------------------------------------------------------------------------
sub getStateCodeFromFile {
    if (scalar(@_) < 2 || scalar(@_) > 3) {
	die("Invalid parameters passed to getStateCodeFromFile\n");
    }
    my ($key,$country,$web) = @_;

    if (!defined($web)) { $web = 0; }
    my %codes;
    
    $key = sprintf("%02d",$key) if ($key =~ /^\d+$/);

    # Open the file and get the correct mappings
    open(FILE, sprintf("/work/software/conversion_modules/Version5/%s.states",lc($country)))
	|| die(sprintf("Cannot open the %s.states file\n",lc($country)));
    while(<FILE>) {
	$_ =~ m/^(.*),(.*)$/;
	my $code = $1;
	my $abbr = $2;

	if ($key =~ m/\d+/) { $codes{sprintf("%02d", $code)} = $abbr; 
	} else { $codes{$abbr} = sprintf("%02d", $code); }
    }
    close(FILE);
    
    # Return the correct value.
    if (defined($codes{$key})) { return $codes{$key}; }
    else {
	if ($_[2]) { 
	    printf(STDOUT "The %s state/code of %s is not known\n",$country,$key); 
	    return "";
	} else { die(sprintf("The %s state/code of %s is not known\n",$country,$key)); }
    }
}

1; # Needed to return from a script call
