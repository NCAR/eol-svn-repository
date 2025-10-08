#! /usr/bin/perl -w

##Module--------------------------------------------------------------------
# <p>The conversions.cgi is a cgi script for displaying a form to perform
# calculations and conversions that are found in the
# <a href="http://www.joss.ucar.edu/dpg/TOOLS/perldoc.cgi?file=">
# Conversions Module</a>.</p>
# <p>The codes used to access the forms are:<ul>
# <li>Area - Convert an Area</li>
# <li>DewPoint - Calculate Dew Point</li>
# <li>Flow - Convert Flow</li>
# <li>LatLong - Convert Latitude or Longitude</li>
# <li>Length - Convert a Length or Distance</li>
# <li>JulianDay - Convert to or from Julian Date</li>
# <li>Pressure - Convert a Pressure</li>
# <li>RelHumid - Calculate Relative Humidity</li>
# <li>SolarRad - Convert a Solar Radiation</li>
# <li>SLPressure - Calculate a Sea Level Pressure</li>
# <li>SpecHumid - Calculate the Specific Humidity</li>
# <li>StateCode - Calculate a state code from state or state from state code</li>
# <li>StnPressure - Calculate Station Pressure</li>
# <li>Temperature - Convert a Temperature</li>
# <li>Time - Convert a period of Time</li>
# <li>UVwind - Convert UV to Wind</li>
# <li>Velocity - Convert a Velocity / Speed</li>
# <li>Volume - Convert a Volume</li>
# <li>WindUV - Convert Wind to UV</li>
#
# @author Joel Clawson
# @version 1.0 Original Creation
##Module--------------------------------------------------------------------
use strict;
use lib "/work/DPG_HTML/BEST_SW/conversion_modules/Version1";
use CGI qw(:standard :html3);
use Conversions;

my $cgi = CGI->new();
# Pipes the STDERR to STDOUT so error messages are displayed on the web page
*STDERR = *STDOUT; 

# A hash of convert codes to functions.
my %functions = ("Area"        => \&area,
		 "DewPoint"    => \&dewpoint,
		 "Flow"        => \&flow,
		 "LatLong"     => \&latlong,
		 "Length"      => \&length_convert,
		 "JulianDay"   => \&julianday,
		 "Pressure"    => \&pressure,
		 "Radiation"   => \&radiation,
		 "RelHumid"    => \&relhumidity,
		 "SLPressure"  => \&sealevelpressure,
		 "SpecHumid"   => \&spechumid,
		 "StateCode"   => \&statecode,
		 "StnPressure" => \&stnpressure,
		 "Temperature" => \&temperature,
		 "Time"        => \&time,
		 "UVwind"      => \&uvwind,
		 "Velocity"    => \&velocity,
		 "Volume"      => \&volume,
		 "WindUV"      => \&winduv);

main();

##------------------------------------------------------------------------
# @signature void main()
# <p>Create the web page holding a form.
##------------------------------------------------------------------------
sub main {
    print $cgi->header();
    print "<html><head><title>Conversions</title>\n";
    print "<link rel=STYLESHEET type=text/css href=";
    print "http://www.joss.ucar.edu/~suldan/qcodiac/main.css>\n";
    print "</head><body bgcolor=#e9e9e9 alink=black vlink=black link=black>\n";
    print "<form method=POST ";
    print "action=/cgi-bin/dpg/conversions/conversions.cgi?Convert=";
    print $cgi->param("Convert");
    print " enctype=application/x-www-form-urlencoded>\n";
    $functions{$cgi->param("Convert")}();
    print $cgi->hidden("Convert", $cgi->param("Convert"))."\n";
    print "</form>";
    print $cgi->end_html();
}

##--------------------------------------------------------------------------
# @signature void area()
# <p>Create the form to convert area values.</p>
##--------------------------------------------------------------------------
sub area {
    # Create the heading and instructions.
    print "<h1>Area</h1>\n";
    print "<p>The area conversion takes a value in one unit of area and ";
    print "converts it to another unit of area.</p>\n";

    # Create the form
    print "<table><tr><td>Area:</td><td>".$cgi->textfield("area");
    print "</td><td>".$cgi->popup_menu(-name=>"areaInUnits", -values=>["acre", "mile2", "km2", "m2", "ft2", "in2", "cm2", "mm2", "yd2"]);
    print "</td></tr>\n<tr><td>Area Output Units:</td><td>";
    print $cgi->popup_menu(-name=>"areaOutUnits", -values=>["acre", "mile2", "km2", "m2", "ft2", "in2", "cm2", "mm2", "yd2"]);
    print "</td><td>".$cgi->submit("Convert Area")."</td></tr></table>\n<br>";

    # Default values for the results table.
    my $result_title = "&nbsp;";
    my $result_value = "&nbsp;";

    # Convert the Area
    if (defined($cgi->param('area')) && isNumber($cgi->param('area'))) {
	$result_title = "Results (Full float):";
	my $value = Conversions::convertArea($cgi->param("area"),
					     $cgi->param("areaInUnits"),
					     $cgi->param("areaOutUnits"));
	$result_value = sprintf("<font size=+1>%f</font> %s = <font size=+1>%f</font> %s", $cgi->param("area"), $cgi->param("areaInUnits"), $value,
				$cgi->param("areaOutUnits"));
    } elsif (defined($cgi->param('area')) && !isNumber($cgi->param('area'))) {
	$result_value = sprintf("Area %s is not a number.",
				$cgi->param('area'));
    }

    # Build the results table.
    print "<table><tr><td><font size=+1><b>$result_title</b></font></td>\n";
    print "<td><b><i>$result_value</i></b></td></tr></table>\n<br>\n";

    # Build the HTML table containing the simple conversions and abbrevs.
    print "<table border=5 width=100%><tr bgcolor=#336699>";
    print "<th><font color=white size=+1>Simple Conversions</font></th>\n";
    print "<th><font color=white size=+1>Abbreviations</th></font></tr><td>\n";
    print "<table align=center>\n";
    print "<tr><td>1 mile<sup>2</sup> = 640 acre</td></tr>\n";
    print "<tr><td>1 ft<sup>2</sup> = 144 in<sup>2</sup></td></tr>\n";
    print "<tr><td>1 yd<sup>2</sup> = 9 ft<sup>2</sup></td></tr>\n";
    print "<tr><td>1 acre = 43560 ft<sup>2</sup> = 4840 yd<sup>2</sup></td></tr>\n";
    print "<tr><td>1 km<sup>2</sup> = 1000000 m<sup>2</sup></td></tr>\n";
    print "<tr><td>1 m<sup>2</sup> = 10000 cm<sup>2</sup></td></tr>\n";
    print "<tr><td>1 cm<sup>2</sup> = 100 mm<sup>2</sup></td></tr>\n";
    print "<tr><td>1 ft<sup>2</sup> = (.3048)<sup>2</sup> m<sup>2</sup> = .09290 m<sup>2</sup></td></tr>\n";
    print "<tr><td>1 mile<sup>2</sup> = (1.609344)<sup>2</sup> km<sup>2</sup> = 2.58999 km<sup>2</sup></td></tr>\n";
    print "</table></td><td><table align=center>\n";
    print "<tr><td>acre</td><td>=</td><td>acres</td></tr>\n";
    print "<tr><td>cm<sup>2</sup></td><td>=</td><td>square centimeters</td></tr>\n";
    print "<tr><td>ft<sup>2</sup></td><td>=</td><td>square feet</td></tr>\n";
    print "<tr><td>in<sup>2</sup></td><td>=</td><td>square inches</td></tr>\n";
    print "<tr><td>km<sup>2</sup></td><td>=</td><td>square kilometers</td></tr>\n";
    print "<tr><td>m<sup>2</sup></td><td>=</td><td>square meters</td></tr>\n";
    print "<tr><td>mile<sup>2</sup></td><td>=</td><td>square miles</td></tr>\n";
    print "<tr><td>mm<sup>2</sup></td><td>=</td><td>square millimeters</td></tr>\n";
    print "<tr><td>yd</sup>2</sup></td><td>=</td><td>square yards</td></tr>\n";

    print "</table></td></tr></table>\n";
}

##--------------------------------------------------------------------------
# @signature void dewpoint()
# <p>Creates the form for calculating the dew point.</p>.
##--------------------------------------------------------------------------
sub dewpoint {
    # Create the heading and instructions.
    print "<h1>Calculate Dew Point</h1>\n";
    print "<p>The dew point calculator calculates the dew point from the ";
    print "temperature and relative humidity.</p>";
    print "<p><font color=RED size=+1>Warning: </font>The calculation \n";
    print "converts the Temperature to &deg;C and Relative Humidty to % ";
    print "before performing any of the calculation.</p>\n";

    # Create the form
    print "<table><tr><td>Temperature:</td><td>";
    print $cgi->textfield('tempDP')."</td><td>";
    print $cgi->popup_menu(-name=>"dpTempUnits", -values=>["C", "F", "K"]);
    print "</td></tr>\n<tr><td>Relative Humidity:</td><td>";
    print $cgi->textfield('rhDP')."</td><td>";
    print $cgi->popup_menu(-name=>"dpRhUnits", -values=>["%", "Decimal"]);
    print "</td></tr>\n<tr><td>Dew Point Output Units:</td><td>";
    print $cgi->popup_menu(-name=>"dpOutUnits", -values=>["C", "F", "K"]);
    print "</td><td>".$cgi->submit("Calculate Dew Point")."</td></tr>\n";
    print "</table>";

    # Default values for the result table
    my $result_qcf_title = "&nbsp;";
    my $result_full_title = "&nbsp;";
    my $qcf_converted = "&nbsp;";
    my $full_converted = "&nbsp;";

    # Calculate the dew point.
    if (defined($cgi->param('tempDP')) && defined($cgi->param('rhDP')) &&
	isNumber($cgi->param('tempDP')) && isNumber($cgi->param('rhDP'))) {
	my $rh = $cgi->param("rhDP");
	if ($cgi->param("dpRhUnits") eq "Decimal") { $rh *= 100; }
	$result_qcf_title = "Results (QCF 7.2 float)";
	$result_full_title = "Results (Full float)";
	my $value = Conversions::convertTemperature(Conversions::calculateDewPoint(Conversions::convertTemperature($cgi->param('tempDP'), $cgi->param('dpTempUnits'), "C"), $rh, 1), "C", $cgi->param("dpOutUnits")), $cgi->param('dpOutUnits');
	$qcf_converted = sprintf("<font size=+1>%7.2f</font> &deg;%s",
				 $value, $cgi->param('dpOutUnits'));
	$full_converted = sprintf("<font size=+1>%f</font> &deg;%s",
				 $value, $cgi->param('dpOutUnits'));      
    } elsif (defined($cgi->param('tempDP')) && 
	     !isNumber($cgi->param('tempDP'))){
	$qcf_converted = sprintf("Temperature %s is not a number.",
				 $cgi->param('tempDP'));
    } elsif (defined($cgi->param('rhDP')) && !isNumber($cgi->param('rhDP'))) {
	$qcf_converted = sprintf("Relative Humidity %s is not a number.",
				 $cgi->param('rhDP'));	
    }

    # Build the results table.
    print "<br><table><tr><td><font size=+1><b>";
    print $result_qcf_title."</b></font></td><td><b><i>";
    print $qcf_converted."</i></b></td></tr>\n<tr><td><font size=+1><b>";
    print $result_full_title."</b></font></td>\n<td><b><i>";
    print $full_converted."</i></b></td></tr></table><br>\n";


    # Build the HTML table containing the results
    print "<table border=5 width=100%><tr bgcolor=#336699>";
    print "<th><font color=white size=+1>Calculation Used</th></font></tr>\n";
    print "<td>sat_vapor_pressure = 6.112 * exp((17.67 * temperature)";
    print "/ (temperature + 243.5))<br>\n";
    print "vapor_pressure = sat_vapor_pressure * (rel_humidity / 100.0)<br>\n";
    print "log_value = log(vapor_pressure / 6.112)<br>\n";
    print "dew_point = log_value * 243.5 / (17.67 - log_value)</td></table>\n";
}

sub flow {
    # Create the heading and the instructions.
    print "<h1>Flow</h1>\n";
    print "<p>Convert a flow value from one unit of flow to another.</p>\n";

    # Build the form for entering values.
    print "<table><tr><td>Flow:</td><td>".$cgi->textfield("flow");
    print "</td><td>".$cgi->popup_menu(-name=>"flowInUnits", 
				       -values=>["ft3/s","m3/s"]);
    print "</td></tr>\n<tr><td>Flow Output Units:</td><td>";
    print $cgi->popup_menu(-name=>"flowOutUnits", -values=>["m3/s","ft3/s"]);
    print "</td><td>".$cgi->submit("Convert Flow")."</td></tr></table>\n";

    # Default values for the results table.
    my $full_title = "&nbsp;";
    my $full_value = "&nbsp;";
    my $qcf_title = "&nbsp;";
    my $qcf_value = "&nbsp;";

    # Convert the flow
    if (defined($cgi->param('flow')) && isNumber($cgi->param('flow'))) {
	$full_title = "Results (Full float):";
	$qcf_title = "Results (QCF 7.2 float):";
	my $value = Conversions::convertFlow($cgi->param("flow"),
					     $cgi->param("flowInUnits"),
					     $cgi->param("flowOutUnits"));
	$full_value = sprintf("<font size=+1>%f</font> %s = <font size=+1>%f</font> %s", $cgi->param("flow"), $cgi->param("flowInUnits"), $value,
			      $cgi->param("flowOutUnits"));
	$qcf_value = sprintf("<font size=+1>%7.2f</font> %s = <font size=+1>%7.2f</font> %s", $cgi->param("flow"), $cgi->param("flowInUnits"), $value,
			      $cgi->param("flowOutUnits"));
    } elsif (defined($cgi->param('flow'))) {
	$qcf_value = sprintf("Flow value %s is not a number.",
			     $cgi->param('flow'));
    }

    # Create the results table
    print "<table><tr><td><b><font size=+1>$qcf_title</font></b></td><td>";
    print "<b><i>$qcf_value</i></b></td></tr>\n";
    print "<tr><td><b><font size=+1>$full_title</font></b></td><td>";
    print "<b><i>$full_value</i></b></td></tr></table>\n";

    # Create the conversions and abbreviations table.
    print "<table border=5 width=100%><tr bgcolor=#336699>";
    print "<th><font color=white size=+1>Simple Conversions</font></th>\n";
    print "<th><font color=white size=+1>Abbreviations</th></font></tr><td>\n";
    print "<table align=center>\n";
    print "<tr><td>1 ft&sup3;/s = (.3048)&sup3; m&sup3;/s = .0283168 m&sup3;/s</td></tr>\n";
    print "</table></td><td><table align=center>\n";
    print "<tr><td>ft&sup3;/s</td><td>=</td><td>cubic feet per second</td></tr>\n";
    print "<tr><td>m&sup3;/s</td><td>=</td><td>cubic meters per second</td></tr>\n";
    print "</table></td></table>\n";

}

##-------------------------------------------------------------------------
# @signature int isNumber(String value)
# <p>Check to see if a string is a number.</p>
#
# @input $value The value to check to see if it is a number.
# @output $retVal Returns 1 if the value is a number, 0 otherwise.
##-------------------------------------------------------------------------
sub isNumber {
    my $value = shift;
    if ($value =~ m/^-?([0-9]+[\.]?[0-9]*|[0-9]*[\.]?[0-9]+)$/) {
	return 1;
    } else {
	return 0;
    }
}

##-------------------------------------------------------------------------
# @signature void latlong()
# <p>Build the form for converting latitude and longitude values.</p>
##-------------------------------------------------------------------------
sub latlong {
    # Create the heading and instructions.
    print "<h1>Latitude / Longitude</h1>\n";
    print "<p>Convert a DMS latitude or longitude value to a decimal value or";
    print " a decimal latitude or longitude value to a DMS value</p>";
    print "<p>The conversion assumes that a degree only value to be converted";
    print " to a DMS value and a DMS value to decimal.</p>\n<table>";

    # Create the form to enter the lat/long value
    print "<tr><td>".$cgi->textfield("lDeg")." &deg;</td></tr>\n";
    print "<tr><td>".$cgi->textfield("lMin")." '</td></tr>\n";
    print "<tr><td>".$cgi->textfield("lSec")." \"</td></tr>\n";
    print "<tr><td>".$cgi->submit("Convert", "LatLong")."</td></tr></table>";

    # Default values for the result table.
    my $result_dec_title = "&nbsp;";
    my $result_dms_title = "&nbsp;";
    my $dec_converted = "&nbsp;";
    my $dms_converted = "&nbsp;";	

    # Only do the conversion if the degree value is a number.
    if (defined($cgi->param('lDeg')) && isNumber($cgi->param('lDeg')) &&
	($cgi->param('lMin') eq "" || isNumber($cgi->param('lMin'))) &&
	($cgi->param('lSec') eq "" || isNumber($cgi->param('lSec')))) {
	my $deg = $cgi->param("lDeg");
	my $min = $cgi->param("lMin");
	my $sec = $cgi->param("lSec");

	# Create the format for the conversion
	my $fmt = "";
	if ($deg < 0) {
	    $fmt = "-";
	} 
	while (length($fmt) < length($deg)) { $fmt = $fmt."D"; }
	while (length($fmt) < length($deg) + length($min)) { $fmt = $fmt."M"; }
	while (length($fmt) < length($deg) + length($min) + length($sec)) {
	    $fmt = $fmt."S";
	}

	$result_dec_title = "Decimal:";
	$result_dms_title = "DMS:";

	# Perform the conversion
	$dec_converted = sprintf("<font size=+1>%11.5f</font>",
			       Conversions::convertLatLong("$deg$min$sec",
							   $fmt, "D"));
	my $degree; my $minute; my $second;
	($degree, $minute, $second) = 
	  Conversions::convertLatLong("$deg$min$sec", $fmt, "DMS");
	$dms_converted = sprintf("<font size=+1>%d&deg; %d' %f\"",
				 $degree, $minute, $second);
    }

    # Build the HTML table containing the results
    print "<br><table><tr><td><font size=+1><b>";
    print $result_dec_title."</b></font></td><td><b><i>";
    print $dec_converted."</i></b></td></tr>\n<tr><td><font size=+1><b>";
    print $result_dms_title."</b></font></td>\n<td><b><i>";
    print $dms_converted."</i></b></td></tr></table><br>\n";
}

##-------------------------------------------------------------------------
# @signature void length_convert()
# <p>Builds the HTML page for doing a Length/Distance conversion on the 
# web.</p>
##-------------------------------------------------------------------------
sub length_convert {
    # Build the title and instructions.
    print "<h1>Length/Distance</h1>\n";
    print "<p>The length/distance conversion takes in a value in one unit\n";
    print "of length and converts it to another unit of length.</p>\n";

    # Build the Form to enter a length/distance.
    print "<table>\n";
    print "<tr><td>Length:</td><td>".$cgi->textfield("length")."</td><td>";
    print $cgi->popup_menu(-name=>"lenInUnits", -values=>["in","ft","mm","m","cm","km","mile","yd","nmile","dm","ti","hi"]);
    print "</td></tr>\n";
    print "<tr><td>Length Output Units:</td><td>";
    print $cgi->popup_menu(-name=>"lenOutUnits", -values=>["mm","m","in","ft","cm","km","mile","yd","nmile","dm","ti","hi"]);
    print "</td><td>".$cgi->submit("Convert", "Length")."</td></tr>";
    print "</table>\n";

    # Default values for the result table
    my $result_qcf_title = "&nbsp;";
    my $result_full_title = "&nbsp;";
    my $qcf_converted = "&nbsp;";
    my $full_converted = "&nbsp;";

    # If the provided length is a number instantiate the results.
    if (defined($cgi->param("length")) && isNumber($cgi->param("length"))) {
	$result_qcf_title = "Result (QCF 7.2 float):";
	$result_full_title = "Result (full float):";
	my $value = Conversions::convertLength($cgi->param("length"),
					       $cgi->param("lenInUnits"),
					       $cgi->param("lenOutUnits"));
	$qcf_converted = sprintf("<font size=+1>%7.2f</font> %s = <font size=+1>%7.2f</font> %s",
				 $cgi->param("length"), 
				 $cgi->param("lenInUnits"), $value,
				 $cgi->param("lenOutUnits"));
	$full_converted = sprintf("<font size=+1>%f</font> %s = <font size=+1>%f</font> %s",
				  $cgi->param("length"), 
				  $cgi->param("lenInUnits"), $value,
				  $cgi->param("lenOutUnits"));
    } elsif (defined($cgi->param("length"))) {
	$qcf_converted = sprintf("Length/Distance %s is not a number.",
				 $cgi->param("length"));
    }

    # Build the HTML table containing the results
    print "<br><table><tr><td><font size=+1><b>";
    print $result_qcf_title."</b></font></td><td><b><i>";
    print $qcf_converted."</i></b></td></tr>\n<tr><td><font size=+1><b>";
    print $result_full_title."</b></font></td>\n<td><b><i>";
    print $full_converted."</i></b></td></tr></table><br>\n";

    # Build the HTML table containing the simple conversions and abbrevs.
    print "<table border=5 width=100%><tr bgcolor=#336699>";
    print "<th><font color=white size=+1>Simple Conversions</font></th>\n";
    print "<th><font color=white size=+1>Abbreviations</th></font></tr><td>\n";
    print "<table align=center><tr><td>10 mm = 1 cm</td></tr>\n";
    print "<tr><td>100 cm = 10 dm = 1 m</td></tr>\n";
    print "<tr><td>1000 m = 1 km</td></tr>\n";
    print "<tr><td>100 hi = 10 ti = 1 in</td></tr>\n";
    print "<tr><td>12 in = 1 ft</td></tr>\n";
    print "<tr><td>36 in = 3 ft = 1 yd</td></tr>\n";
    print "<tr><td>5280 ft = 1760 yd = 1 mile</td></tr>\n";
    print "<tr><td>1 nmile = 1.150779 mile</td></tr>\n";
    print "<tr><td>25.4 mm = 2.54 cm = 1 in</td></tr>\n";
    print "<tr><td>.3048 ft = 1 m</td></tr>\n";
    print "<tr><td>1852 m = 1.852 km = 1 nmile</td></tr>\n";
    print "<tr><td>1609.344 m = 1.609344 km = 1 mile</td></tr></table></td>\n";
    print "<td><table align=center>\n";
    print "<tr><td>cm</td><td>=</td><td>centimeters</td></tr>\n";
    print "<tr><td>dm</td><td>=</td><td>decimeters</td></tr>\n";
    print "<tr><td>ft</td><td>=</td><td>feet</td></tr>\n";
    print "<tr><td>hi</td><td>=</td><td>hundredths of inches</td></tr>\n";
    print "<tr><td>in</td><td>=</td><td>inches</td></tr>\n";
    print "<tr><td>km</td><td>=</td><td>kilometers</td></tr>\n";
    print "<tr><td>m</td><td>=</td><td>meters</td></tr>\n";
    print "<tr><td>mile</td><td>=</td><td>miles</td></tr>\n";
    print "<tr><td>mm</td><td>=</td><td>millimeters</td></tr>\n";
    print "<tr><td>nmile</td><td>=</td><td>nautical miles</td></tr>\n";
    print "<tr><td>ti</td><td>=</td><td>tenths of inches</td></tr>\n";
    print "<tr><td>yd</td><td>=</td><td>yards</td></tr>\n";
    print "</table></td></table>\n";
}

##---------------------------------------------------------------------------
# @signature void julianday()
# <p>Create the form to convert julian days.</p>
##---------------------------------------------------------------------------
sub julianday {
    # Build the heading and instructions.
    print "<h1>Julian Day</h1>\n";
    print "<p>Convert a julian date to a standard form date, or a standard\n";
    print " from date to a julian date.  The year must be four digits.  The ";
    print "days and month can only be one or two digits.</p>\n";

    if (defined($cgi->param("Convert Standard Date"))) {
	$cgi->param("jdJYear", "");
	$cgi->param("jdJDay", "");
    } elsif (defined($cgi->param("Convert Julian Date"))) {
	$cgi->param("jdYear", "");
	$cgi->param("jdDay", "");
	$cgi->param("jdMonth", "");
    }

    # Build the form for converting dates.
    print "<table><tr><td><table>";
    print "<tr><td>Year:</td><td>".$cgi->textfield('jdJYear')."</td></tr>\n";
    print "<tr><td>Julian Day:</td><td>".$cgi->textfield("jdJDay")."</td>";
    print "</tr>\n<tr><td colspan=2>".$cgi->submit("Convert Julian Date");
    print "</td></tr></table></td>\n<td><table>";
    print "<tr><td>Year:</td><td>".$cgi->textfield('jdYear')."</td></tr>\n";
    print "<tr><td>Day:</td><td>".$cgi->textfield('jdDay')."</td></tr>\n";
    print "<tr><td>Month:</td><td>".$cgi->textfield('jdMonth')."</td></tr>\n";
    print "<tr><td colspan=2>".$cgi->submit("Convert Standard Date");
    print "</td></tr></table></td></tr></table>\n";

    # Default result values
    my $standard_title = "&nbsp;";
    my $julian_title = "&nbsp;";
    my $standard_date = "&nbsp;";
    my $julian_date = "&nbsp;";

    # Convert the date.
    if (defined($cgi->param('jdJYear')) && defined($cgi->param('jdJDay')) &&
	isNumber($cgi->param('jdJYear')) && isNumber($cgi->param('jdJDay'))) {
	$standard_title = "Standard Date:";
	$julian_title = "Julian Date:";
	my $month;
	my $day;
	($month, $day) = Conversions::convertJulian($cgi->param("jdJYear"),
						    $cgi->param("jdJDay"));
	while(length($month) < 2) { $month = "0".$month; }
	while(length($day) < 2) { $day = "0".$day; }
	$standard_date = sprintf("%s/%s/%s", 
				 $cgi->param("jdJYear"), $month, $day);
	$julian_date = sprintf("%s/%s", 
			       $cgi->param("jdJYear"), $cgi->param("jdJDay"));

	if (!Conversions::validDate($standard_date)) {
	    $standard_date = sprintf("%s is not a valid date.",
				     $julian_date);
	    $standard_title = "&nbsp;";
	    $julian_title = "&nbsp;";
	    $julian_date = "&nbsp;";
	}
    } elsif (defined($cgi->param('jdJYear')) && 
	     defined($cgi->param("Convert Julian Date")) &&
	     !isNumber($cgi->param('jdJYear'))) {
	$standard_date = sprintf("%s is not a valid year.",
				 $cgi->param("jdJYear"));
    } elsif (defined($cgi->param('jdJDay')) &&
	     defined($cgi->param("Convert Julian Date")) &&
	     !isNumber($cgi->param("jdJDay"))) {
	$standard_date = sprintf("%s is not a valid julian day",
				 $cgi->param("jdJDay"));
    } elsif (defined($cgi->param('jdYear')) && !isNumber($cgi->param("jdYear"))
	     && defined("Convert Standard Date")) {
	$standard_date = sprintf("%s is not a valid year.",
				 $cgi->param('jdYear'));
    } elsif (defined($cgi->param('jdMonth')) && 
	     !isNumber($cgi->param("jdMonth")) &&
	     defined($cgi->param("Convert Standard Date"))) {
	$standard_date = sprintf("%s is not a valid month.",
				 $cgi->param('jdMonth'));
    } elsif (defined($cgi->param("jdDay")) && !isNumber($cgi->param("jdDay"))
	     && defined("Convert Standard Date")) {
	$standard_date = sprintf("%s is not a valid day.",
				 $cgi->param('jdDay'));
    } elsif (defined($cgi->param('jdYear')) && isNumber($cgi->param('jdYear'))
	     && defined($cgi->param("jdDay")) && isNumber($cgi->param("jdDay"))
	     && defined($cgi->param("jdMonth")) && 
	     isNumber($cgi->param("jdMonth"))) {
	$standard_title = "Standard Date:";
	$julian_title = "Julian Date:";
	my $julian = Conversions::convertJulian($cgi->param("jdYear"),
						$cgi->param("jdDay"),
						$cgi->param("jdMonth"));
	my $day = $cgi->param("jdDay");
	my $month = $cgi->param("jdMonth");
	while (length($day) < 2) { $day = "0".$day; }
	while (length($month) < 2) { $month = "0".$month; }

	$standard_date = sprintf("%s/%s/%s",
				 $cgi->param("jdYear"), $month, $day);
	$julian_date = sprintf("%s/%s", 
			       $cgi->param("jdYear"), $julian);

	if (!Conversions::validDate($standard_date)) {
	    $standard_date = sprintf("%s is not a valid date.",
				     $standard_date);
	    $standard_title = "&nbsp;";
	    $julian_title = "&nbsp;";
	    $julian_date = "&nbsp;";
	}	
    }

    # Build the results table.
    print "<br><table>";
    print "<tr><td><font size=+1><b>".$standard_title."</b></font></td>\n";
    print "<td><b><i><font size=+1>".$standard_date."</font></i></b></td>";
    print "</tr>\n";
    print "<tr><td><font size=+1><b>".$julian_title."</b></font></td>\n";
    print "<td><b><i><font size=+1>".$julian_date."</font></i></b></td></tr>";
    print "</table>\n";
}

##---------------------------------------------------------------------------
# @signature void pressure()
# <p>Build the form for converting pressure values.</p>
##---------------------------------------------------------------------------
sub pressure {
    # Build the heading and the instructions.
    print "<h1>Pressure</h1>\n";
    print "<p>The pressure conversion takes in a value in one unit of \n";
    print "pressure and converts it to another unit of pressure.</p>\n";

    # Build the Form for converting pressure values.
    print "<table>\n";
    print "<tr><td>Pressure:</td><td>".$cgi->textfield("pressure");
    print "</td><td>";
    print $cgi->popup_menu(-name=>"psInUnits", -values=>["inHg","mb","mmHg","kPa","atm","bar","Pa","dyne/cm2"]);
    print "</td></tr>\n";
    print "<tr><td>Pressure Output Units:</td><td>";
    print $cgi->popup_menu(-name=>"psOutUnits", -values=>["mb","inHg","mmHg","kPa","atm","bar","Pa","dyne/cm2"]);
    print "</td><td>".$cgi->submit("Convert Pressure")."</td></tr>";
    print "</table>\n";

    # Default values for the result table
    my $result_qcf_title = "&nbsp;";
    my $result_full_title = "&nbsp;";
    my $qcf_converted = "&nbsp;";
    my $full_converted = "&nbsp;";

    # Find the results if the pressure is a number.
    if (defined($cgi->param("pressure")) && 
	isNumber($cgi->param("pressure"))) { 
	$result_qcf_title = "Results (QCF 7.2 float):";
	$result_full_title = "Results (Full float):";
	my $value = Conversions::convertPressure($cgi->param("pressure"),
						 $cgi->param("psInUnits"),
						 $cgi->param("psOutUnits"));
	$qcf_converted = sprintf("<font size=+1>%7.2f</font> %s = <font size=+1>%7.2f</font> %s",
				 $cgi->param("pressure"),
				 $cgi->param("psInUnits"), $value,
				 $cgi->param("psOutUnits"));
	$full_converted = sprintf("<font size=+1>%f</font> %s = <font size=+1>%f</font> %s",
				  $cgi->param("pressure"),
				  $cgi->param("psInUnits"), $value,
				  $cgi->param("psOutUnits"));
    } elsif (defined($cgi->param("pressure"))) {
	$qcf_converted = sprintf("Pressure %s is not a number.",
				 $cgi->param("pressure"));
    }

    # Build the HTML table containing the results
    print "<br><table><tr><td><font size=+1><b>";
    print $result_qcf_title."</b></font></td><td><b><i>";
    print $qcf_converted."</i></b></td></tr>\n<tr><td><font size=+1><b>";
    print $result_full_title."</b></font></td>\n<td><i><b>";
    print $full_converted."</b></i></td></tr></table><br>\n";

    # Build the HTML table containing the simple conversions and abbrevs.
    print "<table border=5 width=100%><tr bgcolor=#336699>";
    print "<th><font color=white size=+1>Simple Conversions</font></th>\n";
    print "<th><font color=white size=+1>Abbreviations</th></font></tr><td>\n";
    print "<table align=center>\n";
    print "<tr><td>1 atm = 1013.25 mb</td></tr>\n";
    print "<tr><td>1 atm = 101.325 kPa</td></tr>\n";
    print "<tr><td>1 atm = 760 / 25.4 inHg = 29.921260 inHg</td></tr>\n";
    print "<tr><td>1 bar = 1000 mb</td></tr>\n";
    print "<tr><td>1 bar = 1000000 dyne/cm2</td></tr>\n";
    print "<tr><td>1 kPa = 1000 Pa</td></tr>\n";
    print "<tr><td>1 inHg = 25.4 mmHg</td></tr>\n";
    print "<tr><td>1 inHg = (1013.25 * 25.4)/760 mb = 33.86388 mb</td></tr>\n";
    print "</table></td>\n<td><table align=center>";
    print "<tr><td>atm</td><td>=</td><td>atmosphere</td></tr>\n";
    print "<tr><td>bar</td><td>=</td><td>bar</td><tr>\n";
    print "<tr><td>dyne/cm2</td><td>=</td><td>dyne per square centimeter</td></tr>\n";
    print "<tr><td>inHg</td><td>=</td><td>inches of mercury</td></tr>\n";
    print "<tr><td>kPa</td><td>=</td><td>kilopascals</td></tr>\n";
    print "<tr><td>mb</td><td>=</td><td>millibar</td><tr>\n";
    print "<tr><td>mmHg</td><td>=</td><td>millimeters of mercury</td></tr>\n";
    print "<tr><td>Pa</td><td>=</td><td>pascals</td></tr>\n";
    print "</table></td></table>\n";
}

##---------------------------------------------------------------------------
# @signature void radiation()
# <p>Create a form for converting radiation values.</p>
##---------------------------------------------------------------------------
sub radiation {
    print "<h1>Radiation</h1>\n";
    print "<p>The radiation converter converts a radiation value from one ";
    print "unit of radiation to another.</p>\n";

    # Build the form to enter the values
    print "<table>\n";
    print "<tr><td>Radiation:</td><td>".$cgi->textfield("radiation");
    print "</td><td>".$cgi->popup_menu(-name=>"radInUnits",
				       -values=>["langly","w/m2"]);
    print "</td></tr>\n<tr><td>Radiation Output Units:</td><td>";
    print $cgi->popup_menu(-name=>"radOutUnits", -values=>["w/m2","langly"]);
    print "</td><td>".$cgi->submit("Convert Radiation")."</td></tr></table>\n";

    # Default values for the radiation
    my $full_title = "&nbsp;";
    my $full_value = "&nbsp;";
    my $qcf_title = "&nbsp;";
    my $qcf_value = "&nbsp;";

    # Convert the radiation
    if (defined($cgi->param("radiation")) && 
	isNumber($cgi->param("radiation"))) {
	$full_title = "Results (Full float):";
	$qcf_title = "Results (QCF 7.2 float):";
	my $value = Conversions::convertRadiation($cgi->param("radiation"),
						  $cgi->param("radInUnits"),
						  $cgi->param("radOutUnits"));
	$qcf_value = sprintf("<font size=+1>%7.2f</font> %s = <font size=+1>%7.2f</font> %s", $cgi->param("radiation"), $cgi->param("radInUnits"), $value,
			     $cgi->param("radOutUnits"));
	$full_value = sprintf("<font size=+1>%f</font> %s = <font size=+1>%f</font> %s", $cgi->param("radiation"), $cgi->param("radInUnits"), $value,
			     $cgi->param("radOutUnits"));
	
    } elsif (defined($cgi->param("radiation"))) {
	$qcf_value = sprintf("Radiation %s is not a number.",
			     $cgi->param("radiation"));
    }

    # Create the results table
    print "<table><tr><td><b><font size=+1>$qcf_title</font></b></td><td>";
    print "<b><i>$qcf_value</i></b></td></tr>\n";
    print "<tr><td><b><font size=+1>$full_title</font></b></td><td>";
    print "<b><i>$full_value</i></b></td></tr></table>\n";

    # Build the Conversion and Abbreviation Table
    print "<table border=5 width=100%><tr bgcolor=#336699>";
    print "<th><font color=white size=+1>Conversions</font></th>\n";
    print "<th><font color=white size=+1>Abbreviations</font></th></tr>\n";
    print "<tr><td><table align=center>\n";
    print "<tr><td>1 langly = 10 w/m&sup2;</td></tr></table>\n";
    print "</td><td><table align=center>\n";
    print "<tr><td>langly</td></tr>\n";
    print "<tr><td>w/m&sup2;</td></tr>\n";
    print "</td></tr></table>\n";
}

##---------------------------------------------------------------------------
# @signature void radiation()
# <p>Create a form for converting radiation values.</p>
##---------------------------------------------------------------------------
sub radiation {
    print "<h1>Radiation</h1>\n";
    print "<p>The radiation converter converts a radiation value from one ";
    print "unit of radiation to another.</p>\n";

    # Build the form to enter the values
    print "<table>\n";
    print "<tr><td>Radiation:</td><td>".$cgi->textfield("radiation");
    print "</td><td>".$cgi->popup_menu(-name=>"radInUnits",
				       -values=>["langly","w/m2"]);
    print "</td></tr>\n<tr><td>Radiation Output Units:</td><td>";
    print $cgi->popup_menu(-name=>"radOutUnits", -values=>["w/m2","langly"]);
    print "</td><td>".$cgi->submit("Convert Radiation")."</td></tr></table>\n";

    # Default values for the radiation
    my $full_title = "&nbsp;";
    my $full_value = "&nbsp;";
    my $qcf_title = "&nbsp;";
    my $qcf_value = "&nbsp;";

    # Convert the radiation
    if (defined($cgi->param("radiation")) && 
	isNumber($cgi->param("radiation"))) {
	$full_title = "Results (Full float):";
	$qcf_title = "Results (QCF 7.2 float):";
	my $value = Conversions::convertRadiation($cgi->param("radiation"),
						  $cgi->param("radInUnits"),
						  $cgi->param("radOutUnits"));
	$qcf_value = sprintf("<font size=+1>%7.2f</font> %s = <font size=+1>%7.2f</font> %s", $cgi->param("radiation"), $cgi->param("radInUnits"), $value,
			     $cgi->param("radOutUnits"));
	$full_value = sprintf("<font size=+1>%f</font> %s = <font size=+1>%f</font> %s", $cgi->param("radiation"), $cgi->param("radInUnits"), $value,
			     $cgi->param("radOutUnits"));
	
    } elsif (defined($cgi->param("radiation"))) {
	$qcf_value = sprintf("Radiation %s is not a number.",
			     $cgi->param("radiation"));
    }

    # Create the results table
    print "<table><tr><td><b><font size=+1>$qcf_title</font></b></td><td>";
    print "<b><i>$qcf_value</i></b></td></tr>\n";
    print "<tr><td><b><font size=+1>$full_title</font></b></td><td>";
    print "<b><i>$full_value</i></b></td></tr></table>\n";

    # Build the Conversion and Abbreviation Table
    print "<table border=5 width=100%><tr bgcolor=#336699>";
    print "<th><font color=white size=+1>Conversions</font></th>\n";
    print "<th><font color=white size=+1>Abbreviations</font></th></tr>\n";
    print "<tr><td><table align=center>\n";
    print "<tr><td>1 langly = 10 w/m&sup2;</td></tr></table>\n";
    print "</td><td><table align=center>\n";
    print "<tr><td>langly</td></tr>\n";
    print "<tr><td>w/m&sup2;</td></tr>\n";
    print "</td></tr></table>\n";
}

##----------------------------------------------------------------------------
# @signature void relhumidity()
# <p>Create the form for calculating a relative humidity value.</p>
##----------------------------------------------------------------------------
sub relhumidity {
    print "<h1>Calculate Relative Humidity</h1>\n";
    print "<p>The relative humidity calculator calculates the relative ";
    print "humidity from the temperature and dew point.</p>\n";
    print "<p><font size=+1 color=red>Warning: </font>The calculation ";
    print "converts the temperature and dew point to &deg;C before ";
    print "performing any calculations.</p>\n";

    # Create the form.
    print "<table><tr><td>Temperature:</td>\n<td>";
    print $cgi->textfield("rhTemp")."</td>\n<td>";
    print $cgi->popup_menu(-name=>"rhTempUnits", -values=>["C", "F", "K"]);
    print "</td></tr>\n<tr><td>Dew Point:</td>\n<td>";
    print $cgi->textfield("rhDP")."</td>\n<td>";
    print $cgi->popup_menu(-name=>"rhDpUnits", -values=>["C", "F", "K"]);
    print "</td></tr>\n<tr><td colspan=3>";
    print $cgi->submit("Convert Relative Humidity")."</td></tr></table>\n";

    # Default values for the result table.
    my $result_title = "&nbsp;";
    my $result_value = "&nbsp;";

    # Calculate the relative humidity.
    if (defined($cgi->param("rhTemp")) && isNumber($cgi->param("rhTemp")) &&
	defined($cgi->param("rhDP")) && isNumber($cgi->param("rhDP"))) {
	$result_title = "Results (Full Float):";
	my $value = Conversions::calculateRelativeHumidity(Conversions::convertTemperature($cgi->param("rhTemp"), $cgi->param("rhTempUnits"), "C"), Conversions::convertTemperature($cgi->param("rhDP"), $cgi->param("rhDpUnits"), "C"), 1);
	$result_value = sprintf("<font size=+1>%f %s</font>", $value, "%");
    } elsif (defined($cgi->param("rhTemp")) && 
	     !isNumber($cgi->param("rhTemp"))) {
	$result_value = sprintf("Temperature %f is not a number.",
				$cgi->param("rhTemp"));
    } elsif (defined($cgi->param("rhDP")) && !isNumber($cgi->param("rhDP"))) {
	$result_value = sprintf("Dew point %f is not a number.",
				$cgi->param("rhDP"));
    }

    # Build the results table.
    print "<br>\n";
    print "<table><tr><td><font size=+1><b>".$result_title."</b><font></td>\n";
    print "<td><b><i>".$result_value."</i></b></td></tr></table>\n";

    # Build the HTML table containing the simple conversions and abbrevs.
    print "<br>\n<table border=5 width=100%><tr bgcolor=#336699>";
    print "<th><font color=white size=+1>Calculation Used</font></th></tr>\n";
    print "<tr><td>sat_vapor_pressure = 6.112 * exp((17.67 * temperature) / ";
    print "(temperature + 243.5))<br>\n";
    print "E_sat_vapor_pressure = 6.112 * exp((17.67 * dewpoint) / ";
    print "(dewpoint + 243.5))<br>";
    print "rel_humidity = 100.0 * (E_sat_vapor_pressure/sat_vapor_pressure)\n";
    print "</td></tr></table>\n";

}

##----------------------------------------------------------------------------
# @signature void sealevelpressure()
# <p>Create the form that calculates sea level pressure.</p>
##----------------------------------------------------------------------------
sub sealevelpressure {
    # Build the heading and instructions.
    print "<h1>Calculate Sea Level Pressure</h1>\n";
    print "<p>The sea level pressure calculator calculates a sea level \n";
    print "pressure from the station pressure, the elevation, the dew point,";
    print "\nand the temperature.</p>\n";
    print "<p><font color=RED size=+1>Warning: </font>The calculation \n";
    print "converts the Pressure to mb, the Elevation to m, the Dew Point \n";
    print "and Temperature to &deg;C before perfoming any of the calculation.";
    print "</p>\n";
    
    # Build the Form for calculating sea level pressure.
    print "<table>\n";
    print "<tr><td>Pressure:</td><td>".$cgi->textfield('slpPressure');
    print "</td><td>".$cgi->popup_menu(-name=>"slpPressUnits", -values=>["mb","inHg","mmHg","kPa","atm","bar","Pa","dyne/cm2"]);
    print "</td></tr>\n";
    print "<tr><td>Elevation:</td><td>".$cgi->textfield("slpElevation");
    print "</td><td>".$cgi->popup_menu(-name=>"slpElevUnits", 
				       -values=>["m","ft"])."</td></tr>\n";
    print "<tr><td>Dew Point:</td><td>".$cgi->textfield("slpDewPoint");
    print "</td><td>".$cgi->popup_menu(-name=>"slpDpUnits", 
				       -values=>["C", "F", "K"]);
    print "</td></tr>\n";
    print "<tr><td>Temperature:</td><td>".$cgi->textfield("slpTemp");
    print "</td><td>".$cgi->popup_menu(-name=>"slpTempUnits", 
				       -values=>["C", "F", "K"]);
    print "</td></tr>\n";
    print "<tr><td>Sea Level Pressure Output Units:</td><td>";
    print $cgi->popup_menu(-name=>"slpOutUnits", 
	    -values=>["mb","inHg","mmHg","kPa","atm","bar","Pa","dyne/cm2"]);
    print "</td><td>".$cgi->submit("Calculate");
    print "</td></tr></table>\n";

    # Default values for the result table
    my $result_qcf_title = "&nbsp;";
    my $result_full_title = "&nbsp;";
    my $qcf_converted = "&nbsp;";
    my $full_converted = "&nbsp;";

    # Find the results if the temperature is a number.
    if (defined($cgi->param("slpPressure")) && 
	isNumber($cgi->param("slpPressure")) &&
	defined($cgi->param("slpElevation")) &&
	isNumber($cgi->param("slpElevation")) &&
	defined($cgi->param("slpDewPoint")) && 
	isNumber($cgi->param("slpDewPoint")) && 
	isNumber($cgi->param("slpTemp")) &&
	defined($cgi->param("slpTemp"))) {
	print "<font color=RED>";
	my $value = 
	  Conversions::convertPressure(Conversions::calculateSeaLevelPressure(Conversions::convertPressure($cgi->param("slpPressure"), $cgi->param("slpPressUnits"), "mb"), Conversions::convertLength($cgi->param("slpElevation"), $cgi->param("slpElevUnits"), "m"), Conversions::convertTemperature($cgi->param("slpDewPoint"), $cgi->param("slpDpUnits"), "C"), Conversions::convertTemperature($cgi->param("slpTemp"), $cgi->param("slpTempUnits"), "C"), 1), "mb", $cgi->param("slpOutUnits"));
	$result_qcf_title = "Results (QCF 7.2 float):";
	$result_full_title = "Results (Full float):";
	$qcf_converted = sprintf("<b><i><font size=+1>%7.2f</font> %s</i></b>",
				 $value, $cgi->param("slpOutUnits"));
	$full_converted = sprintf("<b><i><font size=+1>%f</font> %s</i></b>",
				  $value, $cgi->param("slpOutUnits"));
	print "</font>\n";
    } elsif (defined($cgi->param("slpPressure")) && 
	     !isNumber($cgi->param("slpPressure"))) {
	$qcf_converted = sprintf("Pressure %s is not a number.",
				 $cgi->param("slpPressure"));
    } elsif (defined($cgi->param("slpElevation")) && 
	     !isNumber($cgi->param("slpElevation"))) {
	$qcf_converted = sprintf("Elevation %s is not a number.",
				 $cgi->param("slpElevation"));
    } elsif (defined($cgi->param("slpDewPoint")) && 
	     !isNumber($cgi->param("slpDewPoint"))) {
	$qcf_converted = sprintf("Dew Point %s is not a number.",
				 $cgi->param("slpDewPoint"));
    } elsif (defined($cgi->param("slpTemp")) && 
	     !isNumber($cgi->param("slpTemp"))) {
	$qcf_converted = sprintf("Temperature %s is not a number.",
				 $cgi->param("slpTemp"));
    }

    # Build the HTML table containing the results
    print "<br><table><tr><td><font size=+1><b>";
    print $result_qcf_title."</b></font></td><td>";
    print $qcf_converted."</td></tr>\n<tr><td><font size=+1><b>";
    print $result_full_title."</b></font></td>\n<td>";
    print $full_converted."</td></tr></table><br>\n";

    # Build the HTML table containing the constants and calculations.
    print "<table border=5 width=100%><tr bgcolor=#336699>";
    print "<th><font color=white size=+1>Constants</font></th>\n";
    print "<th><font color=white size=+1>Calculations</th></font></tr><td>\n";
    print "<table align=center>\n";
    print "<tr><td>gamma = 6.5</td></tr>\n";
    print "<tr><td>gravity = 9.80616</td></tr>\n";
    print "<tr><td>r = 287.04</td></tr></table></td>\n<td>";
    print "kelvin = temperature - 273.15<br>";
    print "vapr = 6.112 * exp ((17.67 * dewpoint) / (dewpoint + 243.5))<br>";
    print "e = vapr * (1.001 + (pressure - 100.0) / 900.0 * 0.0034)<br>";
    print "mixr = 0.62197 * (e / (pressure - e)) * 1000.0<br>";
    print "virt_temp = kelvin * ((1.0 + (0.001 * mixr) / 0.62197) / ";
    print "(1.0 + 0.001 * mixr))<br>";
    print "deltv = gamma * elevation / 1000.0<br>";
    print "avg_virt_temp = (virt_temp + (virt_temp - deltv)) / 2.0<br>";
    print "slpressure = pressure * exp((gravity * elevation) / ";
    print "(r * avg_virt_temp)))</td></table>\n";
}

##---------------------------------------------------------------------------
# @signature void spechumid()
# <p> Create a form for calculating specific humidity.</p>
##---------------------------------------------------------------------------
sub spechumid {
    # Build the heading and instructions.
    print "<h1>Calculate Specific Humidity</h1>\n";
    print "<p>The specific humidity calculator calculates the specific ";
    print "humidity from the station pressure and dewpoint.<\p>\n";
    print "<p><font size=+1 color=red>Warning: </font>The station pressure ";
    print "is converted to mb and the dewpoint is converted to &deg;C before ";
    print "any performing the calculation.</p>";

    # Build the form
    print "<table><tr><td>Station Pressure:</td><td>";
    print $cgi->textfield("shPress")."</td><td>";
    print $cgi->popup_menu(-name=>"shPressUnits", -values=>["mb","inHg","mmHg","kPa","atm","bar","Pa","dyne/cm2"]);
    print "</td></tr>\n<tr><td>Dew Point:</td><td>".$cgi->textfield("shDp");
    print "</td><td>".$cgi->popup_menu(-name=>"shDpUnits",
				       -values=>["C","F","K"]);
    print "</td></tr>\n<tr><td colspan=3>";
    print $cgi->submit("Calculate Specific Humidity")."</td></tr></table>\n";
    
    # Default result values
    my $full_title = "&nbsp;";
    my $full_value = "&nbsp;";
    my $qcf_title = "&nbsp;";
    my $qcf_value = "&nbsp;";

    # Calculate the specific humidity
    if (defined($cgi->param("shPress")) && isNumber($cgi->param("shPress")) &&
	defined($cgi->param("shDp")) && isNumber($cgi->param("shDp"))) {
	$full_title = "Results (Full float):";
	$qcf_title = "Results (QCF 7.2 float):";
	my $value = Conversions::calculateSpecificHumidity(Conversions::convertPressure($cgi->param("shPress"), $cgi->param("shPressUnits"), "mb"), Conversions::convertTemperature($cgi->param("shDp"), $cgi->param("shDpUnits"), "C"));
	$qcf_value = sprintf("<font size=+1>%7.2f</font> kg/kg", $value);
	$full_value = sprintf("<font size=+1>%f</font> kg/kg", $value);
    } elsif (defined($cgi->param("shPress"))) {
	$qcf_value = sprintf("Station Pressure %s is not a number.",
			     $cgi->param("shPress"));
    } elsif (defined($cgi->param("shDp"))) {
	$qcf_value = sprintf("Dew Point %s is not a number.",
			     $cgi->param("shDp"));
    }

    # Create the results table.
    print "<table><tr><td><b><font size=+1>$qcf_title</font></b></td><td>";
    print "<b><i>$qcf_value</i></b></td></tr>\n";
    print "<tr><td><b><font size=+1>$full_title</font></b></td><td>";
    print "<b><i>$full_value</i></b></td></tr></table>\n";
    
    # Build the calculations table.
    print "<table border=5 width=100%><tr bgcolor=#336699>";
    print "<th><font color=white size=+1>Calculation Used</th></font></tr>\n";
    print "<tr><td>";
    print "e = 6.112 * exp((17.67 * dewpoint)/(243.5 + dewpoint))<br>\n";
    print "specific_humidity = (0.622 * e)/(pressure - (0.378 * e))</td></tr>";
    print "\n</table>\n";
}

##-------------------------------------------------------------------------
# @signature void statecode()
# <p>Build the form for the state codes.</p>
##-------------------------------------------------------------------------
sub statecode {
    # build the heading and instructions.
    print "<h1>State/State Code Calculation for Station Lists</h1>\n";
    print "<p>Determine the state code from the two character state \n";
    print "abbreviation or the state abbreviation from the two digit\n";
    print "state code.</p>\n";
    print "<p><font color=RED>The state abbreviation needs to be in all\n";
    print "upper case letters.</font></p>\n";

    # Build the Form to enter the value.
    print "<table>\n";
    print "<tr><td>State Code/Abbreviation:</td><td>";
    print $cgi->textfield('stcode')."</td><td>\n";
    print $cgi->submit("Calculate State Code")."</td></tr></table><br>\n";

    # Default values for the result table
    my $result = "&nbsp;";
    my $converted = "&nbsp;";

    # Find the results
    if (defined($cgi->param('stcode'))) {
	$result = "Result:";
	my $value = Conversions::getStateCode($cgi->param('stcode'), 1);
	if (isNumber($cgi->param('stcode'))) {
	    $converted = sprintf("<i><b>State Code <font size=+1>%s</font> is for State <font size=+1>%s</font></b></i>",
				 $cgi->param('stcode'), $value);
	} else {
	    $converted = sprintf("<i><b>State <font size=+1>%s</font> has State Code <font size=+1>%s</font></b></i>",
				 $cgi->param('stcode'), $value);
	}
    }
    
    # Build the HTML table containing the results
    print "<br><table><tr><td><font size=+1><b>";
    print $result."</b></font></td><td>";
    print $converted."</td></tr></table><br>\n";

    # Build the Abbreviation Table
    print "<table border=5 width=100%><tr bgcolor=#336699>";
    print "<th><font color=white size=+1>State</font></th>\n";
    print "<th><font color=white size=+1>Abbrev</th></font>\n";
    print "<th><font color=white size=+1>State</font></th>\n";
    print "<th><font color=white size=+1>Abbrev</th></font>\n";
    print "<th><font color=white size=+1>State</font></th>\n";
    print "<th><font color=white size=+1>Abbrev</th></font>\n";
    print "<th><font color=white size=+1>State</font></th>\n";
    print "<th><font color=white size=+1>Abbrev</th></font></tr>\n";
    print "<tr><td>Alabama</td><td>AL</td><td>Indiana</td><td>IN</td>\n";
    print "<td>Nebraska</td><td>NE</td><td>South Carolina</td><td>SC</td>";
    print "</tr>\n";
    print "<tr><td>Alaska</td><td>AK</td><td>Iowa</td><td>IA</td>\n";
    print "<td>Nevada</td><td>NV</td><td>South Dakota</td><td>SD</td></tr>\n";
    print "<tr><td>Arizona</td><td>AZ</td><td>Kansas</td><td>KS</td>\n";
    print "<td>New Hampshire</td><td>NH</td><td>Tennessee</td><td>TN</td>";
    print "</tr>\n";
    print "<tr><td>Arkansas</td><td>AR</td><td>Kentucky</td><td>KY</td>\n";
    print "<td>New Jersey</td><td>NJ</td><td>Texas</td><td>TX</td></tr>\n";
    print "<tr><td>California</td><td>CA</td><td>Louisiana</td><td>LA</td>\n";
    print "<td>New Mexico</td><td>NM</td><td>Utah</td><td>UT</td></tr>\n";
    print "<tr><td>Colorado</td><td>CO</td><td>Maine</td><td>ME</td>\n";
    print "<td>New York</td><td>NY</td><td>Vermont</td><td>TD</td></tr>\n";
    print "<tr><td>Connecticut</td><td>CT</td><td>Maryland</td><td>MD</td>\n";
    print "<td>North Carolina</td><td>NC</td><td>Virginia</td><td>VA</td>";
    print "</tr>\n";
    print "<tr><td>Deleware</td><td>DE</td><td>Massachusetts</td><td>MA</td>";
    print "\n<td>North Dakota</td><td>ND</td><td>Washington</td><td>WA</td>";
    print "</tr>\n";
    print "<tr><td>Florida</td><td>FL</td><td>Michigan</td><td>MI</td>\n";
    print "<td>Ohio</td><td>OH</td><td>Wasington DC</td><td>DC</td></tr>\n";
    print "<tr><td>Georgia</td><td>GA</td><td>Minnesota</td><td>MN</td>\n";
    print "<td>Oklahoma</td><td>OK</td><td>West Virginia</td><td>WV</td>";
    print "</tr>\n";
    print "<tr><td>Hawaii</td><td>HI</td><td>Mississippi</td><td>MS</td>\n";
    print "<td>Oregon</td><td>OR</td><td>Wisconsin</td><td>WI</td></tr>\n";
    print "<tr><td>Idaho</td><td>ID</td><td>Missouri</td><td>MO</td>\n";
    print "<td>Pennsylvania</td><td>PA</td><td>Wyoming</td><td>WY</td></tr>\n";
    print "<tr><td>Illinois</td><td>IL</td><td>Montana</td><td>MT</td>\n";
    print "<td>Rhode Island</td><td>RI</td><td>Unknown</td><td>XX</td></tr>\n";
    print "</table>\n";
}

##--------------------------------------------------------------------------
# @signature void stnpressure()
# <p>Create the form for calculating station pressure.</p>
##--------------------------------------------------------------------------
sub stnpressure {
    # Build the heading and instructions
    print "<h1>Calculate Station Pressure</h1>\n";
    print "<p>Calculate a station pressure from an altimeter reading and ";
    print "elevation value.</p>";
    print "<p><font color=RED size=+1>Warning: </font>The calculation \n";
    print "converts the altimeter reading to mb and the elevation to m ";
    print "before performing the calculations.</p>";

    # Build the form
    print "<table><tr><td>Altimeter:</td><td>".$cgi->textfield("stnAlt");
    print "</td><td>".$cgi->popup_menu(-name=>"stnAltUnits", -values=>["mb","inHg","mmHg","kPa","atm","bar","Pa","dyne/cm2"]);
    print "</td></tr>\n<tr><td>Elevation:</td><td>".$cgi->textfield("stnElev");
    print "</td><td>";
    print $cgi->popup_menu(-name=>"stnElevUnits", -values=>["m", "ft"]);
    print "</td></tr>\n<tr><td>Station Pressure Output Units:</td><td>";
    print $cgi->popup_menu(-name=>"stnOutUnits", -values=>["mb","inHg","mmHg","kPa","atm","bar","Pa","dyne/cm2"]);
    print "</td><td>".$cgi->submit("Calculate Station Pressure");
    print "</td></tr></table>\n";

    # Default values for results table.
    my $full_result_title = "&nbsp;";
    my $qcf_result_title = "&nbsp;";
    my $full_value = "&nbsp;";
    my $qcf_value = "&nbsp;";

    # Calculate the station pressure
    if (defined($cgi->param('stnAlt')) && isNumber($cgi->param('stnAlt')) &&
	defined($cgi->param('stnElev')) && isNumber($cgi->param('stnElev'))) {
	$full_result_title = "Results (Full float):";
	$qcf_result_title = "Results (QCF 7.2 float):";
	my $value = Conversions::convertPressure(Conversions::calculateStationPressure(Conversions::convertPressure($cgi->param('stnAlt'), $cgi->param('stnAltUnits'), "mb"), Conversions::convertLength($cgi->param('stnElev'), $cgi->param('stnElevUnits'), "m")), "mb", $cgi->param('stnOutUnits'));
	$qcf_value = sprintf("<font size=+1>%7.2f</font> %s", $value,
			     $cgi->param('stnOutUnits'));
	$full_value = sprintf("<font size=+1>%f</font> %s", $value,
			      $cgi->param('stnOutUnits'));
    } elsif (defined($cgi->param('stnElev')) && 
	     !isNumber($cgi->param('stnElev'))) {
	$qcf_value = sprintf("Elevation %s is not a number.",
			     $cgi->param('stnElev'));
    } elsif (defined($cgi->param('stnAlt')) && 
	     !isNumber($cgi->param('stnAlt'))) {
	$qcf_value = sprintf("Altimeter reading %s is not a number.",
			     $cgi->param('stnAlt'));
    }

    # Build the results table.
    print "<table><tr><td><font size=+1><b>$qcf_result_title</i></font></td>";
    print "\n<td><b><i>$qcf_value</i></b></td></tr>\n<tr><td><font size=+1>";
    print "<b>$full_result_title</b></font></td><td><b><i>$full_value</i></b>";
    print "</td></tr></table>\n";

    # Build the HTML table containing the constants and calculations.
    print "<table border=5 width=100%><tr bgcolor=#336699>";
    print "<th><font color=white size=+1>Constants</font></th>\n";
    print "<th><font color=white size=+1>Calculations</th></font></tr><td>\n";
    print "<table align=center>\n";
    print "<tr><td>p0 = 1013.25</td></tr>\n";
    print "<tr><td>a = .0065</td></tr>\n";
    print "<tr><td>t0 = 288</td></tr>\n";
    print "<tr><td>n = .190284</td></tr></table></td>\n";
    print "<td>stn_pressure = (((altimeter ** n) - ((((p0 ** n) * a) / t0) * ";
    print "elevation)) ** (1 / n)) + 0.01</td></td></table>\n";
}

##--------------------------------------------------------------------------
# @signature void temperature()
# <p>Build the form for converting temperature values.
##--------------------------------------------------------------------------
sub temperature {
    # Build the heading and instructions.
    print "<h1>Temperature</h1>\n";
    print "<p>The temperature conversions takes a value in one unit of\n";
    print "temperature and converts it to another unit of temperature.</p>\n";

    # Build the Form to convert temperature values.
    print "<table>\n";
    print "<tr><td>Temperature:</td><td>".$cgi->textfield('temperature');
    print "</td><td>&deg;".$cgi->popup_menu(-name=>"tempInUnits", 
				       -values=>["F", "C", "K"]);
    print "</td></tr>\n<tr><td>Temperature Output Units:</td><td>&deg;";
    print $cgi->popup_menu(-name=>"tempOutUnits",-values=>["C", "F", "K"]);
    print "</td><td>".$cgi->submit("Convert Temperature");
    print "</td></tr></table><br>\n";

    # Default values for the result table
    my $result_qcf_title = "&nbsp;";
    my $result_full_title = "&nbsp;";
    my $qcf_converted = "&nbsp;";
    my $full_converted = "&nbsp;";

    # Find the results if the temperature is a number.
    if (defined($cgi->param("temperature")) && 
	isNumber($cgi->param("temperature"))) { 
	$result_qcf_title = "Results (QCF 7.2 float):";
	$result_full_title = "Results (Full float):";
	my $value = Conversions::convertTemperature($cgi->param("temperature"),
						    $cgi->param("tempInUnits"),
						 $cgi->param("tempOutUnits"));
	$qcf_converted = sprintf("<i><b><font size=+1>%7.2f</font> &deg;%s = <font size=+1>%7.2f</font> &deg;%s</b></i>",
				 $cgi->param("temperature"),
				 $cgi->param("tempInUnits"), $value,
				 $cgi->param("tempOutUnits"));
	$full_converted = sprintf("<i><b><font size=+1>%f</font> &deg;%s = <font size=+1>%f</font> &deg;%s</b></i>",
				  $cgi->param("temperature"),
				  $cgi->param("tempInUnits"), $value,
				  $cgi->param("tempOutUnits"));
    } elsif (defined($cgi->param("temperature"))) {
	$qcf_converted = sprintf("Temperature %s is not a number.",
				 $cgi->param("temperature"));
    }

    # Build the HTML table containing the results
    print "<br><table><tr><td><font size=+1><b>";
    print $result_qcf_title."</b></font></td><td>";
    print $qcf_converted."</td></tr>\n<tr><td><font size=+1><b>";
    print $result_full_title."</b></font></td>\n<td>";
    print $full_converted."</td></tr></table><br>\n";

    # Build the HTML table containing the simple conversions and abbrevs.
    print "<table border=5 width=100%><tr bgcolor=#336699>";
    print "<th><font color=white size=+1>Simple Conversions</font></th>\n";
    print "<th><font color=white size=+1>Abbreviations</th></font></tr><td>\n";
    print "<table align=center>\n";
    print "<tr><td>Absolute Zero</td><td>-273.15 &deg;C</td><td>=</td>";
    print "<td>-459.67 &deg;F</td><td>=</td><td>0 K</td></tr>";
    print "<tr><td>Freezing Point (Water)<td>0 &deg;C</td><td>=</td>";
    print "<td>32 &deg;F</td><td>=</td><td>273.15 K</td></tr>\n";
    print "<tr><td>Boiling Point (Water)</td><td>100 &deg;C</td><td>";
    print "=</td><td>212 &deg;F</td><td>=</td><td>373.15 K</td></tr>\n";
    print "<tr><td>Equation:</td>";
    print "<td colspan=5>&deg;C = (5/9) * (&deg;F - 32)</td></tr>\n";
    print "<tr><td>Equation:</td>";
    print "<td colspan=5>&deg;C = K - 273.15</td></tr>\n";
    print "</table></td>\n";
    print "<td><table align=center>\n";
    print "<tr><td>&deg;C</td><td>=</td><td>degrees Celsius</td></tr>\n";
    print "<tr><td>&deg;F</td><td>=</td><td>degrees Fahrenheit</td></tr>\n";
    print "<tr><td>K</td><td>=</td><td>Kelvin</td></tr></table></td>\n";
    print "</table>";
}

##---------------------------------------------------------------------------
# @signature void time()
# <p>Create a form for converting a time value between units.</p>
##---------------------------------------------------------------------------
sub time {
    # Build the title and instructions.
    print "<h1>Time</h1>\n";
    print "<p>The time conversion takes a value in one unit of time and ";
    print "converts it to another unit of time.</p>";
    
    # Create the form
    print "<table><tr><td>Time:</td><td>".$cgi->textfield("time");
    print "</td><td>".$cgi->popup_menu(-name=>"timeInUnits",
				       -values=>["sec", "min", "hr", "day"]);
    print "</td></tr>\n<tr><td>Time Output Units:</td><td>";
    print $cgi->popup_menu(-name=>"timeOutUnits", 
			   -values=>["sec", "min", "hr", "day"]);
    print "</td><td>".$cgi->submit("Convert Time")."</td></tr></table>\n";

    # Default result table values.
    my $result_title = "&nbsp;";
    my $result_value = "&nbsp;";

    # Convert the time
    if (defined($cgi->param('time')) && isNumber($cgi->param('time'))) {
	$result_title = "Results (Full float):";
	my $value = Conversions::convertTimePeriod($cgi->param('time'),
						   $cgi->param('timeInUnits'),
						  $cgi->param('timeOutUnits'));
	$result_value = sprintf("<font size=+1>%f %s = %f %s</font>",
				$cgi->param('time'), 
				$cgi->param("timeInUnits"),
				$value, $cgi->param('timeOutUnits'));
    } elsif (defined($cgi->param('time')) && !isNumber($cgi->param("time"))) {
	$result_value = sprintf("Time %s is not a number.",
				$cgi->param('time'));
    }

    # Build the results table.
    print "<br><table><tr><td><b><font size=+1>$result_title</font></b></td>";
    print "\n<td><b><i>$result_value</i></b></td></tr></table>\n";
    
    # Build the HTML table containing the simple conversions and abbrevs.
    print "<table border=5 width=100%><tr bgcolor=#336699>";
    print "<th><font color=white size=+1>Simple Conversions</font></th>\n";
    print "<th><font color=white size=+1>Abbreviations</th></font></tr><td>\n";
    print "<table align=center><tr><td>1 min = 60 sec</td></tr>\n";
    print "<tr><td>1 hr = 60 min</td></tr>\n";
    print "<tr><td>1 day = 24 hr</td></tr>\n";
    print "<tr><td>&nbsp;</td></table></td>\n<td>";
    print "<table align=center><tr><td>day</td><td>=</td><td>days</td></tr>\n";
    print "<tr><td>hr</td><td>=</td><td>hours</td></tr>\n";
    print "<tr><td>min</td><td>=</td><td>minutes</td></tr>\n";
    print "<tr><td>sec</td><td>=</td><td>seconds</td></tr></table></td>\n";
    print "</table>\n";    
}

##-------------------------------------------------------------------------
# @signature void uvwind()
# <p>Create a form that calaculates the wind speed and direction from UV
# vector components.</p>
##-------------------------------------------------------------------------
sub uvwind {
    # Create the heading and instructions.
    print "<h1>UV Vector Components to Wind Speed and Direction</h1>\n";
    print "<p>Calculate the wind speed and wind direction from the UV ";
    print "vector components.</p>\n";
    print "<p><font size=+1 color=red>Warning: </font>The UV components will ";
    print "be converted to m/s before performing the calculation.</p>\n";

    # Create the form.
    print "<table><tr><td>U Component:</td><td>".$cgi->textfield("ucomp");
    print "</td><td rowspan=2>".$cgi->popup_menu(-name=>"compInUnits", -values=>["m/s","mph","km/hr","ft/s", "knot"]);
    print "</td></tr>\n<tr><td>V Component:</td><td>";
    print $cgi->textfield("vcomp")."</td></tr>\n<tr><td>Wind Speed Output ";
    print "Units </td><td>".$cgi->popup_menu(-name=>"wsOutUnits", -values=>["m/s","mph","km/hr","ft/s", "knot"]);
    print "</td><td>".$cgi->submit("Calculate Wind")."</td></tr></table>\n";

    # Default result values
    my $full_title = "&nbsp;";
    my $full_value = "&nbsp;";
    my $qcf_title = "&nbsp;";
    my $qcf_value = "&nbsp;";

    # Calculate the results
    if (defined($cgi->param("ucomp")) && isNumber($cgi->param("ucomp")) &&
	defined($cgi->param("vcomp")) && isNumber($cgi->param("vcomp"))) {
	$full_title = "Results (Full float):";
	$qcf_title = "Results (QCF 7.2 float):";
	(my $ws, my $wd) = Conversions::calculateWindFromUV(Conversions::convertVelocity($cgi->param("ucomp"), $cgi->param("compInUnits"), "m/s"), Conversions::convertVelocity($cgi->param("vcomp"), $cgi->param("compInUnits"), "m/s"));
	$ws=Conversions::convertVelocity($ws,"m/s",$cgi->param("wsOutUnits"));
	$qcf_value = sprintf("Wind Speed: <font size=+1>%7.2f</font> %s, Wind Direction: <font size=+1>%7.2f</font> degress.", $ws, $cgi->param("wsOutUnits"), $wd);
	$full_value = sprintf("Wind Speed: <font size=+1>%f</font> %s, Wind Direction: <font size=+1>%f</font> degress.", $ws, $cgi->param("wsOutUnits"), $wd);
    } elsif (defined($cgi->param("ucomp"))) {
	$qcf_value = sprintf("U component %s is not a number.",
			     $cgi->param("ucomp"));
    } elsif (defined($cgi->param("vcomp"))) {
	$qcf_value = sprintf("V component %s is not a number.",
			     $cgi->param("vcomp"));
    }

    # Create the results table
    print "<table><tr><td><b><font size=+1>$qcf_title</font></b></td><td>";
    print "<b><i>$qcf_value</i></b></td></tr>\n";
    print "<tr><td><b><font size=+1>$full_title</font></b></td><td>";
    print "<b><i>$full_value</i></b></td></tr></table>\n<br>\n";

    # Create the calculations table
    print "<table border=5 width=100%><tr bgcolor=#336699>";
    print "<th><font color=white size=+1>Calculations</th></font></tr><td>\n";
    print "if (vcomp == 0) {<br>\n";
    print "&nbsp;&nbsp;if (ucomp == 0) { return (0, 0); }<br>\n";
    print "&nbsp;&nbsp;elsif (ucomp > 0) { return (ucomp, 270); }<br>\n";
    print "&nbsp;&nbsp;else { return ((-1 * ucomp), 90); }<br>\n";
    print "}<br>\n";
    print "wind_speed = sqrt(ucomp ** 2 + vcomp ** 2);<br>\n";
    print "wind_dir = (atan(ucomp / vcomp)) * 180 / PI;<br>\n";
    print "if (vcomp >= 0) { wind_dir += 180; }\n</td></table>\n";
}

##---------------------------------------------------------------------------
# @signature void velocity()
# <p>Create the form to convert velocity values.</p>
##---------------------------------------------------------------------------
sub velocity {
    # Build the title and instructions.
    print "<h1>Velocity</h1>\n";
    print "<p>The velocity conversion takes a value in one unit of velocity\n";
    print "and converts it to another unit of velocity.</p>";

    # Build the Form to input the velocity.
    print "<table><tr><td>Velocity:</td><td>".$cgi->textfield("velocity");
    print "</td><td>".$cgi->popup_menu(-name=>"velInUnits", 
			   -values=>["mph","m/s","km/hr","ft/s", "knot"]);
    print "</td></tr>\n";
    print "<tr><td>Velocity Output Units:</td><td>";
    print $cgi->popup_menu(-name=>"velOutUnits", 
			   -values=>["m/s","mph","km/hr","ft/s", "knot"]);
    print "</td><td>".$cgi->submit("Convert Velocity");
    print "</td></tr></table><br>\n";

    # Default values for the result table
    my $result_qcf_title = "&nbsp;";
    my $result_full_title = "&nbsp;";
    my $qcf_converted = "&nbsp;";
    my $full_converted = "&nbsp;";

    # Do the conversion if the velocity is a number.
    if (defined($cgi->param("velocity")) && isNumber($cgi->param("velocity"))){
	$result_qcf_title = "Results (QCF 7.2 float):";
	$result_full_title = "Results (Full float):";
	my $value = Conversions::convertVelocity($cgi->param("velocity"),
						 $cgi->param("velInUnits"),
						 $cgi->param("velOutUnits"));
	$qcf_converted = sprintf("<i><b><font size=+1>%7.2f</font> %s = <font size=+1>%7.2f</font> %s</b></i>",
				 $cgi->param("velocity"),
				 $cgi->param("velInUnits"), $value,
				 $cgi->param("velOutUnits"));
	$full_converted = sprintf("<i><b><font size=+1>%f</font> %s = <font size=+1>%f</font> %s</b></i>",
				  $cgi->param("velocity"),
				  $cgi->param("velInUnits"), $value,
				  $cgi->param("velOutUnits"));
    } elsif (defined($cgi->param("velocity"))) {
	$qcf_converted = sprintf("Velocity %s is not a number.",
				 $cgi->param("velocity"));
    }

    # Build the HTML table containing the results
    print "<br><table><tr><td><font size=+1><b>";
    print $result_qcf_title."</b></font></td><td>";
    print $qcf_converted."</td></tr>\n<tr><td><font size=+1><b>";
    print $result_full_title."</b></font></td>\n<td>";
    print $full_converted."</td></tr></table><br>\n";

    # Build the HTML table containing the simple conversions and abbrevs.
    print "<table border=5 width=100%><tr bgcolor=#336699>";
    print "<th><font color=white size=+1>Simple Conversions</font></th>\n";
    print "<th><font color=white size=+1>Abbreviations</th></font></tr><td>\n";
    print "<table align=center><tr><td>1 ft/s = .3048 m/s</td></tr>\n";
    print "<tr><td>1 mph = 1.609344 km/hr</td></tr>\n";
    print "<tr><td>1 m/s = 3.6 km/hr</td></tr>\n";
    print "<tr><td>1 mph = 1609.344 / 3600 m/s = .44704 m/s</td></tr>\n";
    print "<tr><td>1 knot = 463 / 900 m/s = .51444 m/s</td></tr>\n";
    print "<tr><td>1 knot = 1852 / 1609.344 mph = 1.15078 mph</td></tr>\n";
    print "</table></td>\n<td>";
    print "<table align=center>\n";
    print "<tr><td>ft/s</td><td>=</td><td>feet per second</td></tr>\n";
    print "<tr><td>km/hr</td><td>=</td><td>kilometers per hour</td></tr>\n";
    print "<tr><td>knot</td><td>=</td><td>nautical mile per hour</td></tr>\n";
    print "<tr><td>m/s</td><td>=</td><td>meters per second</td></tr>\n";
    print "<tr><td>mph</td><td>=</td><td>miles per hour</td></tr>\n";
    print "<tr><td>&nbsp;</td></tr>\n";
    print "</td></tr></table>\n";
}

##---------------------------------------------------------------------------
# @signature void volume()
# <p>Create the form for converting volumes.</p>
##---------------------------------------------------------------------------
sub volume {
    # Build the heading and instructions.
    print "<h1>Volume</h1>\n";
    print "<p>The volume conversion convert a value from one unit of volume ";
    print "to another unit of volume.</p>\n";

    # Build the form
    print "<table><tr><td>Volume:</td><td>".$cgi->textfield("vol");
    print "</td><td>".$cgi->popup_menu(-name=>"volInUnits", -values=>["ft3", "m3", "af", "gal", "liter", "ml", "cm3", "in3", "qt", "pt", "oz"]);
    print "</td></tr>\n<tr><td>Volume Output Units:</td><td>";
    print $cgi->popup_menu(-name=>"volOutUnits", -values=>["m3", "ft3", "af","gal", "liter", "ml", "cm3", "in3", "qt", "pt", "oz"]);
    print "</td><td>".$cgi->submit("Convert Volume")."</td></tr></table>\n";
    
    # Default values for the results table.
    my $result_title = "&nbsp;";
    my $result_value = "&nbsp;";

    # Convert the volume
    if (defined($cgi->param("vol")) && isNumber($cgi->param("vol"))) {
	$result_title = "Results (Full float):";
	my $value = Conversions::convertVolume($cgi->param("vol"),
					       $cgi->param("volInUnits"),
					       $cgi->param("volOutUnits"));
	$result_value = sprintf("<font size=+1>%f</font> %s = <font size=+1>%f</font> %s",
				$cgi->param("vol"), $cgi->param("volInUnits"),
				$value, $cgi->param("volOutUnits"));
    } elsif (defined($cgi->param("vol")) && !isNumber($cgi->param("vol"))) {
	$result_value = sprintf("Volume %s is not a number.",
				$cgi->param("vol"));
    }

    # Build the results table
    print "<table><tr><td><font size=+1><b>$result_title</b></td>\n";
    print "<td><b><i>$result_value</i></b></td></tr></table>\n<br>\n";

    # Build the HTML table containing the simple conversions and abbrevs.
    print "<table border=5 width=100%><tr bgcolor=#336699>";
    print "<th><font color=white size=+1>Simple Conversions</font></th>\n";
    print "<th><font color=white size=+1>Abbreviations</th></font></tr><td>\n";
    print "<table align=center><tr><td>1 cm&sup3; = 1 ml</td></tr>\n";
    print "<tr><td>1000 ml = 1 liter</td></tr>\n";
    print "<tr><td>1 m&sup3; = 1000000 cm&sup3;</td></tr>\n";
    print "<tr><td>1 ft&sup3; = 1728 in&sup3;</td></tr>\n";
    print "<tr><td>1 af = 43560 ft&sup3;</td></tr>\n";
    print "<tr><td>16 oz = 1 pt</td></tr>\n";
    print "<tr><td>2 pt = 1 qt</td></tr>\n";
    print "<tr><td>4 qt = 1 gal</td></tr>\n";
    print "<tr><td>1 in&sup3; = (2.54)&sup3; cm&sup3; = 16.38706 cm&sup3;</td></tr>\n";
    print "<tr><td>1 ft&sup3; = (.3048)&sup3; m&sup3; = .02832 m&sup3;</td></tr>\n";
    print "<tr><td>1 gal = 34628947/9148000 liter = 3.7854 liter</td></tr>\n";
    print "<tr><td>1 oz = 34628947/1170944 ml = 29.57353 ml</td></tr>\n";
    print "</table></td><td><table align=center>\n";
    print "<tr><td>af</td><td>=</td><td>acre-feet</td></tr>\n";
    print "<tr><td>cm&sup3;</td><td>=</td><td>cubic centimeters</td></tr>\n";
    print "<tr><td>ft&sup3;</td><td>=</td><td>cubic feet</td></tr>\n";
    print "<tr><td>gal</td><td>=</td><td>gallons</td></tr>\n";
    print "<tr><td>in&sup3;</td><td>=</td><td>cubic inches</td></tr>\n";
    print "<tr><td>liter</td><td>=</td><td>liters</td></tr>\n";
    print "<tr><td>m&sup3;</td><td>=</td><td>cubic meters</td></tr>\n";
    print "<tr><td>ml</td><td>=</td><td>milliliters</td></tr>\n";
    print "<tr><td>oz</td><td>=</td><td>ounces</td></tr>\n";
    print "<tr><td>pt</td><td>=</td><td>pints</td></tr>\n";
    print "<tr><td>qt</td><td>=</td><td>quarts</td></tr>\n";
    print "<tr><td>&nbsp;</td></tr>\n";
    print "</table></td></table>\n";
}

##---------------------------------------------------------------------------
# @signature void winduv()
# <p>Create a form for calculating the uv components from wind speed and 
# direction.</p>
##---------------------------------------------------------------------------
sub winduv {
    # Create the heading and instructions
    print "<h1>Wind Speed and Direction to UV Vector Components</h1>";
    print "<p>Calculate the UV vector components from a wind speed and ";
    print "direction.<\p>\n";
    print "<p><font size+=1 color=red>Warning: </font>The wind speed will be ";
    print "converted to m/s before performing the calculation.<\p>\n";

    # Build the form
    print "<table><tr><td>Wind Speed:</td><td>".$cgi->textfield("wspeed");
    print "</td><td>".$cgi->popup_menu(-name=>"wsInUnits", -values=>["m/s","mph","km/hr","ft/s", "knot"]);
    print "</td></tr>\n<tr><td>Wind Direction:</td><td>";
    print $cgi->textfield("wdir")."</td><td>degrees</td></tr>\n";
    print "<tr><td>UV Output Units:</td><td>";
    print $cgi->popup_menu(-name=>"uvOutUnits", -values=>["m/s","mph","km/hr","ft/s", "knot"]);
    print "</td><td>".$cgi->submit("Convert Wind")."</td></tr></table>\n";
	
    # Default result values
    my $full_title = "&nbsp;";
    my $qcf_title = "&nbsp;";
    my $full_value = "&nbsp;";
    my $qcf_value = "&nbsp;";
    
    # Calculate the uv components
    if (defined($cgi->param("wspeed")) && isNumber($cgi->param("wspeed")) &&
	defined($cgi->param("wdir")) && isNumber($cgi->param("wdir"))) {
	$full_title = "Results (Full float):";
	$qcf_title = "Results (QCF 7.2 float):";
	(my $u, my $v) = Conversions::calculateUVfromWind(Conversions::convertVelocity($cgi->param("wspeed"), $cgi->param("wsInUnits"), "m/s"), $cgi->param("wdir"));
	$u = Conversions::convertVelocity($u, "m/s",$cgi->param("uvOutUnits"));
	$v = Conversions::convertVelocity($v, "m/s",$cgi->param("uvOutUnits"));
	$full_value = sprintf("U: <font size=+1>%f</font> %s, V: <font size=+1>%f</font> %s", $u, $cgi->param("uvOutUnits"), $v, $cgi->param("uvOutUnits"));
	$qcf_value = sprintf("U: <font size=+1>%7.2f</font> %s, V: <font size=+1>%7.2f</font> %s", $u, $cgi->param("uvOutUnits"), $v, $cgi->param("uvOutUnits"));
    } elsif (defined($cgi->param("wspeed"))) {
	$qcf_value = sprintf("Wind speed %s is not a number.",
			     $cgi->param("wspeed"));
    } elsif (defined($cgi->param("wdir"))) {
	$qcf_value = sprintf("Wind direction %s is not a number.",
			     $cgi->param('wdir'));
    }

    # Create the results table
    print "<table><tr><td><b><font size=+1>$qcf_title</font></b></td><td>";
    print "<b><i>$qcf_value</i></b></td></tr>\n";
    print "<tr><td><b><font size=+1>$full_title</font></b></td><td>";
    print "<b><i>$full_value</i></b></td></tr></table>\n<br>\n";

    # Create the calculations table
    print "<table border=5 width=100%><tr bgcolor=#336699>";
    print "<th><font color=white size=+1>Calculations</th></font></tr><td>\n";
    print "ucomp = -1 * sin(PI / 180 * (wind_dir)) * wind_speed<br>\n";
    print "vcomp = -1 * cos(PI / 180 * (wind_dir)) * wind_speed\n</td>";
    print "</table>";
}
