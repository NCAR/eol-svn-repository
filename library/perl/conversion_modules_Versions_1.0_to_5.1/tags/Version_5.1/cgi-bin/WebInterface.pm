#! /usr/bin/perl -w


package WebInterface;
use strict;
use lib "/work/software/conversion_modules/Version3";
use CGI;
use DpgCalculations qw(:DEFAULT);
use DpgConversions qw(:DEFAULT);
use DpgDate qw(:DEFAULT);
use FileHandle;
our @ISA = ("CGI");

# Make error messages be printed to the screen
*STDERR = *STDOUT;

##------------------------------------------------------------------------------------------
# @signature String createEquationTable()
# <p>Create the HTML that defines the table and internal frame that is
# used to display the equations that are used in the calculation or
# conversion.</p>
#
# @output $table The HTML table code.
##------------------------------------------------------------------------------------------
sub createEquationTable {
    my $self = shift;
    my $table = "<table><tbody>\n";

    $table .= sprintf("<tr><th>Equations</th></tr>\n");
    $table .= sprintf("<tr><td class=equation><iframe src=\"/dpg/TOOLS/conversions/%s\">If internal frames do not work, click <a href=\"javascript: openwindow('/dpg/TOOLS/conversions/%s')\">here</a> to see the equation.</iframe></td></tr>\n",$self->getEquationPage(),$self->getEquationPage());

    $table .= "</tbody></table>\n";

    return $table;
}

##------------------------------------------------------------------------------------------
# @signature String createForm()
# <p>Create the HTML that defines the parts of the form.</p>
#
# @output $form The HTML form code.
##------------------------------------------------------------------------------------------
sub createForm { return "Insert Form Here!\n"; }

##------------------------------------------------------------------------------------------
# @signature String getAngleUnits(String label)
# <p>Get the popup menu that contains the angle units.</p>
#
# @input $label The name of the popup menu.
# @output $menu The popup menu.
##------------------------------------------------------------------------------------------
sub getAngleUnits {
    my $self = shift;
    return $self->popup_menu(-name   => $_[0],
			     -values => ["deg","rad"]);
}

##------------------------------------------------------------------------------------------
# @signature String getAreaUnits(String label)
# <p>Get the popup menu that contains the area units.</p>
#
# @input $label The name of the popup menu.
# @output $menu The popup menu.
##------------------------------------------------------------------------------------------
sub getAreaUnits {
    my $self = shift;
    return $self->popup_menu(-name   => $_[0],
			     -values => ["m2","ft2","cm2","in2","mm2","yd2","km2","mile2","acres"]);
}

##------------------------------------------------------------------------------------------
# @signature String getCGI()
# <p>Get the name of the script that is to be run when the form is submitted.</p>
# @output $cgi The cgi script name.
##------------------------------------------------------------------------------------------
sub getCGI { return ""; }

##------------------------------------------------------------------------------------------
# @signature String getDirectoryList()
# <p>Create the list of conversions and calculations to be listed in the menu
# for all of the web scripts.</p>
# @output $list The list of web scripts.
##------------------------------------------------------------------------------------------
sub getDirectoryList {
    my $self = shift;
    my $list = "";

    $list .= "<table><tbody>\n";
    $list .= "<tr><th>Calculations</th></tr>\n";
    $list .= "<tr><td><a href=\"../calculations/airdensity\">Air Density</a></td></tr>\n";
    $list .= "<tr><td><a href=\"../calculations/altitude\">Altitude</a></td></tr>\n";
    $list .= "<tr><td><a href=\"../calculations/dewpoint\">Dew Point</a></td></tr>\n";
    $list .= "<tr><td><a href=\"../calculations/mixingratio\">Mixing Ratio</a></td></tr>\n";
    $list .= "<tr><td><a href=\"../calculations/relativehumidity\">Relative Humidity</a></td></tr>\n";
    $list .= "<tr><td><a href=\"../calculations/sealevelpressure\">Sea Level Pressure</a></td></tr>\n";
    $list .= "<tr><td><a href=\"../calculations/specifichumidity\">Specific Humidity</a></td></tr>\n";
    $list .= "<tr><td><a href=\"../calculations/stationpressure\">Station Pressure</a></td></tr>\n";
    $list .= "<tr><td><a href=\"../calculations/uvwinds\">UV Wind Components</a></td></tr>\n";
    $list .= "<tr><td><a href=\"../calculations/vaporpressure\">Vapor Pressure</a></td></tr>\n";
    $list .= "<tr><td><a href=\"../calculations/virtualtemperature\">Virtual Temperature</a></td></tr>\n";
    $list .= "<tr><td><a href=\"../calculations/windvalues\">Wind Speed/Direction</a></td></tr>\n";

    $list .= "<tr><th>Conversions</th></tr>\n";
    $list .= "<tr><td><a href=\"../conversions/angles\">Angles</a></td></tr>\n";
    $list .= "<tr><td><a href=\"../conversions/area\">Area</a></td></tr>\n";
    $list .= "<tr><td><a href=\"../conversions/flow\">Flow</a></td></tr>\n";
    $list .= "<tr><td><a href=\"../conversions/length\">Length/Distance</a></td></tr>\n";
    $list .= "<tr><td><a href=\"../conversions/pressure\">Pressure</a></td></tr>\n";
    $list .= "<tr><td><a href=\"../conversions/radiation\">Radiation</a></td></tr>\n";
    $list .= "<tr><td><a href=\"../conversions/temperature\">Temperature</a></td></tr>\n";
    $list .= "<tr><td><a href=\"../conversions/time\">Time</a></td></tr>\n";
    $list .= "<tr><td><a href=\"../conversions/velocity\">Velocity</a></td></tr>\n";
    $list .= "<tr><td><a href=\"../conversions/volume\">Volume</a></td></tr>\n";

    $list .= "<tr><th>Other Conversions</th></tr>\n";
    $list .= "<tr><td><a href=\"../other/latlong\">Latitude/Longitude</a></td></tr>\n";
    $list .= "<tr><td><a href=\"../other/julian\">Julian</a></td></tr>\n";
    $list .= "<tr><td><a href=\"../other/statecode\">State Code</a></td></tr>\n";

    $list .= "</tbody></table>\n";

    return $list;
}

##------------------------------------------------------------------------------------------
# @signature String getEquationsPage()
# <p>Get the name of the page where the equation for the web script is stored.</p>
# @output $page The name of the equation page.
##------------------------------------------------------------------------------------------
sub getEquationPage { return "equations_calc.html"; }

##------------------------------------------------------------------------------------------
# @signature String getFlowUnits(String label)
# <p>Get the popup menu that contains the flow units.</p>
#
# @input $label The name of the popup menu.
# @output $menu The popup menu.
##------------------------------------------------------------------------------------------
sub getFlowUnits {
    my $self = shift;
    return $self->popup_menu(-name   => $_[0],
			     -values => ["m3/s","ft3/s"]);
}

##------------------------------------------------------------------------------------------
# @signature String getInstructions()
# <p>Get the instructions on how to use the web scripts.</p>
# @output $instr The instructions.
##------------------------------------------------------------------------------------------
sub getInstructions { return "Insert Instructions Here!\n"; }

##------------------------------------------------------------------------------------------
# @signature String getJavascript()
# <p>Get the javascript code that is needed to run the web scripts.</p>
# @output $js The javascript functions.
##------------------------------------------------------------------------------------------
sub getJavascript {
    my $self = shift;
    my $js = "function checkValue(label,item) {
                  var number = /^-?[0-9]+[\.]?[0-9]*\$/;
                  var decimal = /^-?[\.][0-9]+\$/;
                  if (!number.exec(item.value) && !decimal.exec(item.value)) {
                       alert(\"Value (\"+item.value+\") is not a number for \"+label+\".\");
                       return false;
                  }
                  return true;
              }

              function checkRhWindSpd(label,item) {
                  if (checkValue(label,item)) {
                      if (item.value > 0) { return true; }
                      alert(\"Value (\"+item.value+\") is <= 0 for \"+label+\".\");
                  }
                  return false;
              }

              function checkWindDirection(label,item) {
                  if (checkValue(label,item)) {
                      if (0 <= item.value && item.value <= 360) { return true; }
                      alert(\"Value (\"+item.value+\") is not in [0,360] for \"+label+\".\");
                  }
                  return false;
              }

              function checkAltitudeDewPoint(label,item) {
                  if (item.value == \"\") { return true; }
                  return checkValue(label,item);
              }\n";
    return $js;
}

##------------------------------------------------------------------------------------------
# @signature String getLastEditDate()
# <p>Get the date when the web scripts were last edited.</p>
# @output $date The last edit date.
##------------------------------------------------------------------------------------------
sub getLastEditDate { return "Feb 25 2005"; }

##------------------------------------------------------------------------------------------
# @siganture String getLastEditor()
# <p>Get the name of the person who last edited the web scripts.</p>
# @output $name The name of the last editor.
##------------------------------------------------------------------------------------------
sub getLastEditor { return "Joel Clawson"; }

##------------------------------------------------------------------------------------------
# @signature String getLengthUnits(String label)
# <p>Get the popup menu that contains the length units.</p>
#
# @input $label The name of the popup menu.
# @output $menu The popup menu.
##------------------------------------------------------------------------------------------
sub getLengthUnits {
    my $self = shift;
    return $self->popup_menu(-name   => $_[0],
			     -values => ["m","ft","mm","hi","ti","cm","in","dm","yd","Hft","mile","km","nmile"]);
}

##------------------------------------------------------------------------------------------
# @signature String getOnSubmit()
# <p>Get the javascript command to execute when the form is submitted.</p>
# @output $js The submit javascript command.
##------------------------------------------------------------------------------------------
sub getOnSubmit { return "javascript: alert('On Submit not overridden'); return true;"; }

##------------------------------------------------------------------------------------------
# @signature String getNotes()
# <p>Get the general notes about all of the web scripts.</p>
# @output $notes The notes about the web scripts.
##------------------------------------------------------------------------------------------
sub getNotes {
    my $self = shift;
    my $notes = "<p class=notes>This script uses Version 3 of the conversion modules.  For more information on the modules, go <a href=".">here</a>.</p>\n";

    $notes .= sprintf("<p class=footnote>This page was last updated on %s by %s.</p>\n",$self->getLastEditDate(),$self->getLastEditor());

    return $notes;
}

##------------------------------------------------------------------------------------------
# @signature String getPressureUnits(String label)
# <p>Get the popup menu that contains the pressure units.</p>
#
# @input $label The name of the popup menu.
# @output $menu The popup menu.
##------------------------------------------------------------------------------------------
sub getPressureUnits {
    my $self = shift;
    return $self->popup_menu(-name   => $_[0],
			     -values => ["mbar","inHg","hPa","Pa","mmHg","atm","bar","kPa","dyne/cm2"]);
}

##------------------------------------------------------------------------------------------
# @signature String getRadiationUnits(String label)
# <p>Get the popup menu that contains the radiation units.</p>
#
# @input $label The name of the popup menu.
# @output $menu The popup menu.
##------------------------------------------------------------------------------------------
sub getRadiationUnits {
    my $self = shift;
    return $self->popup_menu(-name   => $_[0],
			     -values => ["langly","w/m2"]);
}

##------------------------------------------------------------------------------------------
# @signature String getRelativeHumidityUnits(String label)
# <p>Get the popup menu that contains the relative humidity units.</p>
#
# @input $label The name of the popup menu.
# @output $menu The popup menu.
##------------------------------------------------------------------------------------------
sub getRelativeHumidityUnits { 
    my $self = shift;
    return $self->popup_menu(-name   => $_[0],
			     -values => ["%"]);
}

##------------------------------------------------------------------------------------------
# @signature String getStyleSheet()
# <p>Get the location of the style sheet to use for the script.</p>
# @output $css The style sheet location.
##------------------------------------------------------------------------------------------
sub getStyleSheet { return "/dpg/TOOLS/conversions/conv.css"; }

##------------------------------------------------------------------------------------------
# @signature String getTemperatureUnits(String label)
# <p>Get the popup menu that contains the temperature units.</p>
#
# @input $label The name of the popup menu.
# @output $menu The popup menu.
##------------------------------------------------------------------------------------------
sub getTemperatureUnits {
    my $self = shift;
    return $self->popup_menu(-name   => $_[0],
			     -values => ["C","F","K"]);
}

##------------------------------------------------------------------------------------------
# @signature String getTitle()
# <p>Get the title of the page.</p>
# @output $title The web page title.
##------------------------------------------------------------------------------------------
sub getTitle { return "Web Interface for the Conversion Modules"; }

##------------------------------------------------------------------------------------------
# @signature String getTimeUnits(String label)
# <p>Get the popup menu that contains the time units.</p>
#
# @input $label The name of the popup menu.
# @output $menu The popup menu.
##------------------------------------------------------------------------------------------
sub getTimeUnits {
    my $self = shift;
    return $self->popup_menu(-name   => $_[0],
			     -values => ["sec","min","hr","day"]);
}

##------------------------------------------------------------------------------------------
# @signature String getVelocityUnits(String label)
# <p>Get the popup menu that contains the velocity units.</p>
#
# @input $label The name of the popup menu.
# @output $menu The popup menu.
##------------------------------------------------------------------------------------------
sub getVelocityUnits {
    my $self = shift;
    return $self->popup_menu(-name   => $_[0],
			     -values => ["m/s","mph","knot","ft/s","km/hr"]);
}

##------------------------------------------------------------------------------------------
# @signature String getVolumeUnits(String label)
# <p>Get the popup menu that contains the volume units.</p>
#
# @input $label The name of the popup menu.
# @output $menu The popup menu.
##------------------------------------------------------------------------------------------
sub getVolumeUnits {
    my $self = shift;
    return $self->popup_menu(-name   => $_[0],
			     -values => ["m3","ft3","cm3","in3","ml","liter","oz","pt","qt","gal","af"]);
}

##------------------------------------------------------------------------------------------
# @signature void makePage()
# <p>Create the web page that will allow a user to convert or
# calculate some value using the conversion modules.</p>
##------------------------------------------------------------------------------------------
sub makePage {
    my $web = shift;
    
    printf("%s\n",$web->header());
    printf("<html>\n<head>\n\t<title>%s</title>\n",$web->getTitle());
    printf("\t<link rel=stylesheet type=text/css href=%s>\n",
	   $web->getStyleSheet());
    printf("\t<script type=text/javascript>\n%s\t</script>\n",$web->getJavascript());
    printf("</head>\n\n<body>\n");

    printf("<div id=instructions>\n%s%s</div>\n",$web->getInstructions(),
	   $web->getNotes());

    printf("<div id=directory>\n%s</div>\n",$web->getDirectoryList());

    printf("<div id=header>\n<h1>%s</h1>\n</div>\n",$web->getTitle());


    printf("\n<!-- Form -->\n<div id=dataform>\n");
    printf("<form method=POST action=/cgi-bin/dpg/conversions/%s",
	   $web->getCGI());
    printf(" enctype=application/x-www-form-urlencoded\n   onsubmit=\"%s\">\n\n",
	   $web->getOnSubmit());
    printf("%s</form>\n</div>\n\n",$web->createForm());

    printf("\n<!-- Data Table -->\n<div id=datatable>\n%s</div>\n\n",
	   $web->createEquationTable());


    printf("</body>\n</html>");
}
