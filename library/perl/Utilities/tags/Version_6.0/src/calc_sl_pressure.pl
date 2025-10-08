#! /usr/bin/perl -w

##Module--------------------------------------------------------------------
# <p>Call the Conversions::calculateSeaLevelPressure function from the 
# command line.</p>
#
# @author Joel Clawson
##Module--------------------------------------------------------------------
use strict;
use lib ".";
use Conversions;

&main();

##---------------------------------------------------------------------------
# @signature void main(float press, float elev, float dewpt, float temp)
# <p>Calculate the sea level pressure from the station pressure, elevetion,
# dew point, and temperature and print the result to the screen.</p>
#
# @input $press The station pressure in mbar.
# @input $elev The elevation of the station in m.
# @input $dewpt The dew point in degrees C.
# @input $temp The temperature in degrees C.
##--------------------------------------------------------------------------
sub main {
    if (scalar(@ARGV) != 4) {
	printf("Usage: calc_dew_point.pl <pressure> <elevation> <dew_point> <temperature>\n");
	exit(1);
    }

    printf("%f\n", Conversions::calculateSeaLevelPressure(@ARGV));
}
