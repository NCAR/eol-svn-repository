#! /usr/bin/perl -w

##Module--------------------------------------------------------------------
# <p>Call the Conversions::calculateDewPoint function from the command line.
# </p>
#
# @author Joel Clawson
##Module--------------------------------------------------------------------
use strict;
use lib ".";
use Conversions;

&main();

##---------------------------------------------------------------------------
# @signature void main(float temp, float rel_humid)
# <p>Calculate the dew point from the temperature and relative humidity and
# print the result to the screen.</p>
#
# @input $temp The temperature in degrees C.
# @input $rel_humid The relative humidity in percent.
##--------------------------------------------------------------------------
sub main {
    if (scalar(@ARGV) != 2) {
	printf("Usage: calc_dew_point.pl <temperature> <relative_humidity>\n");
	exit(1);
    }

    printf("%f\n", Conversions::calculateDewPoint(@ARGV));
}
