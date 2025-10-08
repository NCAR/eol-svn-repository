#! /usr/bin/perl -w

##Module--------------------------------------------------------------------
# <p>The DpgConstants.pm module are a collection of functions that define
# constant values.  This provides a universal access point for the constant
# values for other modules.</p>
#
# @author Joel Clawson
# @version 3.0  <p>This is the original definition of the module.  It used
# to be in the Conversions.pm and only contained <b>$PI</b>.  All other
# constants were originally used in their own functions.</p>
##Module--------------------------------------------------------------------
package DpgConstants;
use strict;
require Exporter;
our @ISA = ("Exporter");
our @EXPORT = qw($TRUE $FALSE $EPS $ES0 $GRAVITY $PI $Rdry $Rvapor $StdSLPress $StdSLTemp $PRESS_LAPSE);

our $TRUE = 1;
our $FALSE = !$TRUE;

our $EPS = .62197;
our $ES0 = 6.1121;
our $GRAVITY = 9.80616; # m/s^2
our $PI = (4 * atan2(1,1));
our $Rdry = 287.04; # J/ kgK
our $Rvapor = 461.5; # J/kgK
our $StdSLPress = 1013.25; # mbar
our $StdSLTemp = 15; # deg C
our $PRESS_LAPSE = .0065; # deg C/m

1;
