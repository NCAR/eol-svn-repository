#!/bin/perl

#-----------------------------
# get our subroutines in
#-----------------------------
unshift (@INC, ".");
require ("calc_UV_winds-NEW.pl");


$wspd = 12.4;
$wspd_flag = "D";
$wdir = 82;
$wdir_flag = "B";

&calc_UV_winds($wspd, $wspd_flag, $wdir, $wdir_flag);
printf ("U wind and flag: %7.2f %s, V wind and flag: %7.2f %s\n", $U_wind, $U_wind_flag, $V_wind, $V_wind_flag);
