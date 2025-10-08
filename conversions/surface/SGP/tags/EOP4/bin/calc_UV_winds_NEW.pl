#!/bin/perl -w

#-------------------------------------------------------------------
# calc_UV_winds-NEW.pl
#
# Calculate U and V wind components from the wind speed and
# direction.
#
# Converted from C 02 Dec 2002 JAG
#
# This code can be used either as a standalone script or as a subroutine
# in another perl code. To use this as a subroutine in a piece of perl code,
# add the following lines to the top of the Perl code which uses the subroutine.
#
#   #-----------------------------
#   # get our subroutines in
#   #-----------------------------
#   unshift (@INC, ".");
#   require ("./calc_UV_winds-NEW.pl");
#
# rev 9/03, ds
# 	added flags for CEOP reference format
# rev 10 May 04, ds
#   added flag values for input parameters, which
#     affect final flag for the calculated value
#-------------------------------------------------------------------

#-------------------------------------------------------------------
# Only execute this code if this program is being called as a standalone
# routine.  If it is being called from within another piece of code using
# the above lines, then only execute the subroutine.
#-------------------------------------------------------------------

if ($0 =~ /calc_UV_winds-NEW.pl/) {
    if (@ARGV <2) {
        print STDERR "Usage: $0 Wind_Speed(m/s) wspd_flag Wind_Direction(degrees) wdir_flag\n";
        exit;
    }
    $wspd = $ARGV[0];
    $wspd_flag = $ARGV[1];
    $wdir = $ARGV[2];
    $wdir_flag = $ARGV[3];

    &calc_UV_winds($wspd, $wspd_flag, $wdir, $wdir_flag);
    printf "%7.2f %7.2f\n",$U_wind,$V_wind;
}

#-------------------------------------------------------------------
# This line assures that this routine returns true when included in a file
# via the "require" command.
#-------------------------------------------------------------------
1;

#-------------------------------------------------------------------
#  From GEMPAK:
#     U = -sin(direction) * wind_speed
#     V = -cos(direction) * wind_speed
#
#     Note that the C sin and cos functions expect radians
#     and the input values are degrees.  Also note that by meteorological
#     convention, zero degrees is winds from the North (i.e. a vector
#     pointing South), and 90 degrees is winds from the East.
#
#  Input: wind speed 		m/s
#         wind direction 	degrees
#
#  Output: U wind component	m/s
#          U wind flag
#          V wind component	m/s
#          V wind flag
#
#-------------------------------------------------------------------
sub calc_UV_winds
   {

    my $wspd = $_[0];
    my $wspd_flag = $_[1];
    my $wdir  = $_[2];
    my $wdir_flag  = $_[3];

    $DEG_TO_RAD = 0.017453292;
	$flag_precedence = {"M"=>11, "N"=>10, "C"=>9, "I"=>8, "X"=>7, "B"=>6, "E"=>5, "D"=>4, "U"=>3, "G"=>2, "T"=>1};

   #-------------------------------------------------------------------
   # Note that wind direction should always be between 0 and
   # 360 degrees. Wind speed should always be positive
   # except when missing (-999.99).
   #-------------------------------------------------------------------
   if ($wspd < 0.0 || $wdir < 0.0  || $wdir > 360.0)
      {
      $U_wind = "-999.99";
	  $U_wind_flag = "M";
      $V_wind = "-999.99";
	  $V_wind_flag = "M";
      return;
      }	elsif ((${$flag_precedence}{$wspd_flag} > ${$flag_precedence}{"G"}) ||
	 		   (${$flag_precedence}{$wdir_flag} > ${$flag_precedence}{"G"})) {
		$flag_override = ${$flag_precedence}{$wspd_flag} > ${$flag_precedence}{$wdir_flag} ? $wspd_flag : $wdir_flag;
#		print "flag precedence values: wspd = ${$flag_precedence}{$wspd_flag}\n";
#		print "flag precedence values: wdir = ${$flag_precedence}{$wdir_flag}\n";
       	$U_wind_flag = $flag_override;
       	$V_wind_flag = $flag_override;
	  } else {
	  	$U_wind_flag = "U";
	  	$V_wind_flag = "U";
	  }

   #-------------------------------------------------------------------
   # Compute the wind components
   #-------------------------------------------------------------------
   $U_wind = -1.0*sin($DEG_TO_RAD * $wdir) * $wspd;
   $V_wind = -1.0*cos($DEG_TO_RAD * $wdir) * $wspd;


   #-------------------------------------------------------------------
   # WARNING: Negative zeroes can be computed.
   #-------------------------------------------------------------------
   if ($U_wind > -0.005 && $U_wind < 0.005) {$U_wind = 0.00;}
   if ($V_wind > -0.005 && $V_wind < 0.005) {$V_wind = 0.00;}

   #-------------------------------------------------------------------
   # Check to be sure computed values are within reasonable limits.  If
   # not, warn user.
   #-------------------------------------------------------------------
   if ($U_wind < -200.00 || $U_wind > 200.00) {
       print "WARNING: Calculated U wind component is very large: $U_wind\n";
   }
   if ($V_wind < -200.00 || $V_wind > 200.00) {
       print "WARNING: Calculated V wind component is very large: $V_wind\n";
   }

   } # end compute U, V components
