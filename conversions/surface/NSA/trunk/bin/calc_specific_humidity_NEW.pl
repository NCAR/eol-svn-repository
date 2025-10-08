#!/bin/perl
#
# calc_specific_humidity_NEW.pl
# Calculate specific humidity from dew point(C) and surface pressure (mb)
# Specific humidity is output in kg/kg, else the string "-999.99" is output.
#
# Created 21 Nov 2002, JAG
#
# Updated 21 May 2003, JAG 
#    To use Bolton instead of the AMS Glossary of Meteorology
# rev 10 May 04, ds
#   added flag values for input parameters, which
#     affect final flag for the calculated value
#
# Modified to be used either as a standalone script or as a subroutine
# in another perl code. To use this as a subroutine in a piece of perl code,
# add the following lines to the top of the Perl code which uses the subroutine.
#
# rev 25 Jun 2008, SJS
#    now returns a reference to a hash where the keys are specific_humidity
#    and specific_humidity_flag
#
#   #-----------------------------
#   # get our subroutines in
#   #-----------------------------
#   unshift (@INC, ".");
#   require ("./calc_specific_humidity_NEW.pl");
#

#-------------------------------------------------------------------
# Only execute this code if this program is being called as a standalone
# routine.  If it is being called from within another piece of code using
# the above lines, then only execute the subroutine.
#-------------------------------------------------------------------

if ($0 =~ /calc_specific_humidity_NEW.pl/) {
    if (@ARGV <2) {
        print STDERR "Usage: $0 Dew_Point(C) Surface_Pressure(mb)\n";
        exit;
    }
    $DewPt = $ARGV[0];
    $DewPt_flag = $ARGV[1];
    $Surf_Press = $ARGV[2];
    $Surf_Press_flag = $ARGV[3];

    &calc_specific_humidity($DewPt,$DewPt_flag,$Surf_Press,$Surf_Press_flag);
    printf "%7.2f\n",$specific_humidity;
}

# This line assures that this routine returns true when included in a file
# via the "require" command.
1;


#-------------------------------------------------------------------
# compute_specific_humidity.c - Compute specific humidity
#    Note that the specific humidity is defined in the Glossary of 
#    Meteorology as "In a system of moist air, the (dimensionless) 
#    ratio of the mass of water vapor to the total mass of the system."
#    It can be approximated using the mixing ratio as w/(1+w). From
#    another source the specific humidity is defined as 
#    the ratio of the mass of vapor in a certain volume to
#    the total mass of air and vapor in the same volume:
#
    # As of May 21, 2003, we have decided that we do not want to use the
    # constants in the equation below to calc specific humidity.  Use Bolton
    # instead: e = 6.112*exp((17.67*Td)/(Td + 243.5));

#    e = 6.1078 * exp((17.2693882 * Td)/ (237.3 + Td))
#    q = (0.622 * e)/(p - (0.378 * e))
#
#    where:
#       e = vapor pressure (in mb)
#       Td = dew point (in deg C)
#       p = surface pressure (in mb)
#       q = specific humidity.
#
#    Reference:  (ask SL)
#
#  Input: Dew Point (C)
#         Surface Pressure (mb)
#
#  Output: specific humidity (kg/kg)
#
#------------------------------------------------------------------*/
sub calc_specific_humidity {

    $e = 0.00000;
    my $dewpnt = $_[0];
    my $dewpnt_flag = $_[1];
    my $staprs = $_[2];
    my $staprs_flag = $_[3];

    my %hash;

    my $flag_precedence = {"M"=>11, "N"=>10, "C"=>9, "I"=>8, "X"=>7, "B"=>6, "E"=>5, "D"=>4, "U"=>3, "G"=>2, "T"=>1};

    if ($staprs < 0.0 || $dewpnt <-999.00 ) {
#		print("staprs = $staprs, dewpnt = $dewpnt, setting specific humidity missing\n");
#        $specific_humidity = "-999.99";
#	$specific_humidity_flag = "M";
      $hash{'specific_humidity'} = "-999.99";
      $hash{'specific_humidity_flag'} = "M";
      return \%hash;
    } # endif

   
    $e = 6.112 * exp((17.67 * $dewpnt)/(243.5 + $dewpnt));

    my $specific_humidity = (0.622 * $e)/($staprs - (0.378 * $e));
    my $specific_humidity_flag;

    #----------------------------------------------
	# Set flag
    #----------------------------------------------*/
    if ((${$flag_precedence}{$dewpnt_flag} >= ${$flag_precedence}{"G"}) ||
	 (${$flag_precedence}{$staprs_flag} >= ${$flag_precedence}{"G"})) {
        $flag_override = ${$flag_precedence}{$dewpnt_flag} > ${$flag_precedence}{$staprs_flag} ? $dewpnt_flag : $staprs_flag;
	$specific_humidity_flag = $flag_override;
    } else {
	$specific_humidity_flag = "U";
    } # endif

    if ($specific_humidity < 0.00 || $specific_humidity >0.04) {
       print ("spec hum = $specific_humidity, out of range\n");
       print STDERR "Specific humidity out of range: $specific_humidity ";
       print STDERR "Set to missing.\n";
       $specific_humidity = "-999.99";
       $specific_humidity_flag = "M";
    } # endif

    $hash{'specific_humidity'} = $specific_humidity;
    $hash{'specific_humidity_flag'} = $specific_humidity_flag;

    # return a reference to the specific humidity and corresponding flag
    return \%hash;

}
