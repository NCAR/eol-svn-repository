#-------------------------------------------------------------------
# calc_dewpoint.pl
#
# Originally taken from a Gempak algorithm.
# This perl version is based on the Aug 03 2000 version (Version5) of
# calc_dewpoint.c in /work/DPG_HTML/BEST_SW/GENERAL_TOOLS/Calc_SLP_DP_RH_src.
#
# Input: relative humidity 	percent(0-100)
#	 temperature		Celsius
#
# Output:dew point 		Celsius
#	 dew point flag		1 char
# 
# rev 26 Nov 03, ds
# 	customized by ds
#   flag changed for CEOP reference datasets: "I"->"M"
# rev 10 May 04, ds
#   added flag values for input parameters, which
#     affect final flag for the calculated value
# rev 13 Nov 09, ss
#   added code to return a hash containing the dew_pt and dew_pt flag
#   since the global variables no longer work with perl5x
#------------------------------------------------------------------*/
sub calc_dewpoint {

    my ($rel_humidity, $rh_flag, $temperature, $temp_flag) = @_;

    my (%hash, $dew_point, $dew_point_flag);

    $dew_point = -999.99;
    $dew_point_flag = 'M';						# no "I" values for CEOP, but "M"
    $flag_precedence = {"M"=>11, "N"=>10, "C"=>9, "I"=>8, "X"=>7, "B"=>6, "E"=>5, "D"=>4, "U"=>3, "G"=>2, "T"=>1};

    #--------------------------------------------------------
    # If any of the values passed in are NULL, then  
    # leave the default dew_point value as -999.99
    #--------------------------------------------------------
    if (!defined($rel_humidity) || !defined($temperature)) {
      $hash{'dew_pt'} = $dew_point;
      $hash{'dew_pt_flag'} = $dew_point_flag;
      return \%hash;
#     return $dew_point_flag;
    } # endif
        
    #--------------------------------------------------------
    # If relative humidity is missing (-999.99), then return 
    #-------------------------------------------------------*/
    if ($rel_humidity < -899.99) {
      $hash{'dew_pt'} = $dew_point;
      $hash{'dew_pt_flag'} = $dew_point_flag;
      return \%hash;
#        return;
    }

    #----------------------------------------------------------
    # If Relative Humidity is out-of-range, leave the Dew-Point 
    # as -999.99.
    # If Relative Humidity is zero, dew_pt calculation will
    # blow up yielding a NaN.
    #---------------------------------------------------------*/
    if (($rel_humidity <= 0.50) || ($rel_humidity > 100.0)) {
        printf STDERR "Invalid relative humidity = %7.2f", $rel_humidity;
        printf STDERR " Dew Point will not be calculated.\n";
        $hash{'dew_pt'} = $dew_point;
        $hash{'dew_pt_flag'} = $dew_point_flag;
        return \%hash;
        #return;
    }

    #-------------------------------------------------------------
    # If the recorded temperature is missing (-999.99),
    # then return leaving default dewpoint-qcf values as missing.
    #------------------------------------------------------------*/
    if ($temperature <= -999.98) {
       $hash{'dew_pt'} = $dew_point;
       $hash{'dew_pt_flag'} = $dew_point_flag;
       return \%hash;
      #return;
    }

    #--------------------------------------------------------
    # If Temperature is out-of-range, leave the Dew-Point as 
    # -999.99
    #-------------------------------------------------------*/
    if (($temperature < -100.0) || ($temperature > 100.0)) {
       printf STDERR "Invalid Temperature = %7.2f", $temperature;
       printf STDERR " Dew Point will not be calculated.\n";
       $hash{'dew_pt'} = $dew_point;
       $hash{'dew_pt_flag'} = $dew_point_flag;
       return \%hash;
       #return;
    }

    #-------------------------------------------------------------
    # If code reached this point then the temperature, and relative
    # humidity were both measured and are within the
    # acceptable range of values, so calculate dew point
    #------------------------------------------------------------*/

    #-------------------------------------------------------------
    # Compute Saturation Vapor Pressure from dry bulb temperature.
    #------------------------------------------------------------*/
    $sat_vapor_pressure = 6.112 * exp((17.67 * $temperature) / ($temperature + 243.5));

    #-----------------------------------------------
    # Compute Vapor Pressure from Relative Humidity.
    #----------------------------------------------*/
    $vapor_pressure = $sat_vapor_pressure * ($rel_humidity / 100.0);

    #-------------------------------------------
    # Compute Dew Point temperature in Celsius.
    #------------------------------------------*/
    $log_value = log($vapor_pressure / 6.112);
    $dew_point = $log_value * 243.5 / (17.67 - $log_value);

    #----------------------------------------------
	# Set flag
    #----------------------------------------------*/
    if ((${$flag_precedence}{$rh_flag} > ${$flag_precedence}{"G"}) ||
	    (${$flag_precedence}{$temp_flag} > ${$flag_precedence}{"G"})) {
			$flag_override = ${$flag_precedence}{$rh_flag} > ${$flag_precedence}{$temp_flag} ? $rh_flag : $temp_flag;
			$dew_point_flag = $flag_override;
	} else {
    	$dew_point_flag = 'U';
	}

    #----------------------------------------------
    # When qcf record is printed, dew points between 
    # 0.00 and -0.005 are printed as -0.00.  Force 
    # these values to print out as 0.00.
    #---------------------------------------------*/
    if ($dew_point < 0.00 && $dew_point > -0.005) {
		$dew_point = 0.0;
	}

    $hash{'dew_pt'} = $dew_point;
    $hash{'dew_pt_flag'} = $dew_point_flag;

    return \%hash;
}
1;
