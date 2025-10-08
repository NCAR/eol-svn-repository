    #---------------------------------------------------------------------------
    # following section fixes flags according to DQRs
    #---------------------------------------------------------------------------
    # C1
    #---------------------------------------------------------------------------
    } elsif ($id eq "C1") {
        if ( $var eq "temp_60m" || $var eq "temp_25m" || $var eq "rh_60m" || $var eq "rh_25m") {
           if (($datetime >= 20031006.1713 && $datetime <= 20031006.1828) ||
               ($datetime >= 20031007.1530 && $datetime <= 20031007.1533) ||
               ($datetime >= 20031119.0214 && $datetime <= 20031119.2118) ||
               ($datetime >= 20040213.1751 && $datetime <= 20040213.2138) ||
               ($datetime >= 20040415.0215 && $datetime <= 20040415.2154) ||
               ($datetime >= 20040721.1527 && $datetime <= 20040721.1613) ||
               ($datetime >= 20040805.1412 && $datetime <= 20040805.1451)) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ( $var eq "temp_60m" || $var eq "temp_25m" || $var eq "rh_60m" || $var eq "rh_25m") {
           if (($datetime >= 20031208.1941 && $datetime <= 20031209.1422) ||
               ($datetime >= 20040210.1856 && $datetime <= 20040210.2038) ||
               ($datetime >= 20040220.1458 && $datetime <= 20040223.1840) ||
               ($datetime >= 20040927.1438 && $datetime <= 20040927.2056)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "temp_60m") {
           if ($datetime >= 20040209.2210 && $datetime <= 20040209.2215) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "rh_60m") {
           if ($datetime >= 20030815.1345 && $datetime <= 20040210.2310) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "rh_25m") {
           if ($datetime >= 20030815.1345 && $datetime <= 20040210.2310) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    }

    return $new_flag;
}
