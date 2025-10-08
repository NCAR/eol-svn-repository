    #---------------------------------------------------------------------------
    # following section fixes flags according to DQRs
    #---------------------------------------------------------------------------
    # E1
    #---------------------------------------------------------------------------
    } elsif ($id eq "E1") {
        if ( $var eq "up_long_hemisp" || $var eq "down_long_hemisp_shaded" || $var eq "up_short_hemisp" || $var eq "down_short_hemisp") {
           if ($datetime >= 20031014.2023 && $datetime <= 20031021.1930) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "down_long_hemisp_shaded") {
           if ($datetime >= 20031111.0438 && $datetime <= 20031118.0220) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "down_short_hemisp") {
           if (($datetime >= 20040202.1200 && $datetime <= 20040202.2000) ||
               ($datetime >= 20040210.2050 && $datetime <= 20040420.2015)) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "up_short_hemisp") {
           if ($datetime >= 20040210.2050 && $datetime <= 20040420.2015) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E2
    #---------------------------------------------------------------------------
    } elsif ($id eq "E2") {
        if ($var eq "down_short_hemisp") {
           if (($datetime >= 20040202.1200 && $datetime <= 20040202.2000) ||
               ($datetime >= 20040212.1612 && $datetime <= 20040325.1600)) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "up_short_hemisp") {
           if ($datetime >= 20040212.1612 && $datetime <= 20040325.1600) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E3
    #---------------------------------------------------------------------------
    } elsif ($id eq "E3") {
        if ($var eq "down_short_hemisp") {
           if ($datetime >= 20040202.1200 && $datetime <= 20040202.2000) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E5
    #---------------------------------------------------------------------------
    } elsif ($id eq "E5") {
        if ($var eq "down_short_hemisp") {
           if (($datetime >= 20040202.1200 && $datetime <= 20040202.2000) ||
               ($datetime >= 20040411.1200 && $datetime <= 20040505.1645)) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E7
    #---------------------------------------------------------------------------
    } elsif ($id eq "E7") {
        if ($var eq "down_short_hemisp") {
           if (($datetime >= 20031121.0317 && $datetime <= 20031203.0000) ||
               ($datetime >= 20040330.1430 && $datetime <= 20040406.0210) ||
               ($datetime >= 20040210.2050 && $datetime <= 20040420.2015)) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "down_long_hemisp_shaded") {
           if ($datetime >= 20040420.1825 && $datetime <= 20040629.1915) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "up_short_hemisp") {
           if ($datetime >= 20040210.2050 && $datetime <= 20040420.2015) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E8
    #---------------------------------------------------------------------------
    } elsif ($id eq "E8") {
        if ($var eq "down_short_hemisp") {
           if (($datetime >= 20040202.1200 && $datetime <= 20040202.2000) ||
               ($datetime >= 20040315.0150 && $datetime <= 20040331.1200) ||
               ($datetime >= 20040210.1825 && $datetime <= 20040420.1825)) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "up_short_hemisp") {
           if ($datetime >= 20040210.1825 && $datetime <= 20040420.1825) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E9
    #---------------------------------------------------------------------------
    } elsif ($id eq "E9") {
        if ($var eq "down_short_hemisp") {
           if (($datetime >= 20031014.0000 && $datetime <= 20031021.1600) ||
               ($datetime >= 20040220.1600 && $datetime <= 20040223.0500)) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E10
    #---------------------------------------------------------------------------
    } elsif ($id eq "E10") {
        if ($var eq "down_short_hemisp") {
           if ($datetime >= 20040210.1630 && $datetime <= 20040421.1425) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "up_short_hemisp") {
           if ($datetime >= 20040210.1630 && $datetime <= 20040421.1425) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E11
    #---------------------------------------------------------------------------
    } elsif ($id eq "E11") {
        if ($var eq "down_short_hemisp") {
           if (($datetime >= 20040220.1500 && $datetime <= 20040301.0130) ||
               ($datetime >= 20040217.1741 && $datetime <= 20040505.0000)) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "up_short_hemisp") {
           if ($datetime >= 20040217.1741 && $datetime <= 20040505.0000) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "up_long_hemisp") {
           if ($datetime >= 20040504.0000 && $datetime <= 20040622.1855) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E12
    #---------------------------------------------------------------------------
    } elsif ($id eq "E12") {
        if ( $var eq "up_long_hemisp" || $var eq "down_long_hemisp_shaded" || $var eq "up_short_hemisp" || $var eq "down_short_hemisp") {
           if ($datetime >= 20040219.2212 && $datetime <= 20040310.2247) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E15
    #---------------------------------------------------------------------------
    } elsif ($id eq "E15") {
        if ($var eq "down_short_hemisp") {
           if ($datetime >= 20040220.1900 && $datetime <= 20040229.1700) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E16
    #---------------------------------------------------------------------------
    } elsif ($id eq "E16") {
        if ($var eq "down_short_hemisp") {
           if ($datetime >= 20040206.2200 && $datetime <= 20040218.1650) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E21
    #---------------------------------------------------------------------------
    } elsif ($id eq "E21") {
        if ($var eq "down_short_hemisp") {
           if (($datetime >= 20031018.1600 && $datetime <= 20031029.1620) ||
               ($datetime >= 20031019.0000 && $datetime <= 20040226.2359) ||
               ($datetime >= 20040528.1500 && $datetime <= 20040609.1342)) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    }

    return $new_flag;
}
