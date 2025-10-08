    #---------------------------------------------------------------------------
    # following section fixes flags according to DQRs
    #---------------------------------------------------------------------------
    # E1
    #---------------------------------------------------------------------------
    } elsif ($id eq "E1") {
        if ( $var eq "h" || $var eq "lv_e" || $var eq "fc") {
           if ($datetime >= 20041019.2030 && $datetime <= 20041025.1735) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E3
    #---------------------------------------------------------------------------
    } elsif ($id eq "E3") {
        if ($var eq "lv_e" || $var eq "fc") {
           if ($datetime >= 20040616.1730 && $datetime <= 20040630.1730) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ( $var eq "h" || $var eq "lv_e" || $var eq "fc") {
           if ($datetime >= 20041219.1353 && $datetime <= 20041220.0931) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E10
    #---------------------------------------------------------------------------
    } elsif ($id eq "E10") {
        if ( $var eq "h" || $var eq "lv_e" || $var eq "fc") {
           if ($datetime >= 20031203.2200 && $datetime <= 20031205.2030) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ( $var eq "h" || $var eq "lv_e" || $var eq "fc") {
           if ($datetime >= 20040714.0100 && $datetime <= 20040813.1800) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E16
    #---------------------------------------------------------------------------
    } elsif ($id eq "E16") {
        if ( $var eq "h" || $var eq "lv_e" || $var eq "fc") {
           if ($datetime >= 20040112.0000 && $datetime <= 20040428.1500) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E24
    #---------------------------------------------------------------------------
    } elsif ($id eq "E24") {
        if ( $var eq "h" || $var eq "lv_e" || $var eq "fc") {
           if ($datetime >= 20040604.1000 && $datetime <= 20040916.1730) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    }

    return $new_flag;
}
