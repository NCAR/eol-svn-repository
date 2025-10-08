    #---------------------------------------------------------------------------
    # following section fixes flags according to DQRs
    #---------------------------------------------------------------------------
    # E3
    #---------------------------------------------------------------------------
    } elsif ($id eq "E3") {
        if ($var eq "watcont_W") {
           if (($datetime >= 20040114.0500 && $datetime <= 20040114.0800) ||
               ($datetime >= 20040114.1800 && $datetime <= 20040114.2100) ||
               ($datetime >= 20040307.1000 && $datetime <= 20040310.1715) ||
               ($datetime >= 20040602.1700 && $datetime <= 20040606.1300) ||
               ($datetime >= 20040610.2000 && $datetime <= 20040610.2100)) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E4
    #---------------------------------------------------------------------------
    } elsif ($id eq "E4") {
        if ($var eq "watcont_W") {
           if (($datetime >= 20030910.1600 && $datetime <= 20031008.1600) ||
               ($datetime >= 20040212.1500 && $datetime <= 20040212.1900)) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E7
    #---------------------------------------------------------------------------
    } elsif ($id eq "E7") {
        if ($var eq "watcont_W") {
           if ($datetime >= 20040608.1900 && $datetime <= 20040608.2000) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E12
    #---------------------------------------------------------------------------
    } elsif ($id eq "E12") {
        if ($var eq "tsoil_W") {
           if ($datetime >= 20040219.2212 && $datetime <= 20040310.2247) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "watcont_W") {
           if ($datetime >= 20040219.2212 && $datetime <= 20040310.2247) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E20
    #---------------------------------------------------------------------------
    } elsif ($id eq "E20") {
        if ($var eq "watcont_W") {
           if ($datetime >= 20040709.1800 && $datetime <= 20040721.1900) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E24
    #---------------------------------------------------------------------------
    } elsif ($id eq "E24") {
        if ($var eq "watcont_W") {
           if (($datetime >= 20040205.1700 && $datetime <= 20040219.1700) ||
               ($datetime >= 20040430.2200 && $datetime <= 20040513.1900) ||
               ($datetime >= 20040513.1800 && $datetime <= 20040513.1900)) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E27
    #---------------------------------------------------------------------------
    } elsif ($id eq "E27") {
        if ($var eq "watcont_W") {
           if ($datetime >= 20030820.1700 && $datetime <= 20040218.1700) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    }

    return $new_flag;
}
