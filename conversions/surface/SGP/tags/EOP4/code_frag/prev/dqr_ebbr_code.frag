    #---------------------------------------------------------------------------
    # following section fixes flags according to DQRs
    #---------------------------------------------------------------------------
    # E2
    #---------------------------------------------------------------------------
    } elsif ($id eq "E2") {
        if ( $var eq "c_shf1" || $var eq "g1" || $var eq "e" || $var eq "h") {
           if ($datetime >= 20031009.1800 && $datetime <= 20031009.1800) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "h" || $var eq "e") {
           if (($datetime >= 20031009.1700 && $datetime <= 20031009.1730) ||
               ($datetime >= 20031214.1500 && $datetime <= 20031214.2200) ||
               ($datetime >= 20040831.1300 && $datetime <= 20040909.1500) ||
               ($datetime >= 20041021.1500 && $datetime <= 20041021.1530)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "h" || $var eq "e") {
           if ($datetime >= 20041002.0630 && $datetime <= 20041021.1500) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "c_shf1" || $var eq "g1") {
           if ($datetime >= 20040411.1830 && $datetime <= 20040601.1800) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E4
    #---------------------------------------------------------------------------
    } elsif ($id eq "E4") {
        if ($var eq "h" || $var eq "e") {
           if (($datetime >= 20031008.1530 && $datetime <= 20031008.1645) ||
               ($datetime >= 20041020.1600 && $datetime <= 20041020.1630)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "h" || $var eq "e") {
           if ($datetime >= 20040725.1200 && $datetime <= 20040728.1500) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E7
    #---------------------------------------------------------------------------
    } elsif ($id eq "E7") {
        if ($var eq "h" || $var eq "e") {
           if (($datetime >= 20030727.1330 && $datetime <= 20031007.2000) ||
               ($datetime >= 20031013.0000 && $datetime <= 20031118.2000)) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "h" || $var eq "e") {
           if (($datetime >= 20031019.1730 && $datetime <= 20031019.1930) ||
               ($datetime >= 20031020.1330 && $datetime <= 20031020.1630) ||
               ($datetime >= 20031021.1630 && $datetime <= 20031021.1830) ||
               ($datetime >= 20031022.0000 && $datetime <= 20031022.1830) ||
               ($datetime >= 20031023.0000 && $datetime <= 20031023.1630) ||
               ($datetime >= 20031104.2200 && $datetime <= 20031104.2230) ||
               ($datetime >= 20040810.1925 && $datetime <= 20040824.1820) ||
               ($datetime >= 20041005.2000 && $datetime <= 20041005.2030)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E8
    #---------------------------------------------------------------------------
    } elsif ($id eq "E8") {
        if ($var eq "h" || $var eq "e") {
           if (($datetime >= 20031007.1700 && $datetime <= 20031007.1735) ||
               ($datetime >= 20031213.1400 && $datetime <= 20031214.2200) ||
               ($datetime >= 20040830.1200 && $datetime <= 20040907.1730) ||
               ($datetime >= 20041019.1730 && $datetime <= 20041019.1800)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "h" || $var eq "e") {
           if ($datetime >= 20040405.0000 && $datetime <= 20040406.1755) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E9
    #---------------------------------------------------------------------------
    } elsif ($id eq "E9") {
        if ($var eq "h" || $var eq "e") {
           if (($datetime >= 20031104.1730 && $datetime <= 20031104.1800) ||
               ($datetime >= 20031214.1000 && $datetime <= 20031214.1930) ||
               ($datetime >= 20040628.1200 && $datetime <= 20040629.1530) ||
               ($datetime >= 20041005.1630 && $datetime <= 20041005.1700)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E12
    #---------------------------------------------------------------------------
    } elsif ($id eq "E12") {
        if ( $var eq "c_shf1" || $var eq "g1" || $var eq "e" || $var eq "h") {
           if ($datetime >= 20040219.2212 && $datetime <= 20040310.2247) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "h" || $var eq "e") {
           if (($datetime >= 20031014.1300 && $datetime <= 20031014.1800) ||
               ($datetime >= 20031028.1750 && $datetime <= 20031028.1845) ||
               ($datetime >= 20041012.1630 && $datetime <= 20041012.1700)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E13
    #---------------------------------------------------------------------------
    } elsif ($id eq "E13") {
        if ($var eq "h" || $var eq "e") {
           if (($datetime >= 20030929.1800 && $datetime <= 20031002.1845) ||
               ($datetime >= 20031013.1830 && $datetime <= 20031013.1900) ||
               ($datetime >= 20031214.1730 && $datetime <= 20031214.1830) ||
               ($datetime >= 20040811.0230 && $datetime <= 20040811.1824) ||
               ($datetime >= 20040908.1200 && $datetime <= 20040915.1930) ||
               ($datetime >= 20041028.1830 && $datetime <= 20041028.1900)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E15
    #---------------------------------------------------------------------------
    } elsif ($id eq "E15") {
        if ( $var eq "c_shf1" || $var eq "g1" || $var eq "e" || $var eq "h") {
           if ($datetime >= 20011011.1500 && $datetime <= 20040928.1509) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "h" || $var eq "e") {
           if (($datetime >= 20031011.2230 && $datetime <= 20031012.2330) ||
               ($datetime >= 20031105.1400 && $datetime <= 20031111.1800) ||
               ($datetime >= 20031014.1700 && $datetime <= 20031014.1730) ||
               ($datetime >= 20031213.1500 && $datetime <= 20031215.0000) ||
               ($datetime >= 20041026.1630 && $datetime <= 20041026.1700)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "h" || $var eq "e") {
           if (($datetime >= 20031011.2230 && $datetime <= 20031012.2359) ||
               ($datetime >= 20031013.0000 && $datetime <= 20031014.1830)) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E18
    #---------------------------------------------------------------------------
    } elsif ($id eq "E18") {
        if ($var eq "h" || $var eq "e") {
           if (($datetime >= 20031028.2225 && $datetime <= 20031028.2315) ||
               ($datetime >= 20040928.2230 && $datetime <= 20040928.2300)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "h" || $var eq "e") {
           if (($datetime >= 20040126.1915 && $datetime <= 20030413.2045) ||
               ($datetime >= 20040419.0500 && $datetime <= 20040613.1700) ||
               ($datetime >= 20040706.1830 && $datetime <= 20040706.2100)) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "g1") {
           if ($datetime >= 20040628.1300 && $datetime <= 20040728.1500) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E19
    #---------------------------------------------------------------------------
    } elsif ($id eq "E19") {
        if ( $var eq "c_shf1" || $var eq "g1" || $var eq "e" || $var eq "h") {
           if (($datetime >= 20031103.2000 && $datetime <= 20031113.1615) ||
               ($datetime >= 20031010.1800 && $datetime <= 20031010.1800) ||
               ($datetime >= 20040529.2215 && $datetime <= 20040610.1510)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ( $var eq "c_shf1" || $var eq "g1" || $var eq "e" || $var eq "h") {
           if ($datetime >= 20040108.1600 && $datetime <= 20040122.1530) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "h" || $var eq "e") {
           if (($datetime >= 20031114.2115 && $datetime <= 20031126.1600) ||
               ($datetime >= 20031010.1530 && $datetime <= 20031010.1600) ||
               ($datetime >= 20031016.1500 && $datetime <= 20031016.1530) ||
               ($datetime >= 20041028.1430 && $datetime <= 20041028.1500)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "h" || $var eq "e") {
           if ($datetime >= 20040205.1700 && $datetime <= 20040415.1530) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E20
    #---------------------------------------------------------------------------
    } elsif ($id eq "E20") {
        if ( $var eq "c_shf1" || $var eq "g1" || $var eq "e" || $var eq "h") {
           if ($datetime >= 20040915.1930 && $datetime <= 20040915.2030) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "h" || $var eq "e") {
           if (($datetime >= 20031015.0210 && $datetime <= 20031029.0230) ||
               ($datetime >= 20030930.0000 && $datetime <= 20031001.2100) ||
               ($datetime >= 20031029.2200 && $datetime <= 20031029.2245) ||
               ($datetime >= 20040929.1730 && $datetime <= 20040929.1800)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "h" || $var eq "e" || $var eq "g1") {
           if ($datetime >= 20030927.1800 && $datetime <= 20031210.2230) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "h" || $var eq "e") {
           if (($datetime >= 20040301.0000 && $datetime <= 20040331.2355) ||
               ($datetime >= 20040803.0300 && $datetime <= 20040915.0200) ||
               ($datetime >= 20040924.0700 && $datetime <= 20040929.1800) ||
               ($datetime >= 20040908.2130 && $datetime <= 20040915.1930) ||
               ($datetime >= 20041126.2100 && $datetime <= 20041208.2100)) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E22
    #---------------------------------------------------------------------------
    } elsif ($id eq "E22") {
        if ($var eq "h" || $var eq "e") {
           if (($datetime >= 20030929.1800 && $datetime <= 20031001.1630) ||
               ($datetime >= 20031015.1900 && $datetime <= 20031015.1930) ||
               ($datetime >= 20040622.0400 && $datetime <= 20040623.1650) ||
               ($datetime >= 20040701.1330 && $datetime <= 20040707.1900) ||
               ($datetime >= 20041027.1700 && $datetime <= 20041027.1730)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E26
    #---------------------------------------------------------------------------
    } elsif ($id eq "E26") {
        if ( $var eq "c_shf1" || $var eq "g1" || $var eq "e" || $var eq "h") {
           if (($datetime >= 20041027.1930 && $datetime <= 20041105.1000) ||
               ($datetime >= 20041110.0600 && $datetime <= 20041124.1500) ||
               ($datetime >= 20041029.1100 && $datetime <= 20041029.1600)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "h" || $var eq "e") {
           if (($datetime >= 20031030.1545 && $datetime <= 20031113.1545) ||
               ($datetime >= 20031030.1535 && $datetime <= 20031030.1615) ||
               ($datetime >= 20040930.1530 && $datetime <= 20040930.1600)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "h" || $var eq "e") {
           if ($datetime >= 20040321.1530 && $datetime <= 20031012.2355) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E27
    #---------------------------------------------------------------------------
    } elsif ($id eq "E27") {
        if ( $var eq "c_shf1" || $var eq "g1" || $var eq "e" || $var eq "h") {
           if ($datetime >= 20040721.1700 && $datetime <= 20040721.1900) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "h" || $var eq "e") {
           if (($datetime >= 20031015.1030 && $datetime <= 20031117.0630) ||
               ($datetime >= 20030906.1200 && $datetime <= 20031011.0730) ||
               ($datetime >= 20031029.1830 && $datetime <= 20031029.1945) ||
               ($datetime >= 20030912.1700 && $datetime <= 20031012.2359) ||
               ($datetime >= 20031206.0900 && $datetime <= 20031206.0900) ||
               ($datetime >= 20040301.0000 && $datetime <= 20040331.2355) ||
               ($datetime >= 20040610.0600 && $datetime <= 20040721.1725)) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "h" || $var eq "e") {
           if ($datetime >= 20040929.2000 && $datetime <= 20040929.2030) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    }

    return $new_flag;
}
