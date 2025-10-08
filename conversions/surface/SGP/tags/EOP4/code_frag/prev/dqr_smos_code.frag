    #---------------------------------------------------------------------------
    # following section fixes flags according to DQRs
    #---------------------------------------------------------------------------
    # E1
    #---------------------------------------------------------------------------
    } elsif ($id eq "E1") {
        if ( $var eq "wspd" || $var eq "wdir" || $var eq "temp" || $var eq "rh" || $var eq "bar_pres" ) {
           if (($datetime >= 20031021.1745 && $datetime <= 20031021.1945) ||
               ($datetime >= 20040420.1955 && $datetime <= 20040420.2020) ||
               ($datetime >= 20041019.1920 && $datetime <= 20041019.1950)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E3
    #---------------------------------------------------------------------------
    } elsif ($id eq "E3") {
        if ( $var eq "wspd" || $var eq "wdir" || $var eq "temp" || $var eq "rh" || $var eq "bar_pres" ) {
           if (($datetime >= 20031105.1733 && $datetime <= 20031105.1815) ||
               ($datetime >= 20040407.1645 && $datetime <= 20040407.1730) ||
               ($datetime >= 20041006.1534 && $datetime <= 20041006.1620)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E4
    #---------------------------------------------------------------------------
    } elsif ($id eq "E4") {
        if ( $var eq "wspd" || $var eq "wdir" || $var eq "temp" || $var eq "rh" || $var eq "bar_pres" ) {
           if (($datetime >= 20031022.1455 && $datetime <= 20031022.1655) ||
               ($datetime >= 20040421.1529 && $datetime <= 20040421.1550) ||
               ($datetime >= 20041020.1515 && $datetime <= 20041020.1535)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "rh") {
           if (($datetime >= 20030922.0447 && $datetime <= 20031027.1655) ||
               ($datetime >= 20031002.1136 && $datetime <= 20031022.1648)) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "rh") {
           if (($datetime >= 20040117.1545 && $datetime <= 20040119.1730) ||
               ($datetime >= 20040422.1631 && $datetime <= 20040506.1531)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E5
    #---------------------------------------------------------------------------
    } elsif ($id eq "E5") {
        if ( $var eq "wspd" || $var eq "wdir" || $var eq "temp" || $var eq "rh" || $var eq "bar_pres" ) {
           if (($datetime >= 20031021.1915 && $datetime <= 20031021.1942) ||
               ($datetime >= 20040421.1830 && $datetime <= 20040421.1900) ||
               ($datetime >= 20041020.1906 && $datetime <= 20041020.1918)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E6
    #---------------------------------------------------------------------------
    } elsif ($id eq "E6") {
        if ( $var eq "wspd" || $var eq "wdir" || $var eq "temp" || $var eq "rh" || $var eq "bar_pres" ) {
           if (($datetime >= 20031106.1525 && $datetime <= 20031106.1600) ||
               ($datetime >= 20040408.1511 && $datetime <= 20040408.1600) ||
               ($datetime >= 20041007.1407 && $datetime <= 20041007.1500)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E7
    #---------------------------------------------------------------------------
    } elsif ($id eq "E7") {
        if ( $var eq "wspd" || $var eq "wdir" || $var eq "temp" || $var eq "rh" || $var eq "bar_pres" ) {
           if (($datetime >= 20031104.2144 && $datetime <= 20031104.2230) ||
               ($datetime >= 20040406.1958 && $datetime <= 20040406.2040) ||
               ($datetime >= 20041005.1939 && $datetime <= 20041005.2020)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E8
    #---------------------------------------------------------------------------
    } elsif ($id eq "E8") {
        if ( $var eq "wspd" || $var eq "wdir" || $var eq "temp" || $var eq "rh" || $var eq "bar_pres" ) {
           if (($datetime >= 20031021.1700 && $datetime <= 20031021.1735) ||
               ($datetime >= 20040420.1740 && $datetime <= 20040420.1820) ||
               ($datetime >= 20041019.1730 && $datetime <= 20041019.1800)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E9
    #---------------------------------------------------------------------------
    } elsif ($id eq "E9") {
        if ( $var eq "wspd" || $var eq "wdir" || $var eq "temp" || $var eq "rh" || $var eq "bar_pres" ) {
           if (($datetime >= 20031104.1654 && $datetime <= 20031104.1735) ||
               ($datetime >= 20040406.1553 && $datetime <= 20040406.1640) ||
               ($datetime >= 20041005.1616 && $datetime <= 20041005.1650)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E11
    #---------------------------------------------------------------------------
    } elsif ($id eq "E11") {
        if ( $var eq "wspd" || $var eq "wdir" || $var eq "temp" || $var eq "rh" || $var eq "bar_pres" ) {
           if (($datetime >= 20031014.1930 && $datetime <= 20031014.1941) ||
               ($datetime >= 20040413.2011 && $datetime <= 20040413.2035) ||
               ($datetime >= 20041026.1840 && $datetime <= 20041026.1900)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "rh") {
           if ($datetime >= 20040414.0055 && $datetime <= 20040713.1525) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E13
    #---------------------------------------------------------------------------
    } elsif ($id eq "E13") {
        if ( $var eq "wspd" || $var eq "wdir" || $var eq "temp" || $var eq "rh" || $var eq "bar_pres" ) {
           if (($datetime >= 20031016.1827 && $datetime <= 20031016.1855) ||
               ($datetime >= 20040415.1958 && $datetime <= 20040415.2020) ||
               ($datetime >= 20041028.1835 && $datetime <= 20041028.1855)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E15
    #---------------------------------------------------------------------------
    } elsif ($id eq "E15") {
        if ( $var eq "wspd" || $var eq "wdir" || $var eq "temp" || $var eq "rh" || $var eq "bar_pres" ) {
           if (($datetime >= 20031014.1700 && $datetime <= 20031014.1730) ||
               ($datetime >= 20040413.1615 && $datetime <= 20040413.1700) ||
               ($datetime >= 20041026.1640 && $datetime <= 20041026.1750)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "rh") {
           if (($datetime >= 20031007.1425 && $datetime <= 20031007.1428) ||
               ($datetime >= 20031009.0105 && $datetime <= 20031009.0159)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E20
    #---------------------------------------------------------------------------
    } elsif ($id eq "E20") {
        if ( $var eq "wspd" || $var eq "wdir" || $var eq "temp" || $var eq "rh" || $var eq "bar_pres" ) {
           if (($datetime >= 20031029.2149 && $datetime <= 20031029.2230) ||
               ($datetime >= 20040428.2027 && $datetime <= 20040428.2105) ||
               ($datetime >= 20040929.1607 && $datetime <= 20040929.1655)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "wspd") {
           if ($datetime >= 20040930.1900 && $datetime <= 20041013.1737) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E21
    #---------------------------------------------------------------------------
    } elsif ($id eq "E21") {
        if ( $var eq "wspd" || $var eq "wdir" || $var eq "temp" || $var eq "rh" || $var eq "bar_pres" ) {
           if (($datetime >= 20031029.1519 && $datetime <= 20031029.1615) ||
               ($datetime >= 20040428.1430 && $datetime <= 20040428.1530) ||
               ($datetime >= 20041108.2200 && $datetime <= 20041108.2220)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "bar_pres") {
           if ($datetime >= 20040512.1532 && $datetime <= 20040628.1520) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E24
    #---------------------------------------------------------------------------
    } elsif ($id eq "E24") {
        if ( $var eq "wspd" || $var eq "wdir" || $var eq "temp" || $var eq "rh" || $var eq "bar_pres" ) {
           if (($datetime >= 20031030.1641 && $datetime <= 20031030.1730) ||
               ($datetime >= 20040429.1631 && $datetime <= 20040429.1715) ||
               ($datetime >= 20040930.1634 && $datetime <= 20040930.1715)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "precip") {
           if ($datetime >= 20040229.0000 && $datetime <= 20040318.2359) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E27
    #---------------------------------------------------------------------------
    } elsif ($id eq "E27") {
        if ( $var eq "wspd" || $var eq "wdir" || $var eq "temp" || $var eq "rh" || $var eq "bar_pres" ) {
           if (($datetime >= 20031029.1808 && $datetime <= 20031029.1855) ||
               ($datetime >= 20040428.1712 && $datetime <= 20040428.1755) ||
               ($datetime >= 20040929.2012 && $datetime <= 20040929.2045)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    }

    return $new_flag;
}
