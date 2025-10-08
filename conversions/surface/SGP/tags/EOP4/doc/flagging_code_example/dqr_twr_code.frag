    #---------------------------------------------------------------------------
    # following section fixes flags according to DQRs
        if ($var eq "temp_25m") {
           if (($datetime >= 20030516.2053 && $datetime <= 20030516.2054) ||
               ($datetime >= 20030805.1837 && $datetime <= 20030805.1914) ||
               ($datetime >= 20030814.1445 && $datetime <= 20030814.1521) ||
               ($datetime >= 20030814.1902 && $datetime <= 20030814.1915) ||
               ($datetime >= 20030814.1924 && $datetime <= 20030814.1937) ||
               ($datetime >= 20030815.1344 && $datetime <= 20030815.1945) ||
               ($datetime >= 20030819.1416 && $datetime <= 20030819.1640)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "rh_25m") {
           if (($datetime >= 20030516.2053 && $datetime <= 20030516.2054) ||
               ($datetime >= 20030805.1837 && $datetime <= 20030805.1914) ||
               ($datetime >= 20030814.1445 && $datetime <= 20030814.1521) ||
               ($datetime >= 20030814.1902 && $datetime <= 20030814.1915) ||
               ($datetime >= 20030814.1924 && $datetime <= 20030814.1937) ||
               ($datetime >= 20030815.1344 && $datetime <= 20030815.1945) ||
               ($datetime >= 20030819.1416 && $datetime <= 20030819.1640)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "rh_25m") {
           if ($datetime >= 20030815.1345 && $datetime <= 20040210.2310) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "temp_60m") {
           if (($datetime >= 20030516.2053 && $datetime <= 20030516.2054) ||
               ($datetime >= 20030805.1837 && $datetime <= 20030805.1914) ||
               ($datetime >= 20030814.1445 && $datetime <= 20030814.1521) ||
               ($datetime >= 20030814.1902 && $datetime <= 20030814.1915) ||
               ($datetime >= 20030814.1924 && $datetime <= 20030814.1937) ||
               ($datetime >= 20030815.1344 && $datetime <= 20030815.1945) ||
               ($datetime >= 20030819.1416 && $datetime <= 20030819.1640)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "rh_60m") {
           if (($datetime >= 20030516.2053 && $datetime <= 20030516.2054) ||
               ($datetime >= 20030805.1837 && $datetime <= 20030805.1914) ||
               ($datetime >= 20030814.1445 && $datetime <= 20030814.1521) ||
               ($datetime >= 20030814.1902 && $datetime <= 20030814.1915) ||
               ($datetime >= 20030814.1924 && $datetime <= 20030814.1937) ||
               ($datetime >= 20030815.1344 && $datetime <= 20030815.1945) ||
               ($datetime >= 20030819.1416 && $datetime <= 20030819.1640)) {
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
    }

    return $new_flag;
}
