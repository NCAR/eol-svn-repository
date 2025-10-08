#!/opt/bin/perl -w 

open (OUT_SFC_HDR, ">../out/sfc.header") || die "Can't open sfc.header";
open (OUT_TWR_HDR, ">../out/twr.header") || die "Can't open twr.header";

writeHeader("sfc");
writeHeader("twr");


#------------------------------------------------------------
# print out the header lines 
#------------------------------------------------------------       

sub writeHeader {
    my($out_type) = @_;
          
    if ($out_type eq "sfc") {
		print OUT_SFC_HDR "   date    time     date    time    CSE ID      site ID        station ID        lat        lon      elev  ";
		foreach $param ("stn pres", " f", "temp_air", " f", " dew pt ", " f", " rel hum", " f", "spec hum", " f", " wnd spd", " f", " wnd dir", " f", " U wind ", " f", " V wind ", " f", " precip ", " f", "  snow  ", " f", " short in", " f", " shortout", " f", " long in ", " f", " long out", " f", "  net rad", " f", " skintemp", " f", " par_in  ", " f", " par_out ", " f") {
			print OUT_SFC_HDR "$param"; 
		}
		print OUT_SFC_HDR "\n";
		print OUT_SFC_HDR "---------- ----- ---------- ----- ---------- --------------- --------------- ---------- ----------- -------"; 
		for ($i=0; $i<11; $i++) {
			print OUT_SFC_HDR " ------- -"; 
		}
		for ($i=0; $i<8; $i++) {
			print OUT_SFC_HDR " -------- -"; 
		}
		print OUT_SFC_HDR "\n";
    } elsif ($out_type eq "twr") {
		print OUT_TWR_HDR "   date    time     date    time    CSE ID      site ID        station ID        lat        lon      elev  snsor ht";
		foreach $param ("stn pres", " f", "temp_air", " f", " dew pt ", " f", " rel hum", " f", "spec hum", " f", " wnd spd", " f", " wnd dir", " f", " U wind ", " f", " V wind ", " f") {
			print OUT_TWR_HDR "$param"; 
		}
		print OUT_TWR_HDR "\n";
		print OUT_TWR_HDR "---------- ----- ---------- ----- ---------- --------------- --------------- ---------- ----------- ------- -------"; 
		for ($i=0; $i<9; $i++) {
			print OUT_TWR_HDR " ------- -"; 
		}
		print OUT_TWR_HDR "\n";
    } else {
        die "don't know this output type: $out_type!";
    }
}


