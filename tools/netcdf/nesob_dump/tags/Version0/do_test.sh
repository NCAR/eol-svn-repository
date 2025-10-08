#!/bin/tcsh

# foreach i (./raw/Dsgpmfr10mC1.a1.9604*.cdf)
foreach i (./raw/Dsgpmfr10mC1.a1.96082**.cdf ./raw/Dsgpmfr10mC1.a1.96083*.cdf ./raw/Dsgpmfr10mC1.a1.96090*.cdf)
# foreach i (./raw/Dsgpmfr10mC1.a1.960820*.cdf)
        nesob_dump -v base_time,time_offset,lat,lon,alt,sfc_temp -l 50000 $i | process_skintemp.pl
end
