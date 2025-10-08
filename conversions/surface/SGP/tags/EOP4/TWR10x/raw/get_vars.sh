#!/bin/tcsh

#-------------------------
# for TWR10x
#-------------------------

foreach i (sgp*.cdf) 
	../../nesob_dump -v base_time,time_offset,lat,lon,alt,temp_60m,qc_temp_60m,temp_25m,qc_temp_25m,rh_60m,qc_rh_60m,rh_25m,qc_rh_25m $i > ../$i:r.dat
end

