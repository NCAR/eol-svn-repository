#!/bin/tcsh

#-------------------------
# for ECOR
#-------------------------

foreach i (sgp*.cdf) 
	../../nesob_dump -v base_time,time_offset,lat,lon,alt,h,qc_h,lv_e,qc_lv_e,fc,qc_fc $i > ../$i:r.dat
end

