#!/bin/tcsh

#-------------------------
# for SMOS
#-------------------------

foreach i (sgp*.cdf) 
	echo "processing $i"
	../../nesob_dump -v base_time,time_offset,lat,lon,alt,wspd,qc_wspd,wdir,qc_wdir,temp,qc_temp,rh,qc_rh,bar_pres,qc_bar_pres,precip,qc_precip $i > ../$i:r.dat
end
