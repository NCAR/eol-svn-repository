#!/bin/tcsh

#-------------------------
# for MET
#-------------------------

foreach i (met*.cdf) 
	../../nesob_dump -v base_time,time_offset,lat,lon,alt,atmos_pressure,temp_mean,rh_mean,wspd_vec_mean,wdir_vec_mean,tbrg_precip_total_corr $i > ../$i:r.dat
end

