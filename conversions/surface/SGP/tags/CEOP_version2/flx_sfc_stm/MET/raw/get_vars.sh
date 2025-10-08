#!/bin/tcsh

#-------------------------
# for MET
#-------------------------

foreach i (sgpmet*.cdf) 
	echo "processing $i"
	../../nesob_dump -v base_time,time_offset,lat,lon,alt,atmos_pressure,qc_atmos_pressure,temp_mean,qc_temp_mean,rh_mean,qc_rh_mean,wspd_vec_mean,qc_wspd_vec_mean,wdir_vec_mean,qc_wdir_vec_mean,tbrg_precip_total_corr,qc_tbrg_precip_total_corr $i > ../$i:r.dat
end

