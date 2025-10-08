#!/bin/tcsh

#-------------------------
# for NSA mettwr2h
#-------------------------

foreach i (nsa*2009*.cdf) 
	../../../nesob_dump -v base_time,time_offset,lat,lon,alt,atmos_pressure,qc_atmos_pressure,wspd_vec_mean,qc_wspd_vec_mean,wdir_vec_mean,qc_wdir_vec_mean,temp_mean,qc_temp_mean,rh_mean,qc_rh_mean,dew_point_mean,qc_dew_point_mean,pws_precip_rate_mean_1min,qc_pws_precip_rate_mean_1min $i > $i:r.dat
end
