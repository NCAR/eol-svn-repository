#!/bin/tcsh

#-------------------------
# for NSA mettwr
#-------------------------

foreach i (nsa*.cdf) 
	../../nesob_dump -v base_time,time_offset,lat,lon,alt,atmos_pressure,qc_atmos_pressure,wind_spd_mean,qc_wind_spd_mean,wind_dir_vec_avg,qc_wind_dir_vec_avg,temp_mean,qc_temp_mean,relh_mean,qc_relh_mean,dew_pt_temp_mean,qc_dew_pt_temp_mean $i > $i:r.dat
end

