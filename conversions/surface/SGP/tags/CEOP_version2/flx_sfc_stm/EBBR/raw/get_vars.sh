#!/bin/tcsh

#--------------------------------------------------
# for EBBR
#
# split up by year since OS can't handle long lists
#--------------------------------------------------

foreach i (sgp*2005*cdf) 
	../../nesob_dump -v base_time,time_offset,lat,lon,alt,c_shf1,g1,e,qc_e,h,qc_h $i > ../$i:r.dat
end

foreach i (sgp*2006*cdf) 
	../../nesob_dump -v base_time,time_offset,lat,lon,alt,c_shf1,g1,e,qc_e,h,qc_h $i > ../$i:r.dat
end

foreach i (sgp*2007*cdf) 
	../../nesob_dump -v base_time,time_offset,lat,lon,alt,c_shf1,g1,e,qc_e,h,qc_h $i > ../$i:r.dat
end

foreach i (sgp*2008*cdf) 
	../../nesob_dump -v base_time,time_offset,lat,lon,alt,c_shf1,g1,e,qc_e,h,qc_h $i > ../$i:r.dat
end

foreach i (sgp*2009*cdf) 
	../../nesob_dump -v base_time,time_offset,lat,lon,alt,corr_soil_heat_flow_1,surface_soil_heat_flux_avg,latent_heat_flux,qc_latent_heat_flux,sensible_heat_flux,qc_sensible_heat_flux $i > ../$i:r.dat
end
