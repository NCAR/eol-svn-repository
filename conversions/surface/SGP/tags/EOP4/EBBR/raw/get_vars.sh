#!/bin/tcsh

#-------------------------
# for EBBR
#-------------------------

foreach i (sgp*.cdf) 
	../../nesob_dump -v base_time,time_offset,lat,lon,alt,c_shf1,g1,e,qc_e,h,qc_h $i > ../$i:r.dat
end

