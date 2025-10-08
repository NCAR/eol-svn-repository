#!/bin/tcsh

#-------------------------
# for IRT
#-------------------------

foreach i (sgp*.cdf) 
	echo "processing $i"
	../../nesob_dump -v base_time,time_offset,lat,lon,alt,sfc_ir_temp $i > ../$i:r.dat
end

