#!/bin/tcsh

#-------------------------
# for SWATS
#-------------------------

foreach i (sgp*.cdf) 
	echo "processing $i"
	../../nesob_dump -v base_time,time_offset,lat,lon,alt,depth,tsoil_W,qc_tsoil_W,watcont_W,qc_watcont_W -l 50000 $i > ../$i:r.dat
end

