#!/bin/tcsh

#-------------------------
# for SIRS
#-------------------------

foreach i (sgp*.cdf) 
	echo "processing $i"
	../../nesob_dump -v base_time,time_offset,lat,lon,alt,up_long_hemisp,down_long_hemisp_shaded,up_short_hemisp,down_short_hemisp $i > ../$i:r.dat
end

