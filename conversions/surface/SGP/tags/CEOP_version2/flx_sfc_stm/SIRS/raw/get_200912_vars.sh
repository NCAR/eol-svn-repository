#!/bin/tcsh

#-------------------------
# for SIRS
#-------------------------

foreach i (sgp*E7*200912*.cdf sgp*E19*200912*.cdf sgp*E21*200912*.cdf) 
	echo "processing $i"
	../../nesob_dump -v base_time,time_offset,lat,lon,alt,up_long_hemisp,down_long_hemisp_shaded,up_short_hemisp,down_short_hemisp $i >! ../$i:r.dat
end

