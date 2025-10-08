#!/bin/tcsh

#-------------------------
# for NSA gndrad
#-------------------------

foreach i (nsa*.cdf) 
	../../nesob_dump -v base_time,time_offset,lat,lon,alt,up_short_hemisp,qc_up_short_hemisp,up_long_hemisp,qc_up_long_hemisp,sfc_ir_temp,qc_sfc_ir_temp $i > $i:r.dat
end
