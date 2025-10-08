#!/bin/tcsh

# foreach i (./raw/*10m*.cdf)
#		echo " "
#		echo "processing $i"
#       nesob_dump -v base_time,time_offset,lat,lon,alt,sfc_temp -l 50000 $i | process_skintemp-10m.pl
# end

foreach i (./raw/*25m*.cdf)
		echo " "
		echo "processing $i"
        nesob_dump -v base_time,time_offset,lat,lon,alt,sfc_temp -l 50000 $i | process_skintemp-25m.pl
end
