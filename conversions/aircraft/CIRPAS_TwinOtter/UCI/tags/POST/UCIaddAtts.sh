#!/bin/sh
#
# ncatted gives error on tsunami:
#   *** glibc detected *** double free or corruption (fasttop): 0x086cb0c8 ***
#   Abort
# but seems to run anyway. I don't get an error on bora, so run there.

for file in `ls -1 *nc`
do
    ncatted -a units,ap,m,c,"m" $file
    ncatted -a long_name,ap,m,c,"Pressured altitude (adjusted to radar altitude)" $file
    ncatted -a units,lat,m,c,"degN" $file
    ncatted -a long_name,lat,m,c,"LATitude from UCIs C-MIGITS III" $file
    ncatted -a units,lon,m,c,"degE" $file
    ncatted -a long_name,lon,m,c,"LONgitude from UCIs C-MIGITS III" $file
    ncatted -a units,hdg,m,c,"deg" $file
    ncatted -a long_name,hdg,m,c,"true HeaDinG from UCIs C-MIGITS III range [0 360] deg " $file
    ncatted -a units,wx,m,c,"m/s" $file
    ncatted -a long_name,wx,m,c,"Wind component in the east direction (X-axis)" $file
    ncatted -a units,wy,m,c,"m/s" $file
    ncatted -a long_name,wy,m,c,"Wind component in the north direction (Y-axis)" $file
    ncatted -a units,wz,m,c,"m/s" $file
    ncatted -a long_name,wz,m,c,"Wind component in the vertical direction (Z-axis)" $file
    ncatted -a units,ah,m,c,"g/m3" $file
    ncatted -a long_name,ah,m,c,"Absolute Humidity [TO01-TO04: Chilled mirror; TO05-TO17 LI-COR 7500] (see note 1)" $file
    ncatted -a units,ta,m,c,"deg C" $file
    ncatted -a long_name,ta,m,c,"static Ambient Temperature from UCIs Rosemount fast-response sensor" $file
    ncatted -a units,td,m,c,"deg C" $file
    ncatted -a long_name,td,m,c,"ambient Dewpoint Temperature from CIRPASs Edgtech Chilled mirror sensor" $file
    ncatted -a units,ts,m,c,"deg C" $file
    ncatted -a long_name,ts,m,c,"Sea surface Temperature from CIRPASs downlooking Heiman KT 19.85 IR sensor " $file
    ncatted -a units,ps,m,c,"hPa" $file
    ncatted -a long_name,ps,m,c,"Static atmospheric Pressure from fuselage flush ports and Setra 270 transducer " $file
    ncatted -a units,tas,m,c,"m/s" $file
    ncatted -a long_name,tas,m,c,"True Air Speed (Dry Air)" $file
    ncatted -a units,rhoa,m,c,"kg/m^3" $file
    ncatted -a long_name,rhoa,m,c,"Moist Air density" $file
    ncatted -a units,mr,m,c,"g/kg" $file
    ncatted -a long_name,mr,m,c,"Mixing Ratio from UCIs LI-COR 7500 [TO01-TO04: Chilled mirror; TO05-TO17 LI-COR 7500] (see note 1)" $file
    ncatted -a units,thet,m,c,"K" $file
    ncatted -a long_name,thet,m,c,"potential temperature (theta)" $file
    ncatted -a units,tvir,m,c,"deg C" $file
    ncatted -a long_name,tvir,m,c,"VIRtual Temperature" $file
    ncatted -a units,thete,m,c,"K" $file
    ncatted -a long_name,thete,m,c,"Equivalent potential temperature (thetae)" $file
    ncatted -a units,tirup,m,c,"deg C" $file
    ncatted -a long_name,tirup,m,c,"Temperature from UCIs IR UPward-looking temperature sensor" $file
    ncatted -a units,flip,m,c,"V" $file
    ncatted -a long_name,flip,m,c,"FLIP-flop 1/2 Hz GPS synchronisation signal from 1-Hz CIRPAS C-MIGITS III pulse " $file
    ncatted -a units,tdl,m,c,"deg C" $file
    ncatted -a long_name,tdl,m,c,"Dewpoint Temperature from UCIs LI-COR 7500 [TO01-TO04: fillValue] (see note 1)" $file

    ncatted -a ConventionsURL,global,c,c,"http://www.eol.ucar.edu/raf/Software/netCDF.html" $file
    ncatted -a Version,global,c,c,"28April2009" $file
    ncatted -a comment,global,c,c,"Note (1): UCIs LI-COR 7500 H2O, CO2 gas analyser (variables ah, mr, tdl) was installed on July 28, 2009 (Flight TO05) and was operational through the last flight of August 15, 2009 (TO17). Prior to TO05 (TO01-TO04 i.e., flights 080716, 080717, 080719 and 080721), variables ah and mr are calculated using data from the chilled mirror dewpointer and variable tdl values are filled with the netCDF fillValue." $file
    ncatted -a history,global,d,, $file
done
