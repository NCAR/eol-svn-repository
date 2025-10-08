#!/bin/csh


foreach file (./*txt)
  set ofile=`echo $file | sed 's/txt/nc/'`
  set date=`echo $file | sed 's/.\/G/2008-08-/' | sed 's/.\/L/2008-07-/' | sed 's/DRICCN.txt//'`
  echo "Processing" $ofile
  asc2cdf -d $date $file $ofile
  ncatted -a units,CCNT,m,c,"#/cm3" $ofile
  ncatted -a units,1.5S,m,c,"#/cm3" $ofile
  ncatted -a units,1.0S,m,c,"#/cm3" $ofile
  ncatted -a units,0.6S,m,c,"#/cm3" $ofile
  ncatted -a units,0.4S,m,c,"#/cm3" $ofile
  ncatted -a units,0.3S,m,c,"#/cm3" $ofile
  ncatted -a units,0.2S,m,c,"#/cm3" $ofile
  ncatted -a units,0.1S,m,c,"#/cm3" $ofile
  ncatted -a units,0.08S,m,c,"#/cm3" $ofile
  ncatted -a units,0.06S,m,c,"#/cm3" $ofile
  ncatted -a units,0.04S,m,c,"#/cm3" $ofile
  ncatted -a long_name,CCNT,m,c,"total CCN concentrations at some supersaturation above 1.5% normalized to sea level" $ofile
  ncatted -a long_name,1.5S,m,c,"cumulative CCN concentration at 1.5% supersaturation normalized to sea level" $ofile
  ncatted -a long_name,1.0S,m,c,"cumulative CCN concentration at 1% supersaturation normalized to sea level" $ofile
  ncatted -a long_name,0.6S,m,c,"cumulative CCN concentration at 0.6% supersaturation normalized to sea level" $ofile
  ncatted -a long_name,0.4S,m,c,"cumulative CCN concentration at 0.4% supersaturation normalized to sea level" $ofile
  ncatted -a long_name,0.3S,m,c,"cumulative CCN concentration at 0.3% supersaturation normalized to sea level" $ofile
  ncatted -a long_name,0.2S,m,c,"cumulative CCN concentration at 0.2% supersaturation normalized to sea level" $ofile
  ncatted -a long_name,0.1S,m,c,"cumulative CCN concentration at 0.1% supersaturation normalized to sea level" $ofile
  ncatted -a long_name,0.08S,m,c,"cumulative CCN concentration at 0.08% supersaturation normalized to sea level" $ofile
  ncatted -a long_name,0.06S,m,c,"cumulative CCN concentration at 0.06% supersaturation normalized to sea level" $ofile
  ncatted -a long_name,0.04S,m,c,"cumulative CCN concentration at 0.04% supersaturation normalized to sea level" $ofile
  ncatted -a history,global,d,c, $ofile
end
