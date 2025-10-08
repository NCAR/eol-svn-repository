#!/bin/sh

exec > ./merge_o3.log 2>&1

for srcfile in `ls -1 O3/VOCALS*.nc`
do
    destfile=`echo $srcfile | sed 's/O3\/VOCALSrf\(..\).fo3_cl.nc/VOCALSrf\1.nc/'`
    ncmerge -v FO3_CL $destfile $srcfile
done
