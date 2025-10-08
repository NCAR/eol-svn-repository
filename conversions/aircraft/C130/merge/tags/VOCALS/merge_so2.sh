#!/bin/sh

exec > ./merge_so2.log 2>&1

for srcfile in `ls -1 SO2/RF*VOCALS*.nc`
do
    destfile=`echo $srcfile | sed 's/SO2\/RF\(..\)_VOCALS_SO2_v2.nc/VOCALSrf\1.nc/'`
    ncmerge -v SO2 $destfile $srcfile
done
