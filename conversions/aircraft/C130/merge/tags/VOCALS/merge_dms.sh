#!/bin/sh

exec > ./merge_dms.log 2>&1

for srcfile in `ls -1 DMS/RF*VOCALS*.nc`
do
    destfile=`echo $srcfile | sed 's/DMS\/RF\(..\)_VOCALS_DMS_v3.nc/VOCALSrf\1.nc/'`
    ncmerge -v DMS $destfile $srcfile
done
