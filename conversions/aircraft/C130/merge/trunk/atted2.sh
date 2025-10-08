#!/bin/sh

for file in `ls -1 VOCALSrf[0-9][0-9].nc`
do
    echo "Modifying FO3 attributes for $file"
    ncatted -a Category,FO3_CL,m,c,"Final" $file
    ncatted -a DataQuality,FO3_CL,m,c,"Good" $file
done
