#!/bin/csh -f

# doall.sc - run all the stuff needed to extract precip
#            data from 5-min QCF composites
#
# The separate scripts include commands to compress files.
# To eliminate a compress at the end of one script followed by an
# immediate uncompress in the next, I have included the commands
# from each script in this script, but without unnecessary compress'es.
# This script also rm's any temporary files (*.pre, *.sort).
#
# run by typing '(nohup /usr/bin/nice -10 doall.sc) >& doall.log &'
#
# uncomment various exits, if you want to check the output before continuing
#
# fixed up for a repeat 1995 run, 4 sep 96, ds

unalias rm
unalias cd

echo "begin doall.sc"
echo "Have you set your COSORT_TUNER specs?"

set QCFDIR =  "/rraid1/ESOP96_PROCESSING/ARMSFC/final/5min_0qc"
set OUTDIR =  "/rraid1/ESOP96_PROCESSING/ARMSFC/final/15min_0pqc"
set PROGDIR = "/rraid1/ESOP96_PROCESSING/ARMSFC/final/15min_0pqc"


echo "extract precip from composites"

cd $QCFDIR

foreach i (*.0qc)
echo $i

# nomdat, nomtim, obsdat, obstim, lat, lon, occ, precip, precip QC flag, network, station
awk '{ print $1, $2 ":00", $3, $4 ":00", substr($0,58,10), substr($0,69,11), substr($0,81,3), substr($0,163,7), substr($0,171,1), substr($0,31,10), substr($0,42,15) }' $i >> $OUTDIR/5min_all
end

cd $OUTDIR

# Need to get to file named "5min_all_pre" either by copying 5min_all, 
# or by running strip_ok.pl on 5min_all.  Uncomment next 4 lines, as needed.

cp $OUTDIR/5min_all $OUTDIR/5min_all_pre

# strip the OKMESO stations from the file
# echo "strip the OKMESO stations from the file"
# $PROGDIR/strip_ok.pl 5min_all


# ******************* 
# exit
# *******************

rm $OUTDIR/5min_all

#
# sortqpq5.sc functionality
#

echo "break down precip files"

# create the smaller .pre files
awk -f $PROGDIR/pre.awk 5min_all.pre

echo "gzip 5min_all.pre"
gzip $OUTDIR/5min_all.pre

echo "sort precip files"

# sort the small .pre files
foreach i (*.pre)
 cosort $PROGDIR/qpqnew.spec < $i > $i:r.sort
 echo "delete $i"
 rm $OUTDIR/$i
end

#*****************
#exit
#*****************

echo "run qpq5"

# create the pqcf files
foreach i (*.sort)
 $PROGDIR/qpq5 < $i > $i:r.pq
 rm $OUTDIR/$i
end

#
# dayfs.sc functionality
#

echo "run dayfs.sc"

# put small pqcf files into day files
foreach i (*.pq)
 $PROGDIR/dayfs < $i
 rm $OUTDIR/$i
end

echo "end doall.sc"
