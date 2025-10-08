#! /bin/sh

# note that your kinit must be active to run this script

cd /h/eol/joss/hpss_retrieval/hpss_lists_all_eol
thisday=`date +'%y%m%d'`

ofile="EOL_by_year.${thisday}.list"
date
echo $ofile
/opt/local/bin/hsi ls -lR '/EOL/[1-2]*' >& $ofile

ofile="EOL_by_platform.${thisday}.list"
date
echo $ofile
/opt/local/bin/hsi ls -lR /EOL/by_platform >& $ofile

ofile="EOL_operational.${thisday}.list"
date
echo $ofile
/opt/local/bin/hsi ls -lR /EOL/operational >& $ofile

# Moved all /JOSS files to /EOL/JOSS Feb 2014
ofile="EOL_joss_all.${thisday}.list"
date
echo $ofile
/opt/local/bin/hsi ls -lR /EOL/JOSS >& $ofile

# Moved all /RAF files to /EOL/RAF Dec 2013
ofile="EOL_raf_all.${thisday}.list"
date
echo $ofile
/opt/local/bin/hsi ls -lR /EOL/RAF >& $ofile

exit;
