#! /bin/csh -f
#
# This is an example script.
# Set PROJECT and uncomment each command to archive that data type.

###############
#   Project   #
###############
echo "Make sure netCDF files have been reordered before archiving!"

set NUM = '250'
set YEAR = '1988'

### HRT
#/net/work/bin/scripts/mass_store/archAC/archive.py HRT/NetCDF $cwd/HRT nc /RAF/$YEAR/$NUM stroble@ucar.edu

set NUM = 'ELDOM'

### ELDOM
/net/work/bin/scripts/mass_store/archAC/archive.py HRT/NetCDF $cwd/ELDOM nc /RAF/$YEAR/$NUM stroble@ucar.edu

