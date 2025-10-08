#! /bin/csh -f
#
# This is an example script.
# Set PROJECT and uncomment each command to archive that data type.

###############
#   Project   #
###############
echo "Make sure netCDF files have been reordered before archiving!"

set NUM = '244'
set YEAR = '1991'

### HRT
/net/work/bin/scripts/mass_store/archAC/archive.py HRT/NetCDF $cwd/HRT cdf /RAF/$YEAR/$NUM

### LRT
#/net/work/bin/scripts/mass_store/archAC/archive.py LRT/NetCDF $cwd/LRT nc /RAF/$YEAR/$NUM

