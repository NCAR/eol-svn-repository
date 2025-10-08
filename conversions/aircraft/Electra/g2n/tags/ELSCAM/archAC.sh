#! /bin/csh -f
#
# This is an example script.
# Set PROJECT and uncomment each command to archive that data type.

###############
#   Project   #
###############
echo "Make sure netCDF files have been reordered before archiving!"

set NUM = '851'
set YEAR = '1987'

### HRT
/net/work/bin/scripts/mass_store/archAC/archive.py HRT/NetCDF $cwd/HRT nc /RAF/$YEAR/$NUM

### LRT
#/net/work/bin/scripts/mass_store/archAC/archive.py LRT/NetCDF $cwd/LRT nc /RAF/$YEAR/$NUM

