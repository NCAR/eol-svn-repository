#! /bin/csh -f
#
# This is an example script.
# Set PROJECT and uncomment each command to archive that data type.

###############
#   Project   #
###############
echo "Make sure netCDF files have been reordered before archiving!"

set NUM = '793'
set YEAR = '1989'

### HRT
#/net/work/bin/scripts/mass_store/archAC/archive.py HRT/NetCDF $cwd/HRT nc /RAF/$YEAR/$NUM

### LRT
/net/work/bin/scripts/mass_store/archAC/archive.py LRT/NetCDF $cwd/LRT nc /RAF/$YEAR/$NUM

### LRT20
#/net/work/bin/scripts/mass_store/archAC/archive.py LRT20/NetCDF $cwd/LRT20 nc /RAF/$YEAR/$NUM

### LRT20S
#/net/work/bin/scripts/mass_store/archAC/archive.py LRT20S/NetCDF $cwd/LRT20S nc /RAF/$YEAR/$NUM
