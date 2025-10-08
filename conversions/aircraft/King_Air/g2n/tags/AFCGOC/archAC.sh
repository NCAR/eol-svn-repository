#! /bin/csh -f
#
# This is an example script.
# Set PROJECT and uncomment each command to archive that data type.

###############
#   Project   #
###############
echo "Make sure netCDF files have been reordered before archiving!"

set PROJ = 'afcgoc'
set YEAR = '1983'
set PLAT = 'kingair_n312d'

### HRT
/net/work/bin/scripts/mass_store/archAC/archive.py HRT/NetCDF $cwd/TL0483 nc /EOL/$YEAR/$PROJ/aircraft/$PLAT stroble@ucar.edu

### LRT
#/net/work/bin/scripts/mass_store/archAC/archive.py LRT/NetCDF $cwd/LRT nc /RAF/$YEAR/$NUM stroble@ucar.edu

