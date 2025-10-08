#! /bin/csh -f
#
# This is an example script.
# Set PROJECT and uncomment each command to archive that data type.

###############
#   Project   #
###############
echo "Make sure netCDF files have been reordered before archiving!"

set PROJ = 'brs'
set YEAR = '1985'
set PLAT = 'kingair_n312d'

### LRT
/net/work/bin/scripts/mass_store/archAC/archive.py LRT/NetCDF $cwd/TL0910 nc /EOL/$YEAR/$PROJ/aircraft/$PLAT stroble@ucar.edu

### LRT
#/net/work/bin/scripts/mass_store/archAC/archive.py LRT/NetCDF $cwd/LRT nc /RAF/$YEAR/$NUM stroble@ucar.edu

