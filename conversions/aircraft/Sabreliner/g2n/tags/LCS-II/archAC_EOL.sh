#! /bin/csh -f
#
# This is an example script.
# Set PROJECT and uncomment each command to archive that data type.

###############
#   Project   #
###############
echo "Make sure netCDF files have been reordered before archiving!"

set PROJ = 'lcs-II'
set YEAR = '1985'
set PLAT = 'sabreliner_n307d'

### HRT
#/net/work/bin/scripts/mass_store/archAC/archive.py HRT/NetCDF $cwd/HRT nc /EOL/$YEAR/$PROJ/aircraft/$PLAT stroble@ucar.edu

### LRT
/net/work/bin/scripts/mass_store/archAC/archive.py LRT/NetCDF $cwd/TL0910 nc /EOL/$YEAR/$PROJ/aircraft/$PLAT stroble@ucar.edu

