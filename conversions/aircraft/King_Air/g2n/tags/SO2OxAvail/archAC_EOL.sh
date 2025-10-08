#! /bin/csh -f
#
# This is an example script.
# Set PROJECT and uncomment each command to archive that data type.

###############
#   Project   #
###############
echo "Make sure netCDF files have been reordered before archiving!"

set FOLD = 'TL0910'

set PROJ = 'so2ox'
set YEAR = '1984'
set PLAT = 'kingair_n312d'

### HRT
#/net/work/bin/scripts/mass_store/archAC/archive.py HRT/NetCDF $cwd/$FOLD nc /EOL/$YEAR/$PROJ/aircraft/$PLAT stroble@ucar.edu

### LRT
/net/work/bin/scripts/mass_store/archAC/archive.py LRT/NetCDF $cwd/$FOLD nc /EOL/$YEAR/$PROJ/aircraft/$PLAT stroble@ucar.edu


echo 
