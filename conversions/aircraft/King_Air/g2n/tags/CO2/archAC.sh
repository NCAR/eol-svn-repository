#! /bin/csh -f
#
# This is an example script.
# Set PROJECT and uncomment each command to archive that data type.

###############
#   Project   #
###############
echo "Make sure netCDF files have been reordered before archiving!"

set FOLD1 = 'TL0630'
set FOLD2 = 'TL0688'

set YEAR = 1983
set PROJ = "co2"
set PLAT = "kingair_n312d"

### HRT
#/net/work/bin/scripts/mass_store/archAC/archive.py HRT/NetCDF $cwd/$FOLD1 nc /RAF/$FOLD1 stroble@ucar.edu
#/net/work/bin/scripts/mass_store/archAC/archive.py HRT/NetCDF $cwd/$FOLD2 nc /RAF/$FOLD2 stroble@ucar.edu

/net/work/bin/scripts/mass_store/archAC/archive.py HRT/NetCDF $cwd/TL0630 nc /EOL/$YEAR/$PROJ/aircraft/$PLAT stroble@ucar.edu
/net/work/bin/scripts/mass_store/archAC/archive.py HRT/NetCDF $cwd/TL0688 nc /EOL/$YEAR/$PROJ/aircraft/$PLAT stroble@ucar.edu
