#! /bin/csh -f
#
# This is an example script.
# Set PROJECT and uncomment each command to archive that data type.

###############
#   Project   #
###############
echo "Make sure netCDF files have been reordered before archiving!"

set FOLD = 'TL1175'
set YEAR = "1985"
set PROJ = "rmm"
set PLAT = "electra_n308d"

### HRT
#/net/work/bin/scripts/mass_store/archAC/archive.py HRT/NetCDF $cwd/HRT nc /RAF/$YEAR/$NUM stroble@ucar.edu

### LRT
/net/work/bin/scripts/mass_store/archAC/archive.py LRT/NetCDF $cwd/$FOLD nc /EOL/$YEAR/$PROJ/aircraft/$PLAT stroble@ucar.edu

