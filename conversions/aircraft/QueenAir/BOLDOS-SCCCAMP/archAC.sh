#! /bin/csh -f
#
# This is an example script.
# Set PROJECT and uncomment each command to archive that data type.

###############
#   Project   #
###############
echo "Make sure netCDF files have been reordered before archiving!"

set PROJ = 'boldos-scccamp'
set YEAR = '1985'
set PLAT = "queenair_n306d"
### HRT
#/net/work/bin/scripts/mass_store/archAC/archive.py HRT/NetCDF $cwd/TL1170 nc /EOL/$YEAR/$PROJ/aircraft/$PLAT stroble@ucar.edu
#/net/work/bin/scripts/mass_store/archAC/archive.py HRT/NetCDF $cwd/TL1175 nc /EOL/$YEAR/$PROJ/aircraft/$PLAT stroble@ucar.edu
/net/work/bin/scripts/mass_store/archAC/archive.py HRT/NetCDF $cwd/TL1170/new nc /EOL/$YEAR/$PROJ/aircraft/$PLAT stroble@ucar.edu

### LRT
#/net/work/bin/scripts/mass_store/archAC/archive.py LRT/NetCDF $cwd/LRT nc /RAF/$YEAR/$NUM stroble@ucar.edu

