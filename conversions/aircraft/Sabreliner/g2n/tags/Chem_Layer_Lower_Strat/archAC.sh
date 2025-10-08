#! /bin/csh -f
#
# This is an example script.
# Set PROJECT and uncomment each command to archive that data type.

###############
#   Project   #
###############
echo "Make sure netCDF files have been reordered before archiving!"
set PROJECT = 'Chem_Layer_Lower_Strat'
set NUM = '739'
set YEAR = '1991'

### HRT
#/net/work/bin/scripts/mass_store/archAC/archive.py HRT/NetCDF /net/work/Projects/g2n_conversion/$PROJECT/HRT nc /RAF/$YEAR/$NUM

### LRT
/net/work/bin/scripts/mass_store/archAC/archive.py LRT/NetCDF /net/work/Projects/g2n_conversion/$PROJECT/LRT nc /RAF/$YEAR/$NUM

