#! /bin/csh -f
#
# This is an example script.
# Set PROJECT and uncomment each command to archive that data type.

###############
#   Project   #
###############
echo "Make sure netCDF files have been reordered before archiving!"

### HRT
/net/work/bin/scripts/mass_store/archAC/archive.py HRT/NetCDF $cwd nc /RAF/1993/226
