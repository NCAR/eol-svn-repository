# Alley Robinson
# @version VORTEX-SE 2017 MSU Mobile Sounding Data
#       This script removes all spaces
#       in the raw data files and spaces in the msu_2017_locs.txt file
#   
#       This script is meant to be ran before the perl converter
#
#*************************************************************
sed -i 's/ //g' /net/work/Projects/VORTEX-SE/2017/data_processing/upper_air/radiosonde/MSU/raw_data/*.csv
sed -i 's/ //g' /net/work/Projects/VORTEX-SE/2017/data_processing/upper_air/radiosonde/MSU/software/msu_2017_locs.txt
