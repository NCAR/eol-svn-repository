#************************************************************* 
# @author Alley Robinson 
# @version VORTEX-SE 2017 ULM Mobile Sounding Data
#	This script removes all spaces and  %END% terminators
#	in the raw data files and spaces in the ULM_sfc_alt_2017 
#	text file.
#	This script is meant to be ran before the perl converter
#
#*************************************************************
sed -i 's/ //g' /net/work/Projects/VORTEX-SE/2017/data_processing/upper_air/radiosonde/ULM_mobile/raw_data/*.txt
sed -i '/%END%/d' /net/work/Projects/VORTEX-SE/2017/data_processing/upper_air/radiosonde/ULM_mobile/raw_data/*.txt
sed -i 's/ //g' /net/work/Projects/VORTEX-SE/2017/data_processing/upper_air/radiosonde/ULM_mobile/software/ULM_sfc_alt_2017.txt
