PREDICT USAF Dropsonde Reprocessing - 2017

The dropsondes for this project were reprocessed by Kate Young in ISF due to a dry bias error (particularly in colder temperatures). This caused us to reprocess the data from EOL format to ESC format, generating this new version of the data. 

This also impacted the associated sounding data composite, if one was formed for this project.

/net/work/Projects/PREDICT/upper_air/USAF_dropsonde/ESC_Processing/Reprocess_Dropsonde_Corrections

Directories within the /Reprocess_Dropsonde_Corrections:

/build

/dayfiles - Dayfiles of the corrected ESC format data

/test - Testing of the reprocessing converter on PREDICT data

/eol_data: The corrected EOL format data from /net/isf/dropsonde3/kbeierle/PREDICT-2010/USAF_C130/qc_eol/TDDryBiasCorr to be used in the reprocessing effort.

/esc_data: The originally processed uncorrected data from /net/work/Projects/PREDICT/upper_air/USAF_dropsonde/ESC_Processing/final.

/final - The output of the dropsonde reprocessing converter (Dropsonde_Reprocess_Converter.pl) with the ".corr" extension removed.

/output - The output of the dropsonde reprocessing converter (Dropsonde_Reprocess_Converter.pl).

/software - Contains the reprocessing converter (Dropsonde_Reprocess_Converter.pl).

/src - Empty but required to run the "ant dayfiles" command.

