How to process the SWEX 2022 Santa Barbara Fire Dept BUFR files into ESC sounding format:
-----------------------------------------------------------------------------------------

0. The "Raw" BUFR files for the Santa Barbara Fire Dept (2 stations: SBFDHQ and SBFDS38) were 
   FTP'd to /net/ftp/pub/data area for SWEX during the field phase in Spring 2022. Note that only
   the 96 SBFDHQ files (where the last five of the HQ files are actually from Montecito) and 
   the 91 files from SBFDS38 should be processed. Note that the SBFDS18 files in the FTP area
   were collected before the project began (during a test phase) and then that equipment was
   moved to the SBFDS38 site. Do not process the five SBFDS18 BUFR files for SWEX. 

   Rename raw input files to have "bufr" as suffix for preprocessing code.  


1. Convert incoming BUFR data files to more readable forms:
   preprocess_GTS_BUFR.pl ../raw_data ../output_ascii_data ../output_preproc_data > & preproc_SWEX_SB.log &


2. Convert more readable ascii (*.asc) form of input data to ESC (*.cls) Class form. Drop all Man/Sig recs
   so set last input parameter to zero as show next. 

    SWEX_GTSBUFR_Radiosonde_Converter.pl  SWEX_2022 ../output_preproc_data ../output_esc 0 >& runSWEX.log &

3. Strip out any recs with Time=-999.99. This step can be done before or after sorting the data in the step below. Probably best to do before Sort command in step 4 below.

   StripRecsMissTime.pl /net/work/Projects/CFACT/data_processing/upper_air/radiosonde/NWS_GTS_BUFR/output_esc/

OR  StripRecsMissTime.pl . >& strip.log &

    StripRecsMissTime.pl /net/work/Projects/SWEX/data_processing/upper_air/radiosonde/SantaBarbaraFireDept/output_esc/



4. Sort the sounding data. See the build.xml file for the proper command to use. 
    ant sort_esc   (This command also does the Check Format command.) 

4A. Hand strip any duplicate recs that check_format finds in sorted data. Error will be like 
    "The sounding does not have a consistant time sequence at times 3170.0 and 3170.0.". Generally,
    it's the second record that needs to be stripped and it will all or almost all the fields
    as missing. Note that the rec to strip can generally be found as the last record in the 
    non-sorted /output_esc/*.cls files.  Hand correct the sorted version and this must be done
    before doing the autoQC processing step. 


5. Run autoqc on the Sorted Class files to generate files used in Visual QC. See the build.xml file.
   ant autoqc    (This command also does the Check Format command on QC'd output.)


6. Notify SL that he can do Visual QC. Once done there are *.cls.qc files in the /final directory.
