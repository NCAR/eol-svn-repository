How to process the CAESAR 2024 NWS GTS BUFR files into ESC sounding format:

0. See dataset 100.030 and copy all required *.tar files. Tar -xvf the *.tar files and keep only the
   stations need to process for the project.  To form the raw_data file, LEC used
   all *ius*.bufr data files. Not *iuk* files were used since there was one to one match 
   between the available 205 *ius* and *iuk* files.
 
   For CAESAR use only the ENMI files which contain the data for 
   the 4 stations to process (01001, 01004, 01010, 01028)

*** See /net/archive/data/operational/upper*air/radiosonde/GTS_BUFR/*, but confirm /net/archive location.
   This is the location of the data from dataset 100.030.

---------------
1. Convert incoming GTS BUFR NWS files to more readable form:

    preprocess_NWS_GTS_BUFR.pl ../raw_data  ../output_ascii_data ../output_preproc_data  >&  preproc_CAESAR-2024.log &

---------------
2. Convert more readable ascii (*.asc) form of input data to Class form. 

 DROP MANDATORY Recs:
    NWS_GTSBUFR_Radiosonde_Converter.pl  CAESAR_2024 ../output_preproc_data ../output_esc 0 >& runCAESAR_2024.log &

---------------
3. Strip out any recs with Time=-999.99. This step can be done before or after sorting the data in the step below. Probably best to do before Sort command in step 4 below.

   StripRecsMissTime.pl /net/work/Projects/CAESAR/data_processing/upper_air/radiosonde/GTS_BUFR/output_esc/ 
         /net/work/Projects/CAESAR/data_processing/upper_air/radiosonde/GTS_BUFR/output_esc/

OR  StripRecsMissTime.pl . >& strip.log &

---------------
4. Sort the sounding data. See the build.xml file for the proper command to use. 

---------------
5. Run autoqc on the Sorted Class files to generate files used in Visual QC. See the build.xml file.

---------------------------------------
---------------------------------------
