How to process the TORUS_LItE 2023 NWS GTS BUFR files into ESC sounding format:

0. See dataset 100.030 and copy all required *.tar files. Tar -xvf the *.tar files and keep only the
   stations need to process for the project. For TORUS-LItE, the stations were similar to TORUS 2022. 
   The data was pulled out of the original tar files by SLoeher. To form the raw_data file, LEC used
   all *ius*.bufr data files that SL created plus 10 additional *iuk*.bufr files to fill in some time
   gaps.

*** See /net/archive/data/operational/upper*air/radiosonde/GTS_BUFR/*, but confirm /net/archive location.
   This is the location of the data from dataset 100.030.

---------------
1. Convert incoming GTS BUFR NWS files to more readable form:

    preprocess_NWS_GTS_BUFR.pl ../raw_data  ../output_ascii_data ../output_preproc_data  >&  preproc_TORUS_LItE-2023.log &

---------------
2. Convert more readable ascii (*.asc) form of input data to Class form. 

    NWS_GTSBUFR_Radiosonde_Converter.pl  TORUS_LItE_2023 ../output_preproc_data ../output_esc 0 >& runTORUS_LItE_2023.log &

---------------
3. Strip out any recs with Time=-999.99. This step can be done before or after sorting the data in the step below. Probably best to do before Sort command in step 4 below.

   StripRecsMissTime.pl /net/work/Projects/TORUS/TORUS-LItE/data_processing/upper_air/radiosonde/NWS_GTS_BUFR/output_esc/

/net/work/Projects/TORUS/2022/data_processing/upper_air/radiosonde/NWS_GTS_BUFR/output_esc/

OR  StripRecsMissTime.pl . >& strip.log &

---------------
4. Sort the sounding data. See the build.xml file for the proper command to use. 

---------------
5. Run autoqc on the Sorted Class files to generate files used in Visual QC. See the build.xml file.

---------------------------------------
---------------------------------------
