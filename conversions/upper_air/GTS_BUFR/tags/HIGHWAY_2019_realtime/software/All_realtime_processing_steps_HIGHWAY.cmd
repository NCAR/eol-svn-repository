Steps for Converting *.bfr Files to Sorted *.cls Files to Plot   - Updated for HIGHWAY on 27 August 2019 LEC
--------------------------------------------------------------

SetUp: 
       Create a /lib directory one level up from where you are running. Put the libeccodes.so file in /lib.

       To run with the exact commands below, you need to create the following dirs:
       ../raw_gts_bufr_data,  ../output_ascii_data,  ../output_preproc_dat, and ../output_esc.

       Also, the "ant sort_esc" calls code that expects the ./src directory to exist. It does not need to
       have anything in it. ./src just needs to exist.

       Expected input file naming convention: HIGHWAY_HKNC_YYYYMMDDhhmm_ius_sounding.bufr
                                        (eg., HIGHWAY_HKNC_201908151200_ius_sounding.bufr)
       Note that Greg says he can rename the files during realtime processing.
   
       Greg is grabbing the raw data from the LDM and renaming the file and then placing those files at ??????

       Data from HKNC (Nairobi, Kenya) is expected to be sent at 00 and 12 UTC only. Is does not need 
       to be captured immediately?


Execution Steps:

1. Convert BUFR to Readable ASCII:
  preprocess_GTS_BUFR.pl ../raw_gts_bufr_data ../output_ascii_data ../output_preproc_data >& preproc_HIGHWAY.log &



2. Convert Readable ASCII to ESC (*.cls) format:
  GTSBUFR_Radiosonde_Converter_RealTime.pl  HIGHWAY ../output_preproc_data ../output_esc 0 >& runHIGHWAY_rt.log & 

**Currently generates a "warning.log" file that will be empty. May decide to write errors and
messages to that file.


3. Sort ESC (*.cls) files:  
  ant sort_esc

  Notes on Step 3: Ensure that ./src exists. Executing this ant cmd will generate the 
  record_sorter.log file in the ../output_esc dir. Once done, you will find *.cls and *.cls.unsort 
  files. Plot the *.cls files. This may also cause any "0.00" values to be "-0.00" or
  vice versa depending on the machine you are running on.


4. Plot sorted ESC files:  See Janet's cron script that controls all processing
   including plotting and moving the plots to the field catalog ingest/ftp area.

-----end of file-----
