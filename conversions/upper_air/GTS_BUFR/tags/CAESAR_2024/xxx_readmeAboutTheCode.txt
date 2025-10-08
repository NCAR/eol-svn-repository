Added in the scripts, code and additional info for running the CFACT 2022 NWS GTS BUFR data processing. Beware to use the NWS forms of the code to process the NWS GTS BUFR data. The original GTS BUFR conversion s/w (w/o NWS in the *.pl name) will not correctly process the NWS GTS BUFR data. If you are trying to process some ASCII form of the GTS BUFR data, then do NOT use any code in this area. See the GTS_NWS SVN area. That code is located there.

Use the set of code, scripts in this GTS_BUFR svn area to process NWS GTS BUFR (binary) data.

Updated: 29 July 2022 LEC


When CAESAR 2024 GTS BUFR processing was done, all IUS data was processed a GTS_BUFR directory (which is this
top level repo directory). Then the IUK data files were processed in a separate GTS_BUFR_IUK directory.
The GTS_BUFR and GTS_BUFR_IUK directories were at the same dir level when the processing was performed.
After both were processed (but not visually QC'd yet), data from both of those top level dirs' /final directories
was copied into the final_IUS_IUK combo dir found here in repo. SL then asked for updates to some of the
nominal times so that post processing work was done in the final_IUS_IUK_UpdatedNomTime dir.  SL then QC'd the "final"
data in final_IUS_IUK_UpdatedNomTime which was converted into dayfiles and the 5mb dayfiles. 

4 Sept 2024 - LEC

