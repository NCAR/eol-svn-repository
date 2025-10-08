-----------------------------------------------------------
WARNING: Read this before using the software in this SVN
GTS_BUFR/tags/ACCLIP_2020_realtime folders!  This s/w was
based on the SWEX realtime data processing software.
-----------------------------------------------------------
This s/w has not been totally tested. We began the processing
of setting up and creating the realtime data processing/plotting
for the ACCLIP BUFR data but were stopped when a project participant
created similar software. Their s/w is being used in the Dry Run
Field Catalog. 

Note that this s/w can not be used "as is" to post process data
for the archive. See the WARNINGS at the top of the *.pl sofware.
Beware that the nominal time is not processed but is simply 
reassigned to be the same as the actual time. That was all that
was needed for plotting the data.

BEWARE that any s/w used points to the proper ebdump tool!!!

---Notes from previously checkin realtime software.----
This s/w is NOT THE SAME as the GTS BUFR software located 
in the trunk of the SVN GTS BUFR folder.  ONLY USE THIS S/W
TO PROCESS REALTIME GTS BUFR DATA. DO NOT USE THIS S/W TO
PROCESS GTS BUFR DATA NOR THE GTS NWS DATA. There's separate
software (GTSBUFR_Radiosonde_Converter.pl) to do that processing. 
For the non-realtime data process for GTS BUFR data see
the SOCRATES or RELAMAPGO tags on the GTS_BUFR folder.   
For processing the GTS NWS data, see the GTS_NWS folder 
(separate from the GTS_BUFR folder in SVN).

LECully
1 July 2020
