
***********************************************************************************************
Spring 2020 - SWEX Postponed!
******************************
The SWEX project was postponed in late Spring 2020 to take place the following year 2021
but at the same time of year.  So this software has not been completely tested nor finalized.
Janet Scannell worked on the control software that calls the software in these dirs. She has
also placed that control software into Subversion. Search for that software to create the 
required SWEX realtime input BUFR sounding data to plots for the SWEX Field Catalog. 
***********************************************************************************************

This work area is for Realtime GTS BUFR processing of SWEX project data, ONLY!
Not for final data processing.
LEC

Need agreed upon naming convention for bufr files - need finalized.
Will s/w recognise the Vaisala and GRAW types. The code does specific
matches.    NEED Example files!


------------------------

Emailed info from CC on 5 Feb 2020:

1. There 4 radiosonde systems (2 - EOL/ISS, 2 - San Jose State)
2. EOL will be using the Vaisala system and San Jose State will be using a Graw system
3. Radiosonde launches will occurly 3-hourly during IOPs and EOPs (not regularly)
4. There's a possibility that one of the San Jose State systems could be mobile (will confirm tomorrow at the planning meeting if Craig Clements is there)

We spent a good amount of time talking about sending the BUFR files to GTS 
and generating the skewTs. We did just create the ftp location for everyone 
to send the BUFR files.

/net/ftp/pub/data/incoming/swex/radiosonde

Right now, that location is just an anonymous ftp location. I'm going 
to check with the science team tomorrow to verify if they do/don't want 
this location to be password protected.

Anyways, Holger and Bill were suggesting that since 
Linda/Janet will likely need to create the skewTs 
for the 2 San Jose States sites, then it would make 
sense for them to also create the skewTs for 
the 2 EOL/ISS sites. I already spoke with Linda and Janet 
about this, and they seemed to think that was a pretty good idea. 
This way all the skewTs for SWEX will be generated using 
the same processing and directly sent to the field catalog.
-------------------------

