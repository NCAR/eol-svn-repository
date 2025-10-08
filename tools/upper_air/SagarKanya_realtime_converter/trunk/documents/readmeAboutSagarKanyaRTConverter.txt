
R/V Sagar Kanya ship sounding converter for Real Time Use
---------------------------------------------------------
(LEC 22 September 2011)

No specific How To has been created for the 
SagarKanyaSounding_converter.pl tool. Read
the header of the s/w to learn how to run
this tool via a crontab and to learn how
it is (as of 22 Sept 2011) being executed
in a real time environment.  To run the tool
at the command line, type SagarKanyaSounding_converter.pl
but beware that this tool has assumptions 
about where the ingest, output, archive, and
gts directories are located. This tool also
automatically emails several DMG people 
monitoring the tool realtime use.  The "processing"
version should be used for all testing but
the same "bewares" should be applied. 

This Perl tool is only part of the process
used to convert the R/V Sagar Kanya soundings.

Process description for the DYNAMO project:

1. The sounding is taken on the Sagar Kanya
and is emailed to the catalog with a specific
header subject. 

2. The catalog user (catuser) then moves the 
sounding from the email to data ingest area at
net/ingest/dynamo/upper_air/RV_Sagar_Kanya/realtime/highres 
(for DYNAMO project).

3. A crontab is being run by Scot Loehrer on merlot
for the DYNAMO project. This cron runs every 10 mins
on the 10's as of 22 September 2011. When a properly
named (see the code for the exact format) is found
in the ingest area, the script converts the data,
makes copies of the input *.tsv file(s) and any
output files (*.cls and *.errlog) and places them
in the archive directory. If and only if the errlog
is empty (no errors during processing), the associated
*.cls file is then copied to the GTS directory.

4. The s/w eng group (SEG/GG) then takes the file
in the GTS dir, runs it through ASPEN to QC the data
and to convert to a GTS message. That GTS message 
is then placed on the GTS for the public to grab.
This completes the process for the DYNAMO project.

---end of file---
