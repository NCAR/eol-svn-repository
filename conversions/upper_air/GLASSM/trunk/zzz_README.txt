The files and directories contained in the GLASSM sounding conversion are
a combination of two philosophies on conversion.

The original BAMEX conversion was done by Darren Gallant and was ebuffered
at that time.  The software that was available at the time it was added to
the subversion repository is included along with most of the directory
structure.

In 2007, new data was received for the MIPS station and was processed by
Joel Clawson.  At this time, there was no longer EOL was no longer 
ebuffering data and was put online in ESC/CLASS formatted day files.

Because the two versions are merged together (since only the MIPS data
was reprocessed and not the mobile1 and mobile2 stations) the software
should be cleaned up for the next project that will use this software.  The
best conversion software is the Java files and Ant layout located within
the mips_2007 directory.  Any further processing should use this as the
starting point for the processing and clean up the directory structure
to match this at that time.

- Joel Clawson - January 2008