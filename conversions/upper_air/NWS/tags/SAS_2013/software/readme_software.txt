November  2009 LEC

NWS_Converter.pl  - version used to convert NWS data. Including PLOWS,
                    ICE-L, VORTEX2, and START08.

nwsVIZ and nwsVaisala - s/w rebuilt on Pandora with OS Solaris 9 with g++
                        compiler 2.95.3. This was done because Cabernet
                        had to be rebuilt and although the libraries recompiled
                        and formed OK on Cabernet, the nwsVIZ and NWSVaisala
                        s/w would not. But both built fine on Pandora, while
                        compiler glitches were being ironed out on Cabernet.

us_plains_autoqc.properties - standard properties file.

check_class_header.pl - not used.

Note these changes for final ICE-L version of software:
-------------------------------------------------------
12c12
< # Created Circa 1990's by Kenday Southwick.
---
> # Created Circa 1990's by Kendal Southwick.
32,33c32,35
< # Updated project to be for ICE-L 2007 project. - L. Cully. 11 May 2009
< # Updated project to be for POST 2008 project. - L. Cully. 7 May 2009
---
> # August 2009 - L. Cully
> #   Updated project to process PLOWS_2008-2009 data.
> # September 2009 - L. Cully
> #   Updated project to process ICE-L_2007 data.
123c125
< sub getProjectName() { return "ICE-L"; }  # HARDCODED Project Name
---
> sub getProjectName() { return "ICE-L_2007"; }  # HARDCODED Project Name
---end of list---
