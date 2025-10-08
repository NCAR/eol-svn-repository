The UAH_Mobile_Sounding_Converter.pl script was created during DC3 and expects the raw data to consist of a *.LOG file.

The MIPS_Mobile_Radiosonde_Converter.pl script was created for the PECAN 2015 project with raw data consisting of two separate files. The converter reads the *.STD files to get the header info when loadHeaderInfo is called. It reads the *.LOG to get the raw data records.

(LEH 19 Jan 2016)
