How to process the TORUS 2022 NWS GTS BUFR files into ESC sounding format:

0. See dataset 100.030 and copy all required *.tar files. Tar -xvf the *.tar files and keep only the
   stations need to process for the project. For TORUS, those stations are 
   XXXXXXX

. See /net/archive/data/operational/upper*air/radiosonde/GTS_BUFR/*, but confirm /net/archive location.

1. Convert incoming GTS BUFR NWS files to more readable form:

    preprocess_NWS_GTS_BUFR.pl   ../raw_gts_bufr_data    ../output_ascii_data   ../output_preproc_data  >&  preproc_TORUS.log &


2. Convert more readable ascii (*.asc) form of input data to Class form. 

    NWS_GTSBUFR_Radiosonde_Converter.pl  TORUS_2022 ../output_preproc_data ../output_esc 0 >& runTORUS.log &


3. Strip out any recs with Time=-999.99. This step can be done before or after sorting the data in the step below. Probably best to do before Sort command in step 4 below.

   StripRecsMissTime.pl /net/work/Projects/TORUS/2022/data_processing/upper_air/radiosonde/NWS_GTS_BUFR/output_esc/

OR  StripRecsMissTime.pl . >& strip.log &


4. Sort the sounding data. See the build.xml file for the proper command to use. 


5. Run autoqc on the Sorted Class files to generate files used in Visual QC. See the build.xml file.



---------------------------------------
---------------------------------------
Linda,

In terms of GTS BUFR data files there can be two types:

KALY_202203060000_iuk_sounding.bufr
KALY_202203060000_ius_sounding.bufr

If you have a choice you should use the *_ius_sounding.bufr files. 
The file structures are the same but the ius files contain the 
full sounding while the iuk files contain just the data up to 100mb.

There may be some occasions (likely not in the US) 
where only the iuk files are available so in that case 
we would have to use them, but if both are present we should use the ius.

Scot

----------------------------------------
----------------------------------------
Hi Linda,

Here are links to the software and "how to" for GTS BUFR processing:

GTS_BUFR software: http://svn.eol.ucar.edu/websvn/listing.php?repname=dmg&path=%2Fconversions%2Fupper_air%2FGTS_BUFR%2Ftrunk%2Fsoftware%2F&#ae599a702263514ee3bde21cca133111d

https://internal.eol.ucar.edu/content/how-process-gts-bufr-binary-sounding-data-esc-format

LindaE
------------------------------------
------------------------------------
