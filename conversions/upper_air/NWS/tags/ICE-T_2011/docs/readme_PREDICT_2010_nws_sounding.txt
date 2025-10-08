National Weather Service (NWS) High Resolution Radiosonde Data

1.0  General Description

This is one of the upper air data sets developed for the Pre-Depression
Investigation of Cloud-systems in the Tropics (PREDICT) 2010 project.
This data set includes 841 high vertical resolution (1-second) 
Radiosonde Replacement System (RRS) soundings from National 
Weather Service (NWS) rawinsonde stations. For PREDICT this data set
includes data from 1 August 2010 through 30 September 2010 for six (6) 
NWS stations (Key West, FL, Miami, FL, Tampa Bay, FL, Brownsville, TX, 
Corpus Christi, TX, and San Juan, Puerto Rico).  At the request of GRIP
investigators this data set also includes data from 1 September 2010 
through 5 September 2010 for seven (7) stations along the east coast
of the United States (Jacksonville, FL, Charleston, SC, Newport, NC, 
Sterling, VA, Upton, NY, Chatham, MA, and Gray, ME) for Hurricane Earl.  
The soundings were typically released twice a day (0000 and 1200 UTC) and 
any special releases are also included.  The data are in EOL Sounding 
Composite format (columnar ascii).


1.1  Data Set Contact

Steve Williams
NCAR/EOL
sfw@ucar.edu


2.0  Detailed Data Description


2.0.1 National Weather Service High-Resolution Sounding Algorithms

The detailed description of the NWS Radiosonde Replacement System (RRS)
sounding collection and instrumentation is located in
http://www.ua.nws.noaa.gov/RRS.htm   

2.1  Detailed Format Description

     All upper air soundings were converted to the National Center for
Atmospheric Research/Earth Observing Laboratory (NCAR/EOL) Sounding
Composite Format (ESC).  ESC is a version of the National Center for 
Atmospheric Research (NCAR) CLASS format and is an ASCII format 
consisting of 15 header records for each sounding followed by the data 
records with associated QC information.

Header Records

     The header records (15 total records) contain data type, project ID,
site ID, site location, release time, sonde type, meteorological and wind
data processors, and the operator's name and comments.  The first five
header lines contain information identifying the sounding, and have a
rigidly defined form.  The following 7 header lines are used for auxiliary
information and comments about the sounding, and may vary from dataset
to dataset.  The last 3 header records contain header information for the
data columns.  Line 13 holds the field names, line 14 the field units, and
line 15 contains dashes ('-' characters) delineating the extent of the
field.

     The five standard header lines are as follows:


     Line  Label (padded to 35 char)              Contents

       1   Data Type:                        Description of type and
                                               resolution of data.
       2   Project ID:                       ID of weather project.
       3   Release Site Type/Site ID:        Description of release site.
       4   Release Location (lon,lat,alt):   Position of release site, in
                                               format described below.
       5   UTC Release Time (y,m,d,h,m,s):   Time of release, in format:
                                               yyyy, mm, dd, hh:mm:ss


     The release location is given as:  lon (deg min), lat (deg min), lon
(dec. deg), lat (dec. deg), alt (m)

     Longitude in deg min is in the format:  ddd mm.mm'W where ddd
is the number of degrees from True North (with leading zeros if
necessary), mm.mm is the decimal number of minutes, and W represents
W or E for west or east longitude, respectively. Latitude has the same
format as longitude, except there are only two digits for degrees and N or
S for north/south latitude. The decimal equivalent of longitude and
latitude and station elevation follow.

     The seven non-standard header lines may contain any label and
contents.  The labels are padded to 35 characters to match the standard
header lines.  Records for the RRS data at the remaining stations include 
the following non-standard header lines:

       6   Ascension Number:                  64
       7   Radiosonde Serial Number:          85081439
       8   Balloon Manufacturer/Type:         Kaysam / GP26
       9   Balloon Lot Number/Weight:         8 / 0.700
       10  Radiosonde Type/RH Sensor Type:    Sippican Mark IIA with chip thermistor, pressure / Sippican Mark IIA Carbon Hygristor
       11  Surface Observations:              P: 958.1, T: -2.6, RH: 65.0, WS: 10.8, WD: 305.0

Data Records

     The data records each contain time from release, pressure,
temperature, dew point, relative humidity, U and V wind components, wind
speed and direction, ascent rate, balloon position data, altitude, and
quality control flags (see the QC code description).  Each data line
contains 21 fields, separated by spaces, with a total width of 130
characters.  The data are right-justified within the fields.  All fields
have one decimal place of precision, with the exception of latitude and
longitude, which have three decimal places of precision.  The contents
and sizes of the 21 fields that appear in each data record are as follows:

Field     Format
No.   Width          Parameter               Units               Missing
                                                                  Value
------------------------------------------------------------------------
  1     6  F6.1  Time                        Seconds             9999.0
  2     6  F6.1  Pressure                    Millibars           9999.0
  3     5  F5.1  Dry-bulb Temperature        Degrees C            999.0
  4     5  F5.1  Dew Point Temperature       Degrees C            999.0
  5     5  F5.1  Relative Humidity           Percent              999.0
  6     6  F6.1  U Wind Component            Meters / Second     9999.0
  7     6  F6.1  V Wind Component            Meters / Second     9999.0
  8     5  F5.1  Wind Speed                  Meters / Second      999.0
  9     5  F5.1  Wind Direction              Degrees              999.0
 10     5  F5.1  Ascension Rate              Meters / Second      999.0
 11     8  F8.3  Longitude                   Degrees             9999.0
 12     7  F7.3  Latitude                    Degrees              999.0
 13     5  F5.1  Elevation Angle             Degrees              999.0
 14     5  F5.1  Azimuth Angle               Degrees              999.0
 15     7  F7.1  Altitude                    Meters             99999.0
 16     4  F4.1  QC for Pressure             Code (see below)      99.0
 17     4  F4.1  QC for Temperature          Code (see below)      99.0
 18     4  F4.1  QC for Humidity             Code (see below)      99.0
 19     4  F4.1  QC for U Component          Code (see below)      99.0
 20     4  F4.1  QC for V Component          Code (see below)      99.0
 21     4  F4.1  QC for Ascension Rate       Code (see below)      99.0

     Fields 16 through 21 contain the Quality Control information
derived at the NCAR Earth Observing Laboratory (NCAR/EOL).
Any QC information from the original sounding is replaced by the
following EOL codes:

Code      Description

99.0  Unchecked (QC information is "missing.")  ("UNCHECKED")
1.0   Checked, datum seems physically reasonable.  ("GOOD")
2.0   Checked, datum seems questionable on physical basis.("MAYBE")
3.0   Checked, datum seems to be in error.  ("BAD")
4.0   Checked, datum is interpolated.  ("ESTIMATED")
9.0   Checked, datum was missing in original file.  ("MISSING")

Sample Data

     The following is a sample record of PREDICT 2010 NWS Radiosondee
upper air data in EOL ESC format.   The data portion is much longer 
than 80 characters and, therefore, may wrap around to a second
line.  See section 2.1 for an exact format specification

Data Type:                         National Weather Service Sounding/Ascending
Project ID:                        PREDICT_2010
Release Site Type/Site ID:         KKEY Key West, FL / 72201
Release Location (lon,lat,alt):    081 47.32'W, 24 33.18'N, -81.789, 24.553, 13.0
UTC Release Time (y,m,d,h,m,s):    2010, 09, 02, 17:36:33
Ascension Number:                  483
Radiosonde Serial Number:          85158918
Balloon Manufacturer/Type:         Totex / GP26
Balloon Lot Number/Weight:         32010 / 0.700
Radiosonde Type/RH Sensor Type:    Sippican Mark IIA with chip thermistor, 
pressure / Sippican Mark IIA Carbon Hygristor
Surface Observations:              P: 1011.7, T: 999.0, RH: 61.0, WS: 3.1, WD: 110.0
Nominal Release Time (y,m,d,h,m,s):2010, 09, 02, 18:00:00
 Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat   
Ele   Azi    Alt    Qp   Qt   Qrh  Qu   Qv   QdZ
  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   
deg   deg     m    code code code code code code
------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- 
----- ----- ------- ---- ---- ---- ---- ---- ----
   0.0 1011.6  31.0  22.6  61.0   -1.8    1.0   2.1 119.1 999.0  -81.789  24.553 
999.0 999.0    13.0  1.0  1.0  1.0  1.0  1.0  9.0
   1.0 1011.1  30.8  22.5  61.2   -1.5    0.8   1.7 118.1   5.0  -81.789  24.553 
999.0 999.0    18.0  1.0  3.0  3.0  1.0  1.0 99.0
   2.0 1010.5  30.6  22.4  61.5   -1.3    0.7   1.5 118.3   6.0  -81.789  24.553 
999.0 999.0    24.0  1.0  3.0  3.0  1.0  1.0 99.0
   3.0 1009.8  30.4  22.2  61.7   -1.4    0.8   1.6 119.7   6.0  -81.789  24.553 
999.0 999.0    30.0  1.0  3.0  3.0  1.0  1.0 99.0
   4.0 1009.1  30.2  22.1  61.9   -1.5    0.9   1.7 121.0   6.0  -81.789  24.553 
999.0 999.0    36.0  1.0  3.0  3.0  1.0  1.0 99.0
   5.0 1008.3  30.0  22.0  62.2   -1.5    0.9   1.7 121.0   7.0  -81.789  24.553 
999.0 999.0    43.0  1.0  3.0  3.0  1.0  1.0 99.0


2.2  Station List

ID   SITE            STATE  COUNTRY   LAT     LONG    ELEV (m) SONDE TYPE
-------------------------------------------------------------------------
TJSJ San Juan          --     PR     18.4    -66.0     3.0     Sippican Mark IIA 
KCHH Chatham           MA     US     41.7    -70.0    15.0     Sippican Mark IIA
KCHS Charleston        SC     US     32.9    -80.0    13.0     Sippican Mark IIA
KGYX Gray (Portland)   ME     US     43.9    -70.3   124.0     Sippican Mark IIA
KIAD Sterling          VA     US     39.0    -77.5    88.0     Sippican Mark IIA
KJAX Jacksonville      FL     US     30.5    -81.7    10.0     Sippican Mark IIA
KMHX Newport           NC     US     34.8    -76.9    11.0     Sippican Mark IIA
KOKX Upton-Brookhaven  NY     US     40.9    -72.9    20.0     Sippican Mark IIA
KBRO Brownsville       TX     US     25.9    -97.4     7.0     Sippican Mark IIA
KCRP Corpus Christi    TX     US     27.8    -97.5    15.0     Sippican Mark IIA
KKEY Key West          FL     US     24.6    -81.8    13.0     Sippican Mark IIA
KMIA Miami             FL     US     25.8    -80.4     4.0     Sippican Mark IIA
KTBW Tampa Bay         FL     US     27.7    -82.4    13.0     Sippican Mark IIA



These soundings utilized the Global Positioning System (GPS) to derive the winds.  
NCAR/EOL did no additional processing to the wind data provided in these soundings.  
NCAR/EOL utilized the processed version of the RRS data.  This means the data were 
smoothed and had corrections applied (e.g. solar radiation correction) by the NWS.  
NCAR/EOL conducted no additional processing to the data values.  However,
additional quality assurance  was applied as described in the following
section.

3.0  Quality Control Processing


        This dataset underwent an automated QC process.  The dataset
underwent internal consistency checks which included two types of checks,
gross limit checks on all parameters and rate-of-change checks on
temperature, pressure and ascension rate. Some further information on the
QC processing conducted by EOL can be found in Loehrer et al. (1996) and
Loehrer et al. (1998).

3.1  Gross Limit Checks

        These checks were conducted on each sounding and data were
automatically flagged as appropriate.  Only the data point under
examination was flagged.  EOL conducted the following gross limit
checks on the T-REX NWS sounding dataset.  In the table P = pressure, T =
temperature, RH = relative humidity, U = U wind component, V = V wind
component, B = bad, and Q = questionable.


     __________________________________________________________________
                                               Parameter(s)      Flag
     Parameter           Gross Limit Check       Flagged       Applied
     __________________________________________________________________
     Pressure           < 0 mb or > 1050 mb         P             B

     Altitude           < 0 m  or > 40000 m      P, T, RH         Q

     Temperature        < -90C or > 45C             T             Q

     Dew Point          < -99.9C or > 33C          RH             Q
                        > Temperature             T, RH           Q

     Relative Humidity  < 0% or > 100%             RH             B

     Wind Speed         < 0 m/s or > 100 m/s      U, V            Q
                        > 150 m/s                 U, V            B

     U Wind Component   < 0 m/s or > 100 m/s        U             Q
                        > 150 m/s                   U             B

     V Wind Component   < 0 m/s or > 100 m/s        V             Q
                        > 150 m/s                   V             B

     Wind Direction     < 0 deg or > 360 deg      U, V            B

     Ascent Rate        < -10 m/s or > 10 m/s    P, T, RH         Q
     _________________________________________________________________


3.2  Vertical Consistency Checks

     These checks were conducted on each sounding and data were
automatically flagged as appropriate.  These checks were started at the
lowest level of the sounding and compared neighboring 6-sec data points
(except at pressures less than 100 mb where 30-sec average values were
used.  In the case of checks ensuring that the values increased/decreased
as expected, only the data point under examination was flagged.  However,
for the other checks, all of the data points used in the examination were
flagged.   All items within the table are as previously defined.

     _____________________________________________________________________
                      Vertical Consistency        Parameter(s)       Flag
     Parameter               Check                  Flagged        Applied
     _____________________________________________________________________
     Time               decreasing/equal              None           None

     Altitude           decreasing/equal            P, T, RH          Q

     Pressure           increasing/equal            P, T, RH          Q
                        > 1 mb/s or < -1 mb/s       P, T, RH          Q
                        > 2 mb/s or < -2 mb/s       P, T, RH          B

     Temperature        < -15 C/km                  P, T, RH          Q
                        < -30 C/km                  P, T, RH          B
                        >  50 C/km (not applied
                            at p < 250mb)           P, T, RH          Q
                        > 100 C/km (not applied
                            at p < 250mb)           P, T, RH          B
     Ascent Rate        change of > 3 m/s
                               or < -3 m/s             P              Q
                        change of > 5 m/s
                               or < -5 m/s             P              B
     _____________________________________________________________________

3.3  Visual Checks

     All soundings were visually examined utilizing the EOL XQC tool
which allows data quality flags to be adjusted.  Visual examination is 
conducted only up to 50mb.

3.4  Data Quality Comments

KBRO
BRO_20100804114633 no RH above 580mb
KBRO_20100922110758 no RH above 405mb
KBRO_20100925230625 no data above 310mb

KCHH
KCHH_20100903053142 no RH above 210mb

KCHS
KCHS_201009012304 no GPS
KCHS_20100902230403 noisy RH 550-400mb
KCHS_20100903053308 noisy RH 570-480mb

KCRP
KCRP_20100802231508 no GPS
KCRP_20100921230654 no RH above 345mb
KCRP_20100922230648 no RH above 350mb

KGYX

KIAD
KIAD_20100904110543 no GPS

KJAX

KKEY
KKEY_20100908230156 no RH above 285mb
KKEY_20100913230516 no RH above 390mb
KKEY_20100929230141 no RH above 515mb

KMHX

KMIA
KMIA_20100810111323 temp noisy 510-435mb
KMIA_20100810111323 no RH below 370mb

KOKX
KOKX_20100902054000 no data above 175mb
KOKX_20100903235635 no data above 510mb

KTBW
KTBW_20100803230547 no data above 300mb
KTBW_20100822235132 no data above 525mb
KTBW_20100822235132 noisy temp/RH 765-610mb
KTBW_20100824230212.cls temp get cold above 380mb
KTBW_20100904110340 no RH above 410mb (noisy above 500mb)
KTBW_20100907110329 no RH above 395mb
KTBW_20100913230426 no data above 195mb

TJSJ
TJSJ_20100807110800 no RH above 415mb
TJSJ_20100818110900 no RH above 400mb
TJSJ_20100823230939 no RH above 350mb
TJSJ_20100905110929 no data above 960mb
TJSJ_20100910230711 no RH above 590mb
TJSJ_20100911110600 no RH above 495mb
TJSJ_20100914232059 no RH above 595mb
TJSJ_20100915110918 no RH above 630mb
TJSJ_20100915230335 no RH above 470mb
TJSJ_20100916232217 no RH above 500mb 


4.0  RRS Code Tables

The Radisonde Replacement System (RRS) soundings use a set of codes to
define sections of the metadata for the sounding.  The codes are included
in the sounding headers and can be translated with the following tables.

4.1  Code Table 9-1

Radiosonde Type
-------------------------------------------------------------------------
Code           |      Meaning
-------------------------------------------------------------------------
 0-50            Defined or Reserved
 51              VIZ-B2 (USA)
 52              Vaisala RS80-57H
 53-86           Defined or Reserved
 87              Sippican Mark IIA with chip thermistor, pressure
 88-254          Defined or Reserved
 255             Mising value
-------------------------------------------------------------------------

4.2  Code Table 9-9

Balloon Manufacturer
-------------------------------------------------------------------------
 Code          |       Meaning
-------------------------------------------------------------------------
 0               Kaysam
 1               Totex
 2               KKS
 3-61            Reserved
 62              Other
 63              Missing value
-------------------------------------------------------------------------

4.3  Code Table 9-10a

Type of Balloon
-------------------------------------------------------------------------
 Code          |       Meaning
-------------------------------------------------------------------------
 0               GP26
 1               GP28
 2               GP30
 3               HM26
 4               HM28
 5               HM30
 6               SV16
 7-29            Reserved
 30              Other
 31              Missing value
-------------------------------------------------------------------------

5.0  References

Loehrer, S. M., T. A. Edmands, and J. A. Moore, 1996: TOGA COARE
     upper-air sounding data archive: development and quality control
     procedures.  Bull. Amer. Meteor. Soc., 77, 2651-2671.

Loehrer, S. M., S. F. Williams, and J. A. Moore, 1998: Results from
     UCAR/JOSS quality control of atmospheric soundings from field
     projects.  Preprints, Tenth Symposium on Meteorological
     Observations and Instrumentation, Phoenix, AZ, Amer. Meteor.
     Soc., 1-6.

