National Weather Service (NWS) High Resolution Rawinsonde Data

1.0  General Description

This is one of the upper air data sets developed for the 
Stratosphere-Troposphere Analyses of Regional Transport 2008 (START08).
This data set includes 8407 high vertical resolution (1-second) 
Radiosonde Replacement System (RRS) from 50 National Weather Service (NWS) 
rawinsonde stations in the START08 area of interest.  Additionally, 
3249 high vertical resolution (6-second) MicroArt soundings from 20 stations
included.  The data cover the period from time 12:00 on 1 April 2008 to 
30 June 2008.  The soundings were typically released twice a 
day (0000 and 1200 UTC). Note that not all of the 00:00 1 April 2008 soundings
were included in this dataset. Three stations (Albuquerque, NM; Del Rio, TX; Santa
Teresa/El Paso, TX) switched from MicroArt to RRS soundings during this time period.
The data are in EOL Sounding Composite format (columnar ascii).

1.1  Data Set Contact

Steve Williams
NCAR/EOL
sfw@ucar.edu


2.0  Detailed Data Description


2.0.1 National Weather Service High-Resolution Sounding Algorithms

The detailed description of NWS MicroArt sounding collection and
instrumentation is located in NWS (1991).  

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
header lines. Records for the MicroArt data at Topeka, KS include the 
following three non-standard header lines.

     Line  Label (padded to 35 char)              Contents

       6   Ascension No:                      1299
       7   Radiosonde Serial Number:          152551614
       8   Radiosonde Manufacturer:           Vaisala

Records for the RRS data at the remaining stations include the following
non-standard header lines:

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

     The following is a sample record of START08 Sounding Rawinsonde NWS
upper air data in EOL ESC format.   The data portion is
much longer than 80 characters and, therefore, may wrap around to a second
line.  See section 2.1 for an exact format specification

Data Type:                         National Weather Service Sounding/Ascending
Project ID:                        START08
Release Site Type/Site ID:         KSGF Springfield, MO / 72440
Release Location (lon,lat,alt):    093 24.13'W, 37 14.15'N, -93.402, 37.236, 391.0
UTC Release Time (y,m,d,h,m,s):    2008, 04, 23, 23:09:19
Ascension Number:                  241
Radiosonde Serial Number:          85049639
Balloon Manufacturer/Type:         Kaysam / GP26
Balloon Lot Number/Weight:         261007 / 0.700
Radiosonde Type/RH Sensor Type:    Sippican Mark IIA with chip thermistor,
 pressure / Sippican Mark IIA Carbon Hygristor
Surface Observations:              P: 968.1, T: 14.2, RH: 53.0, WS: 6.2, WD: 154.0
Nominal Release Time (y,m,d,h,m,s):2008, 04, 24, 00:00:00
 Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat   
Ele   Azi    Alt    Qp   Qt   Qrh  Qu   Qv   QdZ
  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   
deg   deg     m    code code code code code code
------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- 
----- ----- ------- ---- ---- ---- ---- ---- ----
   0.0  968.3  25.6  15.6  54.0   -2.3    4.0   4.6 150.1 999.0  -93.402
37.236 999.0 999.0   391.0  1.0  1.0  1.0  1.0  1.0  9.0
   1.0  968.1  25.5  15.5  53.9   -2.2    4.6   5.1 154.4   2.0  -93.402
37.236 999.0 999.0   393.0  1.0  1.0  3.0  1.0  1.0 99.0
   2.0  967.6  25.4  15.4  53.8   -2.2    5.1   5.6 156.7   4.0  -93.403
37.237 999.0 999.0   397.0  1.0  1.0  3.0  1.0  1.0 99.0
   3.0  967.1  25.4  15.3  53.6   -2.3    5.3   5.8 156.5   5.0  -93.403
37.237 999.0 999.0   402.0  1.0  1.0  3.0  1.0  1.0 99.0
   4.0  966.6  25.4  15.3  53.5   -2.4    5.6   6.1 156.8   5.0  -93.403
37.237 999.0 999.0   407.0  1.0  1.0  3.0  1.0  1.0 99.0
   5.0  966.0  25.3  15.2  53.4   -2.5    5.8   6.3 156.7   5.0  -93.403
37.237 999.0 999.0   412.0  1.0  1.0  3.0  1.0  1.0 99.0


2.2  Data Remarks

     For the MicroArt station data the use of the raw 6-sec resolution 
elevation and azimuth angle data to derive the winds sometimes led to 
large oscillations in wind speed, due to the presence of oscillations 
in the elevation angle data, particularly at low elevation angles.  
The general approach to correct this problem was to remove the out-lier 
radiosonde position data before computing the wind components (Williams et al. 1993).  
For both the azimuth and elevation angles from 360 sec to the end of the sounding,
a ninth order polynomial was fit to the curve.  The residuals were
calculated and compared to the observed values.  The outliers of the
residuals were then removed.

    Then to help correct the more extensive problems at low elevation
angles within 10 degrees of the limiting angles (LA) some additional
smoothing was applied.  If the elevation angle was between (LA + 7.5)
and (LA + 10), the new elevation angle was computed with a 2 min linear
fit.  If the elevation angle was between (LA + 5) and (LA + 7.5), the new
elevation angle was computed with a 3 min linear fit.  If the elevation
angle was less than (LA + 5), the new elevation angle was computed with
a 4 min linear fit.  If the number of observations with low elevation angles
was greater than 20% of the total number of observations for the sounding
no frequency smoothing occurred.

     Then, for the elevation angle only, a finite Fourier analysis was
performed on the residuals.  Periods from 90-190 sec were removed and
those below 30 sec were flattened.

     Finally, a 2 min second order polynomial was then fit to the
position to derive the u and v wind components, except for the beginning
and end minute (or 1.5 minutes if over 50 mb) which used a 3 min fit.  If
there were less than 15% of the total number of points, not counting the
beginning or end of the flight, on one side of the point for which the wind
value was being computed, a linear fit was used.

     For further information on this methodology and its changes since
Williams et al. (1993) please see Williams, et al. (1998).

2.3  Station List

ID   SITE            STATE  COUNTRY    LONG    LAT   ELEV (m) SONDE TYPE
-------------------------------------------------------------------------
20 MicroArt Stations
--------------------
ABQ Albuquerque        NM     US     -106.6   35.0   1615.0   VIZ B2
BRO Brownsville        TX     US      -97.4   25.9      7.0   VIZ B2
CAR Caribou            ME     US      -68.0   46.9    191.0   VIZ B2
CHH Chatham            MA     US      -70.0   41.7     16.0   Vaisala
DDC Dodge City         KS     US     -100.0   37.8    788.0   VIZ B2
DNR Denver INT APT     CO     US     -104.9   39.8   1611.0   Vaisala
DRA Desert Rock        NV     US     -116.0   36.6   1007.0   VIZ B2
DRT Del Rio            TX     US     -100.9   29.4    314.0   VIZ B2
DTX White Lake         MI     US      -83.5   42.7    329.0   Vaisala
EPZ Santa Teresa       NM     US     -106.7   31.9   1252.0   Vaisala
GRB Green Bay          WI     US      -88.1   44.5    214.0   VIZ B2
INL Int'l Falls        MN     US      -93.4   48.5    361.0   Vaisala
KEY Key West           FL     US      -81.8   24.5     15.0   VIZ B2
MFR Medford            OR     US     -122.9   42.4    397.0   VIZ B2
NKX San Diego          CA     US     -117.1   32.8    134.0   VIZ B2
OAK Oakland            CA     US     -122.2   37.7      2.0   VIZ B2
OKX Brookhaven         NY     US      -72.9   40.9     20.0   Vaisala
PBZ Pittsburgh         PA     US      -80.2   40.5    360.0   VIZ B2
TFX Great Falls        MT     US     -111.4   47.5   1132.0   VIZ B2
TOP Topeka             KS     US      -95.6   39.1    270.0   Vaisala

50 RRS stations
---------------
KABQ Albuquerque       NM     US    -106.6     35.0 1619.0    Sippican Mark IIA
KABR Aberdeen          SD     US     -98.4     45.5  398.0    Sippican Mark IIA
KAMA Amarillo          TX     US    -101.7     35.2 1095.0    Sippican Mark IIA
KAPX Gaylord           MI     US     -84.7     44.9  448.0    Sippican Mark IIA
KBIS Bismarck          ND     US    -100.8     46.8  506.0    Sippican Mark IIA
KBMX Birmingham        AL     US     -86.8     33.2  174.0    Sippican Mark IIA     
KBOI Boise             ID     US    -116.2     43.6  873.0    Sippican Mark IIA
KBUF Buffalo           NY     US     -78.7     42.9  218.0    Sippican Mark IIA
KCHS Charleston        SC     US     -80.0     32.9   13.0    Sippican Mark IIA
KCRP Corpus Christi    TX     US     -97.5     27.8   15.0    Sippican Mark IIA
KDRT Del Rio           TX     US    -100.9     29.4  290.0    Sippican Mark IIA
KDVN Quad Cities       IA     US     -90.6     41.6  230.0    Sippican Mark IIA
KEPZ Santa Teresa/El Paso  NM US    -106.7     31.9 1254.0    Sippican Mark IIA
KFFC Peachtree City    GA     US     -84.6     33.4  245.0    Sippican Mark IIA
KFGZ Flagstaff         AZ     US    -111.8     35.2 2179.0    Sippican Mark IIA
KFWD Fort Worth        TX     US     -97.3     32.8  195.0    Sippican Mark IIA
KGGW Glasgow           MT     US    -106.6     48.2  693.0    Sippican Mark IIA
KGJT Grand Junction    CO     US    -108.5     39.1 1474.0    Sippican Mark IIA
KGSO Greensboro        NC     US     -79.9     36.1  276.0    Sippican Mark IIA
KGYX Gray (Portland)   ME     US     -70.3     43.9  124.0    Sippican Mark IIA
KIAD Sterling          VA     US     -77.5     39.0   86.0    Sippican Mark IIA
KILN Wilmington        OH     US     -83.8     39.4  323.0    Sippican Mark IIA
KILX Lincoln           IL     US     -89.3     40.2  179.0    Sippican Mark IIA
KJAN Jackson           MS     US     -90.1     32.3   91.0    Sippican Mark IIA
KJAX Jacksonville      FL     US     -81.7     30.5   10.0    Sippican Mark IIA
KLBF North Platte      NE     US    -100.7     41.1  849.0    Sippican Mark IIA
KLCH Lake Charles      LA     US     -93.2     30.1    5.0    Sippican Mark IIA
KLIX Slidell           LA     US     -89.8     30.3   10.0    Sippican Mark IIA
KLKN Elko              NV     US    -115.7     40.9 1593.0    Sippican Mark IIA
KLZK Little Rock       AR     US     -92.3     34.8  173.0    Sippican Mark IIA
KMAF Midland           TX     US    -102.2     31.9  874.0    Sippican Mark IIA
KMFL Miami             FL     US     -80.4     25.8    4.0    Sippican Mark IIA
KMPX Minneapolis       MN     US     -93.6     44.8  290.0    Sippican Mark IIA
KMHX Newport           NC     US     -76.9     34.8   11.0    Sippican Mark IIA
KOAX Omaha/Valley      NE     US     -96.4     41.3  351.0    Sippican Mark IIA
KOHX Old Hickory/Nashville TN US     -86.6     36.3  180.0    Sippican Mark IIA
KOTX Spokane           WA     US    -117.6     47.7  729.0    Sippican Mark IIA
KOUN Norman            OK     US     -97.4     35.2  345.0    Sippican Mark IIA
KRAP Rapid City        SD     US    -103.2     44.1 1029.0    Sippican Mark IIA
KREV Reno              NV     US    -119.8     39.6 1518.0    Sippican Mark IIA
KRIW Riverton          WY     US    -108.5     43.1 1699.0    Sippican Mark IIA
KRNK Blacksburg        VA     US     -80.4     37.2  639.0    Sippican Mark IIA
KSGF Springfield       MO     US     -93.4     37.2  391.0    Sippican Mark IIA
KSHV Shreveport        LA     US     -93.8     32.5   85.0    Sippican Mark IIA
KSLC Salt Lake City    UT     US    -112.0     40.8 1289.0    Sippican Mark IIA
KSLE Salem             OR     US    -123.0     44.9   62.0    Sippican Mark IIA
KTAE Tallahassee       FL     US     -84.3     30.4   53.0    Sippican Mark IIA
KTBW Tampa Bay         FL     US     -82.4     27.7   13.0    Sippican Mark IIA
KTWC Tucson            AZ     US    -111.0     32.2  751.0    Sippican Mark IIA
KUIL Quillayute        WA     US    -124.6     47.9   57.0    Sippican Mark IIA

The RRS soundings utilized the Global Positioning System (GPS) to derive the winds.  
NCAR/EOL did no additional processing to the wind data provided in these
soundings.  NCAR/EOL utilized the processed version of the RRS data.  This means the
data were smoothed and had corrections applied (e.g. solar radiation correction) 
applied by the NWS.  NCAR/EOL conducted no additional processing on these data.


3.0  RRS Code Tables

The Radisonde Replacement System (RRS) soundings use a set of codes to
define sections of the metadata for the sounding.  The codes are included
in the sounding headers and can be translated with the following tables.

3.1  Code Table 9-1

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

3.2  Code Table 9-9

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

3.3  Code Table 9-10a

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

4.0  References

NWS, 1991:  Micro-ART Observation and Rework Programs Technical
     Document,      National Weather Service, National Oceanic and
     Atmospheric    Administration, Washington, D.C., March 1991.

Williams, S. F., C. G. Wade, and C. Morel, 1993:  A comparison of high
     resolution radiosonde winds: 6-second Micro-ART winds versus
     10-second CLASS LORAN winds.  Preprints, Eighth Symposium
     on Meteorological Observations and Instrumentation, Anaheim,
     California, Amer. Meteor. Soc., 60-65.

Williams, S. F., S. M. Loehrer, and D. R. Gallant, 1998: Computation of
     high-resolution National Weather Service rawinsonde winds.
     Preprints, Tenth Symposium on Meteorological Observations and
     Instrumentation, Phoenix, AZ, Amer. Meteor. Soc., 387-391.
