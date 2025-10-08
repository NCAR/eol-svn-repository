National Weather Service (NWS) High Resolution Rawinsonde Data

1.0  General Description

This is one of the upper air data sets developed for the Verification
of the Origins of Rotation in Tornadoes Experiment 2 (VORTEX2 2009).
This data set includes 1986 high vertical resolution (1-second) 
Radiosonde Replacement System (RRS) from 16 National Weather Service (NWS) 
rawinsonde stations in the VORTEX2 2009 area of interest.  Additionally, 
125 high vertical resolution (6-second) MicroArt soundings from Dodge City, KS 
NWS rawinsonde are included.  The data cover the period from 1 May to 
30 June 2009.  The soundings were typically released twice a 
day (0000 and 1200 UTC).  The data are in EOL Sounding Composite 
format (columnar ascii).


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

     The following is a sample record of ICE-L 2007 Sounding Rawinsonde NWS
upper air data in EOL ESC format.   The data portion is
much longer than 80 characters and, therefore, may wrap around to a second
line.  See section 2.1 for an exact format specification

Data Type:                         National Weather Service Sounding.
Project ID:                        ICE-L_2007
Release Site Type/Site ID:         ABQ Albuquerque, NM
Release Location (lon,lat,alt):    106 36.00'W, 35 00.00'N, -106.600, 35.000, 1615.0
UTC Release Time (y,m,d,h,m,s):    2007, 11, 28, 23:00:00
Ascension No:                      1665
Radiosonde Serial Number:          84999761.CSN
Radiosonde Manufacturer:           VIZ B2
/
/
/
Nominal Release Time (y,m,d,h,m,s):2007, 11, 29, 00:00:00
 Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat   
Ele   Azi    Alt    Qp   Qt   Qrh  Qu   Qv   QdZ
  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   
deg   deg     m    code code code code code code
------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- 
----- ----- ------- ---- ---- ---- ---- ---- ----
   0.0  842.9  10.6 -14.2  16.0    0.0   -5.7   5.7 360.0 999.0 -106.600  35.000 
999.0 999.0  1615.0  1.0  1.0  1.0  1.0  1.0  9.0
   6.0  838.3  10.2 -21.9   8.6    0.2   -5.6   5.6 358.0   7.5 -106.599  34.997  
20.8   1.4  1660.0  1.0  1.0  4.0  4.0  4.0 99.0
  12.0  834.4   9.7 -21.4   9.3    0.3   -5.6   5.6 356.9   6.5 -106.599  34.994  
23.7   2.4  1699.0  1.0  1.0  4.0  4.0  4.0 99.0
  18.0  830.5   9.3 -21.1   9.8    0.5   -5.5   5.5 354.8   6.3 -106.599  34.993  
26.7   3.4  1737.0  2.0  2.0  2.0  4.0  4.0 99.0
  24.0  827.2   8.8 -21.0  10.2    0.7   -5.5   5.5 352.7   5.5 -106.598  34.991  
26.8 359.5  1770.0  2.0  2.0  2.0  4.0  4.0 99.0


2.2  Data Remarks

     For the MicroArt station data at Topeka, KS the use of the raw 
6-sec resolution elevation and azimuth angle
data to derive the winds sometimes led to large oscillations in wind
speed, due to the presence of oscillations in the elevation angle data,
particularly at low elevation angles.  The general approach to correct
this problem was to remove the out-lier radiosonde position data before
computing the wind components (Williams et al. 1993).  For both the
azimuth and elevation angles from 360 sec to the end of the sounding, a
ninth order polynomial was fit to the curve.  The residuals were
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

ID   SITE            STATE  COUNTRY    LONG     LAT   ELEV (m) SONDE TYPE
-------------------------------------------------------------------------
DDC  Dodge City        KS     US    -100.0     37.8  788.9     VIZ B2
KABQ Albuquerque       NM     US    -106.6     35.0 1619.0     Sippican Mark IIA
KABR Aberdeen          SD     US     -98.4     45.5  398.0     Sippican Mark IIA
KAMA Amarillo          TX     US    -101.7     35.2 1095.0     Sippican Mark IIA
KDNR Denver            CO     US    -104.9     39.8 1611.0     Sippican Mark IIA
KFWD Fort Worth        TX     US     -97.3     32.8  195.0     Sippican Mark IIA
KLBF North Platte      NE     US    -100.7     41.1  849.0     Sippican Mark IIA
KLZK Little Rock       AR     US     -92.3     34.8  173.0     Sippican Mark IIA
KMAF Midland           TX     US    -102.2     31.9  874.0     Sippican Mark IIA
KMPX Minneapolis       MN     US     -93.6     44.8  290.0     Sippican Mark IIA
KOAX Omaha/Valley      NE     US     -96.4     41.3  351.0     Sippican Mark IIA
KOUN Norman            OK     US     -97.4     35.2  345.0     Sippican Mark IIA
KRAP Rapid City        SD     US    -103.2     44.1 1029.0     Sippican Mark IIA
KRIW Riverton          WY     US    -108.5     43.1 1699.0     Sippican Mark IIA
KSGF Springfield       MO     US     -93.4     37.2  391.0     Sippican Mark IIA
KSHV Shreveport        LA     US     -93.8     32.5   85.0     Sippican Mark IIA
KTOP Topeka            KS     US     -95.6     39.1  268.0     Sippican Mark IIA


Note all but the Topeka, Kansas site used the Radiosonde Replacement System 
(RRS).   These soundings utilized the Global Positioning System (GPS) to derive
the winds.  NCAR/EOL did no additional processing to the wind data provided in these
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
