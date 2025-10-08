National Weather Service (NWS) High Resolution Radiosonde Data

1.0  General Description

This is one of the upper air data sets developed for the 
Ice in Clouds Experiment - Tropical (ICE-T) 2011 project.  
This data set includes 60 high vertical resolution (1-second) 
Radiosonde Replacement System (RRS) soundings from National 
Weather Service (NWS) rawinsonde stations. This ICE-T data set
includes data from 1 July 2011 through 31 July 2011 for the
NWS station in San Juan, Puerto Rico. The soundings were 
typically released twice a day (0000 and 1200 UTC). The data 
are in EOL Sounding Composite format (columnar ascii).

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

     The following is a sample record of ICE-T 2011 NWS Radiosondee
upper air data in EOL ESC format.   The data portion is much longer 
than 80 characters and, therefore, may wrap around to a second
line.  See section 2.1 for an exact format specification

Data Type:                         National Weather Service Sounding/Ascending
Project ID:                        ICE-T_2011
Release Site Type/Site ID:         TJSJ San Juan, PR / 78526
Release Location (lon,lat,alt):    065 59.51'W, 18 25.88'N, -65.992, 18.431,
3.0
UTC Release Time (y,m,d,h,m,s):    2011, 07, 10, 11:08:26
Ascension Number:                  376
Radiosonde Serial Number:          85208848
Balloon Manufacturer/Type:         Other / GP26
Balloon Lot Number/Weight:         91 / 0.700
Radiosonde Type/RH Sensor Type:    Sippican Mark IIA with chip thermistor,
pressure / Sippican Mark IIA Carbon Hygristor
Surface Observations:              P: 1015.9, T: 999.0, RH: 91.0, WS: 2.1, WD:
113.0
Nominal Release Time (y,m,d,h,m,s):2011, 07, 10, 12:00:00
 Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon
Lat   Ele   Azi    Alt    Qp   Qt   Qrh  Qu   Qv   QdZ
  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg
deg   deg   deg     m    code code code code code code
------ ------ ----- ----- ----- ------ ------ ----- ----- ----- --------
------- ----- ----- ------- ---- ---- ---- ---- ---- ----
   0.0 1015.9  26.6  24.8  90.0   -2.3    1.0   2.5 113.5 999.0  -65.992
18.431 999.0 999.0     3.0  2.0  2.0  2.0  1.0  1.0  9.0
   1.0 1015.7  26.3  24.5  90.0   -2.6    0.4   2.6  98.7   2.0  -65.992
18.431 999.0 999.0     5.0  2.0  2.0  3.0  1.0  1.0 99.0
   2.0 1015.1  26.2  24.4  90.0   -2.9   -0.2   2.9  86.1   6.0  -65.992
18.431 999.0 999.0    11.0  2.0  1.0  3.0  1.0  1.0 99.0
   3.0 1014.4  26.2  24.4  90.1   -3.0   -0.2   3.0  86.2   5.0  -65.992
18.431 999.0 999.0    16.0  1.0  1.0  3.0  1.0  1.0 99.0
   4.0 1013.7  26.2  24.4  90.1   -3.2   -0.2   3.2  86.4   6.0  -65.992
18.431 999.0 999.0    22.0  1.0  1.0  3.0  1.0  1.0 99.0


2.2  Station List

ID   SITE            STATE  COUNTRY   LAT     LONG    ELEV (m) SONDE TYPE
-------------------------------------------------------------------------
TJSJ San Juan          --     PR     18.4    -66.0     3.0     Sippican Mark IIA 


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

None.


5.0  References

Loehrer, S. M., T. A. Edmands, and J. A. Moore, 1996: TOGA COARE
     upper-air sounding data archive: development and quality control
     procedures.  Bull. Amer. Meteor. Soc., 77, 2651-2671.

Loehrer, S. M., S. F. Williams, and J. A. Moore, 1998: Results from
     UCAR/JOSS quality control of atmospheric soundings from field
     projects.  Preprints, Tenth Symposium on Meteorological
     Observations and Instrumentation, Phoenix, AZ, Amer. Meteor.
     Soc., 1-6.
