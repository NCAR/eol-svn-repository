CuPIDO 2006 Quality Controlled Radiosonde Data Set - Addendum

1.0  General Description

     The NCAR/EOL GAUS radiosonde data set was converted into the EOL
Sounding Composite format for CuPIDO.  The format described in the
included PDF document is incorrect.  The correct format is described
within this addendum.


2.0  Detailed Data Description


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
       5   GMT Launch Time (y,m,d,h,m,s):    Time of release, in format:
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
header lines. Records for this dataset include the following three
non-standard header lines.

     Line  Label (padded to 35 char)              Contents

       6   Ascension No:                      1299
       7   Radiosonde Serial Number:          152551614
       8   Radiosonde Manufacturer:           Vaisala

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

     The following is a sample record of MGAUS Sounding Rawinsonde
upper air data in EOL ESC format.   The data portion is
much longer than 80 characters and, therefore, wraps around to a second
line.  See section 2.1 for an exact format specification

Data Type:                         NCAR GAUS/Ascending
Project ID:                        CuPIDO
Release Site Type/Site ID:         mgaus01_2006_07_24_straftoncanyon
Release Location (lon,lat,alt):    110 40.89'W, 32 30.35'N, -110.682, 32.506, 1388.9
UTC Release Time (y,m,d,h,m,s):    2006, 07, 24, 16:01:58
Post Processing Comments:          Aspen Version
Reference Launch Data Source/Time: Vaisala WXT510/16
Sonde Id/Sonde Type:               061354787/Vaisala RS92-SGP (ccGPS)
System Operator/Comments:          Bryan/none, Good Sounding
/
/
Nominal Release Time (y,m,d,h,m,s): 2006, 07, 24, 16:01:58
 Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat    Ele   Azi   Alt    Qp   Qt   Qrh  Qu   Qv   QdZ
  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg    deg   deg    m    code code code code code code
------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----
  -1.0  860.1  30.7   8.6  24.7   -1.6    1.9   2.5 141.0 999.0 -110.682  32.506 999.0 999.0  1388.9 99.0 99.0 99.0 99.0 99.0  9.0
   0.0  859.8  30.1   8.4  25.3   -0.8    2.0   2.1 158.5   4.1 -110.682  32.506 999.0 999.0  1392.0 99.0 99.0 99.0 99.0 99.0 99.0
   1.0  859.4  29.7   8.2  25.6   -1.1    2.0   2.3 151.1   4.3 -110.682  32.506 999.0 999.0  1396.1 99.0 99.0 99.0 99.0 99.0 99.0
   2.0  859.0  29.4   8.2  26.0   -1.4    2.0   2.4 144.4   4.7 -110.682  32.506 999.0 999.0  1400.7 99.0 99.0 99.0 99.0 99.0 99.0
   3.0  858.5  29.2   8.2  26.3   -1.7    1.9   2.6 138.4   5.0 -110.682  32.506 999.0 999.0  1405.6 99.0 99.0 99.0 99.0 99.0 99.0
