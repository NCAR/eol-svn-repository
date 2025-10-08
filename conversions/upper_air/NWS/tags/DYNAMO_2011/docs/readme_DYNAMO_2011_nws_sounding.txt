PRELIMARY National Weather Service (NWS) High Resolution Rawinsonde Data

1.0  General Description

This is a PRELIMINARY dataset.

This is one of the upper air data sets developed for the Dynamics of the
Madden-Julian Oscillation (DYNAMO) 2011-2012 project. This preliminary data set 
includes 668 high vertical resolution soundings from the National Weather Service
(NWS) rawinsonde stations at Guam (62 soundings; 6-second resolution) in the
Mariana Islands and Yap (267 soundings; 1-second resolution) and Koror 
(339 soundings; 1-second resolution) both in the Western Caroline Islands.  
These data were converted into the EOL Sounding Composite format (columnar
ascii).

Soundings were typically released twice daily during this period. However, Yap was
once daily until 16 November 2011 (and had no soundings from 30 October to 7
November 2011 and from 19-28 March 2012) and Koror had outage problems from 
14-22 December.  

This preliminary version of the data set contains only data from 1 October 
to 31 March 2012 for Yap and Koror and only 1-31 October 2011 data for Guam.  
Eventually, this data set will include data through March 2012 for all
stations.  Additionally, there may be corrections applied to these data at a later 
point.  If you order this data set now you will be notified when additional and/or 
corrected data becomes available.

This is a PRELIMINARY dataset.


1.1  Data Set Contact

Steve Williams
NCAR/EOL
sfw@ucar.edu

2.0  Detailed Data Description

2.0.1 National Weather Service High-Resolution Sounding Algorithms

The detailed description of NWS MicroArt sounding collection and
instrumentation is located in NWS (1991).  

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
header lines. Records for the MicroArt data at Guam include the 
following three non-standard header lines.

     Line  Label (padded to 35 char)              Contents

       6   Ascension No:                      1531
       7   Radiosonde Serial Number:          87047718.CSN
       8   Radiosonde Manufacturer:           VIZ B2

Records for the RRS data at Koror and Yap include the following non-standard
header lines:

     Line  Label (padded to 35 char)              Contents
       
       6   Ascension Number:                  422
       7   Radiosonde Serial Number:          85221265
       8   Balloon Manufacturer/Type:         Other / GP26
       9   Balloon Lot Number/Weight:         1012011 / 0.700
      10   Radiosonde Type/RH Sensor Type:    Sippican Mark IIA with chip thermistor, pressure / Sippican Mark IIA Carbon Hygristor
      11   Surface Observations:              P: 1008.1, T: 999.0, RH: 94.0, WS: 3.6, WD: 193.0

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

     The following is a sample record of DYNAMO Sounding Rawinsonde NWS
upper air data in EOL ESC format.   The data portion is
much longer than 80 characters and, therefore, may wrap around to a second
line.  See section 2.1 for an exact format specification


Data Type:                         National Weather Service Sounding/Ascending
Project ID:                        DYNAMO
Release Site Type/Site ID:         PTYA Yap, WCI / 91413
Release Location (lon,lat,alt):    138 04.90'E, 09 29.83'N, 138.082, 9.497, 27.0
UTC Release Time (y,m,d,h,m,s):    2011, 11, 08, 23:14:44
Ascension Number:                  452
Radiosonde Serial Number:          85216911
Balloon Manufacturer/Type:         Other / GP26
Balloon Lot Number/Weight:         1012011 / 0.700
Radiosonde Type/RH Sensor Type:    Sippican Mark IIA with chip thermistor, pressure / Sippican Mark IIA Carbon Hygristor
Surface Observations:              P: 1009.1, T: 27.0, RH: 88.0, WS: 0.0, WD: 360.0
Nominal Release Time (y,m,d,h,m,s):2011, 11, 09, 00:00:00
 Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat   Ele   Azi    Alt    Qp   Qt   Qrh  Qu   Qv   QdZ
  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     m    code code code code code code
------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----
   0.0 1008.4  31.8  27.7  79.0   -1.6    1.3   2.1 129.1 999.0  138.082   9.497 999.0 999.0    27.0  1.0  1.0  1.0  1.0  1.0  9.0
   1.0 1006.9  31.6  27.1  77.2   -1.4    0.9   1.7 122.7  13.0  138.082   9.497 999.0 999.0    40.0  1.0  3.0  1.0  1.0  1.0 99.0
   2.0 1006.1  31.4  27.0  77.3   -1.5    0.9   1.7 121.0   7.0  138.081   9.497 999.0 999.0    47.0  1.0  3.0  1.0  1.0  1.0 99.0
   3.0 1005.4  31.2  26.9  77.8   -1.6    1.0   1.9 122.0   7.0  138.081   9.497 999.0 999.0    54.0  1.0  3.0  1.0  1.0  1.0 99.0
   4.0 1004.7  31.0  26.5  77.1   -1.7    1.1   2.0 122.9   6.0  138.081   9.497 999.0 999.0    60.0  1.0  3.0  1.0  1.0  1.0 99.0
   5.0 1004.0  30.8  26.1  76.1   -1.8    1.1   2.1 121.4   7.0  138.081   9.497 999.0 999.0    67.0  1.0  3.0  1.0  1.0  1.0 99.0
:
:
:


2.2  Data Remarks

     For the Guam MicroArt station data the use of the raw 6-sec resolution elevation 
and azimuth angle data to derive the winds sometimes led to large oscillations 
in wind speed, due to the presence of oscillations in the elevation angle data,
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
PTY  Yap                      WCI     138.1     9.5    27.0    Sippican Mark IIA
PTR  Koror, Palau             WCI     134.4     7.3    34.0    Sippican Mark IIA
pgac Guam, Marianna Island            144.8    13.5    75.0    VIZ B2

Yap and Koror utilize GPS windfinding.

Guam utilizes radio theolodite windfinding.

We have utilized the processed PTU and GPS data from the RRS sounding systems
to generate these files.  The raw position, temperature and RH data are 
normalized by linear interpolation into 1 second processed data.  The raw 
pressure data are normalized by least square interpolation into 1 second processed 
data.  The pressure data are smoothed over 11 seconds of corrected pressure and
the result is applied to the 6th corrected pressure within the 11 second
spread.  The temperature data are smoothed over 9 seconds of uncorrected
temperature and the result is applied to the 5th uncorrected temperature
within the 9 second spread.  There must be at least 2 good raw temperature
elements with the 9 second spread.

The following corrections were applied by the RRS sounding system.

Pressure correction - pressure correction is used to compensate for offsets of
the radiosonde pressure sensor as compared to the station's pressure sensor.
The pressure offset is determined during the radiosonde baseline operations.
The correction is applied to the uncorrected pressure prior to pressure
smoothing.

This correction is defined as:

Pc = Pu * (Pstn/Psonde)
where Pc is the corrected pressure
      Pu is the uncorrected pressure
      Pstn is the station pressure
      Psonde is the radiosonde surface pressure 

Temperature correction - temperature correction is used to compensate for
solar radiation.  The correction is applied to the smoothed temperature.
These corrections are proprietary to Sippican. 


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
checks on the DYNAMO NWS sounding dataset.  In the table P = pressure, T =
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
which allows data quality flags to be adjusted.

3.4  Data Quality Comments

Koror
-----
No Koror soundings were taken from  12 UTC 15 October through 00 UTC 22
October.

Yap
---
No Yap soundings were taken from 30 October to 7 November due to a tracking
antenna failure.

Yap sounding were once per day (at 00 UTC) from 1-29 October and 8-15 November
due to a failure of their hydrogen gas generator.

No Yap soundings were taken from 00 UTC 19 March to 00 UTC 28 March.

The Yap sounding at 20120208230739 from ~115-40mb the pressure data are
highly questionable.

Guam
----
None.


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
