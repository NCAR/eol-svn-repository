National Weather Service (NWS) High Resolution Rawinsonde Data

1.0  General Description

This is one of the upper air data sets developed for the Impact of Typhoons
on the Ocean in the Pacific (ITOP) 2010.  This data set includes 976 high 
vertical resolution (6-second) MicroArt soundings from the six (6)
National Weather Service (NWS) rawinsonde stations in the ITOP 2010 region
(Yap, WCI; Ponape, ECI; Koror, Palau WCI; Chuuk, ECI; Majuro, Marshall Island; 
Guam, Marianna Island). The data cover the period from 1 August 2010 to 
31 October 2010.  The soundings were typically released twice a day 
(0000 and 1200 UTC).  This data set currently does not contain soundings
from Majuro for the month of September nor Yap for the month of October.
The data are in EOL Sounding Composite format (columnar ascii).


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
header lines. Records for the MicroArt data at Topeka, KS include the 
following three non-standard header lines.

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

     The following is a sample record of ITOP 2010 Sounding Rawinsonde NWS
upper air data in EOL ESC format.   The data portion is
much longer than 80 characters and, therefore, may wrap around to a second
line.  See section 2.1 for an exact format specification

Data Type:                         National Weather Service Sounding.
Project ID:                        ITOP_2010
Release Site Type/Site ID:         PTP Ponape, ECI
Release Location (lon,lat,alt):    158 12.00'E, 07 00.00'N, 158.200, 7.000,
39.0
UTC Release Time (y,m,d,h,m,s):    2010, 09, 15, 23:13:00
Ascension No:                      1474
Radiosonde Serial Number:          87040691.CSN
Radiosonde Manufacturer:           VIZ B2
/
/
/
Nominal Release Time (y,m,d,h,m,s):2010, 09, 16, 00:00:00
 Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon
Lat   Ele   Azi    Alt    Qp   Qt   Qrh  Qu   Qv   QdZ
  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg
deg   deg   deg     m    code code code code code code
------ ------ ----- ----- ----- ------ ------ ----- ----- ----- --------
------- ----- ----- ------- ---- ---- ---- ---- ---- ----
   0.0 1008.8  23.9  23.1  95.0   -1.2    1.0   1.6 129.8 999.0  158.200
7.000  52.7 130.3    39.0  1.0  1.0  1.0  1.0  1.0  9.0
   6.0 1003.9  24.0  22.4  90.7   -1.5    1.8   2.3 140.2   7.2  158.200
7.000  52.7 130.3    82.0  1.0  1.0  1.0  4.0  4.0 99.0
  12.0  999.7  23.8  21.9  89.2   -1.8    2.5   3.1 144.2   6.2  158.200
7.001  52.4 141.8   119.0  1.0  1.0  1.0  4.0  4.0 99.0
  18.0  994.9  24.0  21.6  86.6   -2.1    3.3   3.9 147.5   7.0  158.200
7.001  50.6 152.7   161.0  1.0  1.0  1.0  4.0  4.0 99.0
  24.0  991.2  24.0  21.6  86.3   -2.4    4.0   4.7 149.0   5.5  158.200
7.002  47.6 162.3   194.0  1.0  1.0  1.0  4.0  4.0 99.0
  30.0  987.4  23.8  21.4  86.2   -2.6    4.6   5.3 150.5   5.5  158.200
7.002  43.5 170.5   227.0  1.0  1.0  1.0  4.0  4.0 99.0


2.2  Data Remarks

     For the MicroArt station data the use of the raw 6-sec resolution elevation 
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
PTY  Yap                      WCI     138.1     9.5    22.0    Vaisala
PTP  Ponape                   ECI     158.2     7.0    39.0    VIZ B2
PTR  Koror, Palau             WCI     134.5     7.3    30.0    VIZ B2
PTK  Chuuk  (Truk)            ECI     151.8     7.4     3.0    VIZ B2
PMK  Majuro, Marshall Island          171.3     7.1     4.0    VIZ B2
pgac Guam, Marianna Island            144.8    13.5    75.0    VIZ B2


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
which allows data quality flags to be adjusted.

3.4  Data Quality Comments

Majuro
------
PMK_201008292306 - bad winds from 350-180mb
PMK_201010011108 - bad winds from 175-80mb
PMK_201010042314 - no data above 945mb, little wind.
PMK_201010052312 - no wind above 265mb
PMK_201010061106 - no data above 170mb
PMK_201010062312 - no data above 180mb; no wind above 520mb
PMK_201010071119 - no winds
PMK_201010072306 - no winds; no data above 180mb
PMK_201010081224 - no winds; no data above 180mb
PMK_201010082304 - no winds; no data above 245mb
PMK_201010091112 - bad winds; no data above 230mb
PMK_201010092312 - no winds; no data above 125mb
PMK_201010101105 - no winds; no data above 150mb
PMK_201010102308 - no winds
PMK_201010111132 - no winds; no data above 105mb
PMK_201010112303 - bad winds
PMK_201010121112 - no winds; no data above 215mb
PMK_201010122317 - no winds; no data above 180mb
PMK_201010131109 - no winds
PMK_201010132306 - no winds
PMK_201010141112 - no winds
PMK_201010142313 - no winds
PMK_201010161103 - no data above 200mb
PMK_201010171108 - no winds; no data above 290mb
PMK_201010212333 - winds somewhat noisy

Chuuk
-----
PTK_201008051126 - no data above 185mb; bad T/DP 590-295mb
PTK_201008192304 - bad winds above 170mb
PTK_201008281117 - no data above 720mb; T/RH bad above 790mb
PTK_201009151103 - no data above 475mb
PTK_201009262350 - T/RH bad above 420mb
PTK_201010011114 - no data above 515mb
PTK_201010132308 - no data above 250mb
PTK_201010141111 - no data above 203mb; bad winds
PTK_201010221104 - winds mostly bad

Ponape
------
PTP_201008292300 - No winds
PTP_201009112301 - No data above 920mb
PTP_201010091102 - No winds
PTP_201010221101 - No data above 850mb

Koror
-----
PTR_201008011104 - Bad winds above 635mb
PTR_201008061104 - No data above 640mb
PTR_201008092302 - Bad winds from 690-320mb
PTR_201009102303 - Bad winds from 860-570mb
PTR_201009281104 - No data above 910mb
PTR_201010041102 - No data above 120mb
PTR_201010091102 - No data above 420mb
PTR_201010121101 - Bad winds 820-620mb
PTR_201010132304 - No data above 960mb
PTR_201010212302 - No data above 140mb

Yap
---
PTY_201008062327 - No data above 295mb
PTY_201008082306 - No data above 600mb
PTY_201008132304 - Bad winds
PTY_201008141105 - Bad winds
PTY_201008201104 - Bad winds
PTY_201008222307 - Bad winds
PTY_201009082304 - No data above 350mb
PTY_201009121104 - No data above 550mb
PTY_201009282310 - No data above 135mb

Guam
----
pgac_201008152309 - No data above 160mb
pgac_201008211103 - No data above 170mb
pgac_201008312307 - Winds bad above 180mb
pgac_201009032309 - No data above 250mb
pgac_201009041104 - No data above 210mb; no good winds above 430mb
pgac_201009042326 - No data above 150mb
pgac_201009091109 - No data above 200mb
pgac_201009101107 - No data above 135mb
pgac_201009111103 - No data above 140mb
pgac_201009151112 - No data above 285mb
pgac_201009251103 - Bad winds below 420mb
pgac_201010042302 - Bad winds 620-435mb; No data above 325mb
pgac_201010211103 - Bad winds 620-330mb
pgac_201010212302 - No data above 435mb
pgac_201010282302 - No data above 200mb

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
