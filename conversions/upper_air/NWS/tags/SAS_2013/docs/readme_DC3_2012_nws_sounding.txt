National Weather Service (NWS) High Resolution Rawinsonde Data

1.0  General Description

This is one of the upper air data sets developed for the Deep Convective
Clouds and Chemistry Project (DC3) 2012 project. This data set 
includes 5520 high vertical resolution soundings from 53 1-second
National Weather Service (NWS) rawinsonde stations located throughout 
the United States for the period covering 10 May 2012 through 30 June 2012.
Soundings were typically released twice daily during this period. 
These data were converted into the EOL Sounding Composite (ESC) format 
(columnar ascii).

1.1  Data Set Contact

Steve Williams
NCAR/EOL
sfw@ucar.edu

2.0  Detailed Data Description

2.0.1 National Weather Service High-Resolution Sounding Algorithms

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

     The following is a sample record of DC3 Sounding Rawinsonde NWS
upper air data in EOL ESC format.   The data portion is
much longer than 80 characters and, therefore, may wrap around to a second
line.  See section 2.1 for an exact format specification

Data Type:                         National Weather Service Sounding/Ascending
Project ID:                        DC3
Release Site Type/Site ID:         KDNR Denver, CO / 72469
Release Location (lon,lat,alt):    104 52.17'W, 39 46.05'N, -104.870, 39.768, 1611.0
UTC Release Time (y,m,d,h,m,s):    2012, 05, 25, 23:02:00
Ascension Number:                  292
Radiosonde Serial Number:          85263871
Balloon Manufacturer/Type:         Totex / GP26
Balloon Lot Number/Weight:         260112 / 0.600
Radiosonde Type/RH Sensor Type:    Sippican Mark IIA with chip thermistor, pressure / Sippican Mark IIA Carbon Hygristor
Surface Observations:              P: 828.9, T: 12.0, RH: 30.0, WS: 4.6, WD: 169.0
Nominal Release Time (y,m,d,h,m,s):2012, 05, 26, 00:00:00
 Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat   Ele   Azi    Alt    Qp   Qt   Qrh  Qu   Qv   QdZ
  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     m    code code code code code code
------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----
   0.0  828.9  24.3   5.6  30.0    0.1    5.7   5.7 181.0 999.0 -104.869  39.767 999.0 999.0  1611.0  1.0  1.0  1.0  1.0  1.0  9.0
   1.0  828.5  24.1   5.5  30.0    0.0    5.8   5.8 180.0   5.0 -104.869  39.768 999.0 999.0  1616.0  1.0  3.0  3.0  1.0  1.0 99.0
   2.0  828.1  24.0   5.4  30.1   -0.1    5.9   5.9 179.0   4.0 -104.869  39.768 999.0 999.0  1620.0  1.0  3.0  3.0  1.0  1.0 99.0
   3.0  827.7  23.8   5.2  30.1   -0.2    6.1   6.1 178.1   4.0 -104.869  39.768 999.0 999.0  1624.0  1.0  3.0  3.0  1.0  1.0 99.0
   4.0  827.2  23.7   5.2  30.1   -0.3    6.2   6.2 177.2   5.0 -104.869  39.768 999.0 999.0  1629.0  1.0  3.0  3.0  1.0  1.0 99.0
   5.0  826.7  23.5   5.0  30.1   -0.4    6.3   6.3 176.4   5.0 -104.869  39.768 999.0 999.0  1634.0  1.0  3.0  3.0  1.0  1.0 99.0
   6.0  826.2  23.4   4.9  30.2   -0.5    6.5   6.5 175.6   6.0 -104.869  39.768 999.0 999.0  1640.0  1.0  3.0  3.0  1.0  1.0 99.0
   7.0  825.7  23.2   4.8  30.2   -0.6    6.6   6.6 174.8   5.0 -104.869  39.768 999.0 999.0  1645.0  1.0  3.0  3.0  1.0  1.0 99.0
   8.0  825.2  23.1   4.7  30.2   -0.7    6.7   6.7 174.0   5.0 -104.869  39.768 999.0 999.0  1650.0  1.0  3.0  3.0  1.0  1.0 99.0
   9.0  824.7  22.9   4.6  30.3   -0.8    6.9   6.9 173.4   6.0 -104.869  39.768 999.0 999.0  1656.0  1.0  3.0  3.0  1.0  1.0 99.0
  10.0  824.2  22.8   4.5  30.3   -0.9    7.0   7.1 172.7   6.0 -104.869  39.768 999.0 999.0  1662.0  1.0  3.0  3.0  1.0  1.0 99.0
:
:
:

2.2  Data Remarks  

None

2.3  Station List

All sites are in the United States.


ID   SITE, STATE                        Latitude  Longitude   ELEV (m) SONDE TYPE           QC
------------------------------------------------------------------------------------------------
KABQ Albuquerque, NM /72365             35.03809  -106.62280    1619.0 Sippican Mark IIA    *
KABR Aberdeen, SD /72659                45.45450   -98.41416     398.0 Sippican Mark IIA
KAMA Amarillo, TX /72363                35.23253  -101.70874    1095.0 Sippican Mark IIA    *
KAPX Gaylord, MI /72634                 44.90826   -84.71936     448.0 Sippican Mark IIA
KBIS Bismarck, ND /72764                46.77166  -100.76158     506.0 Sippican Mark IIA
KBMX Birmingham, AL /72230              33.18010   -86.78269     174.0 Sippican Mark IIA    *
KBOI Boise, ID /72681                   43.56775  -116.21092     873.0 Sippican Mark IIA
KBRO Brownsville, TX /72250             25.91620   -97.41993       7.0 Sippican Mark IIA
KBUF Buffalo, NY /72528                 42.94003   -78.72474     218.0 Sippican Mark IIA
KCHS Charleston, SC /72208              32.89473   -80.02776      13.0 Sippican Mark IIA
KCRP Corpus Christi, TX /72251          27.77933   -97.50495      15.0 Sippican Mark IIA
KDDC Dodge City, KS /72451              37.76164   -99.96936     790.0 Sippican Mark IIA    *
KDNR Denver, CO /72469                  39.76749  -104.86945    1611.0 Sippican Mark IIA    *
KDRT Del Rio, TX /72261                 29.37448  -100.91828     314.0 Sippican Mark IIA
KDTX Detroit/White Lake, MI /72632      42.69915   -83.47160     330.0 Sippican Mark IIA
KDVN Quad Cities, IA /74455             41.61238   -90.58209     230.0 Sippican Mark IIA
KEPZ Santa Teresa/El Paso, NM /72364    31.87268  -106.69709    1254.0 Sippican Mark IIA
KFFC Peachtree City, GA /72215          33.35611   -84.56734     245.0 Sippican Mark IIA    *
KFGZ Flagstaff, AZ /72376               35.23057  -111.82019    2179.0 Sippican Mark IIA    *
KFWD Fort Worth, TX /72249              32.83508   -97.29794     195.0 Sippican Mark IIA    *
KGGW Glasgow, MT /72768                 48.20600  -106.62659     693.0 Sippican Mark IIA
KGJT Grand Junction, CO /72476          39.11974  -108.52431    1474.0 Sippican Mark IIA    *
KGRB Green Bay, WI /72645               44.49755   -88.11171     209.0 Sippican Mark IIA
KGSO Greensboro, NC /72317              36.09813   -79.94300     276.0 Sippican Mark IIA
KILX Lincoln, IL /74560                 40.15120   -89.33763     179.0 Sippican Mark IIA
KINL Intl Falls, MN /72747              48.56467   -93.39739     357.0 Sippican Mark IIA
KJAN Jackson, MS /72235                 32.31999   -90.08031      91.0 Sippican Mark IIA    *
KJAX Jacksonville, FL /72206            30.48332   -81.70111      10.0 Sippican Mark IIA    *
KKEY Key West, FL /72201                24.55311   -81.78872      13.0 Sippican Mark IIA
KLBF North Platte, NE /72562            41.13395  -100.69991     849.0 Sippican Mark IIA    *
KLCH Lake Charles, LA /72240            30.12551   -93.21709       5.0 Sippican Mark IIA
KLIX Slidell, LA /72233                 30.33763   -89.82507      10.0 Sippican Mark IIA    *
KLKN Elko, NV /72582                    40.86018  -115.74146    1593.0 Sippican Mark IIA    *
KLZK Little Rock, AR /72340             34.83640   -92.25976     173.0 Sippican Mark IIA    *
KMAF Midland, TX /72265                 31.94267  -102.18986     874.0 Sippican Mark IIA
KMFL Miami, FL /72202                   25.75547   -80.38355       4.0 Sippican Mark IIA
KMPX Chanhassen/Minneapolis, MN /72649  44.84900   -93.56431     290.0 Sippican Mark IIA
KOAX Omaha/Valley, NE /72558            41.31950   -96.36633     351.0 Sippican Mark IIA
KOHX Old Hickory/Nashville, TN /72327   36.24694   -86.56178     180.0 Sippican Mark IIA    *
KOUN Norman, OK /72357                  35.18095   -97.43787     345.0 Sippican Mark IIA    *
KPBZ Pittsburgh, PA /72520              40.53156   -80.21748     360.0 Sippican Mark IIA
KRIW Riverton, WY /72672                43.06485  -108.47667    1699.0 Sippican Mark IIA    *
KRNK Blacksburg, VA /72318              37.20589   -80.41436     639.0 Sippican Mark IIA
KSGF Springfield, MO /72440             37.23583   -93.40216     391.0 Sippican Mark IIA    *
KSHV Shreveport, LA /72248              32.45176   -93.84169      85.0 Sippican Mark IIA    *
KSLC Salt Lake City, UT /72572          40.77244  -111.95470    1289.0 Sippican Mark IIA    *
KTAE Tallahassee, FL /72214             30.44630   -84.29963      53.0 Sippican Mark IIA    *
KTBW Tampa Bay, FL /72210               27.70529   -82.40127      13.0 Sippican Mark IIA    *
KTFX Great Falls, MT /72776             47.46056  -111.38533    1134.0 Sippican Mark IIA
KTOP Topeka, KS /72456                  39.07297   -95.62983     268.0 Sippican Mark IIA
KTWC Tucson, AZ /72274                  32.22794  -110.95601     741.0 Sippican Mark IIA
KUNR Rapid City, SD /72662              44.07301  -103.21027    1029.0 Sippican Mark IIA
KVEF Las Vegas, NV / 72388              36.04714  -115.18464     697.0 Sippican Mark IIA

The stations with the * in the QC column are in or within 100km of the three
DC3 subregions (Colorado, Oklahoma, and Alabama and underwent visual quality 
control in addition to the automated quality control that all soundings
included in this data set underwent.

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
checks on the DC3 NWS sounding dataset.  In the table P = pressure, T =
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
     
     The sounding stations noted in section 2.3 were visually examined utilizing 
the EOL XQC tool which allows data quality flags to be adjusted.

3.4  Data Quality Comments

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
