TITLE: Great Basin Unified Air Pollution Control Division (GBUAPCD) automatic weather station data
AUTHOR: Mr. Jim Parker, GBUAPCD, 157 Short Street, Bishop, CA 93514-3537, e-mail: gb1@greatbasinapcd.org, Phone (760)872-1285 x236
Dave Whiteman requested this automatic weather station data for the T-REX program. The data were supplied by Jim Parker. Jim should be contacted with any questions.
DATA SET OVERVIEW: The data are historical weather data from automatic weather stations operated by GBUAPCD. Many of the original sites have been abandonned over time. The original files of historical data were requested from Mr. Parker and received before the T-REX Special Observing Period. Stations that were still in operation during the T-REX SOP have two files - one of historical data ending before the T-REX SOP, and one of more current data received after the end of 2006 (and including the Mar/April 2006 T-REX field period). There are different periods of record for each of the sites and the documentation of the data appears in header records in the individual files. The data source is GBUAPCD. It is my impression that some of the historical data came from an earlier agency that merged into the GBUAPCD. Some of the earliest data came from automatic weather stations where the data were stored on audio cassette tapes. There is quite a bit of missing data from the early periods, and there are some data quality problems here and there. Missing data is indicated in the data sets by 999.
INSTRUMENT DESCRIPTION: Information is unavailable on the specific instruments and their specifications at the individual sites.
DATA COLLECTION AND PROCESSING: Unavailable.
DATA FORMAT: The table below indicates the names of all the ascii files and contains the header information as well as the first and last lines of data in each of the files. The order of variables and the data units are indicated in the header records. An e-mail from Jim Parker indicates that the data are hourly and in Pacific Standard Time. The time indicated is the ending time of the one-hour averaging period. The one-hour average is formed from 1800 two-second samples. According to Jim, the elevations (with different degrees of accuracy) are (in ft above MSL)
Lee Vining-6780
Mommoth Lakes-7850
Mono Lake(Simis)-6378
Leeler-3617
Lone Pine-3481
DATA REMARKS: Altitude information is missing for many of the sites. These could be estimated from the latitude/longitude information. The GBUAPCD has maps on their website, www.gbuapcd.org, that show some of the locations.
An e-mail from Jim Parker on Nov 30, 2005 discusses data quality issues, as follows:
"5) Documentation of causes of lost data is somewhat sketchy, especially as
age of data increases.
In most cases, hours of missing data are filled with "999" to indicate no
data available for that parameter for that hour.
Other gaps in the record are assumed to be dates/hours of non-collection or
unacceptable data quality.
You ask for information about instrument failures, etc.  Are you asking
about short-duration data losses (e.g., anemometer
replacements) or long-duration losses (e.g., site removed for building
remodel)?
6) I have some information on missing data, but not in any form similar to a
compiled log.  Changes in site locations, when more
than minor moves such as from one side of a lot to the other, have resulted
in changes in site names/numbers, so none of our
data sets is a mixture of data from more than one location.
7) You state that you would like a file listing the basic metadata for all
GBUAPCD sites.  I would like to compile that one of
these days, but it remains a work in progress.  Most of our meteorological
data are from Owens Lake and vicinity.  In years past we
collected met data in Bishop, Independence, Big Pine and a couple of other
locations.  Most of these sites had only a few years of data
collection and were terminated by the early 1990s.  We have longer records
from the area of the Coso Geothermal Area to the south
of Owens Lake (30 miles south?) east of the southern end of the Sierra
Nevada.  Would these be of interest to you?"
A further e-mail on 17 Feb 2006, points out a change in wind speed units which may be otherwise undocumented, as follows:
"I have just posted four new files to your ftp\incoming folder.
BishAir.txt from Bishop Airport
BishDown.txt from Downtown Bishop
Indepmet.txt from Independence
BigPine.txt from Steward's Ranch just E of Big Pine
As you will see, all are older records, for only a few years each.
PLEASE NOTE:  All of the wind speeds in these files are in MILES PER HOUR, whereas the other files I sent you were in meters/sec. Let me know if you have any questions about these. Jim"
REFERENCES: None
-----------------------------------------------------------------
LOPIMET.TXT - Data from GBUAPCD Lone Pine Monitoring Site
Latitude:  36.6073	Longitude:  -118.0479
Fields:
Date, Hour, 10-m Avg wind Spd (m/s), 10-m Avg Wind Dir (deg), 10-m Avg Temp (deg C)
5/14/1986,1700,5.588,5,27
12/16/2004,1000,3.053,4.315,7.93

LONEPINE.TXT - Data from GBUAPCD Lone Pine Monitoring Site
Latitude:  36.6073	Longitude:  -118.0479
Fields:
Date, Hour, 10-m Avg wind Spd (m/s), 10-m Avg Wind Dir (deg), 10-m Avg Temp (deg C)
12/16/2004,1100,4.082,4.156,9.21
1/19/2007,1000,6.008,351.8,5.306
----------
MAMMET.TXT - Data from GBUAPCD Mammoth Lakes Monitoring Site
Latitude:  37.64806	Longitude:  -118.9733
Fields:
Date, Hour, 10-m Avg wind Spd (m/s), 10-m Avg Wind Dir (deg), 10-m Avg Temp (deg C)
5/09/1984,1300,2.45872,180,999
12/08/2004,1800,4.056,238.4,1.87

MAMMOTH.TXT - Data from GBUAPCD Mammoth Lakes Monitoring Site
Latitude:  37.64806	Longitude:  -118.9733
Fields:
Date, Hour, 10-m Avg wind Spd (m/s), 10-m Avg Wind Dir (deg), 10-m Avg Temp (deg C)
12/08/2004,1900,4.293,239.2,1.722
1/23/2007,1600,2.843,330.3,10.62
----------
LEEMET.TXT - Data from GBUAPCD Lee Vining Monitoring Site
Latitude:  37.95417	Longitude:  -119.1144
Fields:
Date, Hour, 10-m Avg wind Spd (m/s), 10-m Avg Wind Dir (deg), 10-m Avg Temp (deg C), Barometric Pressure (inches Hg)
12/01/1985,100,999,999,999,999
12/16/2004,1700,.837,194,1.572,30.43

LEVINING.TXT - Data from GBUAPCD Lee Vining Monitoring Site
Latitude:  37.95417	Longitude:  -119.1144
Fields:
Date, Hour, 10-m Avg wind Spd (m/s), 10-m Avg Wind Dir (deg), 10-m Avg Temp (deg C), Barometric Pressure (inches Hg)
12/16/2004,1800,2.103,192.5,.471,30.44
1/23/2007,1400,999,999,4.842,30.31
---------
KELRMET.TXT - Data from GBUAPCD Keeler Monitoring Site
Latitude:  36.4916	Longitude:  -117.8779
Fields:
Date, Hour, 10-m Avg wind Spd (m/s), 10-m Avg Wind Dir (deg), 10-m Avg Temp (deg C), Total Hourly Precipitation (inches)
3/14/1985,100,999,999,999,999
2/03/2005,1200,9.35,12.1,14.82,0

KEELER.TXT - Data from GBUAPCD Keeler Monitoring Site
Latitude:  36.4916	Longitude:  -117.8779
Fields:
Date, Hour, 10-m Avg wind Spd (m/s), 10-m Avg Wind Dir (deg), 10-m Avg Temp (deg C), Total Hourly Precipitation (inches)
2/03/2005,1300,11.63,19.8,14.85,0
1/19/2007,1200,3.899,160.2,6.814,0
---------
MONOTXT.TXT - Data from GBUAPCD Mono Lake (Simis Residence) Monitoring Site
Latitude:  38.09145	Longitude:  -118.9979
Fields:
Date, Hour, 10-m Avg wind Spd (m/s), 10-m Avg Wind Dir (deg), 10-m Avg Temp (deg C), Total Hourly Precip (inches)
7/13/1982,1300,3.57632,155,999,999
12/16/2004,1500,1.853,229.4,3.763,0

MONOLAKE.TXT - Data from GBUAPCD Mono Lake (Simis Residence) Monitoring Site
Latitude:  38.09145	Longitude:  -118.9979
Fields:
Date, Hour, 10-m Avg wind Spd (m/s), 10-m Avg Wind Dir (deg), 10-m Avg Temp (deg C), Total Hourly Precip (inches)
12/16/2004,1600,999,999,999,999
12/31/2006,2400,2.036,331.9,-4.011,0
---------
OLANCHA1.TXT - Data from GBUAPCD Olancha1 Monitoring Site
Latitude:  36.28	Longitude:  -118.00	Elevation: 3650'
Fields:
Site, Date, Hour, 10-m Avg wind Spd (m/s), 10-m Avg Wind Dir (deg), 10-m Avg Temp (deg C), 10-m Avg Barometric Pressure (inches Hg)
10710,11/22/1985,100,999,999,999,999
10710,5/20/1993,900,3.668,9.13,20.71,29.832
----------
OLANCHA2.TXT - Data from GBUAPCD Olancha2 Monitoring Site
Latitude:  36.28	Longitude:  -117.49	Elevation: 3694'
Fields:
Site, Date, Hour, 10-m Avg wind Spd (m/s), 10-m Avg Wind Dir (deg), 10-m Avg Temp (deg C), 10-m Avg Barometric Pressure (inches Hg)
725,3/16/1993,1400,6.678587393831,170.8,20.01,999
725,6/30/1995,2400,999,999,999,999
--------
OLANCHA3.TXT - Data from GBUAPCD Olancha3 Monitoring Site
Latitude:  36.2675	Longitude:  -117.9930	Elevation: 3682'
Fields:
Site, Date, Hour, 10-m Avg wind Spd (m/s), 10-m Avg Wind Dir (deg), 10-m Avg Temp (deg C), 10-m Avg Relative Humidity (pct), 10-m Avg Barometric Pressure (inches Hg)
729,7/01/1995,100,999,999,999,999,999
10729,11/01/2005,1300,7.99,179.6,21.79,17.03,30.05

OLANCHA3contd.TXT - Data from GBUAPCD Olancha3 Monitoring Site
Latitude:  36.2675	Longitude:  -117.9930	Elevation: 3682'
Fields:
Date, Hour, 10-m Avg wind Spd (m/s), 10-m Avg Wind Dir (deg), 10-m Avg Temp (deg C), 10-m Avg Relative Humidity (pct), 10-m Avg Barometric Pressure (millibars)
11/01/2005,1400,8.55,176.8,22.12,16.49,887.8514899955
1/03/2007,1500,1.508,13.5,14.02,15.8,888
----------
INDEPMET.TXT - Data from GBUAPCD Independence Monitoring Site
Latitude:  36.8111        Longitude:  -118.1819     Elevation: 1193.3 meters
Fields:
Site, Date, Hour, 10-m Avg wind Spd (mph), 10-m Avg Wind Dir (deg), 10-m Avg Temp (deg C)
10700,7/01/1988,100,4.946,117.2,24.96
10700,8/01/1991,1200,5.053,124.1,32.91
----------
BISHDOWN.TXT - Data from GBUAPCD Downtown Bishop Monitoring Site
Latitude:  37.3611      Longitude:  -118.3936   Elevation: 1256.4 meters
Fields:
Site, Date, Hour, 10-m Avg wind Spd (mph), 10-m Avg Wind Dir (deg), 10-m Avg Temp (deg C), 10-m Avg Relative Humidity (pct), 10-m Avg Barometric Pressure (inches Hg)
10723,3/15/1990,1800,9.8,348.1,19.06,14.23
10723,4/12/1995,2400,999,999,999,999
--------
BISHAIR.TXT - Data from GBUAPCD Bishop Airport Monitoring Site
Latitude:  37.3672        Longitude:  -118.3528     Elevation: 1254 meters
Fields:
Site, Date, Hour, 10-m Avg wind Spd (mph), 10-m Avg Wind Dir (deg), 10-m Avg Temp (deg C)
10712,1/01/1988,100,999,999,999
10712,8/23/1991,1200,15.04,178.9,32.98
---------
BIGPINE.TXT - Data from GBUAPCD Steward's Ranch Monitoring Site (just East of Big Pine)
Latitude:  37.1764        Longitude:  -118.2331     Elevation: 1234.38 meters
Fields:
Site, Date, Hour, 10-m Avg wind Spd (mph), 10-m Avg Wind Dir (deg), 10-m Avg Temp (deg C)
10709,8/23/1988,1200,11.35,181.9,33.72
10709,5/11/1990,800,2.485,269.8,16.36
