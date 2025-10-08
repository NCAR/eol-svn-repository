README Terrain Induced Rotor Experiment (T-REX) from ASU

This archive contains data taken during the TREX field campaign by the Arizona
State University Environmental Fluid Dynamics Group. The T-REX campaign took
place in Independence, CA from March 1 to May 1,2006.

During the T-REX field campaign, the ASU team deployed a variety of instruments
in three separate locations

****************
* INSTRUMENTS: *
****************

 - Coherent Technologies Doppler LIDAR - LIDAR data archive contains 36 GB of
base data.  All data was sampled at 500Hz using 500 sample averaging.

 - one Scintec SODAR/RASS profiler

 - three 3D sonic anemometers/thermometers (R.M. Young 81000 and Campbell Sci. CSAT3)

 - four thermistors for measuring soil temperature at different depths

 - fine wire (Campbell Sci. FW05)

 - one Infra Red thermometer for measuring surface ("skin")
temperature (Everest Interscience Inc. 4000.4 ZL)

 - soil heat flux plate (Hukseflux, HFP01SC)

 - Krypton Hydrometer (Campbell Sci. KH20)

 - net-radiometer (Kipp & Zonen CNR1, transmission range from 0.3 to 3
micrometers for the short wave and from 5 to 50 micrometers for the long wave)

 - one barometric pressure gauge


**********************************
* LOCATIONS & OPERATION REGIMES: *
**********************************

The ASU lidar was situated on Mazourka Canyon Rd, approximately 1.5km East of 
highway 395 on a slope of 2.8deg. The lidar was continuously operational with 
the exception of March 4 and March 22 through 23 due to repairs. LIDAR coordinates 
were: 
N 36.79753deg, W 118.17578, 1179m +-7m ASL.

The ASU Flux tower was located directly adjacent to the ASU Lidar with coordinates:
N 36.79827deg, W 118.17578, 1172m +-7m ASL.  

The following instruments were outfitted on the tower:
  (3) 3D ultra-sonic anemometers:
	Two R.M. Young at 11.4m AGL and 7.3m AGL, and CampbellSci at 1.9m AGL. 
  (1) IR sensor mounted on tower at 1.2m AGL to measure surface temperature
  (1) Kipp and Zonen Net Radiometer at 12.4m AGL
  (1) Krypton Hygrometer at 1.9m AGL (coupled with CS Sonic)
  (1) Finewire thermocouple at 1.9m AGL (coupled with CS Sonic)  
  (1) Relative Humidity sensor at 9.8m AGL
  (1) Barometric Pressure gauge at 1m AGL
  (1) Soil heat flux plate at 10cm below ground level
  (4) thermistors at 4.5cm, 5cm, 7cm, and 9.5cm below ground level

The soil heat flux plate, the 4 soil thermistors, and the IR sensor acquired data using a Campbell Sci
DataLogger CR23x at a rate of 1Hz and the remainder of the instruments acquired data using
a Campbell Sci DataLogger CR5000 at a rate of 10Hz. 

Raw data of three velocity components, temperature, wind magnitude, and wind direction was 
stored from the RM Young Sonic at 11.4m AGL.

Raw data of three velocity components and temperature was stored from the RM Young 
sonic at 7.3m AGL.  

Raw data of three velocity components and temperature was stored from the CS Sonic at 1.9m AGL.

Raw data for each of the four radiation components (incoming shortwave, incoming longwave, 
outgoing shortwave, and outgoing longwave) was saved.

Raw data from the relative humidity sensor, pressure gauge, krypton hygrometer, finewire, 
and the latitude and longitude was stored.  

Raw data from each of the four thermistors (T1,T2,T3,T4) and the IR sensor were stored, and
soil heat flux (W/m2), voltage potential 1 and voltage potential 2 was stored from the soil 
heat flux plate.

All data were stored with accompanying timestamp and errors were given as -1.#QNAN in the sonic files.

Data acquisition began on 02/28/06 at 14:22:18.7 and ended on 05/01/06 at 17:08:55.5 for instruments
running on the CS CR5000 and began on Julian Day 57 at 15:25:49.1 and ended on Julian Day 121 at 
17:09:56.1 for instruments running on the CS CR23x.
All instruments ran continuously for the entire period except for instances of intermittent power failure
or mandatory downtime due to data transmission.  Those periods are listed below, partitioned into
two regimes:  one for the CR23x datalogger, and one for the CR5000 datalogger.
******************************************************************************************************


*******************CR23x Datalogger Intermittent Missing Data Points********************

  (in format  "JulianDay, MMDDYY, HHMM,SS.S-HHMM,SS.S, where first daily timestamp is 0001,0.1 and
	the last daily timestamp is 2400,59.1)

68, 030906, 0747,41.1-2400,59.1
69, 031006,
	Heat flux plate:  0001,0.1-0339,0.1
			  2007,21.1-2400,59.1
	Thermistors:      0001,0.1-0111.6.1
			  2007,21.1-2400,59.1
70, 031106, Entire Day
71, 031206, 0001,0.1-0918,54.1
	    2335,39.1-2339,52.1
99, 040906, 2234,6.1-2238,24.1
105, 041506, 0501,49.1-2400,59.1
106, 041606, 0001,0.1-0101,12.1
118, 042806, 0147,4.1-0900,59.1
****************************************************************************************



****************CR5000 DataLogger Intermittent Missing Data Points********************

  (in format "MMDDYY, HHMM,SS.S-HHMM,SS.S, where first daily timestamp is 0000,00.0 and the last daily timestamp is
	2359,59.9)

022806, 1426,14.5-1551,48.1
	1553,02.1-2352,00.1
030106, 2307,26.7-2331,58.3
030206, 2013,58.8-2020,56.8
030506, 0542,38.2-0550,16.7
	2311,07.3-2321,31.3
030706, 0047,13.1-0101,42.1
	2239,59.9-2248,54.8
030906, 0349,33.6-0401,10.0
031006, 0012,45.0-0045,15.9
	2002,22,8-2014,04.2
031206, 2343,29.3-2359,59.9
031306, 0000,00.0-0006,50.2
	2347,07.0-2357,07.2
031406, 1807,29.1-1819,37.5
031506, 1858,57.4-1942,45.4
031706, 0032,39.7-0043,07.5
031806, 0142,38.6-0215,57.2
	2323,21.2-2333,16.2
031906, 2058,35.4-2104,16.4
032006, 1833,58.6-1846,29.3
032106, 1914,31.3-1932,45.3
032206, 1938,38.7-1939,03.0
	1944,15.9-2226,27.6
032306, 2148,31.2-2155,00.4
032406, 2030,30.2-2030,54.0
	2037,19.5-2037,42.9
	2037,47.6-2038,10.5
	2038,15.0-2038,38.6
	2053,04.5-2053,28.1
	2053,28.9-2053,52.3
	2100,11.2-2100,34.9
	2100,59.5-0119,41.0
032506, 2029,29.3-2035,15.5
	2035,19.8-2359,59.9
032606, 0000,00.0-2245,07.7
	2248,48.0-2249,05.7
	2250,44.7-2359,59.9
032706, Entire Day
032806, 0000,00.0-0149,33.9
	2316,00.5-2327,34.5
033006, 0101,52.3-0111,55.5
	1953,40.0-2028,36.4
033106, 2137,04.0-2201,29.0
040106, 1905,06.8-1905,30.9
	1905,33.6-1905,56.6
	1905,58.9-1906,22.6
	1910,04.9-1910,28.5
	1910,30.0-1910,53.7
	1910,57.7-1911,20.6
	1915,09.0-1915,51.6
	1915,54.4-1916,17.6
	1918,59.9-1919,43.1
	1919,44.1-1920,07.6
	1920,08.6-1920,31.4
	1923,37.8-1924,20.6
	1924,26.4-1924,49.6
	1930,02.9-1930,26.6
	1930,31.0-1931,14.6
	1936,56.0-1937,19.6
	1937,20.9-1938,49.9
	1940,48.2-1941,36.3
	1941,42.8-1942,05.6
	1944,18.9-1945,27.9
	1945,32.1-1945,55.5
	1946,01.0-1946,24.6
	1948,13.0-1948,36.6
	1948,39.4-1949,51.6
	1949,54.0-2359,59.9
040206, Entire Day
040506, 0042,14.3-0050,22.4
040606, 0049,36.3-0054,48.4
	1838,15.3-1838,38.9
	1838,44.6-1839,27.5
	1842,30.0-1842,53.6
	1843,03.9-1843,26.6
	1847,17.9-1847,41.5
	1848,11.8-1848,36.6
	1850,18.9-1851,06.3
	1851,12,2-1851,35.9
	1853,34.2-1854,22.6
	1854,29.4-1855,16.6
	1856,33.9-1857,45.3
	1900,33.8-1900,56.5
	1901,04.0-1901,27.9
	1905,04.2-1905,51.0
	1908,09.0-1908,56.5
	1909,02.3-1909,25.6
	1911,26.0-1913,13.5
	1914,43.0-1915,30.6
	1915,35.4-1915,58.6
	1916,04.0-1916,31.0
	1916,33.6-1917,16.5
	1918,57.0-1920,13.3
	1922,13.1-1923,00.6
	1923,08.0-1923,30.6
	1925,27.0-1926,14.3
	1926,20.2-1926,43.6
	1928,42.0-1929,50.6
	1932,17.9-1933,05.6
	1935,17.3-1936,28.6
	1938,35.9-1939,23.3
	1939,30.7-1939,53.9
	1941,59.2-1943,10.6
	1945,02.9-1945,25.6
	1945,34.0-1946,21.8
	1946,25.5-1946,48.5
	1946,50.0-1947,13.6
	1947,18.9-1947,41.5
	1948,37.0-1948,59.6
	1949,04.5-1950,16.5
	1950,21.5-1951,09.5
	1951,15.4-1951,38.5
	1951,46.5-1952,09.6
	1952,17.4-2227,53.5
	2228,14.6-2359,59.9
040706, Entire Day
040806, 0000,00.0-0118,43.9
	1917,51.7-1918,15.4
	1919,10.4-2359,59.9
040906, 0000,00.0-0051,39.2
	1840,16.2-2234,24.7
	2238,08.3-2245,39.6
041006, 2207,52.0-2214,11.0
041106, 2145,31.4-2151,18.8
041306, 0130,25.9-0139,10.0
	1826,10.5-1826,56.3
	1832,39.6-1833,28.9
	1837,12.6-1838,02.4
	2303,10.5-2312,11.3
041506, 0506,52.3-0513,48.1
	1955,46.7-2359,59.9
041606, 0000,00.0-0102,17.3
	0110,31.3-0116,03.6
041706, 0112,56.4-0129,06.9
	2316,33.6-2323,13.9
041806, 2245,17.1-2307,38.0
042006, 0059,54.1-0109,20.6
042106, 0059,13.3-0107,55.8
042206, 0013,23.4-0020,14.5
042306, 0210,00.9-0217,45.2
042406, 0106,17.0-0114,43.3
042506, 0204,23.6-0210,52.8
042606, 0142,44.9-0154,09.6
042706, 0131,22.6-0140,22.9
042806, 0148,51.9-0156,41.7
042906, 1711,07.7-1717,40.2
043006, 1814,03.0-1823,13.5
*********************************************************


********IMPORTANT 3-D SONIC NOTE!!!**************
Due to an intense wind period during 03/25/06, the booms on which the RM Young sonics were situated rotated by the
force of the wind.  As a result of this, the two upper sonics data sets from 25 March should only be taken 
with a large error bound.  Additionally, a correction factor needs to be added for all data from 26 March until 8 April,
at which point the boom arms were rotated to their correct orientation.  The following are the corrections:

RM Young Sonic at 11.4m AGL:  Add 11deg to raw data from 26 March-8 April
RM Young Sonic at 7.3m AGL:   Add 104deg to raw data from 26 March-8 April
**********************************************************


*************ADDITIONAL FLUX TOWER NOTE*******************
The latitude and longitude coordinates saved in the data files are remnants of a previous field campaign and should be ignored.
**********************************************************


The ASU Sodar/RASS was situated on Dump Rd adjacent to the TREX mapr site, approximately 1km East of 
highway 395 on a slope of 2.4deg. Due to instrument instability during the early portion of TREX,
the data acquired from 03/01/06-03/25/06 was not set forth in this dataset presentation.
Left, is data from 03/26/06 through to 04/29/06 taken continuously for the entire period and given as either 15-min,
20-min, or 30-min averages.  Raw data stored included height(z) at 10m intervals with accompanying wind speed and direction.  Additionally,
the u,v,w components, sigma w, backscatter, temperature, virutal temperature, and error were saved as raw data with accompanying timestamp.
Sodar/RASS coordinates were: 
N 118deg 10'44.45'', W 36deg 47'14.85'', 1192m +-7m ASL.


***************
* DATA FILES: *
***************


Data is presented in a unified form with time stamps added for each
data point. Averaged data is also presented for the sodar/RASS. Time stamp presents 
the end time of the averaging period.

Time stamps are all in Coordinated Universal Time (UTC).

Positive U wind velocity component is from the west to the east
Positive V wind velocity component is from the south to the north
Positive W wind velocity component is upward

First lines sonic files contain the header lines with column names.
Columns are space delimited for sodar files, and comma delimited for soilflux files
and sonic files.

*****
Unified Sonic Data Files
*****

File folder location: Flux_Tower\ASU_Flux_Tower_Sonics (folder for each day)

These files contain instantaneous data taken every 0.1 sec (10 Hz sampling
rate). Files are named: 
	sonics_MMDDYY.dat     where MMDDYY are month, day, year (UTC)

Header Lines
	1-Instrument Info
	2-Column Titles
	3-Space
	4-Space

Column 1 -  Timestamp in form "YYYY-MM-DD HH:MM:SS.S" (UTC)
Column 2 -  u1mod    - u component of sonic at 11.4m (m/s)
Column 3 -  v1mod    - v component of sonic at 11.4m (m/s)
Column 4 -  w1       - w component of sonic at 11.4m (m/s)
Column 5 -  tc1      - temperature from sonic at 11.4m (deg C)
Column 6 -  m1       - magnitude of wind vector from sonic at 11.4m (m/s)
Column 7 -  d1       - direction of wind from sonic at 11.4m (deg)
Column 8 -  u2mod    - u component of sonic at 7.3m (m/s)
Column 9 -  v2mod    - v component of sonic at 7.3m (m/s)
Column 10 - w2       - w component of sonic at 7.3m (m/s)
Column 11 - tc2      - temperature from sonic at 7.3m (deg C)
Column 12 - rh       - relative humidity at 9.8m
Column 13 - kh       - Krypton hygrometer
Column 14 - cm3up    - incoming shortwave radiation (W/m2)
Column 15 - cm3dn    - outgoing shortwave radiation (W/m2)
Column 16 - cg3up    - incoming longwave radiation (W/m2)
Column 17 - cg3dn    - outgoing longwave radiation (W/m2)
Column 18 - fw       - finewire temperature (deg C)
Column 19 - bp       - barometric pressure (bar)
Column 20 - csatumod - u component of sonic at 1.9m
Column 21 - csatvmod - v component of sonic at 1.9m
Column 22 - csat(3)  - w component of sonic at 1.9m
Column 23 - csat(4)  - temperature from sonic at 1.9m (deg C)
column 24 - lat      - latitude (deg)  --->ignore
Column 25 - lon      - longitude (deg)  --->ignore


Data at times when instruments were not present or for power issues have the value -1.#QNAN


*****
IR Sensor, Thermistors (below the ground) and soil heat flux data file (folder for each day)
*****

File folder location: Flux_Tower\ASU_Flux_Tower_SoilFlux

These files contain instantaneous data taken every 1.0 sec (1 Hz sampling
rate). Files are named: 
	TREX_soilflux_MMDDYY.dat
where MMDDYY are month, day, year (UTC)

Header Lines:
	None


File contains two lines of data for each data point.

Line 1:
Column 1 - CR23x function number (220)
Column 2 - Julian Day (UTC)
Column 3 - HHMM (UTC)
Column 4 - SS.S (UTC)
Column 5 - Temperature 1 (deg C)(at 4.5cm below ground)
Column 6 - Temperature 2 (deg C)(at 5 cm below ground)
Column 7 - Temperature 3 (deg C)(at 7 cm below ground)
Column 8 - Temperature 4 (deg C)(at 9.5 cm below ground)
Column 9 - IR sensor (deg C)(surface)

Line 2:
Column 1 - CR23x function number (106)
Column 2 - Soil Heat flux (W/m2)
Column 3 - voltage 1(V)
Column 4 - voltage 2(V)

NOTE:  There is a third "function" line spaced at every 5-minute interval that should be ignored
*****************************************************************


*****
Sodar/RASS
*****

File folder location: Sodar (file for each day)

These files contain data averaged over 15-,20-,or 30-min periods with timestamp indicating last time
during period of averaging.

Files are named: 
	YYMMDD      where YYMMDD are year, month, day (UTC)

Header Lines: 34

Column 1 - Height of data point (m)
Column 2 - speed (m/s)
Column 3 - U (m/s)
Column 4 - V (m/s)
Column 5 - W (m/s)
Column 6 - sigW
Column 7 - backscatter
Column 8 - temperature (deg C)
Column 9 - virtual temperature (deg C)
Column 10 - error


Data contained in this archive arise from TREX campaign. The data is intended
to be used primarily by the program participants. The data may also be used for
research and educational purposes. To use the data the appropriate
acknowledgment should be given to the persons in any publications that result.
"Appropriate" may range from co-authorship to simple inclusion in an
acknowledgment section. Any uses of the data must receive approval from
ASU/EFD([Prof H.J.S. Fernando, j.fernando@asu.edu or Prof. Ronald Calhoun,
ronald.calhoun@asu.edu). 
(ASU - Arizona State University, EFD - Environmental Fluid Dynamics program)


Questions should be directed to: 

Adam Christman, adam.christman@asu.edu
Charles Retallack, charles.retallack@asu.edu

September, 2006. AC

