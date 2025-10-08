TITLE: University of Utah HOBO data

VERSION: 1.0 (13 June 2006), describes HOBO data version 1.0 (13 June 2006)

AUTHOR: 
Dr. C. David Whiteman
Research Professor
Meteorology Department
University of Utah
135 S 1460 E, Room 819
Salt Lake City, UT 84112-0110
Tel: (801)585-1414
Fax: (801)581-4362
whiteman@met.utah.edu

Collaborator: Dr. Sharon Zhong, Michigan State University, East Lansing, MI.

1.0 DATA SET OVERVIEW:
This dataset contains temperature data at 5-min intervals from 54 temperature data loggers located on (a) a West-East line running through Independence, California from the Sierras to the Inyos, (b) a West-East line running through Manzanar, California from the Sierras to the valley center, and (c) on a South-North line running from Lone Pine, California through Bishop, CA. Maps showing the locations of these lines are on the T-REX map server (http://mapserver.eol.ucar.edu/trex/, select T-REX Sensors, U. Utah HOBOs) or can be accessed from my T-REX web site (see URL in References section below). The reported temperatures are instantaneous data points recorded every 5 minutes. They are NOT averages of multiple data points. The sensor, however, has an e-folding response time of 2 minutes so that the instantaneous data point is actually averaged somewhat by the slow response of the sensor. Data collection was started on February 23 at 2005 UTC and stopped on May 2 at 2000 UTC. All sites were operational for the full period of the T-REX field experiment.

The sites for the temperature data loggers, also called HOBOs, are named by a one-letter prefix followed by a 2-digit number. This is optionally followed by a letter U. The sites are numbered consecutively from low elevations to higher elevations. On line (a) the HOBOs running westward up the Sierras from the Owens Valley floor are given the prefix W for West, on line (b) the hobos running up the east sidewall into the Inyo Mountains are given the naming prefix E for East. The HOBOs on line (c) are given the prefix M for Manzanar, while the HOBOs running up the valley floor from Lone Pine to Bishop are given the prefix V for Valley_floor. The HOBO temperature sensors were generally exposed in Gill-type 6-plate radiation shields, with the HOBO-radiation shield assembly bolted to fence posts so that the temperature sensors were generally 1 m above the ground. There was some variation in this height. Please send me an e-mail asking for an MS Word table containing site latitudes,longitudes, elevations and instrument heights. I'll post this on my T-REX web site, as well. At three sites (E14, V05, and W07) a second HOBO was installed to allow comparisons between two HOBOs at the same site. 

Seven of the sites on the valley floor had radiation shields that differed from the Gill-type shields mentioned above. These sites (V05U and V06-V11) used the 8-plate solar radiation shields that Onset Computer sells for the HOBOs. We do not anticipate that the type of radiation shield will be an important factor in the reported temperatures, as the radiation shields are both of the "self-aspirated" type and are comparable in design and effectiveness.

Pairs of photos of each of the sites (The first one looking up-valley and the second one looking down-valley) can be found on the web by following the URL in the references section at the end of this document.

2.0 INSTRUMENT DESCRIPTION:
The data logger is the HOBO H8 Pro Temp/Ext Temp data logger sold by Onset Computer of Bourne, Massachusetts (see URL below). This data logger is designed for outdoor use and is described in detail by Onset's commercial literature. The temperature data logger was tested for meteorological usage by Whiteman et al. (2003). Specifications and characteristics of the data logger are provided in that article. The data logger is attached to a Gill-type 6-plate radiation shield sold by the RM Young Co. (and by Campbell Scientific). The data logger is attached to the underside of the 6-plate shield with the temperature sensor itself exposed inside the shield. The radiation shield was attached to vertically-standing T-type steel fence posts that were pounded into the ground about 6 inches. The temperature sensors were nominally 1 m above ground. The effectiveness of the unaspirated radiation shields is described in papers referenced by Whiteman et al. (2000).

HOBO specifications:

Characteristic:	Specification
Number of Channels,	2 (internal and external temperature)
Operating Range (logger),	-30 to +50¡C
Time Accuracy,	± 1 minute per week at 20¡C
Measurement Capacity,	21,763 measurements (one channel at 12-bit and one at 8-bit resolution)
Memory,	non-volatile eeprom
Data Offload Time,	<1 minute
Size,	102 mm high, 81 mm wide, 55 mm deep
Weight,	145 g
Battery,	1/2 AA lithium, user-replaceable
Battery life (continuous use),	3 years
Storage Temperature,	-30 to +75 ¡C
External Temperature Sensor,	thermistor on 1.8 m lead
Response Time,	<5 minutes in still air
Resolution,	variable over temperature range -- <0.1¡C over the range 0 to 40¡C
Accuracy,	variable over temp range -- better than 0.4¡C over the range from 
-10 to 50¡C
Cost,	$169 (June 2006)

3.0 DATA COLLECTION AND PROCESSING:
The HOBOs on the sidewalls were, to the extent possible, placed on ridges on the sidewalls (rather than in gullies) so that they would provide temperature data representative of the valley atmosphere, rather than being located in non-representative microclimates. The HOBOs on the valley floor were generally located as near as possible (given landowner constraints) to the lowest elevation on the cross section, and therefore rather near the Owens River. This was done in an attempt to observe the lowest nighttime minima on the cross sections. These valley sites, though, probably will be more representative of microclimatic cold air pools than of the valley atmosphere in general.

4.0 DATA FORMAT:

All data files are ascii space-delimited files containing 10 REMARK lines followed by a line listing the parameters in the data columns, a line containing the units for each of the columns, and then 19584 lines of data at 5-min intervals. The 10th REMARK line in files HOBO_E14U.txt, HOBO_V05U, and HOBO_W07U explains that these are extra HOBOs placed at these three sites to compare against the primary HOBOs at these sites.

You programmers, please note that two bytes are required to store the date/time, latitude and longitude numbers, which carry a lot of decimal places.

A listing of the 54 HOBO files follows:

Hobo_E01.txt
Hobo_E02.txt
Hobo_E03.txt
Hobo_E04.txt
Hobo_E05.txt
Hobo_E06.txt
Hobo_E07.txt
Hobo_E08.txt
Hobo_E09.txt
Hobo_E10.txt
Hobo_E11.txt
Hobo_E12.txt
Hobo_E13.txt
Hobo_E14.txt
Hobo_E14U.txt
Hobo_M01.txt
Hobo_M02.txt
Hobo_M03.txt
Hobo_M04.txt
Hobo_M05.txt
Hobo_M06.txt
Hobo_M07.txt
Hobo_M08.txt
Hobo_M09.txt
Hobo_M10.txt
Hobo_M11.txt
Hobo_M12.txt
Hobo_M13.txt
Hobo_V01.txt
Hobo_V02.txt
Hobo_V03.txt
Hobo_V04.txt
Hobo_V05.txt
Hobo_V05U.txt
Hobo_V06.txt
Hobo_V07.txt
Hobo_V08.txt
Hobo_V09.txt
Hobo_V10.txt
Hobo_V11.txt
Hobo_W01.txt
Hobo_W02.txt
Hobo_W03.txt
Hobo_W04.txt
Hobo_W05.txt
Hobo_W06.txt
Hobo_W07.txt
Hobo_W07U.txt
Hobo_W08.txt
Hobo_W09.txt
Hobo_W10.txt
Hobo_W11.txt
Hobo_W12.txt
Hobo_W13.txt

The data lines in these files were written out from a FORTRAN program in 
this format:
12345678901234567890
      write(7,110) jutc(m*num),dlat(is),dlong(is),iht(is),t(k,m)
  110 format(i12.12,1x,f8.5,1x,f9.5,1x,i4,1x,a6)

Sample contents of a file follows:
  
PI/DATA CONTACT= Whiteman, Dave (U Utah)
DATA COVERAGE= START:200602232005;STOP:200605022000 UTC
PLATFORM/SITE= V11
INSTRUMENT= HOBO H8 Pro temperature datalogger
LOCATION= See below lat, long, elevation
DATA VERSION= 1.0 (13 June 2006), PRELIMINARY
REMARKS= MISSING OR BAD DATA = 99.99;
REMARKS= Data at time indicated is one instantaneous sample only
REMARKS= T sensor time constant is 2 minutes
DATE/TIME       LAT       LONG  ELEV  TEMP
UTC             Deg       Deg   m     Deg_C)
200602232005 36.97410 118.21132 1181  14.16
200602232010 36.97410 118.21132 1181  14.25
200602232015 36.97410 118.21132 1181  14.54
.
.
200605022000 36.97410 118.21132 1181  99.99

5.0 DATA REMARKS:

When the HOBOs were retrieved we found that some of the HOBOs had been disturbed sometime during the T-REX March/April period by grazing cows. We also found that some of the HOBOs had sopped collecting data partway through the experimental period. We are not yet clear why these HOBOs stopped. It appears to have been due to several causes, including static electricity. Failures are indicated in the dataset by the 99.99 code in place of actual temperatures. Note that it took several days to install and retrieve the HOBOs. In order to put as much data into the individual files as possible and to have all files start and end at the same times, we simply coded 99.99 for all times before the HOBOs were actually placed at their sites at the beginning of the experiment and for all times after the HOBOs were removed. We doubt that many investigators will be interested in these times at the beginning and ending of our files, but if you are interested you can see from the data when the HOBOs were installed or removed.

The "stoppages" and "failures" were as follows:
W13 - Feb 27 at 1805 UTC (Feb 27 at 1005 PST
W12 - Feb 23 at 1900 UTC (Feb 23 at 1100 PST)
W07 - Feb 24 at 2045 UTC (Feb 24 at 1245 PST)
W06 - April 5 at 1805 UTC (Apr 5 at 1005 PST)
W05 - April 5 at 1805 UTC (Apr 5 at 1005 PST)
W03 - May 1 at 1245 UTC (May 1 at 0445 PST)
E06 - Feb 23 at 1905 UTC (Feb 23 at 1105 PST)
E08 - May 1 at 1645 UTC (May 1 at 0845 PST)
E12 - bad temperatures during entire deployment (failure)
M13 - Apr 26 at 0845 UTC (Apr 26 at 0045 PST)
M01 - Mar 26 at 0205 UTC (Mar 25 at 1805 PST)
V04 - Apr 22 at 2325 UTC (Apr 22 at 1525 PST)

We had originally planned to install HOBOs at sites above W13 in the Sierras, but we could not get permission from the US Forest Service for reasons of avalanche safety. At the conclusion of T-REX we loaned 5 HOBOs to Dr. Stephen Mobbs at the University of Leeds. He had permission to install HOBOs at sites in the Onion Vally above our site W13 and he is planning to install these HOBOs at these sites and leave them there until late August 2006. So, there is more data coming, although it is outside the main T-REX experimental period.

Sites V08, E07, E12, and E13 were co-located with soil sensors operated by Dr. Tina Chow and Dr. Greg Poulos.

Sites W03 and V04 were knocked over by cows sometime during the deployment. The user should check data from these sites to determine when this happened and how it affected the data. Other sites (M03, M13, and W04) were disturbed slightly by cows rubbing against the fence posts and tilting the radiation shields upward (but not knocking them down). Data from these "disturbed" sites are probably OK without modification.

Sites V03 and M02 were co-located with automatic weather stations (AWS) installed by other T-REX investigators. The HOBOs were generally attached to either a fence post or the AWS tower at 1 m height.

Post-experiment calibrations were performed on all HOBOs and the following corrections were made in ver. 1.1:

W07 : 0.30¡C were added to temperatures measured at this site.
W02 : 0.28¡C were added to temperatures measured at this site.
V05 : 0.18¡C were added to temperatures measured at this site.
W01 : 4.08¡C were added to temperatures measured at this site.

We are labeling this release as PRELIMINARY. Our intercomparison of graphs of measured temperatures at adjacent HOBO sites has now been completed and the data all look reasonable. We are also performing post-T-REX calibrations (actually, single point intercomparisons of all HOBOs in a constant temperature environment), just as we did before the experiment. We will also be comparing HOBO data to automatic weather station data at several sites where these were co-located. 

Please give me a call if you have any questions or have a need for the data in a format that might be easier for you to handle. For example, I am averaging the 5-min data to other averaging periods and am working with the data primarily in PST time coordinates. Also, in conjunction with other T-REX investigators, I will be converting the HOBO temperatures to potential temperatures using pressure data from other T-REX data networks and integrating different temperature and potential temperature data sets. And, please let me know if you find any errors or corrections.

6.0 REFERENCES:
HOBO manufacturer
http://www.onsetcomp.com

Radiation shield manufacturer
http://www.rmyoung.com

Whiteman's web page
http://wwwmet.utah.edu/Members/whiteman

Whiteman's T-REX web page
http://www.met.utah.edu/whiteman/T_REX

Whiteman's T-REX HOBO site photos (includes maps)
http://homepage.mac.com/davidwhiteman/PhotoAlbum4.html

T-REX map server
http://mapserver.eol.ucar.edu/trex/

Journal paper on HOBO use in meteorology
Whiteman, C. D., J. M. Hubbe, and W. J. Shaw, 2000: Evaluation of an inexpensive temperature data logger for meteorological applications. J. Atmos. Oceanic Technol., 17, 77-81.

Journal papers in which HOBO analyses were featured
Whiteman, C. D., S. Zhong, W. J. Shaw, J. M. Hubbe, X. Bian, and J. Mittelstadt, 2001: Cold pools in the Columbia Basin. Weather and Forecasting, 16, 432-447.

Mayr, G. J., L. Armi, S. Arnold, R. M. Banta, L. S. Darby, D. D. Durran, C. Flamant, S. Gabersek, A. Gohm, R. Mayr, S. Mobbs, L. B. Nance, I. Vergeiner, J. Vergeiner, and C. D. Whiteman, 2004: GAP flow measurements during the Mesoscale Alpine Programme. Meteorology and Atmospheric Physics, 86, no. 1-2, 99-119.

Whiteman, C. D., T. Haiden, B. Pospichal, S. Eisenbach, and R. Steinacker, 2004: Minimum temperatures, diurnal temperature ranges and temperature inversions in limestone sinkholes of different size and shape. J. Appl. Meteor., 43 (8), 1224-1236.

Whiteman, C. D., S. Eisenbach, B. Pospichal, and R. Steinacker, 2004: Comparison of vertical soundings and sidewall air temperature measurements in a small Alpine basin. J. Appl. Meteor., 43 (11), 1635-1647.

De Wekker, S. F. J., and C. D. Whiteman, 2006: On the time scale of nocturnal boundary layer cooling in valleys and basins and over plains. J. Appl. Meteor., 45 (6), 813-820.
