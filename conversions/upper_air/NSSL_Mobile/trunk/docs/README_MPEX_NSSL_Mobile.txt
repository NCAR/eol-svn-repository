Title: MPEX-NSSL-upsonde-data

Authors:
Michael Coniglio and Stacey Hitchcock
National Weather Center, Room 2237
120 David L Boren Blvd
Norman, OK 73072
Michael.Coniglio@noaa.gov

Data Set Overview:
This data set contains radiosonde data obtained by the National Severe Storms Laboratory (NSSL) mobile sounding unit. 

Files are named in the following format: NSSL_yyyymmdd_hhmm.txt, where hhmm is the launch time in minutes.

Instrument Description:
NSSL used two 403 MHz GPS soundings systems from InterMet Systems using the iMet-1 radiosondes.  The only difference in the two systems was the meteorological processor- one was the iMet-3050 and the other was the iMet-3150.  These systems process the pressure, temperature and humidity data once per second.  Both systems use a C-Code GPS receiver to locate the radiosonde in space.  This information is processed to produce wind speed and direction data.  Accuracy and precision data for the temperature and humidity probes was not provided in any user documents.  I suggest contacting InterMet if this information is needed (support@intermetsystems.com).  Intercomparison data between the iMet-1 radiosondes and the Vaisala RS-92 radiosondes is available from the PI .

Data collection and Processing:
Surface measurements from independent instruments are used as the first data point in the processed soundings.  The surface data was collected with instruments mounted on top of a Dodge Minivan.  The temperature and humidity probes were mounted within a U-tube and an RM Young anemometer was used for wind data.  A high-accuracy barometer was used for pressure measurements.  Note, the measurements were taken at approximately 4 m above ground, so the wind speeds are frequently much less than those just a few tens of meters above.  Radiosonde data was collected from the NSSL mobile sounding unit using 200g balloons filled with enough helium (~400 psi) for a nominal ascent rate of ~300 m/min.  A dereeler was used for each launch.  The processed temperature data contains a standard radiation correction provided in the iMet processing system.  The wind data is processed by iMet to remove spiraling shortly after release time.  The data was quality controlled extensively prior to processing by EOL with multiple passes through automated algorithms and manual checks.  Obviously bad data were removed, with other questionable data (often resulting from the radiosonde's proximity to convection) flagged in two different categories representing "questionable" and "use-at-your-own-risk" in our subjective opinions.    The raw data also is still available from this PI if interested.

Data Format:
The data is in column delimited ASCII format with the following convention:

Field 1: Always 0
Field 2: Date of observation (YYYYMMDD.)
Field 3: Time of observation to closest minute (UTC)
Field 4: Latitude of observation (deg N positive)
Field 5: Longitude of observation (deg W positive)
Field 6: QC code for lat/lon
Field 7: Pressure in mb
Field 8: QC code for pressure
Field 9: Temperature (deg C)
Field 10: QC code for temperature
Field 11: Relative Humidity (%)
Field 12: QC code for RH
Field 13: Geopotential Height (m)
Field 14: QC code for geopotential height
Field 15: u-component of the wind (m/s) (positive to east)
Field 16: v-component of the wind (m/s) (positive to north)
Field 17: QC code for wind
Field 18: Comments

QC code 1.0: Data appears to be good.
QC code 2.0: Data is questionable.
QC code 3.0: Use data at your own risk.
QC code 9.0: Data was bad and was removed.

Missing state variables are recorded as -99.0, except for pressure, which is recorded as 9999.0
Missing lat/lon values are recorded as 999.000
Missing times are 9999


Data Note:

The sounding NSSL_20130523_2341.txt did not have any good latitude or
longitude data.  The approximate release location for this sounding 
was Sweetwater, TX (32.44, -100.40).


