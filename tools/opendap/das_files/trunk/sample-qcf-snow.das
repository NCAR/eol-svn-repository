Attributes {
   date {
      String Description "UTC Actual Date of Observation (YYYY/MM/DD)";
   }
   time {
      String Description "UTC Actual Time of Observation (HH:mm)";
   }
   network_name {
      String Description "Network Identifier";
   }
   platform_name {
      String Description "Station Identifier";
   }
   latitude {
      String Description "Station Latitude";
      String Units "degrees north";
   }
   longitude {
      String Description "Station Longitude";
      String Units "degrees east";
   }
   station_occurrence {
      String Description "Station Occurrence";
      String Long_Description "Value indicating that there are multiple, co-located stations in this dataset. Co-located stations may be from the same or different networks. Value is from 0 to n indicating there are n+1 co-located stations.  Each co-located station is assigned a unique station occurrence number.";
   }
   surface_altitude {
      String Description "Station Elevation";
      String Units "m";
   }
   air_temperature_maximum {
      String Description "Maximum Temperature";
      String Units "degrees C";
   }
   air_temperature_maximum_flag1 {
      String Description "Maximum Temperature QC Flag 1";
      String Values "0-Reserved; 1-Accumulated amount since last measurement; 2-Accumulated amount includes estimated value; 3-Estimated; 4-Value has been manually validated; 5-Included in a subsequent value; 6-Trace; 7-Expert system edited not validated; 8-Expert system approved edited value; 9-Flag not needed; 11 through 255-Reserved";
   }
   air_temperature_maximum_flag2 {
      String Description "Maximum Temperature QC Flag 2";
      String Values "0-Reserved; 1-Valid data element; 2-Valid data element from unknown source; 3-Invalid data element replacement follows; 4-Invalid data element no replacement follows; 5-Validity unknown, not checked; 6-Original value has been replaced by numeric value; 7-Substituted TOBS for TMAX or TMIN; 8-Time shifted value; 9-Precipitation estimated by snowfall; 10-Transposed digits; 11-Changed units; 12-Adjusted TMAX or TMIN by a multiple of + or - 10 degrees; 13-Changed algebraic sign; 14-Moved decimal point; 15-Rescaling other than 12, 13, or 14; 16-subjectively derived value; 17-Extracted from an accumulated value; 18-Switched to TMAX or TMIN; 19-Switched TOBS with TMAX or TMIN; 20-Substitution of 3 nearest station mean; 21-Switched snow and precipitation data value; 22-Added snowfall to snow depth; 23-Switched snowfall and snow depth; 24-Precipitation not reported, estimated as 0; 25-Manually edited value; 26-Failed internal consistency check; 27-Failed areal consistency check (beginning October 1992); 28 through 255-Reserved";
   }
   air_temperature_minimum {
      String Description "Minimum Temperature";
      String Units "degrees C";
   }
   air_temperature_minimum_flag1 {
      String Description "Minimum Temperature QC Flag 1";
      String Values "0-Reserved; 1-Accumulated amount since last measurement; 2-Accumulated amount includes estimated value; 3-Estimated; 4-Value has been manually validated; 5-Included in a subsequent value; 6-Trace; 7-Expert system edited not validated; 8-Expert system approved edited value; 9-Flag not needed; 11 through 255-Reserved";
   }
   air_temperature_minimum_flag2 {
      String Description "Minimum Temperature QC Flag 2";
      String Values "0-Reserved; 1-Valid data element; 2-Valid data element from unknown source; 3-Invalid data element replacement follows; 4-Invalid data element no replacement follows; 5-Validity unknown, not checked; 6-Original value has been replaced by numeric value; 7-Substituted TOBS for TMAX or TMIN; 8-Time shifted value; 9-Precipitation estimated by snowfall; 10-Transposed digits; 11-Changed units; 12-Adjusted TMAX or TMIN by a multiple of + or - 10 degrees; 13-Changed algebraic sign; 14-Moved decimal point; 15-Rescaling other than 12, 13, or 14; 16-subjectively derived value; 17-Extracted from an accumulated value; 18-Switched to TMAX or TMIN; 19-Switched TOBS with TMAX or TMIN; 20-Substitution of 3 nearest station mean; 21-Switched snow and precipitation data value; 22-Added snowfall to snow depth; 23-Switched snowfall and snow depth; 24-Precipitation not reported, estimated as 0; 25-Manually edited value; 26-Failed internal consistency check; 27-Failed areal consistency check (beginning October 1992); 28 through 255-Reserved";
   }
   surface_snow_thickness {
      String Description "Total Snow Depth";
      String Units "m";
   }
   surface_snow_thickness_flag1 {
      String Description "Total Snow Depth QC Flag 1";
      String Values "0-Reserved; 1-Accumulated amount since last measurement; 2-Accumulated amount includes estimated value; 3-Estimated; 4-Value has been manually validated; 5-Included in a subsequent value; 6-Trace; 7-Expert system edited not validated; 8-Expert system approved edited value; 9-Flag not needed; 11 through 255-Reserved";
   }
   surface_snow_thickness_flag2 {
      String Description "Total Snow Depth QC Flag 2";
      String Values "0-Reserved; 1-Valid data element; 2-Valid data element from unknown source; 3-Invalid data element replacement follows; 4-Invalid data element no replacement follows; 5-Validity unknown, not checked; 6-Original value has been replaced by numeric value; 7-Substituted TOBS for TMAX or TMIN; 8-Time shifted value; 9-Precipitation estimated by snowfall; 10-Transposed digits; 11-Changed units; 12-Adjusted TMAX or TMIN by a multiple of + or - 10 degrees; 13-Changed algebraic sign; 14-Moved decimal point; 15-Rescaling other than 12, 13, or 14; 16-subjectively derived value; 17-Extracted from an accumulated value; 18-Switched to TMAX or TMIN; 19-Switched TOBS with TMAX or TMIN; 20-Substitution of 3 nearest station mean; 21-Switched snow and precipitation data value; 22-Added snowfall to snow depth; 23-Switched snowfall and snow depth; 24-Precipitation not reported, estimated as 0; 25-Manually edited value; 26-Failed internal consistency check; 27-Failed areal consistency check (beginning October 1992); 28 through 255-Reserved";
   }
   precipitation_amount {
      String Description "Total Precipitation";
      String Units "mm";
   }
   precipitation_amount_flag1 {
      String Description "Total Precipitation QC Flag 1";
      String Values "0-Reserved; 1-Accumulated amount since last measurement; 2-Accumulated amount includes estimated value; 3-Estimated; 4-Value has been manually validated; 5-Included in a subsequent value; 6-Trace; 7-Expert system edited not validated; 8-Expert system approved edited value; 9-Flag not needed; 11 through 255-Reserved";
   }
   precipitation_amount_flag2 {
      String Description "Total Precipitation QC Flag 2";
      String Values "0-Reserved; 1-Valid data element; 2-Valid data element from unknown source; 3-Invalid data element replacement follows; 4-Invalid data element no replacement follows; 5-Validity unknown, not checked; 6-Original value has been replaced by numeric value; 7-Substituted TOBS for TMAX or TMIN; 8-Time shifted value; 9-Precipitation estimated by snowfall; 10-Transposed digits; 11-Changed units; 12-Adjusted TMAX or TMIN by a multiple of + or - 10 degrees; 13-Changed algebraic sign; 14-Moved decimal point; 15-Rescaling other than 12, 13, or 14; 16-subjectively derived value; 17-Extracted from an accumulated value; 18-Switched to TMAX or TMIN; 19-Switched TOBS with TMAX or TMIN; 20-Substitution of 3 nearest station mean; 21-Switched snow and precipitation data value; 22-Added snowfall to snow depth; 23-Switched snowfall and snow depth; 24-Precipitation not reported, estimated as 0; 25-Manually edited value; 26-Failed internal consistency check; 27-Failed areal consistency check (beginning October 1992); 28 through 255-Reserved";
   }
   thickness_of_snowfall_amount {
      String Description "Daily Snow Fall";
      String Units "m";
   }
   thickness_of_snowfall_amount_flag1 {
      String Description "Daily Snow Fall QC Flag 1";
      String Values "0-Reserved; 1-Accumulated amount since last measurement; 2-Accumulated amount includes estimated value; 3-Estimated; 4-Value has been manually validated; 5-Included in a subsequent value; 6-Trace; 7-Expert system edited not validated; 8-Expert system approved edited value; 9-Flag not needed; 11 through 255-Reserved";
   }
   thickness_of_snowfall_amount_flag2 {
      String Description "Daily Snow Fall QC Flag 2";
      String Values "0-Reserved; 1-Valid data element; 2-Valid data element from unknown source; 3-Invalid data element replacement follows; 4-Invalid data element no replacement follows; 5-Validity unknown, not checked; 6-Original value has been replaced by numeric value; 7-Substituted TOBS for TMAX or TMIN; 8-Time shifted value; 9-Precipitation estimated by snowfall; 10-Transposed digits; 11-Changed units; 12-Adjusted TMAX or TMIN by a multiple of + or - 10 degrees; 13-Changed algebraic sign; 14-Moved decimal point; 15-Rescaling other than 12, 13, or 14; 16-subjectively derived value; 17-Extracted from an accumulated value; 18-Switched to TMAX or TMIN; 19-Switched TOBS with TMAX or TMIN; 20-Substitution of 3 nearest station mean; 21-Switched snow and precipitation data value; 22-Added snowfall to snow depth; 23-Switched snowfall and snow depth; 24-Precipitation not reported, estimated as 0; 25-Manually edited value; 26-Failed internal consistency check; 27-Failed areal consistency check (beginning October 1992); 28 through 255-Reserved";
   }
   Global {
       String Title "Surface Meteorological (1-minute) Multi-Network Composite";
       String Project "IHOP_2002: International H2O Project 2002";
       String Description "This data set contains one-minute resolution surface meteorological data in National Center for Atmospheric Research/Earth Observing Laboratory (NCAR/EOL) Quality Control (QC) format from stations within the following networks: Automated Surface Observing System (ASOS), Department of Energy (DOE) Atmospheric Radiation Measurement (ARM) Southern Great Plains (SGP) Surface Meteorological System (SMOS) ?ARMSFC?, DOE Atmospheric Boundary Layer Experiments (ABLE) Automatic Weather Station (AWS), National Center for Atmospheric Research Atmospheric Technology Division (NCAR/ATD) Supplemental Surface Meteorological data (NCAR_supp), and the NCAR/ATD Homestead Integrated Sounding System (ISS) observation stations. Data for the International H2O Project 2002 (IHOP 2002) domain (32N to 42N latitude and 90W to 105W longitude) and time period (13 May 2002 through 25 June 2002) are contained within this dataset. This dataset contains data from 66 stations.";
       String Start_Date_Time "2002-05-14 00:00:00";
       String End_Date_Time "2002-05-14 23:59:59";
       Url Documentation https://data.eol.ucar.edu/datafile/nph-get/77.109/ihop_1minSfcComp.html;
       String Detailed_Description "The IHOP 2002 One Minute Surface Composite observation data contains ten metadata parameters and 38 data parameters and flags. The metadata parameters describe the station location and time at which the data were collected. The time of observation is reported both in Universal Time Coordinated (UTC) Nominal and UTC actual time. For this one minute surface composite, reported nominal time and actual time are the same. Days begin at UTC 0100 and end at UTC 0000 the following day. Several data parameters have an associated Quality Control (QC) Flag Code which are assigned by the Joint Office for Science Support (JOSS).";
       String Processing_Version "1.0";
       String Release_Date "2011-08-24";
       String Author "EOL Data Support <eol-datahelp at ucar dot edu>";
       String References "DOI: https://doi.org/10.5065/D64T6GN1";
       String Institution "National Center for Atmospheric Research - Earth Observing Laboratory";
       Url Creator_URL http://www.eol.ucar.edu;
       String Creator_Email "eol-datahelp at ucar dot edu";
   }
}
