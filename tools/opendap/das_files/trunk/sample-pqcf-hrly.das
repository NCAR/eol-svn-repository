Attributes {
   date {
      String Description "UTC Date of Observation (YYYY/MM/DD)";
   }
   time {
      String Description "UTC Time of Observation (HH:mm:SS)";
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
   precipitation_amount_hour0 {
      String Description "Hourly Precipitation Value for UTC 0000";
      String Units "mm";
   }
   quality_flag_hour0 {
      String Description "Quality Flag for UTC 0000";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_hour0 {
      String Description "Hourly Precipitation QC Flag for UTC 0000";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_hour1 {
      String Description "Hourly Precipitation Value for UTC 0100";
      String Units "mm";
   }
   quality_flag_hour1 {
      String Description "Quality Flag for UTC 0100";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_hour1 {
      String Description "Hourly Precipitation QC Flag for UTC 0100";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_hour2 {
      String Description "Hourly Precipitation Value for UTC 0200";
      String Units "mm";
   }
   quality_flag_hour2 {
      String Description "Quality Flag for UTC 0200";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_hour2 {
      String Description "Hourly Precipitation QC Flag for UTC 0200";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_hour3 {
      String Description "Hourly Precipitation Value for UTC 0300";
      String Units "mm";
   }
   quality_flag_hour3 {
      String Description "Quality Flag for UTC 0300";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_hour3 {
      String Description "Hourly Precipitation QC Flag for UTC 0300";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_hour4 {
      String Description "Hourly Precipitation Value for UTC 0400";
      String Units "mm";
   }
   quality_flag_hour4 {
      String Description "Quality Flag for UTC 0400";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_hour4 {
      String Description "Hourly Precipitation QC Flag for UTC 0400";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_hour5 {
      String Description "Hourly Precipitation Value for UTC 0500";
      String Units "mm";
   }
   quality_flag_hour5 {
      String Description "Quality Flag for UTC 0500";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_hour5 {
      String Description "Hourly Precipitation QC Flag for UTC 0500";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_hour6 {
      String Description "Hourly Precipitation Value for UTC 0600";
      String Units "mm";
   }
   quality_flag_hour6 {
      String Description "Quality Flag for UTC 0600";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_hour6 {
      String Description "Hourly Precipitation QC Flag for UTC 0600";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_hour7 {
      String Description "Hourly Precipitation Value for UTC 0700";
      String Units "mm";
   }
   quality_flag_hour7 {
      String Description "Quality Flag for UTC 0700";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_hour7 {
      String Description "Hourly Precipitation QC Flag for UTC 0700";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_hour8 {
      String Description "Hourly Precipitation Value for UTC 0800";
      String Units "mm";
   }
   quality_flag_hour8 {
      String Description "Quality Flag for UTC 0800";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_hour8 {
      String Description "Hourly Precipitation QC Flag for UTC 0800";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_hour9 {
      String Description "Hourly Precipitation Value for UTC 0900";
      String Units "mm";
   }
   quality_flag_hour9 {
      String Description "Quality Flag for UTC 0900";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_hour9 {
      String Description "Hourly Precipitation QC Flag for UTC 0900";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_hour10 {
      String Description "Hourly Precipitation Value for UTC 1000";
      String Units "mm";
   }
   quality_flag_hour10 {
      String Description "Quality Flag for UTC 1000";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_hour10 {
      String Description "Hourly Precipitation QC Flag for UTC 1000";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_hour11 {
      String Description "Hourly Precipitation Value for UTC 1100";
      String Units "mm";
   }
   quality_flag_hour11 {
      String Description "Quality Flag for UTC 1100";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_hour11 {
      String Description "Hourly Precipitation QC Flag for UTC 1100";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_hour12 {
      String Description "Hourly Precipitation Value for UTC 1200";
      String Units "mm";
   }
   quality_flag_hour12 {
      String Description "Quality Flag for UTC 1200";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_hour12 {
      String Description "Hourly Precipitation QC Flag for UTC 1200";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_hour13 {
      String Description "Hourly Precipitation Value for UTC 1300";
      String Units "mm";
   }
   quality_flag_hour13 {
      String Description "Quality Flag for UTC 1300";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_hour13 {
      String Description "Hourly Precipitation QC Flag for UTC 1300";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_hour14 {
      String Description "Hourly Precipitation Value for UTC 1400";
      String Units "mm";
   }
   quality_flag_hour14 {
      String Description "Quality Flag for UTC 1400";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_hour14 {
      String Description "Hourly Precipitation QC Flag for UTC 1400";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_hour15 {
      String Description "Hourly Precipitation Value for UTC 1500";
      String Units "mm";
   }
   quality_flag_hour15 {
      String Description "Quality Flag for UTC 1500";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_hour15 {
      String Description "Hourly Precipitation QC Flag for UTC 1500";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_hour16 {
      String Description "Hourly Precipitation Value for UTC 1600";
      String Units "mm";
   }
   quality_flag_hour16 {
      String Description "Quality Flag for UTC 1600";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_hour16 {
      String Description "Hourly Precipitation QC Flag for UTC 1600";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_hour17 {
      String Description "Hourly Precipitation Value for UTC 1700";
      String Units "mm";
   }
   quality_flag_hour17 {
      String Description "Quality Flag for UTC 1700";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_hour17 {
      String Description "Hourly Precipitation QC Flag for UTC 1700";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_hour18 {
      String Description "Hourly Precipitation Value for UTC 1800";
      String Units "mm";
   }
   quality_flag_hour18 {
      String Description "Quality Flag for UTC 1800";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_hour18 {
      String Description "Hourly Precipitation QC Flag for UTC 1800";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_hour19 {
      String Description "Hourly Precipitation Value for UTC 1900";
      String Units "mm";
   }
   quality_flag_hour19 {
      String Description "Quality Flag for UTC 1900";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_hour19 {
      String Description "Hourly Precipitation QC Flag for UTC 1900";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_hour20 {
      String Description "Hourly Precipitation Value for UTC 2000";
      String Units "mm";
   }
   quality_flag_hour20 {
      String Description "Quality Flag for UTC 2000";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_hour20 {
      String Description "Hourly Precipitation QC Flag for UTC 2000";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_hour21 {
      String Description "Hourly Precipitation Value for UTC 2100";
      String Units "mm";
   }
   quality_flag_hour21 {
      String Description "Quality Flag for UTC 2100";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_hour21 {
      String Description "Hourly Precipitation QC Flag for UTC 2100";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_hour22 {
      String Description "Hourly Precipitation Value for UTC 2200";
      String Units "mm";
   }
   quality_flag_hour22 {
      String Description "Quality Flag for UTC 2200";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_hour22 {
      String Description "Hourly Precipitation QC Flag for UTC 2200";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_hour23 {
      String Description "Hourly Precipitation Value for UTC 2300";
      String Units "mm";
   }
   quality_flag_hour23 {
      String Description "Quality Flag for UTC 2300";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_hour23 {
      String Description "Hourly Precipitation QC Flag for UTC 2300";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   Global {
       String Title "Precipitation (Hourly) Multi-Network Composite";
       String Project "IHOP_2002: International H2O Project 2002";
       String Description "The Hourly Precipitation Composite was formed from several data sources (i.e., National Climatic Data Center hourly precipitation data, National Centers for Environmental Prediction/Environmental Modeling Center (NCEP/EMC) hourly precipitation data, and the precipitation data extracted from the IHOP_2002 Mesonet: Hourly Surface Meteorological Composite). Data from these sources were quality controlled and merged to form this precipitation composite. In July 2005 the Oklahoma Climatological Survey reprocessed and updated the Oklahoma Mesonet surface meteorological data. EOL has not updated the composite files, however the newer OKMESO data is available.";
       String Start_Date_Time "2002-05-27 00:00:00";
       String End_Date_Time "2002-05-27 23:59:59";
       Url Documentation https://data.eol.ucar.edu/datafile/nph-get/77.114/ihop_pcp60c.html;
       String Detailed_Description "The IHOP 2002 Hourly Precipitation Composite contains seven metadata parameters and three data parameters. The metadata parameters describe the date/time, network, station, and location at which the data was collected. The three data parameters repeat once for each UTC hour from 0000 through UTC hour 2300. Data reported for a designated hour represents data collected during the previous hour. All times are reported in UTC, and precipitation data values are reported in millimeters. A Quality Flag and a QC Code are assigned to each data value during the University Corporation for Atmospheric Research/Joint Office for Science Support (UCAR/JOSS) Quality Control (QC) processing.";
       String Processing_Version "1.0";
       String Release_Date "2011-08-24";
       String Author "EOL Data Support <eol-datahelp at ucar dot edu>";
       String References "DOI: https://doi.org/10.5065/D68G8J1B";
       String Institution "National Center for Atmospheric Research - Earth Observing Laboratory";
       Url Creator_URL http://www.eol.ucar.edu;
       String Creator_Email "eol-datahelp at ucar dot edu";
   }
}
