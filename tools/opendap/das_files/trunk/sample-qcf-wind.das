Attributes {
   date_nominal {
      String Description "UTC Nominal Date of Observation (YY/MM/DD)";
   }
   time_nominal {
      String Description "UTC Nominal Time of Observation (HH:mm)";
   }
   date {
      String Description "UTC Actual Date of Observation (YY/MM/DD)";
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
   surface_air_pressure {
      String Description "Station Pressure";
      String Units "hPa (mb)";
   }
   surface_air_pressure_flag {
      String Description "Station Pressure QC Flag";
      String Values "U-unchecked; G-good; M-missing; D-questionable; B-unlikely; N-not_available_or_not_observed; X-glitch; E-estimated; C-exceeds_9999.99mm_or_negative; T-trace_amount_recorded; I-not_computed_due_to_insufficient_data";
   }
   air_pressure_at_mean_sea_level {
      String Description "Station Pressure at Mean Sea Level";
      String Units "hPa (mb)";
   }
   air_pressure_at_mean_sea_level_flag {
      String Description "Station Pressure at Mean Sea Level QC Flag";
      String Values "U-unchecked; G-good; M-missing; D-questionable; B-unlikely; N-not_available_or_not_observed; X-glitch; E-estimated; C-exceeds_9999.99mm_or_negative; T-trace_amount_recorded; I-not_computed_due_to_insufficient_data";
   }
   air_pressure_at_mean_sea_level_computed {
      String Description "Station Pressure at Mean Sea Level Computed";
      String Units "hPa (mb)";
   }
   air_pressure_at_mean_sea_level_computed_flag {
      String Description "Station Pressure at Mean Sea Level Computed QC Flag";
      String Values "U-unchecked; G-good; M-missing; D-questionable; B-unlikely; N-not_available_or_not_observed; X-glitch; E-estimated; C-exceeds_9999.99mm_or_negative; T-trace_amount_recorded; I-not_computed_due_to_insufficient_data";
   }
   air_temperature {
      String Description "Dry Bulb Temperature";
      String Units "degrees C";
   }
   air_temperature_flag {
      String Description "Dry Bulb Temperature QC Flag";
      String Values "U-unchecked; G-good; M-missing; D-questionable; B-unlikely; N-not_available_or_not_observed; X-glitch; E-estimated; C-exceeds_9999.99mm_or_negative; T-trace_amount_recorded; I-not_computed_due_to_insufficient_data";
   }
   dew_point_temperature {
      String Description "Dew Point Temperature";
      String Units "degrees C";
   }
   dew_point_temperature_flag {
      String Description "Dew Point Temperature QC Flag";
      String Values "U-unchecked; G-good; M-missing; D-questionable; B-unlikely; N-not_available_or_not_observed; X-glitch; E-estimated; C-exceeds_9999.99mm_or_negative; T-trace_amount_recorded; I-not_computed_due_to_insufficient_data";
   }
   wind_speed {
      String Description "Wind Speed";
      String Units "m/s";
   }
   wind_speed_flag {
      String Description "Wind Speed QC Flag";
      String Values "U-unchecked; G-good; M-missing; D-questionable; B-unlikely; N-not_available_or_not_observed; X-glitch; E-estimated; C-exceeds_9999.99mm_or_negative; T-trace_amount_recorded; I-not_computed_due_to_insufficient_data";
   }
   wind_from_direction {
      String Description "Wind From Direction";
      String Units "degrees";
   }
   wind_from_direction_flag {
      String Description "Wind From Direction QC Flag";
      String Values "U-unchecked; G-good; M-missing; D-questionable; B-unlikely; N-not_available_or_not_observed; X-glitch; E-estimated; C-exceeds_9999.99mm_or_negative; T-trace_amount_recorded; I-not_computed_due_to_insufficient_data";
   }
   precipitation_amount {
      String Description "Total Precipitation Amount";
      String Units "mm";
   }
   precipitation_amount_flag {
      String Description "Total Precipitation Amount QC Flag";
      String Values "U-unchecked; G-good; M-missing; D-questionable; B-unlikely; N-not_available_or_not_observed; X-glitch; E-estimated; C-exceeds_9999.99mm_or_negative; T-trace_amount_recorded; I-not_computed_due_to_insufficient_data";
   }
   gust_indicator {
      String Description "Squall/Gust Indicator";
      String Values "blank-No_Squall_or_Gust; S-Squall; G-Gust";
   }
   wind_speed_of_gust {
      String Description "Squall/Gust Wind Speed";
      String Units "m/s";
   }
   wind_speed_of_gust_flag {
      String Description "Squall/Gust Wind Speed QC Flag";
      String Values "U-unchecked; G-good; M-missing; D-questionable; B-unlikely; N-not_available_or_not_observed; X-glitch; E-estimated; C-exceeds_9999.99mm_or_negative; T-trace_amount_recorded; I-not_computed_due_to_insufficient_data";
   }
   present_weather {
      String Description "Present Weather";
      String Values "See table 0 20 003 at https://www.wmo.int/pages/prog/www/WMOCodes/WMO306_vI2/LatestVERSION/WMO306_vI2_BUFRCREX_CodeFlag_en.pdf";
   }
   present_weather_flag {
      String Description "Present Weather QC Flag";
      String Values "U-unchecked; G-good; M-missing; D-questionable; B-unlikely; N-not_available_or_not_observed; X-glitch; E-estimated; C-exceeds_9999.99mm_or_negative; T-trace_amount_recorded; I-not_computed_due_to_insufficient_data";
   }
   visibility_in_air {
      String Description "Visibility in Air";
      String Units "m"; 
   }
   visibility_in_air_flag {
      String Description "Visibility in Air QC Flag";
      String Values "U-unchecked; G-good; M-missing; D-questionable; B-unlikely; N-not_available_or_not_observed; X-glitch; E-estimated; C-exceeds_9999.99mm_or_negative; T-trace_amount_recorded; I-not_computed_due_to_insufficient_data";
   }
   cloud_layer_first_base_altitude {
      String Description "Ceiling Height (first layer)";
      String Units "hundreds of feet";
   }
   cloud_layer_first_base_indicator {
      String Description "Ceiling Flag Indicator (first layer)";
      String Values "0-None; 1-Thin; 2-Clear_below_12,000_feet; 3-Estimated; 4-Measured; 5-Indefinite; 6-Balloon; 7-Aircraft; 8-Measured/Variable; 9-Clear_below_6,000_feet; 10-Estimated/Variable; 11-Indefinite/Variable; 12-14-Reserved; 15-Missing";
   }
   cloud_layer_first_base_indicator_flag {
      String Description "Ceiling Flag Indicator (first layer) QC Flag";
      String Values "U-unchecked; G-good; M-missing; D-questionable; B-unlikely; N-not_available_or_not_observed; X-glitch; E-estimated; C-exceeds_9999.99mm_or_negative; T-trace_amount_recorded; I-not_computed_due_to_insufficient_data";
   }
   cloud_layer_first_area_fraction {
      String Description "Cloud Amount Indicator (first layer)";
      String Values "See table 0 20 011 at https://www.wmo.int/pages/prog/www/WMOCodes/WMO306_vI2/LatestVERSION/WMO306_vI2_BUFRCREX_CodeFlag_en.pdf";
   }
   cloud_layer_first_area_fraction_flag {
      String Description "Cloud Amount Indicator (first layer) QC Flag";
      String Values "U-unchecked; G-good; M-missing; D-questionable; B-unlikely; N-not_available_or_not_observed; X-glitch; E-estimated; C-exceeds_9999.99mm_or_negative; T-trace_amount_recorded; I-not_computed_due_to_insufficient_data";
   }
   cloud_layer_second_base_altitude {
      String Description "Ceiling Height (second layer)";
      String Units "hundreds of feet";
   }
   cloud_layer_second_base_indicator {
      String Description "Ceiling Flag Indicator (second layer)";
      String Values "0-None; 1-Thin; 2-Clear_below_12,000_feet; 3-Estimated; 4-Measured; 5-Indefinite; 6-Balloon; 7-Aircraft; 8-Measured/Variable; 9-Clear_below_6,000_feet; 10-Estimated/Variable; 11-Indefinite/Variable; 12-14-Reserved; 15-Missing";
   }
   cloud_layer_second_base_indicator_flag {
      String Description "Ceiling Flag Indicator (second layer) QC Flag";
      String Values "U-unchecked; G-good; M-missing; D-questionable; B-unlikely; N-not_available_or_not_observed; X-glitch; E-estimated; C-exceeds_9999.99mm_or_negative; T-trace_amount_recorded; I-not_computed_due_to_insufficient_data";
   }
   cloud_layer_second_area_fraction {
      String Description "Cloud Amount Indicator (second layer)";
      String Values "See table 0 20 011 at https://www.wmo.int/pages/prog/www/WMOCodes/WMO306_vI2/LatestVERSION/WMO306_vI2_BUFRCREX_CodeFlag_en.pdf";
   }
   cloud_layer_second_area_fraction_flag {
      String Description "Cloud Amount Indicator (second layer) QC Flag";
      String Values "U-unchecked; G-good; M-missing; D-questionable; B-unlikely; N-not_available_or_not_observed; X-glitch; E-estimated; C-exceeds_9999.99mm_or_negative; T-trace_amount_recorded; I-not_computed_due_to_insufficient_data";
   }
   cloud_layer_third_base_altitude {
      String Description "Ceiling Height (third layer)";
      String Units "hundreds of feet";
   }
   cloud_layer_third_base_indicator {
      String Description "Ceiling Flag Indicator (third layer)";
      String Values "0-None; 1-Thin; 2-Clear_below_12,000_feet; 3-Estimated; 4-Measured; 5-Indefinite; 6-Balloon; 7-Aircraft; 8-Measured/Variable; 9-Clear_below_6,000_feet; 10-Estimated/Variable; 11-Indefinite/Variable; 12-14-Reserved; 15-Missing";
   }
   cloud_layer_third_base_indicator_flag {
      String Description "Ceiling Flag Indicator (third layer) QC Flag";
      String Values "U-unchecked; G-good; M-missing; D-questionable; B-unlikely; N-not_available_or_not_observed; X-glitch; E-estimated; C-exceeds_9999.99mm_or_negative; T-trace_amount_recorded; I-not_computed_due_to_insufficient_data";
   }
   cloud_layer_third_area_fraction {
      String Description "Cloud Amount Indicator (third layer)";
      String Values "See table 0 20 011 at https://www.wmo.int/pages/prog/www/WMOCodes/WMO306_vI2/LatestVERSION/WMO306_vI2_BUFRCREX_CodeFlag_en.pdf";
   }
   cloud_layer_third_area_fraction_flag {
      String Description "Cloud Amount Indicator (third layer) QC Flag";
      String Values "U-unchecked; G-good; M-missing; D-questionable; B-unlikely; N-not_available_or_not_observed; X-glitch; E-estimated; C-exceeds_9999.99mm_or_negative; T-trace_amount_recorded; I-not_computed_due_to_insufficient_data";
   }
   eastward_wind {
      String Description "U Wind Component";
      String Units "m/s";
   }
   eastward_wind_flag {
      String Description "U Wind Component QC Flag";
      String Values "U-unchecked; G-good; M-missing; D-questionable; B-unlikely; N-not_available_or_not_observed; X-glitch; E-estimated; C-exceeds_9999.99mm_or_negative; T-trace_amount_recorded; I-not_computed_due_to_insufficient_data";
   }
   northward_wind {
      String Description "V Wind Component";
      String Units "m/s";
   }
   northward_wind_flag {
      String Description "V Wind Component QC Flag";
      String Values "U-unchecked; G-good; M-missing; D-questionable; B-unlikely; N-not_available_or_not_observed; X-glitch; E-estimated; C-exceeds_9999.99mm_or_negative; T-trace_amount_recorded; I-not_computed_due_to_insufficient_data";
   }
   specific_humidity {
      String Description "Specific Humidity";
      String Units "kg/kg";
   }
   specific_humidity_flag {
      String Description "Specific Humidity QC Flag";
      String Values "U-unchecked; G-good; M-missing; D-questionable; B-unlikely; N-not_available_or_not_observed; X-glitch; E-estimated; C-exceeds_9999.99mm_or_negative; T-trace_amount_recorded; I-not_computed_due_to_insufficient_data";
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
