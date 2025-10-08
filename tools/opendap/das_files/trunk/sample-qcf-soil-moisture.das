Attributes {
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
   volume_fraction_of_condensed_water_in_soil_at_5cm_a {
      String Description "Soil Volumetric Water Content";
      String Units "m3/m3";
   }
   soil_matric_potential_at_5cm_a {
      String Description "Soil Matric Potential";
      String Units "kPa";
   }
   soil_depth_1a {
      String Description "Soil Depth";
      String Units "cm";
   }
   volume_fraction_of_condensed_water_in_soil_at_5cm_a_flag {
      String Description "Soil Volumetric Water Content Flag";
      String Values "U-unchecked; G-good; M-missing; D-questionable; B-unlikely; N-not_available_or_not_observed; X-glitch; E-estimated; C-exceeds_9999.99mm_or_negative; T-trace_amount_recorded; I-not_computed_due_to_insufficient_data";
   }
   volume_fraction_of_condensed_water_in_soil_at_5cm_b {
      String Description "Soil Volumetric Water Content";
      String Units "m3/m3";
   }
   soil_matric_potential_at_5cm_b {
      String Description "Soil Matric Potential";
      String Units "kPa";
   }
   soil_depth_1b {
      String Description "Soil Depth";
      String Units "cm";
   }
   volume_fraction_of_condensed_water_in_soil_at_5cm_b_flag {
      String Description "Soil Volumetric Water Content Flag";
      String Values "U-unchecked; G-good; M-missing; D-questionable; B-unlikely; N-not_available_or_not_observed; X-glitch; E-estimated; C-exceeds_9999.99mm_or_negative; T-trace_amount_recorded; I-not_computed_due_to_insufficient_data";
   }
   volume_fraction_of_condensed_water_in_soil_at_5cm_c {
      String Description "Soil Volumetric Water Content";
      String Units "m3/m3";
   }
   soil_matric_potential_at_5cm_c {
      String Description "Soil Matric Potential";
      String Units "kPa";
   }
   soil_depth_1c {
      String Description "Soil Depth";
      String Units "cm";
   }
   volume_fraction_of_condensed_water_in_soil_at_5cm_c_flag {
      String Description "Soil Volumetric Water Content Flag";
      String Values "U-unchecked; G-good; M-missing; D-questionable; B-unlikely; N-not_available_or_not_observed; X-glitch; E-estimated; C-exceeds_9999.99mm_or_negative; T-trace_amount_recorded; I-not_computed_due_to_insufficient_data";
   }
   volume_fraction_of_condensed_water_in_soil_at_10cm {
      String Description "Soil Volumetric Water Content";
      String Units "m3/m3";
   }
   soil_matric_potential_at_10cm {
      String Description "Soil Matric Potential";
      String Units "kPa";
   }
   soil_depth_2 {
      String Description "Soil Depth";
      String Units "cm";
   }
   volume_fraction_of_condensed_water_in_soil_at_10cm_flag {
      String Description "Soil Volumetric Water Content Flag";
      String Values "U-unchecked; G-good; M-missing; D-questionable; B-unlikely; N-not_available_or_not_observed; X-glitch; E-estimated; C-exceeds_9999.99mm_or_negative; T-trace_amount_recorded; I-not_computed_due_to_insufficient_data";
   }
   volume_fraction_of_condensed_water_in_soil_at_15cm {
      String Description "Soil Volumetric Water Content";
      String Units "m3/m3";
   }
   soil_matric_potential_at_15cm {
      String Description "Soil Matric Potential";
      String Units "kPa";
   }
   soil_depth_3 {
      String Description "Soil Depth";
      String Units "cm";
   }
   volume_fraction_of_condensed_water_in_soil_at_15cm_flag {
      String Description "Soil Volumetric Water Content Flag";
      String Values "U-unchecked; G-good; M-missing; D-questionable; B-unlikely; N-not_available_or_not_observed; X-glitch; E-estimated; C-exceeds_9999.99mm_or_negative; T-trace_amount_recorded; I-not_computed_due_to_insufficient_data";
   }
   volume_fraction_of_condensed_water_in_soil_at_20cm {
      String Description "Soil Volumetric Water Content";
      String Units "m3/m3";
   }
   soil_matric_potential_at_20cm {
      String Description "Soil Matric Potential";
      String Units "kPa";
   }
   soil_depth_4 {
      String Description "Soil Depth";
      String Units "cm";
   }
   volume_fraction_of_condensed_water_in_soil_at_20cm_flag {
      String Description "Soil Volumetric Water Content Flag";
      String Values "U-unchecked; G-good; M-missing; D-questionable; B-unlikely; N-not_available_or_not_observed; X-glitch; E-estimated; C-exceeds_9999.99mm_or_negative; T-trace_amount_recorded; I-not_computed_due_to_insufficient_data";
   }
   volume_fraction_of_condensed_water_in_soil_at_25cm {
      String Description "Soil Volumetric Water Content";
      String Units "m3/m3";
   }
   soil_matric_potential_at_25cm {
      String Description "Soil Matric Potential";
      String Units "kPa";
   }
   soil_depth_5 {
      String Description "Soil Depth";
      String Units "cm";
   }
   volume_fraction_of_condensed_water_in_soil_at_25cm_flag {
      String Description "Soil Volumetric Water Content Flag";
      String Values "U-unchecked; G-good; M-missing; D-questionable; B-unlikely; N-not_available_or_not_observed; X-glitch; E-estimated; C-exceeds_9999.99mm_or_negative; T-trace_amount_recorded; I-not_computed_due_to_insufficient_data";
   }
   volume_fraction_of_condensed_water_in_soil_at_60cm {
      String Description "Soil Volumetric Water Content";
      String Units "m3/m3";
   }
   soil_matric_potential_at_60cm {
      String Description "Soil Matric Potential";
      String Units "kPa";
   }
   soil_depth_6 {
      String Description "Soil Depth";
      String Units "cm";
   }
   volume_fraction_of_condensed_water_in_soil_at_60cm_flag {
      String Description "Soil Volumetric Water Content Flag";
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
