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
   precipitation_amount_0000 {
      String Description "15 Minute Precipitation Value for UTC 0000";
      String Units "mm";
   }
   quality_flag_0000 {
      String Description "Quality Flag for UTC 0000";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_0000 {
      String Description "15 Minute Precipitation QC Flag for UTC 0000";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_0015 {
      String Description "15 Minute Precipitation Value for UTC 0015";
      String Units "mm";
   }
   quality_flag_0015 {
      String Description "Quality Flag for UTC 0015";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_0015 {
      String Description "15 Minute Precipitation QC Flag for UTC 0015";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_0030 {
      String Description "15 Minute Precipitation Value for UTC 0030";
      String Units "mm";
   }
   quality_flag_0030 {
      String Description "Quality Flag for UTC 0030";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_0030 {
      String Description "15 Minute Precipitation QC Flag for UTC 0030";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_0045 {
      String Description "15 Minute Precipitation Value for UTC 0045";
      String Units "mm";
   }
   quality_flag_0045 {
      String Description "Quality Flag for UTC 0045";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_0045 {
      String Description "15 Minute Precipitation QC Flag for UTC 0045";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_0100 {
      String Description "15 Minute Precipitation Value for UTC 0100";
      String Units "mm";
   }
   quality_flag_0100 {
      String Description "Quality Flag for UTC 0100";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_0100 {
      String Description "15 Minute Precipitation QC Flag for UTC 0100";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_0115 {
      String Description "15 Minute Precipitation Value for UTC 0115";
      String Units "mm";
   }
   quality_flag_0115 {
      String Description "Quality Flag for UTC 0115";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_0115 {
      String Description "15 Minute Precipitation QC Flag for UTC 0115";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_0130 {
      String Description "15 Minute Precipitation Value for UTC 0130";
      String Units "mm";
   }
   quality_flag_0130 {
      String Description "Quality Flag for UTC 0130";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_0130 {
      String Description "15 Minute Precipitation QC Flag for UTC 0130";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_0145 {
      String Description "15 Minute Precipitation Value for UTC 0145";
      String Units "mm";
   }
   quality_flag_0145 {
      String Description "Quality Flag for UTC 0145";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_0145 {
      String Description "15 Minute Precipitation QC Flag for UTC 0145";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_0200 {
      String Description "15 Minute Precipitation Value for UTC 0200";
      String Units "mm";
   }
   quality_flag_0200 {
      String Description "Quality Flag for UTC 0200";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_0200 {
      String Description "15 Minute Precipitation QC Flag for UTC 0200";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_0215 {
      String Description "15 Minute Precipitation Value for UTC 0215";
      String Units "mm";
   }
   quality_flag_0215 {
      String Description "Quality Flag for UTC 0215";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_0215 {
      String Description "15 Minute Precipitation QC Flag for UTC 0215";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_0230 {
      String Description "15 Minute Precipitation Value for UTC 0230";
      String Units "mm";
   }
   quality_flag_0230 {
      String Description "Quality Flag for UTC 0230";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_0230 {
      String Description "15 Minute Precipitation QC Flag for UTC 0230";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_0245 {
      String Description "15 Minute Precipitation Value for UTC 0245";
      String Units "mm";
   }
   quality_flag_0245 {
      String Description "Quality Flag for UTC 0245";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_0245 {
      String Description "15 Minute Precipitation QC Flag for UTC 0245";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_0300 {
      String Description "15 Minute Precipitation Value for UTC 0300";
      String Units "mm";
   }
   quality_flag_0300 {
      String Description "Quality Flag for UTC 0300";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_0300 {
      String Description "15 Minute Precipitation QC Flag for UTC 0300";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_0315 {
      String Description "15 Minute Precipitation Value for UTC 0315";
      String Units "mm";
   }
   quality_flag_0315 {
      String Description "Quality Flag for UTC 0315";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_0315 {
      String Description "15 Minute Precipitation QC Flag for UTC 0315";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_0330 {
      String Description "15 Minute Precipitation Value for UTC 0330";
      String Units "mm";
   }
   quality_flag_0330 {
      String Description "Quality Flag for UTC 0330";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_0330 {
      String Description "15 Minute Precipitation QC Flag for UTC 0330";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_0345 {
      String Description "15 Minute Precipitation Value for UTC 0345";
      String Units "mm";
   }
   quality_flag_0345 {
      String Description "Quality Flag for UTC 0345";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_0345 {
      String Description "15 Minute Precipitation QC Flag for UTC 0345";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_0400 {
      String Description "15 Minute Precipitation Value for UTC 0400";
      String Units "mm";
   }
   quality_flag_0400 {
      String Description "Quality Flag for UTC 0400";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_0400 {
      String Description "15 Minute Precipitation QC Flag for UTC 0400";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_0415 {
      String Description "15 Minute Precipitation Value for UTC 0415";
      String Units "mm";
   }
   quality_flag_0415 {
      String Description "Quality Flag for UTC 0415";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_0415 {
      String Description "15 Minute Precipitation QC Flag for UTC 0415";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_0430 {
      String Description "15 Minute Precipitation Value for UTC 0430";
      String Units "mm";
   }
   quality_flag_0430 {
      String Description "Quality Flag for UTC 0430";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_0430 {
      String Description "15 Minute Precipitation QC Flag for UTC 0430";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_0445 {
      String Description "15 Minute Precipitation Value for UTC 0445";
      String Units "mm";
   }
   quality_flag_0445 {
      String Description "Quality Flag for UTC 0445";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_0445 {
      String Description "15 Minute Precipitation QC Flag for UTC 0445";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_0500 {
      String Description "15 Minute Precipitation Value for UTC 0500";
      String Units "mm";
   }
   quality_flag_0500 {
      String Description "Quality Flag for UTC 0500";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_0500 {
      String Description "15 Minute Precipitation QC Flag for UTC 0500";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_0515 {
      String Description "15 Minute Precipitation Value for UTC 0515";
      String Units "mm";
   }
   quality_flag_0515 {
      String Description "Quality Flag for UTC 0515";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_0515 {
      String Description "15 Minute Precipitation QC Flag for UTC 0515";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_0530 {
      String Description "15 Minute Precipitation Value for UTC 0530";
      String Units "mm";
   }
   quality_flag_0530 {
      String Description "Quality Flag for UTC 0530";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_0530 {
      String Description "15 Minute Precipitation QC Flag for UTC 0530";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_0545 {
      String Description "15 Minute Precipitation Value for UTC 0545";
      String Units "mm";
   }
   quality_flag_0545 {
      String Description "Quality Flag for UTC 0545";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_0545 {
      String Description "15 Minute Precipitation QC Flag for UTC 0545";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_0600 {
      String Description "15 Minute Precipitation Value for UTC 0600";
      String Units "mm";
   }
   quality_flag_0600 {
      String Description "Quality Flag for UTC 0600";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_0600 {
      String Description "15 Minute Precipitation QC Flag for UTC 0600";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_0615 {
      String Description "15 Minute Precipitation Value for UTC 0615";
      String Units "mm";
   }
   quality_flag_0615 {
      String Description "Quality Flag for UTC 0615";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_0615 {
      String Description "15 Minute Precipitation QC Flag for UTC 0615";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_0630 {
      String Description "15 Minute Precipitation Value for UTC 0630";
      String Units "mm";
   }
   quality_flag_0630 {
      String Description "Quality Flag for UTC 0630";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_0630 {
      String Description "15 Minute Precipitation QC Flag for UTC 0630";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_0645 {
      String Description "15 Minute Precipitation Value for UTC 0645";
      String Units "mm";
   }
   quality_flag_0645 {
      String Description "Quality Flag for UTC 0645";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_0645 {
      String Description "15 Minute Precipitation QC Flag for UTC 0645";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_0700 {
      String Description "15 Minute Precipitation Value for UTC 0700";
      String Units "mm";
   }
   quality_flag_0700 {
      String Description "Quality Flag for UTC 0700";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_0700 {
      String Description "15 Minute Precipitation QC Flag for UTC 0700";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_0715 {
      String Description "15 Minute Precipitation Value for UTC 0715";
      String Units "mm";
   }
   quality_flag_0715 {
      String Description "Quality Flag for UTC 0715";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_0715 {
      String Description "15 Minute Precipitation QC Flag for UTC 0715";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_0730 {
      String Description "15 Minute Precipitation Value for UTC 0730";
      String Units "mm";
   }
   quality_flag_0730 {
      String Description "Quality Flag for UTC 0730";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_0730 {
      String Description "15 Minute Precipitation QC Flag for UTC 0730";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_0745 {
      String Description "15 Minute Precipitation Value for UTC 0745";
      String Units "mm";
   }
   quality_flag_0745 {
      String Description "Quality Flag for UTC 0745";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_0745 {
      String Description "15 Minute Precipitation QC Flag for UTC 0745";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_0800 {
      String Description "15 Minute Precipitation Value for UTC 0800";
      String Units "mm";
   }
   quality_flag_0800 {
      String Description "Quality Flag for UTC 0800";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_0800 {
      String Description "15 Minute Precipitation QC Flag for UTC 0800";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_0815 {
      String Description "15 Minute Precipitation Value for UTC 0815";
      String Units "mm";
   }
   quality_flag_0815 {
      String Description "Quality Flag for UTC 0815";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_0815 {
      String Description "15 Minute Precipitation QC Flag for UTC 0815";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_0830 {
      String Description "15 Minute Precipitation Value for UTC 0830";
      String Units "mm";
   }
   quality_flag_0830 {
      String Description "Quality Flag for UTC 0830";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_0830 {
      String Description "15 Minute Precipitation QC Flag for UTC 0830";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_0845 {
      String Description "15 Minute Precipitation Value for UTC 0845";
      String Units "mm";
   }
   quality_flag_0845 {
      String Description "Quality Flag for UTC 0845";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_0845 {
      String Description "15 Minute Precipitation QC Flag for UTC 0845";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_0900 {
      String Description "15 Minute Precipitation Value for UTC 0900";
      String Units "mm";
   }
   quality_flag_0900 {
      String Description "Quality Flag for UTC 0900";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_0900 {
      String Description "15 Minute Precipitation QC Flag for UTC 0900";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_0915 {
      String Description "15 Minute Precipitation Value for UTC 0915";
      String Units "mm";
   }
   quality_flag_0915 {
      String Description "Quality Flag for UTC 0915";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_0915 {
      String Description "15 Minute Precipitation QC Flag for UTC 0915";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_0930 {
      String Description "15 Minute Precipitation Value for UTC 0930";
      String Units "mm";
   }
   quality_flag_0930 {
      String Description "Quality Flag for UTC 0930";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_0930 {
      String Description "15 Minute Precipitation QC Flag for UTC 0930";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_0945 {
      String Description "15 Minute Precipitation Value for UTC 0945";
      String Units "mm";
   }
   quality_flag_0945 {
      String Description "Quality Flag for UTC 0945";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_0945 {
      String Description "15 Minute Precipitation QC Flag for UTC 0945";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_1000 {
      String Description "15 Minute Precipitation Value for UTC 1000";
      String Units "mm";
   }
   quality_flag_1000 {
      String Description "Quality Flag for UTC 1000";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_1000 {
      String Description "15 Minute Precipitation QC Flag for UTC 1000";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_1015 {
      String Description "15 Minute Precipitation Value for UTC 1015";
      String Units "mm";
   }
   quality_flag_1015 {
      String Description "Quality Flag for UTC 1015";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_1015 {
      String Description "15 Minute Precipitation QC Flag for UTC 1015";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_1030 {
      String Description "15 Minute Precipitation Value for UTC 1030";
      String Units "mm";
   }
   quality_flag_1030 {
      String Description "Quality Flag for UTC 1030";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_1030 {
      String Description "15 Minute Precipitation QC Flag for UTC 1030";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_1045 {
      String Description "15 Minute Precipitation Value for UTC 1045";
      String Units "mm";
   }
   quality_flag_1045 {
      String Description "Quality Flag for UTC 1045";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_1045 {
      String Description "15 Minute Precipitation QC Flag for UTC 1045";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_1100 {
      String Description "15 Minute Precipitation Value for UTC 1100";
      String Units "mm";
   }
   quality_flag_1100 {
      String Description "Quality Flag for UTC 1100";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_1100 {
      String Description "15 Minute Precipitation QC Flag for UTC 1100";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_1115 {
      String Description "15 Minute Precipitation Value for UTC 1115";
      String Units "mm";
   }
   quality_flag_1115 {
      String Description "Quality Flag for UTC 1115";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_1115 {
      String Description "15 Minute Precipitation QC Flag for UTC 1115";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_1130 {
      String Description "15 Minute Precipitation Value for UTC 1130";
      String Units "mm";
   }
   quality_flag_1130 {
      String Description "Quality Flag for UTC 1130";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_1130 {
      String Description "15 Minute Precipitation QC Flag for UTC 1130";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_1145 {
      String Description "15 Minute Precipitation Value for UTC 1145";
      String Units "mm";
   }
   quality_flag_1145 {
      String Description "Quality Flag for UTC 1145";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_1145 {
      String Description "15 Minute Precipitation QC Flag for UTC 1145";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_1200 {
      String Description "15 Minute Precipitation Value for UTC 1200";
      String Units "mm";
   }
   quality_flag_1200 {
      String Description "Quality Flag for UTC 1200";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_1200 {
      String Description "15 Minute Precipitation QC Flag for UTC 1200";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_1215 {
      String Description "15 Minute Precipitation Value for UTC 1215";
      String Units "mm";
   }
   quality_flag_1215 {
      String Description "Quality Flag for UTC 1215";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_1215 {
      String Description "15 Minute Precipitation QC Flag for UTC 1215";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_1230 {
      String Description "15 Minute Precipitation Value for UTC 1230";
      String Units "mm";
   }
   quality_flag_1230 {
      String Description "Quality Flag for UTC 1230";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_1230 {
      String Description "15 Minute Precipitation QC Flag for UTC 1230";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_1245 {
      String Description "15 Minute Precipitation Value for UTC 1245";
      String Units "mm";
   }
   quality_flag_1245 {
      String Description "Quality Flag for UTC 1245";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_1245 {
      String Description "15 Minute Precipitation QC Flag for UTC 1245";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_1300 {
      String Description "15 Minute Precipitation Value for UTC 1300";
      String Units "mm";
   }
   quality_flag_1300 {
      String Description "Quality Flag for UTC 1300";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_1300 {
      String Description "15 Minute Precipitation QC Flag for UTC 1300";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_1315 {
      String Description "15 Minute Precipitation Value for UTC 1315";
      String Units "mm";
   }
   quality_flag_1315 {
      String Description "Quality Flag for UTC 1315";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_1315 {
      String Description "15 Minute Precipitation QC Flag for UTC 1315";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_1330 {
      String Description "15 Minute Precipitation Value for UTC 1330";
      String Units "mm";
   }
   quality_flag_1330 {
      String Description "Quality Flag for UTC 1330";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_1330 {
      String Description "15 Minute Precipitation QC Flag for UTC 1330";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_1345 {
      String Description "15 Minute Precipitation Value for UTC 1345";
      String Units "mm";
   }
   quality_flag_1345 {
      String Description "Quality Flag for UTC 1345";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_1345 {
      String Description "15 Minute Precipitation QC Flag for UTC 1345";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_1400 {
      String Description "15 Minute Precipitation Value for UTC 1400";
      String Units "mm";
   }
   quality_flag_1400 {
      String Description "Quality Flag for UTC 1400";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_1400 {
      String Description "15 Minute Precipitation QC Flag for UTC 1400";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_1415 {
      String Description "15 Minute Precipitation Value for UTC 1415";
      String Units "mm";
   }
   quality_flag_1415 {
      String Description "Quality Flag for UTC 1415";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_1415 {
      String Description "15 Minute Precipitation QC Flag for UTC 1415";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_1430 {
      String Description "15 Minute Precipitation Value for UTC 1430";
      String Units "mm";
   }
   quality_flag_1430 {
      String Description "Quality Flag for UTC 1430";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_1430 {
      String Description "15 Minute Precipitation QC Flag for UTC 1430";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_1445 {
      String Description "15 Minute Precipitation Value for UTC 1445";
      String Units "mm";
   }
   quality_flag_1445 {
      String Description "Quality Flag for UTC 1445";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_1445 {
      String Description "15 Minute Precipitation QC Flag for UTC 1445";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_1500 {
      String Description "15 Minute Precipitation Value for UTC 1500";
      String Units "mm";
   }
   quality_flag_1500 {
      String Description "Quality Flag for UTC 1500";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_1500 {
      String Description "15 Minute Precipitation QC Flag for UTC 1500";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_1515 {
      String Description "15 Minute Precipitation Value for UTC 1515";
      String Units "mm";
   }
   quality_flag_1515 {
      String Description "Quality Flag for UTC 1515";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_1515 {
      String Description "15 Minute Precipitation QC Flag for UTC 1515";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_1530 {
      String Description "15 Minute Precipitation Value for UTC 1530";
      String Units "mm";
   }
   quality_flag_1530 {
      String Description "Quality Flag for UTC 1530";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_1530 {
      String Description "15 Minute Precipitation QC Flag for UTC 1530";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_1545 {
      String Description "15 Minute Precipitation Value for UTC 1545";
      String Units "mm";
   }
   quality_flag_1545 {
      String Description "Quality Flag for UTC 1545";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_1545 {
      String Description "15 Minute Precipitation QC Flag for UTC 1545";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_1600 {
      String Description "15 Minute Precipitation Value for UTC 1600";
      String Units "mm";
   }
   quality_flag_1600 {
      String Description "Quality Flag for UTC 1600";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_1600 {
      String Description "15 Minute Precipitation QC Flag for UTC 1600";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_1615 {
      String Description "15 Minute Precipitation Value for UTC 1615";
      String Units "mm";
   }
   quality_flag_1615 {
      String Description "Quality Flag for UTC 1615";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_1615 {
      String Description "15 Minute Precipitation QC Flag for UTC 1615";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_1630 {
      String Description "15 Minute Precipitation Value for UTC 1630";
      String Units "mm";
   }
   quality_flag_1630 {
      String Description "Quality Flag for UTC 1630";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_1630 {
      String Description "15 Minute Precipitation QC Flag for UTC 1630";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_1645 {
      String Description "15 Minute Precipitation Value for UTC 1645";
      String Units "mm";
   }
   quality_flag_1645 {
      String Description "Quality Flag for UTC 1645";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_1645 {
      String Description "15 Minute Precipitation QC Flag for UTC 1645";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_1700 {
      String Description "15 Minute Precipitation Value for UTC 1700";
      String Units "mm";
   }
   quality_flag_1700 {
      String Description "Quality Flag for UTC 1700";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_1700 {
      String Description "15 Minute Precipitation QC Flag for UTC 1700";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_1715 {
      String Description "15 Minute Precipitation Value for UTC 1715";
      String Units "mm";
   }
   quality_flag_1715 {
      String Description "Quality Flag for UTC 1715";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_1715 {
      String Description "15 Minute Precipitation QC Flag for UTC 1715";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_1730 {
      String Description "15 Minute Precipitation Value for UTC 1730";
      String Units "mm";
   }
   quality_flag_1730 {
      String Description "Quality Flag for UTC 1730";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_1730 {
      String Description "15 Minute Precipitation QC Flag for UTC 1730";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_1745 {
      String Description "15 Minute Precipitation Value for UTC 1745";
      String Units "mm";
   }
   quality_flag_1745 {
      String Description "Quality Flag for UTC 1745";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_1745 {
      String Description "15 Minute Precipitation QC Flag for UTC 1745";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_1800 {
      String Description "15 Minute Precipitation Value for UTC 1800";
      String Units "mm";
   }
   quality_flag_1800 {
      String Description "Quality Flag for UTC 1800";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_1800 {
      String Description "15 Minute Precipitation QC Flag for UTC 1800";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_1815 {
      String Description "15 Minute Precipitation Value for UTC 1815";
      String Units "mm";
   }
   quality_flag_1815 {
      String Description "Quality Flag for UTC 1815";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_1815 {
      String Description "15 Minute Precipitation QC Flag for UTC 1815";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_1830 {
      String Description "15 Minute Precipitation Value for UTC 1830";
      String Units "mm";
   }
   quality_flag_1830 {
      String Description "Quality Flag for UTC 1830";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_1830 {
      String Description "15 Minute Precipitation QC Flag for UTC 1830";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_1845 {
      String Description "15 Minute Precipitation Value for UTC 1845";
      String Units "mm";
   }
   quality_flag_1845 {
      String Description "Quality Flag for UTC 1845";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_1845 {
      String Description "15 Minute Precipitation QC Flag for UTC 1845";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_1900 {
      String Description "15 Minute Precipitation Value for UTC 1900";
      String Units "mm";
   }
   quality_flag_1900 {
      String Description "Quality Flag for UTC 1900";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_1900 {
      String Description "15 Minute Precipitation QC Flag for UTC 1900";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_1915 {
      String Description "15 Minute Precipitation Value for UTC 1915";
      String Units "mm";
   }
   quality_flag_1915 {
      String Description "Quality Flag for UTC 1915";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_1915 {
      String Description "15 Minute Precipitation QC Flag for UTC 1915";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_1930 {
      String Description "15 Minute Precipitation Value for UTC 1930";
      String Units "mm";
   }
   quality_flag_1930 {
      String Description "Quality Flag for UTC 1930";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_1930 {
      String Description "15 Minute Precipitation QC Flag for UTC 1930";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_1945 {
      String Description "15 Minute Precipitation Value for UTC 1945";
      String Units "mm";
   }
   quality_flag_1945 {
      String Description "Quality Flag for UTC 1945";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_1945 {
      String Description "15 Minute Precipitation QC Flag for UTC 1945";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_2000 {
      String Description "15 Minute Precipitation Value for UTC 2000";
      String Units "mm";
   }
   quality_flag_2000 {
      String Description "Quality Flag for UTC 2000";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_2000 {
      String Description "15 Minute Precipitation QC Flag for UTC 2000";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_2015 {
      String Description "15 Minute Precipitation Value for UTC 2015";
      String Units "mm";
   }
   quality_flag_2015 {
      String Description "Quality Flag for UTC 2015";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_2015 {
      String Description "15 Minute Precipitation QC Flag for UTC 2015";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_2030 {
      String Description "15 Minute Precipitation Value for UTC 2030";
      String Units "mm";
   }
   quality_flag_2030 {
      String Description "Quality Flag for UTC 2030";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_2030 {
      String Description "15 Minute Precipitation QC Flag for UTC 2030";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_2045 {
      String Description "15 Minute Precipitation Value for UTC 2045";
      String Units "mm";
   }
   quality_flag_2045 {
      String Description "Quality Flag for UTC 2045";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_2045 {
      String Description "15 Minute Precipitation QC Flag for UTC 2045";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_2100 {
      String Description "15 Minute Precipitation Value for UTC 2100";
      String Units "mm";
   }
   quality_flag_2100 {
      String Description "Quality Flag for UTC 2100";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_2100 {
      String Description "15 Minute Precipitation QC Flag for UTC 2100";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_2115 {
      String Description "15 Minute Precipitation Value for UTC 2115";
      String Units "mm";
   }
   quality_flag_2115 {
      String Description "Quality Flag for UTC 2115";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_2115 {
      String Description "15 Minute Precipitation QC Flag for UTC 2115";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_2130 {
      String Description "15 Minute Precipitation Value for UTC 2130";
      String Units "mm";
   }
   quality_flag_2130 {
      String Description "Quality Flag for UTC 2130";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_2130 {
      String Description "15 Minute Precipitation QC Flag for UTC 2130";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_2145 {
      String Description "15 Minute Precipitation Value for UTC 2145";
      String Units "mm";
   }
   quality_flag_2145 {
      String Description "Quality Flag for UTC 2145";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_2145 {
      String Description "15 Minute Precipitation QC Flag for UTC 2145";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_2200 {
      String Description "15 Minute Precipitation Value for UTC 2200";
      String Units "mm";
   }
   quality_flag_2200 {
      String Description "Quality Flag for UTC 2200";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_2200 {
      String Description "15 Minute Precipitation QC Flag for UTC 2200";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_2215 {
      String Description "15 Minute Precipitation Value for UTC 2215";
      String Units "mm";
   }
   quality_flag_2215 {
      String Description "Quality Flag for UTC 2215";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_2215 {
      String Description "15 Minute Precipitation QC Flag for UTC 2215";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_2230 {
      String Description "15 Minute Precipitation Value for UTC 2230";
      String Units "mm";
   }
   quality_flag_2230 {
      String Description "Quality Flag for UTC 2230";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_2230 {
      String Description "15 Minute Precipitation QC Flag for UTC 2230";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_2245 {
      String Description "15 Minute Precipitation Value for UTC 2245";
      String Units "mm";
   }
   quality_flag_2245 {
      String Description "Quality Flag for UTC 2245";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_2245 {
      String Description "15 Minute Precipitation QC Flag for UTC 2245";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_2300 {
      String Description "15 Minute Precipitation Value for UTC 2300";
      String Units "mm";
   }
   quality_flag_2300 {
      String Description "Quality Flag for UTC 2300";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_2300 {
      String Description "15 Minute Precipitation QC Flag for UTC 2300";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_2315 {
      String Description "15 Minute Precipitation Value for UTC 2315";
      String Units "mm";
   }
   quality_flag_2315 {
      String Description "Quality Flag for UTC 2315";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_2315 {
      String Description "15 Minute Precipitation QC Flag for UTC 2315";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_2330 {
      String Description "15 Minute Precipitation Value for UTC 2330";
      String Units "mm";
   }
   quality_flag_2330 {
      String Description "Quality Flag for UTC 2330";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_2330 {
      String Description "15 Minute Precipitation QC Flag for UTC 2330";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   precipitation_amount_2345 {
      String Description "15 Minute Precipitation Value for UTC 2345";
      String Units "mm";
   }
   quality_flag_2345 {
      String Description "Quality Flag for UTC 2345";
      String Values "0-No qualifier required; 1-Accumulation period; 2-End of Accumulation period; 3-Deleted value. Original data was unreadable.; 4-Trace of precipitation.; 5-Probable amount as a result of melting frozen precipitation.; 6-Suspect amount relative to time or period of occurrence.; 7-Missing value";
   }
   precipitation_amount_flag_2345 {
      String Description "15 Minute Precipitation QC Flag for UTC 2345";
      String Values "U-Unchecked; G-Good; M-Normally recorded but missing; D-Questionable; B-Unlikely; N-Not available or Not observed; X-Glitch; E-Estimated; C-Reported value exceeds output format field size or was negative precipitation; T-Trace precipitation amount recorded; I-Derived parameter can not be computed due to insufficient data.";
   }
   Global {
       String Title "Precipitation (15-minute) Multi-Network Composite";
       String Project "BAMEX: Bow Echo and Mesoscale Convective Vortex Experiment 2003";
       String Description "The BAMEX Fifteen Minute Precipitation Composite contains National Climatic Data Center Fifteen Minute Precipitation data and the 15 minute precipitation extracted from the BAMEX 2003 Mesonet: 1-minute Surface Meteorological Composite. Data from these sources were quality controlled to form this precipitation composite.";
       String Start_Date_Time "2003-05-22 00:00:00";
       String End_Date_Time "2003-05-22 23:59:59";
       Url Documentation https://data.eol.ucar.edu/datafile/nph-get/85.117/bamex_pcp15c.html;
       String Detailed_Description "The BAMEX 2003 Fifteen Minute Precipitation Composite contains seven metadata parameters and three data parameters. The metadata parameters describe the date/time, network, station and location at which the data were collected. The three data parameters repeat once for each fifteen minutes from UTC 0000 through UTC hour 2345. Data reported for a designated 15-minute observation represents data collected during the previous 15-minutes. All times are reported in UTC, and precipitation data values are reported in millimeters. A Quality Flag and a QC Code are assigned to each data value during the University Corporation for Atmospheric Research/Joint Office for Science Support (UCAR/JOSS) quality control processing.";
       String Processing_Version "1.0";
       String Release_Date "2011-08-24";
       String Author "EOL Data Support <eol-datahelp at ucar dot edu>";
       String References "DOI: https://doi.org/10.5065/D6PK0DFS";
       String Institution "National Center for Atmospheric Research - Earth Observing Laboratory";
       Url Creator_URL http://www.eol.ucar.edu;
       String Creator_Email "eol-datahelp at ucar dot edu";
   }
}
