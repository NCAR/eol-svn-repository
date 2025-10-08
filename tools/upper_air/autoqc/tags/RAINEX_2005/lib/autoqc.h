/**
 * This contains the structures and definitions used by the auto qc program
 * for QC'ing sounding files in CLASS format.
 *
 * @author Joel Clawson - 2005/01/19: Refactored to be easier to use and read.
 **/
#define HOT        1
#define COLD       0

#define MAX_OB 15000
#define LINE_LEN 512

typedef struct class_node {
  float time;
  float press;
  float temp;
  float dewpt;
  float rh;
  float u_comp;
  float v_comp;
  float wind_spd;
  float wind_dir;
  float asc_rate;
  float lon;
  float lat;
  float elev;
  float azim;
  float alt;
  float press_flag;
  float temp_flag;
  float rh_flag;
  float uwind_flag;
  float vwind_flag;
  float asc_rate_flag;
} *CLASSPTR;

typedef class_node CLASS_ARRAY[MAX_OB];

// General control functions
void check_max_min(ofstream&,CLASS_ARRAY,int);
void check_rates(ofstream&,CLASS_ARRAY,int);
streampos copyheader(ifstream&,ofstream&,char*);
void helpscreen(char*);
int initarray(ifstream&,ofstream&,streampos,CLASS_ARRAY);
void mark_flags(CLASSPTR,int,int,int,int,int,int,int);

// Functions for NWS QC
void nws_check(ofstream&,CLASS_ARRAY,int);
float nws_check_ascension_rate_change(ofstream&,CLASS_ARRAY,int,int,float,int);

// Checks for field widths for initarray
void check_altitude_field(class_node&,class_node&,ofstream&);
void check_ascension_rate_field(class_node&,class_node&,ofstream&);
void check_dew_point_field(class_node&,class_node&,ofstream&);
void check_elevation_azimuth_fields(class_node&,class_node&,ofstream&);
void check_latitude_field(class_node&,class_node&,ofstream&);
void check_lontitude_field(class_node&,class_node&,ofstream&);
void check_pressure_field(class_node&,class_node&,int,ofstream&);
void check_relative_humidity_field(class_node&,class_node&,ofstream&);
void check_temperature_field(class_node&,class_node&,ofstream&);
void check_time_field(class_node&,class_node&,ofstream&);
void check_uv_fields(class_node&,class_node&,ofstream&);
void check_wind_fields(class_node&,class_node&,ofstream&);

// Checks for values to be in min/max range for check_min_max
void check_altitude_range(class_node&,ofstream&);
void check_ascension_rate_range(class_node&,ofstream&);
void check_dew_point_range(class_node&,ofstream&);
void check_pressure_range(class_node&,int,ofstream&);
void check_relative_humidity_range(class_node&,ofstream&);
void check_temperature_range(class_node&,ofstream&);
void check_time_range(class_node&,ofstream&);
void check_uwind_range(class_node&,ofstream&);
void check_vwind_range(class_node&,ofstream&);
void check_wind_direction_range(class_node&,ofstream&);
void check_wind_speed_range(class_node&,ofstream&);

// Helper functions for check_rate
int decrease_pressure_check(ofstream&,CLASS_ARRAY,int);
float decreasing_time_check(ofstream&,class_node&,class_node&,float);
float differential_ascension_rate(ofstream&,CLASS_ARRAY,int,int,float,int);
int getAveragingValue();
void increasing_altitude_check(ofstream&,class_node&,class_node&,int);
float increasing_time_check(ofstream&,class_node&,class_node&,float);
void rapid_pressure_check(ofstream&,CLASS_ARRAY,int,int);
void rapid_temp_check(ofstream&,CLASS_ARRAY,int,int,float,float);
void rapid_temperature_check(ofstream&,CLASS_ARRAY,int,int);
void rapid_temperature_for_cold_latitudes(ofstream&,CLASS_ARRAY,int,int,float,float);
void rapid_temperature_for_hot_latitudes(ofstream&,CLASS_ARRAY,int,int);
void super_adiabatic_lapse_rate_check(ofstream&,CLASS_ARRAY,int,int,float,float);






