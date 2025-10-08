/**
 * The nws.h file contains the preprocessor definitions, the structures, and
 * the function prototypes used by the NWS conversion software.  It is used
 * by all of the files that work to the conversion.
 *
 * To compile, see the make file (Makefile) in the ../src directory.
 *
 * @author Joel Clawson Apr 2005
 *
 * The nws.h file si a new file that was created during the refactoring
 * for the NWS software.  It contains every function prototype, preprocessor
 * definition that is not passed in as a parameter at compile time, and
 * the HEADER and OBSER structures.  This was originally called sounding_def.h.
 *
 * CHANGES:
 *   - Added preprocessor types.  This is to allow certain functions to be 
 * used by both the Vaisala and VIZ conversions without having to do nearly 
 * identical functions in their own file.
 *   - Create the HEADER structure to be used to hold all of the header data to
 * allow the passing of a single HEADER struct instead of several individual
 * parameters to the functions.
 *   - Moved all of the prototypes from the .c (new .cpp) files to the header
 * file to keep all of the prototypes together.
 *
 *******************************************************************************
 * The follwing docurmentation is from the pre-refactored code.  -- Joel Clawson
 *******************************************************************************
 *
 * @author Kendall Southwick May 1994 ??
 *
 * A file with some definitions for using in C++ programs to convert soundings.
 **/
#ifndef NWS_H
#define NWS_H
#endif

#include <fstream.h>
#include <iostream.h>

#include "matrix.hpp"

#define MAX_OB 2500
#define LINE_LEN 256
#define MAX_ALT_CHG 40
#define MAX_PRESS_CHG 4

#define ORDER 9
#define THRESHOLD 1.5
#define UPPER_NOTCH_LIMIT 190.0
#define LOWER_NOTCH_LIMIT 90.0
#define END_SPACE 40
#define START_SPACE 40

#define VAISALA_TYPE 0
#define VIZ_TYPE 1

#ifdef VIZ
#define TYPE VIZ_TYPE
#define HUM_COR_FLAG 1
#else
#define TYPE VAISALA_TYPE
#define HUM_COR_FLAG 0
#endif


/**
 * The structure for holding a single reading for a CLASS file.
 **/
typedef struct obser_node {
  float time;       // The time of the observation (s)
  float press;      // Pressure (mb)
  float temp;       // Temperature (deg C)
  float dewpt;      // Dew Point (deg C)
  float rh;         // Relative Humidity (%)
  float u_cmp;      // U Wind Component (m/s)
  float v_cmp;      // V Wind Component (m/s)
  float wind_spd;   // Wind Speed (m/s)
  float wind_dir;   // Wind Direction (m/s)
  float z_cmp;      // Ascension Rate (m/s)
  float lon;        // Longitude (deg)
  float lat;        // Latitude (deg)
  float elev;       // Elevation Angle (deg)
  float azim;       // Azimuth Angle (deg)
  float alt;        // Altitude (m)
  float qp;         // Pressure Flag
  float qt;         // Temperature Flag
  float qrh;        // Relative Humidity Flag
  float qu;         // U Wind Component Flag
  float qv;         // V Wind Component Flag
  float qz;         // Ascension Rate Flag
} OBSER;

/**
 * The structure for holding the header data for a CLASS file.
 **/
typedef struct header {
  float lat;               // The latitude at the release site (deg)
  float lon;               // The longitude at the release site (deg)
  float elev;              // The elevation at the release site (deg)
  int rel_year;            // The actual release year
  int rel_month;           // The actual release month
  int rel_day;             // The actual release day
  int rel_hour;            // The actual release hour
  int rel_min;             // The actual release minute
  int nom_rel_year;        // The nominal release year
  int nom_rel_month;       // The nominal release month
  int nom_rel_day;         // The nominal release day
  int nom_rel_hour;        // The nominal release hour
  char call_letters[5];    // The call letters for the station
  char site_name[28];      // The name of the station
  int ascen_num;           // The ascension number of the sounding.
  char sonde_type[12];     // The type of the released sounding.
  char serial_number[20];  // The serial number of the sounding.
  float rh33_res;          // A relative humidity resolution value ??
  float term_alt;          // The altitude when the sounding stopped.
  int index;               // The unique index for the station.
} HEADER;

/**
 * The definition for an array of sounding observations.
 **/
typedef OBSER SOUND_DATA_ARR[MAX_OB];


void calculate_ascension_rates(ofstream& logfile,SOUND_DATA_ARR data);
void calculate_dew_point(SOUND_DATA_ARR data);
void calculate_lat_and_lon(SOUND_DATA_ARR data, double xs[MAX_OB], 
			   double ys[MAX_OB],float lon,float lat,
			   int qxy[MAX_OB]);
void calculate_position(double elev_smooth[MAX_OB], double azim_smooth[MAX_OB],
			double alt[MAX_OB], double x_pos[MAX_OB],
			double y_pos[MAX_OB], int qxy[MAX_OB], int size);
void calculate_uv_components(ofstream& logfile, SOUND_DATA_ARR data, 
			     double x_pos[MAX_OB], double y_pos[MAX_OB], 
			     double xs[MAX_OB], double ys[MAX_OB], 
			     int qxy[MAX_OB], int angle_status[MAX_OB], 
			     int size);
void calculate_wind_speed_and_direction(SOUND_DATA_ARR data);
void check_met_flags(OBSER *node);
void check_met_values(ofstream& logfile, OBSER *data, int temp_interp, 
		      int rh_interp);
int correct_call_letters(HEADER *header);
void correct_humidity(SOUND_DATA_ARR data, float rh33_res, int nom_rel_year,
		      int nom_rel_month, int size);
void initialize_data(SOUND_DATA_ARR data, HEADER *header);
matrix matrix_solver(matrix a, matrix b);
int read_angle_data(ifstream& infile, SOUND_DATA_ARR data);
int read_calibration_data(ifstream& infile, HEADER *header);
int read_header(ifstream& infile, ofstream& outfile, ofstream& logfile, 
		char* filename,double limit_angle[360], float *wind_spd, 
		float *wind_dir, HEADER *header);
int read_limiting_angles(ifstream& infile, double lim_ang[360]);
int read_met_data(ifstream& infile, ofstream& logfile, SOUND_DATA_ARR data,
		  float term_alt);
int read_pre_administration_data(ifstream& infile, HEADER *header);
int read_pre_flight_data(ifstream& infile, HEADER *header);
int read_release_time(ifstream& infile, HEADER *header);
int read_station_data(ifstream& infile, HEADER *header);
int read_surface_winds(ifstream& infile, float **wind_spd, float **wind_dir);
int read_termination_summary(ifstream& infile, HEADER *header);
void smooth_angle_list(double work_array[MAX_OB], double smooth[MAX_OB],
		 double time[MAX_OB], int size, int angle_status[MAX_OB],
		 double limit_angle[360], SOUND_DATA_ARR data, int azim_flag);
void smooth_azimuth_angles(SOUND_DATA_ARR data, double lim_ang[360], int size,
			   double azim_smooth[MAX_OB], 
			   int angle_status[MAX_OB]);
void smooth_elevation_angles(SOUND_DATA_ARR data, double lim_ang[360],int size,
			     double elev_smooth[MAX_OB], 
			     int angle_status[MAX_OB]);
void write_class_data(ofstream& outfile, SOUND_DATA_ARR data, int size);
int write_header_data(ofstream& outfile, ofstream& logfile, HEADER *header);
