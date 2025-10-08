/**
 * The nws_vaisala.cpp file contains the functions for the NWS conversion
 * that are unique for the Vaisala processing.  It contains functions for
 * reading in the meteorological and angle data, along with the stub for
 * the humidity correction function.
 *
 * To compile, see the maek file (Makefile) in this directory.
 *
 * @author Joel Clawson
 *
 * The nws_vaisala.cpp file is from the nwsVaisala2000.c file that was 
 * changed as part of the refactoring of the entire NWS software set.  It
 * contains functionality that is only used by the Vaisala conversion.  
 * Functions that were the same as the VIZ conversion were moved to the nws.cpp
 * file.  The fefactoring included renaming and moving functions, changing 
 * variable names to become more readable (and understandable), splitting 
 * functions into component pieces, and simplifying functions.
 *
 * NOTE: The correct_humidity function is only a stub.  The Vaisala conversion
 * no longer has a humidity correction.  The stub is needed because the 
 * function prototype is in the nws.h header file.
 *
 * CHANGES:
 *   - Changed the reading of data to use the C++ string class instead of
 * temporary char arrays and C string functions.
 *
 *******************************************************************************
 * The following documentation is from the pre-refactored code.  Changes have
 * been made to match the new function names where applicable.  The changes that
 * were made to both the Vaisala and VIZ conversions and were in functions that
 * were moved to the nws.cpp are in that file.  - Joel Clawson
 *******************************************************************************
 *
 * @author ?? Darren Gallant Dec 2000
 *
 * CHANGES:
 *   - Changed logic in read_met_data and read_ang_data (read_angle_data) to 
 * use sscanf to input meteorological or angle data instead of strcpy and strncat.
 *   - Changed read_met_data to not read when "D/R" or dropping indicator occurs
 * in data line.  (VIZ code already has this feature).
 * 
 * ---------------------------------------------------------------------------
 *
 * @author ?? Darren Gallant Mar 1999
 *
 * CHANGES:
 *   - Added logic in read_met_data to prevent reading in raw met data.
 * 
 * ---------------------------------------------------------------------------
 *
 * @author ?? Darren Gallant Aug 1996
 *
 * CHANGES:
 *   - modified for Vaisala sounding which require no humidity corrections
 *   - modified for differences in ASCII file format
 * 
 * ---------------------------------------------------------------------------
 *
 * @author Kendall Southwick May 1994
 *
 * CHANGES:
 *   - This program will convert NWS ASCII version of store files into OCF (OFPS
 * CLASSS Format).
 **/
#include <iostream.h>
#include <fstream.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include "nws.h"
#include <time.h>

#include <string>

/**
 * At this time, there is no humidity correction.
 **/
void correct_humidity(SOUND_DATA_ARR data, float rh33_res, int nom_rel_year, 
		      int nom_rel_month, int size){}

/**
 * Read in the meteorological data section of the raw data file.
 * @param in_file The input stream containing the raw data.
 * @param log_file The output stream for errors.
 * @param a The array to store the data.
 * @param term_alt The termination altitude of the sounding.
 * @return The number of observations read.
 **/
int read_met_data(ifstream& in_file, ofstream& log_file, SOUND_DATA_ARR a,float term_alt) {
  char line[LINE_LEN+1];

  int ret_val = 0;
  int read_flag = 0;

  int done = 0;
  while(!done && !in_file.eof()) {

    in_file.getline(line,LINE_LEN,'\n');

    read_flag = 1;

    if (strlen(line) <= 5 || strstr(line,"D/R") != '\0' ||
	strstr(line,"6 Second Met Data") != '\0' ||  strcspn(&line[strspn(line," ")]," ") < 3) {
      read_flag = 0;
    }
    
    else if (strstr(line,"6-second Raw Data") != '\0' ||
	     strstr(line,"Position Data") != '\0') {
      read_flag = 0;
      done = 1;
    }
    
    else if (strstr(line,"Time") != '\0' || strstr(line,"TIME") != '\0') {
      read_flag = 0;
    }
    
    if(read_flag) {
      string data_line = line;

      a[ret_val].time = 60 * atof(data_line.substr(0,7).c_str());
      a[ret_val].alt = atof(data_line.substr(7,11).c_str());
      a[ret_val].press = atof(data_line.substr(18,13).c_str());
      if (strstr(data_line.substr(31,12).c_str(),"//") == '\0') {
	a[ret_val].temp = atof(data_line.substr(31,12).c_str());
      }
      if (strstr(data_line.substr(46,6).c_str(),"//") == '\0') {
	a[ret_val].rh = atof(data_line.substr(46,6).c_str());
      }
      int temp_interp = (strstr(data_line.substr(43,3).c_str(),"I") != '\0') ? 1 : 0;
      int rh_interp   = (strlen(line) > 52 && 
			 strstr(data_line.substr(52,3).c_str(),"I") != '\0') ? 1 : 0;

      check_met_values(log_file,&a[ret_val],temp_interp,rh_interp,term_alt);
      check_met_flags(&a[ret_val]);

      if(a[ret_val].alt >= (term_alt- 75.0) && a[ret_val].alt != 99999.0) {
	read_flag = 0;
      } else {
	ret_val++;
      }
    }
  }
  return ret_val;
}

/**
 * Read in the angle data section of the raw data file.
 * @param in_file The input stream containing the raw data.
 * @param a The array to contain the data.
 * @return The number of angle observations read.
 **/
int read_angle_data(ifstream& in_file, SOUND_DATA_ARR a) {

  char line[LINE_LEN+1];

  int ret_val = 0;
  int read_flag = 0;

  int done = 0;
  while(!done && !in_file.eof()) {

    if(strlen(line) <= 5 || strstr(line,"Position Data") != '\0') {
      read_flag = 0;
    }

    if(read_flag) {
      string data_line = line;

      a[ret_val].time = 60.0 * atof(data_line.substr(0,5).c_str());
      a[ret_val].elev = (strstr(data_line.substr(5,17).c_str(),"////") == '\0') ? 
	atof(data_line.substr(5,17).c_str()) : 999.0;
      a[ret_val].azim = (strstr(data_line.substr(22,15).c_str(),"////") == '\0') ?
	atof(data_line.substr(22,15).c_str()) : 999.0;

      if (a[ret_val].elev <= 0.0 || a[ret_val].elev > 90.0 ||
	  a[ret_val].azim < 0.0 || a[ret_val].azim > 360.0 ||
	  a[ret_val].press == 9999.0 && a[ret_val].alt == 99999.0) {
        a[ret_val].elev = 999.0;
        a[ret_val].azim = 999.0;
      }

      ret_val++;
    }

    if (strstr(line,"Time") != '\0' || strstr(line,"TIME") != '\0') { read_flag = 1; }
    if (strstr(line,"6-second Raw Data") != '\0') { done = 1; }
    
    in_file.getline(line,LINE_LEN,'\n');
  }

  return(ret_val);
}
