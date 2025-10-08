/**
 * The nws_viz.cpp file contains the functions for the NWS conversions
 * that are unique for the VIZ processing.  It contains functions for
 * reading in the meteorological and angle data, along with the algorithm
 * to process the humidity corrections.
 *
 * To compire, see the make file (Makefile) in this directory.
 *
 * @author Joel Clawson Apr 2005
 *
 * The nws_viz.cpp file is from the nwsVIZ2000.c file that was changed as
 * part of the refactoring of the entire NWS software set.  It contains
 * functionality that is only used by the VIZ conversion.  Functions that
 * were the same as the Vaisala conversion were moved to the nws.cpp file.
 * The refactoring included renaming and moving functions, changing variable
 * names to become more readable (and understandable), splitting functions 
 * into component pieces, and simplifying functions.
 *
 * NOTE: The correct_humidity function has only been renamed and has not
 * been refactored.  This is because it is a complex algorithm and there
 * was not any test data that needed the humidity correction at the time of
 * the refactoring.
 *
 * CHANGES:
 *   - Changed the reading of data to use the C++ string class instead of 
 * temporary char arrays and C string functions.
 *
 *******************************************************************************
 * The following documentation is from the pre-refactored code.  Changes have
 * been made to match the new function names where applicable.  The changes that
 * were made to both the Vaisala and VIZ conversions and were in functions that
 * were moved to nws.cpp are in that file.  - Joel Clawson
 *******************************************************************************
 *
 * @author ?? Darren Gallant May 1999
 *
 * CHANGES:
 *   - If term_alt = 0, stop reading met data upon first dropping flag (i.e. 
 * 'D/R') encountered only datum with non-missing altitudes kept.
 * 
 * ---------------------------------------------------------------------------
 *
 * @author ?? Darren Gallant Oct 1998
 *
 * CHANGES:
 *   - Using strstr instead of strncmp when checking for "D/R" within 6 second
 * data section.  "D/R" flag changes position within the same ASCII file.
 * 
 * ---------------------------------------------------------------------------
 *
 * @author ?? Darren Gallant Jul 1998
 *
 * CHANGES:
 *   - Another change involving NULL INT, MISSING AZ, and MISSING RNG.  Previous 
 * logic in read_ang_data (read_angle_data) skipped these lines entirely (i.e.
 * ret_val not incremented).  New logic allows missing values for azimuth and 
 * elevation while incrementing ret_val.
 * 
 * ---------------------------------------------------------------------------
 *
 * @author ?? Darren Gallant Jun 1998
 *
 * CHANGES:
 *   - Created non SD VIZ source code by removing Space Data RH correction 
 * subroutine.
 *   - Fixed problem when NULL INT and MIXXING AZ appear in elevation and azimuth
 * fields in read_ang_data (read_angle_data) subroutine.  When either string 
 * appears in a line, a[ret_val].azim and a[ret_val].elev remain missing.  This
 * solution prevents an infinite loop.
 * 
 * ---------------------------------------------------------------------------
 *
 * @author ?? Darrent Gallant Dec 1996
 *
 * CHANGES:
 *   - Changed procedures read_met_data and read_ang_data (read_angle_data) to 
 * use strstr instead of strspn and strncmp to bettern handle spaces in data arrays.
 *   - Added if-else in read_ang_data (read_angle_data) under if (read_flag) to
 * skip past lines containing times but no data, setting azimuth and elevation 
 * angles to missing.
 *   - Assume 5 < strlen(line) < 17 contain no azimuth or elevation angle data and
 * no 00 in slant range field.
 *   - When time and no data but the slant range field is non-empty, no problems
 * occurred.
 *   - Left cout line for error analysis
 * 
 * ---------------------------------------------------------------------------
 *
 * @author Darren Gallant Nov 1996
 *
 * CHANGES:
 *   - Changed header elevation from integer to float.
 * 
 * ---------------------------------------------------------------------------
 *
 * @author Kendall Southwick May 1994
 *
 * This program will convert NWS ASCII version of store file into OCF (OFPS CLASS
 * Format).
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
 * Correct the humidity values.
 * @param sd The array of sounding data.
 * @param rh33_res 
 * @param nom_rel_year The nominal release year of the sounding.
 * @param nom_rel_month The nominal release month of the sounding.
 * @param npts The size of the data array.
 **/
void correct_humidity(SOUND_DATA_ARR sd, float rh33_res, int nom_rel_year,
		      int nom_rel_month, int npts) {

  int j;
  double rh, rh_last, rh_old, res, res_ratio, res_total;
  double rht;
  double interval, f_r, temp;
  char line[50];

  cout << "VIZ humidity correction." << endl;

  for(int i = 1; i < npts; i++) {

    if(sd[i].temp != 999.0 && sd[i].rh != 999.0) {

      // Intial values for iteration.

      j = 0;

      rh = -10.0;
      rh_last = rh_old = -10.0;
      temp = sd[i].temp;

      if (sd[i].rh > 90.0) {
        res_ratio = 500.0;
        interval = 20.0;
      } else if (sd[i].rh > 80.0) {
        res_ratio = 100.0;
        interval = 5.0;
      } else if (sd[i].rh > 70.0) {
        res_ratio = 50.0;
        interval = 5.0;
      } else if (sd[i].rh > 60.0) {
        res_ratio = 10.0;
        interval = 1.0;
      } else if (sd[i].rh > 30.0) {
        res_ratio = 1.0;
        interval = 0.5;
      } else if (sd[i].rh > 15.0) {
        res_ratio = 0.5;
        interval = 0.1;
      } else {
        res_ratio = 0.2;
        interval = 0.05;
      }

// Loop for iteration.

		       while((fabs(rh-(double)(sd[i].rh))>0.00001) && (j<5000)) {

// Calulate f(r) ie. f_r.

        if(res_ratio>=1.0) {
          f_r = (7.885e-1)+(9.286e-3)*temp+(-2.462e-5)*temp*temp;
          f_r += (-3.368e-7)*temp*temp*temp;
          f_r = f_r*log(res_ratio);
	} else {
          f_r = (9.243e-1)+(3.059e-3)*temp+(-1.188e-6)*temp*temp;
          f_r = f_r*log(res_ratio);
	}

        if (f_r >= -0.2) { // The 1A coeff.
           rh = 102.0 -(69.0/(1.000+(7.290e-1)*f_r+(-5.580e-2)*f_r*f_r+
                (7.480e-3)*f_r*f_r*f_r+(1.010e-2)*f_r*f_r*f_r*f_r));
	} else { // The 1B coeff.
	   rh = 0.0 -(69.0/(-2.440+(-7.370)*f_r+(-9.980e1)*f_r*f_r+
                (-2.514e2)*f_r*f_r*f_r+(-2.110e2)*f_r*f_r*f_r*f_r));
	}

        if((fabs(rh-rh_last)<0.001) || (fabs(rh-rh_old)<0.001)) {
          interval = interval*0.95;
	}
 
        rh_old = rh_last;
        rh_last = rh;

        if(fabs(rh-(double)(sd[i].rh))>0.00001) {
          if(rh < sd[i].rh) {
            res_ratio += interval;
          } else {
            res_ratio -= interval;
	  }
	}

        if(res_ratio <=0.0) {
          interval = interval *0.95;
          res_ratio = interval;
	}
        j++;

      } // Iterate.

// If the flight is before Oct 1 1993, correct the parallel res. problem.

      if((nom_rel_year <= 92) ||((nom_rel_year == 93)&&(nom_rel_month <10)))
      { // Do parallel corection.
	res_total = (res_ratio*rh33_res*(1.2e6))/(res_ratio*rh33_res+(1.2e6));
        res = (res_total*(1.0e6))/((1.0e6)-res_total);
        res_ratio = res/rh33_res;
      } // Do parallel corection.

// Use the 1a coeff to recompute the rh with the "correct" res_ratio.
      if(res_ratio < 0.0)
        res_ratio = fabs(res_ratio);

      if(res_ratio >= 1.0){
        f_r = (7.885e-1)+(9.286e-3)*temp+(-2.462e-5)*temp*temp;
        f_r += (-3.368e-7)*temp*temp*temp;
        f_r = f_r*log(res_ratio);
      } else {
        f_r = (9.243e-1)+(3.059e-3)*temp+(-1.188e-6)*temp*temp;
        f_r = f_r*log(res_ratio);
      }

      rht = 102.0 -(69.0/(1.000+(7.290e-1)*f_r+(-5.580e-2)*f_r*f_r+
              (7.480e-3)*f_r*f_r*f_r+(1.010e-2)*f_r*f_r*f_r*f_r));

      rh = 102.0 -(69.0/(1.000+(7.290e-1)*f_r+(-5.580e-2)*f_r*f_r+
            (7.480e-3)*f_r*f_r*f_r+(1.010e-2)*f_r*f_r*f_r*f_r));


      if (j < 5000) {
        sd[i].rh = rh;
      } else {
        sd[i].qrh = 4.0;
      }

      if(sd[i].rh <= 0.05) { sd[i].rh = 0.1; }
    }
  }
}

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

  int done = 0;
  int read_flag = 0;
  int ret_val = 0;

  while(!done && !in_file.eof()) {
    in_file.getline(line,LINE_LEN,'\n');

    read_flag = 1;
    if (strstr(line,"6 Second Met Data") != '\0' || strstr(line,"Time") != '\0' ||
	strstr(line,"6-Second Raw Data") != '\0' || strstr(line,"TIME") != '\0' ||
	strlen(line) <= 5 || strcspn(&line[strspn(line," ")]," ") < 3) {
      read_flag = 0;
    }
    
    else if(strstr(line,"Position Data") != '\0') {
      done = 1;
      read_flag = 0;
    }
    
    else if (strstr(line,"D/R") != '\0' && (!term_alt || 
					    (term_alt && a[ret_val].alt != 99999.0 && 
					     a[ret_val].alt >= (term_alt - 75.0)))) {
      read_flag = 0;
    }

    if(read_flag) {
      string data_line = line;

      a[ret_val].time = 60 * atof(data_line.substr(0,7).c_str());
      a[ret_val].alt = atof(data_line.substr(7,11).c_str());
      a[ret_val].press = atof(data_line.substr(18,13).c_str());
      if (strstr(data_line.substr(37,6).c_str(),"999999") == '\0') {
	a[ret_val].temp = atof(data_line.substr(37,6).c_str());
      }
      a[ret_val].rh = atof(data_line.substr(51,5).c_str());
      int rh_interp = (strlen(line) > 62 && strstr(data_line.substr(62,3).c_str(),"I") != '\0')
	? 1 : 0;

      check_met_values(log_file,&a[ret_val],0,rh_interp,term_alt);
      check_met_flags(&a[ret_val]);

      ret_val++;
    }
  }
  return(ret_val);
}

/**
 * Read in the angle data section of the raw data file.
 * @param in_file The input stream containing the raw data.
 * @param a The array to contain the data.
 * @return The number of angle observations read.
 **/
int read_angle_data(ifstream& in_file, SOUND_DATA_ARR a) {

  char line[LINE_LEN+1];

  int read_flag = 0;
  int ret_val = 0;

  while(!in_file.eof()) {
    in_file.getline(line,LINE_LEN,'\n');

    read_flag = 0;
    if(strstr(line,"Position Data") == '\0' && strstr(line,"6 Second Met Data")== '\0' &&
       strstr(line,"TIME") == '\0' && strlen(line) > 5 && 
       strcspn(&line[strspn(line," ")]," ") > 2) {
      read_flag = 1;
    }

    if (read_flag) {
      string data_line = line;

      a[ret_val].time = 60.0 * atof(data_line.substr(0,5).c_str());

      if (strstr(line,"NULL INT") == '\0' && strstr(line,"MISSING AZ") == '\0') {
	a[ret_val].elev = atof(data_line.substr(16,5).c_str());
	a[ret_val].azim = atof(data_line.substr(31,5).c_str());

	if (a[ret_val].elev <= 0.0 || a[ret_val].elev > 90.0 ||
	    a[ret_val].azim < 0.0 || a[ret_val].azim > 360.0 ||
	    a[ret_val].press == 9999.0 && a[ret_val].alt == 99999.0) {
	  a[ret_val].elev = 999.0;
	  a[ret_val].azim = 999.0;
	}
      }
      
      ret_val++;
    }
  }
  return ret_val;
}
