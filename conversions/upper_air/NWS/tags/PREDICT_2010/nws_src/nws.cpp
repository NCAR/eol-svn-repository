/**
 * The nws.cpp file contains the functions for the NWS conversions
 * that are the same for the Vaisala and the VIZ conversions.  It 
 * contains calculations, angle smoothing algorithms, raw data 
 * header processing, and the main method.
 *
 * To compile, see the make file (Makefile) in this directory.
 *
 * @author Joel Clawson Apr 2005
 *
 * The nws.cpp file is a new file that was created during the refactoring
 * for the NWS software.  It contains functions that were originally 
 * included in the nwsVaisala2000.c, nwsVIZ2000.c, and sounding_compute.c
 * files.  The refactoring included renaming and moving functions, changing
 * variable names to become more readable (and understandable), splitting
 * functions into component pieces, consolidating functions that were in
 * both the nwsVaisala2000.c and nwsVIZ2000.c files that were equilvalent,
 * and simplifying functions.
 *
 * NOTE: The smooth_angle_list function has only been renamed and has not
 * been refactored.  This is because it is a complicated algorithm and at
 * this point, was not worth the time needed to determine how it should be
 * refactored.
 *
 * CHANGES:
 *   - All of the rate change checkers were removed.  This was decided by
 * Scot Loehrer.  The checks should not be performed in the conversion
 * since they will be done in the auto QC.
 *   - The altitude and relative humidity range checks were removed.  This
 * was decided by Scot Loehrer.  These checks are also done in the QC and
 * will not be done in the conversion.
 *   - The HEADER struct was created to hold all of the header data.  This
 * was to minimize the number of variables being passed around the different
 * functions.  This also removed a number of pointers to pointers which get
 * confusing to deal with.
 *   - As many temporary variables as possible were removed to make the code 
 * easier to read and understand.
 *   - The file naming convention was changed from the old ABCMDDhh.cls 
 * (ABC = station id, M = nominal release month in hexadecimal, DD = nominal 
 * release day of the month, and hh == nominal release hour) to 
 * ABC_YYYYMMDDhhmm.cls where (YYYY = actual release year, MM = actual release
 * month, DD = actual release day, hh = actual release hour, mm = actual release
 * minute).
 *   - Changed the dew point calculation to match the equation used by the
 * rest of the sounding conversions.
 *   - Changed the log file from being an append at the end to creating a new
 * file for each sounding.
 *   - The notch limit checking was commented out.  During the conversion of the
 * NAME project, the notch limits were adding oscillations into the data (instead
 * of removing them as expected).  The commenting out of the notch limit checks
 * prevented the oscillations from occuring.
 *
 ********************************************************************************
 * The following documentation is from the code prior to Apr 2005.  Changes have 
 * been made to match the new function names where applicable.  Other changes
 * may have been made that were specific to either the Vaisala or the VIZ
 * conversions.  These are listed in their own files.  - Joel Clawson
 ********************************************************************************
 *
 * @author ?? Darren Gallant Dec 2000 
 *
 * CHANGES:
 *   - Changed the logic in read_header to use if-else if structure instead of
 * seperate if blocks when looking for header sections (from nwsVaisala2000.c)
 * 
 * ---------------------------------------------------------------------------
 *
 * @author ?? Darren Gallant May 1999 
 *
 * CHANGES:
 *   - Changed header GMT to UTC and Launch to release (from nwsVaisala2000.c)
 *   - Made changes to the smooth_elev (smooth_angle_list) routine.  Problem cause
 * by excessibe number of missing elevation and/or azimuth angles.  Current 
 * algorithm just insures end points angles not missing.  There is no check on 
 * number of missing angles contains in the interval.  Files containing a large
 * number of missing elevation or azimuth angles created infinite loops.
 *     SOLUTION: Counted the number of missing angles contained within[start_index,
 * end_index], called miss_freq if miss_freq/num_obs > .25 (i.e. 75% non-missing)
 * than no smoothing performed.  Currently threshold percentage is experimental.
 * Further testing is required. (from sounding_compute.c)
 * 
 * ---------------------------------------------------------------------------
 *
 * @author ?? Darren Gallant Mar 1999
 *
 * CHANGES:
 *   - if num_met_obs < num_ang_obs then num_obs = num_met_obs,
 *     if num_ang_obs < num_met_obs then num_obs = num_met_obs,
 *     typically num_obs = num_met_obs.  This is to prevent nothing being written
 * to the class file when no position data exists. (from nwsVaisala2000.c)
 * 
 * ---------------------------------------------------------------------------
 *
 * @author ?? Darren Gallant Jan 1999
 * 
 * CHANGES:
 *   - Allowed for four digit years in the pre_admin (read_pre_administration_data)
 * section of the header. (from nwsVaisala2000.c)
 *   - Introduced Pressure and Altitude change checks.  If pressure changes 4mb/s
 * or greater or Altitide changed 40 m/s or more between levels than all datum are
 * set to missing and the time is kept (from nwsVIZ2000.c)
 * 
 * ---------------------------------------------------------------------------
 *
 * @author ?? Darren Gallant Oct 1998
 *
 * CHANGES:
 *   - Added log file (nwsVaisala.log/nwsVIZ.log) which contains output file name,
 * input file errors, variables set to missing, and a time stamp. (from 
 * nwsVaisala2000.c, nwsVIZ2000.c)
 *   - Introduced Pressure and Altitude change checks.  If pressure changes 4mb/s
 * or greater or Altitide changed 40 m/s or more between levels than all datum are
 * set to missing and the time is kept (from nwsVaisala2000.c)
 * 
 * ---------------------------------------------------------------------------
 *
 * @author ?? Darren Gallant May 1998
 * 
 * CHANGES:
 *   - The format of the relative humidity changed from dd to dd.d.  The code was
 * changed to read in 6 characters for the field instead of 4. (from 
 * nwsVaisala2000.c)
 *   - Added Sonde Manufacturer and Serial Number lines to the header (nwsVIZ2000.c)
 *   - For VIZ B2 radiosonde no RH correction is performed (nwsVIZ2000.c)
 * 
 * ---------------------------------------------------------------------------
 *
 * @author ?? Darren Gallant Dec 1996
 *
 * CHANGES:
 *   - Created seperate procedures in read_header for each data section.
 *   - Changed program to run with error messages for ASCII files missing pre
 * administration and pre release sections.  Output file: XXXc99999.cls. (nwsVIZ2000.c)
 * 
 * ---------------------------------------------------------------------------
 *
 * @author Kendall Southwick May 1994
 *
 * This program will convert NWS ascii version of store files into OCF (OFPS CLASS
 * Format).  (from nwsVaisala2000.c, newVIZ2000.c)
 **/
#include <ctype.h>
#include <fstream.h>
#include <iostream.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "matrix.hpp"
#include "nws.h"


/**
 * Calculate the dew points for all of the values in the data array.
 * @param data The array holding the sounding data.
 **/
void calculate_dew_point(SOUND_DATA_ARR data) {
  float eso = 6.1121;
  for(int i = 0; i < MAX_OB; i++) {
    if (data[i].rh <= 0.0 || data[i].rh > 100.0 || data[i].temp == 999.0) {
      data[i].dewpt = 999.0;
    } else {
      // Calculate the dew point
      float emb = eso*(data[i].rh/100.0)*
	exp(17.67*data[i].temp/(data[i].temp+243.5));
      float log_val = log(emb / eso);
      data[i].dewpt = (243.5*log_val)/(17.67 - log_val);

      // Handle the case where the dew point is too small for the field
      if(data[i].dewpt < -99.9) {
        data[i].dewpt = -99.9;
        data[i].qrh = 4.0;
      }
    }
  }
}

/**
 * Calculate the ascension rates for the entries in the data array.
 * @param logfile The output stream where warnings are to be written.
 * @param data The array that contains the sounding data.
 **/
void calculate_ascension_rates(ofstream& logfile, SOUND_DATA_ARR data) {

  data[0].z_cmp = 999.0;
  data[0].qz = 9.0;

  float last_alt = data[0].alt;
  float last_time = data[0].time;

  for (int i = 1; i < MAX_OB; i++) {
    if (data[i].time != 9999.0 && data[i].alt != 99999.0 &&
	last_time != 9999.0 && last_alt != 99999.0) {

      data[i].z_cmp = (data[i].alt - last_alt) / (data[i].time - last_time);
      data[i].qz = 99.0;

      if (data[i].z_cmp < -99.9 || data[i].z_cmp > 999.9) {
	char outline[100];
	sprintf(outline,"Ascension Rate: %5.1f does not fit in the field at time %6.1f. Setting to missing.\n",data[i].z_cmp,data[i].time);
	logfile << outline;
	data[i].z_cmp = 999.0;
	data[i].qz = 9.0;
      }
      
      last_time = data[i].time;
      last_alt = data[i].alt;
    }
  }
}

/**
 * Calculate the latitude and longitude of the entries in the data array.
 * @param data The data array that contains the sounding data.
 * @param xs The array of x positions of the sounding.
 * @param ys The array of y positions of the sounding.
 * @param lon The longitude at the surface of the sounding.
 * @param lat The latitude at the surface of the sounding.
 * @param pos_status The status flags for the x and y positions.
 **/
void calculate_lat_and_lon(SOUND_DATA_ARR data, double xs[MAX_OB], double ys[MAX_OB],
			   float lon, float lat, int pos_status[MAX_OB]) {
 
  data[0].lon = lon;
  data[0].lat = lat;
  
  for(int i = 1; i < MAX_OB; i++) {
    if (pos_status[i] == 99 && data[i].alt != 99999.0 && 
	data[i].u_cmp != 9999.0 && data[i].v_cmp != 9999.0) {
      data[i].lat = lat + (ys[i] - ys[0])/111033.0;
      data[i].lon = lon + (xs[i] - xs[0])/(111033.0*cos(data[i].lat*M_PI/180.0));
    } else{
      data[i].lat = 999.000;
      data[i].lon = 9999.000;
    }
  }
}

/**
 * Calculate the x and y positions from the release location.
 * @param elev The array of elevation angles to use to calculate the position.
 * @param azim The array of azimuth angles to use to calculate the position.
 * @param alt The array of altitudes to use to calculate the position.
 * @param x The array to use to store the x positions.
 * @param y The array to use to store the y positions.
 * @param qxy The array to use to store the flags for the x and y positions.
 * @param size The number of elements in the array.
 **/
void calculate_position(double elev[MAX_OB], double azim[MAX_OB], double alt[MAX_OB],
			double x[MAX_OB], double y[MAX_OB], int qxy[MAX_OB], int size) {

  double rad_earth=6378388.0, conv=0.0174533;

  x[0] = 0.0;
  y[0] = 0.0;
  qxy[0] = 99;
  
  for(int i = 1; i < size; i++) {
    if(alt[i] != 99999.0 && azim[i] <= 360.0 && azim[i] >= 0.0 && 
       elev[i] <= 90.0 && elev[i] >= 0.0) {
      
      double elevr = elev[i] * conv;
      double azimr = (azim[i]-180.0) * conv;
      double work_val = asin((rad_earth*cos(elevr))/((rad_earth+alt[i])));

      if (work_val <= 0.0 || work_val >= 3.2) {
        work_val = (rad_earth*cos(elevr))/((rad_earth+alt[i]));
      }

      double arc_length = ((M_PI)/(double)(2.0) - elevr - work_val) * rad_earth;

      x[i] = arc_length * sin(azimr);
      y[i] = arc_length * cos(azimr);
      qxy[i] = 99;
    } else {
      x[i] = x[i-1];
      y[i] = y[i-1];
      qxy[i] = 9;
    }
  }
}

/**
 * Calculate the U and V wind components for the data in the data array.
 * @param logfile The output stream to write errors.
 * @param data The array that contains the data.
 * @param x The list of x positions.
 * @param y The list of y positions.
 * @param xs The list to hold the new x positions from the fitted curve.
 * @param ys The list to hold the new y positions from the fitted curve.
 * @param qxy The list to hold the flags for the new x and y positions.
 * @param angle_status The list of flags for the angles.
 * @param size The number of entries in the data lists.
 **/
void calculate_uv_components(ofstream& logfile, SOUND_DATA_ARR data, double x[MAX_OB], 
			     double y[MAX_OB], double xs[MAX_OB], double ys[MAX_OB], 
			     int qxy[MAX_OB], int angle_status[MAX_OB], int size) {

  for(int i = 1; i < MAX_OB; i++) {
    if(angle_status[i] == 9) { qxy[i] = 9; }
  }

  for(int i = 0; i < size; i++) {
    int min_ind, max_ind;

    int end_flag = 0;
    int index_flag = 0;
    int linear_flag = 0;
    int flag = 0;

    // Half the smoothing interval centered around i.
    int spacing = data[i].press < 50.0 ? 15 : 10;


    // Get start and stop indeces.
    while(!index_flag) {

      if (end_flag) { index_flag = 1; }

      if((i-spacing) < 0) {
        min_ind = 0;
        end_flag = 1;
      } else {
        min_ind = i-spacing;
      }

      if((i+spacing) >= size) {
        max_ind = size - 1;
        end_flag = 1;
      } else {
        max_ind = i+spacing;
      }


      // Check to see if there are the correct number of point to proform the calulation.
      int left_tot = 0;
      for(int j = min_ind; j < i; j++) {
        if(qxy[j] == 99 || j == 0) { left_tot++; }
      }

      int right_tot = 0;
      for(int j = i+1; j <= max_ind; j++) {
        if(qxy[j] == 99) { right_tot++; }
      }

      flag = 0;
      if ((left_tot > 0 && right_tot > 0 && (left_tot + right_tot) > 3 && qxy[i] != 9) ||
	  (left_tot >= 3 && qxy[i] == 99) || (right_tot >= 3 && qxy[i] == 99)) {
        flag = 1;
      }

      linear_flag = (left_tot < spacing / 3 || right_tot < spacing / 3) ? 1 : 0;

      if (end_flag) {
        linear_flag = 0;
        spacing += 5;
      } else {
        index_flag = 1;
      }
    }

    // Compute curve fit for x and y over min_ind to max_ind.
    xs[i] = x[i];
    ys[i] = y[i];
    data[i].u_cmp = 9999.0;
    data[i].v_cmp = 9999.0;

    if (qxy[i] != 9 && flag) {
      
      double st[5], sx[3], sy[3];
      for(int j = 0; j < 5; j++) { st[j] = 0.0; }
      for(int j = 0; j < 3; j++) { sx[j] = sy[j] = 0.0; }
      
      for(int j = min_ind; j < max_ind; j++) {
	if(qxy[j] == 99 || j == 0) {
	  double dt = data[j].time - data[i].time;
          st[0] += 1.0;
          st[1] += dt;
          st[2] += dt*dt;
          st[3] += dt*dt*dt;
          st[4] += dt*dt*dt*dt;
          sx[0] += x[j];
          sx[1] += x[j]*dt;
          sx[2] += x[j]*dt*dt;
          sy[0] += y[j];
          sy[1] += y[j]*dt;
          sy[2] += y[j]*dt*dt;
	}
      }

      double xa0, xa1, ya0, ya1;
      if (linear_flag) {
	xa1 = (st[0]*sx[1]-st[1]*sx[0])/(st[0]*st[2]-st[1]*st[1]);
	ya1 = (st[0]*sy[1]-st[1]*sy[0])/(st[0]*st[2]-st[1]*st[1]);
	xa0 = sx[0]/st[0]-xa1*st[1]/st[0];
	ya0 = sy[0]/st[0]-ya1*st[1]/st[0];
      } else {
	matrix a_mat, y_mat, coef_mat;
	a_mat.assign(3,3);
	for(int k = 1; k <= 3; k++) {
	  for(int j = 1; j <= 3; j++) {
	    a_mat(k,j) = st[(k-1)+(j-1)];
	  }
	}
	
	y_mat.assign(3,1);
	for(int k = 1; k <= 3; k++) {
            y_mat(k,1) = sx[k-1];
	}
	
	coef_mat = matrix_solver(a_mat, y_mat);
	xa0 = coef_mat(1,1);
	xa1 = coef_mat(2,1);
	
	y_mat.assign(3,1);
	for(int k = 1; k <= 3; k++) {
	  y_mat(k,1) = sy[k-1];
	}
	
	coef_mat = matrix_solver(a_mat, y_mat);
	ya0 = coef_mat(1,1);
	ya1 = coef_mat(2,1);
      }

      xs[i] = xa0;
      ys[i] = ya0;
      data[i].u_cmp = xa1;
      data[i].v_cmp = ya1;
    }

    data[i].qu = data[i].qv = (angle_status[i] == 4 || angle_status[i] == 9) ?
      angle_status[i] : qxy[i];

    if (data[i].u_cmp == 9999.0) {
      data[i].qu = data[i].qv = 9.0;
    } else if (fabs(data[i].u_cmp) >= 250.0 || fabs(data[i].v_cmp) >= 250.0) {
      data[i].u_cmp = data[i].v_cmp = 9999.0;
      data[i].qu = data[i].qv = 9.0;

      char outline[150];
      sprintf(outline,"Setting U and V components to missing at time %.1f because one of the values was >= 250.0",data[i].time);
      logfile << outline << endl;
    }
  }
}

/**
 * Calculate the wind speed and wind direction for the entries in the data array.
 * @param data The array that contains the sounding data.
 **/
void calculate_wind_speed_and_direction(SOUND_DATA_ARR data) {

  for(int i = 1; i < MAX_OB; i++) {
    if(data[i].u_cmp == 0.0 && data[i].v_cmp == 0.0) {
      data[i].wind_spd = 0.0;
      data[i].wind_dir = 0.0;
      data[i].qu = 3.0;
      data[i].qv = 3.0;
    } else if(data[i].u_cmp != 9999.0 && data[i].v_cmp != 9999.0) {
      data[i].wind_spd = sqrt(data[i].u_cmp*data[i].u_cmp+data[i].v_cmp*data[i].v_cmp);
      data[i].wind_dir = (atan2(data[i].u_cmp,data[i].v_cmp)*180.0/M_PI)+180;
    } else {
      data[i].wind_spd = 999.0;
      data[i].wind_dir = 999.0;
    }

    if(data[i].wind_spd > 999.0) {
      data[i].u_cmp = 9999.0;
      data[i].v_cmp = 9999.0;
      data[i].wind_spd = 999.0;
      data[i].wind_dir = 999.0;
      data[i].qu = 9.0;
      data[i].qv = 9.0;
    }
  }
}

/**
 * Check the meteorological flags to make sure that they match the values.
 * @param obs The observation being checked.
 **/
void check_met_flags(OBSER *obs) {
  obs->qp  = obs->press == 9999.0 ? 9.0 : obs->qp;
  obs->qt  = obs->temp  ==  999.0 ? 9.0 : obs->qt;
  obs->qrh = obs->rh    ==  999.0 ? 9.0 : obs->qrh;
}

/**
 * Check the meteorological values to see if they are valid.
 * @param log_file The output stream for errors.
 * @param data The reading to be checked.
 * @param temp_interp A flag if the temperature was estimated.
 * @param rh_interp A flag if the relative humidity was estimated.
 **/
void check_met_values(ofstream& log_file, OBSER *data, int temp_interp, int rh_interp, float term_alt) {
  char outline[75];

  if(data->press <= 0.0 || data->press > 1100.0 && data->press != 9999.0) {
    sprintf(outline,"Press: %7.1f mb at time: %.1f.\n",data->press,data->time);
    log_file << outline;
    data->qp = 9.0;
    data->qt = 9.0;
    data->qrh = 9.0;
    data->temp = 999.0;
    data->press = 9999.0;
    data->rh = 999.0;
    data->alt = 99999.0;
  } else {
    data->qp = 99.0;
  }

  if((data->temp < -150.0) || (data->temp > 75.0 && data->temp != 999.0)) {
    sprintf(outline,"Temp at press: %6.1f is: %6.1f deg .C.\n",data->press,data->temp);
    log_file << outline;
    data->temp = 999.0;
    data->rh = 999.0;
    data->qt = 9.0;
    data->qrh = 9.0;
  } else if(temp_interp && data->temp != 999.0) {
    data->qt = 4.0;
  } else {
    data->qt = 99.0;
  }

  if (data->rh < 0 || data->rh > 999.9) {
    sprintf(outline,"RH at press: %6.1f is: %6.1f deg .C.\n",data->press,data->rh);
    log_file << outline;
    data->rh = 999.0;
    data->qrh = 9.0;
  }
  
  data->qrh = data->rh == 999.0 ? 9.0 : (rh_interp ? 4.0 : 99.0);

  if(data->temp < -99.9) {
    sprintf(outline,"Temp at press: %6.1f is: %6.1f deg .C.\n",data->press,data->temp);
    log_file << outline;
    data->rh = 999.0;
    data->qrh = 9.0;
  }
}

/**
 * Correct call letters in the header for special indecies.
 * @param header The container containing the index and call letters.
 * @return If the call letters were able to be corrected.
 **/
int correct_call_letters(HEADER *header) {

  if(header->index == 70026)
    strcpy(header->call_letters,"BRW");
  else if(header->index == 91212)
    strcpy(header->call_letters,"PGUM");
  else if(header->index == 91285)
    strcpy(header->call_letters,"PHTO");
  else if(header->index == 91408)
    strcpy(header->call_letters,"PTRO");
  else if(header->index == 91165)
    strcpy(header->call_letters,"PHLI");
  else if(header->index == 91376)
    strcpy(header->call_letters,"PMKJ");
  else if(header->index == 91765)
    strcpy(header->call_letters,"NSTU");
  else if(header->index == 91348)
    strcpy(header->call_letters,"PTPN");
  else if(header->index == 91334)
    strcpy(header->call_letters,"PTKK");
  else if(header->index == 91413)
    strcpy(header->call_letters,"PTYA");
  else if(header->index == 91245)
    strcpy(header->call_letters,"PWAK");
  else
    return 0;
  return 1;
}

/**
 * Initialize the array for holding sounding data and the header structure
 * with the default values for the data structures.
 * @param data The array to hold the sounding data.
 * @param header The pointer to the storage for the header information.
 **/
void initialize_data(SOUND_DATA_ARR data, HEADER *header) {
  for (int i = 0; i < MAX_OB; i++) {
    data[i].time     =  9999.0;
    data[i].press    =  9999.0;
    data[i].temp     =   999.0;
    data[i].dewpt    =   999.0;
    data[i].rh       =   999.0;
    data[i].u_cmp    =  9999.0;
    data[i].v_cmp    =  9999.0;
    data[i].wind_spd =   999.0;
    data[i].wind_dir =   999.0;
    data[i].z_cmp    =   999.0;
    data[i].lon      =  9999.0;
    data[i].lat      =   999.0;
    data[i].elev     =   999.0;
    data[i].azim     =   999.0;
    data[i].alt      = 99999.0;
    data[i].qp       =     9.0;
    data[i].qt       =     9.0;
    data[i].qrh      =     9.0;
    data[i].qu       =     9.0;
    data[i].qv       =     9.0;
    data[i].qz       =     9.0;
  }

  header->lat        =   999.0;
  header->lon        =  9999.0;
  header->elev       =  -999.0;
  header->rel_year   =      -1;
  header->rel_month  =      -1;
  header->rel_day    =      -1;
  header->rel_hour   =      -1;
  header->rel_min    =      -1;
  header->nom_rel_year  =   -1;
  header->nom_rel_month =   -1;
  header->nom_rel_day   =   -1;
  header->nom_rel_hour  =   -1;
  header->ascen_num  =      -1;
  header->rh33_res   =     -10;
  header->term_alt   =    -500;

  header->index      =      -1;
}

/**
 * Do some sort of matrix solving.
 * @param a The first matrix.
 * @param b The second matrix.
 * @return The resultant matrix.
 **/
matrix matrix_solver(matrix a, matrix b) {
  matrix coef, tmp;

  tmp = a;
  coef = b;

  // Gussian elemination.
  for(int i = 1; i <= tmp.nRows(); i++) {
    if(tmp(i,i) != 0.0) {
      for(int j = i+1; j <= tmp.nRows(); j++) {
        if(tmp(j,i) != 0.0) {
          double val = -1.0*(tmp(i,i)/tmp(j,i));
          for(int k = i; k <= tmp.nCols(); k++) {
            tmp(j,k) = tmp(j,k)*val+tmp(i,k);
          }
          coef(j,1) = coef(j,1)*val + coef(i,1);
        }
      }
    }
  }

  // Back sub.
  for(int i = tmp.nRows(); i >= 1; i--) {
    if(tmp(i,i) != 0) {
      double sum = 0.0;
      for(int j = i+1; j <= tmp.nCols(); j++) {
	sum += tmp(i,j)*coef(j,1);
      }
      coef(i,1) = (coef(i,1) - sum)/tmp(i,i);
    } else {
      coef(i,1) = 0.0;
    }
  }
  
  return(coef);
}

/**
 * Read in the relative humidity calibration data from the raw data header.
 * @param infile The input stream containing the raw data.
 * @param header The container for the header information.
 * @return If the calibration data was read in successfully or not.
 **/
int read_calibration_data(ifstream& infile, HEADER *header) {
  char line[LINE_LEN + 1];
  int success = 0;

  while (!success && !infile.eof()) {

    if (strncmp(line,"RH lock-in Resistance (HR):",27) == 0) {
      sscanf(line,"%*s%*s%*s%*s%f",&header->rh33_res);
      success = 1;
    }

    infile.getline(line,LINE_LEN,'\n');
  }

  return success;
}

/**
 * Read in the raw data header and print it to the output file in the CLASS format.
 * @param in_file The input stream containing the raw data.
 * @param out_file The output stream where the data is to be written.
 * @param log_file The output stream for writing errors.
 * @param i_f_name The name of the input file.
 * @param lim_ang The array that will contain the limiting angles.
 * @param wind_speed The address for holding the surface wind speed.
 * @param wind_dir The address for holding the surface wind direction.
 * @param header The container for the header data.
 * @return If the reading of the header data was successful or not.
 **/
int read_header(ifstream& in_file, ofstream& out_file, ofstream& log_file, 
		char *i_f_name,double lim_ang[360], 
		float *wind_speed, float *wind_dir,HEADER *header) { 

  char line[LINE_LEN+1];
  int success = 0;

  cout << endl << "Processing file: " << i_f_name << endl;

  int read_station = 0;
  int read_pre_admin = 0;
  int read_pre_flight = 0;
  int read_limit_angles = 0;
  int read_sfc_winds = 0;
  int read_release_data = 0;
  int read_term_summary = 0;
  int read_calib_data = 0;

  int done = 0;
  while(!done && !in_file.eof()) {
    in_file.getline(line,LINE_LEN,'\n');
    
    if (strstr(line,"Station Data") != '\0' && !read_station) {
      read_station = read_station_data(in_file,header);
    }

    else if (strstr(line,"Prerelease Admin") != '\0' && !read_pre_admin) {
      read_pre_admin = read_pre_administration_data(in_file,header);
    }

    else if (strstr(line,"Prerelease Flight") != '\0' && !read_pre_flight) {
      read_pre_flight = read_pre_flight_data(in_file,header);
    }

    else if (strstr(line,"Surface Observation") != '\0' && !read_sfc_winds) {
      read_sfc_winds = read_surface_winds(in_file,&wind_speed,&wind_dir);
    }

    else if(strstr(line,"Limiting Angle") != '\0' && !read_limit_angles) {
      read_limit_angles = read_limiting_angles(in_file,lim_ang);
    }

    else if(strstr(line,"Viz Radiosonde Cal") != '\0' && !read_calib_data) {
      read_calib_data = read_calibration_data(in_file,header);
    }
    
    else if(strstr(line,"Viz B2 Radiosonde Cal") != '\0' && !read_calib_data) {
      read_calib_data = read_calibration_data(in_file,header);
    }

    else if(strstr(line,"Release Data") != '\0' && !read_release_data) {
      read_release_data = read_release_time(in_file,header);
    }

    else if(strstr(line,"Termination Summary") != '\0' && !read_term_summary) {
      read_term_summary = read_termination_summary(in_file,header);
    }

    if (strstr(line,"6 Second Met Data") != '\0') { done = 1; }
  }

  if (done) {
    success = 1;
  } else {
    cerr <<"Error in reading file header"<<endl;
    log_file <<"Error in reading file header"<<endl;
    success = 0;
  }

  if(!read_station){
    cerr << "Station Data Record is missing."<< endl;
    log_file << "Station Data Record is missing."<< endl;
    cerr << "File may have been processed with wrong program."<<endl;
    log_file << "File may have been processed with wrong program."<<endl;
    success = 0;
  }

  if(!read_pre_admin) {
    cerr << "Prerelease Administrative Data is missing."<< endl;
    log_file << "Prerelease Administrative Data is missing."<< endl;
    cerr << "File may have been processed with wrong program."<<endl;
    log_file << "File may have been processed with wrong program."<<endl;
    header->nom_rel_hour = 99;
    header->nom_rel_day = 99;
    header->nom_rel_month = 12;
    header->nom_rel_year = 99;
    header->ascen_num = 9999;

  }
  
  if(!read_calib_data && TYPE == VIZ_TYPE){
    cerr << "VIZ Radiosonde Calibration Data is missing."<< endl;
    log_file << "VIZ Radiosonde Calibration Data is missing."<< endl;  
    cerr << "File may have been processed with wrong program."<<endl;
    log_file << "File may have been processed with wrong program."<<endl;
    success = 0;
   
  }

  if(!read_pre_flight) {
    cerr << "Prerelease Flight Equipment Data is missing."<< endl;
    log_file << "Prerelease Flight Equipment Data is missing."<< endl;
    cerr << "File may have been processed with wrong program."<<endl;
    log_file << "File may have been processed with wrong program."<<endl;
    success = 0;
  }

  if(!read_limit_angles) {
    cerr << "Limiting Angle Data is missing."<< endl;
    log_file << "Limiting Angle Data is missing."<< endl;
    cerr << "File may have been processed with wrong program."<<endl;
    log_file << "File may have been processed with wrong program."<<endl;
    success = 0;
  }

  if(!read_sfc_winds) {
    cerr << "Surface Observation Data is missing."<< endl;
    log_file << "Surface Observation Data is missing."<< endl;
    cerr << "File may have been processed with wrong program."<<endl;
    log_file << "File may have been processed with wrong program."<<endl;
    success = 0;
  }

  if(!read_release_data) {
    cerr << "Release Data is missing."<< endl;
    log_file << "Release Data is missing."<< endl;
    cerr << "File may have been processed with wrong program."<<endl;
    log_file << "File may have been processed with wrong program."<<endl;
    header->rel_year = 99;
    header->rel_month = 12;
    header->rel_day = 99;
    header->rel_hour = 99;
    header->rel_min = 99;
  }

  if(!read_term_summary) {
    cerr << "Termination Summary Data is missing."<< endl;
    log_file << "Termination Summary Data is missing."<< endl;
    cerr << "File may have been processed with wrong program."<<endl;
    log_file << "File may have been processed with wrong program."<<endl;
    success = 0;
  }
  
  if (header->call_letters[0] == '_' && !correct_call_letters(header)) {
    cerr <<"Missing call letters for station index "<< header->index <<endl;
    log_file <<"Missing call letters for station index "<< header->index <<endl;
    success = 0;
  }
  
  if (success) { success = write_header_data(out_file,log_file,header); }
  
  return(success);
}


/**
 * Read in the limiting angles from the raw data header.
 * @param infile The input stream containing the raw data.
 * @param lim_ang The array to hold the limiting angles.
 * @return If the limiting angles were read in successfully or not.
 **/
int read_limiting_angles(ifstream& infile, double lim_ang[360]) {
  char line[LINE_LEN + 1];
  int success = 0;

  int angle_index = 0;
  while (!success && !infile.eof()) {

    infile.getline(line,LINE_LEN,'\n');

    if(strlen(line) > 50) {
      int i = strspn(line," ");
      while(strlen(&line[i]) > 0) {
        char tmpstr[20];
        strcpy(tmpstr,"");
        int j = strcspn(&line[i]," ");
        strncpy(tmpstr,&line[i],j);
        tmpstr[j] = '\0';
        lim_ang[angle_index] = atof(tmpstr);
        j += i;
        i = strspn(&line[j]," ") + j;
        angle_index++;
	if (angle_index >= 360) { success = 1; }
      }
    }
  }

  return success;
}

/**
 * Read in the pre administration information section of the raw data header.
 * @param infile The input stream containing the raw data.
 * @param header The container for the header information.
 * @return If the pre administration data was read in successfully or not.
 **/
int read_pre_administration_data(ifstream& infile, HEADER *header) {
  char line[LINE_LEN + 1];
  int success = 0;

  while (!success && !infile.eof()) {

    infile.getline(line,LINE_LEN,'\n');

    if (strncmp(line,"Date:",5) == 0) {
      sscanf(line,"%*s%d/%d/%d",&header->nom_rel_month,&header->nom_rel_day,
	     &header->nom_rel_year);
      if (header->ascen_num > 0 && header->nom_rel_hour >= 0) { success = 1; }
    }

    else if (strncmp(line,"Hour:",5) == 0) {
      sscanf(line,"%*s%d",&header->nom_rel_hour);
      if (header->ascen_num > 0 && header->nom_rel_year > 0) { success = 1; }
    }

    else if (strncmp(line,"Ascension No.:",14) == 0) {
      sscanf(line,"%*s%*s%d",&header->ascen_num);
      if (header->nom_rel_year > 0 && header->nom_rel_hour >= 0) { success = 1; }
    }
  }

  return success;
}

/**
 * Read in the pre flight information section of the raw data header.
 * @param infile The input stream containing the raw data.
 * @param header The container for the header information.
 * @return If the pre flight data was read in successfully or not.
 **/
int read_pre_flight_data(ifstream& infile, HEADER *header) {
  char line[LINE_LEN + 1];
  int success = 0;

  while (!success && !infile.eof()) {

    infile.getline(line,LINE_LEN,'\n');

    if (strncmp(line,"Radiosonde Mfr:",15) == 0) {
      strcpy(header->sonde_type,&line[16+strspn(&line[16]," ")]);
      for (int i = 0; i < strlen(header->sonde_type); i++) {
	if (iscntrl(header->sonde_type[i])) { header->sonde_type[i] = '\0'; }
      }
      if (strlen(header->serial_number) > 0) { success = 1; }
    }

    else if (strncmp(line,"Radiosonde S.N.",15) == 0) {
      sscanf(line,"%*s%*s%s",header->serial_number);
      if (strlen(header->sonde_type) > 0) { success = 1; }
    }
  }
  
  return success;
}

/**
 * Read in the release time from the raw data header.
 * @param infile The input stream containing the raw data.
 * @param header The container for the time information.
 * @return If the release time was read in successfully or not.
 **/
int read_release_time(ifstream& infile, HEADER *header) {
  char line[LINE_LEN + 1];
  int success = 0;

  while (!success && !infile.eof()) {

    infile.getline(line,LINE_LEN,'\n');

    if (strncmp(line,"Released at:",12) == 0) {
      sscanf(line,strstr(line,"on") == '\0' ? 
	     "%*s%*s%d:%d %d/%d/%d" : "%*s%*s%d:%d%*s%d/%d/%d",
             &header->rel_hour,&header->rel_min,&header->rel_month,
	     &header->rel_day,&header->rel_year);
      success = 1;
    }
  }

  return success;
}

/**
 * Read in the station information section of the raw data header.
 * @param infile The input stream containing the raw data.
 * @param header The container for the header information.
 * @return If the station information was read in successfully or not.
 **/
int read_station_data(ifstream& infile, HEADER *header) {
  int success = 0;
  char line[LINE_LEN + 1];

  while (!success && !infile.eof()) {

    infile.getline(line,LINE_LEN,'\n');
    
    if (strncmp(line,"Index:",6) == 0) {
      sscanf(line,"%*s%d%*s%*d",&header->index);
    }

    else if (strncmp(line,"Name:",5) == 0) {
      strcpy(header->site_name,&line[6+strspn(&line[6]," ")]);
      for (int i = 0; i < strlen(header->site_name); i++) {
	if (iscntrl(header->site_name[i])) { header->site_name[i] = '\0'; }
      }
    }

    else if (strncmp(line,"Parent FO:",10) == 0) {
      sscanf(line,"%*s%*s%*s%*s%*s%s",header->call_letters);
      for (int i = 0; i < strlen(header->call_letters); i++) {
	if (iscntrl(header->call_letters[i])) { header->call_letters[i] = '\0'; }
      }
    }

    else if (strncmp(line,"Latitude:",9) == 0) {
      sscanf(line,"%*s%f%*s%f",&header->lat,&header->lon);
      header->lon *= -1;
    }

    else if (strncmp(line,"Elevation:",10) == 0) {
      sscanf(line,"%*s%f",&header->elev);
    }

    if(header->lat != 999.0 && header->lon != 9999.0 && header->index != -1 &&
       strlen(header->call_letters) > 1 && strlen(header->site_name) > 1 && 
       header->elev > -999) {
      success = 1;
    }
  }

  return success;
}

/**
 * Read in the wind speed and direction from the surface location.
 * @param infile The input stream containing the raw data.
 * @param wind_spd The address to hold the wind speed.
 * @param wind_dir The address to hold the wind direction.
 * @return If the reading in of the surface wind values was a success or not.
 **/
int read_surface_winds(ifstream& infile, float **wind_spd, float **wind_dir) {
  char line[LINE_LEN + 1];
  int success = 0;

  **wind_spd = -1;
  **wind_dir = -1;

  while (!success && !infile.eof()) {

    infile.getline(line,LINE_LEN,'\n');
    
    if (strncmp(line,"Wind Direction:",15) == 0) {
      sscanf(line,"%*s%*s%f",*wind_dir);
      if (**wind_spd >= 0) { success = 1; }
    }

    if (strncmp(line,"Wind Speed:",11) == 0) {
      sscanf(line,"%*s%*s%f",*wind_spd);
      if (**wind_dir >= 0) { success = 1; }
    }
  }

  return success;
}

/**
 * Read in the termination summary report from the raw data header.
 * @param infile The input stream containing the raw data.
 * @param header The container where the header information will be stored.
 * @return If the reading in of the termination summary was successful or not.
 **/
int read_termination_summary(ifstream& infile, HEADER *header) {
  char line[LINE_LEN + 1];
  int success = 0;

  while (!success && !infile.eof()) {

    infile.getline(line,LINE_LEN,'\n');

    if (strncmp(line,"Termination Altitude (M):",25) == 0) {
      sscanf(line,"%*s%*s%*s%f",&header->term_alt);
      success = 1;
    }
  }

  return success;
}

/**
 * Smooth the list of angles.
 * @param w_ang The list of angles to smooth.
 * @param elev_smooth The list to hold the smoothed angles.
 * @param time The list of times.
 * @param npts The number of observations in the lists.
 * @param angle_status The list to hold the status of the angles.
 * @param lim_ang The list of limiting angles.
 * @param sd The array of sounding data.
 * @param azim_flag If the angles are azimuth angles or not.
 **/
void smooth_angle_list(double w_ang[MAX_OB], double elev_smooth[MAX_OB],
		       double time[MAX_OB], int npts, int angle_status[MAX_OB],
		       double lim_ang[360], SOUND_DATA_ARR sd, int azim_flag) {

  matrix a_mat, y_mat, b_mat, coef_mat;
  double sumxx[2*ORDER+1], sumxy[ORDER+1], residual[MAX_OB];
  double x_pow, num, stand_dev, stand_dev_sq;
  double fund_freq, delta_t, theta;
  double a_o_coef[MAX_OB], b_o_coef[MAX_OB];
  double a_s_coef[MAX_OB], b_s_coef[MAX_OB];
  double x0, x1, x2, dt, sumyy;
  double val_o, val_s;
  float ratio=0;;

  int orgin_f, num_sample_pts, num_freq;
  int i, j, k, start_index, end_index, num_pts;
  int out_min_ind, out_max_ind, azim_ind;
  int work_flag, low_elev_count, miss_freq;

//
// Inializion of varibles.
//

  start_index = 10;
  end_index = npts;
  work_flag = 1;
  low_elev_count = 0;
  miss_freq = 0;

//
// Adjust the start and end points so that they do
// not fall on missing values.  If the difference
// between the start point and end point is <= 150
// do not create the fourier series. 
//

  while((start_index < end_index)&&(angle_status[start_index] == 9))
    start_index++;
  start_index++;

  while((end_index > start_index)&&(angle_status[end_index] == 9))
    end_index--;

// Check for number of missing elev angles 

  for(i=start_index;i<=end_index;i++){
	if(angle_status[i] == 9)
		miss_freq++;
  }
  
  num_pts = end_index-start_index;

  if(num_pts)
  	ratio = (float)miss_freq/num_pts;
  if(num_pts <= 150 || ratio > .25)
    work_flag = 0;

//
// Remove outliers.
// Use 2 min interval to compute 2nd degree fit to curve
// centered at point i.
// Use only "good" point in fit.
// Use 1 min interval to check points.
//

  for(i=start_index;i<end_index;i++)
  {
    sumyy = 0.0;
    for(j=0;j<2*ORDER+1;j++)
      sumxx[j] = 0.0;
    for(j=0;j<ORDER+1;j++)
      sumxy[j] = 0.0;

    out_min_ind = i-10;
    if(out_min_ind <0)
      out_min_ind = 0;
    out_max_ind = i+10;
    if(out_max_ind>=end_index)
      out_max_ind = end_index-1;

//
// Calulate values for curve by solving Normal Equation
//

    for(j=out_min_ind;j<=out_max_ind;j++)
    { // for j.
      if(angle_status[j] == 0)
      { // Good point.
        dt = time[j] - time[i];
        sumxx[0] += 1.0;
        sumxx[1] += dt;
        sumxx[2] += dt*dt;
        sumxx[3] += dt*dt*dt;
        sumxx[4] += dt*dt*dt*dt;
        sumxy[0] += w_ang[j];
        sumxy[1] += w_ang[j]*dt;
        sumxy[2] += w_ang[j]*dt*dt;
        sumyy += w_ang[j]*w_ang[j];
      } // Good point.
    } // for j.

// Calculate coef.

    a_mat.assign(3,3);
    for(k=1;k<=3;k++)
      for(j=1;j<=3;j++)
        a_mat(k,j) = sumxx[(k-1)+(j-1)];

    y_mat.assign(3,1);
    for(k=1;k<=3;k++)
      y_mat(k,1) = sumxy[k-1];

    coef_mat = matrix_solver(a_mat, y_mat);
    x0 = coef_mat(1,1);
    x1 = coef_mat(2,1);
    x2 = coef_mat(3,1);

    stand_dev_sq = (sumxx[0]*sumyy-sumxy[0]*sumxy[0])/(sumxx[0]*(sumxx[0]-1));
    if(stand_dev_sq <0.0)
      stand_dev_sq *= -1.0;
    stand_dev = sqrt(stand_dev_sq);

    out_min_ind = i-5;
    if(out_min_ind <0)
      out_min_ind = 0;
    out_max_ind = i+5;
    if(out_max_ind>=npts)
      out_max_ind = npts-1;

    for(j=out_min_ind;j<=out_max_ind;j++)
    { //
      dt = time[j] - time[i];
      if(angle_status[j] != 9)
      {
        if(fabs(w_ang[j]-x0-dt*x1-dt*x2*x2) >THRESHOLD*stand_dev)
          angle_status[j] = 4;
      }
    }
  }


// Smooth elev angles due to low elev angle.
// Use the residual array as temp storage.

  for(i=0;i<npts;i++)
    residual[i] = w_ang[i];

  if(azim_flag == 0)
  {
    for(i=start_index;i<end_index;i++)
    {

      azim_ind = (int)(sd[i].azim+180.0);
      if(azim_ind > 360)
        azim_ind -= 360;

      if((angle_status[i] != 9) && (w_ang[i] <= lim_ang[azim_ind]+10.0))
      {

        low_elev_count++;

        for(j=0;j<2*ORDER+1;j++)
	  sumxx[j] = 0.0;
        for(j=0;j<ORDER+1;j++)
          sumxy[j] = 0.0;

        if(w_ang[i] <= lim_ang[azim_ind]+5.0)
	{
          out_min_ind = i-20;
          out_max_ind = i+20;
	}
        else if(w_ang[i] <= lim_ang[azim_ind]+7.5)
	{
          out_min_ind = i-15;
          out_max_ind = i+15;
	}
        else
	{
          out_min_ind = i-10;
          out_max_ind = i+10;
	}
        if(out_min_ind < 0)
          out_min_ind = 0;
        if(out_max_ind >= npts)
          out_max_ind = npts-1;

        for(j=out_min_ind;j<=out_max_ind;j++)
        { // for j.
         if(angle_status[j] == 0)
         { // Good point.
           dt = time[j] - time[i];
           sumxx[0] += 1.0;
           sumxx[1] += dt;
           sumxx[2] += dt*dt;
           sumxy[0] += w_ang[j];
           sumxy[1] += w_ang[j]*dt;
           sumyy += w_ang[j]*w_ang[j];
        } // Good point.
      } // for j.

// Calculate coef.

      coef_mat(2,1) = (sumxx[0]*sumxy[1]-sumxx[1]*sumxy[0])/
                      (sumxx[0]*sumxx[2]-sumxx[1]*sumxx[1]);

      coef_mat(1,1) = sumxy[0]/sumxx[0]-coef_mat(2,1)*sumxx[1]/sumxx[0];

      if((sumxx[0] < 1.0) || ((sumxx[0]*sumxx[2]-sumxx[1]*sumxx[1]) == 0))
        residual[i] = w_ang[i];
      else
        residual[i] = coef_mat(1,1); 

      } // If non-missing data.
    } // for i;


    for(i=0;i<npts;i++)
    {
      w_ang[i] = residual[i];
    }

  } // If azim_flag.

  if(low_elev_count >= (0.2*(end_index-start_index)))
    work_flag = 0;

//
// Collect values for least square fit, then fill the
// matrices with the values.
//

  for(i=0;i<2*ORDER+1;i++)
    sumxx[i] = 0.0;
  for(i=0;i<ORDER+1;i++)
    sumxy[i] = 0.0;

  for(i=start_index;((i<end_index)&&(work_flag));i++)
  {
    if(w_ang[i] != 999.0)
    { // if point is not missing.
      x_pow = 1.0;
      for (j=0;j<2*ORDER+1;j++)
      {
        sumxx[j] += x_pow;
        x_pow = x_pow * time[i];
      }
    } // if point is not missing.
  } // Loop over data set.

  for(i=start_index;i<end_index;i++)
  {
    if(w_ang[i] != 999.0)
    {
      x_pow = w_ang[i];
      for(j=0;j<ORDER+1;j++)
      {
        sumxy[j] += x_pow;
        x_pow = x_pow*time[i];
      }
    } // End of if.
  } // End of loop over data set..

  a_mat.assign(ORDER+1,ORDER+1);
  for(i=1;i<=ORDER+1;i++)
    for(j=1;j<=ORDER+1;j++)
      a_mat(i,j) = sumxx[(i-1)+(j-1)];

  y_mat.assign(ORDER+1,1);
  for(i=1;i<=ORDER+1;i++)
    y_mat(i,1) = sumxy[i-1];

//
// Solve.
//

  if(work_flag)
  {
//    coef_mat = matrix_solver(a_mat, y_mat);
    b_mat = a_mat.inv();
    coef_mat = b_mat * y_mat;
  }

//
// Construct smooth elev from fit curve.
//

  for(i=0;((i<end_index)&&(work_flag));i++)
  {
    if(i<start_index)
      elev_smooth[i] = w_ang[i];
    else
    {
      num = coef_mat(1,1);
      elev_smooth[i] = num;
      x_pow = 1.0;

      for(j=2;j<=ORDER+1;j++)
      {
        num = coef_mat(j,1);
        x_pow = x_pow * time[i];
        elev_smooth[i] += x_pow*num;
      }
    } // Else
//cout << elev_smooth[i] << endl;
  } // For i.


//
// Calculate residuals.
//

  for(i=0;((i<end_index)&&(work_flag));i++)
  {
    if(!angle_status[i])
    {
      residual[i] = w_ang[i] - elev_smooth[i];
    }
    else
      residual[i] = 0.0;
  } //for i;

//
// The Fourier series required that every point must have a value.
// For missing data n=and outliers and interoplated residual will
// be found.
//

  for(i=start_index;((i<end_index)&&(work_flag));i++)
  {
    if(angle_status[i] != 0)
    { // Have an outlier or missing pt.

      j = i-1;
      while((angle_status[j] != 0) && (j>=0))
        j--;

      k=i+1;
      while((angle_status[k] != 0) && (k<npts))
        k++;

      if((j<0) || (k==npts))
        residual[i] = 0.0;
      else
        residual[i] = residual[k] + ((double)(i-k)*(residual[j]-residual[k]))/
                                    (double)(j-k);

    } // Have an outlier or missing pt.
  }


//  for(i=0;i<npts;i++)
//   cout << angle_status[i] << endl;

//
// Finite Fourier series on residuals.
//

// correct starting point, need an even number of points.

  if(work_flag)
  {
    if(((end_index-start_index) % 2) == 1)
      start_index -= 1;

    num_sample_pts = end_index-start_index;
    num_freq = num_sample_pts/2;
    delta_t = time[npts-1]/(npts-1);
    fund_freq = 1.0/((double)(num_sample_pts)*delta_t);
    orgin_f = start_index+num_freq;

    for(i=0;i<=num_freq;i++)
    {
      a_o_coef[i] = 0.0;
      b_o_coef[i] = 0.0;
      for(j=-num_freq;j<num_freq;j++)
      {
        theta = (2.0*M_PI*(double)(i*j))/(double)(num_sample_pts);
        a_o_coef[i] += residual[orgin_f+j]*cos(theta);
        b_o_coef[i] += residual[orgin_f+j]*sin(theta);
      } // for j
      a_o_coef[i] = a_o_coef[i]/num_sample_pts;
      b_o_coef[i] = b_o_coef[i]/num_sample_pts;
    } // for i

    for(i=0;i<=num_freq;i++)
    {
      a_s_coef[i] = a_o_coef[i];
      b_s_coef[i] = b_o_coef[i];
    }

//
// Notch filters.
//

    for(i=1;i<=num_freq;i++)
      if(((double)(i)*fund_freq) >= (1.0/30.0))
      {
        a_s_coef[i] = a_o_coef[i]*1.0e-1;
        b_s_coef[i] = b_o_coef[i]*1.0e-1;
      }

/*
    if(azim_flag == 0)
    {
      for(i=1;i<=num_freq;i++)
        if((((double)(i)*fund_freq) >= (1.0/UPPER_NOTCH_LIMIT)) &&
          (((double)(i)*fund_freq) <= (1.0/LOWER_NOTCH_LIMIT)))

        {
          a_s_coef[i] = a_o_coef[i]*1.0e-10;
          b_s_coef[i] = b_o_coef[i]*1.0e-10;
        }
    }
*/

//
// Reconstruct smooth elev from fit curve and freq coef.
// elev_smooth is the fitted curve.
//

//cout << fund_freq << endl;
//for(i=0;i<=num_freq;i++)
//  cout << a_f_coef[i] << " " << b_f_coef[i] << endl;


     for(i=-num_freq;i<num_freq;i++)
     { // For i for final reconstruct.
       val_o = a_o_coef[0];
       val_s = a_s_coef[0];
       theta = 2.0*M_PI*fund_freq*delta_t*(double)(i);
       for(j=1;j<num_freq;j++)
       {
         val_o += 2.0*(a_o_coef[j]*cos((double)(j)*theta) +
            b_o_coef[j]*sin((double)(j)*theta));
         val_s += 2.0*(a_s_coef[j]*cos((double)(j)*theta) +
            b_s_coef[j]*sin((double)(j)*theta));
       }
       val_o += a_o_coef[num_freq]*cos(2.0*M_PI*(double)(num_freq*i)*fund_freq*delta_t);
       val_s += a_s_coef[num_freq]*cos(2.0*M_PI*(double)(num_freq*i)*fund_freq*delta_t);

       if((orgin_f+i) <= (start_index + START_SPACE))
       {
         elev_smooth[orgin_f+i] += ((val_o-val_s)*(start_index-(orgin_f+i))+
                START_SPACE*val_o)/START_SPACE;
       }
       else  if ((orgin_f+i) >= (end_index - END_SPACE))
       {
         elev_smooth[orgin_f+i] += ((val_s*(END_SPACE+1))+(end_index-END_SPACE-
              (orgin_f+i))*(val_s-val_o))/(END_SPACE+1);
       }
       else
         elev_smooth[orgin_f+i] += val_s;

    } // For i for final reconstruct.
  } else {
    for(i=0;i<npts;i++) { elev_smooth[i] = w_ang[i]; }
  }
}

/**
 * Smooth the azimuth angles in the data array.
 * @param data The array containing the sounding data.
 * @param lim_ang The list of limiting angles.
 * @param size The number of observations in the data array.
 * @param azim_smooth The list to hold the new smoothed angles.
 * @param angle_status The list to hold the azimuth angle flags.
 **/
void smooth_azimuth_angles(SOUND_DATA_ARR data, double lim_ang[360], int size,
			   double azim_smooth[MAX_OB], int angle_status[MAX_OB]) {

  double azim_list[MAX_OB], time_list[MAX_OB];

  for(int i = 0; i < MAX_OB; i++) { 
    azim_list[i] = i < size ? data[i].azim : 999.0; 
    angle_status[i] = (fabs(azim_list[i] - 999.0) < 0.1) ? 9 : 0;
    time_list[i] = data[i].time;
  }

  double offset = 0.0;

  for(int i = 1; i < size; i++) {
    int j = i - 1;

    while(angle_status[j] == 9 && j >= 0) { j--; }

    if(j != -1 && angle_status[i] != 9) {
      double diff = data[j].azim - data[i].azim;
      if(fabs(diff) > 340.0) { offset +=  360.0*(diff/fabs(diff)); }
      azim_list[i] = data[i].azim + offset;
    }
  }

  smooth_angle_list(azim_list,azim_smooth,time_list,size,angle_status,lim_ang,data,1);

  for(int i = 0; i < size; i++) {
    while(azim_smooth[i] < 0.0) { azim_smooth[i] += 360.0; }
    while(azim_smooth[i] >= 360.0) { azim_smooth[i] -= 360.0; }
  }
}

/**
 * Smooth the elevation angles in the data array.
 * @param data The array containing the sounding data.
 * @param lim_ang The list of limiting angles.
 * @param size The number of observations in the data array.
 * @param elev_smooth The list to hold the new smoothed angles.
 * @param angle_status The list to hold the azimuth angle flags.
 **/
void smooth_elevation_angles(SOUND_DATA_ARR data, double lim_ang[360], int size,
			     double elev_smooth[MAX_OB], int angle_status[MAX_OB]) {

  double elev_list[MAX_OB], time_list[MAX_OB];

  for(int i = 0; i < MAX_OB; i++) { 
    elev_list[i] = i < size ? data[i].elev : 999.0; 
    angle_status[i] = (fabs(elev_list[i] - 999.0) < 0.1) ? 9 : 0;
    time_list[i] = data[i].time;
    elev_smooth[i] = 999.0;
  }

  smooth_angle_list(elev_list,elev_smooth,time_list,size,angle_status,lim_ang,data,0);
}

/**
 * Write the class data in the data array to the output file.
 * @param outfile The output stream where data is to be written.
 * @param data The data array containing the sounding data to be written.
 * @param size The number of entries in the data array.
 **/
void write_class_data(ofstream& outfile, SOUND_DATA_ARR data, int size) {
  for (int i = 0; i < size; i++) {
    char outline[150];
    sprintf(outline,"%6.1f %6.1f %5.1f %5.1f %5.1f %6.1f %6.1f %5.1f %5.1f %5.1f %8.3f %7.3f %5.1f %5.1f %7.1f %4.1f %4.1f %4.1f %4.1f %4.1f %4.1f",
	    data[i].time,data[i].press,data[i].temp,data[i].dewpt,data[i].rh,
	    data[i].u_cmp,data[i].v_cmp,data[i].wind_spd,data[i].wind_dir,
	    data[i].z_cmp,data[i].lon,data[i].lat,data[i].elev,data[i].azim,
	    data[i].alt,data[i].qp,data[i].qt,data[i].qrh,data[i].qu,data[i].qv,
	    data[i].qz);
    outfile << outline <<endl;
  }
}

/**
 * Write the header data to the output file.
 * @param out_file The output stream for the header data.
 * @param log_file The log file for the error log.
 * @param header The container that contains the header data.
 * @return If the writing of the header was successful or not.
 **/
int write_header_data(ofstream& out_file, ofstream& log_file, HEADER *header) {
  int success = 1;
  char out_file_name[20] = "";
  char write_line[150] = "";

  // Create output file name.
  sprintf(out_file_name,"%s_%04d%02d%02d%02d%02d.cls",header->call_letters,
	  header->rel_year,header->rel_month,header->rel_day,header->rel_hour,
	  header->rel_min);
  
  out_file.open(out_file_name,ios::out|ios::trunc);
  if(!out_file) {
      cerr <<"Error opening output file "<< out_file_name<<endl;
      log_file <<"Error opening output file "<< out_file_name<<endl;
      success = 0;
  } else {
    cout << "Output file is: " << out_file_name << endl << endl;
    log_file << "Output file is: " << out_file_name << endl << endl;

    strcpy(write_line,"Data Type:                         National Weather Service Sounding.");
    out_file << write_line << endl;

    strcpy(write_line,"Project ID:                        0");
    out_file << write_line << endl;

    sprintf(write_line,"Release Site Type/Site ID:         %s %s",header->call_letters,header->site_name);
    out_file << write_line << endl;
    log_file << write_line << endl;

    sprintf(write_line,"Release Location (lon,lat,alt):    %d %05.2f'%s, %d %04.2f'%s, %6.1f, %5.1f, %6.1f",
            (int)fabs(header->lon),60*(fabs(header->lon)-(int)fabs(header->lon)),
            header->lon > 0 ? "E" : "W",(int)fabs(header->lat),
            60*(fabs(header->lat)-(int)fabs(header->lat)),header->lat > 0 ? "N" : "S",
            header->lon,header->lat,header->elev);
    out_file << write_line << endl;
    log_file << write_line << endl;

    sprintf(write_line,"UTC Release Time (y,m,d,h,m,s):    %04d, %02d, %02d, %02d:%02d:00",
            header->rel_year < 100 ? 1900 + header->rel_year : header->rel_year,
            header->rel_month,header->rel_day, header->rel_hour, header->rel_min);
    out_file << write_line << endl;
    log_file << write_line << endl;

    sprintf(write_line,"Ascension No:                      %d",header->ascen_num);
    out_file << write_line << endl;
    log_file << write_line << endl;

    sprintf(write_line,"Radiosonde Serial Number:          %s",header->serial_number);
    out_file << write_line << endl;

    sprintf(write_line,"Radiosonde Manufacturer:           %s",header->sonde_type);
    out_file << write_line << endl;
    log_file << write_line << endl;

    if(header->rel_day == 99){
      strcpy(write_line,"UTC Release Time is missing");
      out_file << write_line << endl;
      log_file << write_line << endl;
      strcpy(write_line,"/");
      out_file << write_line << endl;
      out_file << write_line << endl;
    }else{
      strcpy(write_line,"/");
      out_file << write_line << endl;
      out_file << write_line << endl;
      out_file << write_line << endl;
    }

    sprintf(write_line,"Nominal Release Time (y,m,d,h,m,s):%04d, %02d, %02d, %02d:00:00",
            header->nom_rel_year < 100 ? 1900 + header->nom_rel_year : header->nom_rel_year,
            header->nom_rel_month,header->nom_rel_day, header->nom_rel_hour);
    out_file << write_line <<endl;

    strcpy(write_line," Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat   Ele   Azi    Alt    Qp   Qt   Qrh  Qu   Qv   QdZ");
    out_file << write_line <<endl;

    strcpy(write_line,"  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     m    code code code code code code");
    out_file << write_line <<endl;

    strcpy(write_line,"------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----");
    out_file << write_line <<endl;
  }
  return success;
}




int main(int argc, char *argv[]) {
  ifstream ifile, n_l_t_file;
  ofstream ofile, logfile;
  SOUND_DATA_ARR sounding_raw_data;
  HEADER header;

  char line[LINE_LEN+1], log_file[20], outline[75];
  double elev_smooth[MAX_OB], azim_smooth[MAX_OB], work_array[MAX_OB];
  double angtime[MAX_OB], x_pos[MAX_OB], y_pos[MAX_OB], xs[MAX_OB], ys[MAX_OB];
  double limit_angle[360];
  float sur_wind_speed, sur_wind_dir;
  int elev_angle_status[MAX_OB], azim_angle_status[MAX_OB];
  int angle_status[MAX_OB], qxy[MAX_OB];
  time_t t1;

  initialize_data(sounding_raw_data,&header);
  sprintf(log_file,"%s.log",argv[1]);

  if(argc != 2) {
    cerr << "Error in command line format" << endl;
    exit(1);
  }

  ifile.open(argv[1],ios::in|ios::nocreate);
  if(!ifile) {
    cerr << "Failed to open input file '" << argv[1] << "'." << endl;
    exit(1);
  }

  logfile.open(log_file,ios::out);
  if(!logfile) {
    cerr << "Failed to open log file '" << log_file << "'." << endl;
    exit(1);
  }

  time(&t1);
  sprintf(outline,"GMT: %s\n", asctime(gmtime(&t1)));
  logfile << outline ;


  int num_obs = 0;
  if (read_header(ifile,ofile,logfile,argv[1],limit_angle,&sur_wind_speed, &sur_wind_dir,&header)) {
    int num_ang_obs = 0;
    int num_met_obs = read_met_data(ifile,logfile,sounding_raw_data,header.term_alt);
    
    num_ang_obs = read_angle_data(ifile,sounding_raw_data);

    int i = 0;
    if(header.term_alt > 0){
      while(((sounding_raw_data[i].alt < (header.term_alt-20.0)) ||
	     (fabs(sounding_raw_data[i].alt - 99999.0) < 0.1))
	    && (i < MAX_OB))
	i++;
    }else{
      while(fabs(sounding_raw_data[i].alt - 99999.0) > 0.1 && (i < MAX_OB))
	i++;
    }

    num_obs = i+1;
    
    if(num_obs > num_met_obs) { num_obs = num_met_obs; }

    if (num_ang_obs > 0) {
      smooth_elevation_angles(sounding_raw_data,limit_angle,num_obs,elev_smooth,
			      elev_angle_status);
      smooth_azimuth_angles(sounding_raw_data,limit_angle,num_obs,azim_smooth,
			    azim_angle_status);

      for (int i = 0; i < MAX_OB; i++) {
        if (azim_angle_status[i] == 9 || elev_angle_status[i] == 9) {
          angle_status[i] = 9;
        } else if (azim_angle_status[i] == 4 || elev_angle_status[i] == 4) {
          angle_status[i] = 4;
        } else {
          angle_status[i] = 0;
        }
      }

      for (int i = 0; i < MAX_OB; i++) {
        work_array[i] = i < num_obs ? sounding_raw_data[i].alt : 99999.0;
      }
      calculate_position(elev_smooth,azim_smooth,work_array,x_pos,y_pos,qxy,num_obs);

      calculate_uv_components(logfile,sounding_raw_data,x_pos,y_pos,xs,ys,qxy,angle_status,num_obs);

      calculate_lat_and_lon(sounding_raw_data,xs,ys,header.lon,header.lat,qxy);

      calculate_wind_speed_and_direction(sounding_raw_data);
    }

    /* Set the base values for the first entry as read in by the header. */
    sur_wind_speed *= 0.51444;
    sounding_raw_data[0].wind_spd = sur_wind_speed;
    sounding_raw_data[0].wind_dir = sur_wind_dir;
    sounding_raw_data[0].u_cmp = sin((sur_wind_dir+180.0)*M_PI/180.0)*sur_wind_speed;
    sounding_raw_data[0].v_cmp = cos((sur_wind_dir+180.0)*M_PI/180.0)*sur_wind_speed;
    sounding_raw_data[0].qu = 99.0;
    sounding_raw_data[0].qv = 99.0;
    sounding_raw_data[0].lon = header.lon;
    sounding_raw_data[0].lat = header.lat;

    if (HUM_COR_FLAG) {
      if (strncmp(header.sonde_type,"VIZ B2",6) == 0) {
//        cout << "No humidity correction for a " << header.sonde_type << " sonde." << endl;
      } else if (strncmp(header.sonde_type,"VIZ",3) == 0) {
        correct_humidity(sounding_raw_data,header.rh33_res,header.nom_rel_year,
			 header.nom_rel_month,num_obs);
//      } else {
//        cout << "You have a " << header.sonde_type << " sonde." << endl;
      }
    }

    calculate_dew_point(sounding_raw_data);
    calculate_ascension_rates(logfile,sounding_raw_data);
  }

  write_class_data(ofile,sounding_raw_data,num_obs);
  ifile.close();
  ofile.close();
  logfile.close();
  return(0);
}
