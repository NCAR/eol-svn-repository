/**
 * <p>This is the file that contains the functions to run the auto QC for
 * checking sounding data in CLASS format.</p>
 *
 * @author Joel Clawson - 2005/01/19 Refactored to make it easier to read and understand.
 * For older revision history before it was refactored, look at the document in the docs
 * directory called old_software_history.txt.
 *
 * @author Joel Clawson - 2005/05/11 Added in the decreasing_time_check function and the
 * logic via -DDROP for qc'ing drop sonde data.
 *
 * @author Joel Clawson - 2005/05/13 Added in limit variables BAD_RAPID_PRESS_INC and
 * QUEST_RAPID_PRESS_INC for the rapid pressure change check instead of having the
 * values hard coded into this file.
 **/
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <math.h>
#include <iostream.h>
#include <fstream.h>
#include <string.h>

#include "autoqc.h"

#include "limits.h"

/**
 * Move the altitude value in the input node to the output node if it has a valid value.
 *
 * @param output The node that will store that data.
 * @param input The node that contains the raw file data.
 * @param err_file The output stream that errors are written to.
 **/
void check_altitude_field(class_node& output, class_node& input, ofstream& err_file) {
  if(input.alt < 99999.9 && input.alt > -9999.9) {
    output.alt = input.alt;
  } else {
    char outline[100];
    sprintf(outline,"altitude at pressure: %6.1f mb is: %7.1f meters.\n",
	    input.press,input.alt);
    err_file << outline;
  } 
}

/**
 * Test the altitude value to make sure that it is in an expected range and flag
 * it if it is not in the range.
 *
 * @param node The node to be tested.
 * @param err_file The output stream where errors are written.
 **/
void check_altitude_range(class_node& node, ofstream& err_file) {
  if ((node.alt > QUEST_MAX_ALT) || (node.alt < QUEST_MIN_ALT)) { 
    char outline[100];
    sprintf(outline,"Alt at press: %6.1f is: %7.1f m.\n",node.press,node.alt); 
    err_file << outline;
    mark_flags(&node,2,1,1,1,0,0,0); 
  }
}

/**
 * Move the ascension rate value and flag in the input node to the output node if 
 * it has a valid value.
 *
 * @param output The node that will store that data.
 * @param input The node that contains the raw file data.
 * @param err_file The output stream that errors are written to.
 **/
void check_ascension_rate_field(class_node& output, class_node& input, ofstream& err_file) {
  if(input.asc_rate < -99.9){
    char outline[100];
    sprintf(outline,"Ascendrate at pressure: %6.1f mb is: %7.1f m/s.\n",
	    input.press,input.asc_rate);
    err_file << outline;
    output.asc_rate = -99.9;
    output.asc_rate_flag = 4.0;
  } else if (input.asc_rate > 999.9) {
    output.asc_rate = 999.0;
    output.asc_rate_flag = 9.0;
    mark_flags(&output,3,1,1,1,1,1,0);
    char outline[100];
    sprintf(outline,"Ascendrate at pressure: %6.1f mb is: %7.1f m/s.\n",
	    input.press,input.asc_rate);
    err_file << outline;
  } else {
    output.asc_rate = input.asc_rate;
    output.asc_rate_flag = input.asc_rate_flag;
  }
}

/**
 * Test the ascension rate value to make sure that it is in an expected range and flag
 * it if it is not in the range.
 *
 * @param node The node to be tested.
 * @param err_file The output stream where errors are written.
 **/
void check_ascension_rate_range(class_node& node, ofstream& err_file) {
  if (fabs(node.asc_rate) > QUEST_MAX_ASC_RATE) { 
    char outline[100];
    sprintf(outline,"Ascendrate at press: %6.1f is: %7.1f m/s.\n",node.press,node.asc_rate);
    err_file << outline; 
    mark_flags(&node,2,1,0,0,0,0,0); 
  }
}

/**
 * Move the dew point value in the input node to the output node if it has a valid value.
 *
 * @param output The node that will store that data.
 * @param input The node that contains the raw file data.
 * @param err_file The output stream that errors are written to.
 **/
void check_dew_point_field(class_node& output, class_node& input, ofstream& err_file) {
  if(input.dewpt < 999.9 && input.dewpt > QUEST_MIN_DEWPT) {
    output.dewpt = input.dewpt;
  } else if(input.dewpt > 999.9) {
    char outline[100];
    sprintf(outline,"DewPt at pressure: %6.1f mb is: %7.1f deg .C.\n",
	    input.press,input.dewpt);
    err_file << outline;
    output.dewpt = 999.0;
  } else {
    char outline[100];
    sprintf(outline,"DewPt at pressure: %6.1f mb is: %7.1f deg .C.\n",
	    input.press,input.dewpt);
    err_file << outline;
    output.dewpt = -99.9;
    output.rh_flag = 2.0;
  }
}

/**
 * Test the dew point value to make sure that it is in an expected range and flag
 * the relative humidity if it is not in the range.
 *
 * @param node The node to be tested.
 * @param err_file The output stream where errors are written.
 **/
void check_dew_point_range(class_node& node, ofstream& err_file) {
  if(node.rh == 999.0) {
    char outline[100];
    sprintf(outline,"Dew point with missing RH at press: %6.1f.\n",node.press);
    err_file << outline;
  }

  if(node.dewpt > QUEST_MAX_DEWPT) {
    char outline[100];
    sprintf(outline,"DewPt at pressure: %6.1f is: %6.1f deg C.\n",node.press,node.dewpt);
    err_file << outline;
    if(node.dewpt >= MISS_MAX_DEWPT) {
      node.dewpt = 999.0;
      node.rh = 999.0;
      mark_flags(&node,9,0,0,1,0,0,0);
    } else { mark_flags(&node,2,0,0,1,0,0,0); }
  }
   
  if(node.dewpt > node.temp) {
    char outline[100];
    sprintf(outline,"Dew point greater than Temp at press: %6.1f.\n",node.press);
    err_file << outline;
    mark_flags(&node,2,0,0,1,0,0,0);
  }
}

/**
 * Move the elevation and azimuth fields in the input node to the output node if it 
 * has a valid value.
 *
 * @param output The node that will store that data.
 * @param input The node that contains the raw file data.
 * @param err_file The output stream that errors are written to.
 **/
void check_elevation_azimuth_fields(class_node& output, class_node& input, 
				    ofstream& err_file) {
  if(input.elev < 999.9 && input.elev > -99.9) {
    output.elev = input.elev;
  } else {
    char outline[100];
    sprintf(outline,"Elev ang at pressure: %6.1f mb is: %7.1f deg.\n",
	    input.press,input.elev);
    err_file << outline;
    output.elev = 999.0;
  }

  if(input.azim < 999.9 && input.azim > -99.9) {
    output.azim = input.azim;
  } else {
    char outline[100];
    sprintf(outline,"Azim ang at pressure: %6.1f mb is: %7.1f deg.\n",
	    input.press,input.azim);
    err_file << outline;
    output.azim = 999.0;
  }  
}

/**
 * Move the latitude value in the input node to the output node if it has a valid value.
 *
 * @param output The node that will store that data.
 * @param input The node that contains the raw file data.
 * @param err_file The output stream that errors are written to.
 **/
void check_latitude_field(class_node& output, class_node& input, ofstream& err_file) {
  if(input.lat < 999.9 && input.lat > -99.9) {
    output.lat = input.lat;
  } else {
    output.lon = 999.0;
    char outline[100];
    sprintf(outline,"Latitude at pressure: %6.1f mb is: %7.1f deg.\n",input.press,input.lat);
    err_file << outline;
  }
}

/**
 * Move the longitude value in the input node to the output node if it has a valid value.
 *
 * @param output The node that will store that data.
 * @param input The node that contains the raw file data.
 * @param err_file The output stream that errors are written to.
 **/
void check_longitude_field(class_node& output, class_node& input, ofstream& err_file) {
  if(input.lon < 9999.9 && input.lon > -999.9) {
    output.lon = input.lon;
  } else {
    output.lon = 9999.0;
    char outline[100];
    sprintf(outline,"Longitude at pressure: %6.1f mb is: %7.1f deg.\n",
	    input.press,input.lon);
    err_file << outline;
  }
}

/**
 * Check the maximum and minimum values for all of the values in the data array.
 *
 * @param err_file The output stream where errors are written.
 * @param data_array The array of data to be checked.
 * @param size The number of entries in the data array.
 **/
void check_max_min(ofstream& err_file, CLASS_ARRAY data_array,int size) { 
  for(int index = 0; index < size; index++) { 

    if (data_array[index].press != 9999.0)   
      { check_pressure_range(data_array[index],index+1,err_file); }
    if (data_array[index].alt != 99999.0)    
      { check_altitude_range(data_array[index],err_file); }
    if (data_array[index].asc_rate != 999.0) 
      { check_ascension_rate_range(data_array[index],err_file); }
    if (data_array[index].wind_spd != 999.0) 
      { check_wind_speed_range(data_array[index],err_file); }
    if (data_array[index].wind_dir != 999.0) 
      { check_wind_direction_range(data_array[index],err_file); }
    if (data_array[index].u_comp != 9999.0)  
      { check_uwind_range(data_array[index],err_file); }
    if (data_array[index].v_comp != 9999.0)  
      { check_vwind_range(data_array[index],err_file); }
    if (data_array[index].temp != 999.0)     
      { check_temperature_range(data_array[index],err_file); }
    if (data_array[index].rh != 999.0)       
      { check_relative_humidity_range(data_array[index],err_file); }
    if (data_array[index].dewpt != 999.0)    
      { check_dew_point_range(data_array[index],err_file); }
    if (data_array[index].time != 999.0)     
      { check_time_range(data_array[index],err_file); }
  }
}

/**
 * Move the pressure value and flag in the input node to the output node if it 
 * has a valid value.
 *
 * @param output The node that will store that data.
 * @param input The node that contains the raw file data.
 * @param line_number The number of the raw file entry.
 * @param err_file The output stream that errors are written to.
 **/
void check_pressure_field(class_node& output, class_node& input, int line_number, 
			  ofstream& err_file) {
  if((input.press < 9999.9) && (input.press > -999.9)) {
    output.press = input.press;
    output.press_flag = input.press_flag;
  } else {
    char outline[100];
    sprintf(outline,"Press: %7.1f mb on line %4d .\n",input.press,line_number);
    err_file << outline;
    output.press = 9999.0;
    output.press_flag = 9.0;
  }
}

/**
 * Test the pressure value to make sure that it is in an expected range and flag
 * it if it is not in the range.
 *
 * @param node The node to be tested.
 * @param err_file The output stream where errors are written.
 **/
void check_pressure_range(class_node& node, int line_number, ofstream& err_file) {
  if ((node.press > QUEST_MAX_PRESS && node.press < MISS_MAX_PRESS) || 
      node.press <= QUEST_MIN_PRESS) {
    char outline[100];
    sprintf(outline,"Press: %6.1f mb on line %4d.\n",node.press,line_number);
    err_file << outline;
    mark_flags(&node,3,1,0,0,0,0,0); // Set pressure flag to bad
  } else if (node.press >= MISS_MAX_PRESS || node.press < 0) {
    char outline[100];
    sprintf(outline,"Press: %6.1f mb on line %4d.\n",node.press,line_number);        
    err_file << outline;
    node.press = 9999.0; // Set pressure to missing
    mark_flags(&node,9,1,0,0,0,0,0); // Set pressure flag to missing
  } 
}

/**
 * Check the different rates for the data in the array.
 *
 * @param err_file The output stream where errors are to be written.
 * @param data_array The array containing the data to be checked.
 * @param size The size of the array of data.
 **/
void check_rates(ofstream& err_file, CLASS_ARRAY data_array, int size){
  int nws_check = 0;
#if NWS
  nws_check = 1;
#endif

  int add = getAveragingValue();
  int p_err = 0, begin = 1;
  float prev_time = 9999.0;
  float prev_asc_rate;

  for(int i = 0; i+1 < size; i++){
    int next = i+add;
    if(next < size){
      p_err = 0;

      if(data_array[i].press != 9999.0 && data_array[next].press != 9999.0) {

	if((nws_check && data_array[next].press > 100) || !nws_check) {
          if(data_array[i].time != 9999.0 && data_array[next].time != 9999.0 ) {
	    rapid_pressure_check(err_file,data_array,i,next);
	    
	    if(data_array[i].alt != 99999.0 && data_array[next].alt != 99999.0) {
	      prev_asc_rate = differential_ascension_rate(err_file,data_array,i,next,
							  prev_asc_rate,begin);
	      if (begin) { begin = 0; }
	    }
          }
        }

        if(data_array[i].alt != 99999.0 && data_array[next].alt != 99999.0) {

	  // Check for surface super adiabatic lapse rates
	  if (fabs(data_array[i].press - data_array[0].press) >= 10) {
	    super_adiabatic_lapse_rate_check(err_file,data_array,i,next,
					     QUEST_LAPSE_RATE,BAD_LAPSE_RATE);
	  } else {
	    super_adiabatic_lapse_rate_check(err_file,data_array,i,next,
					     SFC_QUEST_LAPSE_RATE,SFC_BAD_LAPSE_RATE);
	  }

	  rapid_temperature_check(err_file,data_array,i,next);
        }

        if (data_array[i+1].time != 9999.0 && data_array[i].time != 9999.0 && 
	    data_array[i+1].press != 9999.0) {
	  p_err = decrease_pressure_check(err_file,data_array,i);
	}
	increasing_altitude_check(err_file,data_array[i],data_array[i+1],p_err);
      }
      
      #ifdef DROP
      prev_time = decreasing_time_check(err_file,data_array[i],data_array[i+1],prev_time);
      #else
      prev_time = increasing_time_check(err_file,data_array[i],data_array[i+1],prev_time);
      #endif
    }
  }
}

/**
 * Move the relative humidity value and flag in the input node to the output node if 
 * it has a valid value.
 *
 * @param output The node that will store that data.
 * @param input The node that contains the raw file data.
 * @param err_file The output stream that errors are written to.
 **/
void check_relative_humidity_field(class_node& output, class_node& input, 
				   ofstream& err_file) {
  if(input.rh < 999.9 && input.rh > -99.9) {
    output.rh = input.rh;
    output.rh_flag = input.rh_flag;
  } else {
    char outline[100];
    sprintf(outline,"RH at pressure: %6.1f mb is: %7.1f %.\n",input.press,input.rh);
    err_file << outline;
    output.rh = 999.0;
    output.rh_flag = 9.0;
  }
}

/**
 * Test the relative humidity value to make sure that it is in an expected range and flag
 * it if it is not in the range.
 *
 * @param node The node to be tested.
 * @param err_file The output stream where errors are written.
 **/
void check_relative_humidity_range(class_node& node, ofstream& err_file) {
  if (node.rh > 100.0) {
    char outline[100];
    sprintf(outline,"RH at press: %6.1f is: %6.1f %.\n",node.press,node.rh);
    err_file << outline;
    mark_flags(&node,2,0,0,1,0,0,0); // Questionable RH flag
  } else if (node.rh < 0.0) {
    char outline[100];
    sprintf(outline,"RH at press: %6.1f is: %6.1f %.\n",node.press,node.rh);
    err_file << outline;
    node.rh = 999.0; // set rh to missing
    node.dewpt = 999.0; // set dewpt to missing
    mark_flags(&node,9,0,0,1,0,0,0); // Missing RH flag
  } else {
    // Make dew point and temperature equal if RH = 100%
    if (fabs(node.rh - 100) < .001 && node.temp != node.dewpt) {
      node.dewpt = node.temp; 
    }
  }
}

/**
 * Move the temperature value and flag in the input node to the output node if it 
 * has a valid value.
 *
 * @param output The node that will store that data.
 * @param input The node that contains the raw file data.
 * @param err_file The output stream that errors are written to.
 **/
void check_temperature_field(class_node& output, class_node& input, ofstream& err_file) {
  if(input.temp < 999.9 && input.temp > -99.9){
    output.temp = input.temp;
    output.temp_flag = input.temp_flag;
  } else {
    char outline[100];
    sprintf(outline,"Temp at pressure: %6.1f mb is: %7.1f deg .C.\n",input.press,input.temp);
    err_file << outline;
    output.temp = 999.0;
    output.temp_flag = 9.0;
  }
}

/**
 * Test the temperature value to make sure that it is in an expected range and flag
 * it if it is not in the range.
 *
 * @param node The node to be tested.
 * @param err_file The output stream where errors are written.
 **/
void check_temperature_range(class_node& node, ofstream& err_file) {
  if((node.temp > QUEST_MAX_TEMP || node.temp < QUEST_MIN_TEMP) && 
     node.temp < MISS_MAX_TEMP) {
    char outline[100];
    sprintf(outline,"Temp at press: %6.1f is: %6.1f deg .C.\n",node.press,node.temp);
    err_file << outline;
    mark_flags(&node,2,0,1,0,0,0,0);  // Set Temp Flag to Questionable
  } else if (node.temp >= MISS_MAX_TEMP) {
    char outline[100];
    sprintf(outline,"Temp at press: %6.1f is: %6.1f deg .C.\n",node.press,node.temp);
    err_file << outline;
    node.temp = 999.0; // Set temperature  and dewpt to missing
    node.dewpt = 999.0;
    mark_flags(&node,9,0,1,0,0,0,0); // Missing temperature flag
  }
}

/**
 * Check to see if the time value for the entry is missing.
 * 
 * @param output The storage class_node of the data.
 * @param intput The class_node that was read in from an input stream.
 * @param err_file The stream where errors are to be written.
 **/
void check_time_field(class_node& output, class_node& input, ofstream& err_file) {
  if(input.time < -999.9) { 
    char outline[100];
    sprintf(outline,"Time at press: %6.1f is: %7.1f sec.\n",input.press,input.time);
    err_file << outline;
    input.time = -999.9;
  }
  output.time = input.time;
}

/**
 * Test the time value to make sure that it is in an expected range.
 *
 * @param node The node to be tested.
 * @param err_file The output stream where errors are written.
 **/
void check_time_range(class_node& node, ofstream& err_file) {
  if (node.time <= -600) {
    char outline[100];
    sprintf(outline,"Time at press: %6.1f is: %6.1f sec.\n",node.press,node.time);
    err_file << outline;
  }
}

/**
 * Move the U and V wind component values and flags in the input node to the output 
 * node if it has a valid value.
 *
 * @param output The node that will store that data.
 * @param input The node that contains the raw file data.
 * @param err_file The output stream that errors are written to.
 **/
void check_uv_fields(class_node& output, class_node& input, ofstream& err_file) {
  if(input.u_comp < 9999.9 && input.u_comp > -999.9 &&
     input.v_comp < 9999.9 && input.v_comp > -999.9) {
    output.u_comp = input.u_comp;
    output.uwind_flag = input.uwind_flag;
    output.v_comp = input.v_comp;
    output.vwind_flag = input.vwind_flag;
  } else {
    char outline[100];
    sprintf(outline,"Ucmp at pressure: %6.1f mb is: %7.1f m/s.\n",input.press,input.u_comp);
    err_file << outline;
    sprintf(outline,"Vcmp at pressure: %6.1f mb is: %7.1f m/s.\n",input.press,input.v_comp);
    err_file << outline;
    output.u_comp = output.v_comp = 9999.0;
    output.uwind_flag = output.vwind_flag = 9.0;
  }
}

/**
 * Test the U wind component value to make sure that it is in an expected range and flag
 * it if it is not in the range.
 *
 * @param node The node to be tested.
 * @param err_file The output stream where errors are written.
 **/
void check_uwind_range(class_node& node, ofstream& err_file) {
  if(fabs(node.u_comp) >= QUEST_MAX_WIND_SPD) {
    char outline[100];
    sprintf(outline,"Ucmp at press: %6.1f is: %6.1f m/s.\n",node.press,node.u_comp);
    err_file << outline;
    if(fabs(node.u_comp) >= BAD_MAX_WIND_SPD) { mark_flags(&node,3,0,0,0,1,0,0); }
    else { mark_flags(&node,2,0,0,0,1,0,0); }
  }
}

/**
 * Test the V wind component value to make sure that it is in an expected range and flag
 * it if it is not in the range.
 *
 * @param node The node to be tested.
 * @param err_file The output stream where errors are written.
 **/
void check_vwind_range(class_node& node, ofstream& err_file) {
  if (fabs(node.v_comp) >= QUEST_MAX_WIND_SPD) {
    char outline[100];
    sprintf(outline,"Vcmp at press: %6.1f is: %6.1f m/s.\n",node.press,node.v_comp);
    err_file << outline;
    if (fabs(node.v_comp) >= BAD_MAX_WIND_SPD) { mark_flags(&node,3,0,0,0,0,1,0); }
    else { mark_flags(&node,2,0,0,0,0,1,0); }
  }
}

/**
 * Test the wind direction value to make sure that it is in an expected range and flag
 * the U and V wind components if it is not in the range.
 *
 * @param node The node to be tested.
 * @param err_file The output stream where errors are written.
 **/
void check_wind_direction_range(class_node& node, ofstream& err_file) {
  if(node.wind_dir < 0.0 || node.wind_dir > 360.0) {
    char outline[100];
    sprintf(outline,"Wind dir at press: %6.1f is: %6.1f deg.\n",node.press,node.wind_dir);
    err_file << outline;
    node.wind_spd = 999.0; // Set total wind spd to missing
    node.wind_dir = 999.0; // Set total wind dir to missing
    node.u_comp = 9999.0; // set u wind component to missing
    node.v_comp = 9999.0; // set v wind component to missing
    mark_flags(&node,9,0,0,0,1,1,0); // Missing Qu and Qv flags
  }
}

/**
 * Move the wind speed and direction values in the input node to the output node if 
 * it has a valid value.
 *
 * @param output The node that will store that data.
 * @param input The node that contains the raw file data.
 * @param err_file The output stream that errors are written to.
 **/
void check_wind_fields(class_node& output, class_node& input, ofstream& err_file) {
  if(input.wind_spd < 999.9 && input.wind_spd > -99.9) {
    output.wind_spd = input.wind_spd;
    if(input.wind_dir < 999.9 && input.wind_dir > -99.9) {
      output.wind_dir = input.wind_dir;
    } else {
      char outline[100];
      sprintf(outline,"Wind dir at pressure: %6.1f mb is: %7.1f deg.\n",
	      input.press,input.wind_dir);
      err_file << outline;
    }
  } else {
    char outline[100];
    sprintf(outline,"Windspd at pressure: %6.1f mb is: %6.1f m/s.\n",
	    input.press,input.wind_spd);
    err_file << outline;
    output.wind_spd = 999.0;
    output.u_comp = output.v_comp = 9999.0;
    output.wind_dir = 999.0;
    output.uwind_flag = output.vwind_flag = 9.0; 
  }
}

/**
 * Test the wind speed value to make sure that it is in an expected range and flag
 * the U and V wind components if it is not in the range.
 *
 * @param node The node to be tested.
 * @param err_file The output stream where errors are written.
 **/
void check_wind_speed_range(class_node& node, ofstream& err_file) {
  if (node.wind_spd >= MISS_MAX_WIND_SPD || node.wind_spd < 0) {
    char outline[100];
    sprintf(outline,"Windspd at press: %6.1f is: %6.1f m/s.\n",node.press,node.wind_spd);
    err_file << outline;
    node.wind_spd = 999.0; // Set total wind spd to missing
    node.wind_dir = 999.0; // Set total wind dir to missing
    node.u_comp = 9999.0; // set u wind component to missing
    node.v_comp = 9999.0; // set v wind component to missing
    mark_flags(&node,9,0,0,0,1,1,0); // Missing Qu and Qv flags
  } else if (node.wind_spd > QUEST_MAX_WIND_SPD || node.wind_spd < QUEST_MIN_WIND_SPD) {
    char outline[100];
    sprintf(outline,"Windspd at press: %6.1f is: %6.1f m/s.\n",node.press,node.wind_spd);
    err_file << outline;
    if(node.wind_spd > QUEST_MIN_WIND_SPD && node.wind_spd < BAD_MAX_WIND_SPD) {
      mark_flags(&node,2,0,0,0,1,1,0);
    } else {
      mark_flags(&node,3,0,0,0,1,1,0);
    }
  }
}

/**
 * Copy the header information from the input file to the output file.
 *
 * @param in_file The stream where the data will be read.
 * @param out_file The stream where the data will be written.
 * @param in_file_name The name of the input file.
 * @return The location in the input stream where the header ended.
 **/
streampos copyheader(ifstream& in_file, ofstream& out_file, char* in_file_name) {
  char line[LINE_LEN+1],outline[LINE_LEN+1];
  cout << "Processing file:  " << in_file_name << endl;

  // Loop through the lines that contain the header information
  int i = 0;
  while(i <  15){
    in_file.getline(line,LINE_LEN,'\n');
    strcpy(outline,line);
    out_file << outline << endl;
    i++;
  }

  return in_file.tellg();
}

/**
 * Check for decreasing pressure.
 *
 * @param err_file The output stream where errors are to be written.
 * @param data_array The array that contains the data to be checked.
 * @param index The index of the entry in the array to be checked.
 * @return A generated error flag.
 **/
int decrease_pressure_check(ofstream& err_file, CLASS_ARRAY data_array, int index) {
  int p_err = 0;

  if(data_array[index].press > DEC_PRESS_CHECK){
    if(data_array[index].press PRESS_SIGN data_array[index+1].press){
      p_err = 1;
      char outline[100];
      sprintf(outline,"Pressure increase or equal at pressure: %6.1f and pressure: %6.1f at time %7.1f sec\n",data_array[index].press,data_array[index+1].press,data_array[index+1].time);
      err_file << outline;
      mark_flags(&data_array[index+1],2,1,1,1,0,0,0);
    } // decreasing pressure check
  } else {
    if(data_array[index].press < data_array[index+1].press){
      p_err = 1;
      char outline[100];
      sprintf(outline,"Pressure increase pressure: %6.1f and pressure: %6.1f at time %7.1f sec\n",data_array[index].press,data_array[index+1].press,data_array[index+1].time);
      err_file << outline;
      mark_flags(&data_array[index+1],2,1,1,1,0,0,0);
    }
  }
  return p_err;
}

/**
 * Check to see if the time is decreasing for a drop sonde.
 *
 * @param err_file The output stream where errors are to be written.
 * @param current The class node that is being checked.
 * @param next The node after the current one.
 * @param last_time The last time that was found.
 * @return The latest time found.
 **/
float decreasing_time_check(ofstream& err_file, class_node& current, class_node& next, float last_time) {
  if(current.time != 9999.0 && next.time != 9999.0) {
    if(current.time <= next.time){ 
      last_time = next.time;
      char outline[100];
      sprintf(outline,"Time increase or equal time: %7.1f and %7.1f\n",
	      current.time,next.time);
      err_file << outline;
    }
  } else if (last_time != 9999.0 && next.time != 9999.0) {
    if(last_time <= next.time){
      last_time = next.time;
      char outline[100];
      sprintf(outline,"Time increase or equal time: %7.1f and %7.1f\n",last_time,next.time);
      err_file << outline;
    }
  } else if (next.time != 9999.0) {
    last_time = next.time;
  }
  
  return last_time;
}

/**
 * Calculate the differential ascension rate.
 *
 * @param err_file The output stream where errors are to be written.
 * @param data_array The array that contains the data to be checked.
 * @param start The starting index to use for checking.
 * @param end The ending index to use for checking.
 * @param previous_rate The previous ascension rate.
 * @param beginning A flag if this is the first calculation of the ascension rate.
 * @return The calculated ascension rate.
 **/
float differential_ascension_rate(ofstream& err_file, CLASS_ARRAY data_array, int start, 
				  int end, float previous_rate, int beginning) {
  float asc_rate = 0.0;
  
  if (data_array[start].time != data_array[end].time &&
      data_array[start].alt  != data_array[end].alt) {
    asc_rate = (data_array[end].alt - data_array[start].alt) / 
      (data_array[end].time - data_array[start].time);
  }
    
  if (!beginning) {
    float rate = previous_rate - asc_rate;  
      
    if(fabs(rate) >= QUEST_ASC_RATE_CHANGE){ 
      int type = (fabs(rate) >= BAD_ASC_RATE_CHANGE) ? 3 : 2;
      
      for (int j = start; j <= end; j++) { mark_flags(&data_array[j],type,1,0,0,0,0,0); }
      
      char outline[100];
      sprintf(outline,"Rapid Ascendrate change between pressure: %6.1f and %6.1f is: %6.1f m/s\n",data_array[start].press,data_array[end].press,rate);
      err_file << outline;
    }
  }
  return asc_rate;
}

/**
 * Get the value used for averaging.
 * 
 * @return The averaging value.
 **/
int getAveragingValue() {
//************************************************************************* 
// -DNUMBER is number of datapoints used for averaging
// ex. -DFOUR means Xi and Xi+4 are end points of averaging where Xi is 
// data point. For .5 sec resolution data this would be 2.0 second average
// The following table illustrates average intervals for selected -DNUMBER 
// choices at selected resolutions
//             1.0  1.2  1.5  2.0  3.0   4.0   5.0   6.0   Resolution (sec) 
//--------------------------------------------------------
// default     1.0  1.2  1.5  2.0  3.0   4.0   5.0   6.0 
// -DTWO       1.0  1.2  1.5  2.0  3.0   4.0   5.0   6.0
// -DTHREE     2.0  2.4  3.0  4.0  6.0   8.0  10.0  12.0
// -DFOUR      3.0  3.6  4.5  6.0  9.0  12.0  15.0  18.0
// -DFIVE      4.0  4.8  6.0  8.0 12.0  16.0  20.0  24.0
// -DSIX       5.0  6.0  7.5 10.0 15.0  20.0  25.0   
// -DSEVEN     6.0  7.2  9.0 12.0 18.0  24.0
// -DEIGHT     7.0  8.4 10.5 14.0 21.0
// -DNINE      8.0  9.6 12.0 16.0 24.0
// -DTEN       9.0 10.8 13.5 18.0
// -DELEVEN   10.0 12.0 15.0 20.0
// -DTWELVE   11.0 13.2 16.5 22.0
// -DTHIRTEEN 12.0 14.4 18.0 24.0     
//*************************************************************************

#if THIRTEEN
  return 12;
#elif TWELVE
  return 11;
#elif ELEVEN
  return 10;
#elif TEN
  return 9;
#elif NINE
  return 8;
#elif EIGHT
  return 7;
#elif SEVEN
  return 6;
#elif SIX
  return 5;
#elif FIVE
  return 4;
#elif FOUR 
  return 3;
#elif THREE
  return 2;
#elif TWO
  return 1;
#else
  return 1;
#endif
}

/**
 * Print out a help screen to the user.
 *
 * @param str The name of the program that is being called.
 **/
void helpscreen(char* str) {
  cout << "Error in the command line format of " << str << endl;
  cout << "The proper format is: " << str << " filename filename.2 " << endl;
  cout << "OR" << endl;
  cout << str << " filename filename.2" << endl;
  cout << "Where filename is a CLASS formated sounding in pressure" << endl;
  cout << "descending order, dropsondes need to be inverted. The " << endl; 
  cout << "output of the program is directed to the screen." << endl;
}

/**
 * Check that the altitude is increasing from data point to data point.
 * 
 * @param err_file The output stream where errors are to be written.
 * @param current The current data point being checked.
 * @param next The data point following the current one.
 * @param p_err An error flag.
 **/
void increasing_altitude_check(ofstream& err_file, class_node& current, class_node& next, 
			       int p_err) {
  if(current.alt != 99999.0 && next.alt != 99999.0 && !p_err && next.alt <= current.alt) {
    char outline[100];
    sprintf(outline,"Altitude decrease or equal at pressure %6.1f between altitude %6.1f and %6.1f\n",next.press,next.alt,current.alt);
    err_file << outline;
    mark_flags(&current,2,1,1,1,0,0,0);
    mark_flags(&next,2,1,1,1,0,0,0);
  }
}

/**
 * Check to see if the time is increasing for an upsonde.
 *
 * @param err_file The output stream where errors are to be written.
 * @param current The class node that is being checked.
 * @param next The node after the current one.
 * @param last_time The last time that was found.
 * @return The latest time found.
 **/
float increasing_time_check(ofstream& err_file, class_node& current, class_node& next, float last_time) {
  if(current.time != 9999.0 && next.time != 9999.0) {
    if(current.time >= next.time){ 
      last_time = next.time;
      char outline[100];
      sprintf(outline,"Time decrease or equal time: %7.1f and %7.1f\n",
	      current.time,next.time);
      err_file << outline;
    }
  } else if (last_time != 9999.0 && next.time != 9999.0) {
    if(last_time >= next.time){
      last_time = next.time;
      char outline[100];
      sprintf(outline,"Time decrease or equal time: %7.1f and %7.1f\n",last_time,next.time);
      err_file << outline;
    }
  } else if (next.time != 9999.0) {
    last_time = next.time;
  }
  
  return last_time;
}

/**
 * Initialize the data array with the data from the input file.
 *
 * @param in_file The input stream containing the raw data.
 * @param err_file The output stream to write errors to.
 * @param file_pos The position in the file where the data begins.
 * @param data_array The array to contain the data.
 * @return The size of the data array.
 **/
int initarray(ifstream& in_file, ofstream& err_file, streampos file_pos, CLASS_ARRAY data_array) {
  class_node raw_node;
  char line[LINE_LEN+1];
  int index = 0;

  /* Move to the first line that contains data. */
  in_file.seekg(file_pos);

  /* Read in the line of data. */
  in_file.getline(line,LINE_LEN,'\n');

  /* Read the data until the end of the file is reached. */
  while(!in_file.eof()) {

    if(strstr(line,"         ") == '\0'){

      /* Read the data line into the node. */
      sscanf(line,"%6f%7f%6f%6f%6f%6f%7f%6f%6f%6f%9f%8f%6f%6f%8f%5f%5f%5f%5f%5f%5f",
	     &raw_node.time,&raw_node.press,&raw_node.temp,&raw_node.dewpt,&raw_node.rh,
	     &raw_node.u_comp,&raw_node.v_comp,&raw_node.wind_spd,&raw_node.wind_dir,
	     &raw_node.asc_rate,&raw_node.lon,&raw_node.lat,&raw_node.elev,&raw_node.azim,
	     &raw_node.alt,&raw_node.press_flag,&raw_node.temp_flag,&raw_node.rh_flag,
	     &raw_node.uwind_flag,&raw_node.vwind_flag,&raw_node.asc_rate_flag);


      /* Can only check the data if the time is a real time. */
      if(raw_node.time < 9999.9) {

	check_time_field(data_array[index],raw_node,err_file);
	check_pressure_field(data_array[index],raw_node,index+1,err_file);
	check_temperature_field(data_array[index],raw_node,err_file);
	check_dew_point_field(data_array[index],raw_node,err_file);
	check_relative_humidity_field(data_array[index],raw_node,err_file);
	check_uv_fields(data_array[index],raw_node,err_file);
	check_wind_fields(data_array[index],raw_node,err_file);
	check_ascension_rate_field(data_array[index],raw_node,err_file);
	check_longitude_field(data_array[index],raw_node,err_file);
	check_latitude_field(data_array[index],raw_node,err_file);
	check_elevation_azimuth_fields(data_array[index],raw_node,err_file);
	check_altitude_field(data_array[index],raw_node,err_file);

	index++;
      }
    }

    /* Read in the next line of data. */
    in_file.getline(line,LINE_LEN,'\n');
  }
  return index;
}

/**
 * Mark all of the flags that are set to true to the specified type of flag.
 * 
 * @param node The node to have the flags set.
 * @param type The type of flag being set.  i.e. 3 for bad.
 * @param press_flag The pressure flag.
 * @param temp_flag The temperature flag.
 * @param rh_flag The relative humidity flag.
 * @param uwind_flag The u wind component flag.
 * @param vwind_flag The v wind component flag.
 * @param rate_flag The ascension rate flag.
 **/
void mark_flags(CLASSPTR node,int type,int press_flag,int temp_flag,int rh_flag,
		int uwind_flag,int vwind_flag,int rate_flag) {
  // Handle the BAD flag
  if(type == 3) {
    if(press_flag && (node->press_flag    != 9.0))   { node->press_flag    = type; }
    if(temp_flag  && (node->temp_flag     != 9.0))   { node->temp_flag     = type; }
    if(rh_flag    && (node->rh_flag       != 9.0))   { node->rh_flag       = type; }
    if(uwind_flag && (node->uwind_flag    != 9.0))   { node->uwind_flag    = type; }
    if(vwind_flag && (node->vwind_flag    != 9.0))   { node->vwind_flag    = type; }
    if(rate_flag  && (node->asc_rate_flag != 9.0))   { node->asc_rate_flag = type; }
  }

  // Handle the rest of the flags
  else { 
    if(press_flag && (node->press_flag != 9.0) && (node->press_flag != 3.0))    
      { node->press_flag = type; }
    if(temp_flag  && (node->temp_flag != 9.0) && (node->temp_flag != 3.0))    
      { node->temp_flag = type; }
    if(rh_flag    && (node->rh_flag != 9.0) && (node->rh_flag != 3.0))  
      { node->rh_flag = type; }
    if(uwind_flag && (node->uwind_flag != 9.0) && (node->uwind_flag != 3.0))    
      { node->uwind_flag = type; }
    if(vwind_flag && (node->vwind_flag != 9.0) && (node->vwind_flag != 3.0))    
      { node->vwind_flag = type; }
    if(rate_flag  && (node->asc_rate_flag != 9.0) && (node->asc_rate_flag != 3.0))    
      { node->asc_rate_flag = type; }
  }
}

/**
 * Check for rapid ascension rates, rapid pressure changes and super adiabatic lapse 
 * rates over 100 mb.
 *
 * @param err_file The output stream where errors are to be written.
 * @param data_array The array containing the data points.
 * @param size The number of entries in the data array.
 **/ 
void nws_check(ofstream& err_file,CLASS_ARRAY data_array,int size) {
  int beginning = 1;
  float previous_rate;

  // Loop through all of the data using a 6 data point spread
  for (int endIndex = 5; endIndex < size; endIndex++) {
    int startIndex = endIndex - 5;

    // Only care if the pressures are not missing and the pressure is above 100 mb
    if (data_array[startIndex].press != 9999.0 && data_array[endIndex].press != 9999.0 &&
	data_array[startIndex].press <= 100) {
	
      // Only can calculate rates if altitudes and times are not missing.
      if (data_array[startIndex].alt != 99999.0 && data_array[startIndex].time != 9999.0 &&
	  data_array[endIndex].alt != 99999.0 && data_array[endIndex].time != 9999.0) {

	// Store the ascension rate for the next data point
	previous_rate = nws_check_ascension_rate_change(err_file,data_array,startIndex,
							endIndex,previous_rate,beginning);
	
	// Check to see if this is the first data point and mark it as not if it is.
	if (beginning) { beginning = 0; }	  
      }      
	
      // Check the pressure changes
      rapid_pressure_check(err_file,data_array,startIndex,endIndex);
    }
  }
}

/**
 * Check the ascenstion rate changes for NWS data over 100 mb.
 *
 * @param err_file The output stream where errors are to be written.
 * @param data_array The array of sounding data.
 * @param start The starting index to be checked.
 * @param end The ending index to be checked.
 * @param prev_asc_rate The previous calculated ascension rate.
 * @param first A flag stating if this is the first ascension rate being checked.
 * @return The calculated ascension rate.
 **/
float nws_check_ascension_rate_change(ofstream& err_file, CLASS_ARRAY data_array, int start,
				      int end, float prev_asc_rate, int first) {
  float asc_rate = 0.0; // Default ascension rate.
  
  // Only calculate the rate if the components are not the same
  if(data_array[end].time != data_array[start].time &&
     data_array[end].alt  != data_array[start].alt) {
    
    asc_rate = (data_array[end].alt - data_array[start].alt) / 
      (data_array[end].time - data_array[start].time);
  }

  // Only can calculate the change if there is a previous ascension rate.
  if (!first) {
    // Calculate the change of ascension rate.
    float rate_change = asc_rate - prev_asc_rate;

    // Ascension Rate change is too fast
    if(fabs(rate_change) >= QUEST_ASC_RATE_CHANGE) {
      char outline[100];
      sprintf(outline,"Rapid ascendrate change between pressure %6.1f and pressure %6.1f is %6.1f m/s.\n",data_array[start].press,data_array[end].press,rate_change);
      err_file << outline;

      // Determine if they flag types should be BAD (3) or QUESTIONABLE (2)
      int type = (fabs(rate_change) >= BAD_ASC_RATE_CHANGE) ? 3 : 2;

      // Mark all of the flags between start and end to the flag type
      for(int i = start ; i <= end; i++) { mark_flags(&data_array[i],type,1,0,0,0,0,0); }
    }
  }
  
  // Return the calculated ascension rate
  return asc_rate;
}

/**
 * Check for rapid pressure changes.
 * 
 * @param err_file The output stream where errors are to be written.
 * @param data_array The array of sounding data to be checked.
 * @param start The starting index to use for checking.
 * @param end The ending index to use for checking.
 **/
void rapid_pressure_check(ofstream& err_file, CLASS_ARRAY data_array, int start, int end) {
  // Calculate the pressure change.
  float press_change = fabs((data_array[start].press - data_array[end].press) / 
			    (data_array[start].time  - data_array[end].time));

  if(press_change >= QUEST_RAPID_PRESS_INC) {

    // Determine if the flag should be BAD(3) or QUESTIONABLE(2)
    int type = press_change >= BAD_RAPID_PRESS_INC ? 3 : 2;

    // Mark all of the flags for the pressure,temp,rh to the specified type.
    for(int j = start; j <= end; j++) { mark_flags(&data_array[j],type,1,1,1,0,0,0); }
     
    char outline[100];
    sprintf(outline,"Rapid pressure change between pressure: %6.1f and pressure %6.1f is %7.1f mb/s\n",data_array[start].press,data_array[end].press,press_change);
    err_file << outline;
  }
}

/**
 * Check for rapid temperature changes with the specified acceptable values.
 *
 * @param err_file The output stream where errors are to be written.
 * @param data_array The array thta contains the data to be checked.
 * @param start The The starting index to be checked.
 * @param end The ending index to be checked.
 * @param rapid_temp_inc The value that defines a rapid temperature increase.
 * @param bad_rapid_temp_inc The value that defines a bad rapid temperature increase.
 **/
void rapid_temp_check(ofstream& err_file, CLASS_ARRAY data_array, int start, int end,
		      float rapid_temp_inc, float bad_rapid_temp_inc) {
  float lapse_rate = (data_array[end].temp - data_array[start].temp) / 
    (data_array[end].alt - data_array[start].alt);

  if(lapse_rate >= rapid_temp_inc*ADIABATIC_LAPSE_RATE && 
     lapse_rate*100.0 >= rapid_temp_inc) {
    
    char outline[100];
    sprintf(outline,"Rapid temperature increase between pressure %6.1f and %6.1f is %7.2f deg C/km.\n",data_array[start].press,data_array[end].press,lapse_rate*1000.0);
    err_file << outline;

    // Determine if the flag should be BAD(3) or QUESTIONABLE(2)
    int type = lapse_rate * 1000.0 >= bad_rapid_temp_inc ? 3 : 2;
    
    // Mark the pressure,temp,rh flags between start and end to the specified type.
    for(int j = start; j <= end; j++) { mark_flags(&data_array[j],type,1,1,1,0,0,0); }
  }
}

/**
 * Check for rapid changes in temperature.
 *
 * @param err_file The output stream where errors are to be written.
 * @param data_array The array thta contains the data to be checked.
 * @param start The The starting index to be checked.
 * @param end The ending index to be checked.
 **/
void rapid_temperature_check(ofstream& err_file, CLASS_ARRAY data_array, int start, int end) {
  if(data_array[start].temp != 999.0 && data_array[end].temp != 999.0 && 
     data_array[end].temp - data_array[start].temp > .15) {
    
    if (LAT_TYPE == HOT) {
      rapid_temperature_for_hot_latitudes(err_file,data_array,start,end);
    } else if (LAT_TYPE == COLD) {
      rapid_temperature_for_cold_latitudes(err_file,data_array,start,end,
					   SFC_PRESS_LIMIT,SFC_RAPID_TEMP_INC);
    } else {
      cerr << "LAT_TYPE is not defined as HOT or COLD" << endl;
      exit(1);
    }
  }
}

/**
 * Check for rapid changes in temperature for data in high latitudes or cold weather.
 *
 * @param err_file The output stream where errors are to be written.
 * @param data_array The array that contains the data to be checked.
 * @param start The starting index to be checked.
 * @param end The ending index to be checked.
 * @param surf_p_limit The SURF_P_LIMIT to use for checking.
 * @param surf_rapid_temp The SURF_RAPID_TEMP to use for checking.
 **/
void rapid_temperature_for_cold_latitudes(ofstream& err_file, CLASS_ARRAY data_array,int start,
					  int end,float surf_p_limit,float surf_rapid_temp) {

  // Check for over the P_LIMIT but under the SURF_P_LIMIT
  if(data_array[start].press >= PRESS_LIMIT && data_array[end].press >= PRESS_LIMIT &&
     data_array[start].press < surf_p_limit && data_array[end].press < surf_p_limit) { 
    rapid_temp_check(err_file,data_array,start,end,RAPID_TEMP_INC,BAD_RAPID_TEMP_INC);
  }
  
  // Check for under the P_LIMIT
  else if((data_array[start].press < PRESS_LIMIT || data_array[end].press < PRESS_LIMIT) && 
     data_array[start].press != data_array[end].press) {
    rapid_temp_check(err_file,data_array,start,end,STRAT_RAPID_TEMP_INC,BAD_STRAT_RAPID_TEMP_INC);
  }
  
  // Check for over the SURF_P_LIMIT
  else if(data_array[start].press >= surf_p_limit && data_array[end].press >= surf_p_limit) {
    rapid_temp_check(err_file,data_array,start,end,surf_rapid_temp,BAD_SFC_RAPID_TEMP_INC);
  }
}

/**
 * Check for rapid changes in temperature for data in tropical latitudes or warm weather.
 * 
 * @param err_file The output stream where errors are to be written.
 * @param data_array The array that contains the data to be checked.
 * @param start The starting index to be checked.
 * @param end The ending index to be checked.
 **/
void rapid_temperature_for_hot_latitudes(ofstream& err_file, CLASS_ARRAY data_array, 
					 int start, int end) {
  // Check for over the P_LIMIT
  if(data_array[start].press >= PRESS_LIMIT && data_array[end].press >= PRESS_LIMIT){
    rapid_temp_check(err_file,data_array,start,end,RAPID_TEMP_INC,BAD_RAPID_TEMP_INC);
  }

  // Check for under the P_LIMIT
  else if((data_array[start].press < PRESS_LIMIT || data_array[end].press < PRESS_LIMIT) && 
	  data_array[start].press != data_array[end].press) {
    rapid_temp_check(err_file,data_array,start,end,STRAT_RAPID_TEMP_INC,BAD_STRAT_RAPID_TEMP_INC);
  }
}

/**
 * Check for a super adiabatic lapse rate change.
 *
 * @param err_file The output stream where errors should be written.
 * @param data_array The array that contains the data to be checked.
 * @param start The starting index to be checked.
 * @param end The ending index to be checked.
 **/
void super_adiabatic_lapse_rate_check(ofstream& err_file, CLASS_ARRAY data_array, 
				      int start, int end, 
				      float quest_lapse_rate, float bad_lapse_rate) {
  if (data_array[start].temp != 999.0 && data_array[end].temp != 999.0 &&
      (data_array[start].temp - data_array[end].temp) > LAPSE_RATE_DIFF) {

    if(data_array[start].alt == data_array[end].alt){ // Equal altitudes set Bad QC flags
      char outline[100];
      sprintf(outline,"Super adiabatic lapse rate between pressure %6.1f and %6.1f with no altitude change.\n",data_array[start].press,data_array[end].press);
      err_file << outline;
      
      for(int j = start; j <= end; j++) { mark_flags(&data_array[j],3,1,1,1,0,0,0); }
    } else {
      float lapse_rate = (data_array[start].temp - data_array[end].temp) / 
	(data_array[start].alt - data_array[end].alt);
      
      // Super Adiabatic Lapse rate checking
      if(lapse_rate <= -LAPSE_RATE_LIMIT*ADIABATIC_LAPSE_RATE && 
	 lapse_rate*1000 <= quest_lapse_rate){
	char outline[100];
	sprintf(outline,"Super adiabatic lapse rate between pressure %6.1f and %6.1f is %8.2f deg C/km.\n",data_array[start].press,data_array[end].press,lapse_rate*1000);
	err_file << outline;
	
	int type = lapse_rate * 1000 <= bad_lapse_rate ? 3 : 2;
	for(int j=start;j<=end;j++){ mark_flags(&data_array[j],type,1,1,1,0,0,0); }
      }
    }
  }
}

/**
 * Write the information in the data array into the output file.
 *
 * @param out_file The stream to write the data to.
 * @param array The array that contains the data to be written.
 * @param size The number of entries in the data array.
 **/
void write_class(ofstream& out_file, CLASS_ARRAY array, int size) {
  char outline[LINE_LEN];
  for(int i = 0; i < size; i++) {
    sprintf(outline,"%6.1f %6.1f %5.1f %5.1f %5.1f %6.1f %6.1f %5.1f %5.1f %5.1f %8.3f %7.3f %5.1f %5.1f %7.1f %4.1f %4.1f %4.1f %4.1f %4.1f %4.1f",array[i].time,array[i].press,array[i].temp,array[i].dewpt,array[i].rh,array[i].u_comp,array[i].v_comp,array[i].wind_spd,array[i].wind_dir,array[i].asc_rate,array[i].lon,array[i].lat,array[i].elev,array[i].azim,array[i].alt,array[i].press_flag,array[i].temp_flag,array[i].rh_flag,array[i].uwind_flag,array[i].vwind_flag,array[i].asc_rate_flag);
    out_file << outline << endl;
  }
}

/**
 * Run the checker.
 *
 * @param argc The number of arguements sent to the binary (Should be 2)
 * @param argv The list of arguements sent to the binary
 **/
void main(int argc,char *argv[]) {
  ifstream infile;
  ofstream outfile,errfile;
  char err_file[100];
  streampos file_pos;
  CLASS_ARRAY class_array;
  int numread;
  
  // Print out a help screen if the input is not an input file and an output file.
  if(argc < 2 || argc > 3) {
    helpscreen(argv[0]);
    exit(1);
  }
  
  // Open the input file so it can be read.
  infile.open(argv[1],ios::in|ios::nocreate);
  if(!infile){
    cerr << "Error opening input file:'" << argv[1] << "'." << endl;
    exit(1);
  }

  // Open the output file so it can be written to.
  outfile.open(argv[2],ios::out|ios::trunc);
  if(!outfile){
    cerr << "Error opening output file:'" << argv[2] << "'." << endl;
    exit(1);
  }

  // Create the name of the error file
  strcpy(err_file,argv[1]);
  strcat(err_file,".err");

  // Open the error file so it can be written to.
  errfile.open(err_file,ios::out|ios::trunc);
  if(!errfile){
    cerr << "Error opening Errors file:'" << err_file << "'." << endl;
      exit(1);
  }

  // Read and Write header to outfile
  file_pos = copyheader(infile,outfile,argv[1]);

  // Read met data into array
  numread = initarray(infile,errfile,file_pos,class_array);


  infile.close();

  // Check the min and maximum values for the data.
  check_max_min(errfile,class_array,numread);

  // Check the rates of change for the values in the data.
  check_rates(errfile,class_array,numread);

  // If the checker is to check NWS data
#ifdef NWS
  nws_check(errfile,class_array,numread);
#endif


  // Save the updated data to the output file.
  write_class(outfile,class_array,numread);

  // Close the output files.
  outfile.close();
  errfile.close();
}
