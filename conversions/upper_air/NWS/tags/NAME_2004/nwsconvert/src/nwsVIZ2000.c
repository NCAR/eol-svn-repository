/******************************************/
/*                                        */
/* NWS:                                   */
/*    This program will convert NWS ascii */
/* version of store files into OCF (OFPS  */
/* CLASS Format).                         */
/*                                        */
/* Kendall Southwick                      */
/* UCAR/OFPS                              */
/* May 1994 AD.                           */
/*                                        */
/* Darren R. Gallant                      */
/* UCAR/JOSS                              */
/* NWS:                                   */
/* This program converts NWS ascii store  */
/* files for VIZ and Space Data           */ 
/* Radiosondes                            */
/* compiled program on Thunder(Solaris)   */
/* changed header interger elevation to   */
/* float                                  */
/* August thru November 1996 AD.          */
/* Changed procedures read_met_data and   */ 
/* read_ang_data to use strstr instead of */
/* strspn and strncmp to better handle    */
/* spaces in data arrays added if-else    */
/* in read_ang_data under if(read_flag)   */
/* to skip past lines containing times but*/
/* no data setting azimuth and elevation  */
/* angles to missing                      */
/* assume  5 < strlen(line) < 17 contain  */
/* no azimuth or elevation angle data and */
/* also no 00 in slant range field        */
/* when time and no data but slant range  */
/* field non-empty no problems occurred   */
/* left cout line for error analysis      */
/* December 1996 AD.                      */
/*                                        */
/* created seperate procedures in         */
/* read_header for each data section      */
/* changed program to run with error      */
/* messages for ascii files missing       */
/* pre-admin and pre-release sections     */
/* output file: XXXc9999.cls              */
/*                                        */
/*					  */
/* May 1998 	                          */
/* Added Sonde Manufacturer and 	  */
/* Serial number lines to header          */
/*                                        */
/* For VIZ B2 radiosonde no RH corrction  */
/* performed                              */
/* 				          */
/* June 1998                              */
/* Created non SD VIZ source code by      */
/* removing Space Data RH correction      */
/* subroutine			          */
/* Fixed problem when NULL INT and        */
/* MISSING AZ appear in elevation and     */
/* azimuth fields in read_ang_data sub    */
/* When either string appears in a line   */
/* a[ret_val].azim and a[ret_val].elev    */
/* remain missing                         */
/* This solution prevents infinite loop   */
/*                                        */
/* July 1998                              */
/* Another change involving NULL INT,     */
/* MISSING AZ, and MISSING RNG.           */
/* Previous logic in read_ang_data skipped*/
/* these lines entirely, i.e ret_val not  */
/* incremented. New logic allows missing  */
/* values for azimuth and elevation       */
/* while incrementing ret_val.            */
/*	                                  */
/* October 1998                           */
/* Using strstr instead of strncmp when   */
/* checking for "D/R" within 6 second data*/
/* section. "D/R" flag changes position   */ 
/* within same ascii file.                */
/*                                        */
/* Created log file: nwsVIZ98.log         */
/* output file name, input file errors,   */
/* variables set to missing, a time stamp */
/* ,etc will be written to log file       */
/*                                        */
/* March 1999                             */
/* Introduced Pressure and Altitude change*/
/* checks. If pressure changes 4 mb/s or  */
/* greater or Altitude changes 40 m/s     */
/* between levels then all datum are set  */
/* to missing and time is kept            */
/*				          */
/* May 1999                               */
/* If term_alt = 0, stop reading met data */
/* upon first dropping flag i.e 'D/R'     */
/* encountered only datum with non-missing*/
/* altitudes kept                         */
/*					  */
/* Made following changes in              */
/* sounding_compute.C May 13th 1999       */
/* counted number of missing angle        */
/* contained with interval used by        */
/* smooth_elev			          */
/* determined percentage of missing angles*/
/* added conditional if this ratio exceeds*/
/* 25% then no smoothing occurs           */
/* this is in addition too having at least*/
/* 150 points in interval                 */
/* this change appears to prevent infinite*/
/* looping caused by excessive number of  */
/* missing elevation and azimuth angles   */
/* in sil1718.asc => SILc3012.cls 1997    */
/* 				          */
/* Fixed header by replacing GMT with UTC */
/* and Launch with release                */
/* 				          */
/* Feb 2005			          */
/* Changed the cal_dewpt to match the rest*/
/* of the soundings' calculations.        */
/******************************************/

#include <iostream.h>
#include <fstream.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include "sounding_def.h"
#include <time.h>

#define HUM_COR_FLAG 1

extern void smooth_elev(double work_angle[MAX_OB], double elev_smooth[MAX_OB],
                    double time[MAX_OB], int npts, int angle_status[MAX_OB],
                    double lim_ang[360], SOUND_DATA_ARR sd, int azim_flag);

extern void smooth_azim(double work_angle[MAX_OB], double elev_smooth[MAX_OB],
                    double time[MAX_OB], int npts, int angle_status[MAX_OB],
                    double lim_ang[360], SOUND_DATA_ARR sd);

extern void cal_position(double elev[MAX_OB], double azim[MAX_OB], double 
    alt[MAX_OB], double x[MAX_OB], double y[MAX_OB], int qxy[MAX_OB], int num);

extern void cal_uv(SOUND_DATA_ARR sd, double x[MAX_OB], double y[MAX_OB],
                   double xs[MAX_OB], double ys[MAX_OB], int qxy[MAX_OB],
                   int ang_status[MAX_OB], int num);

extern void cal_latlon(SOUND_DATA_ARR sd, double xs[MAX_OB], double ys[MAX_OB],
                   float lon, float lat, int ang_status[MAX_OB]);

extern void cal_spddir(SOUND_DATA_ARR sd);

extern void cal_ascen(SOUND_DATA_ARR sd);

void init_data(SOUND_DATA_ARR a)
/******************************************/
/*                                        */
/*                                        */
/******************************************/
{
  int i;

  for (i=0;i<MAX_OB;i++)
  {
    a[i].time = 9999.0;
    a[i].press = 9999.0;
    a[i].temp = 999.0;
    a[i].dewpt = 999.0;
    a[i].rh = 999.0;
    a[i].u_cmp = 9999.0;
    a[i].v_cmp = 9999.0;
    a[i].wind_spd = 999.0;
    a[i].wind_dir = 999.0;
    a[i].z_cmp = 999.0;
    a[i].lon = 9999.000;
    a[i].lat = 999.000;
    a[i].elev = 999.0;
    a[i].azim = 999.0;
    a[i].alt = 99999.0;
    a[i].qp = 9.0;
    a[i].qt = 9.0;
    a[i].qrh = 9.0;
    a[i].qu = 9.0;
    a[i].qv = 9.0;
    a[i].qz = 9.0;
  }
} /* End of init_data. */

void write_class(ofstream &out_file, SOUND_DATA_ARR a, int npts)
/******************************************/
/*                                        */
/*                                        */
/******************************************/
{
  int i;
  char outline[150];   

  for(i=0;i<npts;i++)
  {
    sprintf(outline,"%6.1f %6.1f %5.1f %5.1f %5.1f %6.1f %6.1f %5.1f %5.1f %5.1f %8.3f %7.3f %5.1f %5.1f %7.1f %4.1f %4.1f %4.1f %4.1f %4.1f %4.1f",
      a[i].time,a[i].press,a[i].temp,a[i].dewpt,a[i].rh,a[i].u_cmp,a[i].v_cmp,
      a[i].wind_spd,a[i].wind_dir,a[i].z_cmp,a[i].lon,a[i].lat,a[i].elev,
      a[i].azim,a[i].alt,a[i].qp,a[i].qt,a[i].qrh,a[i].qu,a[i].qv,a[i].qz);
    out_file << outline <<endl;
  }
}
int read_station_data(ifstream& in_file,streampos file_pos,float **lat,
float **lon,int *index,int *elev,char call_letters[5],char site_name[28])
/******************************************/
/*                                        */
/*                                        */
/******************************************/
{
  char line[LINE_LEN+1],tmpstr[20];
  int i,j,k,ret_val;
  in_file.seekg(file_pos);
  **lat = 999.0; **lon = 9999.0, *index = -1;*elev = -999;
  strcpy(line,"");
  strcpy(call_letters,"");
  strcpy(site_name,"");
  ret_val = 0;
  while((!ret_val)&&(!in_file.eof()))
  { // While in Station Data section.
    strcpy(tmpstr,"Index:");
    i = strspn(line," ");
    j = strspn(&line[i],tmpstr);
    if((j==strlen(tmpstr))&&(strlen(line)>0))
    { // Have Index: line.
      k = strspn(&line[i+j+1]," ");
      k += i+j+1;
      j = strcspn(&line[k]," ");
      strncpy(tmpstr,&line[k],j);
      tmpstr[j] = '\0';
      *index = atoi(tmpstr);
      //cout << "read_station_data index:" << *index << endl;
    } // Have Index: line.

    strcpy(tmpstr,"Name:");
    i = strspn(line," ");
    j = strspn(&line[i],tmpstr);
    if((j==strlen(tmpstr))&&(strlen(line)>0))
    { // Have Name: line.
      k = strspn(&line[i+j+1]," ");
      k += i+j+1;
      strcpy(site_name,&line[k]);
      j = strlen(&line[k]);
      site_name[j] = '\0';
      for(i=0;i<strlen(site_name);i++)
        if(iscntrl(site_name[i]))
          site_name[i] = '\0';
     } // Have Name: line.

     strcpy(tmpstr,"Parent FO");
     i = strspn(line," ");
     j = strspn(&line[i],tmpstr);
     if((j==strlen(tmpstr))&&(strlen(line)>0))
     { // Have Call Letters: line.
       k = strcspn(&line[j+4],":");
       i = j+k+5;
       k = strspn(&line[i]," ");
       i += k;
       j = strlen(&line[i]);
       strncpy(call_letters,&line[i],j);
       call_letters[j] = '\0';
       for(i=0;i<strlen(call_letters);i++)
         if(iscntrl(call_letters[i]))
           call_letters[i] = '\0';
     } // Have Call Letters: line.

     strcpy(tmpstr,"Latitude:");
     i = strspn(line," ");
     j = strspn(&line[i],tmpstr);
     if((j==strlen(tmpstr))&&(strlen(line)>0))
     { // Have lat/lon: line.
       k = strspn(&line[i+j+1]," ");
       k += i+j+1;
       j = strcspn(&line[k]," ");
       strncpy(tmpstr,&line[k],j);
       tmpstr[j] = '\0';
       **lat = atof(tmpstr);
       i = j+k;
       k = strcspn(&line[i],":");
       i += k+1;
       k = strspn(&line[i]," ");
       i += k;
       j = strcspn(&line[i]," ");
       strncpy(tmpstr,&line[i],j);
       tmpstr[j] = '\0';
       **lon = -1.0 * atof(tmpstr);
     } // Have lat/lon: line.

     strcpy(tmpstr,"Elevation:");
     i = strspn(line," ");
     j = strspn(&line[i],tmpstr);
     if((j==strlen(tmpstr))&&(strlen(line)>0))
     { // Have Elev: line.
       k = strspn(&line[i+j+1]," ");
       k += i+j+1;
       j = strcspn(&line[k]," ");
       strncpy(tmpstr,&line[k],j);
       tmpstr[j] = '\0';
       *elev = atoi(tmpstr);
     } // Have Elev: line.

     if((**lat != 999.0) && (**lon != 9999.0) && (*index != -1) &&
       (strlen(call_letters) >1) && (strlen(site_name) >1) &&
       (*elev > -999))
        ret_val = 1;

     in_file.getline(line,LINE_LEN,'\n');
  } // While in Station Data section
  return(ret_val);
}// end procedure read_station_data

int read_pre_admin(ifstream& in_file,streampos file_pos,int **nom_rel_month,
    int **nom_rel_year,int *nom_rel_day,int *nom_rel_hour,int *ascen_num)
/******************************************/
/*                                        */
/*                                        */
/******************************************/
{
  char line[LINE_LEN+1], tmpstr[20];
  int i,j,k,ret_val;ret_val = 0;
  strcpy(line,"");strcpy(tmpstr,"");
  in_file.seekg(file_pos);
  while((!ret_val)&&(!in_file.eof()))
  { // While in Prerelease Admin section.

    strcpy(tmpstr,"Date:");
    i = strspn(line," ");
    j = strspn(&line[i],tmpstr);
    if((j==strlen(tmpstr))&&(strlen(line)>0))
    { // Have Date: line.
      k = strspn(&line[i+j+1]," ");
      k += i+j+1;
      j = strcspn(&line[k],"/");
      strncpy(tmpstr,&line[k],j);
      tmpstr[j] = '\0';
      **nom_rel_month = atoi(tmpstr);
      k += j+1;
      j = strcspn(&line[k],"/");
      strncpy(tmpstr,&line[k],j);
      tmpstr[j] = '\0';
      *nom_rel_day = atoi(tmpstr);
      k += j+1;
      j = strcspn(&line[k],"/");
      strncpy(tmpstr,&line[k],j);
      tmpstr[j] = '\0';
      **nom_rel_year = atoi(tmpstr);
    } // Have Date: line.

    strcpy(tmpstr,"Hour:");
    i = strspn(line," ");
    j = strspn(&line[i],tmpstr);
    if((j==strlen(tmpstr))&&(strlen(line)>0))
    { // Have Hour: line.
      k = strspn(&line[i+j+1]," ");
      k += i+j+1;
      j = strcspn(&line[k]," ");
      strncpy(tmpstr,&line[k],j);
      tmpstr[j] = '\0';
      *nom_rel_hour = atoi(tmpstr);
    } // Have Hour: line.

    strcpy(tmpstr,"Ascension No.:");
    i = strspn(line," ");
    j = strncmp(&line[i],tmpstr,strlen(tmpstr));
    if((j==0)&&(strlen(line)>0))
    { // Have ascension num: line.
      j = strlen(tmpstr);
      k = strspn(&line[i+j+1]," ");
      k += i+j+1;
      j = strcspn(&line[k],"/");
      strncpy(tmpstr,&line[k],j);
      tmpstr[j] = '\0';
      *ascen_num = atoi(tmpstr);
    } // Have ascension num: line.

    if((*nom_rel_day >0)&&(**nom_rel_month>0)&&(**nom_rel_year>0)&&
          (*nom_rel_hour>=0)&&(*ascen_num>0))
      ret_val = 1;
       
    in_file.getline(line,LINE_LEN,'\n');

  } // While in Prerelease Admin section.
  return(ret_val);
}// end procedure read_pre_admin

int read_pre_flight(ifstream& in_file,streampos file_pos,char sonde_type[12],
char serial_num[20])
/******************************************/
/*                                        */
/*                                        */
/******************************************/
{
  char line[LINE_LEN+1],tmpstr[20],teststr[20];
  int i,j,ret_val;
  strcpy(line,"");strcpy(tmpstr,"");ret_val = 0;
  in_file.seekg(file_pos);
  while((!ret_val)&&(!in_file.eof()))
  {
    strcpy(tmpstr,"Radiosonde Mfr:");
    i = strspn(line," ");
    j = strncmp(&line[i],tmpstr,strlen(tmpstr));
    if((j== 0)&&(strlen(line)>0))
    { // Have Radiosonde Mfr: line.
      j = strlen(tmpstr) + 1;
      i = strspn(&line[j]," ") + j;
      strncat(sonde_type,&line[i],strlen(&line[i]));
      for(i=0;i<strlen(sonde_type);i++)
        if(iscntrl(sonde_type[i]))
          sonde_type[i] = '\0';
    } // Have Radiosonde Mfr: line.

    strcpy(tmpstr,"Radiosonde S");
    if(strstr(line,tmpstr) != '\0')
    { // Have Radiosonde Serial No.: line.
      if(strstr(line,"Serial No.:") != '\0')
	strcpy(tmpstr,"Radiosonde Serial No.:");
      else
	strcpy(tmpstr,"Radiosonde S.N.");
      j = strlen(tmpstr) + 1;
      i = strspn(&line[j]," ") + j;
      strcpy(serial_num,"");
      strncat(serial_num,&line[i],strlen(&line[i]));
      for(i=0;i<strlen(serial_num);i++)
      	if(iscntrl(serial_num[i]))
          serial_num[i] = '\0';
      //strcpy(serial_num,"9999999.CSN");
    } // Have Radiosonde Serial No.: line.

    if(strlen(sonde_type) > 0 && strlen(serial_num) > 0)
      ret_val = 1;

    in_file.getline(line,LINE_LEN,'\n');
  }
  return(ret_val);
}// end procedure read_pre_flight
int read_surface_ob(ifstream& in_file,streampos file_pos,float **wind_dir,
float **wind_speed)
/******************************************/
/*                                        */
/*                                        */
/******************************************/
{
  char line[LINE_LEN+1],tmpstr[20];
  int i,j,ret_val;
  strcpy(line,"");strcpy(tmpstr,"");ret_val = 0;
  in_file.seekg(file_pos);
  while((!ret_val)&&(!in_file.eof()))
  {
    strcpy(tmpstr,"Wind Direction:");
    i = strspn(line," ");
    j = strncmp(&line[i],tmpstr,strlen(tmpstr));
    if((j== 0)&&(strlen(line)>0))
    { // Have wind_dir: line.
      j = strlen(tmpstr);
      i = strspn(&line[j+1]," ");
      i += j+1;
      j = strcspn(&line[i]," ");
      strncpy(tmpstr,&line[i],j);
      tmpstr[j] = '\0';
      **wind_dir = atof(tmpstr);
    } // Have wind_dir: line.

    strcpy(tmpstr,"Wind Speed:");
    i = strspn(line," ");
    j = strncmp(&line[i],tmpstr,strlen(tmpstr));
    if((j== 0)&&(strlen(line)>0))
    { // Have wind_dir: line.
      j = strcspn(line,":");
      i = strspn(&line[j+1]," ");
      i += j+1;
      j = strcspn(&line[i]," ");
      strncpy(tmpstr,&line[i],strlen(&line[i]));
      tmpstr[strlen(&line[i])] = '\0';
      **wind_speed = atof(tmpstr);
    } // Have wind_dir: line.

    if((**wind_dir >=0.0) &&(**wind_speed>=0.0))
      ret_val = 1;

    in_file.getline(line,LINE_LEN,'\n');
  }
  return(ret_val);
}// end procedure read_surface_ob

int read_limit_ang(ifstream& in_file,streampos file_pos,double lim_ang[360])
/******************************************/
/*                                        */
/*                                        */
/******************************************/
{
  char line[LINE_LEN+1],tmpstr[20];
  int i,j,ang_ind,ret_val;
  strcpy(line,"");strcpy(tmpstr,"");ret_val = 0;ang_ind = 0;
  in_file.seekg(file_pos);
  while((!ret_val)&&(!in_file.eof()))
  {
    j = 0;
    if(strlen(line) > 50)
    { // Have angle line.
      i = strspn(line," ");
      while(strlen(&line[i])>0)
      { // Parse angle line.
        strcpy(tmpstr,"");
        j = strcspn(&line[i]," ");
        strncpy(tmpstr,&line[i],j);
        tmpstr[j] = '\0';
        lim_ang[ang_ind] = atof(tmpstr);
        j += i;
        i = strspn(&line[j]," ") + j;
        ang_ind++;;
      } // Parse angle line.
    } // Have angle line.

    if(ang_ind >= 360)
      ret_val = 1;

    in_file.getline(line,LINE_LEN,'\n');
  }
  return(ret_val);
}// end procedure read_limit_ang

int read_calib_data(ifstream& in_file,streampos file_pos,float **rh33_res)
/******************************************/
/*                                        */
/*                                        */
/******************************************/
{
  char line[LINE_LEN+1],tmpstr[20];
  int i,j,ret_val;
  strcpy(line,"");strcpy(tmpstr,"");ret_val = 0;
  in_file.seekg(file_pos);
  while((!ret_val)&&(!in_file.eof()))
  {
    strcpy(tmpstr,"RH lock-in Res");
    i = strspn(line," ");
    j = strncmp(&line[i],tmpstr,strlen(tmpstr));
    if((j== 0)&&(strlen(line)>0))
    { // Have Radiosonde Mfr: line.
      j = strcspn(line,":");
      i = strspn(&line[j+1]," ")+j+1;
      j = strcspn(&line[i]," ");
      strncpy(tmpstr,&line[i],strlen(&line[i]));
      tmpstr[strlen(&line[i])] = '\0';
      **rh33_res = atof(tmpstr);
     } // Have Radiosonde Mfr: line.

     if(**rh33_res > 0)
       ret_val = 1;

     in_file.getline(line,LINE_LEN,'\n');
  }
  return(ret_val);
}// end procedure read_calib_data

int read_rel_data(ifstream& in_file,streampos file_pos,int *rel_min,
int *rel_hour,int *rel_day,int *rel_month,int *rel_year)
/******************************************/
/*                                        */
/*                                        */
/******************************************/
{
  char line[LINE_LEN+1],tmpstr[20];
  int i,j,k,ret_val;
  strcpy(line,"");strcpy(tmpstr,"");ret_val = 0;
  in_file.seekg(file_pos);
  while((!ret_val)&&(!in_file.eof()))
  {
    strcpy(tmpstr,"Released at:");
    i = strspn(line," ");
    j = strncmp(&line[i],tmpstr,strlen(tmpstr));
    if((j== 0)&&(strlen(line)>0))
    { // Have rel_time: line.
      k = strlen(tmpstr);
      j = strcspn(&line[k],":");
      strncpy(tmpstr,&line[k],j);
      tmpstr[j] = '\0';
      *rel_hour = atoi(tmpstr);
      strncpy(tmpstr,&line[k+j+1],2);
      tmpstr[2] = '\0';
      *rel_min = atoi(tmpstr);
      k += j+3;
      j = strcspn(&line[k],"/");
      strncpy(tmpstr,&line[k],j);
      tmpstr[j] = '\0';
      *rel_month = atoi(tmpstr);
      k += j+1;
      j = strcspn(&line[k],"/");
      strncpy(tmpstr,&line[k],j);
      tmpstr[j] = '\0';
      *rel_day = atoi(tmpstr);
      k +=j+1;
      j = strcspn(&line[k],"/");
      strncpy(tmpstr,&line[k],j);
      tmpstr[j] = '\0';
      *rel_year = atoi(tmpstr);
    } // Have rel_time: line.

    if((*rel_year>0)&&(*rel_month>0)&&(*rel_day>0)&&(*rel_hour>=0)&&
      (*rel_min>=0))
      ret_val = 1;
    in_file.getline(line,LINE_LEN,'\n');
    //cout << "in read_rel_data " << line << endl;
  }
  return(ret_val);
}// end procedure read_rel_data

int read_term_summary(ifstream& in_file,streampos file_pos,float **term_alt)
/******************************************/
/*                                        */
/*                                        */
/******************************************/
{
  char line[LINE_LEN+1],tmpstr[20];
  int i,j,ret_val;
  strcpy(line,"");ret_val = 0;
  in_file.seekg(file_pos);strcpy(tmpstr,"Termination Altitude");
  while((!ret_val)&&(!in_file.eof()))
  { 
    //cout << endl << "input line: " << line << " " << strlen(line) << endl;
    if(strstr(line,"Termination Altitude (M):") != '\0')
    { // Have Termination alt. line.
      j = strlen(tmpstr);
      i = strcspn(&line[j],":");
      i += j+1;
      j = strspn(&line[i]," ");
      j += i;
      strncpy(tmpstr,&line[j],strlen(&line[j]));
      tmpstr[strlen(&line[j])] = '\0';
      **term_alt = atof(tmpstr);
    } // Have Termination alt. line.

    if(**term_alt >=-200.0)
      ret_val = 1;

    in_file.getline(line,LINE_LEN,'\n');
  }
  return(ret_val);
}// end procedure read_term_summary

int read_header(ifstream& in_file, ofstream& out_file, ofstream& log_file, 
     char *i_f_name,
     double lim_ang[360], float *lat, float *lon, float *wind_speed, 
     float *wind_dir, float *term_alt, char sonde_type[12], float *rh33_res, 
     int *nom_rel_year, int *nom_rel_month)
/******************************************/
/*                                        */
/*                                        */
/******************************************/
{
  char line[LINE_LEN+1], tmpstr[20], call_letters[5], dummy_letters[4];
  char site_name[28], out_file_name[20], serial_num[20], write_line[150];
  //streampos file_pos,dummy_pos;
  streampos file_pos;
  float min;
  int nom_rel_hour, nom_rel_day;
  int rel_min, rel_hour, rel_day, rel_month, rel_year;
  int index, elev, ascen_num, ret_val;
  int done, station_data_flag, pre_admin_flag, pre_flight_flag;
  int surface_ob_flag, rel_data_flag, term_sum_flag;
  int calibration_data_flag, lim_ang_flag, work_flag;
  cout << endl << "Processing file: " << i_f_name << endl;
  ret_val = 0;
  done = 0;
  station_data_flag = 0;
  pre_admin_flag = 0;
  pre_flight_flag = 0;
  lim_ang_flag = 0;
  surface_ob_flag = 0;
  rel_data_flag = 0;
  term_sum_flag = 0;
  work_flag = 0;
  index = -1;elev = 999;
  calibration_data_flag = 0;
  strcpy(line,"");
  strcpy(call_letters,"");
  strcpy(site_name,"");
  strcpy(serial_num,"");
  while((!done) && (!in_file.eof()))
  {
    
    if(strstr(line,"Station Data Record")!= '\0' && station_data_flag == 0 && !in_file.eof())
    { // In the Station Data Record section.
      station_data_flag  = read_station_data(in_file,in_file.tellg(),&lat,&lon,
&index,&elev,call_letters,site_name);
    } // In the Station Data Record section.
    else if(strstr(line,"Prerelease Admin")!= '\0' && pre_admin_flag == 0 && !in_file.eof())
    { // In the Prerelease Admin section.
      nom_rel_hour = nom_rel_day = *nom_rel_month = *nom_rel_year = -1;
      ascen_num = -1;
      pre_admin_flag = read_pre_admin(in_file,in_file.tellg(),&nom_rel_month,
&nom_rel_year,&nom_rel_day,&nom_rel_hour,&ascen_num);
    } // In the Prerelease Admin section.
    else if(strstr(line,"Prerelease Flight")!= '\0' && pre_flight_flag == 0 && !in_file.eof())
    { // In the Prerelease Flight Equipment section.
      pre_flight_flag = read_pre_flight(in_file,in_file.tellg(),sonde_type,serial_num);
    } // In the Prerelease Flight Equipment section.
    else if(strstr(line,"Surface Observation")!= '\0' && surface_ob_flag == 0 && !in_file.eof())
    { // In the Surface Observation Data section.
      *wind_dir = -1.0;
      *wind_speed = -1.0;
      surface_ob_flag = read_surface_ob(in_file,in_file.tellg(),&wind_dir,
&wind_speed);
    } // In the Surface Observation Data section.
    else if(strstr(line,"Limiting Angle")!= '\0' && lim_ang_flag == 0 && !in_file.eof())
    { // In the Limiting Angle Data section.
      lim_ang_flag = read_limit_ang(in_file,in_file.tellg(),lim_ang);
    } // In the Limiting Angle Data section.
    else if(strstr(line,"Viz Radiosonde Cal")!='\0' && work_flag == 0 && !in_file.eof())
    {  // In the VIZ Radiosonde Calibration Data section.
      *rh33_res = -10;
      work_flag = read_calib_data(in_file,in_file.tellg(),&rh33_res);
    } // In the VIZ Radiosonde Calibration Data section.
    else if(strstr(line,"Viz B2 Radiosonde Cal")!='\0' && work_flag == 0 && !in_file.eof())
    {  // In the VIZ B2 Radiosonde Calibration Data section.
      *rh33_res = -10;
      work_flag = read_calib_data(in_file,in_file.tellg(),&rh33_res);
    } // In the VIZ B2 Radiosonde Calibration Data section.
    else if(strstr(line,"Release Data")!= '\0' && rel_data_flag == 0 && !in_file.eof())
    { // In the Release Record section.
      rel_min = rel_hour = rel_day = rel_month = rel_year = -1;
      rel_data_flag = read_rel_data(in_file,in_file.tellg(),&rel_min,&rel_hour,
&rel_day,&rel_month,&rel_year);
    } // In the Release Record section.
    else if(strstr(line,"Termination Summary")!='\0' && term_sum_flag == 0 && !in_file.eof())
    { // In the Termination Summary Data section.
      *term_alt = -500.0;
      term_sum_flag = read_term_summary(in_file,in_file.tellg(),&term_alt);
    } // In the Termination Summary Data section.

    strcpy(tmpstr,"6 Second Met Data");
    if(strstr(line,tmpstr)!='\0')
      done = 1;

    if(!done)
      file_pos = in_file.tellg();
      
    in_file.getline(line,LINE_LEN,'\n');
    //log_file << "input line " << line << endl;
  }

  if(done)
  {
    in_file.seekg(file_pos,ios::beg);
    ret_val = 1;
  }
  else
  {
    cerr <<"Error in reading file header"<<endl;
    log_file <<"Error in reading file header"<<endl;
    ret_val = 0;
  }

  if(!station_data_flag)
  {
    cerr << "Station Data Record is missing."<< endl;
    log_file << "Station Data Record is missing."<< endl;
    cerr << "File may have been processed with wrong program."<<endl;
    log_file << "File may have been processed with wrong program."<<endl;
    ret_val = 0;
  }

  if((!pre_admin_flag))
  {
    cerr << "Prerelease Administrative Data is missing."<< endl;
    log_file << "Prerelease Administrative Data is missing."<< endl;
    cerr << "File may have been processed with wrong program."<<endl;
    log_file << "File may have been processed with wrong program."<<endl;
    nom_rel_hour = 99;
    nom_rel_day = 99;
    *nom_rel_month = 12;
    *nom_rel_year = 99;
    ascen_num = 9999;

  }
  
  if(!work_flag)
  {
    cerr << "VIZ Radiosonde Calibration Data is missing."<< endl;
    log_file << "VIZ Radiosonde Calibration Data is missing."<< endl;  
    cerr << "File may have been processed with wrong program."<<endl;
    log_file << "File may have been processed with wrong program."<<endl;
    ret_val = 0;
   
  }
  if((!pre_flight_flag))
  {
    cerr << "Prerelease Flight Equipment Data is missing."<< endl;
    log_file << "Prerelease Flight Equipment Data is missing."<< endl;
    cerr << "File may have been processed with wrong program."<<endl;
    log_file << "File may have been processed with wrong program."<<endl;
    ret_val = 0;
  }

  if((!lim_ang_flag))
  {
    cerr << "Limiting Angle Data is missing."<< endl;
    log_file << "Limiting Angle Data is missing."<< endl;
    cerr << "File may have been processed with wrong program."<<endl;
    log_file << "File may have been processed with wrong program."<<endl;
    ret_val = 0;
  }

  if((!surface_ob_flag))
  {
    cerr << "Surface Observation Data is missing."<< endl;
    log_file << "Surface Observation Data is missing."<< endl;
    cerr << "File may have been processed with wrong program."<<endl;
    log_file << "File may have been processed with wrong program."<<endl;
    ret_val = 0;
  }

  if((!rel_data_flag))
  {
    cerr << "Release Data is missing."<< endl;
    log_file << "Release Data is missing."<< endl;
    cerr << "File may have been processed with wrong program."<<endl;
    log_file << "File may have been processed with wrong program."<<endl;
    rel_year = 99;rel_month = 12;rel_day = 99;rel_hour = 99;rel_min = 99;
    
  }

  if((!term_sum_flag))
  {
    cerr << "Termination Summary Data is missing."<< endl;
    log_file << "Termination Summary Data is missing."<< endl;
    cerr << "File may have been processed with wrong program."<<endl;
    log_file << "File may have been processed with wrong program."<<endl;
    ret_val = 0;
  }

//
// If have all the proper data from header (ie ret_val == 1)
// create header and output file and write the header.
//

  if(call_letters[0] =='_')
  { // Missing call letters, hunt for them in lookup table.
    if(index == 70026)
      strcpy(call_letters,"BRW");
    else if(index == 91212)
      strcpy(call_letters,"PGUM");
    else if(index == 91285)
      strcpy(call_letters,"PHTO");
    else if(index == 91408)
      strcpy(call_letters,"PTRO");
    else if(index == 91165)
      strcpy(call_letters,"PHLI");
    else if(index == 91376)
      strcpy(call_letters,"PMKJ");
    else if(index == 91765)
      strcpy(call_letters,"NSTU");
    else if(index == 91348)
      strcpy(call_letters,"PTPN");
    else if(index == 91334)
      strcpy(call_letters,"PTKK");
    else if(index == 91413)
      strcpy(call_letters,"PTYA");
    else if(index == 91245)
      strcpy(call_letters,"PWAK");
    else
    {
      cerr <<"Missing call letters for station index "<< index <<endl;
      log_file <<"Missing call letters for station index "<< index <<endl;
      ret_val = 0;
    }
  }

  if(ret_val)
  { // Create header and output file and write header to file.

//
// Create output file name.
//

    strcpy(out_file_name,"");
    if(index < 91000)
    	strncat(out_file_name,call_letters,3);
    else
        strncat(out_file_name,call_letters,4);
    
    if(*nom_rel_month < 10)
    {
      sprintf(tmpstr,"%1d",*nom_rel_month);
      strcat(out_file_name,tmpstr);
    }
    else if(*nom_rel_month == 10)
      strcat(out_file_name,"a");
    else if(*nom_rel_month == 11)
      strcat(out_file_name,"b");
    else if(*nom_rel_month == 12)
      strcat(out_file_name,"c");
    else
    {
      cerr <<"Error with the nominal release month "<<nom_rel_month<<endl;
      log_file <<"Error with the nominal release month "<<nom_rel_month<<endl;
      ret_val = 0;
    }
    strcpy(tmpstr,"");
    sprintf(tmpstr,"%02d",nom_rel_day);
    strcat(out_file_name,tmpstr);
    strcpy(tmpstr,"");
    sprintf(tmpstr,"%02d",nom_rel_hour);
    strcat(out_file_name,tmpstr);
    strcat(out_file_name,".cls");
 
    if(ret_val)
    { // Have valid file name.
      out_file.open(out_file_name,ios::out|ios::trunc);
      if(!out_file)
      {
        cerr <<"Error opening output file "<< out_file_name<<endl;
        log_file <<"Error opening output file "<< out_file_name<<endl;
        ret_val = 0;
      }
      else
      {
        cout << "Output file is: " << out_file_name << endl << endl;
	log_file << "Output file is: " << out_file_name << endl << endl;
      }

      if(ret_val)
      { // Have open output file.
        strcpy(write_line,"Data Type:                         National Weather Service Sounding.");
        out_file << write_line << endl;
        strcpy(write_line,"Project ID:                        0");
        out_file << write_line << endl;
        strcpy(write_line,"Release Site Type/Site ID:         ");
        strcat(write_line,call_letters);
        strcat(write_line," ");
        strcat(write_line,site_name);
        out_file << write_line << endl;
        log_file << write_line << endl; 
        strcpy(write_line,"Release Location (lon,lat,alt):    ");
        strcpy(tmpstr,"");
        sprintf(tmpstr,"%d",(int)(fabs(*lon)));
        strcat(write_line,tmpstr);
        strcat(write_line," ");
        min = 60.0 * (fabs(*lon)-(int)(fabs(*lon)));
        strcpy(tmpstr,"");
        sprintf(tmpstr,"%05.2f",min);
        strcat(write_line,tmpstr);
        if(*lon>0)
          strcat(write_line,"'E, ");
        else
          strcat(write_line,"'W, ");
        strcpy(tmpstr,"");
        sprintf(tmpstr,"%d",(int)(fabs(*lat)));
        strcat(write_line,tmpstr);
        strcat(write_line," ");
        min = 60.0 * (fabs(*lat)-(int)(fabs(*lat)));
        strcpy(tmpstr,"");
        sprintf(tmpstr,"%05.2f",min);
        strcat(write_line,tmpstr);
        if(*lat>0)
          strcat(write_line,"'N, ");
        else
          strcat(write_line,"'S, ");
        strcpy(tmpstr,"");
        sprintf(tmpstr,"%6.1f, ",*lon);
        strcat(write_line,tmpstr);
        strcpy(tmpstr,"");
        sprintf(tmpstr,"%5.1f, ",*lat);
        strcat(write_line,tmpstr);
        strcpy(tmpstr,"");
        sprintf(tmpstr,"%6.1f",(float)elev);
        strcat(write_line,tmpstr);
        out_file << write_line << endl;
        log_file << write_line << endl;
        if(rel_year < 100)
        {
        	sprintf(write_line,"UTC Release Time (y,m,d,h,m,s):    19%02d, %02d, %02d, %02d:%02d:00",rel_year,rel_month,rel_day, rel_hour, rel_min);
	}
	else
	{
	      	sprintf(write_line,"UTC Release Time (y,m,d,h,m,s):    %04d, %02d, %02d, %02d:%02d:00",rel_year,rel_month,rel_day, rel_hour, rel_min);
	}
        out_file << write_line << endl;
        log_file << write_line << endl;
	strcpy(write_line,"Ascension No:                      ");
        strcpy(tmpstr,"");
        sprintf(tmpstr,"%d",ascen_num);
        strcat(write_line,tmpstr);
        out_file << write_line << endl;
        log_file << write_line << endl;
        strcpy(write_line,"Radiosonde Serial Number:          ");
        strcat(write_line,serial_num);
        out_file << write_line << endl;
        strcpy(write_line,"Radiosonde Manufacturer:           ");
        strcat(write_line,sonde_type);
        out_file << write_line << endl;
        log_file << write_line << endl;
        if(rel_day == 99){
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
        if(*nom_rel_year < 100)
	{
        sprintf(write_line,"Nominal Release Time (y,m,d,h,m,s):19%02d, %02d, %02d, %02d:00:00",*nom_rel_year,*nom_rel_month,nom_rel_day, nom_rel_hour);
	}
	else
	{
	sprintf(write_line,"Nominal Release Time (y,m,d,h,m,s):%04d, %02d, %02d, %02d:00:00",*nom_rel_year,*nom_rel_month,nom_rel_day, nom_rel_hour);
	}
        out_file << write_line <<endl;

        strcpy(write_line," Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat   Ele   Azi    Alt    Qp   Qt   Qrh  Qu   Qv   QdZ");
        out_file << write_line <<endl;

        strcpy(write_line,"  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     m    code code code code code code");
        out_file << write_line <<endl;

        strcpy(write_line,"------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----");
        out_file << write_line <<endl;

      } // Have open output file.
    } // Have valid file name.

  } // Create header and output file and write header to file.

  return(ret_val);
} // End of procedure read_header.

int read_met_data(ifstream& in_file, ofstream& log_file, SOUND_DATA_ARR a, 
float term_alt, int *READ_ANG_FLAG)
/******************************************/
/*                                        */
/*                                        */
/******************************************/
{
  char line[LINE_LEN+1], tmpstr[50] ,outline[75] ,interp[3];
  char time[7], alt[11], press[13], QP[5], temp[7], QT[6], RH[7], QU[5];
  streampos file_pos;
  int done, read_flag, qt, qrh, last_press=0, last_alt=0, ret_val=0, miss_flag;
  float asc_rate, rapid_p, delta_t, delta_alt, delta_press; 
  int i=0,j=0,check_data=1;
  done = 0;
  read_flag = 0;
  miss_flag = 0;
  *READ_ANG_FLAG = 1;
  while((!done) && (!in_file.eof()))
  { // Reading met data.
    in_file.getline(line,LINE_LEN,'\n');
    i = strspn(line," ");
    j = strcspn(&line[i]," ");
    if(strstr(line,"6 Second Met Data") == '\0' &&  strstr(line,"Time") == '\0' && strstr(line,"TIME") == '\0' && j > 2 && strlen(line) > 5)
    	read_flag = 1;
    else
	read_flag = 0;

//
// Check if sonde is droping, if it is quit reading in
// data else next index.
//
   if(term_alt){
   	if((strstr(line,"D/R") != '\0') && 
   		(a[ret_val].alt >= (term_alt-75.0)) &&
        	(a[ret_val].alt != 99999.0))
                 read_flag = 0;
   }else{
	if(strstr(line,"D/R") != '\0')
                 read_flag = 0;

   }
        

    if(strstr(line,"Position Data") != '\0')
    {
	done = 1;
        read_flag = 0;
    }
 
//	cout << "line: " << line << " read_flag " << read_flag << endl;

    if(read_flag)
    {
      strcpy(time,"");strcpy(alt,"");strcpy(press,"");strcpy(temp,"");
      strcpy(RH,"");strcpy(QT,"  ");strcpy(QP,"  ");strcpy(QU,"  ");
      strcpy(tmpstr,"");
      strncat(tmpstr,line,31);tmpstr[31] = '\0';
      sscanf(tmpstr,"%7s%11s%13s",time,alt,press);
      strcpy(tmpstr,"");
      strncat(tmpstr,&line[33],3);tmpstr[3] = '\0';
      sscanf(tmpstr,"%3s",QP);
      strcpy(tmpstr,"");
      strncat(tmpstr,&line[37],6);tmpstr[6] = '\0';
      sscanf(tmpstr,"%6s",temp);
      strcpy(tmpstr,"");
      strncat(tmpstr,&line[46],3);tmpstr[3] = '\0';
      sscanf(tmpstr,"%3s",QT);
      strcpy(tmpstr,"");
      strncat(tmpstr,&line[51],5);tmpstr[5] = '\0';
      sscanf(tmpstr,"%5s",RH);
      strcpy(tmpstr,"");
      strncat(tmpstr,&line[58],3);tmpstr[3] = '\0';
      sscanf(tmpstr,"%3s",QU);

//      cout << time << " " << alt << " " << press << " " << QP << " " << temp << " " << QT << " " << RH << " " << QU << endl;
      
      a[ret_val].time = 60.0*atof(time);
      a[ret_val].alt = atof(alt);
      a[ret_val].press = atof(press);

      if(strstr(temp,"999999") == '\0')
      	a[ret_val].temp = atof(temp);
      else
	a[ret_val].temp = 999.0;

      a[ret_val].rh = atof(RH);

      if(strlen(line) > 62)
      {
      	strncat(tmpstr,&line[62],3);
      	tmpstr[3] = '\0';
      	sscanf(tmpstr,"%3s",interp);
      }
      else
	strcpy(interp,"");
      
//   cout << ret_val<<" "<< a[ret_val].time <<" "<< a[ret_val].press << endl;
//    
// Check the values that were read in and fill out the
// QC flags.
//

      if((a[ret_val].press <=0.0)||(a[ret_val].press > 1100.0)&& a[ret_val].press != 9999.0)
      {
        sprintf(outline,"Press: %7.1f mb on line %4d.\n",a[ret_val].press,ret_val);        
        log_file << outline;
        a[ret_val].qp = 9.0;
        a[ret_val].qt = 9.0;
        a[ret_val].qrh = 9.0;
        a[ret_val].temp = 999.0;
        a[ret_val].press = 9999.0;
        a[ret_val].rh = 999.0;
        a[ret_val].alt = 99999.0;
      }
      else
        a[ret_val].qp = 99.0;
 
      if((a[ret_val].alt <= -450.0) || (a[ret_val].alt >60000.0 && a[ret_val].alt != 99999.0))
      {
        sprintf(outline,"Alt at press: %6.1f is: %8.1f m.\n",a[ret_val].press,a[ret_val].alt); 
        log_file << outline;
        a[ret_val].alt = 99999.0;
      }
      
      if((a[ret_val].temp < -150.0) || (a[ret_val].temp > 75.0 && a[ret_val].temp != 999.0))
      {
        
        sprintf(outline,"Temp at press: %6.1f is: %6.1f deg .C.\n",a[ret_val].press,a[ret_val].temp);
        log_file << outline;
        a[ret_val].temp = 999.0;
        a[ret_val].rh = 999.0;
        a[ret_val].qt = 9.0;
        a[ret_val].qrh = 9.0;
      }
      else
        a[ret_val].qt = 99.0;

      if((a[ret_val].rh < 0.0) || (a[ret_val].rh > 150.0 && a[ret_val].rh != 999.0))
      {
        sprintf(outline,"RH at press: %6.1f is: %6.1f %.\n",a[ret_val].press,a[ret_val].rh);
        log_file << outline;
        a[ret_val].rh = 999.0;      
        a[ret_val].qrh = 9.0;
      }
      else
        a[ret_val].qrh = 99.0;

      if(a[ret_val].temp < -99.9)
      {
        sprintf(outline,"Temp at press: %6.1f is: %6.1f deg .C.\n",a[ret_val].press,a[ret_val].temp);
        log_file << outline;
        a[ret_val].rh = 999.0;      
        a[ret_val].qrh = 9.0;
      }

//
// Check if pressure and/or altitude changes are reasonable
// Defined MAX_ALT_CHG and MAX_PRESS_CHG in "sounding_def.h" to be 40 and 4
// respectively. Keep last valid datum, i.e non missing pressure and 
// altitude. If either datum's pressure or altitude fail this check, then
// all datum's observations set to missing. The surface point is always kept.
//
// Only perform this check after the first few data points
      if(ret_val > 0 && ret_val <= 1){
	if(check_data){
	  if(a[ret_val].alt != 99999.0)
          {
	    delta_t = fabs(a[ret_val].time - a[last_alt].time);
            delta_alt = fabs(a[ret_val].alt - a[last_alt].alt);
            asc_rate = delta_alt/delta_t;
            if(asc_rate >= MAX_ALT_CHG)
            {
	      check_data = 0;*READ_ANG_FLAG = 0;
              sprintf(outline,"Ascentrate change between pressure: %6.1f and %6.1f is: %6.1f m/s\n",a[ret_val].press,a[last_alt].press,asc_rate);
              log_file << outline;
              //cout << "READ_ANG_FLAG " << *READ_ANG_FLAG << endl;
            }
          }
	
        }
      }
      if(ret_val > 1 && check_data)
      {
      	if(a[ret_val].press != 9999.0)
        {
	  delta_t = fabs(a[ret_val].time - a[last_press].time);
          delta_press = fabs(a[ret_val].press - a[last_press].press);
          rapid_p = delta_press/delta_t;
          if(rapid_p >= MAX_PRESS_CHG)
          {
	    miss_flag = 1;
            sprintf(outline,"Rapid pressure change between pressure: %6.1f and pressure %6.1f is %7.1f mb/s\n",a[ret_val].press,a[last_press].press,rapid_p);
            log_file << outline;
          }	
        }
        if(a[ret_val].alt != 99999.0)
        {
	  delta_t = fabs(a[ret_val].time - a[last_alt].time);
          delta_alt = fabs(a[ret_val].alt - a[last_alt].alt);
          asc_rate = delta_alt/delta_t;
          if(asc_rate >= MAX_ALT_CHG)
          {
	    miss_flag = 1;
            sprintf(outline,"Altitude change between pressure: %6.1f and %6.1f is: %6.1f m/s\n",a[ret_val].press,a[last_alt].press,asc_rate);
            log_file << outline;
          }
        }
        if(miss_flag)
        { // if either above checks fail - set all observations to missing
	  a[ret_val].qp = 9.0;
          a[ret_val].qt = 9.0;
          a[ret_val].qrh = 9.0;
          a[ret_val].temp = 999.0;
          a[ret_val].press = 9999.0;
          a[ret_val].rh = 999.0;
          a[ret_val].alt = 99999.0;
          miss_flag = 0;
          sprintf(outline,"Observations set to missing at time: %6.1f seconds\n",a[ret_val].time);
          log_file << outline;
        }
        else
        {
	  if(a[ret_val].press != 9999.0)
	    last_press = ret_val;
          if(a[ret_val].alt != 99999.0)
            last_alt = ret_val;
        }	
      }

// Checking for incorrect QC flags

      if(a[ret_val].press == 9999.0 && a[ret_val].qp != 9.0)
        a[ret_val].qp = 9.0;

      if(a[ret_val].temp == 999.0 && a[ret_val].qt != 9.0)
        a[ret_val].qt = 9.0;

      if(a[ret_val].rh == 999.0 && a[ret_val].qrh != 9.0)
        a[ret_val].qrh = 9.0;
       

//
// Check if sonde is droping, if it is quit reading in
// data else next index.
//

        ret_val++;
    } // Read_flag.

    if(!done)
      file_pos = in_file.tellg();

  } // Reading met data.
  if(done)
    in_file.seekg(file_pos,ios::beg);

  return(ret_val);
} // End of procedure read_met_data.

int read_angle_data(ifstream& in_file, SOUND_DATA_ARR a)
/******************************************/
/*                                        */
/*                                        */
/******************************************/
{
  char line[LINE_LEN+1], tmpstr[20],outline[75];
  int done=0, read_flag=0, ret_val=0,i=0,j=0;

  while(!in_file.eof())
  { // Reading angle data.
    in_file.getline(line,LINE_LEN,'\n');

    i = strspn(line," ");
    j = strcspn(&line[i]," ");
    if(strstr(line,"Position Data") == '\0' && strstr(line,"6 Second Met Data")== '\0' && strstr(line,"TIME") == '\0' && strlen(line) > 5 && j > 2)
	read_flag = 1;
    else
	read_flag = 0;

    if(read_flag)
    {
      strcpy(tmpstr,"");
      strncat(tmpstr,line,5);
      a[ret_val].time = 60.0*atof(tmpstr);
      //cout << "time " << a[ret_val].time << endl; 
      if(strstr(line,"NULL INT") == '\0' && strstr(line,"MISSING AZ") == '\0')
      { 
        strcpy(tmpstr,"");
        strncat(tmpstr,&line[16],5);
        a[ret_val].elev = atof(tmpstr);
        strcpy(tmpstr,"");
        strncat(tmpstr,&line[31],5);
        a[ret_val].azim = atof(tmpstr);
        if((a[ret_val].elev <= 0.0) || (a[ret_val].elev >90.0) ||
          (a[ret_val].azim < 0.0) || (a[ret_val].azim >360.0)  ||
          (a[ret_val].press == 9999.0) && (a[ret_val].alt == 99999.0))
        {
          a[ret_val].elev = 999.0;
          a[ret_val].azim = 999.0;
        }
      }
      else
      {
        a[ret_val].elev = 999.0;
        a[ret_val].azim = 999.0;
      }

      ret_val++;
    } // Read_flag.
  } // Reading angle data.

  return(ret_val);
} // End of procedure read_angle_data.

void cal_dewpt(SOUND_DATA_ARR a)
/******************************************/
/*                                        */
/*                                        */
/******************************************/
{
  int i;
  float eso, emb, log_val;

  eso = 6.1121;
  for(i=0;i<MAX_OB;i++)
  {
    if((a[i].rh<=0.0)||(a[i].rh >100.0)||(a[i].temp==999.0))
      a[i].dewpt = 999.0;
    else
    {
      emb = eso*(a[i].rh/100.0)*exp(17.67*a[i].temp/(a[i].temp+243.5));
      log_val = log(emb / eso);
      a[i].dewpt = (243.5*log_val)/(17.67 - log_val);
      if(a[i].dewpt < -99.9)
      {
        a[i].dewpt = -99.9;
        a[i].qrh = 4.0;
      }
    }
  } // for.
} // End of cal_dewpt.

void correct_viz_hum(SOUND_DATA_ARR sd, float rh33_res, int nom_rel_year,
             int nom_rel_month, int npts)
/******************************************/
/*                                        */
/*                                        */
/******************************************/
{
  int i, j;
  double rh, rh_last, rh_old, res, res_ratio, res_total;
  double rht;
  double interval, f_r, temp;
char line[50];

  cout << "VIZ humidity correction." << endl;
//  cout << rh33_res << endl;
  for(i=1;i<npts;i++)
  { // For each point.

    if((sd[i].temp != 999.0)&&(sd[i].rh != 999.0))
    { // Have a valid point.

// Intial values for iteration.

      j = 0;

      rh = -10.0;
      rh_last = rh_old = -10.0;
      temp = sd[i].temp;
      if(sd[i].rh > 90.0)
      {
        res_ratio = 500.0;
        interval = 20.0;
      }
      else if(sd[i].rh > 80.0)
      {
        res_ratio = 100.0;
        interval = 5.0;
      }
      else if(sd[i].rh > 70.0)
      {
        res_ratio = 50.0;
        interval = 5.0;
      }
      else if(sd[i].rh > 60.0)
      {
        res_ratio = 10.0;
        interval = 1.0;
      }
      else if(sd[i].rh > 30.0)
      {
        res_ratio = 1.0;
        interval = 0.5;
      }
      else if(sd[i].rh > 15.0)
      {
        res_ratio = 0.5;
        interval = 0.1;
      }
      else
      {
        res_ratio = 0.2;
        interval = 0.05;
      }

// Loop for iteration.

      while((fabs(rh-(double)(sd[i].rh))>0.00001) && (j<5000))
      { // Iterate.


// Calulate f(r) ie. f_r.

        if(res_ratio>=1.0)
	{
          f_r = (7.885e-1)+(9.286e-3)*temp+(-2.462e-5)*temp*temp;
          f_r += (-3.368e-7)*temp*temp*temp;
          f_r = f_r*log(res_ratio);
	}
        else
	{
          f_r = (9.243e-1)+(3.059e-3)*temp+(-1.188e-6)*temp*temp;
          f_r = f_r*log(res_ratio);
	}

        if(f_r>=-0.2)
	{ // The 1A coeff.
           rh = 102.0 -(69.0/(1.000+(7.290e-1)*f_r+(-5.580e-2)*f_r*f_r+
                (7.480e-3)*f_r*f_r*f_r+(1.010e-2)*f_r*f_r*f_r*f_r));
	}
        else
	{ // The 1B coeff.
           rh = 0.0 -(69.0/(-2.440+(-7.370)*f_r+(-9.980e1)*f_r*f_r+
                (-2.514e2)*f_r*f_r*f_r+(-2.110e2)*f_r*f_r*f_r*f_r));
	}

        if((fabs(rh-rh_last)<0.001) || (fabs(rh-rh_old)<0.001))
          interval = interval*0.95;
 
        rh_old = rh_last;
        rh_last = rh;

        if(fabs(rh-(double)(sd[i].rh))>0.00001)
	{
          if(rh < sd[i].rh)
            res_ratio += interval;
          else
            res_ratio -= interval;
	}

        if(res_ratio <=0.0)
	{
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
      if(res_ratio <0.0)
        res_ratio = fabs(res_ratio);

      if(res_ratio>=1.0)
      {
        f_r = (7.885e-1)+(9.286e-3)*temp+(-2.462e-5)*temp*temp;
        f_r += (-3.368e-7)*temp*temp*temp;
        f_r = f_r*log(res_ratio);
      }
      else
      {
        f_r = (9.243e-1)+(3.059e-3)*temp+(-1.188e-6)*temp*temp;
        f_r = f_r*log(res_ratio);
      }

      rht = 102.0 -(69.0/(1.000+(7.290e-1)*f_r+(-5.580e-2)*f_r*f_r+
              (7.480e-3)*f_r*f_r*f_r+(1.010e-2)*f_r*f_r*f_r*f_r));

      rh = 102.0 -(69.0/(1.000+(7.290e-1)*f_r+(-5.580e-2)*f_r*f_r+
            (7.480e-3)*f_r*f_r*f_r+(1.010e-2)*f_r*f_r*f_r*f_r));


//sprintf(line,"#%5.1f %5.1f %5.1f %f",sd[i].temp,sd[i].rh,rht,res_ratio);
//cout<<line<<endl;

      if(j<5000)
        sd[i].rh = rh;
      else
        sd[i].qrh = 4.0;

      if(sd[i].rh <= 0.05)
         sd[i].rh = 0.1;

    } // Have a valid point.

  } // For each point.

} // End of correct VIZ hum.

int main(int argc, char *argv[])
{
  ifstream ifile, n_l_t_file;
  ofstream ofile, logfile;
  SOUND_DATA_ARR sounding_raw_data;
  char line[LINE_LEN+1], sonde_type[12], log_file[20], outline[75];
  double elev_smooth[MAX_OB], azim_smooth[MAX_OB], work_array[MAX_OB]; 
  double angtime[MAX_OB], x_pos[MAX_OB], y_pos[MAX_OB], xs[MAX_OB], ys[MAX_OB];
  double limit_angle[360];
  float site_lon, site_lat, sur_wind_speed, sur_wind_dir, term_alt;
  float rh33_res; 
  int elev_angle_status[MAX_OB], azim_angle_status[MAX_OB];
  int angle_status[MAX_OB], qxy[MAX_OB];
  int i, num_met_obs=0, num_ang_obs = 0, num_obs=0;
  int nom_rel_year, nom_rel_month, READ_ANG_FLAG;
  int read_flag;
  time_t t1;

  init_data(sounding_raw_data);
  strcpy(sonde_type,"");strcpy(outline,"");
  strcpy(log_file,"nwsVIZ2000.log");
  rh33_res = -1;
  
  if(argc != 2)
  {
    cerr << "Error in command line format" << endl;
    exit(1);
  }

  ifile.open(argv[1],ios::in|ios::nocreate);
  if(!ifile)
  {
    cerr << "Failed to open input file '" << argv[1] << "'." << endl;
    exit(1);
  }
  
  logfile.open(log_file,ios::app|ios::out);
  if(!logfile)
  {
    cerr << "Failed to open log file '" << log_file << "'." << endl;
    exit(1);
  }
  
  time(&t1);
  sprintf(outline,"GMT: %s\n", asctime(gmtime(&t1)));
  logfile << outline ;

//
// Read header information.
//

  read_flag=read_header(ifile,ofile,logfile,argv[1], limit_angle,
  &site_lat,&site_lon, &sur_wind_speed, &sur_wind_dir, &term_alt, 
  sonde_type, &rh33_res, &nom_rel_year,&nom_rel_month);

//
// Read in the met data.
//

  if(read_flag)
    num_met_obs = read_met_data(ifile,logfile,sounding_raw_data,term_alt,&READ_ANG_FLAG);
//cout << "num_met_obs " << num_met_obs << endl;
//
// Read in the angle data.
//
//  cout << "READ_ANG_FLAG " << READ_ANG_FLAG << endl;

  if(read_flag && READ_ANG_FLAG)
    num_ang_obs = read_angle_data(ifile,sounding_raw_data);
  else{
    sprintf(outline,"Position Data assumed invalid - Class file contains no winds\n");
    logfile << outline ;cout << outline ;
    num_ang_obs = 0;
  }
//cout << "num_ang_obs " << num_ang_obs << endl;  
//
// Determine the total number of observations and then
// determine when the sounding reached its min press and
// stop it there.
//


  i = 0;
  if(term_alt > 0){
//        cout << "if section" << endl;
  	while(((sounding_raw_data[i].alt < (term_alt-20.0)) ||
        	(fabs(sounding_raw_data[i].alt - 99999.0) < 0.1))
         	&& (i < MAX_OB))
    		i++;
        
  }else{
//       cout << "else section" << endl;
	while(fabs(sounding_raw_data[i].alt - 99999.0) > 0.1 && (i < MAX_OB))
    		i++;
  }


  num_obs = i+1;
//cout << "num_obs " << num_obs << endl;

  if(num_obs > num_met_obs)
 	num_obs = num_met_obs;
//  if(num_obs > num_ang_obs)
//    num_obs = num_ang_obs;

//
// Smooth the elev angles.
//
//cout << "Smoothing Elevation angles " << endl;
  if(read_flag && num_ang_obs > 0)
  {
    for(i=0;i<MAX_OB;i++)
    {
      if(i<num_obs)
        work_array[i] = sounding_raw_data[i].elev;
      else
        work_array[i] = 999.0;

      if(fabs(work_array[i]- 999.0)<0.1)
        elev_angle_status[i] = 9;
      else
        elev_angle_status[i] = 0;

      angtime[i] = sounding_raw_data[i].time;   
      elev_smooth[i] = 999.0;
    }
//    cout << "Calling smooth_elev" << endl;
    smooth_elev(work_array,elev_smooth,angtime,num_obs,elev_angle_status,
                limit_angle,sounding_raw_data,0);
  }

//
// Smooth azim. angles.
//
//cout << "Smoothing Azimuth angles " << endl;
  if(read_flag && num_ang_obs > 0)
  {
    for(i=0;i<MAX_OB;i++)
    {
      if(i<num_obs)
        work_array[i] = sounding_raw_data[i].azim;
      else
        work_array[i] = 999.0;

      if(fabs(work_array[i]- 999.0)<0.1)
        azim_angle_status[i] = 9;
      else
        azim_angle_status[i] = 0;
    }
//    cout << "Calling smooth_azim" << endl;
    smooth_azim(work_array,azim_smooth,angtime,num_obs,
                azim_angle_status,limit_angle,sounding_raw_data);
  }

//
// Merge the angle status arrays into one array.
//

  if(read_flag && num_ang_obs > 0)
  {
    for(i=0;i<MAX_OB;i++)
      if((azim_angle_status[i] == 9) ||(elev_angle_status[i] == 9))
        angle_status[i] = 9;
      else if((azim_angle_status[i] == 4) ||(elev_angle_status[i] == 4))
        angle_status[i] = 4;
      else
        angle_status[i] = 0;
  }

//
// Calulate the X and Y position of sonde.
//
//cout << "Calculating X and Y sonde position " << endl;

  if(read_flag && num_ang_obs > 0)
  {
    for(i=0;i<MAX_OB;i++)
    {
      if(i<num_obs)
        work_array[i] = sounding_raw_data[i].alt;
      else
       work_array[i] = 99999.0;
    }
    cal_position(elev_smooth,azim_smooth,work_array,x_pos,y_pos,qxy,num_obs);
  }

//
// Calulate winds.  Then put in surface obs.
//

  if(read_flag)
  {
    cal_uv(sounding_raw_data,x_pos,y_pos,xs,ys,qxy,angle_status,num_obs);
    sur_wind_speed *= 0.51444; // Knots to m/s.
    sounding_raw_data[0].wind_spd = sur_wind_speed;
    sounding_raw_data[0].wind_dir = sur_wind_dir;
    sounding_raw_data[0].u_cmp = sin((sur_wind_dir+180.0)*M_PI/180.0)*
                                 sur_wind_speed;
    sounding_raw_data[0].v_cmp = cos((sur_wind_dir+180.0)*M_PI/180.0)*
                                 sur_wind_speed;
    sounding_raw_data[0].qu = 99.0;
    sounding_raw_data[0].qv = 99.0;
  }

//
// Calclate Lat and lons.
//

  if(read_flag && num_ang_obs > 0)
    cal_latlon(sounding_raw_data,xs,ys,site_lon,site_lat,qxy);
  else
  {
    sounding_raw_data[0].lon = site_lon;
    sounding_raw_data[0].lat = site_lat;
  }
//
// Calclate wind speed and direction..
//

  if(read_flag && num_ang_obs > 0)
    cal_spddir(sounding_raw_data);

//
// Correct the humidities for each of the sonde types.
//

  if((read_flag) && (HUM_COR_FLAG))
    if((strncmp(sonde_type,"VIZ B2",6))==0)
    {
      cout << "No humidity correction for a " << sonde_type << " sonde." << endl; 
    }
    else if((strncmp(sonde_type,"VIZ",3))==0)
    {
      correct_viz_hum(sounding_raw_data,rh33_res,nom_rel_year,
                      nom_rel_month,num_obs);
    }
    else
      cout << "You have a " << sonde_type <<" sonde." << endl;

//
// Calclate dew point.
//

  if(read_flag)
    cal_dewpt(sounding_raw_data);

//
// Calclate ascen. rate.
//

  if(read_flag)
    cal_ascen(sounding_raw_data);

//
// For grins put smoothed value into raw value.
//
/*

  if(num_ang_obs > 0){
    for(i=0;i<num_obs;i++)
    {
      sounding_raw_data[i].elev = elev_smooth[i];
      sounding_raw_data[i].azim = azim_smooth[i];
    }
  }
*/
//
// Write data and close files.
//

  write_class(ofile,sounding_raw_data,num_obs);
  ifile.close();
  ofile.close();
  logfile.close();
  return(0);
}





