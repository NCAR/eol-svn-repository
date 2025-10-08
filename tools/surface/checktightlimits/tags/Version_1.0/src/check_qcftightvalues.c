/*------------------------------------------------------------------
**
** check_qcftightvalues.c
**
** This file contains several functions to do tighter limits checks 
** on data contained in a QCF record.
**
** 01/29/98 lec  Original Version
**
** 000 29 Jan 98 lec
**   The original version is basically L. Pennington's
**   check_qcf_values.c s/w with tighter limits.
** 000 22 Mar 99 lec
**   Updated Lats to contain upper LakeICE limit of 55 degrees.
**
** 005 25 July 2001 jag
**    Modified to be able to check QCF records beginning with either
**    YY/MM/DD or YYYY/MM/DD, but not both.
**----------------------------------------------------------------*/

/*---------------
** Include Files
**--------------*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <nan.h>
#include "convert.h"
#include "qcf.h"

/*------------------
** Global Variables
**-----------------*/
static int NUMDAYS[13] = {0,31,28,31,30,31,30,31,31,30,31,30,31};

/*---------------------
** Function Prototypes
**--------------------*/
int Check_Float_tightValue(float value, char qc_flag, DATAVALUE_TYPE value_type);
int Check_Int_Value(int value, DATAVALUE_TYPE value_type);
int Check_Date_Value(char *datevalue);
int Check_Time_Value(char *datevalue);


/*-------------------------------------------------------------------
** Check_QCFRecord()
**
** This function checks the values of the global QCF record to see if each
** value is within a reasonable range.  If all values of the record
** check out, the function returns TRUE; otherwise, FALSE is returned
** if any one value fails its test.
** 
**------------------------------------------------------------------*/
int Check_QCFRecord(QCF_REC_TYPE qcf_record)
    {
    int istatus, goodStatus;

    goodStatus = TRUE;

    istatus = Check_Float_tightValue(qcf_record.lat, ' ', latitude);
    if (!istatus)
      {
      goodStatus = FALSE;
      fprintf (stderr, "ERROR: Bad lat (%7.2f) for %-s %-s\n",
             qcf_record.lat,
             qcf_record.networkId, qcf_record.stationId);
      }


    istatus = Check_Float_tightValue(qcf_record.lon, ' ', longitude);
    if (!istatus)
       {
       goodStatus = FALSE;
       fprintf (stderr, "ERROR: Bad lon (%7.2f) for %-s %-s\n",
             qcf_record.lon,
             qcf_record.networkId, qcf_record.stationId); 
       }

    istatus = Check_Float_tightValue(qcf_record.elevation, ' ', elevation);
    if (!istatus) 
       {
       goodStatus = FALSE;
       fprintf (stderr, "ERROR: Bad elev (%7.2f) for %-s %-s\n",
             qcf_record.elevation,
             qcf_record.networkId, qcf_record.stationId); 
       }

    /*------------------------------------------------------------
    ** Check the variable only if its corresponding flag indicates
    ** that its not missing.
    **
    ** Run checks on each float variable.
    **------------------------------------------------------------*/
    istatus = Check_Float_tightValue(qcf_record.pressure, qcf_record.press_flag,
      pressure);
    if (!istatus)
       {
       goodStatus = FALSE;
       fprintf (stderr, "ERROR: Bad stn press (%7.2f) for %-s %-s on %-s %-s\n",
             qcf_record.pressure,
             qcf_record.networkId, qcf_record.stationId,
             qcf_record.nomDate, qcf_record.nomTime);
       }


    istatus = Check_Float_tightValue(qcf_record.sealvlprs, qcf_record.seaflag,
       seaLvlPressure);
    if (!istatus)  
       {
       goodStatus = FALSE;
       fprintf (stderr, "ERROR: Bad SLP (%7.2f) for %-s %-s on %-s %-s\n", 
             qcf_record.sealvlprs, 
             qcf_record.networkId, qcf_record.stationId,
             qcf_record.nomDate, qcf_record.nomTime);

       }

    istatus = Check_Float_tightValue(qcf_record.comp_sealvl, qcf_record.compflag,
      cSeaLvlPressure);
    if (!istatus)
       {
       goodStatus = FALSE;
       fprintf (stderr, "ERROR: Bad Calc SLP (%7.2f) for %-s %-son %-s %-s\n", 
             qcf_record.comp_sealvl, 
             qcf_record.networkId, qcf_record.stationId,
             qcf_record.nomDate, qcf_record.nomTime);
       }
/*-------*/

    istatus = Check_Float_tightValue(qcf_record.temperature, qcf_record.tmp_flag,
       temperature);
    if (!istatus)
       {
       goodStatus = FALSE;
       fprintf (stderr, "ERROR: Bad temperature (%7.2f) for %-s %-s on %-s %-s\n",
             qcf_record.temperature,
             qcf_record.networkId, qcf_record.stationId,
             qcf_record.nomDate, qcf_record.nomTime);

       }

    istatus = Check_Float_tightValue(qcf_record.dew_point, qcf_record.dewflag,
       dewPoint);
    if (!istatus)
       {
       goodStatus = FALSE;
       fprintf (stderr, "ERROR: Bad dew_point (%7.2f) for %-s %-s on %-s %-s\n",
               qcf_record.dew_point,
               qcf_record.networkId, qcf_record.stationId,
               qcf_record.nomDate, qcf_record.nomTime);

       }

    if ((qcf_record.temperature>-900.0) &&
         (qcf_record.dew_point >-900.0) &&
         (qcf_record.dew_point > qcf_record.temperature) )
       {
       fprintf (stderr, "ERROR: dew_point (%7.2f) > temperature (%7.2f) for %-s %-s on %-s %-s\n",
               qcf_record.dew_point, qcf_record.temperature, 
               qcf_record.networkId, qcf_record.stationId,
               qcf_record.nomDate, qcf_record.nomTime);

       goodStatus = FALSE;
       }

    if ( (qcf_record.temperature>-900.0) && 
         (qcf_record.dew_point >-900.0) && 
         (qcf_record.temperature - qcf_record.dew_point) > 50)
       { 
       fprintf (stderr, 
               "WARNING: temperature (%7.2f) - dewpoint (%7.2f) > 50 for %-s %-s on %-s %-s\n\n",
               qcf_record.temperature, qcf_record.dew_point,
               qcf_record.networkId, qcf_record.stationId,
               qcf_record.nomDate, qcf_record.nomTime);
       goodStatus = FALSE;
       } 

/*-------*/

    istatus = Check_Float_tightValue(qcf_record.windSpeed, qcf_record.wspdflag,
       windSpeed);
    if (!istatus)
       {
       goodStatus = FALSE;
       fprintf (stderr, "ERROR: Bad Wind Speed (%7.2f) for %-s %-s on %-s %-s\n",
             qcf_record.windSpeed,
             qcf_record.networkId, qcf_record.stationId,
             qcf_record.nomDate, qcf_record.nomTime);
       }
 

    istatus = Check_Float_tightValue(qcf_record.windDir, qcf_record.wdirflag,
       windDir);
    if (!istatus)
       {
       goodStatus = FALSE;
       fprintf (stderr, "ERROR: Bad wind dir (%7.2f) for %-s %-s on %-s %-s\n",
             qcf_record.windDir,
             qcf_record.networkId, qcf_record.stationId,
             qcf_record.nomDate, qcf_record.nomTime);

       } 

    istatus = Check_Float_tightValue(qcf_record.squallGustSpeed,
       qcf_record.squallGustFlag, squallGust);
    if (!istatus)
       { 
       goodStatus = FALSE; 
       fprintf (stderr, "ERROR: Bad squall Gust (%7.2f) for %-s %-s on %-s %-s\n", 
             qcf_record.squallGustSpeed,
             qcf_record.networkId, qcf_record.stationId,
             qcf_record.nomDate, qcf_record.nomTime);
       }

    if ((qcf_record.squallGust != 'G') && (qcf_record.squallGust != 'S') &&
        (qcf_record.squallGust != ' '))
       { 
       fprintf(stderr, "Invalid symbol for SquallGust; indicator = %c\n\n",
         qcf_record.squallGust);
       goodStatus = FALSE;
       } 

/*-------*/

    istatus = Check_Float_tightValue(qcf_record.precip, qcf_record.prcflag,
       precipitation);
    if (!istatus) 
       { 
       goodStatus = FALSE; 
       fprintf (stderr, "ERROR: Bad precip (%7.2f) for %-s %-s on %-s %-s\n", 
             qcf_record.precip,
             qcf_record.networkId, qcf_record.stationId,
             qcf_record.nomDate, qcf_record.nomTime);

       }
/*-------*/

    istatus = Check_Float_tightValue(qcf_record.visibility, qcf_record.vis_flag,
       visibility);
    if (!istatus)  goodStatus = FALSE;

    /*------------------------------------------------------------------
    ** Run checks on each integer code and float value for Ceiling Code
    ** and Ceiling Height.
    **-----------------------------------------------------------------*/
    istatus = Check_Float_tightValue(qcf_record.ceilingHt1, qcf_record.ch1_flag,
       ceilingHeight1);
    if (!istatus)  goodStatus = FALSE;

    istatus = Check_Int_Value(qcf_record.ch1_code, ceilingFlag1);
    if (!istatus)  goodStatus = FALSE;

    istatus = Check_Float_tightValue(qcf_record.ceilingHt2, qcf_record.ch2_flag,
        ceilingHeight2);
    if (!istatus)  goodStatus = FALSE;

    istatus = Check_Int_Value(qcf_record.ch2_code, ceilingFlag2);
    if (!istatus)  goodStatus = FALSE;

    istatus = Check_Float_tightValue(qcf_record.ceilingHt3, qcf_record.ch3_flag,
       ceilingHeight3);
    if (!istatus)  goodStatus = FALSE;

    istatus = Check_Int_Value(qcf_record.ch3_code, ceilingFlag3);
    if (!istatus)  goodStatus = FALSE;

    /*-----------------------------------
    ** Run checks on each integer value.
    **---------------------------------*/
    if ((qcf_record.ca1_flag != 'M') && (qcf_record.ca1_flag != 'N'))
        {
        istatus = Check_Int_Value(qcf_record.cloud_amt1, cloudAmount1);
        if (!istatus)  goodStatus = FALSE;
        }
    else if (qcf_record.ca1_flag == 'M')
        {
        if (qcf_record.ch1_flag != 'M')
           {
           goodStatus = FALSE;
           fputs("Ceiling Height reported without Cloud Amount.\n\n", stderr);
           }
        }

    if ((qcf_record.ca2_flag != 'M') && (qcf_record.ca2_flag != 'N'))
        {
        istatus = Check_Int_Value(qcf_record.cloud_amt2, cloudAmount2);
        if (!istatus)  goodStatus = FALSE;
        }
    else if (qcf_record.ca2_flag == 'M')
        {
        if (qcf_record.ch2_flag != 'M')
           {
           goodStatus = FALSE;
           fputs("Ceiling Height reported without Cloud Amount.\n\n", stderr);
           }
        }

    if ((qcf_record.ca3_flag != 'M') && (qcf_record.ca3_flag != 'N'))
        {
        istatus = Check_Int_Value(qcf_record.cloud_amt3, cloudAmount3);
        if (!istatus)  goodStatus = FALSE;
        }
    else if (qcf_record.ca3_flag == 'M')
        {
        if (qcf_record.ch3_flag != 'M')
           {
           goodStatus = FALSE;
           fputs("Ceiling Height reported without Cloud Amount.\n\n", stderr);
           }
        }

    /*--------------------------------------------
    ** Validate the Dates and Times in the record.
    **--------------------------------------------*/
    assert(qcf_record.nomDate != NULL);
    istatus = Check_Date_Value(qcf_record.nomDate);
    if (!istatus)  goodStatus = FALSE;
        
    assert(qcf_record.actualDate != NULL);
    istatus = Check_Date_Value(qcf_record.actualDate);
    if (!istatus)  goodStatus = FALSE;
       
    assert(qcf_record.nomTime != NULL);
    istatus = Check_Time_Value(qcf_record.nomTime);
    if (!istatus)  goodStatus = FALSE;
     
    assert(qcf_record.actualTime != NULL);
    istatus = Check_Time_Value(qcf_record.actualTime);
    if (!istatus)  goodStatus = FALSE;

    return (goodStatus);
    }

/*-------------------------------------------------------------------
** Check_Float_tightValue()
**
** Checks a floating point value given the data type name
** (i.e. elevation, pressure, latitude, etc).  If the value
** passed in is not within its defined range, the function will
** return FALSE.
**
** INPUTS: value - floating point value to check;
**         value_type - the type of the value (choose from:
**            elevation, latitude, longitude, pressure,
**            seaLvlPressure, cSeaLvlPressure, temperature, dewPoint, 
**            windSpeed, windDir, precipitation, squallGust, visibility, 
**            ceilingHeight1, ceilingHeight2, ceilingHeight3,
** 
**------------------------------------------------------------------*/
int Check_Float_tightValue(float value, char qc_flag, DATAVALUE_TYPE value_type)
    {
    float minimum, maximum;
    char type_str[30];
    
    /*---------------------------------------------------*
     * These values set for Mid North American Continent. 
     *---------------------------------------------------*/
    switch (value_type)
        {
        case elevation: minimum = -5.00; maximum = 4300.00;
           strcpy(type_str, "Elevation");
           break;
        case latitude:  minimum = 20.00; maximum = 55.00; /* was 50 */
           strcpy(type_str, "Latitude");
           break;
        case longitude: minimum = -125.00; maximum = 65.00;
           strcpy(type_str, "Longitude");
           break;
        case pressure:  minimum = 700.00; maximum = 1060.00; /* Above Mt Evans to Siberia */
           strcpy(type_str, "Pressure");
           break;
        case seaLvlPressure:  minimum = 915.00; maximum = 1060.00; /* Hurricane Hugo at 918. */
           strcpy(type_str, "SeaLevelPressure");
           break;
        case cSeaLvlPressure: minimum = 915.00; maximum = 1060.00;
           strcpy(type_str, "computedSeaLvlPres");
           break;
        case temperature: minimum = -52.00; maximum = 50.00; /*-60F(MN 1996)-122 F*/
           strcpy(type_str, "Temperature");
           break;
        case dewPoint:    minimum = -52.00; maximum = 50.00;
           strcpy(type_str, "DewPoint");
           break;
        case windSpeed: minimum = 0.0; maximum = 72.00; /* Hugo Max of 160mph */
           strcpy(type_str, "windSpeed");
           break;
        case windDir:   minimum = 0.0; maximum = 360.00;
           strcpy(type_str, "windDirection");
           break;
        case precipitation: minimum = 0.0; maximum = 200.00; /* Max is Daily limit of 7.9 inches*/
           strcpy(type_str, "Precipitation");
           break;
        case squallGust: minimum = 0.0; maximum = 103.00; /* Mt Wash. max of approx 231 mph */
           strcpy(type_str, "SquallGust");
           break;
        case visibility: minimum = 0.0; maximum = 99999.99;
           strcpy(type_str, "Visibility");
           break;
        case ceilingHeight1: minimum = 0.0; maximum = 9999.99;
           strcpy(type_str, "CeilingHeight1");
           break;
        case ceilingHeight2: minimum = 0.0; maximum = 9999.99;
           strcpy(type_str, "CeilingHeight2");
           break;
        case ceilingHeight3: minimum = 0.0; maximum = 9999.99;
           strcpy(type_str, "CeilingHeight3");
           break;
        default:
           fputs("WARNING: bad datatype value specified\n", stderr);
           return FALSE;
        }

    /*--------------------------------------------------------
    ** If the flag indicates that the value should be missing
    ** or unrecorded, check the value is equal to the missing
    ** value -999.99.
    **--------------------------------------------------------*/
    if ((value_type != elevation) && (value_type != latitude) && 
       (value_type != longitude))
        {
        switch (qc_flag)
            {
            case 'M':
            case 'N':
            case 'I':
            case 'C':
               if ((value > -1000.0) && (value < -999.98))
                  return TRUE;
               else
                  {
                  fprintf(stderr, "Test failed for missing value %s = %7.2f\n", 
                     type_str, value);
                  return FALSE;
                  }
               break;
            }
        }

    /*----------------------------------------------------------
    ** Otherwise check if the value is within its defined range.
    **---------------------------------------------------------*/
    if ((value < minimum) || (value > maximum) || (isnanf(value)))
       {
       fprintf(stderr, "Test failed for %s = %7.2f\n", type_str, value);
       return FALSE;
       }
    else
       return TRUE;
    }


/*-------------------------------------------------------------------
** Check_Int_Value()
**
** Checks an integer value given the the data type name
** (i.e. elevation, pressure, latitude, etc).  If the value
** passed in is not within its defined range, the function will
** return FALSE.
**
** INPUTS: value - integer value to check;
**         value_type - the type of the value (choose from:
**            ceilingFlag1, ceilingFlag2, ceilingFlag3,
**            cloudAmount1, cloudAmount2, cloudAmount3). 
**------------------------------------------------------------------*/
int Check_Int_Value(int value, DATAVALUE_TYPE value_type)
    {
    int minimum, maximum;
    char type_str[30];

    switch (value_type)
        {
        case ceilingFlag1: minimum = 0; maximum = 15;
           strcpy(type_str, "CeilingFlag1");
           break;
        case ceilingFlag2: minimum = 0; maximum = 15;
           strcpy(type_str, "CeilingFlag2");
           break;
        case ceilingFlag3: minimum = 0; maximum = 15;
           strcpy(type_str, "CeilingFlag3");
           break;
        case cloudAmount1: minimum = 0; maximum = 15;
           strcpy(type_str, "CloudAmount1");
           break;
        case cloudAmount2: minimum = 0; maximum = 15;
           strcpy(type_str, "CloudAmount2");
           break;
        case cloudAmount3: minimum = 0; maximum = 15;
           strcpy(type_str, "CloudAmount3");
           break;
        default:
           fprintf(stderr, "WARNING: bad datatype value specified\n\n");
           return FALSE;
        }

    if ((value < minimum) || (value > maximum))
       {
       fprintf(stderr, "Test failed for %s = %d\n", type_str, value);
       return FALSE;
       }
    else
       return TRUE;
    }


/*-------------------------------------------------------------------
** Check_Date_Value()
**
** Check a date string to see if it contains a valid date in the
** form YY/MM/DD or YYYY/MM/DD where YY is the 2-digit year, YYYY is
** the 4 digit year, MM is the 2-digit month, and DD is the 2-digit 
** day.
**------------------------------------------------------------------*/
int Check_Date_Value(char *datevalue)
    {
    int month, day, year, istatus;
    char *temp_str, datestr[11];
    static short year_length = 0;

    istatus = TRUE;

    /*--------------------------------- 
    ** Copy then parse the date string.
    **---------------------------------*/
    strcpy (datestr, datevalue);
    temp_str = strtok(datestr, "/");
    if (year_length == 0)
       {
       year_length = strlen(temp_str);
       }
    else if (strlen(temp_str) != year_length)
       {
       istatus = FALSE;
       fprintf(stderr,
 "WARNING: YEAR = %s changes from %d char year to %d char year\n",
       temp_str,year_length, strlen(temp_str));
       }

    /*--------------------------------------------------------
    ** Convert to an integer, then check if it's within range.
    **-------------------------------------------------------*/
    year = atoi(temp_str);
    if (((year_length == 2)  && (year < 0 || year > 99)) ||
        ((year_length == 4)  && (year < 0 || year > 9999)))
       {
       istatus = FALSE;
       fprintf(stderr, "Invalid YEAR = %d\n", year);
       }

    /* Parse out the month. */
    temp_str = strtok(NULL, "/");
 
    /*--------------------------------------------------------
    ** Convert to an integer, then check if it's within range.
    **-------------------------------------------------------*/
    month = atoi(temp_str);
    if ((month < 1) || (month > 12))
       {
       istatus = FALSE;
       fprintf(stderr, "Invalid MONTH = %d\n", month);
       }
 
    /* Parse out the day. */
    temp_str = strtok(NULL, "/");

    /*--------------------------------------------------------
    ** Convert to an integer, then check if it's within range.
    **-------------------------------------------------------*/
    day = atoi(temp_str);

    /* Leap year test */
    if ((year % 4) == 0)  
        NUMDAYS[2] = 29;

    if ((day < 1) || (day > NUMDAYS[month]))
       {
       istatus = FALSE;
       fprintf(stderr, "Invalid DAY = %d\n", day);
       }
 
    return istatus;
    }

/*-------------------------------------------------------------------
** Check_Time_Value()
**
** Check a time string to see if it contains a valid time in the
** form HH:MM where HH is the 2-digit hour and MM is the 2-digit
** number of minutes past the hour.
**------------------------------------------------------------------*/
int Check_Time_Value(char *timevalue)
    {
    int hour, minute;
    int istatus;
    char *temp_str, timestr[6];

    istatus = TRUE;

    /*--------------------------------- 
    ** Copy then parse the date string.
    **---------------------------------*/
    strcpy(timestr, timevalue);
    temp_str = strtok(timestr, ":");

    /*--------------------------------------------------------
    ** Convert to an integer, then check if it's within range.
    **-------------------------------------------------------*/
    hour = atoi(temp_str);

    if ((hour < 0) || (hour > 23))
       {
       istatus = FALSE;
       fprintf(stderr, "Invalid Hour = %d\n", hour);
       }
 
    temp_str = strtok(NULL, ":");

    /*--------------------------------------------------------
    ** Convert to an integer, then check if it's within range.
    **-------------------------------------------------------*/
    minute = atoi(temp_str);

    if ((minute < 0) || (minute > 59))
       {
       istatus = FALSE;
       fprintf(stderr, "Invalid Minutes = %d\n", minute);
       }

    return (istatus);
    }

