/*------------------------------------------------------------------
**
** File /home/lia/convert_data/lib/check_qcfvalues.c
**
** This file contains several functions to do gross limits checks 
** on data contained in a QCF record.
**
** 02/28/95		Lia Pennington		Original Version
**
** 001 10 Jun 96 RIS
**    Removed N as a valid QC flag for the Squall/Gust Indicator
**    to correspond with the documentation for the hourly surface 
**    composites.
**
** 002 5 Aug 97 JAG
**    Added test to check for NaN's in QCF records and produce a
**    warning.
** 003 14 Jan 97 LEC
**    Update s/w to verify if CSLP is -999.99 then the flag must
**    be 'I'. Since this is always a calculated value, the only
**    acceptable flag with -999.99 is 'I' for unable to calc.
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
int Check_Float_Value(float value, char qc_flag, DATAVALUE_TYPE value_type);
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

    istatus = Check_Float_Value(qcf_record.lat, ' ', latitude);
    if (!istatus)  goodStatus = FALSE;
    istatus = Check_Float_Value(qcf_record.lon, ' ', longitude);
    if (!istatus)  goodStatus = FALSE;
    istatus = Check_Float_Value(qcf_record.elevation, ' ', elevation);
    if (!istatus)  goodStatus = FALSE;

    /*------------------------------------------------------------
    ** Check the variable only if its corresponding flag indicates
    ** that its not missing.
    **
    ** Run checks on each float variable.
    **------------------------------------------------------------*/
    istatus = Check_Float_Value(qcf_record.pressure, qcf_record.press_flag,
      pressure);
    if (!istatus)  goodStatus = FALSE;

    istatus = Check_Float_Value(qcf_record.temperature, qcf_record.tmp_flag,
       temperature);
    if (!istatus)  goodStatus = FALSE;

    istatus = Check_Float_Value(qcf_record.sealvlprs, qcf_record.seaflag,
       seaLvlPressure);
    if (!istatus)  goodStatus = FALSE;

    istatus = Check_Float_Value(qcf_record.comp_sealvl, qcf_record.compflag,
      cSeaLvlPressure);
    if (!istatus)  goodStatus = FALSE;

    istatus = Check_Float_Value(qcf_record.dew_point, qcf_record.dewflag,
       dewPoint);
    if (!istatus)  goodStatus = FALSE;

    istatus = Check_Float_Value(qcf_record.windSpeed, qcf_record.wspdflag,
       windSpeed);
    if (!istatus)  goodStatus = FALSE;

    istatus = Check_Float_Value(qcf_record.windDir, qcf_record.wdirflag,
       windDir);
    if (!istatus)  goodStatus = FALSE;

    istatus = Check_Float_Value(qcf_record.precip, qcf_record.prcflag,
       precipitation);
    if (!istatus)  goodStatus = FALSE;

    if ((qcf_record.squallGust != 'G') && (qcf_record.squallGust != 'S') &&
        (qcf_record.squallGust != ' '))
       {
       fprintf(stderr, "Invalid symbol for SquallGust; indicator = %c\n",
         qcf_record.squallGust);
       goodStatus = FALSE;
       }

    istatus = Check_Float_Value(qcf_record.squallGustSpeed, 
       qcf_record.squallGustFlag, squallGust);
    if (!istatus)  goodStatus = FALSE;

    istatus = Check_Float_Value(qcf_record.visibility, qcf_record.vis_flag,
       visibility);
    if (!istatus)  goodStatus = FALSE;

    /*------------------------------------------------------------------
    ** Run checks on each integer code and float value for Ceiling Code
    ** and Ceiling Height.
    **-----------------------------------------------------------------*/
    istatus = Check_Float_Value(qcf_record.ceilingHt1, qcf_record.ch1_flag,
       ceilingHeight1);
    if (!istatus)  goodStatus = FALSE;

    istatus = Check_Int_Value(qcf_record.ch1_code, ceilingFlag1);
    if (!istatus)  goodStatus = FALSE;

    istatus = Check_Float_Value(qcf_record.ceilingHt2, qcf_record.ch2_flag,
        ceilingHeight2);
    if (!istatus)  goodStatus = FALSE;

    istatus = Check_Int_Value(qcf_record.ch2_code, ceilingFlag2);
    if (!istatus)  goodStatus = FALSE;

    istatus = Check_Float_Value(qcf_record.ceilingHt3, qcf_record.ch3_flag,
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
           fputs("Ceiling Height reported without Cloud Amount.\n", stderr);
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
           fputs("Ceiling Height reported without Cloud Amount.\n", stderr);
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
           fputs("Ceiling Height reported without Cloud Amount.\n", stderr);
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
** Check_Float_Value()
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
int Check_Float_Value(float value, char qc_flag, DATAVALUE_TYPE value_type)
    {
    float minimum, maximum;
    char type_str[30];

    switch (value_type)
        {
        case elevation: minimum = -200.00; maximum = 9000.00;
           strcpy(type_str, "Elevation");
           break;
        case latitude:  minimum = -90.00; maximum = 90.00;
           strcpy(type_str, "Latitude");
           break;
        case longitude: minimum = -180.00; maximum = 180.00;
           strcpy(type_str, "Longitude");
           break;
        case pressure:  minimum = 300.00; maximum = 1200.00;
           strcpy(type_str, "Pressure");
           break;
        case seaLvlPressure:  minimum = 800.00; maximum = 1200.00;
           strcpy(type_str, "SeaLevelPressure");
           break;
        case cSeaLvlPressure: minimum = 800.00; maximum = 1200.00;
           strcpy(type_str, "computedSeaLvlPres");
           break;
        case temperature: minimum = -100.00; maximum = 100.00;
           strcpy(type_str, "Temperature");
           break;
        case dewPoint:    minimum = -100.00; maximum = 100.00;
           strcpy(type_str, "DewPoint");
           break;
        case windSpeed: minimum = 0.0; maximum = 200.00;
           strcpy(type_str, "windSpeed");
           break;
        case windDir:   minimum = 0.0; maximum = 360.00;
           strcpy(type_str, "windDirection");
           break;
        case precipitation: minimum = 0.0; maximum = 9999.99;
           strcpy(type_str, "Precipitation");
           break;
        case squallGust: minimum = 0.0; maximum = 9999.99;
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
    ** If Calc Sea Level Pressure has value of -999.99,
    ** the only acceptable flag is 'I', not 'M' or 'N'.
    ** The parameter is always calculated.
    **--------------------------------------------------------*/
    if ((value_type == cSeaLvlPressure) && (value <= -999.00) &&
       (qc_flag != 'I'))
       { 
       fprintf(stderr, "Bad flag for CSLP. Value Missing. Flag must be I. %s = %7.2f, qc_Flag = %1c\n",
               type_str, value, qc_flag);
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
           fprintf(stderr, "WARNING: bad datatype value specified\n");
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
** form YY/MM/DD where YY is the 2-digit year, MM is the 2-digit
** month, and DD is the 2-digit day.
**------------------------------------------------------------------*/
int Check_Date_Value(char *datevalue)
    {
    int month, day, year, istatus;
    char *temp_str, datestr[9];

    istatus = TRUE;

    /*--------------------------------- 
    ** Copy then parse the date string.
    **---------------------------------*/
    strcpy (datestr, datevalue);
    temp_str = strtok(datestr, "/");

    /*--------------------------------------------------------
    ** Convert to an integer, then check if it's within range.
    **-------------------------------------------------------*/
    year = atoi(temp_str);
    if ((year < 0) || (year > 99))
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

