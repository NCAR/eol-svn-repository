/*-------------------------------------------------------------------
** File /home/lia/convert_data/lib/readQCF.c
**
** This file contains the function ReadQCF_Record() to read a 
** QCF record from a file.
**
** 03/02/95	Lia Pennington		Original Version 
**------------------------------------------------------------------*/

#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "qcf.h"

#define FALSE 0
#define TRUE  1

extern FILE *qcffile;
extern QCF_REC_TYPE qcf_record;


/*-------------------------------------------------------------------
** ReadQCF_Record()
**
** description.
**------------------------------------------------------------------*/
int ReadQCF_Record(void)
    {
    char ch;
    int i;

    fscanf(qcffile, "%8s %5s %8s %5s", 
                       qcf_record.nomDate,
                       qcf_record.nomTime,
                       qcf_record.actualDate,
                       qcf_record.actualTime);

    if (feof(qcffile)) 
       return FALSE;

    ch = getc(qcffile); /* read a blank character */
    if (feof(qcffile)) 
       return FALSE;

    for (i = 0; i < 10; i++)
        qcf_record.networkId[i] = getc(qcffile);
    qcf_record.networkId[10] = '\0';

    ch = getc(qcffile); /* read a blank character */
    if (feof(qcffile)) 
       return FALSE;

    for (i = 0; i < 15; i++)
        qcf_record.stationId[i] = getc(qcffile);
    qcf_record.stationId[15] = '\0';

    ch = getc(qcffile); /* read a blank character */
    if (feof(qcffile)) 
       return FALSE;

    fscanf(qcffile, "%f %f %d %f %f %c %f %c %f %c %f %c %f %c %f %c %f %c %f %c",                     &qcf_record.lat,
                       &qcf_record.lon,
                       &qcf_record.occurence,
                       &qcf_record.elevation,
                       &qcf_record.pressure,
                       &qcf_record.press_flag,
                       &qcf_record.sealvlprs,
                       &qcf_record.seaflag,
                       &qcf_record.comp_sealvl,
                       &qcf_record.compflag,
                       &qcf_record.temperature,
                       &qcf_record.tmp_flag,
                       &qcf_record.dew_point,
                       &qcf_record.dewflag,
                       &qcf_record.windSpeed,
                       &qcf_record.wspdflag,
                       &qcf_record.windDir,
                       &qcf_record.wdirflag,
                       &qcf_record.precip,
                       &qcf_record.prcflag);

    ch = getc(qcffile); /* read a blank character */
    if (feof(qcffile)) 
       return FALSE;

    qcf_record.squallGust = getc(qcffile);
    if (feof(qcffile)) 
       return FALSE;

    ch = getc(qcffile); /* read a blank character */
    if (feof(qcffile)) 
       return FALSE;

    fscanf(qcffile, "%f %c %d %c %f %c %f %d %c %d %c %f %d %c %d %c %f %d %c %d %c\n",
                       &qcf_record.squallGustSpeed,
                       &qcf_record.squallGustFlag,
                       &qcf_record.present_wx,
                       &qcf_record.pw_flag,
                       &qcf_record.visibility,
                       &qcf_record.vis_flag,
                       &qcf_record.ceilingHt1,
                       &qcf_record.ch1_code,
                       &qcf_record.ch1_flag,
                       &qcf_record.cloud_amt1,
                       &qcf_record.ca1_flag,
                       &qcf_record.ceilingHt2,
                       &qcf_record.ch2_code,
                       &qcf_record.ch2_flag,
                       &qcf_record.cloud_amt2,
                       &qcf_record.ca2_flag,
                       &qcf_record.ceilingHt3,
                       &qcf_record.ch3_code,
                       &qcf_record.ch3_flag,
                       &qcf_record.cloud_amt3,
                       &qcf_record.ca3_flag);

   return TRUE;
   }

