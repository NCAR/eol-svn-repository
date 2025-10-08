/*------------------------------------------------------------------
**
** /home/lia/convert_data/src/check_QCF_file.c
**
** Program to do gross limits checks on values in a QCF record.
**
** 03/02/95		Lia Pennington		Original Version
**
** 001 5 Aug 97 jag
**    Modified to compile under CC (C++ compiler).  Main must return
**    int (not void) and must include stdlib.h for exit function
**    prototype. Passed QCF_REC_TYPE to Check_QCFRecord because 
**    Check_QCFRecord as defined in check_qcfvales.c expects it.
** 
** 002 5 May 99 jag
**    Modified to warn user of negative zero values, to accept 
**    missing elevations without issuing a warning, and to issue
**    each complete warning on a single line.
**----------------------------------------------------------------*/

/*---------------
** Include Files
**--------------*/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "qcf.h"

/*------------------
** Global Variables
**-----------------*/
FILE *qcffile;
QCF_REC_TYPE qcf_record;

/*---------------------
** Function Prototypes
**--------------------*/
int ReadQCF_Record(void);
int Check_QCFRecord(QCF_REC_TYPE,int);

/*-------------------------------------------------------------------
** Main
**------------------------------------------------------------------*/
main (int argc, char *argv[])
    {
    char qcffilename[180];
    int count = 0;

    /*-----------------------------------------------------
    ** Check for correct number of command-line arguments.
    **---------------------------------------------------*/
    if (argc != 2) 
       {
       printf("Usage:  check_QCF_file <qcf-filename>\n");
       exit(1);
       }

    /*--------------------------------------------------
    ** The command-line argument is the filename of
    ** the QCF file to be processed.
    **--------------------------------------------------*/
    strcpy(qcffilename, argv[1]);
 
    /*---------------------------------
    ** Open the QCF data file.
    **--------------------------------*/
    if (!(qcffile = fopen(qcffilename, "r")))
       {
       fprintf(stderr, "Error opening file %s.\n", qcffilename);
       exit(1);
       }
    else
       fprintf(stderr, "Checking QCF file: %s.\n", qcffilename);
 
    /*-------------------------------
    ** Read and check each QCF record.
    **-------------------------------*/
    while (!feof(qcffile))
        {
        count++;
        if (!ReadQCF_Record())
            {
            fprintf(stderr, 
              "File READ problem encountered; exiting program.\n");
            exit(2);
            }

        Check_QCFRecord(qcf_record,count);
        }

    fclose(qcffile); 
    }
