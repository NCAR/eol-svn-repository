/*------------------------------------------------------------------
**
** check_QCF_tightlimits.c
**
** Program to do tighter gross limits checks on values in a QCF record.
**
** 01/29/98	LEC			Original Version
**
** 000 01 Jan 98 lec
**  This is really just another version of L. Pennington's
**  check_QCF_file.c program with tighter limits (Version2).
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
       printf("Usage:  check_QCF_tightlimits <qcf-filename> <output-filename>\n");
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
       fprintf(stderr, "\nChecking QCF file: %s...\n", qcffilename);
 
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

	/*
        if (!Check_QCFRecord(qcf_record))
            fprintf(stderr, "WARNING: INVALID QCF value(s) in record: %d.\n\n",
                count);	
		*/
	Check_QCFRecord(qcf_record, count);
	
        }
    
    
    
    fclose(qcffile);
    }


