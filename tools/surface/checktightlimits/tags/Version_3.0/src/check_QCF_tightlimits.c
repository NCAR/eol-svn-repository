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
**
** Edited by: Ben Golden
** Date: 06/09/2008
**
** Added functionality to additionally generate a list of network,
** station, and parameters for each Error. (other than date errors)
** This list is then ouput to a file to be used for 
** plot_converted_data.pl
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
FILE *qcfFile[500];
FILE *logFile;
FILE *uniqueFile;
FILE *inputListFile;
QCF_REC_TYPE qcf_record;
UNIQUE_LIST uList;


/*---------------------
** Function Prototypes
**--------------------*/
int ReadQCF_Record(FILE*);
int Check_QCFRecord(QCF_REC_TYPE,int,UNIQUE_LIST*, int*, FILE*);
/*-------------------------------------------------------------------
** Main
**------------------------------------------------------------------*/
main (int argc, char *argv[])
    {
    char qcfFileName[180];
    char logFileName[180];
    char uniqueFileName[180] = "plotCommandList.txt";
    char defaultLogFile[180] = "defaultLogFile.txt";
    char *qcfFileList[180];
    char date[9] = "00000000";
    char beginDateString[11];
    char endDateString[11];
    char defaultInputListFile[180] = "qcfListFile.txt";

    char plotProjectName[180] = "defaultProjectName";
    char plotOutputDirectory[180] = "";

    int count, uCount, auxCount, logFileIndex, uniqueFileIndex, qcfFileListLength, beginDate, endDate, compareDate;

    count = auxCount = uCount = logFileIndex = uniqueFileIndex = qcfFileListLength = beginDate = endDate = compareDate = 0;

    /*-----------------------------------------------------
    ** Check for correct number of command-line arguments.
    **---------------------------------------------------*/
    if (argc < 2) 
       {
       printf("Usage:  check_QCF_tightlimits -q <qcf-filename> -l <log-filename> -u <plot list-filename> -o <out_dir for plot commands> -P <Project for plot commands>\n");
       printf("*Please Note: -q must be used in the first position, -o and -P must appear after -u and -l. Either -u or -l must be used.\n");       
       exit(1);
       }
    if (strcmp(argv[1], "-q") != 0)
	{
	printf("No files given on the command line. Please run with -q <qcf-file>\n");
	exit(1);
	} 

    /*--------------------------------------------------
    ** Sort appropriate command line arguments into
    ** the necessary place for later use. There is some
    ** complexity as a result of the need to allow for
    ** globbing. 
    **--------------------------------------------------*/

/*---go through argv and find the -u and -l flags for output files.---*/

    for(count=0;count<argc;count++)
    	{

	if(strcmp(argv[count], "-l") ==0)
		{
		strcpy(logFileName, argv[count +1]);
		logFileIndex = count;
		}
	if(strcmp(argv[count], "-u") ==0)
		{
		strcpy(uniqueFileName, argv[count +1]);
		uniqueFileIndex = count;
		}
	if(strcmp(argv[count], "-o") ==0)
		{
		strcpy(plotOutputDirectory, argv[count +1]);
		}
	if(strcmp(argv[count], "-P") ==0)
		{
		strcpy(plotProjectName, argv[count +1]);
		}

    	}





/*---Case where only a -q flag is used.---*/

     if(uniqueFileIndex ==0 && logFileIndex ==0)
	{
	//Since neither optional flag was used, just copy all files to the end of argv.
	for(count=2;count<argc;count++)
		{
		qcfFileList[count-2] = (char*) malloc(sizeof(argv[count]));
		qcfFileList[count-2] = argv[count];
		}
	qcfFileListLength = count -2;
	}
    





/*---Case where both flags are used, copy all files until the first optional flag.---*/ 

    if (uniqueFileIndex != 0 && logFileIndex != 0)
    	{
	if (uniqueFileIndex < logFileIndex)
		{
		for(count=2;count<uniqueFileIndex;count++)
			{
			qcfFileList[count-2] = (char*) malloc(sizeof(argv[count]));
			qcfFileList[count-2] = argv[count];
			}
		qcfFileListLength = count -2;		
		}
	else
        	{
        	for(count=2;count<logFileIndex;count++)
            		{
            		qcfFileList[count-2] = (char*) malloc(sizeof(argv[count]));
            		qcfFileList[count-2] = argv[count];
          		}
        	qcfFileListLength = count -2;
        	}

	}






/*---Case where one optional flag is used, copy all files until the used flag.---*/
 
     if(uniqueFileIndex == 0 && logFileIndex != 0)
	{
	for(count=2;count<logFileIndex;count++)
		{
		qcfFileList[count-2] = (char*) malloc(sizeof(argv[count]));
		qcfFileList[count-2] = argv[count];
		}
	qcfFileListLength = count -2;
	}
     if(logFileIndex == 0 && uniqueFileIndex !=0)
  	{
	for(count=2;count<uniqueFileIndex;count++)
		{
		qcfFileList[count-2] = (char*) malloc(sizeof(argv[count]));
		qcfFileList[count-2] = argv[count];
		}
	qcfFileListLength = count -2;
	}



	
    /*----------------------------------------------
    ** Open the QCF data files and the output files.
    **--------------------------------------------*/
 

/*---logfile for standard log output.--*/

    if (logFileIndex !=0)
	{
    	if (!(logFile = fopen(logFileName, "w")))
       		{
       		fprintf(stderr, "Error opening file %s.\n", logFileName);
      		exit(1);
     		}
    	else
    	fprintf(stderr, "\nLog file: %s is writable...\n", logFileName);
	}

/*---if no logFile was found in the argv, use the default---*/

    else
	{
    	if (!(logFile = fopen(defaultLogFile, "w")))
       		{
       		fprintf(stderr, "Error opening file %s.\n", defaultLogFile);
      		exit(1);
     		}
    	else
    	fprintf(stderr, "\nLog file: %s is writable...\n", defaultLogFile);	
	}

/*---inputListFile file for writing. (list of all qcf files recieved from command line glob, or otherwise)---*/

     if (!(inputListFile = fopen(defaultInputListFile, "w")))
      	{
       	fprintf(stderr, "Error opening file %s.\n", inputListFile);
      	exit(1);
   	}
     else
	{
     	fprintf(stderr, "\nQCF list file: %s is writable...\n", defaultInputListFile);
    	for(count=0;count<qcfFileListLength;count++)
		{
		fprintf(inputListFile,"%s\n", qcfFileList[count]);
		}
	}   

/*---uniquefile for unique list output.----*/

    if (uniqueFileIndex != 0)
	{
    	if (!(uniqueFile = fopen(uniqueFileName, "w")))
       		{
       		fprintf(stderr, "Error opening file %s.\n", uniqueFileName);
      		exit(1);
     		}
    	else
    	fprintf(stderr, "\nUnique List file: %s is writable...\n", uniqueFileName);
	}


/*---QCF files for reading.---*/

    for(count=0;count<qcfFileListLength;count++)
	{
    	if (!(qcfFile[count] = fopen(qcfFileList[count], "r")))
       		{
       		fprintf(stderr, "Error opening file %s.\n", qcfFileList[count]);
      		exit(1);
     		}
    	else
      	fprintf(stderr, "\nPreparing to Check QCF file: %s...\n", qcfFileList[count]);
	}
    
    count = 0;
 

    /*-------------------------------
    ** Read and check each QCF record.
    **-------------------------------*/

    for(auxCount=0;auxCount<qcfFileListLength;auxCount++)
    {
    while (!feof(qcfFile[auxCount]))
        {

        count++;
        if (!ReadQCF_Record(qcfFile[auxCount]))
            {
            fprintf(stderr, 
              "File READ problem encountered; error in file %s exiting program.\n", qcfFileList[auxCount]);
            exit(2);
            }

	Check_QCFRecord(qcf_record, count, &uList, &uCount, logFile);

	//remove the slashes from the date string.
	date[0] = qcf_record.nomDate[0];
	date[1] = qcf_record.nomDate[1];
	date[2] = qcf_record.nomDate[2];
	date[3] = qcf_record.nomDate[3];
	date[4] = qcf_record.nomDate[5];
	date[5] = qcf_record.nomDate[6];
	date[6] = qcf_record.nomDate[8];
	date[7] = qcf_record.nomDate[9];
	
	//convert the date string into an int.
	compareDate = atoi (date);
	
	//assign the largest and smallest values as the iteration continues. 
	if (beginDate == 0)
		{
		beginDate = endDate = compareDate;
		}
	if (compareDate > endDate)
		{
		endDate = compareDate;
		}	
	if (compareDate < beginDate)
		{
		beginDate = compareDate;
		}
	
 	}

    }

    /*-------------------------------------
    **Print the contents of Unique_list
    **-----------------------------------*/

    //convert the numerical date back to a string date.
    sprintf(beginDateString, "%d", beginDate);
    sprintf(endDateString, "%d", endDate);

    //reinsert the slashes
    beginDateString[10] = beginDateString[8];
    beginDateString[9] = beginDateString[7];
    beginDateString[8] = beginDateString[6];
    beginDateString[6] = beginDateString[5];	
    beginDateString[5] = beginDateString[4];
    beginDateString[4] = '/';
    beginDateString[7] = '/';

    endDateString[10] = endDateString[8];
    endDateString[9] = endDateString[7];
    endDateString[8] = endDateString[6];
    endDateString[6] = endDateString[5];	
    endDateString[5] = endDateString[4];
    endDateString[4] = '/';
    endDateString[7] = '/';


    //print the plot commands to the uniqueFile.
    if (uniqueFileIndex != 0)
	{
	
	
    	for(auxCount=0;auxCount<uCount;auxCount++)
		{
		fprintf(uniqueFile, "./plot_converted_data.pl qcf -t %s %s -P %s -p %s -s %s -D %s -o %s\n",
			beginDateString, endDateString, plotProjectName, uList.parameterList[auxCount], 
			uList.stationList[auxCount], defaultInputListFile, plotOutputDirectory );
		}
	}

    /*-------------------------------------
    **Close all the files
    **-----------------------------------*/

    for(count=0;count<qcfFileListLength;count++)
    	{
    	fclose(qcfFile[count]);
	}
    fclose(logFile);
    if(uniqueFile != NULL)
	{
    	fclose(uniqueFile);
	}    
    }


