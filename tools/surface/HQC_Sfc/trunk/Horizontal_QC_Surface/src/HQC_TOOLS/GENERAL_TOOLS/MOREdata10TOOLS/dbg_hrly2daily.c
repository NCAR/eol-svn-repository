#ifndef lint
static char *rcsid = "$Id$";
#endif


/*
 * $Log$
 *
 */

#include <stdio.h>
#include <string.h>
#include <floatingpoint.h>

#define BMAX 127.0
#define DMIN 101.6
#define NTWKLEN 11
#define STNLEN 11
#define DAILYSLEN 16
#define CENTURY "19"
#define ERRFILE "bad_hrly.pqcf"
#define DERRFILE "bad_daily.pqcf"
#define MISSVAL -999.99

struct hrly_obs
{
	float precip;
	int code_flag;
	char QC_code;
};

struct hrly_rec
{
	char date_of_obs[9];
	char time_of_obs[9];
	char ntwk_id[NTWKLEN];
	char stn_id[STNLEN];
	float lat;
	float lon;
	int ocr;
	struct hrly_obs obs[24];
};

struct daily_obs
{
	float precip;
	int code_flag;
	char QC_code;
	int obs_hour;
};

struct daily_rec
{
	char date_of_obs[8];
	char ntwk_id[NTWKLEN];
	char stn_id[DAILYSLEN];
	float lat;
	float lon;
	int ocr;
	struct daily_obs obs[31];
};

/* global variable I want available to all functions */
int recno = 0;

/*******************************************************************************
* Program:	hrly2daily
* Description:	Build files containing daily pqcf precip records from files
*		containing hourly pqcf precip records.
* Author:	David Rowland djr@opfs.ucar.edu
* Arguments:	-b double - value to use as the bad maximum. Values above the
*			bad maximum are considered bad.
*		-d double - value to use as the dubious minimum.  Values above
*			the dubious minimum are considered dubious.
*		-i file - the name of the input file containing the hourly
*			pqcf records.
*		-o file - the name of the output file to contain the daily
*			pqcf records.
*		-x 	exit the program upon encountering an error in a record.
*			otherwise, the program will continue and process the
*			remaining records.
* Last Modified:	3/23/94 (initial version)
*   lec 29 apr 94
*      Updated some messages to better reflect processing. Modified check for
*      bad combo of data/qcCode/qcFlag to only send message but continue processing
*      data. This could result in a bad sum (consider -999.99 0 B), but this 
*      is better than just dropping the data out totally with no record.
*
*******************************************************************************/
main(argc, argv)
int argc;
char **argv;
{
	int i;
	int exitonerror = 0;
	int QC_result;
	double bmax = BMAX;
	double dmin = DMIN;
	char *infile;
	char *outfile;
	FILE *in_fptr;
	FILE *out_fptr;
	FILE *err_fptr;
	struct hrly_rec curr_hour;
	struct daily_rec curr_daily;

	/* Process the command line arguments */
	if (argc < 3)
	{
		printf("Usage: %s -i infile -o outfile [-d DMIN] [-b BMAX]\n", argv[0]);
		exit(1);
	}

	infile = NULL;
	outfile = NULL;

	i=1;

	while (i < argc)
	{
		if (argv[i][0] != '-')
		{
			printf("%s: Invalid option: %s\n", argv[0], argv[i]);
			exit(1);
		}

		switch (argv[i][1])
		{
		case 'b' :
			if (strlen(argv[i]) > 2)
				bmax = atof(argv[i] + 2);
			else
			{
				if (++i >= argc)
				{
					printf("Usage: %s -i infile -o outfile [-d DMIN] [-b BMAX]\n", argv[0]);
					exit(1);
				}
				bmax = atof(argv[i]);
			}
			break;
		case 'd' :
			if (strlen(argv[i]) > 2)
				dmin = atof(argv[i] + 2);
			else
			{
				if (++i >= argc)
				{
					printf("Usage: %s -i infile -o outfile [-d DMIN] [-b BMAX]\n", argv[0]);
					exit(1);
				}
				dmin = atof(argv[i]);
			}
			break;
		case 'i' :
			if (strlen(argv[i]) > 2)
				infile = strdup(argv[i] + 2);
			else
			{
				if (++i >= argc)
				{
					printf("Usage: %s -i infile -o outfile [-d DMIN] [-b BMAX]\n", argv[0]);
					exit(1);
				}
				infile = strdup(argv[i]);
			}
			break;
		case 'o' :
			if (strlen(argv[i]) > 2)
				outfile = strdup(argv[i] + 2);
			else
			{
				if (++i >= argc)
				{
					printf("Usage: %s -i infile -o outfile [-d DMIN] [-b BMAX]\n", argv[0]);
					exit(1);
				}
				outfile = strdup(argv[i]);
			}
			break;
		case 'x' :
			exitonerror = 1;
			break;
		default :
			printf("%s: Invalid option %c\n", argv[0], argv[i][1]);
			exit(1);
		}
		i++;
	}

	/* make sure we have both input and output files */
	if (!infile || !outfile)
	{
		printf("Usage: %s -i infile -o outfile [-d DMIN] [-b BMAX]\n", argv[0]);
		exit(1);
	}

	/* open the input file, output file and error file for bad hourly records */
	if (!(in_fptr = fopen(infile, "r")))
	{
		printf("Unable to open for input: %s\n", infile);
		exit(1);
	}
	if (!(out_fptr = fopen(outfile, "w")))
	{
		printf("Unable to open for output: %s\n", outfile);
		exit(1);
	}
	if (!(err_fptr = fopen(ERRFILE, "w")))
	{
		printf("Unable to open for errors: %s\n", ERRFILE);
		exit(1);
	}

	/* read the first record and process */
        /*
         * If only one rec/stn, this s/w may not
         * handle all cases properly. Needs
         * testing. lec
         */
	if (read_hrly(in_fptr, &curr_hour))
	{
		new_daily(&curr_hour, &curr_daily);
                printf ("call sum_hrly\n");
		if (!(sum_hrly(&curr_hour, &curr_daily)))
		{
			fprintf(stderr, "Error summing record number %d.\n", recno);
			fprintf(err_fptr, "Input record number %d\n", recno);
			if ((!write_hrly(err_fptr, &curr_hour)) || exitonerror)
			{
				close_files(in_fptr, out_fptr, err_fptr);
				exit(1);
			}
		}
	}
	else
	{
		fprintf(stderr, "Error reading first record, exiting...\n");
		close_files(in_fptr, out_fptr, err_fptr);
		exit(1);
	}

	/* process the remaining records */
	while (read_hrly(in_fptr, &curr_hour))
	{
                printf ("call comp_recs\n");

		if (!(comp_recs(&curr_hour, &curr_daily)))
		{
                        printf ("main: call QC_daily\n");

			QC_result = QC_daily(&curr_daily, dmin, bmax);
			switch (QC_result)
			{
			case  1 :
				break;
			case -1 :
				fprintf(stderr, "Fatal error Quality Checking output record!\n");
				close_files(in_fptr, out_fptr, err_fptr);
				exit(1);
			default :
				fprintf(stderr, "QC_daily: unknown return code %d!\n", QC_result);
				close_files(in_fptr, out_fptr, err_fptr);
				exit(1);
			}
			if (!write_daily(out_fptr, &curr_daily))
			{
				close_files(in_fptr, out_fptr, err_fptr);
				exit(1);
			}
			new_daily(&curr_hour, &curr_daily);
		}
                printf ("call sum_hrly\n");
		if (!(sum_hrly(&curr_hour, &curr_daily)))
		{
			fprintf(stderr, "Error summing record number %d.\n", recno);
			fprintf(err_fptr, "Input record number %d\n", recno);
			if ((!write_hrly(err_fptr, &curr_hour)) || exitonerror)
			{
				close_files(in_fptr, out_fptr, err_fptr);
				exit(1);
			}
		}
	}

	/* QC and write the last record - lec added QC check*/
        printf ("main: call QC_daily for last record\n");
 
        QC_result = QC_daily(&curr_daily, dmin, bmax);
        switch (QC_result)
          {
          case  1 :
                 break;
          case -1 :
                 fprintf(stderr, "Fatal error Quality Checking output record!\n");
                 close_files(in_fptr, out_fptr, err_fptr);
                 exit(1);
          default :
                 fprintf(stderr, "QC_daily: unknown return code %d!\n", QC_result);
                 close_files(in_fptr, out_fptr, err_fptr);
                 exit(1);
          }

	if (!write_daily(out_fptr, &curr_daily))
	{
		close_files(in_fptr, out_fptr, err_fptr);
		exit(1);
	}

	/* close the files and exit */
	close_files(in_fptr, out_fptr, err_fptr);
	exit(0);
}

/*******************************************************************************
* Function:	read_hrly
* Description:	read an hourly pqcf record from a stream
* Arguments:	fp - pointer to the stream to read from
*		hrly - pointer to the hourly pqcf record structure to put the
*			data in.
* Return:	1 on success, 0 on failure
*******************************************************************************/
int read_hrly(fp, hrly)
FILE *fp;
struct hrly_rec *hrly;
{
	/* uses global variable recno */

	int i, readstat;
printf ("Call read_hrly\n");

	/* NULL out the ntwk and stn strings so we don't get garbage in them! */
	for (i = 0; i < NTWKLEN; i++)
		hrly->ntwk_id[i] = NULL;

	for (i = 0; i < STNLEN; i++)
		hrly->stn_id[i] = NULL;

	recno++;
	readstat = fscanf(fp, "%s %s %10c %10c %f %f %d", hrly->date_of_obs, hrly->time_of_obs, hrly->ntwk_id, hrly->stn_id, &(hrly->lat), &(hrly->lon), &(hrly->ocr));

	if (readstat != 7)
	{
		if (readstat != EOF)
		{
			fprintf(stderr, "Error reading station information in record number %d.\n", recno);
		}
		return 0;
	}

	for (i = 0; i < 24; i++)
	{
		readstat = fscanf(fp, "%f %d %c", &(hrly->obs[i].precip), &(hrly->obs[i].code_flag), &(hrly->obs[i].QC_code));

		if (readstat != 3)
		{
			fprintf(stderr, "Error reading observation number %d in record number %d.\n", i+1, recno);
			return 0;
		}
	}

	return 1;
}

/*******************************************************************************
* Function:	comp_recs
* Description:	compare the station information in an hourly pqcf record to
*		the station information in a daily pqcf record
* Arguments:	hrly - pointer to the hourly pqcf record structure to be
*			compared
*		daily - pointer to the daily pqcf record structure to compare
*			to
* Return:	1 match, 0 no match
*******************************************************************************/
int comp_recs(hrly, daily)
struct hrly_rec *hrly;
struct daily_rec *daily;
{
	char tmp_date[8];

	conv_date(tmp_date, hrly->date_of_obs);

	if ((strcmp(tmp_date, daily->date_of_obs) == 0) && (strcmp(hrly->ntwk_id, daily->ntwk_id) == 0) && (strcmp(hrly->stn_id, daily->stn_id) == 0) && (hrly->lat == daily->lat) && (hrly->lon == daily->lon) && (hrly->ocr == daily->ocr))
		return 1;
	return 0;
}

/*******************************************************************************
* Function:	write_daily
* Description:	write an daily pqcf record to a stream
* Arguments:	fp - pointer to the stream to write to
*		daily - pointer to the daily pqcf record structure to write to
*			the stream
* Return:	1 on success, 0 on failure
*******************************************************************************/
int write_daily(fp, daily)
FILE *fp;
struct daily_rec *daily;
{
	int i, printres;

	printres = fprintf(fp, "%-7s %-10s %-15s %10.5f %11.5f %3d", daily->date_of_obs, daily->ntwk_id, daily->stn_id, daily->lat, daily->lon, daily->ocr);

	if (printres == EOF)
	{
		fprintf(stderr, "Error writing daily record (1).\n");
		return 0;
	}

	for (i = 0; i < 31; i++)
	{
		printres = fprintf(fp, " %7.2f %1d %1c %2d", daily->obs[i].precip, daily->obs[i].code_flag, daily->obs[i].QC_code, daily->obs[i].obs_hour);
		
		if (printres == EOF)
		{
			fprintf(stderr, "Error writing daily record (2).\n");
			return 0;
		}
	}

	fprintf(fp, "\n");
	return 1;
}

/*******************************************************************************
* Function:	write_hrly
* Description:	write an hourly pqcf record to a stream
* Arguments:	fp - pointer to the stream to write to
*		hrly - pointer to the hourly pqcf record structure to write
* Return:	1 on success, 0 on failure
*******************************************************************************/
int write_hrly(fp, hrly)
FILE *fp;
struct hrly_rec *hrly;
{
	int i, printres;

	printres = fprintf(fp, "%-8s %-8s %-10s %-10s %10.5f %11.5f %3d", hrly->date_of_obs, hrly->time_of_obs, hrly->ntwk_id, hrly->stn_id, hrly->lat, hrly->lon, hrly->ocr);

	if (printres == EOF)
	{
		fprintf(stderr, "Error writing hourly record.\n");
		return 0;
	}

	for (i = 0; i < 24; i++)
	{
		printres = fprintf(fp, " %7.2f %1d %1c", hrly->obs[i].precip, hrly->obs[i].code_flag, hrly->obs[i].QC_code);
		
		if (printres == EOF)
		{
			fprintf(stderr, "Error writing hourly record.\n");
			return 0;
		}
	}

	fprintf(fp, "\n");
	return 1;
}

/*******************************************************************************
* Function:	new_daily
* Description:	initialize a daily pqcf record in preparation for use
* Arguments:	hrly - pointer to the hourly pqcf record structure to copy the
*			information from
*		daily - pointer to the daily pqcf record to initialize
* Return:	None
*******************************************************************************/
new_daily(hrly, daily)
struct hrly_rec *hrly;
struct daily_rec *daily;
{
	int i;

	conv_date(daily->date_of_obs, hrly->date_of_obs);

	/* copy parameters */
	strcpy(daily->ntwk_id, hrly->ntwk_id);
	strcpy(daily->stn_id, hrly->stn_id);
	daily->lat = hrly->lat;
	daily->lon = hrly->lon;
	daily->ocr = hrly->ocr;

	/* initialize observations */
	for (i = 0; i < 31; i++)
	{
		daily->obs[i].precip = -999.99;
		daily->obs[i].code_flag = 7;
		daily->obs[i].QC_code = 'M';
		daily->obs[i].obs_hour = 0;
	}
	
	return;
}

/*******************************************************************************
* Function:	QC_hrly
* Description:	quality check a hourly pqcf record
* Arguments:	hrly - pointer to the hourly pqcf record to check
* Return:	the character to use for the QC_code for the day or NULL on
*		an error
*******************************************************************************/
char QC_hrly(hrly)
struct hrly_rec *hrly;
{
	/* uses global variable recno */
	int i, code_value;
	char code;
	float missing_val = MISSVAL;

	code = NULL;
	code_value = 0;

	for (i = 0; i < 24; i++)
	{
		if ((hrly->obs[i].precip == missing_val) && ((hrly->obs[i].code_flag == 7 || hrly->obs[i].code_flag == 3) && (hrly->obs[i].QC_code == 'M' || hrly->obs[i].QC_code == 'N')));
		else if ((hrly->obs[i].precip != missing_val) && ((hrly->obs[i].code_flag != 7 && hrly->obs[i].code_flag != 3) && (hrly->obs[i].QC_code != 'M' && hrly->obs[i].QC_code != 'N')));
		else if (hrly->obs[i].code_flag == 1);
		else
		{
                /* 
                 * Originally, these 'bad' combos were dropped. Now we let them pass on 
                 * even though they could cause bad sums, such as if the combo was:
                 * -999.99 0 B.
                 */
		fprintf(stderr, "Invalid combination of precip, code_flag and QC_code in observation %d of record %d\n", i + 1, recno);
/* was:		return NULL; */

		}

		switch (hrly->obs[i].QC_code)
		{
			case 'N' :
			case 'M' :
				return hrly->obs[i].QC_code;
			case 'X' :
				if (code_value < 6)
				{
					code_value = 6;
					code = 'X';
				}
				break;
			case 'B' :
				if (code_value < 5)
				{
					code_value = 5;
					code = 'B';
				}
				break;
			case 'E' :
				if (code_value < 4)
				{
					code_value = 4;
					code = 'E';
				}
				break;
			case 'D' :
				if (code_value < 3)
				{
					code_value = 3;
					code = 'D';
				}
				break;
			case 'U' :
				if (code_value < 2)
				{
					code_value = 2;
					code = 'U';
				}
				break;
			case 'G' :
				if (code_value < 1)
				{
					code_value = 1;
					code = 'G';
				}
				break;
			default :
				fprintf(stderr, "Error in observation %d of record %d!\n", i + 1, recno);
				fprintf(stderr, "Invalid QC code: %c\n", hrly->obs[i].QC_code);
				return NULL;
		}
	}
	return code;
}

/*******************************************************************************
* Function:	sum_hrly
* Description:	sum an hourly pqcf record
* Arguments:	hrly - pointer to the hourly pqcf to sum
*		daily - pointer to the dail pqcf record to hold the results.
* Return:	1 on success, 0 on failure
*******************************************************************************/
int sum_hrly(hrly, daily)
struct hrly_rec *hrly;
struct daily_rec *daily;
{
	/* uses global variable recno */

	static int static_accum = 0; /* 1 = accum began on some prev day */
	int accum, missing;
	int index, i;
	float precip_sum = 0.0;
	char qc_code;

	/* make sure the accumulator flag for today, the missing flag, and the
   	   precip sum are initialized to 0 upon entering the function */

	accum = 0;
	missing = 0;
	precip_sum = 0.0;

        printf ("(Enter sum_hrly:: accum, static_accum: %d %d\n", accum, static_accum);

	/* determine the correct index into the daily record's observation array */
	index = atoi((hrly->date_of_obs) + 6) - 1;

	qc_code = QC_hrly(hrly);

	/* cycle through the hourly observations */
	for (i = 0; i < 24; i++)
	{
		switch (hrly->obs[i].code_flag)
		{
			case 0:
				if (accum || static_accum)
				{
					fprintf(stderr, "Error in record number %d!\n", recno);
					fprintf(stderr, 
                                           "Found an hourly reading in the middle of an accumulation period.\n");
					fprintf(stderr, "accum = %d, static_accum = %d\n", accum, static_accum);
					return 0;
				}
				else
				/* normal hourly reading, add to the daily sum */
					precip_sum += hrly->obs[i].precip;
				break;
			case 1:
				/* 
				start of or already in an accumulation period.
				ignore the precip reading and set the accumulator
				flag so we know that we are in an accumulation
				period
				*/
				accum = 1;
				break;
			case 2:
				/* end of an accumulation period */
				/*
				 * if we are at the end of an accumulation period
				 * that has spanned several days, reset the accumulator
				 * flags and leave the daily reading marked as missing
                                 *
                                 * Potential prob: If an accum period ends during this
                                 * day, but another starts after this one ends, orig s/w
                                 * will not properly handle this case. The next day doesn't
                                 * realize that the accum period began prev day. Must check
                                 * all hrs in day even if hit the end of an accum.
				 */

				if (accum == 1 && static_accum == 1)
				{
	                                printf ("accum period has spanned several days - leave missing\n");

					accum = 0;
					static_accum = 0;

                                	/*
					 * Check end of day. Set static accum properly.
                                         * All we care is if we can tell if there's another
                                         * accum period that spans into the next day.
					 */
	                                printf ("Check for another accum period in this day.\n");

                                        if (hrly->obs[23].code_flag == 1)
                                           {
	                                   printf ("Found accum period at end - static_accum=1\n");
                                           static_accum = 1;
                                           }
                                       
				  	return 1;
				}

				/*
				if we are in an accumulation period that has not
				spanned more than one day, add the accumulated sum
				to the daily sum and reset the accumulator flag.
				*/
				else if (accum == 1 && static_accum == 0)
				{
                                printf ("accum period has NOT spanned more than one day\n");
					precip_sum += hrly->obs[i].precip;
					accum = 0;
				}
				/*
				if we are in an accumulation period that started
				on a previous day and ended at the first reading
				of the current day, clear the accumulator flags
				and leave the day marked as missing. 
                                
                                Same potential problem as above. Must check rest of
                                day's hours to see if another accum began that spans
                                into next day!
				*/

				else if (accum == 0 && static_accum == 1)
				{
                                        printf ("accum period ended on first reading of curr day\n");
					static_accum = 0;

                                        /*
                                         * Check end of day. Set static accum properly.
                                         */
                                        printf ("Check for accum period at end of day.\n");
                                        if (hrly->obs[23].code_flag == 1)
                                           {
                                           printf ("Found accum period at end - static_accum=1\n");
                                           static_accum = 1;
                                           }

					return 1;
				}

				/* accum == 0 && static_accum == 0 */
				else
				{
					fprintf(stderr, "Error in record number %d!\n", recno);
					fprintf(stderr, "End of an accumulation period that never started!\n");
					return 0;
				}
				break;
			case 3:
			case 7:
				/* missing data, leave as default */
				/* (daily record values are initialized to missing) */
				missing = 1;
				break;
			default :
				fprintf(stderr, 
                                   "Unknown code flag %d for observation number %d in record number %d\n", 
                                    hrly->obs[i].code_flag, i, recno);
				return 0;
		}
	}

        printf ("accum, static_accum: %d %d\n", accum, static_accum);

	if (accum)
		static_accum = accum;
	else if ((qc_code == NULL) || missing);
	else
	{
		daily->obs[index].precip = precip_sum;
		daily->obs[index].code_flag = 0;
		daily->obs[index].QC_code = qc_code;
		daily->obs[index].obs_hour = atoi(hrly->time_of_obs);
	}

        printf ("EXIT sum_hrly::accum, static_accum: %d %d\n", accum, static_accum);
	return 1;
}

/*******************************************************************************
* Function:	QC_daily
* Description:	quality check a daily pqcf record
* Arguments:	daily - pointer to the daily pqcf record to check
*		dub - the value to use as dubious.  any values above dub
*			are considered dubious.
*		bad - the value to use as bad.  any values above bad
*			are considered bad.
* Return:	1 if the record is good, -1 if there
*		is a problem with the file questionable records are logged to.
* 29 Apr 94 lec
*   Added s/w to print out all bad/dubious values. Also added checks to
*   see if QC_code should be reset when precip val exceeds bmax/dmin.
*******************************************************************************/
int QC_daily(daily, dub, bad)
struct daily_rec *daily;
double dub;
double bad;
{
	FILE *daily_err_fptr;
	int i = 0;
        int return_val = 1;


	if (!(daily_err_fptr = fopen(DERRFILE, "a")))
	{
		fprintf(stderr, "Unable to open for errors: %s\n", DERRFILE);
		return -1;
	}
        printf ("enter QC_daily\n");

	for (i = 0; i < 31; i++)
	{
        printf ("QC_daily: %d\n", i);
        printf ("precip: %f QC_code: %c\n", daily->obs[i].precip, daily->obs[i].QC_code);

		if (daily->obs[i].QC_code != 'M')
		{
			if (daily->obs[i].precip > bad || 
			    daily->obs[i].QC_code == 'B' )
			{
       	                        printf ("precip > bad OR QC_code == B\n");

				fprintf(stderr, "Val exceeds BMAX QC limits. Rec written to bad_daily.pqcf\n");

				/*
				 * If precip val > bad, then the QC_code
                                 * should be 'at least' bad. Follow this
                                 * precedence: M N X B E D U G (hi to low)
                                 */
                                if ((daily->obs[i].QC_code == 'G') ||
                                    (daily->obs[i].QC_code == 'U') || 
                                    (daily->obs[i].QC_code == 'D') || 
                                    (daily->obs[i].QC_code == 'E')   )
                                   {
                                   fprintf (stderr, "Resetting flag %c to B,since val = %f.\n",
                                      daily->obs[i].QC_code, daily->obs[i].precip);

                                   daily->obs[i].QC_code = 'B';
                                   }
                                printf ("fprintf daily_err_fptr -Obs exceeds bad limit\n");

				fprintf(daily_err_fptr, "Observation %d exceeds bad limit.\n", i + 1);
                                printf ("call write_daily\n");
				if (!write_daily(daily_err_fptr, daily))
				{
                                printf ("fclose daily_err_fptr and return -1\n");
					fclose(daily_err_fptr);
					return -1;
				}
			}
			else if (daily->obs[i].precip > dub ||
                                 daily->obs[i].QC_code == 'D')
			{
				fprintf(stderr, "Val exceeds DMIN QC limits. Rec written to bad_daily.pqcf\n");
                                /* 
                                 * If precip val > Dmin, then the QC_code 
                                 * should be 'at least' dub. Follow this
                                 * precedence: M N X B E D U G (hi to low) 
                                 */ 
                                if ((daily->obs[i].QC_code == 'G')|| 
                                    (daily->obs[i].QC_code == 'U') )
                                   { 
                                   fprintf (stderr,"Resetting flag %c to D,since val = %f.\n",
                                      daily->obs[i].QC_code, daily->obs[i].precip);
 
                                   daily->obs[i].QC_code = 'D'; 
                                   }

				fprintf(daily_err_fptr, "Observation %d exceeds dubious limit.\n", i + 1);
				if (!write_daily(daily_err_fptr, daily))
				{
					fclose(daily_err_fptr);
					return -1;
				}
			}
		}
	}
	fclose(daily_err_fptr);
	return return_val;
}

/*******************************************************************************
* Function:	conv_date
* Description:	convert a date from yy/mm/dd to yyyy/mm
* Arguments:	out - pointer to the string to hold the converted date
*		in - pointer to the string that holds the date to be converted
* Return:	None
*******************************************************************************/
conv_date(out, in)
char *out;
char *in;
{
	int i, j;

	/* convert date */
	strcpy(out, CENTURY);
	for (i = 0, j = 2; j < 7; i++, j++)
		out[j] = in[i];
	out[j] = NULL;

	return;
}

/*******************************************************************************
* Function:	close_files
* Description:	close the input, output and error files
* Arguments:	in_file - pointer to the input stream to close
*		out_file - pointer to the input stream to close
*		err_file - pointer to the input stream to close
* Return:	None
*******************************************************************************/
close_files(in_file, out_file, err_file)
FILE *in_file;
FILE *out_file;
FILE *err_file;
{
	fclose(in_file);
	fclose(out_file);
	fclose(err_file);
	return;
}
