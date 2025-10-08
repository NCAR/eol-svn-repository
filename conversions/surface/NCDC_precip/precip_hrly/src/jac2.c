
/*
 * $Log$
 *
 */

#ifndef lint
static char *rcsid = "$Id: jac2.c,v 1.1 1993/02/16 22:42:56 john Exp $";
#endif

/*
 * jac2 - John J. Allison's accumulation program 2.
 * Written 2 Feb 1992 by jja.
 *
 * This program is used to process the precipitation
 * data which contains accumulated data. Accumulated
 * data is identified by a '1' qualifier in the data.
 * Precip values are 0.00 when the '1' qualifier is
 * present. At the end of the accumulation period,
 * the total accumulated precip value is identified
 * by a `2` qualifier.
 *
 * This program is supposed to turn 1 qualifiers 
 * into missing if:
 *
 *  + the accumulation range begins before beginning
 *    of input data (was: Feb 1; is: Mar 16)  OR
 *  + the accumulation range ends after the end
 *    of the input data (was: Mar 16; is: Apr 30/May 1).
 *
 * Usage:
 *    jac2 [x] < in.file > out.file
 *
 * if x is present, then numbkts=96 (15-minute), [x can
 * be anything].  If there are no arguments, then 
 * numbukts=24 (hourly). File in.file is the file to
 * be operated on. File out.file will be the final
 * output file.
 *
 * INPUT: jac2 executable expects a file named jac2.inp
 *    to be located in the execution area. The jac2.inp
 *    file contains the following:
 *  1 - number of month when data collection began.
 *           (Jan = 1; Mar = 3; Dec = 12)
 *  2 - day of first month that data collection began.(16)
 *  3 - number of days collected in first month. (16)
 *  4 - total number of days collected. (e.g., 45 or 46).
 *
 *  Place all four inputs on the same line!!!!!
 *    (2 1 29 45) for 1Feb 92 to 15Mar.
 *    (3 16 16 46) for 16mar to 30apr.
 *
 * 11 Oct 93 lec
 *   Some cleanup and added documentation; Modified to 
 *   run on Mar/Apr time period. Tried to make first
 *   cut at making s/w more generic. S/w now expects
 *   input file named jac2.inp to exist in execution
 *   area. 
 *
 * NOTE: This s/w pretty much assumes a data collection
 *    period of 2 months. Mods may need to be made if
 *    data collection is for a longer period. In fact,
 *    this code has only been tested on a 45 and a 46 
 *    data collection period.
 *
 * Warning: In the do_data() routine, one of the
 *   first checks has been commented out. When it
 *   was left in, it appears that some good data
 *   was being dropped! That good data was never
 *   printed to the output file.
 *
 * Modfied -- 12 Jun 95 mhc
 *	* Increased station id name to 15 characters and changed array 
 *	  indices accordingly.
 *
 */

#include <stdio.h>
#include <string.h>
#include <errno.h>

#define YEAR  "95"

/* totdays[i] is the total number of days in the year *prior* to month i */
/* XXX leap year XXX */
/* int totdays[13] = { 366,0,31,60,91,121,152,182,213,244,274,305,335 }; 
/* leap */
int totdays[13] = { 365,0,31,59,90,120,151,181,212,243,273,304,334 }; /* 
nonleap */

/* julian - convert month m and day d to the Julian date */
#define julian(m,d)     totdays[m]+d

typedef struct {
  float val;
  int qual;
  char qcflag;
  } raytype;

int maxray=0,numbukts=24;

int FirstMonth,       
    FirstDay,
/*********    DaysCollFirstMon, *********/
    TotDaysColl;

int TOTALBUCKETS=0;

FILE *ifp,*ofp;

/*-----------------------------------
 * Main calling program.
 *----------------------------------*/
main(argc,argv)
int  argc;
char *argv[];

{
int  i, items	;
char filnam[256],*ptr;
FILE *data_stream1;


#if 0
/* 
 * This section handles command line arguments.
 */
if (argc==1) {
  fprintf(stderr,"Usage: jac2 files ...\n");
  exit(1);
  }
for (i=1; i<argc; i++) {
  if ((ifp=fopen(argv[i],"r"))==NULL) {
    fprintf(stderr,"jac2: cannot open %s\n",argv[i]);
    exit(1);
    }
  strcpy(filnam,argv[i]);
  ptr = strrchr(filnam,'.');
  if (ptr) *ptr = '\0';
  strcat(filnam,".jac");
  if ((ofp=fopen(filnam,"w")) == NULL) {
    fprintf(stderr,"jac2: error opening output file %s\n",filnam);
    exit(1);
    }
  jac();
  }
#endif

/*
 * If two command line args, assume 15 min data.
 */
if (argc == 2)
  {
  /* fprintf (stderr,"Assume 15 min data!\n"); */
  numbukts=96;
  }

ifp=stdin; ofp=stdout;


/*
 * Read data from jac2.inp file.
 */
if (( data_stream1 = fopen("jac2.inp", "r")) == NULL)
   perror ("Error: Can't open jac2.inp. Required to run jac2!");

#if 0
items = fscanf (data_stream1, "%d %d %d %d", &FirstMonth, &FirstDay,
                &DaysCollFirstMon, &TotDaysColl);
if (items !=4)
   if ( !feof(data_stream1))
      fprintf (stderr, "Error: Read %d of four items from data_stream1!\n",
               items);
#endif


items = fscanf (data_stream1, "%d %d %d", &FirstMonth, &FirstDay,
                &TotDaysColl);
if (items !=3)
   if ( !feof(data_stream1))
      fprintf (stderr, "Error: Read %d of three items from data_stream1!\n",
               items);


if (fclose (data_stream1) == EOF)
   perror ("Can't close jac2.inp file");


/*
 * Call jac program to handle 
 * accumulated precip values!!
 */
jac();

} /* end main */

/*--------------------------------------
 * jac() - John's accumulation program.
 *--------------------------------------*/
jac()

{
char    buf[1230],netstr[11],stnstr[16],tmpnet[11],tmpstn[16],*ptr;
int     tmpjul,FirstJul,tmpmon,tmpday,tmptim,tmpcli,cli=-1,i;
float   tmplat,tmplon,lat=-361.0,lon=-361.0;
raytype *array;

netstr[0]=stnstr[0]='\0';

#if 0

/* This section reads a line, computes
 * number of buckets (numbukts) and then
 * rewinds the file. Currently, if main
 * detechs 2 args, it assumes 15 min precip
 * data. That is, 96 buckets in a 24hr
 * period.
 */
fgets(buf,1222,ifp);
rewind(ifp);
if (strlen(buf) > 1024) numbukts=96;

#endif

maxray=TotDaysColl*numbukts;        /* Hrly: 45*24=1080, 46*24=1104 */
TOTALBUCKETS=TotDaysColl*numbukts; 

if ((array=(raytype *)malloc(sizeof(raytype)*maxray))==NULL) {
  fprintf(stderr,"cannot malloc array\n");
  exit(2);
  }

/* initialize array to missing */
for (i=0;i<TOTALBUCKETS;i++) {
  array[i].val = -999.99;
  array[i].qual = 7;
  array[i].qcflag = 'M';
  }

while (fgets(buf,1227,ifp)) { /* get one data record, while not eof -355 hr/char/line*/

  if (sscanf(buf,"%*d/%d/%d",&tmpmon,&tmpday)<2) { /* skip yr w/ suppression flag*/
    fprintf(stderr,"jac(): sscanf buf tmpmon tmpday error\n");
    exit(1);
    }


  strncpy(tmpnet,buf+18,10); 
  strncpy(tmpstn,buf+29,15);

  tmpnet[10]='\0';
  tmpstn[15]='\0';

  if (sscanf(buf+42,"%f %f %d",&tmplat,&tmplon,&tmpcli)<3) {
    fprintf(stderr,"jac(): sscanf buf tmplatloncli error\n");
    exit(1);
    }


  /* if a different station from our current one, */
  /*   then print out our current one             */

  if ((lat!=tmplat)||(lon!=tmplon)||(cli!=tmpcli)) {
    do_array(array,lat,lon,cli,netstr,stnstr);

    /* set values for current station */
    lat=tmplat; lon=tmplon; cli=tmpcli;
    strcpy(netstr,tmpnet); strcpy(stnstr,tmpstn);

    /* initialize array to missing */
    for (i=0;i<TOTALBUCKETS;i++) {
      array[i].val = -999.99;
      array[i].qual = 7;
      array[i].qcflag = 'M';
      }
    } /* end if lat */


  /* calculate time of first bucket, i.e. what day is it? (0-based) */

tmpjul = (julian(tmpmon,tmpday));
FirstJul = (julian(FirstMonth,FirstDay));

tmptim = (tmpjul - FirstJul);
tmptim *= numbukts;


#if 0
  if (tmpmon == FirstMonth) {            /* was 2 ; 3 for mar*/
    tmptim = (tmpday-FirstDay)*numbukts; /* was -1 for feb01->Mar 15; -16 for Mar 16->Apr30 10*/
    }
  else tmptim = ((tmpday+DaysCollFirstMon-1)*numbukts);  /* was +28days gone by; Note 1992=leap yr*/
                                                         /*was +15 for Mar16 to Apr30 */
#endif


/*-------------------------------------------------------------------*/
/* put values from buf into array                                    */
/* while part: ptr stuff checks that we don't run off the end of buf */
/*             i< checks that we do a day's worth                    */
/*             all three checks should activate at the same time     */
/*-------------------------------------------------------------------*/
  for (ptr=buf+71,i=tmptim;            /* 71 = loc where data begins */
       ((*ptr)&&(*ptr != '\n')&&(i<tmptim+numbukts));
       ptr+=12,i++) {
    
    if (sscanf(ptr,"%f %d %c",
	       &array[i].val,&array[i].qual,&array[i].qcflag) < 3) {
      fprintf(stderr,"jac2: sscanf error in jac()\n");
      exit(1);
      }
    } /* end for ptr */

  } /* end while fgets */


do_array(array,lat,lon,cli,netstr,stnstr);

} /* end jac */

/*---------------------------------------------------------------
 * do_array()
 *  This routine only prints to output file, those records which
 *  have at least one 'good' piece of data.  If a complete period is
 *  missing, deleted, etc. then it is NOT written to output file.
 *
 *--------------------------------------------------------------*/
do_array(a,lat,lon,cli,netstr,stnstr)
raytype a[];
float   lat,lon;
int     cli;
char    *netstr,*stnstr;

{
int i,j,isdata=0;

if (lat == -361.0) return;


#if 0 
/*
 * This section of code has been commented out
 * since it appears to prevent 'good' data from
 * being written to output file.
 */

/*
 * First bucket missing for ncdc ebufr data?
 */
if (a[0].qual !=7) {
  fprintf(stderr,"First bucket is NOT missing for %10.5f %11.5f - don't print record!!\n",lat,lon);
  return;
  }
#endif 


/* 
 * Search for first non-accum 
 */
for (i=0; (a[i].qual==1) && (i<maxray); i++) {
  a[i].val = -999.99;
  a[i].qual = 7;
  a[i].qcflag = 'M';
  }


/*
 * Entire period is accum!! don't print to output file.
 */
if (i==maxray) return;


/* 
 * Reset end of accum, since beginning is unknown 
 */
if (a[i].qual==2) {
  a[i].val = -999.99;
  a[i].qual = 7;
  a[i].qcflag = 'M';
  }

/*
 * look for first non-missing precip value.
 */
for (; (((a[i].qual==3)||(a[i].qual==7))&&(i<maxray)); i++);

/*
 * Entire period is accumulation!! Don't print to output file.
 */
if (i==maxray) return;

/* 
 * Remember this spot ---it's a non-missing (0,1,2)
 */
j=i;

/*
 * Search back from end of data for first non-accum.
 */
for (i=maxray-1; (a[i].qual==1)&&(i>j); i--) 
   {
   a[i].val = -999.99;
   a[i].qual = 7;
   a[i].qcflag = 'M';
   }

/*
 * If all values are accum in [j,max-1] then entire period 
 * is bad since we know that [0,j-1] are all missing.
 * If entire period is accum then don't print to output file.
 */
if ((i==j) && (a[j].qual==1)) return;


/*
 * If make it to this point, the values
 * in [j,i] are keepers (i.e., there's
 * at least one 'good' value in the period.
 * So, print the data.
 */
isdata=0;

/*
 * Array contains total several (45 or 46) day period of 
 * data for one station. Divide the data into days and
 * pass that data to the print_day() fn.
 */
for (i=0; i<maxray+1; i++) 
  {
  /*
   * If data is not missing, then mark that
   * we have data 
   */
  if ((a[i].qual != 3) && (a[i].qual != 7))
     isdata = 1;

  /*
   * If this is the last value of a day then
   * print today.
   */
  if ((i+1)%numbukts == 0) {

    if (isdata) {
       print_day(a,lat,lon,cli,i-numbukts+1,netstr,stnstr);
       isdata = 0;

       } /* end if isdata */
    } /* end if i+1%num - days worth of data */
  } /* end for */

} /* end do_array */

/*-------------------------------------
 * print_day() - prints one days worth
 *    of data.
 *------------------------------------*/
print_day(a,lat,lon,cli,i,netstr,stnstr)
raytype a[];
float   lat,lon;
int     cli,i;
char    *netstr,*stnstr;

{
int j,k,tmpmon,tmpday;


/* calculate the day based on index i */
j=i/numbukts;

for( k=1; k<13; k++ )
  if( totdays[k] >= (julian(FirstMonth,FirstDay) + j) )  break;

tmpmon = k-1;
tmpday = ( (julian(FirstMonth,FirstDay) + j) - julian(tmpmon,0) );


#if 0
if (j < DaysCollFirstMon) { /* was 29 for feb/mar period; 16 for mar/apr period */
  tmpmon = FirstMonth;      /* was 2 for feb.            */
  tmpday = j+FirstDay;      /* was +1 for feb/mar period; was +16 for mar/apr period */
  }
else {
  tmpmon = FirstMonth+1;            /* was 3 for mar; and 4 for apr */
  tmpday = j - (DaysCollFirstMon-1);/* was 28 for feb; Note 1992 was leap yr. was 15 for mar/apr */
  }
#endif



/* print record header stuff */
fprintf(ofp,"%s/%02d/%02d 00:00:00 %-10s %-15s %10.5f %11.5f %3d",
	YEAR,tmpmon,tmpday,netstr,stnstr,lat,lon,cli);


/* print a day's worth beginning at i */
for (j=i; j<i+numbukts; j++)
  fprintf(ofp," %7.2f %1d %c",a[j].val,a[j].qual,a[j].qcflag);

putc('\n',ofp);

} /* end print_day */
