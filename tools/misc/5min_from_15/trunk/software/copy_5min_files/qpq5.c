#ifndef lint
static char *rcsid = "$Id: qpq5.c,v 1.4 1995/06/14 17:45:29 john Exp $";
#endif

/*
 * qpq5.c : 5-minute QCF to 15-minute PQCF (Precip QCF) conversion program
 *
 * Actually, qpq converts a temporary file format to PQCF.
 *
 * This is a revision of qpq5_sim.c, which is
 *  from the 4th paper version of the algorithms.
 *
 * Attempts to not write all-missing lines, or the first newline.
 *
 * ******** this is the current working version
 * Assumes all data is in the same year.
 * Search for XXX to find the spot to change for non/leap years.
 *
 *
 * Discussion:
 *  5-minute QCF results in 288 values per day (12 per hour).
 *  15-minute PQCF results in 96 values per day (4 per hour).
 *  I will write QCF-n to mean the nth time interval in QCF and
 *  PQCF-m for the mth time interval (or slot) in PQCF. I.e., QCF-0
 *  means time 00:00, QCF-1 means 00:05, QCF-287 means 23:55. Also,
 *  PQCF-0 means time 00:00, PQCF-1 means 00:15, PQCF-95 means 23:45.
 *  Note that for QCF a value x at time n means that x units of precip
 *  fell in the past 5 minutes, i.e. the mathematical interval (n-1,n].
 *  Similarly for PQCF, value x at time m means x units in the last
 *  15 minutes, i.e. (m-1,m]. So QCF-0 on day 202 measures the precip
 *  from 23:55:01 on day 201 to 00:00:00 on day 202, i.e. mathematically
 *  the interval is (23:55:00 day 201, 00:00:00 day 202].
 *
 *  So, what we have to do is sum up the QCF values that were measured
 *  during a single PQCF time interval. What this means is that PQCF-m
 *  consists of QCF-(3m-2), QCF-(3m-1), and QCF-(3m). For example,
 *  PQCF-1 is time 00:15. We must sum up the QCF precip values from
 *  QCF-1, QCF-2, and QCF-3, or 00:05, 00:10, and 00:15.
 *
 *  In this program, I expand the notion of QCF-n and PQCF-m to include
 *  the days. Since there are NUMDAYS days in the project,
 *  we have QCF-n for NUMDAYS*288 time intervals. In the main loop below,
 *  i represents the PQCF-m number that is currently being computed, and
 *  j represents the QCF-n number that is currently in memory (in buf).
 *  However, the PQCF-m numbers (i) are expressed in terms of the QCF-n
 *  units. So, i runs from 0..NUMDAYS*288-1 also. Thus, i is always incremented
 *  by 3 while j is incremented by 1. So, it is more correct for us
 *  to say that i represents the QCF-n number that has the same real time
 *  as the PQCF-m number that is currently being computed.
 *
 * Written by:
 *  John J. Allison / john@lightning.ofps.ucar.edu
 *  NCAR/OFPS
 *  12 Oct 1992
 *
 * Modification History:
 *  02 Nov 1992 by John J. Allison
 *     added network and station strings
 *  08 Jun 1995 by John J. Allison
 *     mods for GCIP/GIST: 15 char station name, new QC flags
*/

#include <stdio.h>
#include <string.h>


/* totdays[i] is the total number of days in the year *prior* to month i */
/* XXX leap year XXX */
/* int totdays[13] = { 366,0,31,60,91,121,152,182,213,244,274,305,335 };  leap */
int totdays[13] =    { 365,0,31,59,90,120,151,181,212,243,273,304,334 }; /* nonleap */

/* julian - convert month m and day d to the Julian date */
#define julian(m,d)	totdays[m]+d

/* BASEDAY - the Julian date of the first day */
#define BASEDAY 91                                                     /* start on April 1st */
                                                                       /*   (non-leap year)  */
/* NUMDAYS - the number of days in the time period */
#define NUMDAYS 91                                                     /* run through June 30th */

/* YEAR - all data is assumed to be in the same year (without century) */
#define YEAR 95


char jqcworst();
FILE *ifp=stdin,*ofp=stdout,*efp=stderr;


main()
{
int skip,i,j,prevtim,datact;
float prevlat,prevlon,lat,lon,thisvalue;
int prevocc,occ,mon,day,hr,min,yr;
float total;
char buf[128],*rval;
char qcflag,thisflag;
char prevstn[16],stnstr[16],prevnet[11],netstr[11];

skip=0;
prevlat=-361.0;
prevlon=0.0;
datact=0;
netstr[10]=stnstr[15]='\0';

while (!feof(ifp)) {
 prevtim=0;
 total=0.0;
 qcflag='G';
 for (i=0; i<NUMDAYS*288; ) { /* i is the current (nominal) time we're looking at */
  if (skip)
    skip=0;
  else rval=fgets(buf,128,ifp);	/* get next record */
  if (rval) {	/* got record okay */
    if (sscanf(buf,"%d/%d/%d %d:%d:%*d %*s %*s %f %f %d %f %c %10c %15c",
	 &yr,&mon,&day,&hr,&min,&lat,&lon,&occ,&thisvalue,&thisflag,
	 netstr,stnstr) < 12) {
	 fprintf(efp,"sscanf error\n");
	 exit(1);
	 }
    if (yr!=YEAR) {
      fprintf(efp,"yr=%d YEAR=%d\n",YEAR);
      exit(1);
      }
    } /* end if rval */
  /* else (no record gotten---EOF or error) we reset lon so */
  /*  that j=NUMDAYS*288 and we will finish the current station */
  /*  note: didn't reset lat here because of possible confusions w/prevlat */
  else lon = -361.0;	/* will this cause problems w/next stmt? hope not */
  if (prevlat == -361.0) {	/* is true on first time through loop */
    prevlat = lat;
    prevlon = lon;
    prevocc = occ;
    strcpy(prevnet,netstr);
    strcpy(prevstn,stnstr);
    }

  /* value of i has cycled around to 0 again */
/* (this section is bogus, doesn't work for beginning)
   (also, no real reason to worry about this, esp. w/no missing prints)
  else if (i==0) {
   fprintf(efp,
    "station %10.5f %11.5f %d\n has more values than I know what to do with\n",
    lat,lon,occ);
   fprintf(efp," platlon %10.5f %11.5f i=%d j=%d\n",prevlat,prevlon,i,j);
   exit(1);
   }
*/

  if ((lat != prevlat) || (lon != prevlon) || (occ != prevocc)) {
    j = NUMDAYS*288;
    skip = 1;
/*    j = NUMDAYS*288-3; /* (old version) */
  /* stations have changed
   * since they're sorted, then this is the last value for this station
   * so set j to max and write missing for rest of this station's time slots
   */
    }
  /* else j gets the date out of the buffer buf (via mon,day) */
  else j = (julian(mon,day)-BASEDAY)*288 + hr*12 + min/5;
  if (j < i-3) {
    fprintf(efp,"input not sorted\n");
    exit(1);
    }
  if (j > i)
    for (; i<j; i+=3) {	/* fill out the times between i and j */
/* remember, i is the time we expect to see right now, i.e. it's the */
/*   next (nominal) time in the PQCF output */
      if (prevtim == i)	/* normal case */
       pqprint(i,total,0,qcflag,datact,prevlat,prevlon,prevocc,prevnet,prevstn);
      else if (prevtim < i) /* some QCF times missing for current PQCF time */
	pqprint(i,-999.99,7,'M',datact,prevlat,prevlon,prevocc,prevnet,prevstn);
      else {	/* should never get here */
	fprintf(efp,"previous value seen has time > current time\n");
	exit(1);
	}
      /* reset values for next PQCF time slot */
      total = 0.0;
      qcflag = 'G';
      datact=0;
      } /* end for i<j */
  /* now the current time matches the expected PQCF time slot */
  /*   so add up precip */
  qcflag = jqcworst(qcflag,thisflag);	/* use the worst flag */
  if ((qcflag != 'M') && (qcflag != 'N') && (qcflag != 'C') && (qcflag != 'I'))
    total += thisvalue;	/* don't add -999.99 */
  datact++;
  prevtim = j;
  } /* end for i<NUMDAYS*288 */
 if (skip) prevlat = -361.0; /* reset station */
 } /* end while !feof */
} /* end main */



pqprint(time,total,qual,qcflag,ct,lat,lon,occ,netstr,stnstr)
int time;
float total;
int qual;
char qcflag;
int ct;
float lat,lon;
int occ;
char *netstr,*stnstr;
/*
 * pqprint - print the PQCF values
 *
 * doesn't actually output data on each call.
 * Instead pqprint stores the data in static arrays. Then, when a
 * new day is being printed, pqprint outputs the data only if it is
 * not all missing.
 */
{
int jday,day,mon,ndx,j;
static int smon=-1,sday,socc,squal[96],smiss=1;
static float slat=-361.0,slon=-361.0,stot[96];
static char sqcf[96];
static char sstn[16],snet[11];

jday = time / 288;	/* julian date; 288 QCF times in a day (12*24) */
monday(jday+BASEDAY,&mon,&day);
if (mon <= 0) {
  fprintf(efp,"mon <= 0\n");
  exit(1);
  }
if ((ndx = time % 288) == 0) { /* if it's the start of a new day */
  if (smon != -1)
/*   if ((!smiss) && (strncasecmp(snet,"ncdc",4) != 0)) {
   /* if data is not all missing and not from NCDC then print it */
   if (!smiss) {
     fprintf(ofp,"%02d/%02d/%02d 00:00:00 %-10.10s %-15.15s %10.5f %11.5f %3d",
	     YEAR,smon,sday,snet,sstn,slat,slon,socc);
     for (j=0; j<96; j++)
       fprintf(ofp," %7.2f %d %c",stot[j],squal[j],sqcf[j]);
     putc('\n',ofp);
     } /* end if !smiss */
  /* it's a new day, so reset values */
  smon=mon; sday=day; slat=lat; slon=lon; socc=occ; smiss=1;
  strcpy(snet,netstr);
  strcpy(sstn,stnstr);
  } /* end if time */
if (ndx % 3 != 0) { /* bad */
  fprintf(efp,"ndx mod 3 not 0\n");
  exit(1);
  }
else ndx = ndx / 3;
  /* ndx becomes the number of the PQCF time slot in this day */
  /*  i.e. the number of the 15-minute interval */

/* if data is missing then be sure to use missing values */
if ((qcflag == 'M') || (ct < 3)) {
   stot[ndx] = -999.99;
   squal[ndx] = 7;
   sqcf[ndx] = 'M';
   }
else if (qcflag == 'N') {
   stot[ndx] = -999.99;
   squal[ndx] = 7;
   sqcf[ndx] = 'N';
   }
else if (qcflag == 'C') {
   stot[ndx] = -999.99;
   squal[ndx] = 7;
   sqcf[ndx] = 'C';
   }
else if (qcflag == 'I') {
   stot[ndx] = -999.99;
   squal[ndx] = 7;
   sqcf[ndx] = 'I';
   }
else {	/* data present */
   if (total < 0.0)	/* could have -999.99 X which is not handled above */
     stot[ndx] = -999.99;
   else stot[ndx] = total;
   squal[ndx] = qual;
   sqcf[ndx] = qcflag;
   smiss = 0;	/* tells us that we actually have some data */
   }
} /* end pqprint */



monday(j,m,d)
int j,*m,*d;
/*
 * monday - return the month m and day d for a Julian date j
 */
{
int i;

for (i=12; ((i>0) && (j<=totdays[i])); i--) ;
*m = i;
*d = j - totdays[i];	/* totdays is a global array used w/julian macro */
} /* end monday */



char jqcworst(ja,jb)
char ja,jb;
/*
 * jqcworst - returns the worst flag of a and b
 *
 * Increasing value of flags: M N X B D U E G
 * New Increasing value of flags: M N C I X B E D U G T
 */
{
char rchr=ja;

if (rchr == 'G')
  rchr = jb;
else if ((jb == 'M') || (rchr == 'M'))
  rchr = 'M';
else if ((jb == 'N') || (rchr == 'N'))
  rchr = 'N';
else if ((jb == 'C') || (rchr == 'C'))
  rchr = 'C';
else if ((jb == 'I') || (rchr == 'I'))
  rchr = 'I';
else if ((jb == 'X') || (rchr == 'X'))
  rchr = 'X';
else if ((jb == 'B') || (rchr == 'B'))
  rchr = 'B';
else if ((jb == 'E') || (rchr == 'E'))
  rchr = 'E';
else if ((jb == 'D') || (rchr == 'D'))
  rchr = 'D';
else if ((jb == 'U') || (rchr == 'U'))
  rchr = 'U';
else if ((jb == 'T') || (rchr == 'T'))
  rchr = 'T';
else return('\0');
return(rchr);
} /* end jqcworst */
