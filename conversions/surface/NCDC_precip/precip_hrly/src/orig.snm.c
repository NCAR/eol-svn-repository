
/*
 * $Log$
 *
 */

#ifndef lint
static char *rcsid = "$Id: snm.c,v 1.1 1993/02/16 22:42:56 john Exp $";
#endif

/*
 * "snm.c" - Station Name insertion for PQCF files
 *
 * Synopsis:
 *  snm [-f stnlist] file [[-f stnlist] file] ...
 * where
 *  stnlist is the file to be used instead of "fest_sites_sorted"
 *
 * Reads a PQCF file, outputs a PQCF file, with the following changes:
 *  * changes network name "NCDC" to "COOP"
 *  * inserts a station name derived from the "fest_sites_sorted" file
 *    (was: "/home/john/work/coop.lst")
 *    (the first 10 chars of the station location)
 *  * moves the values up one bin, e.g. bin 0 becomes bin 1
 *    this is because I originally thought that bin 0 was 00:00 and
 *    bin 95 was 23:45, now I'm told that bin 0 is 00:15 and bin 95 is 24:00
 *  * for 15-minute files, changes qualifier code to 1/2 for a certain
 *    (hardcoded) list of 8 stations: values on the hour (xx:00) get 2
 *    and everything else gets 1 (unless missing interferes)
 *
 * ***** ONLY USE THIS PROGRAM ON FILES THAT ARE FROM NCDC EBUFR PRECIP FILES
 *       OTHERWISE THE TIME SHIFT WILL MESS THINGS UP
 *
 * Works on 15-minute or hourly formats automatically.
 *
 * Output:
 *  file.snam : the file with changes
 *  stdout : counts
 *  stderr : errors
 *
 * Written by:
 *  John J. Allison / john@storm.ofps.ucar.edu
 *  29 Jan 1992, modified from "pck.c" (26 Jan 1993) which is
 *  modified from "sck.c" (25 Jan 1993) which is modified from
 *  "pqc.c" (21 Sep 1992 and 04 Nov 1992)
 *
 * Modified by:
 *   15 Oct 93 lec
 *      Default stn info no read from fest_sites_sorted.
 *      Could not locate orig coop.lst that looked correct.
 *
 * Modified by:
 *   12 Jun 95 mhc
 *	* Modified to use standard station list from NCDC also
 * 	  added code to convert degree,minute,direction data to 
 *	  decimal degree data, since the NCDC station list uses 
 * 	  the degree,minute,direction format.
 *
 *      * Also changed the values of several array indices to account 
 *	  for the increased length of the station id (+5 characters)
 *
 *      * Now uses network name "Rec rainga" instead of "COOP"
 *
 *      * NOTE: the procedures "findunk" and "findacum" are now obsolete
 *                and nothing is done with the unknow array, this means
 * 	 	  that data that cannot be matched in the station list
 *                is now dropped instead of included without id.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define USAGE "Usage: snm [-f stnlist] file [[-f stnlist] file] ..."
#define inrange(x,y,r) ((x<=y+r)&&(x>=y-r))
#define MAXUNK 15
#define TOLERANCE 1

int ctrec,ctnot,ctmem,ctfif,cthly;
FILE *ifp,*ofp;

typedef struct {
  long lat,lon;
  float val;
  int qual,dat;
  char qcflg;
  char stnam[20];
  } stntype;
stntype *stnray=NULL;
stntype unkray[MAXUNK];
long len=0,unklen=0;

float ConvertToDecimalDegrees(char*, int);


int latcompare(val1, val2)
/* used for qsort of array in ll function */
const void* val1;
const void* val2;
{
  stntype* value1;
  stntype* value2;
 
  value1 = (stntype*)(val1);
  value2 = (stntype*)(val2);

  return (value1->lat - value2->lat);
}


int loncompare(val1, val2)
/* used for qsort of array in ll function */
const void* val1;
const void* val2;
{
  stntype* value1;
  stntype* value2;
 
  value1 = (stntype*)(val1);
  value2 = (stntype*)(val2);

  return (value1->lon - value2->lon);
}


main(argc,argv)
int argc;
char *argv[];
{
int i,filsen=0,stnsen=0,j;
char filnam[256];
char *ptr;
FILE *sfp;

for (i=1; i<argc; i++) {
  if (strcmp(argv[i],"-f")==0) {
    if ((sfp=fopen(argv[++i],"r")) == NULL) {
      fprintf(stderr,"snm: error opening station file %s\n",argv[i]);
      exit(1);
      }
    ll(sfp);
    stnsen=1;
    fclose(sfp);
    continue;
    }
  if (!stnsen) {
    if ((sfp=fopen("fest_sites_sorted","r")) == NULL) {
      fprintf(stderr,"snm: error opening station file fest_sites_sorted\n");
      exit(1);
      }
    ll(sfp);
    stnsen=1;
    fclose(sfp);
    }
  filsen = 1;
  if ((ifp=fopen(argv[i],"r")) == NULL) {
    fprintf(stderr,"snm: error opening input file %s\n",argv[i]);
    exit(1);
    }
  strcpy(filnam,argv[i]);
  ptr = strrchr(filnam,'.');
  if (ptr) *ptr = '\0';
  strcat(filnam,".snam");
  if ((ofp=fopen(filnam,"w")) == NULL) {
    fprintf(stderr,"snm: error opening output file %s\n",filnam);
    exit(1);
    }
  snm();
  printcts();
  printf("%s:\n\t%d records seen.\n",argv[i],ctrec);
  printf("\t%d unknown stations (not in list).\n",ctnot);
  } /* end for argc */
if (!filsen) {
  fprintf(stderr,"snm: no file given.\n%s\n",USAGE);
  exit(1);
  }
exit(0);
} /* end main */



snm()
/*
 * snm - station name; the actual processing of the input file
 */
{
int i,j,qual,tmpdat,tmpmon,tmpday,acflag;
float lat,lon,val;
char buf[1230],*ptr,qcflg;
stntype *theray;
static float prevlat;
static float prevlon;

ctrec=ctnot=ctmem=0;

while (fgets(buf,1227,ifp)) {

ctrec++;
if (sscanf(buf+45,"%f %f",&lat,&lon) < 2) {
  fprintf(stderr,"snm: could not sscanf buf\n");
  exit(1);
  }

if ((i=bs(lat,lon)) < 0) { 			/* input station not in list */
  ctmem++;
  if (prevlat != lat || prevlon != lon) {
    ctnot++;
  	fprintf(stderr,"%10.5f %11.5f dropped %.4d times\n",lat,lon,ctmem);
  	fflush(stderr);
	ctmem = 0;
	prevlat = lat;
	prevlon = lon;
  }
  continue; /* while fgets */
}
else theray=stnray; /* use regular station list array */

  /* check to see if we should flag this station as accumulations */
  /* if latlon is in list, and this is a 15min station, then flag */
  /* XXX - kludge for 15m stations that were really hourly */
  /* acflag=(findacum(lat,lon) && ((int)strlen(buf)>1029)); */
  acflag=0;

  /* put header stuff */
  buf[17]='\0';
  fputs(buf,ofp);
  buf[17]=' ';
  fputs(" Rec rainga ",ofp);
  fputs(theray[i].stnam,ofp);
  fprintf(ofp," %10.5f %11.5f",lat,lon);
  buf[71]='\0';
  fputs(buf+67,ofp);
  buf[71]=' ';

  /* find out day */
  sscanf(buf,"%*d/%d/%d",&tmpmon,&tmpday);
  if (tmpmon == 2) tmpdat=tmpday;
  else tmpdat=tmpday+29;

  /* if we have it, put left over data from yesterday's 24:00 bucket */
  if (theray[i].dat+1 == tmpdat)
   fprintf(ofp," %7.2f %1d %c",theray[i].val,theray[i].qual,theray[i].qcflg);
  else fprintf(ofp," -999.99 7 M");

  /* put today's data, but save the last bucket for tomorrow */
  for (ptr=buf+71,j=0; ((*(ptr+12))&&(*(ptr+12) != '\n')); ptr+=12,j=(j+1)%4) {
    if (sscanf(ptr,"%f %d %c",&val,&qual,&qcflg) < 3) {
      fprintf(stderr,"snm: sscanf error in snm() after for\n");
      exit(1);
      }

    if (acflag) {
     if ((qual != 3) && (qual != 7)) { /* don't change deleted/missing flags */
      if (j==3)
        qual=2; /* on the hour, set to end of accumulation */
      else qual=1; /* else set to during accumulation */
      } /* end if qual */
     } /* end if acflag */
    fprintf(ofp," %7.2f %1d %c",val,qual,qcflg);
    } /* end for ptr */
  if (sscanf(ptr,"%f %d %c",&val,&qual,&qcflg) < 3) {
    fprintf(stderr,"snm: sscanf error in snm() after for\n");
    exit(1);
    }
  putc('\n',ofp);
  theray[i].dat = tmpdat;
  theray[i].val = val;
  if ((acflag) && (qual != 3) && (qual != 7))
    theray[i].qual = 2;
  else theray[i].qual = qual;
  theray[i].qcflg = qcflg;

} /* end while */
} /* end snm */


ll(fp)
/*
 * ll - load station list from fp; assumes file ptd to by fp is sorted
 */
FILE *fp;
{
long l;
int valid_start, valid_stop, j, index1, index2;
char buf[121];
stntype *p;
float tmplat,tmplon;
char tmplat_indegrees[15];
char tmplon_indegrees[15];


if (fseek(fp,0L,2) < 0) {
  fprintf(stderr,"ll: bad fseek2\n");
  exit(2);
  }

l=ftell(fp);
l=l/81;
if (l != len) {
  if (stnray == NULL) {
    if ((stnray=(stntype *)malloc(l*sizeof(stntype)))==NULL) {
      fprintf(stderr,"ll: error malloc\n");
      exit(3);
      }
    }
  else if ((p=(stntype *)realloc(stnray,l*sizeof(stntype))) == NULL) {
    fprintf(stderr,"ll: error realloc\n");
    exit(3);
    }
  else stnray=p;
  len=l;
  }
if (fseek(fp,0L,0) < 0) {
  fprintf(stderr,"ll: bad fseek0\n");
  exit(2);
  }
l=0;
while (fgets(buf,120,fp)) {

/* Make sure only data for the correct time period: */
/* 19940401 through 19940831 is used */

  sscanf(buf+31, "%d %d", &valid_start, &valid_stop);

  if( (valid_start <= 19940401) && (valid_stop >= 19940831) )
  {  

    for (j=21;j<27;j++) stnray[l].stnam[j-21]=buf[j]; /* station id name */
    for (j=28;j<30;j++) stnray[l].stnam[j-22]=buf[j]; /* station id name continued */
	/* j = 27 is a space in station file which needs to be removed */
    stnray[l].stnam[8]=' ';
    stnray[l].stnam[9]=' ';
    stnray[l].stnam[10]=' ';
    stnray[l].stnam[11]=' ';    /* pad station so output format is correct */
    stnray[l].stnam[12]=' ';
    stnray[l].stnam[13]=' ';
    stnray[l].stnam[14]=' ';
    stnray[l].stnam[15]='\0';

    for (j=49;j<54;j++) tmplat_indegrees[j-49]=buf[j];
    for (j=55;j<61;j++) tmplon_indegrees[j-55]=buf[j];
    tmplat_indegrees[5] ='\0';
    tmplon_indegrees[6] ='\0';
          /* latlon */


    tmplat = ConvertToDecimalDegrees(tmplat_indegrees, 0);
    tmplon = ConvertToDecimalDegrees(tmplon_indegrees, 1);

    if (tmplat < 0.0)
      stnray[l].lat = tmplat*100.0 - 0.5;
    else stnray[l].lat = tmplat*100.0 + 0.5;
    if (tmplon < 0.0)
      stnray[l].lon = tmplon*100.0 - 0.5;
    else stnray[l].lon = tmplon*100.0 + 0.5;
    stnray[l].val=-999.99;
    stnray[l].qual=7;
    stnray[l].qcflg='M';
    stnray[l].dat=0;

    if (++l > len) {
      fprintf(stderr,"ll: l>len\n");
      exit(2); 
      }

  }    /* end if valid */
 } /* end while fgets */

 len=l-1; 
 /* set the length of the array accounting for the +1 from (if (++l > len)) */ 

 /* Now make sure final array is sorted correctly */
 /* NOTE: this is necessary due to the initial format of the station list lat/lon */

 qsort(stnray, len, sizeof(stntype), latcompare);

 for(index1=0; index1<len; index1++)
 {
   if (stnray[index1 + 1].lat == stnray[index1].lat)  
   {
     for(index2=index1 + 1; stnray[index2].lat == stnray[index1].lat; index2++);
     /*now index1 points to the start of the section to be sorted by longitude */
     /*index2 points to one after the end of the section to be sorted */
    
     qsort(stnray + index1, index2-index1, sizeof(stntype), loncompare);
     index1=index2-1;
   }
 }
} /* end ll */


bs(lat,lon)
/*
 * bs - binary search on the station list
 *
 * Returns -1 if not in list; else returns index into stnray
 *
 */
float lat,lon;
{
int    min,max,j, k;
long   lati,loni;


if (lat < 0.0)
  lati = lat*100.0 - 0.5;
else lati = lat*100.0 + 0.5;
if (lon < 0.0)
  loni = lon*100.0 - 0.5;
else loni = lon*100.0 + 0.5;

/* 
 * For Binary Search loop... do binary Search until
 * match of both lat/lon or just lat. If lat matches
 * then we are close. Use a sequential search at that
 * point.
 */
for (min=0,max=len,j=len/2; min<=max; j=min+(max-min)/2 ) 
{ 
  if ( (inrange(stnray[j].lat,lati,TOLERANCE)) && 
       (inrange(stnray[j].lon,loni,TOLERANCE)) ) 
    return(j);
  else if (stnray[j].lat > lati)
    max = j - 1;
  else if (stnray[j].lat < lati)
    min = j + 1;
  else if (stnray[j].lon > loni)
    max = j - 1;
  else min = j + 1;
}


return(-1);
} /* end bs */



printcts()
/*
 * printcts - print only those stations who have exactly one nonzero count
 */
{
int j;
unsigned long *ct,*ac;
/*
for (j=0;j<len;j++)
    printf("%10.5ld %11.5ld %ld %ld %ld %ld\n",stnray[j].lat,stnray[j].lon,
      stnray[j].ct[0],stnray[j].ct[1],stnray[j].ct[2],stnray[j].ct[3]);
for (j=0;j<len;j++)
  if ((stnray[j].ct[0] != 0) || (stnray[j].ct[1] != 0) ||
      (
stnray[j].ct[2] != 0) || (stnray[j].ct[3] != 0))
    printf("%10.5ld %11.5ld %ld %ld %ld %ld\n",stnray[j].lat,stnray[j].lon,
      stnray[j].ct[0],stnray[j].ct[1],stnray[j].ct[2],stnray[j].ct[3]);
*/
/*
printf("lat lon ct0 ct1 ct2 ct3 / ac0 ac1 ac2 ac3 / tot0 tot1 tot2 tot3 / max\n");
printf("KNOWN 15-MINUTE STATIONS:\n");
for (j=0;j<len;j++) {
 ct=&stnray[j].ct[0];
 ac=&stnray[j].ac[0];
 if ((!ct[0] && !ct[1] && !ct[2] && ct[3])||
     (!ct[0] && !ct[1] && !ct[3] && ct[2])||
     (!ct[0] && !ct[2] && !ct[3] && ct[1])||
     (!ct[1] && !ct[2] && !ct[3] && ct[0]))
printf("%10.5ld %11.5ld %ld %ld %ld %ld / %ld %ld %ld %ld / %ld %ld %ld %ld / %7.2f\n",
    stnray[j].lat,stnray[j].lon,ct[0],ct[1],ct[2],ct[3],
    ac[0],ac[1],ac[2],ac[3],ct[0]+ac[0],ct[1]+ac[1],ct[2]+ac[2],ct[3]+ac[3],
    stnray[j].max);
 }
printf("UNKNOWNS:\n");
for (j=0;j<unklen;j++) {
 ct=&unkray[j].ct[0];
 ac=&unkray[j].ac[0];
 if ((!ct[0] && !ct[1] && !ct[2] && ct[3])||
     (!ct[0] && !ct[1] && !ct[3] && ct[2])||
     (!ct[0] && !ct[2] && !ct[3] && ct[1])||
     (!ct[1] && !ct[2] && !ct[3] && ct[0]))
printf("%10.5ld %11.5ld %ld %ld %ld %ld / %ld %ld %ld %ld / %ld %ld %ld %ld / %7.2f\n",
    unkray[j].lat,unkray[j].lon,ct[0],ct[1],ct[2],ct[3],
    ac[0],ac[1],ac[2],ac[3],ct[0]+ac[0],ct[1]+ac[1],ct[2]+ac[2],ct[3]+ac[3],
    unkray[j].max);
 }
*/
} /* end printcts */


findunk(lat,lon)
/*
 * findunk - latlon is not in the station list (stnray), so
 *           find it in the additional unknown array (unkray)
 * returns < 0 on error (array overflow) or index of latlon in unkray
 */
float lat,lon;
{
int j;
long lati,loni;

if (lat < 0.0)
  lati = lat*100000.0 - 0.5;
else lati = lat*100000.0 + 0.5;
if (lon < 0.0)
  loni = lon*100000.0 - 0.5;
else loni = lon*100000.0 + 0.5;

for (j=0;j<unklen;j++)
  if( (inrange(unkray[j].lat,lati,TOLERANCE)) && 
     (inrange(unkray[j].lon,loni,TOLERANCE)) )
    return(j);
if (unklen >= MAXUNK) return(-1);
unkray[unklen].lat = lati;
unkray[unklen].lon = loni;
unkray[unklen].val = -999.99;
unkray[unklen].qual = 7;
unkray[unklen].qcflg = 'M';
unkray[unklen].dat=0;
strcpy(unkray[unklen].stnam,"               ");
unklen++;
return(unklen-1);
} /* end findunk */


long acclat[8] = { 2973333,3413333,3545000,3795000,
		   3938333,4026667,4141667,4250000 };
long acclon[8] = { -8503333,-10851667,-8680000,-9176667,
		   -7443333,-7478333,-7370000,-7086667 };

findacum(lat,lon)
/*
 * findacum - see if latlon is a station that should be marked accumulating
 */
float lat,lon;
{
long lati,loni;
int j;

if (lat < 0.0)
  lati = lat*100000.0 - 0.5;
else lati = lat*100000.0 + 0.5;
if (lon < 0.0)
  loni = lon*100000.0 - 0.5;
else loni = lon*100000.0 + 0.5;

for (j=0;j<8;j++)
  if (inrange(acclat[j],lati,TOLERANCE) && inrange(acclon[j],loni,TOLERANCE))
    return(1);
return(0);
} /* end findacum */

/*****************************************************************
*	ConvertToDecimalDegrees - take degree minute direction
*				- data and covert to decimal degrees
*
* Created -- 12 Jun 95 mhc
******************************************************************/

float ConvertToDecimalDegrees(char* Location, int IsLongitude)
{
  float Degrees = 0;
  float Minutes = 0;
  float Value = 0;
  char Direction;
 
  if( IsLongitude )
    sscanf(Location,"%3f%2f%c", &Degrees, &Minutes, &Direction);
  else
    sscanf(Location,"%2f%2f%c", &Degrees, &Minutes, &Direction);
    
  Value = ( Degrees + (Minutes/60) );
  if( (Direction == 'S') || (Direction == 'W') )
    return( -1 * Value );
  else
    return Value;
} 

