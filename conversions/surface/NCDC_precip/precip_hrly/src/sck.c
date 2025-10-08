
/*
 * $Log$
 *
 */

#ifndef lint
static char *rcsid = "$Id: sck.c,v 1.1 1993/02/16 22:42:56 john Exp $";
#endif

/*
 * "sck.c" - Station Check program for 15-minute PQCF data from NCDC EBUFR
 *
 * Synopsis:
 *  sck [-f stnlist] file [[-f stnlist] file] ...
 * where
 *  stnlist is the file to be used instead of "hpd_stns"
 *
 * Checks a 15-minute PQCF file (made from NCDC EBUFR) for hourly
 * stations. Does so by matching latlon to a file "hpd_stns" in the
 * current directory. Since there will be alot of checking, reads
 * the file into memory and then uses a binary search of the memory.
 *
 * The stations are considered hourly or 15-minute based on what the
 * file "hpd_stns" says. Stations not in "hpd_stns" are not removed.
 *
 * Output:
 *  file.snew : the file with hrly stations removed
 *  stdout : count totals
 *  stderr : errors
 *
 *  Count totals include: number of stations in original file,
 *  number of stations found in hpd_stns, number of original stations
 *  dropped (because they are hourly).
 *
 * Written by:
 *  John J. Allison / john@lightning.ofps.ucar.edu
 *  25 Jan 1992, modified from "pqc.c" (21 Sep 1992 and 04 Nov 1992)
 *
 *   
 * Modfied -- 12 Jun 95 mhc
 *      * Increased station id name to 15 characters and changed array
 *        indices accordingly.
 * 
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define USAGE "Usage: sck [-f stnlist] file [[-f stnlist] file] ..."

int ctrec,ctnot,ctfif,cthly;
FILE *ifp,*ofp;


main(argc,argv)
int argc;
char *argv[];
{
int i,filsen=0,stnsen=0;
char filnam[256];
char *ptr;
FILE *sfp;

for (i=1; i<argc; i++) {
  if (strcmp(argv[i],"-f")==0) {
    if ((sfp=fopen(argv[++i],"r")) == NULL) {
      fprintf(stderr,"sck: error opening station file %s\n",argv[i]);
      exit(1);
      }
    ll(sfp);
    stnsen=1;
    fclose(sfp);
    continue;
    }
  if (!stnsen) {
    if ((sfp=fopen("hpd_stns","r")) == NULL) {
      fprintf(stderr,"sck: error opening station file hpd_stns\n");
      exit(1);
      }
    ll(sfp);
    stnsen=1;
    fclose(sfp);
    }
  filsen = 1;
  if ((ifp=fopen(argv[i],"r")) == NULL) {
    fprintf(stderr,"sck: error opening input file %s\n",argv[i]);
    exit(1);
    }
  strcpy(filnam,argv[i]);
  ptr = strrchr(filnam,'.');
  if (ptr) *ptr = '\0';
  strcat(filnam,".snew");
  if ((ofp=fopen(filnam,"w")) == NULL) {
    fprintf(stderr,"sck: error opening output file %s\n",filnam);
    exit(1);
    }
  sc();
  printf("%s:\n\t%d records seen.\n",argv[i],ctrec);
  printf("\t%d 15-minute stations.\n\t%d hourly stations.\n",ctfif,cthly);
  printf("\t%d unknown stations (not in list).\n",ctnot);
  } /* end for argc */
if (!filsen) {
  fprintf(stderr,"sck: no file given.\n%s\n",USAGE);
  exit(1);
  }
exit(0);
} /* end main */



sc()
/*
 * sc - station check; the actual processing of the input file
 */
{
int i;
float lat,lon;
char buf[1230];

ctrec=ctnot=ctfif=cthly=0;

while (fgets(buf,1227,ifp)) {

ctrec++;
if (sscanf(buf+45,"%f %f",&lat,&lon) < 2) {
  fprintf(stderr,"sc: could not sscanf buf\n");
  exit(1);
  }
if ((i=bs(lat,lon)) < 0) { /* input station not in list */
  fprintf(stderr,"%10.5f %11.5f not in station list\n",lat,lon);
  fputs(buf,ofp);
  ctnot++;
  }
else if (((int)strlen(buf) > 1029) && (i>0)) {
/* input station matches a station in the list and both are 15minute */
  fputs(buf,ofp);
  ctfif++;
  }
else cthly++; /* input station matches but list says hourly */

} /* end while */
} /* end sc */



typedef struct {
  long lat,lon;
  unsigned char fflag;
  } stntype;
stntype *stnray=NULL;
long len=0;


ll(fp)
/*
 * ll - load station list from fp; assumes file ptd to by fp is sorted
 */
FILE *fp;
{
long l;
char buf[41],tmpstr[5];
stntype *p;
float tmplat,tmplon;

if (fseek(fp,0L,2) < 0) {
  fprintf(stderr,"ll: bad fseek2\n");
  exit(2);
  }
l=ftell(fp);
l=l/36;
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
while (fgets(buf,40,fp)) {
  if (sscanf(buf,"%*s %f %f %s",&tmplat,&tmplon,tmpstr) < 3) {
    fprintf(stderr,"ll: sscanf buf error\n");
    exit(1);
    }
  if (tmplat < 0.0)
    stnray[l].lat = tmplat*100000.0 - 0.5;
  else stnray[l].lat = tmplat*100000.0 + 0.5;
  if (tmplon < 0.0)
    stnray[l].lon = tmplon*100000.0 - 0.5;
  else stnray[l].lon = tmplon*100000.0 + 0.5;
  if (strcmp(tmpstr,"15M")==0)
    stnray[l].fflag=1;
  else stnray[l].fflag=0;
  if (++l > len) {
    fprintf(stderr,"ll: l>len\n");
    exit(2);
    }
  } /* end while fgets */
} /* end ll */



bs(lat,lon)
/*
 * bs - binary search on the station list
 *
 * Returns -1 if not in list; 0 if in list as hourly; 1 if in list as 15-min
 */
float lat,lon;
{
int min,max,j;
long lati,loni;

#define inrange(x,y,r) ((x<=y+r)&&(x>=y-r))

if (lat < 0.0)
  lati = lat*100000.0 - 0.5;
else lati = lat*100000.0 + 0.5;
if (lon < 0.0)
  loni = lon*100000.0 - 0.5;
else loni = lon*100000.0 + 0.5;

for (min=0,max=len,j=len/2; min<=max; j=min+(max-min)/2 )
/*  if ((stnray[j].lat == lati) && (stnray[j].lon == loni)) */
  if (inrange(stnray[j].lat,lati,1) && inrange(stnray[j].lon,loni,1))
    return(stnray[j].fflag);
  else if (stnray[j].lat > lati)
    max = j - 1;
  else if (stnray[j].lat < lati)
    min = j + 1;
  else if (stnray[j].lon > loni)
    max = j - 1;
  else min = j + 1;
return(-1);
} /* end bs */
