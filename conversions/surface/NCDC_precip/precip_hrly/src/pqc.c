
/*
 * $Log$
 *
 */

#ifndef lint
static char *rcsid = "$Id: pqc.c,v 1.1 1993/02/16 22:36:37 john Exp $";
#endif

/*
 * "pqc.c" - QC program for Precip data
 *
 * Synopsis:
 *  pqc [-b bmax] [-d dmax] files ...
 * where
 *  value is marked Bad if it is > bmax
 *  value is marked Dubious if it is <= bmax and > dmax
 *  else value is marked Good
 *
 * Simple, it just checks the values against bmax and dmax.
 * But, not all values are checked. Only "normal" values are checked,
 * that is all non-missing (QC flag 'M'), non-not-measured ('N'),
 * values with a qualifier of 0 are checked. Values with qualifiers
 * of 1 or 2 (accumulation stuff) are skipped.
 *
 * Output:
 *  file.pqcf : the QC'd file
 *  file.toss : all records marked as B or D
 *  stdout : count totals
 *  stderr : errors
 *
 * Written by:
 *  John J. Allison / john@lightning.ofps.ucar.edu
 *  21 Sep 1992 and 04 Nov 1992
 *
 * 06 Oct 93 lec
 *   Set default bmax and dmax values to valid hourly
 *   precip values. 
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

/*
 * Following values for hrly precip.
 */
#define BMAX 75.0 
#define DMAX 25.0 

#if 0
/*
 * Following values for 15 min precip.
 * (First set = actual for STORM-FEST;
 *  second and third sets = test sets?
 */
#define BMAX 40.0
#define DMAX 20.0

#define BMAX 25.4
#define DMAX 12.7

#define BMAX 40.0
#define DMAX 15.0

#endif

#define USAGE "Usage: pqc [-b bmax] [-d dmax] files ..."

int ctun,ctrec,ctval,ctbad,ctdub,ctgood;
FILE *ifp,*ofp,*tfp;
float bmax=BMAX,dmax=DMAX;



main(argc,argv)
int argc;
char *argv[];
{
int i,filsen=0;
char filnam[256];
char *ptr;

for (i=1; i<argc; i++) {
  if (argv[i][0] == '-') {
    switch (argv[i][1]) {
      case 'b':
	bmax = atof(argv[++i]);
	break;
      case 'd':
	dmax = atof(argv[++i]);
	break;
      default:
	fprintf(stderr,"pqc: ignoring unknown option %s\n",&argv[i][1]);
	fprintf(stderr,"%s\n",USAGE);
	break;
      } /* end switch */
    continue;
    } /* end if argv == '-' */

  filsen = 1;
  if ((ifp=fopen(argv[i],"r")) == NULL) {
    printf("pqc: error opening input file %s\n",argv[i]);
    exit(1);
    }
  strcpy(filnam,argv[i]);
  ptr = strrchr(filnam,'.');
  if (ptr) *ptr = '\0';
  strcat(filnam,".pqcf");
  if ((ofp=fopen(filnam,"w")) == NULL) {
    printf("pqc: error opening output file %s\n",filnam);
    exit(1);
    }
  strcpy(filnam,argv[i]);
  ptr = strrchr(filnam,'.');
  if (ptr) *ptr = '\0';
  strcat(filnam,".toss");
  if ((tfp=fopen(filnam,"w")) == NULL) {
    printf("pqc: error opening toss file %s\n",filnam);
    exit(1);
    }
  qc();
  printf("pqc totals: %s\n\t%d records.\n\t%d values.\n",argv[i],ctrec,ctval);
  printf("\t%d values originally unchecked.\n",ctun);
  printf("\t%d bad.\n\t%d dubious.\n\t%d good.\n",ctbad,ctdub,ctgood);

} /* end for argc */

if (!filsen) {
  fprintf(stderr,"pqc: no file given.\n%s\n",USAGE);
  exit(1);
  }

exit(0);
} /* end main */



qc()
{
float val;
int qual,tossflag;
char qcf;
char buf[1230],*ptr;

ctun=ctrec=ctval=ctbad=ctdub=ctgood=0;
while (fgets(buf,1227,ifp)) {

ctrec++;
tossflag=0;
buf[71]='\0';
fputs(buf,ofp);
buf[71]=' ';
ptr=buf+71;
while ((*ptr) && (*ptr != '\n')) {
 ctval++;

 if (sscanf(ptr,"%f %d %1s",&val,&qual,&qcf) < 3) {
   fprintf(stderr,"pqc: error sscanfing data in qc()\n"); 
   exit(1); 
   }
 if ((qcf == 'U') && (qual == 0)) {
   ctun++;
   if (val > bmax) {
     qcf = 'B';
     ctbad++;
     ptr[11] = qcf;
     tossflag=1;
/*     putc('\n',ofp); */
     }
   else if (val > dmax) {
     qcf = 'D';
     ctdub++;
     ptr[11] = qcf;
     tossflag=1;
/*     putc('\n',ofp); */
     }
   else {
     qcf = 'G';
     ctgood++;
     }
   } /* end if U */
 fprintf(ofp," %7.2f %1d %c",val,qual,qcf);
 ptr += 12;
 } /* end while ptr */
putc('\n',ofp);
if (tossflag) fputs(buf,tfp);

} /* end while fgets */

} /* end qc */
