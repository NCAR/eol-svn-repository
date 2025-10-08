/* classio.c
 *
 * routines to read/write STORM CLASS Format
 *
 * Mark Bradford, STORM Project Office, NCAR, Boulder, May 1992
 *
 * $Id
 * $Log
 */

#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <dirent.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <string.h>
#include "xqc.h"

char **name=0;
const int NAME_INC=128;
int name_capacity=0;

char hdr[HDRLINES][LINEBUF];
Data d[MAXLINES];
QC qc[MAXLINES];

int compressed;
char path[81];
char site[4];
char cmd[LINEBUF];
int write_okay=0;
int numlines=0;
int numfiles=0;
int thisfile=0;

char *infile, *outfile;

char *new_site(s)
char *s;
    {
    free_dir();
    make_dir();
    if (numfiles < 1)
	/* no files */
	{
	return NULL;
	}
    thisfile=1;
    strcpy(s, name[0]);
    return(s);
    }

int
compar(char **a, char **b) 
    {
    return strcmp(*a, *b);
    }

make_dir()
    {
    DIR *dirp;
    struct dirent *dp;

    dirp=opendir(".");
    if (dirp == NULL)
	oops(BADDIRLIST);
    while (dp=readdir(dirp))
	{
	if (!strncmp(dp->d_name,site,3) && !strstr(dp->d_name,".qc") &&
	    !strstr(dp->d_name,"chk"))
	    {
/*	    printf("Site %s, file %s\n", site, dp->d_name); */
	    /* save name */
	    numfiles++;
	    if (numfiles > name_capacity)
		{
		/* grow capacity */
		int i;
		char **old_name=name;
		name=(char **)malloc((name_capacity + NAME_INC) *
				     sizeof(char *));
		if (name == 0)
		    oops(OUTOFMEMORY);
		if (old_name)
		    {
		    for (i=0; i<name_capacity; i++)
			name[i]=old_name[i];
		    free(old_name);
		    }
		name_capacity+=NAME_INC;
		}
	    name[numfiles-1]=(char *)malloc((strlen(dp->d_name) + 1) *
					    sizeof(char));
	    if (name[numfiles-1] == 0)
		oops(OUTOFMEMORY);
	    strcpy(name[numfiles-1], dp->d_name);
/*	    printf("Copied %s to slot %d.\n", dp->d_name, numfiles-1); */
	    }
	}
    closedir(dirp);
    qsort(name, numfiles, sizeof(char *), compar);
    }

free_dir()
    {
    int i;
    for (i=0; i<numfiles; i++)
	{
	free(name[i]);
	}
    free(name);
    }

setthisfile()
    {
    int i;

/*    printf("Seeking: %s Numfiles: %d\n", infile, numfiles); */
    for (i=0; i<numfiles; i++)
	{
/*	printf("Found: %s Seeking: %s\n", name[i], infile); */
	if (!strncmp(name[i], infile, strlen(infile)))
	    {
	    thisfile=i+1;
	    return(0);
	    }
	}
    return(1);
    }

int read_class(file, numlines)
char *file;
int *numlines;
    {
    FILE *f;
    int i;

    *numlines=0;
    if (compressed)
	{
	strcpy(cmd,"uncompress ");
	strcat(cmd, file);
	if (system(cmd))
	    oops(BADUNCOMPRESS);
	}
    if ((f=fopen(file,"r"))==NULL)
	oops(BADOPEN);
    for (i=0; i<15; i++)
	if (fgets(hdr[i], LINEBUF, f)==NULL)
	    oops(BADREAD);
    while (!(i=read_line(f, &d[*numlines], &qc[*numlines])))
	{
	(*numlines)++;
	if (*numlines > MAXLINES)
	    oops(TOOBIG);
	}
    fclose(f);
    if (compressed)
	{
	strcpy(cmd, "compress ");
	strcat(cmd, file);
	strcat(cmd, " &");
	if (system(cmd))
	    oops(BADCOMPRESS);
	}
    if (i==EOFFOUND)
	return(0);
    else
	oops(i);
    }

int write_class(file, numlines)
char *file;
int numlines;
    {
    FILE *f;
    int i;

    if ((f=fopen(file,"w"))==NULL)
	oops(BADOPEN);
    for (i=0; i<15; i++)
	fputs(hdr[i], f);
    for (i=0; i<numlines; i++)
	if (write_line(f, &d[i], &qc[i]))
	    oops(BADWRITE);
    fclose(f);
    if (compressed)
	{
	strcpy(cmd, "compress -f ");
	strcat(cmd, file);
	strcat(cmd, " &");
	if (system(cmd))
	    oops(BADCOMPRESS);
	}
    return(0);
    }

int read_line(f, d, qc)
FILE *f;
Data *d;
QC *qc;
    {
    char l[LINEBUF];
/*    static char *infmt="%f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %*[^]\n";*/
    static char *infmt="%f %f %f %f %f %f %f %f \
%f %f %f %f %f %f %f %f %f %f %f %f %f%*[^]\n";
    int i;

    if (fgets(l, LINEBUF, f)==NULL)
	if (feof(f))
	    return(EOFFOUND);
	else
	    oops(BADREAD);
    i=sscanf(l, infmt, &(d->time), &(d->pres), &(d->t), &(d->dp), &(d->rh),
	     &(d->uwind), &(d->vwind), &(d->wspd), &(d->wdir), &(d->dz),
	     &(d->lon), &(d->lat), &(d->tmp1), &(d->tmp2), &(d->alt),
	     &(qc->qp), &(qc->qt), &(qc->qh), &(qc->qu), &(qc->qv), &(qc->qw));
    if ((d->pres <= PMAX) && (d->pres >= PMIN))
	{
	if (qc->qp==UNCHECKED)
	    qc->qp=GOOD;
	if (qc->qt==UNCHECKED)
	    qc->qt=GOOD;
	if (qc->qh==UNCHECKED)
	    qc->qh=GOOD;
	if (qc->qu==UNCHECKED)
	    qc->qu=GOOD;
	if (qc->qv==UNCHECKED)
	    qc->qv=GOOD;
	}
    return (0);
    }

int write_line(f, d, qc)
FILE *f;
Data *d;
QC *qc;
    {
    static char *outfmt="%6.1f %6.1f %5.1f %5.1f %5.1f %6.1f %6.1f %5.1f \
%5.1f %5.1f %8.3f %7.3f %5.1f %5.1f %7.1f %4.1f %4.1f %4.1f %4.1f \
%4.1f %4.1f\n";

    return((fprintf(f, outfmt, d->time, d->pres, d->t, d->dp, d->rh,
		    d->uwind, d->vwind, d->wspd, d->wdir, d->dz,
		    d->lon, d->lat, d->tmp1, d->tmp2, d->alt,
		    qc->qp, qc->qt, qc->qh, qc->qu, qc->qv, qc->qw))==EOF);
    }

char *latmin(deg)
float deg;
    {
/*  Mark Bradford, STORM Project Office, April 1992

    Given latitude in decimal degrees (DEG), return a string in the format
    "dd mm.mm'D", where dd is degrees, mm.mm is decimal minutes,
    and D is N or S appropriately.
*/
    int whole;
    float frac;
    char *s, dxn;

    if ((s=(char *)malloc(11*sizeof(char)))==NULL)
	oops(OUTOFMEMORY);
    whole=(int)deg;
    frac=deg-whole;
    if (whole < 0)
	{
	dxn='S';
	whole= -whole;
	frac= -frac;
	}
    else
	dxn='N';
    sprintf(s,"%02.2i %05.2'%c", whole, frac*60., dxn);
    return s;
    }
    
char *lonmin(deg)
float deg;
    {
/*  Mark Bradford, STORM Project Office, April 1992

    Given longitude in decimal degrees (DEG), return a string in the format
    "ddd mm.mm'D", where ddd is degrees, mm.mm is decimal minutes,
    and D is E or W appropriately.
*/

    int whole;
    float frac;
    char *s, dxn;

    if ((s=(char *)malloc(12*sizeof(char)))==NULL)
	oops(OUTOFMEMORY);
    whole=(int)deg;
    frac=deg-whole;
    if (whole < 0)
	{
	dxn='W';
	whole= -whole;
	frac= -frac;
	}
    else
	dxn='E';
    sprintf(s,"%03.3i %05.2'%c", whole, frac*60., dxn);
    return s;
    }

oops(err)
char *err;
    {
    fprintf(stderr, "ERROR: %s!\n", err);
    exit(1);
    }

char *next_file(s)
char *s;
    {
    if (thisfile < numfiles)
	{
	thisfile++;
	strcpy(s, name[thisfile-1]);
	return (s);
	}
    return ((char *)NULL);
    }

char *prev_file(s)
char *s;
    {
    if (thisfile > 1)
	{
	thisfile--;
	strcpy(s, name[thisfile-1]);
	return(s);
	}
    return ((char *)NULL);
    }
