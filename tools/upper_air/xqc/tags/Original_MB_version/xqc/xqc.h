
/* Include file for xqc */
/* $Id */
/* $Log */

typedef struct Data_t
    {
    float time;		/* Time in seconds */
    float pres;		/* Pressure in mb */
    float t;		/* Temperature in deg C */
    float dp;		/* Dewpoint in deg C */
    float rh;		/* Relative humidity in % */
    float uwind, vwind;	/* U and V winds in m/s */
    float wspd, wdir;	/* Wind speed (m/s) and direction (deg) */
    float dz;		/* Ascent rate in m/s */
    float lon, lat;	/* Longitude and latitude in degrees */
    float tmp1, tmp2;	/* Placeholder variables (format-dependent) */
    float alt;		/* Altitude in meters */
    } Data;

typedef struct QC_t
    {
    /* Quality control flags: */
    float qp;	/* Pressure */
    float qt;	/* Temperature */
    float qh;	/* Humidity */
    float qu;	/* U wind */
    float qv;	/* V wind */
    float qw;	/* dZ */
    } QC;

/* QC flags: */
#define UNCHECKED (99.0)
#define GOOD (1.0)
#define MAYBE (2.0)
#define BAD (3.0)
#define ESTIMATE (4.0)
#define MISSING (9.0)

#define LINEBUF 255

#define HDRLINES 15
#define MAXLINES 8000

extern char hdr[HDRLINES][LINEBUF];
extern Data d[MAXLINES];
extern QC qc[MAXLINES];
extern int numlines;

/* Errors: */
#define BADOPEN "Couldn't open file"
#define BADREAD "Couldn't read file"
#define EOFFOUND 3
#define OUTOFMEMORY "Out of memory"
#define BADWRITE "Couldn't write file"
#define BADUNCOMPRESS "Couldn't uncompress file"
#define BADCOMPRESS "Couldn't compress file"
#define TOOBIG "CLASS file has too many lines"
#define BADCOLOR "Couldn't allocate X color"
#define BADDIRLIST "Couldn't read directory"
#define BADSITE "Couldn't decipher site"
#define CANTFIND "Couldn't locate directory entry"
#define CANTCD "Couldn't change to directory"

/* Current file information: */
extern int compressed;
extern char path[81];
extern char site[4];
extern char *infile, *outfile;
extern int write_okay;

extern char *next_file(), *prev_file(), *new_site();

/* Button meanings: */
#define B_CLEAR 1
#define B_READ 2
#define B_WRITE 3
#define B_NEXT 4
#define B_PREV 5
#define B_SITE 6
#define B_QUIT 7

#define PRES 1
#define DP 2
#define TEMP 4
#define WINDS 8

#include <X11/Xlib.h>

extern Display *display;
extern int screen_number;
extern Window dispWin;
extern int should_clear;
extern int parameter, mark_as;
extern int reverseColor;

#define XtDisplay(a) display
#define GWFrame(a) dispWin
#define GWWidth(a) 700
#define GWHeight(a) 700
#define WIDTH 700
#define HEIGHT 700
#define BOXSIZE 100

#define PMIN (50.)
#define PMAX (1050.)
#define TMIN (-40.)
#define TMAX (35.)
