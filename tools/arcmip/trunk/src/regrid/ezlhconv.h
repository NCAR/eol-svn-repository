/*
 * ezlhconv.h - Prototype definitions for SSM/I EASE-Grid conversion programs 
 *
 * 11/01/1994  M. J. Brodzik  brodzik@zamboni.colorado.edu  303-492-8263
 * National Snow & Ice Data Center, University of Colorado, Boulder
 *
 *$Revision: 1.2 $
 */
/* made into a real, useful .h by john@joss.ucar.edu */

#ifndef _ezlhconv_h
#define _ezlhconv_h

#include <math.h>

#undef PI
#define PI M_PI
#define TWO_PI 6.28318530717958647692

#define DEG_TO_RAD 0.01745329251994329576
#define RAD_TO_DEG 57.29577951308232087684

#define rad(t) ((t)*PI/180.0)
#define deg(t) ((t)*180.0/PI)

/* radius of the earth (km), authalic sphere based on International datum */
#define RE_km    6371.228
/* nominal cell size in kilometers */
#define CELL_km    25.067525

#define PI_R 20015.80307914559119508702
#define TWO_PI_R 40031.60615829118239017405

/* scale factor for standard paralles at +/-30.00 degrees */
#define COS_PHI1 .866025403


int ezlh_convert(char grid[], double lat, double lon, double *r, double *s);
int ezlh_inverse(char grid[], double r, double s, double *lat, double *lon);

#endif
