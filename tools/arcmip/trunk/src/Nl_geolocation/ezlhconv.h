/*
 * ezlhconv.h - Prototype definitions for SSM/I EASE-Grid conversion programs 
 *
 * 11/01/1994  M. J. Brodzik  brodzik@zamboni.colorado.edu  303-492-8263
 * National Snow & Ice Data Center, University of Colorado, Boulder
 *
 *$Revision: 1.2 $
 */
int ezlh_convert(char grid[], double lat, double lon, double *r, double *s);
int ezlh_inverse(char grid[], double r, double s, double *lat, double *lon);
