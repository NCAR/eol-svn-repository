/*==========================================================================
 * ezlhconv.c - C routines for conversion of azimuthal 
 *		equal area and equal area cylindrical grid coordinates
 *              defined for lh grids only
 *
 *	30-Jan.-1992 H.Maybee
 *	20-Mar-1992 Ken Knowles  303-492-0644  knowles@kryos.colorado.edu
 *      15-Dec-1993 M. J. Brodzik 303-492-8263 brodzik@jokull.colorado.edu
 *                  Made changes from 40-20-10 km resolution grids to
 *                  25-12.5 km resolutions
 *	12-Sep-1994 David Hoogstrate 303-492-4116 hoogstra@jokull.colorado.edu
 * 	            Changed grid cell size.  Changed "c","f" to "l","h"
 *	25-Oct-1994 David Hoogstrate 303-492-4116 hoogstra@jokull.colorado.edu
 *		    Changed row size from 587 to 586 for Mercator projection
 *		    Changed function names to "ezlh-.."	
 *$Log: ezlhconv.c,v $
 *Revision 1.3  1994/11/01 23:39:57  brodzik
 *Replaced all references to 'ease' with 'ezlh'
 *    	
 *==========================================================================*/

/*
 * spring 2003 John Allison john@joss.ucar.edu
 * added "N5" grid, a 50km version of Nl/Nh
 */

#include <stdio.h>
#include <math.h>

#define PI 3.141592653589793
#define rad(t) ((t)*PI/180)
#define deg(t) ((t)*180/PI)

#undef PI
#define PI M_PI

/* radius of the earth (km), authalic sphere based on International datum */
#define RE_km    6371.228
/* nominal cell size in kilometers */
#define CELL_km    25.067525

/* scale factor for standard paralles at +/-30.00 degrees */
#define COS_PHI1 .866025403


/*--------------------------------------------------------------------------*/
int ezlh_convert(char grid[], double lat, double lon, double *r, double *s)
/*
 *	convert geographic coordinates (spherical earth) to 
 *	azimuthal equal area or equal area cylindrical grid coordinates
 *
 *	status = ezlh_convert (grid, lat, lon, &r, &s)
 *
 *	input : grid - projection name "[NSM][lh]"
 *              where l = "low"  = 25km resolution
 *                    h = "high" = 12.5km resolution
 *		lat, lon - geo. coords. (decimal degrees)
 *
 *	output: r, s - column, row coordinates
 *
 *	result: status = 0 indicates normal successful completion
 *			-1 indicates error status (point not on grid)
 *
 *--------------------------------------------------------------------------*/
{
	double Rg, phi, lam, rho, r0, s0;
	int cols, rows;
	double scale;
	
	if (grid[0] == 'N' || grid[0] == 'S')
	{ cols = 721;
	  rows = 721;
	}
	else if (grid[0] == 'M')
	{ cols = 1383;
	  rows = 586;
	}
	else
	{ fprintf(stderr,"ezlh_convert: unknown projection: %s.\n",grid);
	  return(-1);
	}

	if (grid[1] == 'l')
	{ scale = 1.0;
	}
	else if (grid[1] == 'h')
	{ scale = 2.0;
	}
	else if (grid[1] == '5')
	{ scale = 0.5;
	}
	else
	{ fprintf(stderr,"ezlh_convert: unknown projection: %s.\n",grid);
	  return(-1);
	}

        Rg = scale * RE_km/CELL_km;

/*
 *	r0,s0 are defined such that cells at all scales 
 *	have coincident center points
 */
        r0 = (cols-1)/2. * scale;
        s0 = (rows-1)/2. * scale;

	phi = rad(lat);
        lam = rad(lon);

	if (grid[0] == 'N')
	{ rho = 2 * Rg * sin(PI/4. - phi/2.);
	  *r = r0 + rho * sin(lam);
	  *s = s0 + rho * cos(lam);
	}
	else if (grid[0] == 'S')
	{ rho = 2 * Rg * cos(PI/4. - phi/2.);
	  *r = r0 + rho * sin(lam);
	  *s = s0 - rho * cos(lam);
	}
        else if (grid[0] == 'M')
        { *r = r0 + Rg * lam * COS_PHI1;
          *s = s0 - Rg * sin(phi) / COS_PHI1;
	}

	return(0);
}

/*--------------------------------------------------------------------------*/
int ezlh_inverse(grid, r, s, lat, lon)
char grid[];
double r,s;
double *lat, *lon;
/*
 *	convert azimuthal equal area or equal area cylindrical 
 *	grid coordinates to geographic coordinates (spherical earth)
 *
 *	status = ezlh_inverse (grid, r, s, *lat, *lon)
 *
 *	input : grid - projection name "[NSM][lh]"
 *              where l = "low"  = 25km resolution
 *                    h = "high" = 12.5km resolution
 *		r, s - grid column and row coordinates
 *
 *	output: lat, lon - geo. coords. (decimal degrees)
 *
 *	result: status = 0 indicates normal successful completion
 *			-1 indicates error status (point not on grid)
 *
 *--------------------------------------------------------------------------*/
{
	double Rg, phi, lam, rho, r0, s0;
	double beta, gamma, epsilon, c, x, y, sinphi1, cosphi1;
	int cols, rows;
	double scale;
	
	if (grid[0] == 'N' || grid[0] == 'S')
	{ cols = 721;
	  rows = 721;
	}
	else if (grid[0] == 'M')
	{ cols = 1383;
	  rows = 586;
	}
	else
	{ fprintf(stderr,"ezlh_inverse: unknown projection: %s.\n",grid);
	  return(-1);
	}

	if (grid[1] == 'l')
	{ scale = 1.;
	}
	else if (grid[1] == 'h')
	{ scale = 2.;
	}
	else if (grid[1] == '5')
	{ scale = 0.5;
	}
	else
	{ fprintf(stderr,"ezlh_inverse: unknown projection: %s.\n",grid);
	  return(-1);
	}

        Rg = scale * RE_km/CELL_km;

        r0 = (cols-1)/2. * scale;
        s0 = (rows-1)/2. * scale;

        x = r - r0;
        y = -(s - s0);

        if (grid[0] == 'N' || grid[0] == 'S')
        { rho = sqrt(x*x + y*y);
          if (rho == 0.0)
          { if (grid[0] == 'N') *lat = 90.0;
            if (grid[0] == 'S') *lat = -90.0;
            *lon = 0.0;
	  }
          else
          { if (grid[0] == 'N')
            { sinphi1 = sin(PI/2);
              cosphi1 = cos(PI/2);
              if (y == 0)
              { if (r <= r0) lam = -PI/2;
                if (r >  r0) lam = PI/2;
	      }
	      else
	      { lam = atan2(x,-y);
	      }
	    }
            else if (grid[0] == 'S')
            { sinphi1 = sin(-PI/2);
              cosphi1 = cos(-PI/2);
              if (y == 0)
              { if (r <= r0) lam = -PI/2;
                if (r >  r0) lam = PI/2;
	      }
              else
              { lam = atan2(x,y);
	      }
	    }
            gamma = rho/(2 * Rg);
	    if (fabs(gamma) > 1) return(-1);
            c = 2 * asin(gamma);
            beta = cos(c) * sinphi1 + y * sin(c) * (cosphi1/rho);
	    if (fabs(beta) > 1) return(-1);
            phi = asin(beta);
            *lat = deg(phi);
            *lon = deg(lam);
          }
	}
	else if (grid[0] == 'M')
/*
 *	  allow .5 cell tolerance in arcsin function
 *	  so that grid coordinates which are less than .5 cells
 *	  above 90.00N or below 90.00S are given a lat of 90.00
 */
	{ epsilon = 1 + 0.5/Rg;
          beta = y*COS_PHI1/Rg;
          if (fabs(beta) > epsilon) return(-1);
	  else if (beta <= -1) phi = -PI/2;
	  else if (beta >= 1) phi = PI/2;
          else phi = asin(beta);
          lam = x/COS_PHI1/Rg;
          *lat = deg(phi);
          *lon = deg(lam);
	}

	return(0);
}
