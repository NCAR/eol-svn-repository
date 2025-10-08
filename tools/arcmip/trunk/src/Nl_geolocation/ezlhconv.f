C==========================================================================
C ezlhconv.for - FORTRAN routines for conversion of azimuthal 
C		equal area and equal area cylindrical grid coordinates
C
C	30-Jan.-1992 H.Maybee
C	20-Mar-1992 Ken Knowles  303-492-0644  knowles@kryos.colorado.edu
C       16-Dec-1993 MJ Brodzik   303-492-8263  brodzik@jokull.colorado.edu
C                   Copied from nsmconv.f, changed resolutions from 
C                   40-20-10 km to 25-12.5 km
C       21-Dec-1993 MJ Brodzik   303-492-8263  brodzik@jokull.colorado.edu
C                   Fixed sign of Southern latitudes in ease_inverse.
C	12-Sep-1994 David Hoogstrate 303-492-4116 hoogstra@jokull.colorado.edu
C		    Changed grid cell size. Changed "c","f" to "l","h"
C	25-Oct-1994 David Hoogstrate 303-492-4116 hoogstra@jokull.colorado.edu
C		    Changed row size from 587 to 586 for Mercator projection
C		    Changed function names to "ezlh-.."
C$Log: ezlhconv.f,v $
CRevision 1.3  1994/11/01 23:40:43  brodzik
CReplaced all references to 'ease' with 'ezlh'
C
C==========================================================================

C--------------------------------------------------------------------------
	function ezlh_convert (grid, lat, lon, r, s)
	character*(*) grid
	real lat, lon, r, s
C
C	convert geographic coordinates (spherical earth) to 
C	azimuthal equal area or equal area cylindrical grid coordinates
C
C	status = ezlh_convert (grid, lat, lon, r, s)
C
C	input : grid - projection name '[NSM][lh]'
C               where l = "low"  = 25km resolution
C                     h = "high" = 12.5km resolution
C		lat, lon - geo. coords. (decimal degrees)
C
C	output: r, s - column, row coordinates
C
C	result: status = 0 indicates normal successful completion
C			-1 indicates error status (point not on grid)
C
C--------------------------------------------------------------------------
        integer cols, rows, scale
	real Rg, phi, lam, rho

C	radius of the earth (km), authalic sphere based on International datum 
	parameter (RE_km = 6371.228)
C	nominal cell size in kilometers
	parameter (CELL_km = 25.067525)

C	scale factor for standard paralles at +/-30.00 degrees
	parameter (COS_PHI1 = .866025403)

	parameter (PI = 3.141592653589793)
	rad(t) = t*PI/180.
	deg(t) = t*180./PI

	ezlh_convert = -1

	if ((grid(1:1).eq.'N').or.(grid(1:1).eq.'S')) then
	  cols = 721
	  rows = 721
	else if (grid(1:1).eq.'M') then
	  cols = 1383
	  rows = 586
	else
	  print *, 'ezlh_convert: unknown projection: ', grid
	  return
	endif

	if (grid(2:2).eq.'l') then
	  scale = 1
	else if (grid(2:2).eq.'h') then
	  scale = 2
	else
	  print *, 'ezlh_convert: unknown projection: ', grid
	  return
	endif

        Rg = scale * RE_km/CELL_km

C
C	r0,s0 are defined such that cells at all scales
C	have coincident center points
C
        r0 = (cols-1)/2. * scale
        s0 = (rows-1)/2. * scale

	phi = rad(lat)
        lam = rad(lon)

	if (grid(1:1).eq.'N') then
	  rho = 2 * Rg * sin(PI/4. - phi/2.)
	  r = r0 + rho * sin(lam)
	  s = s0 + rho * cos(lam)

	else if (grid(1:1).eq.'S') then
	  rho = 2 * Rg * cos(PI/4. - phi/2.)
	  r = r0 + rho * sin(lam)
	  s = s0 - rho * cos(lam)

        else if (grid(1:1).eq.'M') then
          r = r0 + Rg * lam * COS_PHI1
          s = s0 - Rg * sin(phi) / COS_PHI1

	endif

	ezlh_convert = 0
	return
	end

C--------------------------------------------------------------------------
	function ezlh_inverse (grid, r, s, lat, lon)
	character*(*) grid
	real r, s, lat, lon
C
C	convert azimuthal equal area or equal area cylindrical 
C	grid coordinates to geographic coordinates (spherical earth)
C
C	status = ezlh_inverse (grid, r, s, lat, lon)
C
C	input : grid - projection name '[NSM][lh]'
C               where l = "low"  = 25km resolution
C                     h = "high" = 12.5km resolution
C		r, s - grid column and row coordinates
C
C	output: lat, lon - geo. coords. (decimal degrees)
C
C	result: status = 0 indicates normal successful completion
C			-1 indicates error status (point not on grid)
C
C--------------------------------------------------------------------------
        integer cols, rows, scale
	real Rg, phi, lam, rho
	real gamma, beta, epsilon, x, y
	real sinphi1, cosphi1

C	radius of the earth (km), authalic sphere based on International datum 
	parameter (RE_km = 6371.228)
C	nominal cell size in kilometers
	parameter (CELL_km = 25.067525)

C       scale factor for standard paralles at +/-30.00 degrees
        parameter (COS_PHI1 = .866025403)

        parameter (PI = 3.141592653589793)
        rad(t) = t*PI/180.
        deg(t) = t*180./PI

	ezlh_inverse = -1

	if ((grid(1:1).eq.'N').or.(grid(1:1).eq.'S')) then
	  cols = 721
	  rows = 721
	else if (grid(1:1).eq.'M') then
	  cols = 1383
	  rows = 586
	else
	  print *, 'ezlh_inverse: unknown projection: ', grid
	  return
	endif

	if (grid(2:2).eq.'l') then
	  scale = 1
	else if (grid(2:2).eq.'h') then
	  scale = 2
	else
	  print *, 'ezlh_inverse: unknown projection: ', grid
	  return
	endif

        Rg = scale * RE_km/CELL_km

        r0 = (cols-1)/2. * scale
        s0 = (rows-1)/2. * scale

        x = r - r0
        y = -(s - s0)

        if ((grid(1:1).eq.'N').or.(grid(1:1).eq.'S')) then 
          rho = sqrt(x*x + y*y)
          if (rho.eq.0.0) then
            if (grid(1:1).eq.'N') lat = 90.0 
            if (grid(1:1).eq.'S') lat = -90.0 
            lon = 0.0
	  else
            if (grid(1:1).eq.'N') then
              sinphi1 = sin(PI/2.)
              cosphi1 = cos(PI/2.)
              if (y.eq.0.) then
                if (r.le.r0) lam = -PI/2.
                if (r.gt.r0) lam = PI/2.
              else
                lam = atan2(x,-y)
	      endif
            else if (grid(1:1).eq.'S') then
              sinphi1 = sin(-PI/2.)
              cosphi1 = cos(-PI/2.)
              if (y.eq.0.) then
                if (r.le.r0) lam = -PI/2.
                if (r.gt.r0) lam = PI/2.
              else
                lam = atan2(x,y)
	      endif
     	    endif
            gamma = rho/(2 * Rg)
	    if (abs(gamma).gt.1.) return
            c = 2 * asin(gamma)
            beta = cos(c) * sinphi1 + y * sin(c) * (cosphi1/rho)
	    if (abs(beta).gt.1.) return
            phi = asin(beta)
            lat = deg(phi)
            lon = deg(lam)
          endif

	else if (grid(1:1).eq.'M') then
C
C	  allow .5 cell tolerance in arcsin function
C	  so that grid coordinates which are less than .5 cells
C	  above 90.00N or below 90.00S are given a lat of 90.00
C
	  epsilon = 1 + 0.5/Rg
          beta = y*COS_PHI1/Rg
          if (abs(beta).gt.epsilon) return
	  if (beta.le.-1.) then
	    phi = -PI/2.
	  else if (beta.ge.1.) then
	    phi = PI/2.
          else
	    phi = asin(beta)
	  endif
          lam = x/COS_PHI1/Rg
          lat = deg(phi)
          lon = deg(lam)
	endif

	ezlh_inverse = 0
	return
	end



