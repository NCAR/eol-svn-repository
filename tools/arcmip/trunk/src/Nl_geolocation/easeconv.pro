;==========================================================================
; easeconv.pro - IDL routines for conversion of azimuthal 
;		equal-area and equal-area cylindrical grid coordinates
;
;	30-Jan.-1992 H.Maybee
;	20-Mar-1992 Ken Knowles  303-492-0644  knowles@kryos.colorado.edu
;       23-Sep-1993 MJ Brodzik   303-492-8263  brodzik@jokull.colorado.edu
;		rollover from nsm to ease
;		changes are: 40-20-10 km grids to 25-12.5 km grids
;       11-Oct-1993 MJB added complete comment lines
;$Log: easeconv.pro,v $
;Revision 1.8  1998/11/03  18:56:38  brodzik
;Changed use of "spherical earth" in documentation comments.
;
;Revision 1.7  1998/11/03  16:07:10  brodzik
;Fixed bug in ease_inverse that was exiting in error
;for any SSM/I grid.
;
;Revision 1.6  1998/11/02  22:36:44  brodzik
;Fixed actual grid boundaries to cols/rows - (not +) 0.5.
;
;Revision 1.5  1998/10/28  21:02:28  brodzik
;Modified for more than SSM/I grids.  Now works for
;AVHRR Polar Pathfinder grids, TOVS Path P, and AARI.
;
;Revision 1.4  1998/10/28  15:41:49  brodzik
;Removed references to prototype grids (c,f).
;
;Revision 1.3  1998/10/28  15:35:49  brodzik
;Fixed comments for use by mk_html_help.
;
;Revision 1.2  1994/09/11  20:36:27  brodzik
;Added new grid definitions, for l, h, and removed references
;to b (100km) grid.
;
;Revision 1.1  1994/09/11  20:07:47  brodzik
;Initial revision
;
;
;==========================================================================
forward_function ssmi_convert
forward_function ssmi_inverse
forward_function ease_convert
forward_function ease_inverse

;+
; NAME:
;	ssmi_convert
;
; PURPOSE:
;	Use a spherical earth model to convert geographic coordinates to
;	azimuthal equal-area or equal-area cylindrical grid coordinates
;	(for SSM/I EASE-Grids, 25 km, 12.5 km)
;
; CATEGORY:
;	Grid coordinate conversion
;
; CALLING SEQUENCE:
;       status = ssmi_convert ( grid, lat, lon, r, s)
;
; INPUTS:
;       grid: projection name '[NSM][lh]'
;             (where l="low" res (25km), h="high" res (12.5km)
;       lat, lon - geo. coords. (decimal degrees)
;
; OUTPUTS:
;	r, s - column, row coordinates
;
; RESULT:
;	status = 0 indicates normal successful completion
;		-1 indicates error status (point not on grid)
;
; EXAMPLE:
;       status = ssmi_convert ('Nl',90.,0.,col,row)
;
;       status will be 0, and the returned col, row will be 360.0, 360.0
;
;-
FUNCTION ssmi_convert, grid, lat, lon, r, s

	on_error,2

	if n_params() lt 5 then begin
	  print,'usage:  status = ssmi_convert (grid, lat, lon, r, s)'
	  return, 1
	endif

	grid = string(grid)
        grid0 = strmid(grid,0,1)
	grid1 = strmid(grid,1,1)
        
	if (grid0 eq 'N') or (grid0 eq 'S') then begin
	  cols = 721
	  rows = 721
	endif else if grid0 eq 'M' then begin
	  cols = 1383
	  rows = 586
	endif else begin
	  message, 'unknown projection: ' + grid
	endelse

	if grid1 eq 'l' then scale = 1 $
	else if grid1 eq 'h' then scale = 2 $
	else message, 'unknown projection: ' + grid

;
;	radius of the earth (km), authalic sphere based on International datum 
	RE_km = 6371.228
;
;	nominal cell size in kilometers
	CELL_km = 25.067525

        Rg = scale * RE_km/CELL_km

;
;	scale factor for standard parallels at +/-30.00 degrees
;
	COS_PHI1 = .866025403

;
;	r0,s0 are defined such that cells at all scales
;	have coincident center points
;
        r0 = (cols-1)/2. * scale
        s0 = (rows-1)/2. * scale

	phi = lat * !PI/180.0
        lam = lon * !PI/180.0

	if grid0 eq 'N' then begin
	  rho = 2 * Rg * sin(!PI/4. - phi/2.)
	  r = r0 + rho * sin(lam)
	  s = s0 + rho * cos(lam)

	endif else if grid0 eq 'S' then begin
	  rho = 2 * Rg * cos(!PI/4. - phi/2.)
	  r = r0 + rho * sin(lam)
	  s = s0 - rho * cos(lam)

        endif else if grid0 eq 'M' then begin
          r = r0 + Rg * lam * COS_PHI1
          s = s0 - Rg * sin(phi) / COS_PHI1

	endif

	return, 0

END ; ssmi_convert

;+
; NAME:
;	ssmi_inverse
;
; PURPOSE:
;	Use a spherical earth model to convert azimuthal equal-area 
;       equal-area cylindrical grid coordinates to geographic coordinates
;	(for SSM/I EASE-Grids, 25 km, 12.5 km)
;
; CATEGORY:
;	Grid coordinate conversion
;
; CALLING SEQUENCE:
;       status = ssmi_inverse ( grid, r, s, lat, lon)
;
; INPUTS:
;       grid: projection name '[NSM][lh]'
;             (where l="low" res (25km), h="high" res (12.5km)
;	r, s - column, row coordinates
;
; OUTPUTS:
;       lat, lon - geo. coords. (decimal degrees)
;
; RESULT:
;	status = 0 indicates normal successful completion
;		-1 indicates error status (point not on grid)
;
; EXAMPLE:
;       status = ssmi_inverse ('Nl',360.0, 360.0, lat, lon)
;
;       status will be 0, and the returned lat, lon will be 90.0, 0.0
;
;-
FUNCTION ssmi_inverse, grid, r, s, lat, lon

	on_error,2

	if n_params() lt 5 then begin
	  print,'usage:  status = ssmi_inverse (grid, r, s, lat, lon)'
	  return, 1
	endif

	grid = string(grid)
        grid0 = strmid(grid,0,1)
	grid1 = strmid(grid,1,1)
        
	if (grid0 eq 'N') or (grid0 eq 'S') then begin
	  cols = 721
	  rows = 721
	endif else if grid0 eq 'M' then begin
	  cols = 1383
	  rows = 586
	endif else begin
	  message, 'unknown projection: ' + grid
	endelse

	if grid1 eq 'l' then scale = 1 $
	else if grid1 eq 'h' then scale = 2 $
	else message, 'unknown projection: ' + grid

	RE_km = 6371.228
	CELL_km = 25.067525

        Rg = scale * RE_km/CELL_km

	COS_PHI1 = .866025403

        r0 = (cols-1)/2. * scale
        s0 = (rows-1)/2. * scale

        x = r - r0
        y = -(s - s0)

        if (grid0 eq 'N') or (grid0 eq 'S') then begin
          rho = sqrt(x*x + y*y)
          if rho eq 0.0 then begin
            if grid0 eq 'N' then lat = 90.0 
            if grid0 eq 'S' then lat = -90.0 
            lon = 0.0
	  endif else begin
            if grid0 eq 'N' then begin
              sinphi1 = sin(!PI/2.)
              cosphi1 = cos(!PI/2.)
              if y eq 0. then begin
                if r le r0 then lam = -!PI/2
                if r gt r0 then lam = !PI/2
              endif else begin
                lam = atan(x,-y)
	      endelse
            endif else if grid0 eq 'S' then begin
              sinphi1 = sin(-!PI/2.)
              cosphi1 = cos(-!PI/2.)
              if y eq 0. then begin
                if r le r0 then lam = -!PI/2
                if r gt r0 then lam = !PI/2
              endif else begin
                lam = atan(x,y)
	      endelse
     	    endif
            gamma = rho/(2 * Rg)
	    if abs(gamma) gt 1. then return, -1
            c = 2 * asin(gamma)
            beta = cos(c) * sinphi1 + y * sin(c) * (cosphi1/rho)
	    if abs(beta) gt 1. then return, -1
            phi = asin(beta)
            lat = phi * 180/!PI
            lon = lam * 180/!PI	
          endelse

	endif else if grid0 eq 'M' then begin
;
;	  allow .5 cell tolerance in arcsin function
;	  so that grid coordinates which are less than .5 cells
;	  above 90.00N or below 90.00S are given a lat of 90.00
;
	  epsilon = 1 + 0.5/Rg
          beta = y*COS_PHI1/Rg
          if abs(beta) gt epsilon then return, -1 $
	  else if beta le -1. then phi = -!PI/2 $
	  else if beta ge 1. then phi = !PI/2 $
          else phi = asin(beta)
          lam = x/COS_PHI1/Rg
          lat = phi * 180/!PI
          lon = lam * 180/!PI	
	endif

	return, 0

END ; ssmi_inverse

;+
; NAME:
;	ease_convert
;
; PURPOSE:
;	Use a spherical earth model to convert geographic coordinates to
;	azimuthal equal-area or equal-area cylindrical grid coordinates
;       for various EASE-Grids.
;
; CATEGORY:
;	Grid coordinate conversion
;
; CALLING SEQUENCE:
;       status = ease_convert ( grid, lat, lon, r, s)
;
; INPUTS:
;       grid: projection name 
;             SSM/I Polar Pathfinder: [NSM][lh]
;                (where l="low" res (25km), h="high" res (12.5km))
;             AVHRR Polar Pathfinder: [NS]a{1,5,25}
;                (where 1=1.25 km res, 5=5 km res, 25=25 km res)
;             TOVS-P Polar Pathfinder: NpathP (100km res)
;             AARI sea ice: AARI (12.5 km res)
;       lat, lon - geo. coords. (decimal degrees)
;
; OUTPUTS:
;	r, s - column, row coordinates
;
; RESULT:
;	status = 0 indicates normal successful completion
;		-1 indicates error status (point not on grid, unknown
;                  projection)
;
; EXAMPLE:
;       status = ease_convert ('Nl',90.,0.,col,row)
;
;       status will be 0, and the returned col, row will be 360.0, 360.0
;
; NOTES:
;       Please see 
;       http://www-nsidc.colorado.edu/NASA/GUIDE/EASE/ease_maps_info.html
;       http://www-nsidc.colorado.edu/NASA/POLAR_PATHFINDERS/polarpf_grids.html
;
;-
FUNCTION ease_convert, grid, lat, lon, r, s

on_error,2

if n_params() lt 5 then begin
    print,'usage:  status = ease_convert (grid, lat, lon, r, s)'
    return, 1
endif

grids =     ['NL','SL','ML','NH','SH','MH',$
             'NA25','SA25','NA5','SA5','NA1','SA1','NPATHP','AARI']
ssmi_grid = ['Nl','Sl','Ml','Nh','Sh','Mh',$
             'Nl'  ,'Sl'  ,'Nl' ,'Sl ','Nl'  ,'Sl'  ,'Nl'    ,'Nh']

; Check for SSM/I grids
ingrid = strupcase(string(grid))
idx = where(ingrid eq grids,count)
if 1 ne count then begin
    message, 'unknown grid: ' + grid,/info
    return,-1
endif

; Look up the coordinates of the corresponding SSM/I grid
status = ssmi_convert(ssmi_grid(idx(0)), lat, lon, r, s)

; Return immediately if it is an SSM/I grid
if 6 gt idx(0) then return,status

; Otherwise, convert coordinates
case ingrid of

    'NA25': begin
        cols = 361
        rows = 361
        factor = 1.
        offset = -180.
    end

    'NA5': begin
        cols = 1805
        rows = 1805
        factor = 5.
        offset = -898.
    end

    'NA1': begin
        cols = 7220
        rows = 7220
        factor = 20.
        offset = -3590.5
    end

    'SA25': begin
        cols = 321
        rows = 321
        factor = 1.
        offset = -200.
    end

    'SA5': begin
        cols = 1605
        rows = 1605
        factor = 5.
        offset = -998.
    end

    'SA1': begin
        cols = 6420
        rows = 6420
        factor = 20.
        offset = -3990.5
    end

    'NPATHP': begin
        cols = 67
        rows = 67
        factor = 0.25
        offset = -57.
    end

    'AARI': begin
        cols = 721
        rows = 721
        factor = 1.
        offset = -360.
    end

    else: message,'unrecognized grid ' + string(grid)

endcase

r = (r * factor) + offset
s = (s * factor) + offset

; Check for actual grid boundaries
if r lt -0.5 or r ge (cols - 0.5) or $
  s lt -0.5 or s ge (rows - 0.5) then status = -1

return,status

end
        
            
;+
; NAME:
;	ease_inverse
;
; PURPOSE:
;	Use a spherical earth model to convert azimuthal equal-area 
;       or equal-area cylindrical grid coordinates to geographic
;       coordinates for various EASE-Grids
;
; CATEGORY:
;	Grid coordinate conversion
;
; CALLING SEQUENCE:
;       status = ease_inverse ( grid, r, s, lat, lon)
;
; INPUTS:
;       grid: projection name 
;             SSM/I Polar Pathfinder: [NSM][lh]
;                (where l="low" res (25km), h="high" res (12.5km))
;             AVHRR Polar Pathfinder: [NS]a{1,5,25}
;                (where 1=1.25 km res, 5=5 km res, 25=25 km res)
;             TOVS-P Polar Pathfinder: NpathP (100km res)
;             AARI sea ice: AARI (12.5 km res)
;	r, s - column, row coordinates
;
; OUTPUTS:
;       lat, lon - geo. coords. (decimal degrees)
;
; RESULT:
;	status = 0 indicates normal successful completion
;		-1 indicates error status (point not on grid)
;
; EXAMPLE:
;       status = ease_inverse ('Nl',360.0, 360.0, lat, lon)
;
;       status will be 0, and the returned lat, lon will be 90.0, 0.0
;
;-
FUNCTION ease_inverse, grid, r, s, lat, lon

on_error,2

if n_params() lt 5 then begin
    print,'usage:  status = ease_inverse (grid, r, s, lat, lon)'
    return, 1
endif

grids =     ['NL','SL','ML','NH','SH','MH',$
             'NA25','SA25','NA5','SA5','NA1','SA1','NPATHP','AARI']
ssmi_grid = ['Nl','Sl','Ml','Nh','Sh','Mh',$
             'Nl'  ,'Sl'  ,'Nl' ,'Sl ','Nl'  ,'Sl'  ,'Nl'    ,'Nh']

; Check for SSM/I grids
ingrid = strupcase(string(grid))
idx = where(ingrid eq grids,count)
if 1 ne count then begin
    message, 'unknown grid: ' + grid,/info
    return,-1
endif

; Convert input col,row from other grid to SSM/I grid
case ingrid of

    'NA25': begin
        cols = 361
        rows = 361
        factor = 1.
        offset = 180.
    end

    'NA5': begin
        cols = 1805
        rows = 1805
        factor = 0.2
        offset = 179.6
    end

    'NA1': begin
        cols = 7220
        rows = 7220
        factor = 0.05
        offset = 179.525
    end

    'SA25': begin
        cols = 321
        rows = 321
        factor = 1.
        offset = 200.
    end

    'SA5': begin
        cols = 1605
        rows = 1605
        factor = 0.2
        offset = 199.6
    end

    'SA1': begin
        cols = 6420
        rows = 6420
        factor = 0.05
        offset = 199.525
    end

    'NPATHP': begin
        cols = 67
        rows = 67
        factor = 4.
        offset = 228.
    end

    'AARI': begin
        cols = 721
        rows = 721
        factor = 1.
        offset = 360.
    end

    else: begin
        factor = 1.
        offset = 0.
    end

endcase

ssmi_r = (r * factor) + offset
ssmi_s = (s * factor) + offset

; Look up the coordinates of the corresponding SSM/I grid
status = ssmi_inverse(ssmi_grid(idx(0)), ssmi_r, ssmi_s, lat, lon)

; Return immediately if it is an SSM/I grid
if 6 gt idx(0) then return,status

; Check for actual grid boundaries
if r lt -0.5 or r ge (cols - 0.5) or $
  s lt -0.5 or s ge (rows - 0.5) then status = -1

return,status

end

