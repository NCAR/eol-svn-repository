c      qcfhdr.f - writes header lines for surface data qcf file
 
c      this subroutine writes 3 header records for the surface
c      data qcf file
 
c      calls to this subroutine look like the next line
 
c      call qcfhdr ( fileno)

c      Required input: fortran file unit number for an already
c      opened file.
 
c      if this write fails, then a message will be written to
c      file unit 6 (standard out), and the program will stop
 
c      written by Ronald A. Murdock
 
c      Date of subroutine creation: 25MAR92
 
c      Dates and list of changes:
 
c      changed format to add variables on 06APR92
c      (added computed sea level pressure,present weather,
c       and three cloud cover layers)
c
c      changed format on 07APR92
c      added station elevation in meters

 
      subroutine qcfhdr ( fileno)
      integer fileno
      write(fileno, 1000, err=2000)
1000  format('Nominal  Time  Actual   Time  Network    Station         ',
     *'Latitude   Longitude   Occ Elev    STN     Q Sea Lvl Q ',
     *'Cmptd   Q Drybulb Q Dewpnt  Q Wind    ',
     *'Q Wind    Q Total   Q S Squall/ Q ',
     *'Pres Q Visib    Q Ceiling CF Q CA Q ',
     *'Ceiling CF Q CA Q Ceiling CF Q CA Q',
     */,
     *'Date     UTC   Date     UTC              ID              '
     *'                       ur          Press   F Press   F ',
     *'Sea Lvl F Temp    F Temp    F Speed   ',
     *'F Dir     F Prcp    F G Gust    F ',
     *'Wx   F          F Height1    F    F ',
     *'Height2    F    F Height3    F    F',
     */,
     *'******** ***** ******** ***** ********** *************** '
     *'********** *********** *** ******* ******* * ******* * ',
     *'******* * ******* * ******* * ******* ',
     *'* ******* * ******* * * ******* * ',
     *'**** * ******** * ******* ** * ** * ',
     *'******* ** * ** * ******* ** * ** *')

      goto 9999

2000  write(6,'('' failure to write header for qcf file'')' )
      stop

9999  continue
       return
       end
