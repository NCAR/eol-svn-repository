C Note - statn array, station identifier, isn't used in nh3e.f
C        After reading in the network ID, the s/w reads 15 characters
C        into an array named CDATA.  The QREC is ignored here. 
C        30 aug 96 ds 
C
C $Id: qcf.inc,v 1.1 1993/02/22 19:11:45 john Exp $
C
      structure /qcf/
         character*2 date(3)    !UTC date of observation
         character*2 qtime(2)   !UTC time of observation
         character*10 qnet      !network identifier (platform abbrev.)
         character*10 statn     !station identifier
         real ll(2)             !latitude,longitude of station
         integer occur          !station occurence
         real staelv            !station elevation in meters
         real staprs            !station pressure in mb
         character*1 staflg     !qc flag for station pressure
         real seaprs            !sea level pressure in mb
         character*1 seaflg     !qc flag for sea level pressure
         real cmpsea            !computed sea level pressure in mb
         character*1 cmpflg     !qc flag for computed sea level pressure
         real temp              !dry bulb temperature in Celsius
         character*1 tmpflg     !qc flag for dry bulb temperature
         real dewpnt            !dew point temperature in Celsius
         character*1 dewflg     !qc flag for dew point
         real wndspd            !wind speed in meters/second
         character*1 spdflg     !qc flag for wind speed
         real wnddir            !wind direction in degrees azimuth
         character*1 dirflg     !qc flag for wind direction
         real precip            !total precipitation in mm
         character*1 prcflg     !qc flag for total precipitation
         character*1 sg         !squall/gust indicator
         real squall            !squall/gust speed in meters/second
         character*1 sqlflg     !qc flag for squall/gust speed
         integer prswea         !present weather code value
         character*1 pwflg      !qc flag for present weather
         real visib             !visibility in meters
         character*1 visflg     !qc flag for visibility
         real celht1            !ceiling height (first layer)- hundreds of ft
         integer celfg1         !ceiling flag (first layer) code value
         character*1 c1flg      !qc flag for ceiling height/flag
         integer clamt1         !cloud amount (first layer) code value
         character*1 ca1flg     !qc flag for cloud amount
         real celht2            !ceiling height (secnd layer)- hundreds of ft
         integer celfg2         !ceiling flag (secnd layer) code value
         character*1 c2flg      !qc flag for ceiling height/flag
         integer clamt2         !cloud amount (secnd layer) code value
         character*1 ca2flg     !qc flag for cloud amount
         real celht3            !ceiling height (third layer)- hundreds of ft
         integer celfg3         !ceiling flag (third layer) code value
         character*1 c3flg      !qc flag for ceiling height/flag
         integer clamt3         !cloud amount (third layer) code value
         character*1 ca3flg     !qc flag for cloud amount
      end structure
