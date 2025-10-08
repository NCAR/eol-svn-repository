      SUBROUTINE STAALT(STAPRS, PRSUNT, 
     *STAELV, ELVUNT, ALTPRS, DIRECT, LOGNUM)

C     SUBROUTINE TO CONVERT STATION PRESSURE TO ALTIMETER PRESSURE
C                  OR
C     CONVERT ALTIMETER PRESSURE TO STATION PRESSURE!!
C
C
C     INCOMING PARAMETERS
C
C     STAPRS - STATION PRESSURE
C     PRSUNT - PRESSURE UNITS WHERE
C              IN = INCHES OF MERCURY
C              MB = MILLIBARS
C     STAELV - STATION ELEVATION IN METERS
C     ELVUNT - ELEVATION UNITS WHERE 
C              FT = FEET    
C              ME = METERS
C     DIRECT - DIRECTION FOR CONVERSION
C              AS = ALTIMETER SETTING TO STATION PRESSURE
C              SA = STATION PRESSURE TO ALTIMETER SETTING
C
C     *************************************
C
C     INCOMING OR RETURNED PARAMETERS
C
C     NOTE: ONE IS INCOMING, THE OTHER IS RETURNED
C           CHOICE IS UP TO USER
C
C     STAPRS - STATION PRESSURE
C     
C     ALTPRS - ALTIMETER PRESSURE
C              UNITS ARE THE SAME AS INCOMING STATION PRESSURE
C
C     ****************************************

c     27apr92 - commented out most messages except errors
 
C     ****************************************

      CHARACTER*2 PRSUNT
      CHARACTER*2 ELVUNT
      CHARACTER*2 DIRECT

      REAL     ALTPRS
      REAL     STAELV
      REAL     STAPRS

      real*8  Amb, Ain, pmb, pin
      real*8  F,p0,p1,a,T0,Hb,n
      REAL*8  SEM, SEF, FTM, MTF
      REAL*8  in2mb,mb2in

      INTEGER LOGNUM

C**************************************
C     ECHO THE INCOMING PARAMETERS
C**************************************
c     WRITE(LOGNUM,'(/)' )
c     WRITE(LOGNUM,'('' **  ENTERING STAALT SUBROUTINE **'')')
c     WRITE(LOGNUM,'('' ALTPRS = '',F5.2)' ) ALTPRS
c     WRITE(LOGNUM,'('' PRSUNT = '',A2)' ) PRSUNT
c     WRITE(LOGNUM,'('' STAELV = '',F7.1)' ) STAELV
c     WRITE(LOGNUM,'('' ELVUNT = '',A2)' ) ELVUNT
c     WRITE(LOGNUM,'('' DIRECT = '',A2)' ) DIRECT

c     Write(6, '('' Entering STAALT '')')
C**************************************
C     CONVERSION FACTORS
C**************************************
C     FTM :  1 foot = 0.3048 meters
      FTM = 0.3048

C     MTF : 1 meter = 3.2808399 feet
      MTF = 3.2808399

C     in2mb = inches mercury to millibars conversion
C     1 inch = 33.86389 millibars
      in2mb = 33.86389

C     mb2in = millibars to inches of mercury conversion
c     1 millibar = 0.02952998 inch of mercury
      mb2in = 0.02952998

C     SEF = STATION ELEVATION IN FEET
C     SEM = STATION ELEVATION IN METERS
C     FTM = FEET TO METERS CONVERSION
C     MTF = METERS TO FEET CONVERSION
C**************************************


C     INFORMATION REGARDING THE STATION PRESSURE TO ALTIMETER
C     SETTING CONVERSION
C
C     The altimeter setting in millibars, Amb, is given by:
C                Amb = (pmb - 0.3)F

C     and in inches of mercury, Ain, by:
C                Ain = (pin - 0.01)F
C
C  where:
C     pmb= station pressure in millibars
C     pin= station pressure in inches of mercury
C     F  = altimeter setting computation factor
C
C     F IS A DIMENSIONLESS FACTOR GIVEN BY THE FOLLOWING EQUATION,
C     VALID ONLY BELOW THE HEIGHT OF THE STANDARD TROPOPAUSE.
C
C     F DEPENDS ONLY ON THE STATION PRESSURE p AND THE STATION
C     ELEVATION Hb.
C


C     F = [ 1 + ( ( ( (p0**n)* a ) / T0 )*( Hb / (p1**n) ) ) ] ** (1/n)
C
C where:
C     p0 = standard sea level pressure(1013.25 mb. or 29.921 in. Hg.)
C     p1 = (pmb - 0.3) when p0=1013.25 mb
C        = (pin - 0.01) when p0=29.921 in Hg.
C     a  = lapse rate in NACA standard atmosphere below the iosthermal
C          layer (0.0065 degrees C. per meter)
C     T0 = standard sea level temperature (288 degrees A)
C     Hb = station elevation in meters (elevation for which station
C          pressures are given)
C      n = aR = 0.190284, where R is the gas constant for dry air
C
C  REFERENCE: SMITHSONIAN METEROLOGICAL TABLES, PAGE 269
C             ALTIMETER SETTING COMPUTATION FACTORS



C     pin = (((Ain**n)-((((p0**n)*a)/T0)*(Hb)))**(1/n))+0.01


C     CHECK THE INCOMING UNITS

      IF ( PRSUNT .EQ. 'IN' .OR. PRSUNT .EQ. 'MB' ) THEN
         CONTINUE
      ELSE
C23456789+123456789+123456789+123456789+123456789+123456789+123456789+12
C*****!*****************************************************************
         WRITE( 6, '(''ERROR FROM STAALT SUBROUTINE'')' )
         WRITE( 6, '(''ERROR ON PRESSURE UNIT PARAMETER'')' )
         WRITE( 6, '(''PRESSURE MUST BE EITHER IN OR MB'')' )
         WRITE( 6, '(''PRSUNT IS '',A2)' ) PRSUNT
         STOP 1
      END IF

      IF ( ELVUNT .EQ. 'FT' .OR. ELVUNT .EQ. 'ME' ) THEN
         CONTINUE
      ELSE
         WRITE( 6, '(''ERROR FROM STAALT SUBROUTINE'')' )
         WRITE( 6, '(''ERROR ON ELEVATION UNIT PARAMETER'')' )
         WRITE( 6, '(''ELEVATION MUST BE EITHER FT OR ME'')' )
         WRITE( 6, '(''ELVUNT IS '',A2)' ) ELVUNT
         STOP 1
      END IF

      IF ( DIRECT .EQ. 'SA' .OR. DIRECT .EQ. 'AS' ) THEN
         CONTINUE
      ELSE
         WRITE( 6, '(''ERROR FROM STAALT SUBROUTINE'')' )
         WRITE( 6, '(''ERROR ON DIRECTION PARAMETER'')' )
         WRITE( 6, '(''DIRECTION MUST BE EITHER SA OR AS'')' )
         WRITE( 6, '(''DIRECT IS '',A2)' ) DIRECT
         STOP 1
      END IF



C*****!*****************************************************************
C     SET THE CONSTANTS HERE
C*****!*****************************************************************

      p0 = 29.92126
C     p0 = 1013.25  (millibars)
C     p0 = 29.92126 (inches Hg.)
      a  = 0.0065
      T0 = 288.0
      n  = 0.190284


C*****!*****************************************************************
C     SET THE STATION ELEVATION IN METERS
C*****!*****************************************************************
      IF ( ELVUNT .EQ. 'FT' ) THEN
         HB = STAELV * FTM
         SEF = STAELV
         SEM = SEF * FTM
      ELSE
         HB = STAELV
         SEM = HB
         SEF = SEM * MTF
      END IF
c     write ( 6, '(''Station elevation is '',F9.4,'' feet or '',f9.4,
c    *'' meters'')' ) SEF, SEM
c     WRITE ( LOGNUM, '('' STATION ELEVATION IS '',F9.4,'' FEET OR '',
c    *F9.4,'' METERS'')' ) SEF, SEM


C*****!*****************************************************************
C     DETERMINE WHICH CONVERSION IS WANTED
C*****!*****************************************************************

      IF ( DIRECT .EQ. 'SA' ) THEN
C        STATION PRESSURE TO ALTIMETER SETTING IS WANTED
         CONTINUE
      ELSE
C        ALTIMETER SETTING TO STATION PRESSURE IS WANTED
         GOTO 2000
      END IF


      IF ( PRSUNT .EQ. 'IN' ) THEN
         pin = STAPRS
         pmb = pin * in2mb
      ELSE
         pmb = STAPRS
         pin = pmb * mb2in
      END IF




      p1 = pin - 0.01


c     write ( 6, '(/,'' Station pressure is '',F8.5,
c    *'' inches Hg. or '',f10.5,'' millibars.'')' ) PIN, pmb

      F = ( 1 + ( (((p0**n)* a) / T0)*( Hb / (p1**n) ) ) )**(1/n)
c     write(6,'(//,
c    *'' ALTIMETER SETTING COMPUTATION FACTOR F = '',F8.6)' ) f

      Ain = (pin - 0.01) * F


      Amb = Ain * in2mb
c     write(6,'(/'' Altimeter setting is '',
c    *f8.5,'' inches Hg. or '',f10.5,'' millibars.'')' ) Ain, Amb

      IF ( PRSUNT .EQ. 'IN' ) THEN
         ALTPRS = AIN
c        WRITE(6,'(''RETURNING ALTIMETER PRESSURE OF '',F8.5)' ) ALTPRS
      ELSE
         ALTPRS = AMB
c        WRITE(6,'(''RETURNING ALTIMETER PRESSURE OF '',F8.5)' ) ALTPRS
      END IF

c     write ( 6, '(//,''**********'')' )
C23456789+123456789+123456789+123456789+123456789+123456789+123456789+12
c     write ( 6,
c    *'(''Alternative computations without correction to pressure:'')' )

C     p1 = pin
C     F = ( 1 + ( (((p0**n)* a) / T0)*( Hb / (p1**n) ) ) )**(1/n)
C     write(6,'(/,
C    *'' ALTIMETER SETTING COMPUTATION FACTOR F = '',F8.6)' ) f

C     Ain = (pin ) * F
C     Amb = Ain * in2mb
C     write ( 6, '(/,
C    *'' Altimeter setting without correction is '',F8.5,
C    *'' inches Hg. or '',f10.5,'' millibars'')' ) AIN,Amb
      GOTO 9999




2000  CONTINUE

C*****!*****************************************************************
C     CONVERT FROM ALTIMETER SETTING TO STATION PRESSURE
C*****!*****************************************************************


      IF ( PRSUNT .EQ. 'IN' ) THEN
         Ain = ALTPRS
         Amb = Ain * in2mb
      ELSE
         Amb = ALTPRS
         Ain = Amb * mb2in
      END IF

c     write(LOGNUM,'(/'' Altimeter setting is '',
c    *f8.5,'' inches Hg. or '',f10.5,'' millibars.'')' ) Ain, Amb


C     calculate the station pressure now
      pin = (((Ain**n)-((((p0**n)*a)/T0)*(Hb)))**(1/n))+0.01


      IF ( PRSUNT .EQ. 'IN' ) THEN
         STAPRS = PIN
c        WRITE(LOGNUM,
c    *'(''RETURNING STATION PRESSURE OF '',
c    *F10.5,'' inches'')' ) STAPRS
      ELSE
         STAPRS = PIN * in2mb
c        WRITE(LOGNUM,
c    *'(''RETURNING STATION PRESSURE OF '',
c    *F10.5,'' mb'')' ) STAPRS
      END IF


9999  CONTINUE
c     WRITE(LOGNUM, '('' STAALT: LEAVING SUBROUTINE NOW'')' )
c     WRITE(LOGNUM,'(/)' )
      RETURN
      END
