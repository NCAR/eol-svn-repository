C
C $Id: calcsm.f,v 1.1 1992/11/02 21:08:32 john Exp $
C
	REAL FUNCTION CDEWPT (TEMP,HUM)
	REAL TEMP,HUM
C
C Calculate Dew Point temperature from dry bulb temp and relative humidity
C References:
C 1.HP-BASIC Subroutine "Dewpoint" by Claude Morel, NCAR/ATD, Dec 1991
C 2.Bolton, David. "The Computation of Equivalent Potential
C   Temperature." Month. Weather Rev.: Vol. 108 (July 1980), page 1046.
C
C SVP = Saturation Vapor Pressure at 0 degrees Celsius
	REAL SVP
	PARAMETER (SVP=6.1121)
C WVP = Water Vapor Pressure
	REAL WVP
C DP = Dew Point
	REAL DP

	WVP = SVP*(HUM/100.0)*EXP(17.67*TEMP/(TEMP+243.5))
	DP = (243.5*LOG(WVP)-440.8)/(19.48-LOG(WVP))
	CDEWPT = DP
	RETURN
	END



	REAL FUNCTION CWNDIR (U,V)
	REAL U,V
C
C Calculate Wind Direction
C (From HP-BASIC Subroutine "Compmb", Label "Comp_dir" by Claude Morel
C
C SU,SV = Signs of U,V components
	INTEGER SU,SV
C DIR = Wind Direction
	REAL DIR
	REAL PI
	PARAMETER (PI=3.1415927)

	SU = INT(SIGN(1.0,U))
	SV = INT(SIGN(1.0,V))
	IF (U .EQ. 0.0) THEN
	  U = 0.000001
	END IF
	DIR = ABS(ATAN(V/U))
	DIR = DIR * 180.0 / PI
	IF ((SU .EQ. 1) .AND. (SV .EQ. 1)) THEN
	  DIR = 90.0 - DIR
	ELSE IF ((SU .EQ. -1) .AND. (SV .EQ. 1)) THEN
	  DIR = 270.0 + DIR
	ELSE IF ((SU .EQ. -1) .AND. (SV .EQ. -1)) THEN
	  DIR = 270.0 - DIR
	ELSE IF ((SU .EQ. 1) .AND. (SV .EQ. -1)) THEN
	  DIR = 90.0 + DIR
	END IF
	DIR = DIR + 180.0
	IF (DIR .GT. 360.0) THEN
	  DIR = DIR - 360.0
	END IF
	CWNDIR = DIR
	RETURN
	END
