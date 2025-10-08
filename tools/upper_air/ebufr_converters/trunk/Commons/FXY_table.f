C
C $Id: FXY_table.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: FXY_table.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C**********************************************************************
C
C  Parameter information deducible from BUFR tables
C
C  Converted to standard ANSII 17 Sep 1991.

C     integer f(maxfxy)         ! F value for parameter
C     integer x(maxfxy)         ! X value for parameter
C     integer y(maxfxy)         ! Y value for parameter
C     integer scale(maxfxy)     ! scale factor for parameter
C     integer refval(maxfxy)    ! reference value for parameter
C     integer btwdth(maxfxy)    ! bit width for parameter storage
C     integer fxytop            ! Total number of (f,x,y) values so far
C     logical ctable(maxfxy)    ! This parameter has a code table value
C     logical ftable(maxfxy)    ! This parameter has a flag table value
C     logical markd(maxfxy)     ! For use by write header routines

      INTEGER F(MAXFXY)
      INTEGER X(MAXFXY)
      INTEGER Y(MAXFXY)
      INTEGER SCALE(MAXFXY)
      INTEGER REFVAL(MAXFXY)
      INTEGER BTWDTH(MAXFXY)
      INTEGER FXYTOP
      LOGICAL CTABLE(MAXFXY)
      LOGICAL FTABLE(MAXFXY)
      LOGICAL MARKD(MAXFXY)
      COMMON /FXYTAB/ F,X,Y,SCALE,REFVAL,BTWDTH, FXYTOP,
     $                CTABLE, FTABLE, MARKD
      SAVE /FXYTAB/
