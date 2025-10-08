      SUBROUTINE CLSFIL(FILENO, FILNAM, IOS, LISNUM)
******!****************************************************************!
C     SUBROUTINE CLSFIL.F
C     
C     SUBROUTINE TO CLOSE FILES
C
C     WRITTEN BY:  RONALD A. MURDOCK
C                  MMM/NCAR
C                  DECEMBER 23, 1991
C
C     DATE LAST MODIFIED:  TODAY
C
C     HISTORY OF CHANGES:
C
C
******!****************************************************************!
C   +    1    +    2    +    3    +    4    +    5    +    6    +    7
C23456789012345678901234567890123456789012345678901234567890123456789012
******!****************************************************************!

      INTEGER   FILENO      !FORTRAN UNIT NUMBER FOR FILE
      INTEGER   LISNUM      !FORTRAN UNIT NUMBER FOR MESSAGES FROM HERE
      CHARACTER*(*) FILNAM  !FILE NAME
      INTEGER IOS           !RETURN CODE FLAG FOR I/O
      integer ierr
      integer fstat, statbuf(13)


C  ***!****************************************************************!
C
C     CLOSE FILE
C
C  ***!****************************************************************!

      ierr = fstat(fileno, statbuf)

c     if (statbuf(8) .eq. 0) then
c     CLOSE( UNIT=FILENO, status='delete', IOSTAT=IOS, ERR=2000 )
c     else
      CLOSE( UNIT=FILENO, IOSTAT=IOS, ERR=2000 )
c     end if

      WRITE( LISNUM, 1000) FILENO, FILNAM
1000  FORMAT(/,'CLSFIL:SUCCESSFULLY CLOSED UNIT ', I2, ' FOR FILE ', A )
      GOTO 9999

2000  WRITE( LISNUM, 3000) FILENO, FILNAM, IOS
3000  FORMAT(/,'CLSFIL:ERROR OCCURRED CLOSING UNIT ', I2, 
     *' FOR FILE ', A,
     */,'RETURN CODE WAS ',I4 )



9999  RETURN
      END
