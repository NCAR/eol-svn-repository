C
C $Id: read-file-descriptions.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: read-file-descriptions.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: read-file-descriptions
C  6 Character Name: RDFILD
C           Purpose: To read the descriptions of files from the control
C                    file.
C Import Parameters: (none)
C Export Parameters: (none)
C     Prerequisites: The control file must have been opened for reading
C                    and positioned correctly.
C Commons directly
C          accessed: CTLFIL
      SUBROUTINE RDFILD()
      INCLUDE '../../Commons/ctl_file.f'
C ====================================================================
C   Local variables
      LOGICAL FSE
C ====================================================================

      FSE = .FALSE.
      
C     while line-type=FILE do
 10   IF (LINCLS .EQ. FILECL) THEN
         FSE = .TRUE.
         CALL RD1FLD
         GO TO 10
      ENDIF

      IF (.NOT. FSE) THEN
         CALL ENCERR(35)
      ENDIF 

      RETURN
      END
