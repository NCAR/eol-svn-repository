C---------------------------------------------------------------------- 
C.KRK       
*
*  Version:     1.03
*   04/22/03/KRK -- revised RTERM 9-5 and 9-17 (for B.T. numbering)
*  Version:     1.02
*   04/14/03/KRK -- revised RTERM 9-17 (bump codes by 1)
*  Version:     1.01
*   11/27/02/KRK -- expands the table lookups to text
*  Version:     1.0
*   02/14/02/KRK -- original, from Jack Woollen's example
*
C.KRK    
C---------------------------------------------------------------------- 
      program dumpdata

      character*8 subset

      open( 8,file='rrs.bfr',form='unformatted')
      open(50,file='rrs.out',form='formatted')

      call openbf(8,'IN',8)
      do while(ireadmg(8,subset,idate).eq.0)
      do while(ireadsb(8).eq.0)
        call mydump(8,50)
      enddo
      enddo

      close(50)
      stop
      end
C
      SUBROUTINE MYDUMP(LUNIT,LUPRT)                                    
                                                                        
      PARAMETER (MAXJL=15000)
      PARAMETER (NF=2)
 
      COMMON /MSGCWD/ NMSG(NF),NSUB(NF),MSUB(NF),INODE(NF),IDATE(NF)    
      COMMON /TABLES/ MAXTAB,NTAB,TAG(MAXJL),TYP(MAXJL),KNT(MAXJL),     
     .                JUMP(MAXJL),LINK(MAXJL),JMPB(MAXJL),              
     .                IBT(MAXJL),IRF(MAXJL),ISC(MAXJL),                 
     .                ITP(MAXJL),VALI(MAXJL),KNTI(MAXJL),               
     .                ISEQ(MAXJL,2),JSEQ(MAXJL)                         
      COMMON /USRINT/ NVAL(NF),INV(MAXJL,NF),VAL(MAXJL,NF)              
      COMMON /TABABD/ NTBA(0:NF),NTBB(0:NF),NTBD(0:NF),MTAB(50,NF),
     .                IDNA(50,NF,2),IDNB(250,NF),IDND(250,NF),
     .                TABA(50,NF),TABB(250,NF),TABD(250,NF)
 
      CHARACTER*600 TABD
      CHARACTER*128 TABB
      CHARACTER*128 TABA

      CHARACTER*80 FMT
      CHARACTER*64 DESC
      CHARACTER*24 UNIT
      CHARACTER*20 LCHR
      CHARACTER*10 TAG,NEMO                                             
      CHARACTER*6  NUMB                                                 
      CHARACTER*8  CVAL,PMISS                                            
      CHARACTER*3  TYP
      CHARACTER*1  TAB
      CHARACTER*40 CODEVAL
      EQUIVALENCE  (RVAL,CVAL)
      REAL*8       VAL,RVAL,BMISS
                                                                        
      DATA BMISS /   10E10  /                                                
      DATA PMISS /' MISSING'/                                            
                                                                        
C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
                                                                        
      if(luprt.eq.0) luout = 6                                          
      if(luprt.ne.0) luout = luprt                                      
                                                                        
C  CHECK THE FILE STATUS AND I-NODE                                     
C  --------------------------------                                     
                                                                        
      CALL STATUS(LUNIT,LUN,IL,IM)                                      
      IF(IL.EQ.0) GOTO 900                                              
      IF(IM.EQ.0) GOTO 901                                              
      IF(INODE(LUN).NE.INV(1,LUN)) GOTO 902                             

      NEMO = TAG(INODE(LUN))
      IF     (NEMO.EQ.'NC002019') THEN
			DESC = 'RRS metadata'
      ELSEIF (NEMO.EQ.'NC002020') THEN
			DESC = 'RRS raw PTU'
      ELSEIF (NEMO.EQ.'NC002021') THEN
			DESC = 'RRS raw GPS unsmoothed'
      ELSEIF (NEMO.EQ.'NC002022') THEN
			DESC = 'RRS raw GPS smoothed'
      ELSEIF (NEMO.EQ.'NC002023') THEN
			DESC = 'RRS processed PTU'
      ELSEIF (NEMO.EQ.'NC002024') THEN
			DESC = 'RRS processed GPS'
      ELSEIF (NEMO.EQ.'NC002025') THEN
			DESC = 'RRS standard & significant levels'
      ELSE
			DESC = '?'
      ENDIF

      WRITE(LUOUT,*) 
      WRITE(LUOUT,*) 'MESSAGE TYPE ',trim(NEMO),' -- ',trim(DESC)
      WRITE(LUOUT,*) 

C  DUMP THE CONTENTS OF COMMON /USRINT/ FOR UNIT LUNIT                  
C  ---------------------------------------------------                  
                                                                        
      DO NV=1,NVAL(LUN)                                                 
         NODE = INV (NV,LUN)                                                 
         NEMO = TAG (NODE)                                                   
         ITYP = ITP (NODE)                                                   
         IF(ITYP.GE.1.AND.ITYP.LE.3) THEN                      
            CALL NEMTAB(LUN,NEMO,IDN,TAB,N)
            IF(TAB.NE.'B') CALL BORT('MYDUMP - BAD ITYP!')
            NUMB = TABB(N,LUN)(1:6)
            DESC = TABB(N,LUN)(16:70)
            UNIT = TABB(N,LUN)(71:94)
            RVAL = VAL(NV,LUN)                                                 
	     IDUM = MYLKUP (NEMO,RVAL,CODEVAL)
         ENDIF

         IF(ITYP.EQ.1.OR.ITYP.EQ.2) THEN                      
            IF(RVAL.LT.BMISS) THEN
               FMT = '(A6,2X,A10,2X,F20.00,2X,A13,2X,A38,2X,A)'
               WRITE(FMT(19:20),'(I2)') MAX(1,ISC(NODE))
               WRITE(LUOUT,FMT) NUMB,NEMO,RVAL,UNIT,DESC,trim(CODEVAL)
            ELSE
               FMT = '(A6,2X,A10,2X,A20,2X,A13,2X,A38,2X,A)'
               WRITE(LUOUT,FMT) NUMB,NEMO,PMISS,UNIT,DESC,trim(CODEVAL)
            ENDIF
         ELSEIF(ITYP.EQ.3) THEN                                            
            LCHR = ' '
            NCHR = IBT(NODE)/8
            IF(NCHR.GT.8) THEN
               CALL READLC(LUNIT,LCHR,NEMO)
            ELSE
               LCHR = CVAL
            ENDIF
            IRET = RJUST(LCHR)
            FMT = '(A6,2X,A10,2X,A20,2X,"(",i2,")",A9,2X,A38)'
            WRITE(LUOUT,FMT) NUMB,NEMO,LCHR,NCHR,UNIT,DESC
         ENDIF                                                             
      ENDDO                                                             
                                                                        
C  EXITS                                                                
C  -----                                                                
                                                                        
      RETURN                                                            
900   CALL BORT('MYDUMP - FILE IS CLOSED                     ')        
901   CALL BORT('MYDUMP - NO MESSAGE OPEN                    ')        
902   CALL BORT('MYDUMP - I-NODE MISMATCH                    ')        
      END                                                               
C
      FUNCTION MYLKUP(NEMO,RVAL,RESULT)
      CHARACTER*10  NEMO
      REAL*8        RVAL
      CHARACTER*40  RESULT
      INTEGER       IVAL

      MYLKUP = 0
      RESULT = ' '
      IVAL = RVAL

      IF     (NEMO.EQ.'RATP') THEN
         IF     (RVAL.EQ.55) THEN 
			RESULT = '9-1   Sippican GPS presumed'
         ELSEIF (RVAL.NE.55) THEN 
			RESULT = '9-1   InterMet GPS presumed'
	  ELSE        
	              RESULT = '9-1   ?'
         ENDIF
      ELSEIF (NEMO.EQ.'PSENS') THEN
         IF     (RVAL.EQ. 0) THEN 
			RESULT = '9-2   capacitance aneroid'
         ELSEIF (RVAL.EQ. 1) THEN 
			RESULT = '9-2   derived from GPS'
         ELSEIF (RVAL.EQ. 2) THEN 
			RESULT = '9-2   resistive strain gage'
	  ELSE        
	              RESULT = '9-2   ?'
         ENDIF
      ELSEIF (NEMO.EQ.'TSENS') THEN
         IF     (RVAL.EQ. 0) THEN 
			RESULT = '9-3   rod thermistor'
         ELSEIF (RVAL.EQ. 1) THEN 
			RESULT = '9-3   bead thermistor'
         ELSEIF (RVAL.EQ. 2) THEN 
			RESULT = '9-3   capacitance bead'
	  ELSE        
			RESULT = '9-3   ?'
         ENDIF
      ELSEIF (NEMO.EQ.'RHSENS') THEN
         IF     (RVAL.EQ. 1) THEN 
			RESULT = '9-4   VIZ Mark II carbon hygristor'
         ELSEIF (RVAL.EQ. 3) THEN 
			RESULT = '9-4   Vaisala H-Humicap'
         ELSEIF (RVAL.EQ. 4) THEN 
			RESULT = '9-4   Capacitance sensor'
         ELSEIF (RVAL.EQ. 6) THEN 
			RESULT = '9-4   Sippican Mark IIA carbon hyg'
	  ELSE        
			RESULT = '9-4   ?'
         ENDIF
      ELSEIF (NEMO.EQ.'RGRSY') THEN
         IF     (RVAL.EQ. 0) THEN 
			RESULT = '9-5   IMS-2000 (TRS)'
         ELSEIF (RVAL.EQ. 1) THEN 
			RESULT = '9-5   IMS-1500'
	  ELSE        
			RESULT = '9-5   ?'
         ENDIF
      ELSEIF (NEMO.EQ.'TTSS') THEN
         IF     (RVAL.EQ. 2) THEN 
			RESULT = '9-6   RDF'
         ELSEIF (RVAL.EQ. 8) THEN 
			RESULT = '9-6   GPS'
	  ELSE        
			RESULT = '9-6   ?'
         ENDIF
      ELSEIF (NEMO.EQ.'SFEQP') THEN
         IF     (RVAL.EQ. 0) THEN 
			RESULT = '9-7   PDB'
         ELSEIF (RVAL.EQ. 1) THEN 
			RESULT = '9-7   RSOIS'
         ELSEIF (RVAL.EQ. 2) THEN 
			RESULT = '9-7   ASOS'
         ELSEIF (RVAL.EQ. 3) THEN 
			RESULT = '9-7   Psychrometer'
         ELSEIF (RVAL.EQ. 4) THEN 
			RESULT = '9-7   F420'
	  ELSE        
			RESULT = '9-7   ?'
         ENDIF
      ELSEIF (NEMO.EQ.'BSHEL') THEN
         IF     (RVAL.EQ. 0) THEN 
			RESULT = '9-8   High bay'
         ELSEIF (RVAL.EQ. 1) THEN 
			RESULT = '9-8   Low bay'
         ELSEIF (RVAL.EQ. 2) THEN 
			RESULT = '9-8   BILS'
         ELSEIF (RVAL.EQ. 3) THEN 
			RESULT = '9-8   Roof-top BILS'
	  ELSE        
			RESULT = '9-8   ?'
         ENDIF
      ELSEIF (NEMO.EQ.'BMFGR') THEN
         IF     (RVAL.EQ. 0) THEN 
			RESULT = '9-9   Kaysam'
         ELSEIF (RVAL.EQ. 1) THEN 
			RESULT = '9-9   Totex'
         ELSEIF (RVAL.EQ. 2) THEN 
			RESULT = '9-9   KKS'
	  ELSE        
			RESULT = '9-9   ?'
         ENDIF
      ELSEIF (NEMO.EQ.'BTYPE') THEN
         IF     (RVAL.EQ. 0) THEN 
			RESULT = '9-10a GP26'
         ELSEIF (RVAL.EQ. 1) THEN 
			RESULT = '9-10a GP28'
         ELSEIF (RVAL.EQ. 2) THEN 
			RESULT = '9-10a GP30'
         ELSEIF (RVAL.EQ. 3) THEN 
			RESULT = '9-10a HM26'
         ELSEIF (RVAL.EQ. 4) THEN 
			RESULT = '9-10a HM28'
         ELSEIF (RVAL.EQ. 5) THEN 
			RESULT = '9-10a HM30'
         ELSEIF (RVAL.EQ. 6) THEN 
			RESULT = '9-10a SV16'
	  ELSE        
			RESULT = '9-10a ?'
         ENDIF
      ELSEIF (NEMO.EQ.'BGTYP') THEN
         IF     (RVAL.EQ. 0) THEN 
			RESULT = '9-10b hydrogen'
         ELSEIF (RVAL.EQ. 1) THEN 
			RESULT = '9-10b helium'
         ELSEIF (RVAL.EQ. 2) THEN 
			RESULT = '9-10b natural gas'
	  ELSE        
			RESULT = '9-10b ?'
         ENDIF
      ELSEIF (NEMO.EQ.'RCONF') THEN
         IF (IVAL.EQ.31) THEN
			RESULT = '9-10c ?'
	  ELSE
			RESULT = '9-10c'
            IF  (BTEST(IVAL,4)) THEN 
			RESULT = trim(RESULT) // ' TrReg'
	     ENDIF
            IF  (BTEST(IVAL,3)) THEN 
			RESULT = trim(RESULT) // ' LtUnit'
	     ENDIF
            IF  (BTEST(IVAL,2)) THEN 
			RESULT = trim(RESULT) // ' Parachute'
	     ENDIF
            IF  (BTEST(IVAL,1)) THEN 
			RESULT = trim(RESULT) // ' RooftopRel'
	     ENDIF
	  ENDIF
      ELSEIF (NEMO.EQ.'FLPC') THEN
         IF (IVAL.EQ.255) THEN
			RESULT = '9-11  ?'
	  ELSE
			RESULT = '9-11: '
            IF  (BTEST(IVAL,7)) THEN 
			RESULT = trim(RESULT) // ' Sm'
	     ENDIF
            IF  (BTEST(IVAL,6)) THEN 
			RESULT = trim(RESULT) // ' Adj'
	     ENDIF
            IF  (BTEST(IVAL,5)) THEN 
			RESULT = trim(RESULT) // ' Norm'
	     ENDIF
            IF  (BTEST(IVAL,4)) THEN 
			RESULT = trim(RESULT) // ' O.chk'
	     ENDIF
            IF  (BTEST(IVAL,3)) THEN 
			RESULT = trim(RESULT) // ' P.chk'
	     ENDIF
            IF  (BTEST(IVAL,2)) THEN 
			RESULT = trim(RESULT) // ' C.chk'
	     ENDIF
            IF  (BTEST(IVAL,1)) THEN 
			RESULT = trim(RESULT) // ' Inter'
	     ENDIF
	  ENDIF
      ELSEIF (NEMO.EQ.'SIRC') THEN
         IF     (RVAL.EQ. 0) THEN 
			RESULT = '9-13  no correction'
         ELSEIF (RVAL.EQ. 4) THEN 
			RESULT = '9-13  solar & infrared corrected'
         ELSEIF (RVAL.EQ. 5) THEN 
			RESULT = '9-13  solar corrected'
	  ELSE        
			RESULT = '9-13  ?'
         ENDIF
      ELSEIF (NEMO.EQ.'RTERM') THEN
         IF     (RVAL.EQ. 1) THEN 
			RESULT = '9-17  balloon burst'
         ELSEIF (RVAL.EQ. 2) THEN 
			RESULT = '9-17  balloon icing'
         ELSEIF (RVAL.EQ. 3) THEN 
			RESULT = '9-17  floating balloon'
         ELSEIF (RVAL.EQ. 4) THEN 
			RESULT = '9-17  weak signal'
         ELSEIF (RVAL.EQ. 5) THEN 
			RESULT = '9-17  battery'
         ELSEIF (RVAL.EQ. 6) THEN 
			RESULT = '9-17  ground equipment or sw'
         ELSEIF (RVAL.EQ. 7) THEN 
			RESULT = '9-17  signal interference'
         ELSEIF (RVAL.EQ. 8) THEN 
			RESULT = '9-17  radiosonde failure'
         ELSEIF (RVAL.EQ. 9) THEN 
			RESULT = '9-17  excessive missing data'
         ELSEIF (RVAL.EQ.10) THEN 
			RESULT = '9-17  ?'
         ELSEIF (RVAL.EQ.11) THEN 
			RESULT = '9-17  excessive missing P'
         ELSEIF (RVAL.EQ.12) THEN 
			RESULT = '9-17  excessive missing T'
         ELSEIF (RVAL.EQ.13) THEN 
			RESULT = '9-17  user terminated'
	  ELSE        
			RESULT = '9-17  ?'
         ENDIF
      ELSEIF (NEMO.EQ.'LEVSIG') THEN
         IF     (RVAL.EQ. 1) THEN 
			RESULT = '9-18  within 20hPa of surface'
         ELSEIF (RVAL.EQ. 2) THEN 
			RESULT = '9-18  P < 10hPa'
         ELSEIF (RVAL.EQ. 3) THEN 
			RESULT = '9-18  base pressure for SSI'
         ELSEIF (RVAL.EQ. 5) THEN 
			RESULT = '9-18  begin missing data'
         ELSEIF (RVAL.EQ. 8) THEN 
			RESULT = '9-18  highest level before descent'
         ELSEIF (RVAL.EQ.10) THEN 
			RESULT = '9-18  end missing data'
         ELSEIF (RVAL.EQ.13) THEN 
			RESULT = '9-18  freezing level'
         ELSEIF (RVAL.EQ.14) THEN 
			RESULT = '9-18  standard P'
         ELSEIF (RVAL.EQ.15) THEN 
			RESULT = '9-18  operator added level'
         ELSEIF (RVAL.EQ.16) THEN 
			RESULT = '9-18  operator deleted level'
         ELSEIF (RVAL.EQ.17) THEN 
			RESULT = '9-18  balloon re-ascended'
         ELSEIF (RVAL.EQ.18) THEN 
			RESULT = '9-18  sig RH'
         ELSEIF (RVAL.EQ.20) THEN 
			RESULT = '9-18  surface'
         ELSEIF (RVAL.EQ.21) THEN 
			RESULT = '9-18  sig T'
         ELSEIF (RVAL.EQ.23) THEN 
			RESULT = '9-18  flight termination'
         ELSEIF (RVAL.EQ.24) THEN 
			RESULT = '9-18  tropopause'
         ELSEIF (RVAL.EQ.28) THEN 
			RESULT = '9-18  mandatory wind'
         ELSEIF (RVAL.EQ.29) THEN 
			RESULT = '9-18  max wind'
         ELSEIF (RVAL.EQ.30) THEN 
			RESULT = '9-18  fixed regional wind'
         ELSEIF (RVAL.EQ.32) THEN 
			RESULT = '9-18  wind termination'
         ELSEIF (RVAL.EQ.33) THEN 
			RESULT = '9-18  P 100-110hPa'
         ELSEIF (RVAL.EQ.40) THEN 
			RESULT = '9-18  inversion'
         ELSEIF (RVAL.EQ.41) THEN 
			RESULT = '9-18  sig RH (NCDC)'
         ELSEIF (RVAL.EQ.42) THEN 
			RESULT = '9-18  sig T  (NCDC)'
	  ELSE        
			RESULT = '9-18  ?'
         ENDIF
      ELSEIF (NEMO.EQ.'MAQC') THEN
         IF     (RVAL.EQ. 0) THEN 
			RESULT = '9-19  pass, not manually checked'
         ELSEIF (RVAL.EQ. 1) THEN 
			RESULT = '9-19  pass, manual pass'
         ELSEIF (RVAL.EQ. 2) THEN 
			RESULT = '9-19  pass, manually deleted'
         ELSEIF (RVAL.EQ. 3) THEN 
			RESULT = '9-19  fail, not manually checked'
         ELSEIF (RVAL.EQ. 4) THEN 
			RESULT = '9-19  fail, manually deleted'
         ELSEIF (RVAL.EQ. 5) THEN 
			RESULT = '9-19  fail, manually re-inserted'
         ELSEIF (RVAL.EQ. 6) THEN 
			RESULT = '9-19  questionable, not manually checked'
         ELSEIF (RVAL.EQ. 7) THEN 
			RESULT = '9-19  questionable, manually deleted'
         ELSEIF (RVAL.LT.15) THEN 
			RESULT = '9-19  ?'
	  ELSE        
			RESULT = '9-19  missing data'
         ENDIF
      ELSEIF (NEMO.EQ.'QCCHEK') THEN
         IF     (RVAL.EQ. 0) THEN 
			RESULT = '9-20  passed all checks'
         ELSEIF (RVAL.EQ. 1) THEN 
			RESULT = '9-20  missing data'
         ELSEIF (RVAL.EQ. 2) THEN 
			RESULT = '9-20  descend/reascend balloon check'
         ELSEIF (RVAL.EQ. 3) THEN 
			RESULT = '9-20  plausibility (above limit) check'
         ELSEIF (RVAL.EQ. 4) THEN 
			RESULT = '9-20  plausibility (below limit) check'
         ELSEIF (RVAL.EQ. 5) THEN 
			RESULT = '9-20  superadiabatic lapse rate check'
         ELSEIF (RVAL.EQ. 6) THEN 
			RESULT = '9-20  limiting angles check'
         ELSEIF (RVAL.EQ. 7) THEN 
			RESULT = '9-20  ascension rate check'
         ELSEIF (RVAL.EQ. 8) THEN 
			RESULT = '9-20  excessive change from prev flight check'
         ELSEIF (RVAL.EQ. 9) THEN 
			RESULT = '9-20  balloon overhead check'
         ELSEIF (RVAL.EQ.10) THEN 
			RESULT = '9-20  wind speed check'
         ELSEIF (RVAL.EQ.11) THEN 
			RESULT = '9-20  wind direction check'
         ELSEIF (RVAL.EQ.12) THEN 
			RESULT = '9-20  dependency/inconsistency check'
         ELSEIF (RVAL.EQ.13) THEN 
			RESULT = '9-20  data valid but modified flag'
         ELSEIF (RVAL.EQ.14) THEN 
			RESULT = '9-20  outlier check'
	  ELSE        
			RESULT = '9-20  ?'
         ENDIF
      ELSEIF (NEMO.EQ.'DATSIG') THEN
         IF     (RVAL.EQ. 0) THEN 
			RESULT = '9-21  parent/WFO site'
         ELSEIF (RVAL.EQ. 1) THEN 
			RESULT = '9-21  RRS obs site'
         ELSEIF (RVAL.EQ. 2) THEN 
			RESULT = '9-21  balloon manufacture date'
         ELSEIF (RVAL.EQ. 3) THEN 
			RESULT = '9-21  balloon launch point'
         ELSEIF (RVAL.EQ. 4) THEN 
			RESULT = '9-21  surface obs'
         ELSEIF (RVAL.EQ. 5) THEN 
			RESULT = '9-21  surface obs displacement'
         ELSEIF (RVAL.EQ. 6) THEN 
			RESULT = '9-21  flight level obs'
         ELSEIF (RVAL.EQ. 7) THEN 
			RESULT = '9-21  flight level termination'
	  ELSE        
			RESULT = '9-21  ?'
         ENDIF
      ENDIF

      RETURN
      END
