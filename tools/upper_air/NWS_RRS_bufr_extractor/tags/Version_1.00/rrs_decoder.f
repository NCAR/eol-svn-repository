C-----------------------------------------------------------------------
C     This program was designed as a tool to assist users of the 
C     Radiosonde Replacement System's BUFR data to convert one or
C     more message types to text files.  
C
C     Credit goes to Jack Woollen of NCEP for the BUFR decoder
C     (bufr_1_5.f) and a skeleton program for this program.
C
C     The flow of this program is to open and read the BUFR data
C     record by record.  Each record is tested to determine its 
C     message type.  Once the message type is determined, an array
C     for that message type gets initiated (a19 - a25).  Once the
C     appropriate array gets initialized, subroutines are called to
C     decode each variable and perform any necessary conversions.
C
C     NCDC provides this program "as is" with no warranties or 
C     guarantees.
C-----------------------------------------------------------------------
      PROGRAM RRS_DECODER   

      implicit none

      INTEGER MAXARR, IRET
      PARAMETER (MAXARR=100)

      CHARACTER*8 SUBSET
      REAL*8 ARR(MAXARR)
      REAL*8 a19(96), a20(36), a21(38), a22(38), a23(47), 
     &       a24(37), a25(33)
c
c    NCDC ADDED VARIABLES...
      character unxbfr*60, outdir*60, of19*60, of20*60,
     &  of21*60, of22*60, of23*60, of24*60, of25*60
c
      integer narg, iargc, idate, ireadsb, ireadmg, lendir, ii
      logical gotunx

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
c
c    NCDC ADDED CODE...
      write(6, 20) 
 20   format(/'Executing Program rrs_decoder ')
c
      unxbfr = ' '
      narg = iargc()
      if (narg .ne. 2) then
c       write(6,*) 'rrs_decoder:  incorrect usage'
c       write(6,*) 'Usage:  rrs_decoder UNIXBUFRfile Output_Dir_Path'
c       call EXIT(2)
        write(6,*)'ERR 2'
      endif
c
      call getarg(1,unxbfr)
      call getarg(2,outdir)
      lendir = -1
      do ii = 1, 60
       if (lendir .eq. -1 .and. outdir(ii+1:ii+1) .eq. ' ') then
        lendir = ii
       endif
      enddo
c
c     write(6, 30) unxbfr
 30   format(/'UNIX BUFR:    ',a/)
c
      inquire(file=unxbfr, exist=gotunx)
      if (.not. gotunx) then
        write(6, 40) 
 40     format(/'ERROR:  UNIX BUFR File ',
     &   'NOT FOUND  !!!!')
        write(6,*) 'ERR 1'
        stop 
      endif
c

C  OPEN THE FILE TO THE PROGRAM AND TO THE BUFRLIB
C  -----------------------------------------------

      OPEN( 8, FILE=unxbfr, FORM='UNFORMATTED')
      CALL OPENBF(8,'IN',8)
      of19 = outdir(1:lendir) // 'NC002019.DAT'
      of20 = outdir(1:lendir) // 'NC002020.DAT'
      of21 = outdir(1:lendir) // 'NC002021.DAT'
      of22 = outdir(1:lendir) // 'NC002022.DAT'
      of23 = outdir(1:lendir) // 'NC002023.DAT'
      of24 = outdir(1:lendir) // 'NC002024.DAT'
      of25 = outdir(1:lendir) // 'NC002025.DAT'
      open (19, file=of19, status='unknown')
      open (20, file=of20, status='unknown')
      open (21, file=of21, status='unknown')
      open (22, file=of22, status='unknown')
      open (23, file=of23, status='unknown')
      open (24, file=of24, status='unknown')
      open (25, file=of25, status='unknown')
c
c  WRITE COLUMN HEADERS FOR CERTAIN FILES...
c
      write(20, 70)
 70   format('Message Type NC002020 -- RRS raw PTU'
     &/'  WMO  WBAN RT DS Year Mo Da Hr Mn   Sec     Lat ',
     & '       Lon  Bar_Ht    Ht DS Year Mo Da Hr Mn   Sec',
     & ' FLPC  Press  %C MQ Ck RawRh  %C MQ Ck SR Td(k)  %C MQ Ck')
c
      write(21, 75)
 75   format('Message Type NC002021 -- RRS raw GPS unsmoothed'
     &/'  WMO  WBAN RT DS Year Mo Da Hr Mn   Sec      Lat ',
     & '       Lon Bar_Ht    Ht DS Year Mo Da Hr Mn   Sec',
     & '   GPS-Lat MQ Ck    GPS-Lon MQ Ck GP_HT MQ Ck',
     & '  U-Wnd MQ Ck  V-Wnd MQ Ck  %C')
c
      write(22, 80)
 80   format('Message Type NC002022 -- RRS raw GPS smoothed'
     &/'  WMO  WBAN RT DS Year Mo Da Hr Mn   Sec      Lat ',
     & '       Lon Bar_Ht    Ht DS Year Mo Da Hr Mn   Sec',
     & '   GPS-Lat MQ Ck    GPS-Lon MQ Ck GP_HT MQ Ck',
     & '  U-Wnd MQ Ck  V-Wnd MQ Ck  %C')
c
      write(23, 85)
 85   format('Message Type NC002023 -- RRS processed PTU'
     &/'  WMO  WBAN RT DS Year Mo Da Hr Mn   Sec      Lat ',
     & '       Lon Bar_Ht    Ht DS Year Mo Da Hr Mn   Sec',
     & ' FLPC  Press MQ Ck FLPC  Press MQ Ck    Rh MQ Ck',
     & ' SR Td(k) MQ Ck SR Td(k) MQ Ck DP(k) MQ Ck  GPHT MQ Ck')
c
      write(24, 90)
 90   format('Message Type NC002024 -- RRS processed GPS'
     &/'  WMO  WBAN RT DS Year Mo Da Hr Mn   Sec      Lat ',
     & '       Lon Bar_Ht    Ht DS Year Mo Da Hr Mn   Sec',
     & '   GPS-Lat MQ Ck    GPS-Lon MQ Ck GP_HT MQ Ck',
     & '  U-Wnd MQ Ck  V-Wnd MQ Ck')
c 
      write(25, 95)
 95   format(
     & 'Message Type NC002025 -- RRS Standard & significant levels',
     &/'  WMO  WBAN RT DS Year Mo Da Hr Mn   Sec      Lat ',
     & '       Lon Bar_Ht    Ht DS Year Mo Da Hr Mn   Sec',
     & ' LS FLPC  Press    RH SC   Tmp   DPD  GPHT  HGHT   WSpd  WD')

C  READ THE RECORDS SEQUENTIALLY; PROCESS EACH TYPE IN KIND
C  --------------------------------------------------------

      DO WHILE(IREADMG(8,SUBSET,IDATE).EQ.0)
      DO WHILE(IREADSB(8).EQ.0)

      CALL UFBSEQ(8,ARR,MAXARR,1,IRET,SUBSET)

      IF(SUBSET.EQ.'NC002019') THEN

C assignment list for NC002019 002-019  9.4.1 RRS radiosonde complete registrat

       a19( 1) = arr(  1) ! CODE TAB  008041  RRS DATA SIGNIFICANCE
       a19( 2) = arr(  2) ! CCITT IA  001062  SHORT ICAO LOCATION IDENTIFIER
       a19( 3) = arr(  3) ! CODE TAB  008041  RRS DATA SIGNIFICANCE
       a19( 4) = arr(  4) ! CCITT IA  001062  SHORT ICAO LOCATION IDENTIFIER
       a19( 5) = arr(  5) ! NUMERIC   001001  WMO BLOCK NUMBER
       a19( 6) = arr(  6) ! NUMERIC   001002  WMO STATION NUMBER
       a19( 7) = arr(  7) ! NUMERIC   001094  WBAN NUMBER
       a19( 8) = arr(  8) ! CODE TAB  002011  RADIOSONDE TYPE
       a19( 9) = arr(  9) ! CCITT IA  001018  SHORT STATION OR SITE NAME (XXX
       a19(10) = arr( 10) ! CCITT IA  001095  RADIOSONDE OBSERVER
C       a19(11) = arr( 11) ! CCITT IA  025061  RRS WORKSTATION SOFTWARE VERSION
C       CALL READLC (8, SOFTV, 'SOFTV')
       a19(12) = arr( 12) ! NUMERIC   025068  RRS NUMBER OF ARCHIVE RECOMPUTES
       a19(13) = arr( 13) ! NUMERIC   001082  RRS RADIOSONDE ASCENSION NUMBER
       a19(14) = arr( 14) ! NUMERIC   001083  RRS RADIOSONDE RELEASE NUMBER
C       a19(15) = arr( 15) ! CCITT IA  001081  RRS RADIOSONDE SERIAL NUMBER
C       CALL READLC (8, RSERL, 'RSERL')
       a19(16) = arr( 16) ! HZ        002067  RRS RADIOSONDE OPERATING RADIO F
       a19(17) = arr( 17) ! CODE TAB  002066  RRS RADIOSONDE GROUND RECEIVING
       a19(18) = arr( 18) ! CODE TAB  002014  TRACKING TECHNIQUE/STATUS OF SYS
       a19(19) = arr( 19) ! PASCAL    025067  RRS RELEASE POINT PRESSURE CORRE
       a19(20) = arr( 20) ! DEGREE    025065  RRS ORIENTATION CORRECTION (AZIM
       a19(21) = arr( 21) ! DEGREE    025066  RRS ORIENTATION CORRECTION (ELEV
       a19(22) = arr( 22) ! CODE TAB  002095  RRS PRESSURE SENSOR TYPE CODE TA
       a19(23) = arr( 23) ! CODE TAB  002096  RRS TEMPERATURE SENSOR TYPE CODE
       a19(24) = arr( 24) ! CODE TAB  002097  RRS HUMIDITY SENSOR TYPE CODE TA
       a19(25) = arr( 25) ! FLAG TAB  002016  RRS RADIOSONDE CONFIGURATION
       a19(26) = arr( 26) ! CODE TAB  002083  RRS BALLOON SHELTER TYPE
       a19(27) = arr( 27) ! CODE TAB  002080  RRS BALLOON MANUFACTURER
       a19(28) = arr( 28) ! CODE TAB  002081  RRS BALLOON TYPE
C       a19(29) = arr( 29) ! CCITT IA  001093  RRS BALLOON LOT NUMBER
C       CALL READLC (8, BLOTN, 'BLOTN')
       a19(30) = arr( 30) ! CODE TAB  002084  RRS BALLOON GAS TYPE USED
       a19(31) = arr( 31) ! KG        002085  RRS BALLOON GAS AMOUNT USED
       a19(32) = arr( 32) ! METER     002086  RRS BALLOON FLIGHT TRAIN LENGTH
       a19(33) = arr( 33) ! KG        002082  RRS BALLOON WEIGHT
       a19(34) = arr( 34) ! CODE TAB  008041  RRS DATA SIGNIFICANCE
       a19(35) = arr( 35) ! YEAR      004001  YEAR
       a19(36) = arr( 36) ! MONTH     004002  MONTH
       a19(37) = arr( 37) ! DAY       004003  DAY
       a19(38) = arr( 38) ! CODE TAB  008041  RRS DATA SIGNIFICANCE
       a19(39) = arr( 39) ! YEAR      004001  YEAR
       a19(40) = arr( 40) ! MONTH     004002  MONTH
       a19(41) = arr( 41) ! DAY       004003  DAY
       a19(42) = arr( 42) ! HOUR      004004  HOUR
       a19(43) = arr( 43) ! MINUTE    004005  MINUTE
       a19(44) = arr( 44) ! SECOND    004006  SECOND
       a19(45) = arr( 45) ! DEGREE    005001  LATITUDE (HIGH ACCURACY)
       a19(46) = arr( 46) ! DEGREE    006001  LONGITUDE (HIGH ACCURACY)
       a19(47) = arr( 47) ! METER     007001  STATION ELEVATION (BAROMETER LOC
       a19(48) = arr( 48) ! METER     007007  HEIGHT
       a19(49) = arr( 49) ! CODE TAB  008041  RRS DATA SIGNIFICANCE
       a19(50) = arr( 50) ! CODE TAB  002115  RRS SURFACE WEATHER OBSERVING EQ
       a19(51) = arr( 51) ! PASCAL    010004  PRESSURE
       a19(52) = arr( 52) ! CODE TAB  002115  RRS SURFACE WEATHER OBSERVING EQ
       a19(53) = arr( 53) ! PERCENT   013003  RELATIVE HUMIDITY
       a19(54) = arr( 54) ! CODE TAB  002115  RRS SURFACE WEATHER OBSERVING EQ
       a19(55) = arr( 55) ! DEGREE T  011001  WIND DIRECTION
       a19(56) = arr( 56) ! METER/SE  011002  WIND SPEED
       a19(57) = arr( 57) ! CODE TAB  002115  RRS SURFACE WEATHER OBSERVING EQ
       a19(58) = arr( 58) ! DEGREE K  012101  TEMPERATURE/DRY BULB TEMPERATURE
       a19(59) = arr( 59) ! HOUR      004024  TIME PERIOD OR DISPLACEMENT (HOU
       a19(60) = arr( 60) ! DEGREE K  012101  TEMPERATURE/DRY BULB TEMPERATURE
       a19(61) = arr( 61) ! HOUR      004024  TIME PERIOD OR DISPLACEMENT (HOU
       a19(62) = arr( 62) ! CODE TAB  002115  RRS SURFACE WEATHER OBSERVING EQ
       a19(63) = arr( 63) ! DEGREE K  012103  DEW-POINT TEMPERATURE
       a19(64) = arr( 64) ! DEGREE K  012102  WET BULB TEMPERATURE
       a19(65) = arr( 65) ! CODE TAB  020012  CLOUD TYPE
       a19(66) = arr( 66) ! CODE TAB  020012  CLOUD TYPE
       a19(67) = arr( 67) ! CODE TAB  020012  CLOUD TYPE
       a19(68) = arr( 68) ! CODE TAB  020011  CLOUD AMOUNT
C      a19(69) = arr( 69) ! CODE TAB  020007  HEIGHT ABOVE SURFACE FOR BASE OF (OBSOLETE)
       a19(69) = arr( 69) ! CODE TAB  020011  HEIGHT OF BASE OF CLOUD         
       a19(70) = arr( 70) ! CODE TAB  020003  PRESENT WEATHER
       a19(71) = arr( 71) ! CODE TAB  020003  PRESENT WEATHER
       a19(72) = arr( 72) ! CODE TAB  008041  RRS DATA SIGNIFICANCE
       a19(73) = arr( 73) ! DEGREE T  005021  BEARING OR AZIMUTH
       a19(74) = arr( 74) ! METER     007005  HEIGHT INCREMENT
       a19(75) = arr( 75) ! METERS    006021  DISTANCE
       a19(76) = arr( 76) ! CODE TAB  008041  RRS DATA SIGNIFICANCE
       a19(77) = arr( 77) ! MINUTE    004025  TIME PERIOD OR DISPLACEMENT (MIN
       a19(78) = arr( 78) ! SECOND    004026  TIME PERIOD OR DISPLACEMENT (SEC
       a19(79) = arr( 79) ! DEGREE    005001  LATITUDE (HIGH ACCURACY)
       a19(80) = arr( 80) ! DEGREE    006001  LONGITUDE (HIGH ACCURACY)
       a19(81) = arr( 81) ! YEAR      004001  YEAR
       a19(82) = arr( 82) ! MONTH     004002  MONTH
       a19(83) = arr( 83) ! DAY       004003  DAY
       a19(84) = arr( 84) ! HOUR      004004  HOUR
       a19(85) = arr( 85) ! MINUTE    004005  MINUTE
       a19(86) = arr( 86) ! SECOND    004006  SECOND
       a19(87) = arr( 87) ! FLAG TAB  025069  RRS FLIGHT LEVEL PRESSURE CORREC
       a19(88) = arr( 88) ! PASCAL    007004  PRESSURE
       a19(89) = arr( 89) ! PERCENT   013003  RELATIVE HUMIDITY
       a19(90) = arr( 90) ! CODE TAB  002013  SOLAR AND INFRARED RADIATION COR
       a19(91) = arr( 91) ! DEGREE K  012101  TEMPERATURE/DRY BULB TEMPERATURE
       a19(92) = arr( 92) ! METER     007009  GEOPOTENTIAL HEIGHT
C      a19(93) = arr( 93) ! CODE TAB  008077  TYPE OF RADIOSONDE TERMINATION (OBSOLETE)
C      a19(94) = arr( 94) ! CODE TAB  025022  REASON FOR TERMINATION (OBSOLETE)
C      a19(95) = arr( 95) ! CODE TAB  008077  TYPE OF RADIOSONDE TERMINATION (OBSOLETE)
C      a19(96) = arr( 96) ! CODE TAB  025022  REASON FOR TERMINATION (OBSOLETE)
       a19(93) = arr( 93) ! CODE TAB  008040  LVL SIGNIFICANCE FOR SOUNDING 
       a19(94) = arr( 94) ! CODE TAB  035035  REASON FOR TERMINATION
       a19(95) = arr( 95) ! CODE TAB  008040  LVL SIGNIFICANCE FOR SOUNDING 
       a19(96) = arr( 96) ! CODE TAB  035035  REASON FOR TERMINATION
       call unpk_19 (a19)

      ELSEIF(SUBSET.EQ.'NC002020') THEN

C assignment list for NC002020 002-020  9.4.2 RRS raw PTU

       a20( 1) = arr(  1) ! NUMERIC   001001  WMO BLOCK NUMBER
       a20( 2) = arr(  2) ! NUMERIC   001002  WMO STATION NUMBER
       a20( 3) = arr(  3) ! NUMERIC   001094  WBAN NUMBER
       a20( 4) = arr(  4) ! CODE TAB  002011  RADIOSONDE TYPE
       a20( 5) = arr(  5) ! CODE TAB  008041  RRS DATA SIGNIFICANCE
       a20( 6) = arr(  6) ! YEAR      004001  YEAR
       a20( 7) = arr(  7) ! MONTH     004002  MONTH
       a20( 8) = arr(  8) ! DAY       004003  DAY
       a20( 9) = arr(  9) ! HOUR      004004  HOUR
       a20(10) = arr( 10) ! MINUTE    004005  MINUTE
       a20(11) = arr( 11) ! SECOND    004006  SECOND
       a20(12) = arr( 12) ! DEGREE    005001  LATITUDE (HIGH ACCURACY)
       a20(13) = arr( 13) ! DEGREE    006001  LONGITUDE (HIGH ACCURACY)
       a20(14) = arr( 14) ! METER     007001  STATION ELEVATION (BAROMETER LOC
       a20(15) = arr( 15) ! METER     007007  HEIGHT
       a20(16) = arr( 16) ! CODE TAB  008041  RRS DATA SIGNIFICANCE
       a20(17) = arr( 17) ! YEAR      004001  YEAR
       a20(18) = arr( 18) ! MONTH     004002  MONTH
       a20(19) = arr( 19) ! DAY       004003  DAY
       a20(20) = arr( 20) ! HOUR      004004  HOUR
       a20(21) = arr( 21) ! MINUTE    004005  MINUTE
       a20(22) = arr( 22) ! SECOND    004006  SECOND
       a20(23) = arr( 23) ! FLAG TAB  025069  RRS FLIGHT LEVEL PRESSURE CORREC
       a20(24) = arr( 24) ! PASCAL    007004  PRESSURE
       a20(25) = arr( 25) ! %         033007  DATA PERCENT CONFIDENCE (vendor-
       a20(26) = arr( 26) ! CODE TAB  033016  DATA QUALITY-MARK  INDICATOR (wo
       a20(27) = arr( 27) ! CODE TAB  033015  DATA QUALITY-CHECK INDICATOR (wo
       a20(28) = arr( 28) ! PERCENT   013009  RAW RELATIVE HUMIDITY
       a20(29) = arr( 29) ! %         033007  DATA PERCENT CONFIDENCE (vendor-
       a20(30) = arr( 30) ! CODE TAB  033016  DATA QUALITY-MARK  INDICATOR (wo
       a20(31) = arr( 31) ! CODE TAB  033015  DATA QUALITY-CHECK INDICATOR (wo
       a20(32) = arr( 32) ! CODE TAB  002013  SOLAR AND INFRARED RADIATION COR
       a20(33) = arr( 33) ! DEGREE K  012101  TEMPERATURE/DRY BULB TEMPERATURE
       a20(34) = arr( 34) ! %         033007  DATA PERCENT CONFIDENCE (vendor-
       a20(35) = arr( 35) ! CODE TAB  033016  DATA QUALITY-MARK  INDICATOR (wo
       a20(36) = arr( 36) ! CODE TAB  033015  DATA QUALITY-CHECK INDICATOR (wo
       call unpk_20 (a20)

      ELSEIF(SUBSET.EQ.'NC002021') THEN

C assignment list for NC002021 002-021  9.4.3 RRS raw GPS unsmoothed

       a21( 1) = arr(  1) ! NUMERIC   001001  WMO BLOCK NUMBER
       a21( 2) = arr(  2) ! NUMERIC   001002  WMO STATION NUMBER
       a21( 3) = arr(  3) ! NUMERIC   001094  WBAN NUMBER
       a21( 4) = arr(  4) ! CODE TAB  002011  RADIOSONDE TYPE
       a21( 5) = arr(  5) ! CODE TAB  008041  RRS DATA SIGNIFICANCE
       a21( 6) = arr(  6) ! YEAR      004001  YEAR
       a21( 7) = arr(  7) ! MONTH     004002  MONTH
       a21( 8) = arr(  8) ! DAY       004003  DAY
       a21( 9) = arr(  9) ! HOUR      004004  HOUR
       a21(10) = arr( 10) ! MINUTE    004005  MINUTE
       a21(11) = arr( 11) ! SECOND    004006  SECOND
       a21(12) = arr( 12) ! DEGREE    005001  LATITUDE (HIGH ACCURACY)
       a21(13) = arr( 13) ! DEGREE    006001  LONGITUDE (HIGH ACCURACY)
       a21(14) = arr( 14) ! METER     007001  STATION ELEVATION (BAROMETER LOC
       a21(15) = arr( 15) ! METER     007007  HEIGHT
       a21(16) = arr( 16) ! CODE TAB  008041  RRS DATA SIGNIFICANCE
       a21(17) = arr( 17) ! YEAR      004001  YEAR
       a21(18) = arr( 18) ! MONTH     004002  MONTH
       a21(19) = arr( 19) ! DAY       004003  DAY
       a21(20) = arr( 20) ! HOUR      004004  HOUR
       a21(21) = arr( 21) ! MINUTE    004005  MINUTE
       a21(22) = arr( 22) ! SECOND    004006  SECOND
       a21(23) = arr( 23) ! DEGREE    006001  LONGITUDE (HIGH ACCURACY)
       a21(24) = arr( 24) ! CODE TAB  033016  DATA QUALITY-MARK  INDICATOR (wo
       a21(25) = arr( 25) ! CODE TAB  033015  DATA QUALITY-CHECK INDICATOR (wo
       a21(26) = arr( 26) ! DEGREE    005001  LATITUDE (HIGH ACCURACY)
       a21(27) = arr( 27) ! CODE TAB  033016  DATA QUALITY-MARK  INDICATOR (wo
       a21(28) = arr( 28) ! CODE TAB  033015  DATA QUALITY-CHECK INDICATOR (wo
       a21(29) = arr( 29) ! METER/SE  011003  U-COMPONENT
       a21(30) = arr( 30) ! CODE TAB  033016  DATA QUALITY-MARK  INDICATOR (wo
       a21(31) = arr( 31) ! CODE TAB  033015  DATA QUALITY-CHECK INDICATOR (wo
       a21(32) = arr( 32) ! METER/SE  011004  V-COMPONENT
       a21(33) = arr( 33) ! CODE TAB  033016  DATA QUALITY-MARK  INDICATOR (wo
       a21(34) = arr( 34) ! CODE TAB  033015  DATA QUALITY-CHECK INDICATOR (wo
       a21(35) = arr( 35) ! METER     007007  HEIGHT
       a21(36) = arr( 36) ! CODE TAB  033016  DATA QUALITY-MARK  INDICATOR (wo
       a21(37) = arr( 37) ! CODE TAB  033015  DATA QUALITY-CHECK INDICATOR (wo
       a21(38) = arr( 38) ! %         033007  DATA PERCENT CONFIDENCE (vendor-
       call unpk_21 (a21)

       ELSEIF(SUBSET.EQ.'NC002022') THEN

C assignment list for NC002022 002-022  9.4.4 RRS raw GPS smoothed

       a22( 1) = arr(  1) ! NUMERIC   001001  WMO BLOCK NUMBER
       a22( 2) = arr(  2) ! NUMERIC   001002  WMO STATION NUMBER
       a22( 3) = arr(  3) ! NUMERIC   001094  WBAN NUMBER
       a22( 4) = arr(  4) ! CODE TAB  002011  RADIOSONDE TYPE
       a22( 5) = arr(  5) ! CODE TAB  008041  RRS DATA SIGNIFICANCE
       a22( 6) = arr(  6) ! YEAR      004001  YEAR
       a22( 7) = arr(  7) ! MONTH     004002  MONTH
       a22( 8) = arr(  8) ! DAY       004003  DAY
       a22( 9) = arr(  9) ! HOUR      004004  HOUR
       a22(10) = arr( 10) ! MINUTE    004005  MINUTE
       a22(11) = arr( 11) ! SECOND    004006  SECOND
       a22(12) = arr( 12) ! DEGREE    005001  LATITUDE (HIGH ACCURACY)
       a22(13) = arr( 13) ! DEGREE    006001  LONGITUDE (HIGH ACCURACY)
       a22(14) = arr( 14) ! METER     007001  STATION ELEVATION (BAROMETER LOC
       a22(15) = arr( 15) ! METER     007007  HEIGHT
       a22(16) = arr( 16) ! CODE TAB  008041  RRS DATA SIGNIFICANCE
       a22(17) = arr( 17) ! YEAR      004001  YEAR
       a22(18) = arr( 18) ! MONTH     004002  MONTH
       a22(19) = arr( 19) ! DAY       004003  DAY
       a22(20) = arr( 20) ! HOUR      004004  HOUR
       a22(21) = arr( 21) ! MINUTE    004005  MINUTE
       a22(22) = arr( 22) ! SECOND    004006  SECOND
       a22(23) = arr( 23) ! DEGREE    006001  LONGITUDE (HIGH ACCURACY)
       a22(24) = arr( 24) ! CODE TAB  033016  DATA QUALITY-MARK  INDICATOR (wo
       a22(25) = arr( 25) ! CODE TAB  033015  DATA QUALITY-CHECK INDICATOR (wo
       a22(26) = arr( 26) ! DEGREE    005001  LATITUDE (HIGH ACCURACY)
       a22(27) = arr( 27) ! CODE TAB  033016  DATA QUALITY-MARK  INDICATOR (wo
       a22(28) = arr( 28) ! CODE TAB  033015  DATA QUALITY-CHECK INDICATOR (wo
       a22(29) = arr( 29) ! METER/SE  011003  U-COMPONENT
       a22(30) = arr( 30) ! CODE TAB  033016  DATA QUALITY-MARK  INDICATOR (wo
       a22(31) = arr( 31) ! CODE TAB  033015  DATA QUALITY-CHECK INDICATOR (wo
       a22(32) = arr( 32) ! METER/SE  011004  V-COMPONENT
       a22(33) = arr( 33) ! CODE TAB  033016  DATA QUALITY-MARK  INDICATOR (wo
       a22(34) = arr( 34) ! CODE TAB  033015  DATA QUALITY-CHECK INDICATOR (wo
       a22(35) = arr( 35) ! METER     007007  HEIGHT
       a22(36) = arr( 36) ! CODE TAB  033016  DATA QUALITY-MARK  INDICATOR (wo
       a22(37) = arr( 37) ! CODE TAB  033015  DATA QUALITY-CHECK INDICATOR (wo
       a22(38) = arr( 38) ! %         033007  DATA PERCENT CONFIDENCE (vendor-
       call unpk_22 (a22)

       ELSEIF(SUBSET.EQ.'NC002023') THEN

C assignment list for NC002023 002-023  9.4.5 RRS processed PTU

       a23( 1) = arr(  1)  ! NUMERIC   001001  WMO BLOCK NUMBER
       a23( 2) = arr(  2)  ! NUMERIC   001002  WMO STATION NUMBER
       a23( 3) = arr(  3)  ! NUMERIC   001094  WBAN NUMBER
       a23( 4) = arr(  4)  ! CODE TAB  002011  RADIOSONDE TYPE
       a23( 5) = arr(  5)  ! CODE TAB  008041  RRS DATA SIGNIFICANCE
       a23( 6) = arr(  6)  ! YEAR      004001  YEAR
       a23( 7) = arr(  7)  ! MONTH     004002  MONTH
       a23( 8) = arr(  8)  ! DAY       004003  DAY
       a23( 9) = arr(  9)  ! HOUR      004004  HOUR
       a23(10) = arr( 10)  ! MINUTE    004005  MINUTE
       a23(11) = arr( 11)  ! SECOND    004006  SECOND
       a23(12) = arr( 12)  ! DEGREE    005001  LATITUDE (HIGH ACCURACY)
       a23(13) = arr( 13)  ! DEGREE    006001  LONGITUDE (HIGH ACCURACY)
       a23(14) = arr( 14)  ! METER     007001  STATION ELEVATION (BAROMETER LOC
       a23(15) = arr( 15)  ! METER     007007  HEIGHT
       a23(16) = arr( 16)  ! CODE TAB  008041  RRS DATA SIGNIFICANCE
       a23(17) = arr( 17)  ! YEAR      004001  YEAR
       a23(18) = arr( 18)  ! MONTH     004002  MONTH
       a23(19) = arr( 19)  ! DAY       004003  DAY
       a23(20) = arr( 20)  ! HOUR      004004  HOUR
       a23(21) = arr( 21)  ! MINUTE    004005  MINUTE
       a23(22) = arr( 22)  ! SECOND    004006  SECOND
       a23(23) = arr( 23)  ! FLAG TAB  025069  RRS FLIGHT LEVEL PRESSURE CORREC
       a23(24) = arr( 24)  ! PASCAL    007004  PRESSURE
       a23(25) = arr( 25)  ! CODE TAB  033016  DATA QUALITY-MARK  INDICATOR (wo
       a23(26) = arr( 26)  ! CODE TAB  033015  DATA QUALITY-CHECK INDICATOR (wo
       a23(27) = arr( 27)  ! FLAG TAB  025069  RRS FLIGHT LEVEL PRESSURE CORREC
       a23(28) = arr( 28)  ! PASCAL    007004  PRESSURE
       a23(29) = arr( 29)  ! CODE TAB  033016  DATA QUALITY-MARK  INDICATOR (wo
       a23(30) = arr( 30)  ! CODE TAB  033015  DATA QUALITY-CHECK INDICATOR (wo
       a23(31) = arr( 31)  ! PERCENT   013003  RELATIVE HUMIDITY
       a23(32) = arr( 32)  ! CODE TAB  033016  DATA QUALITY-MARK  INDICATOR (wo
       a23(33) = arr( 33)  ! CODE TAB  033015  DATA QUALITY-CHECK INDICATOR (wo
       a23(34) = arr( 34)  ! CODE TAB  002013  SOLAR AND INFRARED RADIATION COR
       a23(35) = arr( 35)  ! DEGREE K  012101  TEMPERATURE/DRY BULB TEMPERATURE
       a23(36) = arr( 36)  ! CODE TAB  033016  DATA QUALITY-MARK  INDICATOR (wo
       a23(37) = arr( 37)  ! CODE TAB  033015  DATA QUALITY-CHECK INDICATOR (wo
       a23(38) = arr( 38)  ! CODE TAB  002013  SOLAR AND INFRARED RADIATION COR
       a23(39) = arr( 39)  ! DEGREE K  012101  TEMPERATURE/DRY BULB TEMPERATURE
       a23(40) = arr( 40)  ! CODE TAB  033016  DATA QUALITY-MARK  INDICATOR (wo
       a23(41) = arr( 41)  ! CODE TAB  033015  DATA QUALITY-CHECK INDICATOR (wo
       a23(42) = arr( 42)  ! DEGREE K  012103  DEW-POINT TEMPERATURE
       a23(43) = arr( 43)  ! CODE TAB  033016  DATA QUALITY-MARK  INDICATOR (wo
       a23(44) = arr( 44)  ! CODE TAB  033015  DATA QUALITY-CHECK INDICATOR (wo
       a23(45) = arr( 45)  ! METER     007009  GEOPOTENTIAL HEIGHT
       a23(46) = arr( 46)  ! CODE TAB  033016  DATA QUALITY-MARK  INDICATOR (wo
       a23(47) = arr( 47)  ! CODE TAB  033015  DATA QUALITY-CHECK INDICATOR (wo
       call unpk_23(a23)

       ELSEIF(SUBSET.EQ.'NC002024') THEN

C assignment list for NC002024 002-024  9.4.6 RRS processed GPS

       a24( 1) = arr(  1)  ! NUMERIC   001001  WMO BLOCK NUMBER
       a24( 2) = arr(  2)  ! NUMERIC   001002  WMO STATION NUMBER
       a24( 3) = arr(  3)  ! NUMERIC   001094  WBAN NUMBER
       a24( 4) = arr(  4)  ! CODE TAB  002011  RADIOSONDE TYPE
       a24( 5) = arr(  5)  ! CODE TAB  008041  RRS DATA SIGNIFICANCE
       a24( 6) = arr(  6)  ! YEAR      004001  YEAR
       a24( 7) = arr(  7)  ! MONTH     004002  MONTH
       a24( 8) = arr(  8)  ! DAY       004003  DAY
       a24( 9) = arr(  9)  ! HOUR      004004  HOUR
       a24(10) = arr( 10)  ! MINUTE    004005  MINUTE
       a24(11) = arr( 11)  ! SECOND    004006  SECOND
       a24(12) = arr( 12)  ! DEGREE    005001  LATITUDE (HIGH ACCURACY)
       a24(13) = arr( 13)  ! DEGREE    006001  LONGITUDE (HIGH ACCURACY)
       a24(14) = arr( 14)  ! METER     007001  STATION ELEVATION (BAROMETER LOC
       a24(15) = arr( 15)  ! METER     007007  HEIGHT
       a24(16) = arr( 16)  ! CODE TAB  008041  RRS DATA SIGNIFICANCE
       a24(17) = arr( 17)  ! YEAR      004001  YEAR
       a24(18) = arr( 18)  ! MONTH     004002  MONTH
       a24(19) = arr( 19)  ! DAY       004003  DAY
       a24(20) = arr( 20)  ! HOUR      004004  HOUR
       a24(21) = arr( 21)  ! MINUTE    004005  MINUTE
       a24(22) = arr( 22)  ! SECOND    004006  SECOND
       a24(23) = arr( 23)  ! DEGREE    006001  LONGITUDE (HIGH ACCURACY)
       a24(24) = arr( 24)  ! CODE TAB  033016  DATA QUALITY-MARK  INDICATOR (wo
       a24(25) = arr( 25)  ! CODE TAB  033015  DATA QUALITY-CHECK INDICATOR (wo
       a24(26) = arr( 26)  ! DEGREE    005001  LATITUDE (HIGH ACCURACY)
       a24(27) = arr( 27)  ! CODE TAB  033016  DATA QUALITY-MARK  INDICATOR (wo
       a24(28) = arr( 28)  ! CODE TAB  033015  DATA QUALITY-CHECK INDICATOR (wo
       a24(29) = arr( 29)  ! METER/SE  011003  U-COMPONENT
       a24(30) = arr( 30)  ! CODE TAB  033016  DATA QUALITY-MARK  INDICATOR (wo
       a24(31) = arr( 31)  ! CODE TAB  033015  DATA QUALITY-CHECK INDICATOR (wo
       a24(32) = arr( 32)  ! METER/SE  011004  V-COMPONENT
       a24(33) = arr( 33)  ! CODE TAB  033016  DATA QUALITY-MARK  INDICATOR (wo
       a24(34) = arr( 34)  ! CODE TAB  033015  DATA QUALITY-CHECK INDICATOR (wo
       a24(35) = arr( 35)  ! METER     007007  HEIGHT
       a24(36) = arr( 36)  ! CODE TAB  033016  DATA QUALITY-MARK  INDICATOR (wo
       a24(37) = arr( 37)  ! CODE TAB  033015  DATA QUALITY-CHECK INDICATOR (wo
       call unpk_24(a24)

       ELSEIF(SUBSET.EQ.'NC002025') THEN

C assignment list for NC002025 002-025  9.4.7 RRS standard and significant leve

       a25( 1) = arr(  1)  ! NUMERIC   001001  WMO BLOCK NUMBER
       a25( 2) = arr(  2)  ! NUMERIC   001002  WMO STATION NUMBER
       a25( 3) = arr(  3)  ! NUMERIC   001094  WBAN NUMBER
       a25( 4) = arr(  4)  ! CODE TAB  002011  RADIOSONDE TYPE
       a25( 5) = arr(  5)  ! CODE TAB  008041  RRS DATA SIGNIFICANCE
       a25( 6) = arr(  6)  ! YEAR      004001  YEAR
       a25( 7) = arr(  7)  ! MONTH     004002  MONTH
       a25( 8) = arr(  8)  ! DAY       004003  DAY
       a25( 9) = arr(  9)  ! HOUR      004004  HOUR
       a25(10) = arr( 10)  ! MINUTE    004005  MINUTE
       a25(11) = arr( 11)  ! SECOND    004006  SECOND
       a25(12) = arr( 12)  ! DEGREE    005001  LATITUDE (HIGH ACCURACY)
       a25(13) = arr( 13)  ! DEGREE    006001  LONGITUDE (HIGH ACCURACY)
       a25(14) = arr( 14)  ! METER     007001  STATION ELEVATION (BAROMETER LOC
       a25(15) = arr( 15)  ! METER     007007  HEIGHT
       a25(16) = arr( 16)  ! CODE TAB  008041  RRS DATA SIGNIFICANCE
       a25(17) = arr( 17)  ! YEAR      004001  YEAR
       a25(18) = arr( 18)  ! MONTH     004002  MONTH
       a25(19) = arr( 19)  ! DAY       004003  DAY
       a25(20) = arr( 20)  ! HOUR      004004  HOUR
       a25(21) = arr( 21)  ! MINUTE    004005  MINUTE
       a25(22) = arr( 22)  ! SECOND    004006  SECOND
       a25(23) = arr( 23)  ! CODE TAB  008040  LEVEL SIGNIFICANCE FOR RRS SOUND
       a25(24) = arr( 24)  ! FLAG TAB  025069  RRS FLIGHT LEVEL PRESSURE CORREC
       a25(25) = arr( 25)  ! PASCAL    007004  PRESSURE
       a25(26) = arr( 26)  ! PERCENT   013003  RELATIVE HUMIDITY
       a25(27) = arr( 27)  ! CODE TAB  002013  SOLAR AND INFRARED RADIATION COR
       a25(28) = arr( 28)  ! DEGREE K  012101  TEMPERATURE/DRY BULB TEMPERATURE
       a25(29) = arr( 29)  ! DEGREE K  012103  DEW-POINT TEMPERATURE
       a25(30) = arr( 30)  ! METER     007009  GEOPOTENTIAL HEIGHT
       a25(31) = arr( 31)  ! METER     007007  HEIGHT
       a25(32) = arr( 32)  ! METER/SE  011002  WIND SPEED
       a25(33) = arr( 33)  ! DEGREE T  011001  WIND DIRECTION
       call unpk_25(a25)                                    

      ENDIF

C  END OF THE READ LOOPS
C  ---------------------

      ENDDO
      ENDDO
c
      close (19)
      close (20)
      close (21)
      close (22)
      close (23)
      close (24)
      close (25)
c
      write(6,*) 'AOK'
      STOP 
      END
C
C========================================================
C
      SUBROUTINE unpk_19(a19)                                    
      implicit none
c
      integer dsig1_19, dsig2_19, wmo19, wban19, ratp19, nrre, ascn,
     &     rrlse, rfreq, rgrsy, ttss, rrppc, psens, tsens, rhsens,
     &     rconf, bshel, bmfgr, btype, bgtyp, dsig3_19, b_yy, b_mo,
     &     b_dd, dsig4_19, r_yy19, r_mo19, r_dd19, r_hh19, rmin19,
     &     hbmsl19, heit19, dsig5_19, hinc, dsig6_19,
     &     sfeqp1, pr_19, sfeqp2, sfeqp3, wdir19, sfeqp4,
     &     tphr1, tphr2, sfeqp5, cltp1, cltp2, cltp3, clam, hocb, 
     &     prwe1, prwe2, dsig7_19, tpmi, t_yy, t_mo, t_dd, t_hh, tmin,
     &     flpc19, termpr, termsirc, termgph, termwsig, rtermwnd,
     &     termflt, rtermflt 
      real orcraz, orcrel, bgamt, bftln, bwght, rsec19, clat19, clon19,
     &     bearaz, dist, rh_19, wspd19, pasttd1, pasttd2, tdp19, tmwb,  
     &     tpse, clatterm, clonterm, tsec, termrh, term_td 
      character iclx1_19*4, iclx2_19*4, sstn19*5, obsvr19*4, SOFTV*12,
     &     RSERL*20, BLOTN*12

      common /admin/ dsig1_19, dsig2_19, wmo19, wban19, ratp19, nrre, 
     &  ascn, rrlse, rfreq, rgrsy, ttss, rrppc, psens, tsens, rhsens,
     &  rconf, bshel, bmfgr, btype, bgtyp, dsig3_19, b_yy, b_mo,
     &  b_dd, dsig4_19, r_yy19, r_mo19, r_dd19, r_hh19, rmin19,
     &  hbmsl19, heit19, dsig5_19, hinc, dist, dsig6_19,
     &  sfeqp1, pr_19, sfeqp2, sfeqp3, wdir19, sfeqp4,
     &  tphr1, tphr2, sfeqp5, cltp1, cltp2, cltp3, clam, hocb, 
     &  prwe1, prwe2, dsig7_19, tpmi, t_yy, t_mo, t_dd, t_hh, tmin,
     &  flpc19, termpr, termsirc, termgph, termwsig, rtermwnd,
     &  termflt, rtermflt,
     &  orcraz, orcrel, bgamt, bftln, bwght, rsec19, clat19, clon19,
     &  bearaz, rh_19, wspd19, pasttd1, pasttd2, tdp19, tmwb,  
     &  tpse, clatterm, clonterm, tsec, termrh, term_td, 
     &  iclx1_19, iclx2_19, sstn19, obsvr19, SOFTV,
     &  RSERL, BLOTN 
c
      CHARACTER*8  CVAL                                            
      EQUIVALENCE  (RVAL,CVAL)
      REAL*8       RVAL,BMISS
      REAL*8       a19(96)
                                                                        
      DATA BMISS /   10E10  /                                                
C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C
c  INITIALIZE FIELDS TO MISSING...
c
      dsig1_19 = -9
      iclx1_19 = '    '
      dsig2_19 = -9
      iclx2_19 = '    '
      wmo19 = 99999
      wban19 = 99999
      ratp19 = -9
      sstn19 = '     '
      obsvr19 = '    '
      SOFTV = '            '
      nrre = -9
      ascn = -9
      rrlse = -9
      RSERL = '                    '
      rfreq = -9
      rgrsy = -9
      ttss = -9
      rrppc = 99999
      orcraz = -999.9
      orcrel = -999.9
      psens = -9
      tsens = -9
      rhsens = -9
      rconf = -9
      bshel = -9
      bmfgr = -9
      btype = -9
      BLOTN = '            '
      bgtyp = -9
      bgamt = -99.9
      bftln = -99.9
      bwght = -99.9
      dsig3_19 = -9
      b_yy = 9999
      b_mo = 99
      b_dd = 99
      dsig4_19 = -9
      r_yy19 = 9999
      r_mo19 = 99
      r_dd19 = 99
      r_hh19 = 99
      rmin19 = 99
      rsec19 = 99.99
      clat19 = -99.99999
      clon19 = -999.99999
      hbmsl19 = 99999
      heit19 = 99999
      dsig5_19 = -9
      bearaz = 999.9
      hinc = 99999
      dist = 999.9
      dsig6_19 = -9
      sfeqp1 = -9
      pr_19 = 999999
      sfeqp2 = -9
      rh_19 = -99.9 
      sfeqp3 = -9
      wdir19 = 999
      wspd19 = 9999.9
      sfeqp4 = -9
      pasttd1 = 999.99
      tphr1 = 99
      pasttd2 = 999.99
      tphr2 = 99
      sfeqp5 = -9
      tdp19 = 999.99
      tmwb = 999.99
      cltp1 = -9
      cltp2 = -9
      cltp3 = -9
      clam = -9
      hocb = -9999
      prwe1 = -9
      prwe2 = -9
      dsig7_19 = -9
      tpmi = -99
      tpse = 99.99
      clatterm = -99.99999
      clonterm = -999.99999
      t_yy = 9999
      t_mo = 99
      t_dd = 99
      t_hh = 99
      tmin = 99
      tsec = 99.99
      flpc19 = 999
      termpr = 999999
      termrh = -99.9
      termsirc = -9
      term_td = 999.9 
      termgph = 99999
      termwsig = -9
      rtermwnd = -9
      termflt = -9
      rtermflt = -9
c
      if (a19(1) .lt. bmiss) dsig1_19 = int(a19(1))
      rval = a19(2)
      iclx1_19 = cval(1:4)
      if (a19(3) .lt. bmiss) dsig2_19 = int(a19(3))
      rval = a19(4)
      iclx2_19 = cval(1:4)
c
c       WMO Block & Station No.
      if (a19(5) .lt. bmiss .and. a19(6) .lt. bmiss) then
       wmo19 = int(a19(5)*1000) + int(a19(6))
      endif
c
c       WBAN No.
      if (a19(7) .lt. bmiss) wban19 = int(a19(7))
c
      if (a19(8) .lt. bmiss) ratp19 = int(a19(8))
      rval = a19(9)
      sstn19 = cval(1:5)
      rval = a19(10)
      obsvr19 = cval(1:4)
      CALL READLC (8, SOFTV, 'SOFTV')
      if (a19(12) .lt. bmiss) nrre = int(a19(12))
      if (a19(13) .lt. bmiss) ascn = int(a19(13))
      if (a19(14) .lt. bmiss) rrlse = int(a19(14))
      CALL READLC (8, RSERL, 'RSERL')

      if (a19(16) .lt. bmiss) rfreq = int(a19(16))
      if (a19(17) .lt. bmiss) rgrsy = int(a19(17))
      if (a19(18) .lt. bmiss) ttss = int(a19(18))
      if (a19(19) .lt. bmiss) rrppc = int(a19(19))
      if (a19(20) .lt. bmiss) orcraz = a19(20)
      if (a19(21) .lt. bmiss) orcrel = a19(21)
      if (a19(22) .lt. bmiss) psens = int(a19(22))
      if (a19(23) .lt. bmiss) tsens = int(a19(23))
      if (a19(24) .lt. bmiss) rhsens = int(a19(24))
      if (a19(25) .lt. bmiss) rconf = int(a19(25))
      if (a19(26) .lt. bmiss) bshel = int(a19(26))
      if (a19(27) .lt. bmiss) bmfgr = int(a19(27))
      if (a19(28) .lt. bmiss) btype = int(a19(28))
      CALL READLC (8, BLOTN, 'BLOTN')
      if (a19(30) .lt. bmiss) bgtyp = int(a19(30))
      if (a19(31) .lt. bmiss) bgamt = a19(31)
      if (a19(32) .lt. bmiss) bftln = a19(32)
      if (a19(33) .lt. bmiss) bwght = a19(33)
      if (a19(34) .lt. bmiss) dsig3_19 = int(a19(34))
      if (a19(35) .lt. bmiss) b_yy = int(a19(35))
      if (a19(36) .lt. bmiss) b_mo = int(a19(36))
      if (a19(37) .lt. bmiss) b_dd = int(a19(37))
      if (a19(38) .lt. bmiss) dsig4_19 = int(a19(38))
      if (a19(39) .lt. bmiss) r_yy19 = int(a19(39))
      if (a19(40) .lt. bmiss) r_mo19 = int(a19(40))
      if (a19(41) .lt. bmiss) r_dd19 = int(a19(41))
      if (a19(42) .lt. bmiss) r_hh19 = int(a19(42))
      if (a19(43) .lt. bmiss) rmin19 = int(a19(43))
      if (a19(44) .lt. bmiss) rsec19 = a19(44)
      if (a19(45) .lt. bmiss) clat19 = a19(45)
      if (a19(46) .lt. bmiss) clon19 = a19(46)
      if (a19(47) .lt. bmiss) hbmsl19 = int(a19(47))
      if (a19(48) .lt. bmiss) heit19 = int(a19(48))
      if (a19(49) .lt. bmiss) dsig5_19 = int(a19(49))
      if (a19(50) .lt. bmiss) bearaz = a19(50)
      if (a19(51) .lt. bmiss) hinc = int(a19(51))
      if (a19(52) .lt. bmiss) dist = a19(52)
      if (a19(53) .lt. bmiss) dsig6_19 = int(a19(53))
      if (a19(54) .lt. bmiss) sfeqp1 = int(a19(54))
      if (a19(55) .lt. bmiss) pr_19 = int(a19(55))
      if (a19(56) .lt. bmiss) sfeqp2 = int(a19(56))
      if (a19(57) .lt. bmiss) rh_19 = a19(57)
      if (a19(58) .lt. bmiss) sfeqp3 = int(a19(58))
      if (a19(59) .lt. bmiss) wdir19 = int(a19(59))
      if (a19(60) .lt. bmiss) wspd19 = a19(60)
      if (a19(61) .lt. bmiss) sfeqp4 = int(a19(61))
      if (a19(62) .lt. bmiss) pasttd1 = a19(62)
      if (a19(63) .lt. bmiss) tphr1 = int(a19(63))
      if (a19(64) .lt. bmiss) pasttd2 = a19(64)
      if (a19(65) .lt. bmiss) tphr2 = int(a19(65))
      if (a19(66) .lt. bmiss) sfeqp5 = int(a19(66))
      if (a19(67) .lt. bmiss) tdp19 = a19(67)
      if (a19(68) .lt. bmiss) tmwb = a19(68)
      if (a19(69) .lt. bmiss) cltp1 = int(a19(69))
      if (a19(70) .lt. bmiss) cltp2 = int(a19(70))
      if (a19(71) .lt. bmiss) cltp3 = int(a19(71))
      if (a19(72) .lt. bmiss) clam = int(a19(72))
      if (a19(73) .lt. bmiss) hocb = int(a19(73))
      if (a19(74) .lt. bmiss) prwe1 = int(a19(74))
      if (a19(75) .lt. bmiss) prwe2 = int(a19(75))
      if (a19(76) .lt. bmiss) dsig7_19 = int(a19(76))
      if (a19(77) .lt. bmiss) tpmi = int(a19(77))
      if (a19(78) .lt. bmiss) tpse = a19(78)
      if (a19(79) .lt. bmiss) clatterm = a19(79)
      if (a19(80) .lt. bmiss) clonterm = a19(80)
      if (a19(81) .lt. bmiss) t_yy = int(a19(81))
      if (a19(82) .lt. bmiss) t_mo = int(a19(82))
      if (a19(83) .lt. bmiss) t_dd = int(a19(83))
      if (a19(84) .lt. bmiss) t_hh = int(a19(84))
      if (a19(85) .lt. bmiss) tmin = int(a19(85))
      if (a19(86) .lt. bmiss) tsec = a19(86)
      if (a19(87) .lt. bmiss) flpc19 = int(a19(87))
      if (a19(88) .lt. bmiss) termpr = int(a19(88))
      if (a19(89) .lt. bmiss) termrh = a19(89)
      if (a19(90) .lt. bmiss) termsirc = int(a19(90))
      if (a19(91) .lt. bmiss) term_td = a19(91)
      if (a19(92) .lt. bmiss) termgph = int(a19(92))
      if (a19(93) .lt. bmiss) termwsig = int(a19(93))
      if (a19(94) .lt. bmiss) rtermwnd = int(a19(94))
      if (a19(95) .lt. bmiss) termflt = int(a19(95))
      if (a19(96) .lt. bmiss) rtermflt = int(a19(96))
c
      write(19, 100)
 100  format(/' DS CLX1 DS CLX2   WMO  WBAN RT  SStn Obs',
     &  '  SoftV        NR Ascn R#')
      write(19, 105)dsig1_19,iclx1_19,dsig2_19,iclx2_19,wmo19,
     &  wban19,ratp19,sstn19,obsvr19,SOFTV,nrre,ascn,rrlse
 105  format(i3,1x,a4,i3,1x,a4,1x,i5.5,1x,i5.5,i3,1x,a5,1x,a4,
     &  1x,a12,i3,i5,i3) 
      write(19, 110)
 110  format(/' Serial #             Frequency GS TT RRPPC ORCRAZ',
     &  ' ORCREL PS TS HS RC BS')
      write(19, 115)RSERL,rfreq,rgrsy,ttss,rrppc,orcraz,orcrel,
     &  psens,tsens,rhsens,rconf,bshel
 115  format(a20,i11,i3,i3,i6,2(f7.2),i3,i3,i3,i3,i3)
      write(19, 120)
 120  format(/'BM BT Balloon_Lot# GT G_Amt TrLen BWght DS',
     &  ' B_Yr Mo Da') 
      write(19, 125)bmfgr,btype,BLOTN,bgtyp,bgamt,bftln,bwght,
     &  dsig3_19,b_yy,b_mo,b_dd
 125  format(i2,i3,1x,a12,i3,f6.3,f6.1,f6.3,i3,1x,i4.4,2(1x,i2))
      write(19, 130)
 130  format(/'DS R_Yr Mo Da Hr Mn   Sec      Lat        Lon ',
     &  'Bar_Ht    Ht')
      write(19, 135)dsig4_19,r_yy19,r_mo19,r_dd19,r_hh19,rmin19,
     &  rsec19,clat19,clon19,hbmsl19,heit19
 135  format(i2,1x,i4.4,4(i3),f6.2,f10.5,f11.5,i6,i6)
      write(19, 140)
 140  format(/'DS BearAz  HInc  Dist DS EQ  Press EQ    RH EQ',
     &  '  WD   WSpd')
      write(19, 145)dsig5_19,bearaz,hinc,dist,dsig6_19,sfeqp1,
     &  pr_19,sfeqp2,rh_19,sfeqp3,wdir19,wspd19
 145  format(i2,f7.1,i6,f6.1,i3,i3,i7,i3,f6.1,i3,i4,f7.1)
      write(19, 150)
 150  format(/'EQ  Td(k)  TP  Td(k)  TP EQ Tdp(k)  Tw(k) C1 C2 C3 CA',
     &  '  HOCB PW PW')
      write(19, 155)sfeqp4,pasttd1,tphr1,pasttd2,tphr2,sfeqp5,
     &  tdp19,tmwb,cltp1,cltp2,cltp3,clam,hocb,prwe1,prwe2
 155  format(i2,f7.2,i4,f7.2,i4,i3,2f7.2,4i3,i6,2i3)
      write(19, 160)
 160  format(
     &  /'DS Tmi  Tsec  Term-Lat   Term-Lon T-Yr Mo Da Hr Mn   Sec')
      write(19, 165)dsig7_19,tpmi,tpse,clatterm,clonterm,t_yy,t_mo,
     & t_dd,t_hh,tmin,tsec
 165  format(i2,i4,f6.2,f10.5,f11.5,1x,i4.4,4i3,f6.2)
      write(19, 170)
 170  format(/'FLPC TermPr  T_RH SR  T_Td T_GPH LS RT LS RT')
      write(19, 175)flpc19,termpr,termrh,termsirc,term_td,
     &  termgph,termwsig,rtermwnd,termflt,rtermflt
 175  format(i4,i7,f6.1,i3,f6.1,i6,4i3)
c
      return
      end

C
C========================================================
C
      SUBROUTINE unpk_20(a20)                                    
      implicit none
                                                                        
      integer wmo20, wban20, ratp20, dsig1_20, r_yy20, r_mo20,
     &   r_dd20, r_hh20, rmin20, hbmsl20, heit20, dsig2_20,
     &   f_yy20, f_mo20, f_dd20, f_hh20, fmin20, flpc20,
     &   pr_20, prpc20, prqc20, prqck20, rhpc20, rhqc20, rhqck20,
     &   sirc20, tdpc20, tdqc20, tdqck20 

       real rsec20, clat20, clon20, fsec20, rh_20, td_20
c
      common /levels/  wmo20, wban20, ratp20, dsig1_20, 
     &   r_yy20, r_mo20, r_dd20, r_hh20, rmin20, rsec20,
     &   clat20, clon20, hbmsl20, heit20, dsig2_20,
     &   f_yy20, f_mo20, f_dd20, f_hh20, fmin20, fsec20,
     &   flpc20, pr_20, prpc20, prqc20, prqck20, rh_20, rhpc20,
     &   rhqc20, rhqck20, sirc20, td_20, tdpc20, tdqc20, tdqck20
c
      CHARACTER*8  CVAL,PMISS                                            
      EQUIVALENCE  (RVAL,CVAL)
      REAL*8       RVAL,BMISS
      REAL*8       a20(36)
                                                                        
      DATA BMISS /   10E10  /                                                
      DATA PMISS /' MISSING'/                                            
                                                                        
C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C
      wmo20 = 99999
      wban20 = 99999
      ratp20 = -9
      dsig1_20 = -9
      r_yy20 = 9999
      r_mo20 = 99 
      r_dd20 = 99
      r_hh20 = 99
      rmin20 = 99
      rsec20 = 99.99
      clat20 = -99.99999
      clon20 = -999.99999
      hbmsl20 = 99999
      heit20 = 99999
      dsig2_20 = -9
      f_yy20 = 9999
      f_mo20 = 99
      f_dd20 = 99
      f_hh20 = 99
      fmin20 = 99
      fsec20 = 99.99
      flpc20 = 999
      pr_20 = 999999
      prpc20 = -99
      prqc20 = -9
      prqck20 = -9
      rh_20 = -99.9
      rhpc20 = -99
      rhqc20 = -9
      rhqck20 = -9
      sirc20 = -9
      td_20 = 999.9
      tdpc20 = -99
      tdqc20 = -9
      tdqck20 = -9
c
c       WMO Block & Station No.
      if (a20(1) .lt. bmiss .and. a20(2) .lt. bmiss) then
       wmo20 = int(a20(1)*1000) + int(a20(2))
      endif
c
c       WBAN No.
      if (a20(3) .lt. bmiss) wban20 = int(a20(3))
c
c       Radiosonde Type
      if (a20(4) .lt. bmiss) ratp20 = int(a20(4))
c
c       Data Significance - Balloon Launch Point
      if (a20(5) .lt. bmiss) dsig1_20 = int(a20(5))
c
c       Balloon Release Year, mon, day, hr, min, sec
      if (a20(6) .lt. bmiss) r_yy20 = int(a20(6))
      if (a20(7) .lt. bmiss) r_mo20 = int(a20(7))
      if (a20(8) .lt. bmiss) r_dd20 = int(a20(8))
      if (a20(9) .lt. bmiss) r_hh20 = int(a20(9))
      if (a20(10) .lt. bmiss) rmin20 = int(a20(10))
      if (a20(11) .lt. bmiss) rsec20 = a20(11)
c
c       Latitude / Longitude
      if (a20(12) .lt. bmiss) clat20 = a20(12)
      if (a20(13) .lt. bmiss) clon20 = a20(13)
c
c       Ht Barometer Above Mean Sea Lvl
c       Height
      if (a20(14) .lt. bmiss) hbmsl20 = int(a20(14))
      if (a20(15) .lt. bmiss) heit20 = int(a20(15))
c
c       Data Significance - Flight Level Observation
      if (a20(16) .lt. bmiss) dsig2_20 = int(a20(16))
c
c       Flight Level Year, mon, day, hr, min, sec
      if (a20(17) .lt. bmiss) f_yy20 = int(a20(17))
      if (a20(18) .lt. bmiss) f_mo20 = int(a20(18))
      if (a20(19) .lt. bmiss) f_dd20 = int(a20(19))
      if (a20(20) .lt. bmiss) f_hh20 = int(a20(20))
      if (a20(21) .lt. bmiss) fmin20 = int(a20(21))
      if (a20(22) .lt. bmiss) fsec20 = a20(22)
c
c       Flight Level Pressure Correction
      if (a20(23) .lt. bmiss) flpc20 = int(a20(23))
c
c       Pressure 
      if (a20(24) .lt. bmiss) pr_20 = int(a20(24))
c
c       Data Pct Confidence, Man/Auto QC, Data Quality-Check Ind.
      if (a20(25) .lt. bmiss) prpc20 = int(a20(25))
      if (a20(26) .lt. bmiss) prqc20 = int(a20(26))
      if (a20(27) .lt. bmiss) prqck20 = int(a20(27))
c
c       Raw Relative Humidity
      if (a20(28) .lt. bmiss) rh_20 = a20(28)
c
c       Data Pct Confidence, Man/Auto QC, Data Quality-Check Ind.
      if (a20(29) .lt. bmiss) rhpc20 = int(a20(29))
      if (a20(30) .lt. bmiss) rhqc20 = int(a20(30))
      if (a20(31) .lt. bmiss) rhqck20 = int(a20(31))
c
c       Solar & Infrared Rad. Corr.
      if (a20(32) .lt. bmiss) sirc20 = int(a20(32))
c
c       Temperature/Dry Bulb Temperature                
      if (a20(33) .lt. bmiss) td_20 = a20(33)
c
c       Data Pct Confidence, Man/Auto QC, Data Quality-Check Ind.
      if (a20(34) .lt. bmiss) tdpc20 = int(a20(34))
      if (a20(35) .lt. bmiss) tdqc20 = int(a20(35))
      if (a20(36) .lt. bmiss) tdqck20 = int(a20(36))
c
      write(20, 100) wmo20, wban20, ratp20, dsig1_20, 
     &   r_yy20, r_mo20, r_dd20, r_hh20, rmin20, rsec20,
     &   clat20, clon20, hbmsl20, heit20, dsig2_20,
     &   f_yy20, f_mo20, f_dd20, f_hh20, fmin20, fsec20,
     &   flpc20, pr_20, prpc20, prqc20, prqck20, rh_20, rhpc20,
     &   rhqc20, rhqck20, sirc20, td_20, tdpc20, tdqc20, tdqck20 
 100  format(i5.5,1x,i5.5,1x,i2,1x,i2,
     &   1x,i4.4,4(1x,i2),1x,f5.2,
     &   1x,f9.5,1x,f10.5,1x,i5,1x,i5,1x,i2,
     &   1x,i4.4,4(1x,i2),1x,f5.2,
     &   1x,i4,1x,i6,1x,i3,1x,i2,1x,i2,1x,f5.1,1x,i3,
     &   1x,i2,1x,i2,1x,i2,1x,f5.1,1x,i3,1x,i2,1x,i2)
c
      RETURN                                                            
      END                                                               
C
C========================================================
C
      SUBROUTINE unpk_21(a21)                                    
      implicit none
                                                                        
      integer wmo21, wban21, ratp21, dsig1_21, r_yy21, r_mo21,
     &   r_dd21, r_hh21, rmin21, hbmsl21, heit21, dsig2_21,
     &   f_yy21, f_mo21, f_dd21, f_hh21, fmin21, 
     &   latmq21, latck21, lonmq21, lonck21, gps_ht21, htmq21, htck21,
     &   uwmq21, uwck21, vwmq21, vwck21, pccf21

       real rsec21, clat21, clon21, fsec21, gpslat21, gpslon21,
     &   uwnd21, vwnd21
c
      common /levels/  wmo21, wban21, ratp21, dsig1_21, 
     &   r_yy21, r_mo21, r_dd21, r_hh21, rmin21, rsec21,
     &   clat21, clon21, hbmsl21, heit21, dsig2_21,
     &   f_yy21, f_mo21, f_dd21, f_hh21, fmin21, fsec21,
     &   gpslat21, latmq21, latck21, gpslon21, lonmq21, lonck21, 
     &   gps_ht21, htmq21, htck21,
     &   uwnd21, uwmq21, uwck21, vwnd21, vwmq21, vwck21, pccf21
c
      CHARACTER*8  CVAL,PMISS                                            
      EQUIVALENCE  (RVAL,CVAL)
      REAL*8       RVAL,BMISS
      REAL*8       a21(38)
                                                                        
      DATA BMISS /   10E10  /                                                
      DATA PMISS /' MISSING'/                                            
                                                                        
C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C
      wmo21 = 99999
      wban21 = 99999
      ratp21 = -9
      dsig1_21 = -9
      r_yy21 = 9999
      r_mo21 = 99 
      r_dd21 = 99
      r_hh21 = 99
      rmin21 = 99
      rsec21 = 99.99
      clat21 = -99.99999
      clon21 = -999.99999
      hbmsl21 = 99999
      heit21 = 99999
      dsig2_21 = -9
      f_yy21 = 9999
      f_mo21 = 99
      f_dd21 = 99
      f_hh21 = 99
      fmin21 = 99
      fsec21 = 99.99
      gpslat21 = -99.99999
      latmq21 = -9
      latck21 = -9
      gpslon21 = -999.99999
      lonmq21 = -9
      lonck21 = -9
      gps_ht21 = 99999
      htmq21 = -9
      htck21 = -9
      uwnd21 = -999.9
      uwmq21 = -9
      uwck21 = -9
      vwnd21 = -999.9
      vwmq21 = -9
      vwck21 = -9
      pccf21 = -99
c
c       WMO Block & Station No.
      if (a21(1) .lt. bmiss .and. a21(2) .lt. bmiss) then
       wmo21 = int(a21(1)*1000) + int(a21(2))
      endif
c
c       WBAN No.
      if (a21(3) .lt. bmiss) wban21 = int(a21(3))
c
c       Radiosonde Type
      if (a21(4) .lt. bmiss) ratp21 = int(a21(4))
c
c       Data Significance - Balloon Launch Point
      if (a21(5) .lt. bmiss) dsig1_21 = int(a21(5))
c
c       Balloon Release Year, mon, day, hr, min, sec
      if (a21(6) .lt. bmiss) r_yy21 = int(a21(6))
      if (a21(7) .lt. bmiss) r_mo21 = int(a21(7))
      if (a21(8) .lt. bmiss) r_dd21 = int(a21(8))
      if (a21(9) .lt. bmiss) r_hh21 = int(a21(9))
      if (a21(10) .lt. bmiss) rmin21 = int(a21(10))
      if (a21(11) .lt. bmiss) rsec21 = a21(11)
c
c       Latitude / Longitude
      if (a21(12) .lt. bmiss) clat21 = a21(12)
      if (a21(13) .lt. bmiss) clon21 = a21(13)
c
c       Ht Barometer Above Mean Sea Lvl
c       Height
      if (a21(14) .lt. bmiss) hbmsl21 = int(a21(14))
      if (a21(15) .lt. bmiss) heit21 = int(a21(15))
c
c       Data Significance - Flight Level Observation
      if (a21(16) .lt. bmiss) dsig2_21 = int(a21(16))
c
c       Flight Level Year, mon, day, hr, min, sec
      if (a21(17) .lt. bmiss) f_yy21 = int(a21(17))
      if (a21(18) .lt. bmiss) f_mo21 = int(a21(18))
      if (a21(19) .lt. bmiss) f_dd21 = int(a21(19))
      if (a21(20) .lt. bmiss) f_hh21 = int(a21(20))
      if (a21(21) .lt. bmiss) fmin21 = int(a21(21))
      if (a21(22) .lt. bmiss) fsec21 = a21(22)
c
c       GPS Latitude
      if (a21(23) .lt. bmiss) gpslat21 = a21(23)
c
c       GPS Latitude Man/Auto QC, Data Quality-Check Ind.
      if (a21(24) .lt. bmiss) latmq21 = int(a21(24))
      if (a21(25) .lt. bmiss) latck21 = int(a21(25))
c
c       GPS Longitude
      if (a21(26) .lt. bmiss) gpslon21 = a21(26)
c
c       GPS Longitude Man/Auto QC, Data Quality-Check Ind.
      if (a21(27) .lt. bmiss) lonmq21 = int(a21(27))
      if (a21(28) .lt. bmiss) lonck21 = int(a21(28))
c
c       GPS Height   
      if (a21(29) .lt. bmiss) gps_ht21 = a21(29)
c
c       GPS Height Man/Auto QC, Data Quality-Check Ind.
      if (a21(30) .lt. bmiss) htmq21 = int(a21(30))
      if (a21(31) .lt. bmiss) htck21 = int(a21(31))
c
c       GPS u wind Component
      if (a21(32) .lt. bmiss) uwnd21 = a21(32)
c
c       GPS u wind Component Man/Auto QC, Data Quality-Check Ind.
      if (a21(33) .lt. bmiss) uwmq21 = int(a21(33))
      if (a21(34) .lt. bmiss) uwck21 = int(a21(34))
c
c       GPS v wind Component
      if (a21(35) .lt. bmiss) vwnd21 = a21(35)
c
c       GPS v wind Component Man/Auto QC, Data Quality-Check Ind.
      if (a21(36) .lt. bmiss) vwmq21 = int(a21(36))
      if (a21(37) .lt. bmiss) vwck21 = int(a21(37))
c
c       Data Percent Confidence
      if (a21(38) .lt. bmiss) pccf21 = int(a21(38))
c
      write(21, 100) wmo21, wban21, ratp21, dsig1_21, 
     &   r_yy21, r_mo21, r_dd21, r_hh21, rmin21, rsec21,
     &   clat21, clon21, hbmsl21, heit21, dsig2_21,
     &   f_yy21, f_mo21, f_dd21, f_hh21, fmin21, fsec21,
     &   gpslat21, latmq21, latck21, gpslon21, lonmq21, lonck21, 
     &   gps_ht21, htmq21, htck21,
     &   uwnd21, uwmq21, uwck21, vwnd21, vwmq21, vwck21, pccf21
 100  format(i5.5,1x,i5.5,1x,i2,1x,i2,
     &   1x,i4.4,4(1x,i2),1x,f5.2,
     &   1x,f9.5,1x,f10.5,1x,i5,1x,i5,1x,i2,
     &   1x,i4.4,4(1x,i2),1x,f5.2,
     &   1x,f9.5,1x,i2,1x,i2,1x,f10.5,1x,i2,1x,i2,
     &   1x,i5,1x,i2,1x,i2,
     &   1x,f6.1,1x,i2,1x,i2,1x,f6.1,1x,i2,1x,i2,1x,i3)
c
      RETURN                                                            
      END
C
C========================================================
C
      SUBROUTINE unpk_22(a22)                                    
      implicit none
                                                                        
      integer wmo22, wban22, ratp22, dsig1_22, r_yy22, r_mo22,
     &   r_dd22, r_hh22, rmin22, hbmsl22, heit22, dsig2_22,
     &   f_yy22, f_mo22, f_dd22, f_hh22, fmin22, 
     &   latmq22, latck22, lonmq22, lonck22, gps_ht22, htmq22, htck22,
     &   uwmq22, uwck22, vwmq22, vwck22, pccf22

       real rsec22, clat22, clon22, fsec22, gpslat22, gpslon22,
     &   uwnd22, vwnd22
c
      common /levels/  wmo22, wban22, ratp22, dsig1_22, 
     &   r_yy22, r_mo22, r_dd22, r_hh22, rmin22, rsec22,
     &   clat22, clon22, hbmsl22, heit22, dsig2_22,
     &   f_yy22, f_mo22, f_dd22, f_hh22, fmin22, fsec22,
     &   gpslat22, latmq22, latck22, gpslon22, lonmq22, lonck22, 
     &   gps_ht22, htmq22, htck22,
     &   uwnd22, uwmq22, uwck22, vwnd22, vwmq22, vwck22, pccf22
c
      CHARACTER*8  CVAL,PMISS                                            
      EQUIVALENCE  (RVAL,CVAL)
      REAL*8       RVAL,BMISS
      REAL*8       a22(38)
                                                                        
      DATA BMISS /   10E10  /                                                
      DATA PMISS /' MISSING'/                                            
                                                                        
C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C
      wmo22 = 99999
      wban22 = 99999
      ratp22 = -9
      dsig1_22 = -9
      r_yy22 = 9999
      r_mo22 = 99 
      r_dd22 = 99
      r_hh22 = 99
      rmin22 = 99
      rsec22 = 99.99
      clat22 = -99.99999
      clon22 = -999.99999
      hbmsl22 = 99999
      heit22 = 99999
      dsig2_22 = -9
      f_yy22 = 9999
      f_mo22 = 99
      f_dd22 = 99
      f_hh22 = 99
      fmin22 = 99
      fsec22 = 99.99
      gpslat22 = -99.99999
      latmq22 = -9
      latck22 = -9
      gpslon22 = -999.99999
      lonmq22 = -9
      lonck22 = -9
      gps_ht22 = 99999
      htmq22 = -9
      htck22 = -9
      uwnd22 = -999.9
      uwmq22 = -9
      uwck22 = -9
      vwnd22 = -999.9
      vwmq22 = -9
      vwck22 = -9
      pccf22 = -99
c
c       WMO Block & Station No.
      if (a22(1) .lt. bmiss .and. a22(2) .lt. bmiss) then
       wmo22 = int(a22(1)*1000) + int(a22(2))
      endif
c
c       WBAN No.
      if (a22(3) .lt. bmiss) wban22 = int(a22(3))
c
c       Radiosonde Type
      if (a22(4) .lt. bmiss) ratp22 = int(a22(4))
c
c       Data Significance - Balloon Launch Point
      if (a22(5) .lt. bmiss) dsig1_22 = int(a22(5))
c
c       Balloon Release Year, mon, day, hr, min, sec
      if (a22(6) .lt. bmiss) r_yy22 = int(a22(6))
      if (a22(7) .lt. bmiss) r_mo22 = int(a22(7))
      if (a22(8) .lt. bmiss) r_dd22 = int(a22(8))
      if (a22(9) .lt. bmiss) r_hh22 = int(a22(9))
      if (a22(10) .lt. bmiss) rmin22 = int(a22(10))
      if (a22(11) .lt. bmiss) rsec22 = a22(11)
c
c       Latitude / Longitude
      if (a22(12) .lt. bmiss) clat22 = a22(12)
      if (a22(13) .lt. bmiss) clon22 = a22(13)
c
c       Ht Barometer Above Mean Sea Lvl
c       Height
      if (a22(14) .lt. bmiss) hbmsl22 = int(a22(14))
      if (a22(15) .lt. bmiss) heit22 = int(a22(15))
c
c       Data Significance - Flight Level Observation
      if (a22(16) .lt. bmiss) dsig2_22 = int(a22(16))
c
c       Flight Level Year, mon, day, hr, min, sec
      if (a22(17) .lt. bmiss) f_yy22 = int(a22(17))
      if (a22(18) .lt. bmiss) f_mo22 = int(a22(18))
      if (a22(19) .lt. bmiss) f_dd22 = int(a22(19))
      if (a22(20) .lt. bmiss) f_hh22 = int(a22(20))
      if (a22(21) .lt. bmiss) fmin22 = int(a22(21))
      if (a22(22) .lt. bmiss) fsec22 = a22(22)
c
c       GPS Latitude
      if (a22(23) .lt. bmiss) gpslat22 = a22(23)
c
c       GPS Latitude Man/Auto QC, Data Quality-Check Ind.
      if (a22(24) .lt. bmiss) latmq22 = int(a22(24))
      if (a22(25) .lt. bmiss) latck22 = int(a22(25))
c
c       GPS Longitude
      if (a22(26) .lt. bmiss) gpslon22 = a22(26)
c
c       GPS Longitude Man/Auto QC, Data Quality-Check Ind.
      if (a22(27) .lt. bmiss) lonmq22 = int(a22(27))
      if (a22(28) .lt. bmiss) lonck22 = int(a22(28))
c
c       GPS Height   
      if (a22(29) .lt. bmiss) gps_ht22 = a22(29)
c
c       GPS Height Man/Auto QC, Data Quality-Check Ind.
      if (a22(30) .lt. bmiss) htmq22 = int(a22(30))
      if (a22(31) .lt. bmiss) htck22 = int(a22(31))
c
c       GPS u wind Component
      if (a22(32) .lt. bmiss) uwnd22 = a22(32)
c
c       GPS u wind Component Man/Auto QC, Data Quality-Check Ind.
      if (a22(33) .lt. bmiss) uwmq22 = int(a22(33))
      if (a22(34) .lt. bmiss) uwck22 = int(a22(34))
c
c       GPS v wind Component
      if (a22(35) .lt. bmiss) vwnd22 = a22(35)
c
c       GPS v wind Component Man/Auto QC, Data Quality-Check Ind.
      if (a22(36) .lt. bmiss) vwmq22 = int(a22(36))
      if (a22(37) .lt. bmiss) vwck22 = int(a22(37))
c
c       Data Percent Confidence
      if (a22(38) .lt. bmiss) pccf22 = int(a22(38))
c
      write(22, 100) wmo22, wban22, ratp22, dsig1_22, 
     &   r_yy22, r_mo22, r_dd22, r_hh22, rmin22, rsec22,
     &   clat22, clon22, hbmsl22, heit22, dsig2_22,
     &   f_yy22, f_mo22, f_dd22, f_hh22, fmin22, fsec22,
     &   gpslat22, latmq22, latck22, gpslon22, lonmq22, lonck22, 
     &   gps_ht22, htmq22, htck22,
     &   uwnd22, uwmq22, uwck22, vwnd22, vwmq22, vwck22, pccf22
 100  format(i5.5,1x,i5.5,1x,i2,1x,i2,
     &   1x,i4.4,4(1x,i2),1x,f5.2,
     &   1x,f9.5,1x,f10.5,1x,i5,1x,i5,1x,i2,
     &   1x,i4.4,4(1x,i2),1x,f5.2,
     &   1x,f9.5,1x,i2,1x,i2,1x,f10.5,1x,i2,1x,i2,
     &   1x,i5,1x,i2,1x,i2,
     &   1x,f6.1,1x,i2,1x,i2,1x,f6.1,1x,i2,1x,i2,1x,i3)
c
      RETURN                                                            
      END
C
C========================================================
C
      SUBROUTINE unpk_23(a23)                               
c
      implicit none
c
c     COMMON Data Definitions..                              
      integer wmo23, wban23, ratp23, dsig1_23, r_yy23, r_mo23,
     &   r_dd23, r_hh23, rmin23, hbmsl23, heit23, dsig2_23,
     &   f_yy23, f_mo23, f_dd23, f_hh23, fmin23, flpc1_23, pr1_23,
     &   pr1maqc, pr1qcck, flpc2_23, pr2_23, pr2maqc, pr2qcck,
     &   rh_maqc, rh_qcck, sirc1_23, td1_maqc, td1_qcck, sirc2_23,
     &   td2_maqc, td2_qcck, tdp_maqc,
     &   tdp_qcck, gpht23, gpht_maqc, gpht_qcck 

       real rsec23, clat23, clon23, fsec23, rh_23, td1_23, td2_23, 
     &  tdp_23
c
      common /hi_res_ptu/ wmo23, wban23, ratp23, dsig1_23, r_yy23, 
     &   r_mo23, r_dd23, r_hh23, rmin23, hbmsl23, heit23, dsig2_23,
     &   f_yy23, f_mo23, f_dd23, f_hh23, fmin23, flpc1_23, pr1_23,
     &   pr1maqc, pr1qcck, flpc2_23, pr2_23, pr2maqc, pr2qcck,
     &   rh_maqc, rh_qcck, sirc1_23, td1_maqc, td1_qcck, sirc2_23,
     &   td2_maqc, td2_qcck, tdp_maqc,
     &   tdp_qcck, gpht23, gpht_maqc, gpht_qcck,
     &   rsec23, clat23, clon23, fsec23, rh_23, td1_23, td2_23, tdp_23
c
      CHARACTER*8  CVAL,PMISS                                           
      EQUIVALENCE  (RVAL,CVAL)
      REAL*8       RVAL,BMISS
      REAL*8       a23(47)
                                                                        
      DATA BMISS /   10E10  /                                                
      DATA PMISS /' MISSING'/                                            
                                                                        
C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C
      wmo23 = 99999
      wban23 = 99999
      ratp23 = -9
      dsig1_23 = -9
      r_yy23 = 9999
      r_mo23 = 99 
      r_dd23 = 99
      r_hh23 = 99
      rmin23 = 99
      rsec23 = 99.99
      clat23 = -99.99999
      clon23 = -999.99999
      hbmsl23 = 99999
      heit23 = 99999
      dsig2_23 = -9
      f_yy23 = 9999
      f_mo23 = 99
      f_dd23 = 99
      f_hh23 = 99
      fmin23 = 99
      fsec23 = 99.99
      flpc1_23 = 999
      pr1_23 = 999999
      pr1maqc = -9
      pr1qcck = -9
      flpc2_23 = 999
      pr2_23 = 999999
      pr2maqc = -9
      pr2qcck = -9
      rh_23 = -99.9
      rh_maqc = -9
      rh_qcck = -9
      sirc1_23 = -9
      td1_23 = 999.9
      td1_maqc = -9
      td1_qcck = -9
      sirc2_23 = -9
      td2_23 = 999.9
      td2_maqc = -9
      td2_qcck = -9
      tdp_23 = 999.9
      tdp_maqc = -9
      tdp_qcck = -9
      gpht23 = 99999
      gpht_maqc = -9
      gpht_qcck = -9
c
c       WMO Block & Station No.
      if (a23(1) .lt. bmiss .and. a23(2) .lt. bmiss) then
       wmo23 = int(a23(1)*1000) + int(a23(2))
      endif
c       WBAN No.
      if (a23(3) .lt. bmiss) wban23 = int(a23(3))
c
c       Radiosonde Type
      if (a23(4) .lt. bmiss) ratp23 = int(a23(4))
c
c       Data Significance
      if (a23(5) .lt. bmiss) dsig1_23 = int(a23(5))
c
c       Balloon Release Year, mon, day, hr, min, sec
      if (a23(6) .lt. bmiss) r_yy23 = int(a23(6))
      if (a23(7) .lt. bmiss) r_mo23 = int(a23(7))
      if (a23(8) .lt. bmiss) r_dd23 = int(a23(8))
      if (a23(9) .lt. bmiss) r_hh23 = int(a23(9))
      if (a23(10) .lt. bmiss) rmin23 = int(a23(10))
      if (a23(11) .lt. bmiss) rsec23 = a23(11)
c
c       Latitude / Longitude
      if (a23(12) .lt. bmiss) clat23 = a23(12)
      if (a23(13) .lt. bmiss) clon23 = a23(13)
c
c       Ht Barometer Above Mean Sea Lvl
c       Height
      if (a23(14) .lt. bmiss) hbmsl23 = int(a23(14))
      if (a23(15) .lt. bmiss) heit23 = int(a23(15))
c
c       Data Significance
      if (a23(16) .lt. bmiss) dsig2_23 = int(a23(16))
c
c       Flight Level Year, mon, day, hr, min, sec
      if (a23(17) .lt. bmiss) f_yy23 = int(a23(17))
      if (a23(18) .lt. bmiss) f_mo23 = int(a23(18))
      if (a23(19) .lt. bmiss) f_dd23 = int(a23(19))
      if (a23(20) .lt. bmiss) f_hh23 = int(a23(20))
      if (a23(21) .lt. bmiss) fmin23 = int(a23(21))
      if (a23(22) .lt. bmiss) fsec23 = a23(22)
c
c       Flt level Pressure Correction 
      if (a23(23) .lt. bmiss) flpc1_23 = int(a23(23))
c
c       Corrected Pressure 
      if (a23(24) .lt. bmiss) pr1_23 = int(a23(24))
c
c       Corrected Pressure - MAQC 
      if (a23(25) .lt. bmiss) pr1maqc = int(a23(25))
c
c       Corrected Pressure - QC CHECK
      if (a23(26) .lt. bmiss) pr1qcck = int(a23(26))
c
c       Flt level Pressure Correction (smoothed) 
      if (a23(27) .lt. bmiss) flpc2_23 = int(a23(27))
c
c       Smoothed Pressure 
      if (a23(28) .lt. bmiss) pr2_23 = int(a23(28))
c
c       Smoothed Pressure - MAQC 
      if (a23(29) .lt. bmiss) pr2maqc = int(a23(29))
c
c       Smoothed Pressure - QC CHECK
      if (a23(30) .lt. bmiss) pr2qcck = int(a23(30))
c
c       Corrected Relative Humidity
      if (a23(31) .lt. bmiss) rh_23 = a23(31)
c
c       Corrected RH - MAQC 
      if (a23(32) .lt. bmiss) rh_maqc = int(a23(32))
c
c       Corrected RH - QC CHECK
      if (a23(33) .lt. bmiss) rh_qcck = int(a23(33))
c
c       Solar & Infrared Rad. Corr. (Uncorrected Temperature)
      if (a23(34) .lt. bmiss) sirc1_23 = int(a23(34))
c
c       Uncorrected Temperature                
      if (a23(35) .lt. bmiss) td1_23 = a23(35)
c
c       Uncorrected Temp. - MAQC 
      if (a23(36) .lt. bmiss) td1_maqc = int(a23(36))
c
c       Uncorrected Temp. - QC CHECK
      if (a23(37) .lt. bmiss) td1_qcck = int(a23(37))
c
c       Solar & Infrared Rad. Corr. (Corrected Temperature)
      if (a23(38) .lt. bmiss) sirc2_23 = int(a23(38))
c
c       Corrected Temperature                
      if (a23(39) .lt. bmiss) td2_23 = a23(39)
c
c       Corrected Temp. - MAQC 
      if (a23(40) .lt. bmiss) td2_maqc = int(a23(40))
c
c       Corrected Temp. - QC CHECK
      if (a23(41) .lt. bmiss) td2_qcck = int(a23(41))
c
c       Derived Dew Pt. Temperature                
      if (a23(42) .lt. bmiss) tdp_23 = a23(42)
c
c       Dew Point Temp. - MAQC 
      if (a23(43) .lt. bmiss) tdp_maqc = int(a23(43))
c
c       Dew Point Temp. - QC CHECK
      if (a23(44) .lt. bmiss) tdp_qcck = int(a23(44))
c
c       Derived Geopotential Height                
      if (a23(45) .lt. bmiss) gpht23 = int(a23(45))
c
c       Dew Point Temp. - MAQC 
      if (a23(46) .lt. bmiss) gpht_maqc = int(a23(46))
c
c       Dew Point Temp. - QC CHECK
      if (a23(47) .lt. bmiss) gpht_qcck = int(a23(47))
c
      write(23, 100) wmo23, wban23, ratp23, dsig1_23, 
     &  r_yy23, r_mo23, r_dd23, r_hh23, rmin23, rsec23, 
     &  clat23, clon23, hbmsl23, heit23, dsig2_23, 
     &  f_yy23, f_mo23, f_dd23, f_hh23, fmin23, fsec23, 
     &  flpc1_23, pr1_23, pr1maqc, pr1qcck, 
     &  flpc2_23, pr2_23, pr2maqc, pr2qcck, rh_23, rh_maqc, rh_qcck, 
     &  sirc1_23, td1_23, td1_maqc, td1_qcck, 
     &  sirc2_23, td2_23, td2_maqc, td2_qcck, 
     &  tdp_23, tdp_maqc, tdp_qcck, gpht23, gpht_maqc, gpht_qcck

 100  format(i5.5,1x,i5.5,1x,i2,1x,i2,
     &   1x,i4.4,4(1x,i2),1x,f5.2,
     &   1x,f9.5,1x,f10.5,1x,i5,1x,i5,1x,i2,
     &   1x,i4.4,4(1x,i2),1x,f5.2,
     &   1x,i4,1x,i6,1x,i2,1x,i2,
     &   1x,i4,1x,i6,1x,i2,1x,i2,1x,f5.1,1x,i2,1x,i2,
     &   1x,i2,1x,f5.1,1x,i2,1x,i2,
     &   1x,i2,1x,f5.1,1x,i2,1x,i2,
     &   1x,f5.1,1x,i2,1x,i2,1x,i5,1x,i2,1x,i2)
c
      RETURN 
      END
C
C========================================================
C
      SUBROUTINE unpk_24(a24)                                    
      implicit none
                                                                        
      integer wmo24, wban24, ratp24, dsig1_24, r_yy24, r_mo24,
     &   r_dd24, r_hh24, rmin24, hbmsl24, heit24, dsig2_24,
     &   f_yy24, f_mo24, f_dd24, f_hh24, fmin24, 
     &   latmq24, latck24, lonmq24, lonck24, gps_ht24, htmq24, htck24,
     &   uwmq24, uwck24, vwmq24, vwck24

       real rsec24, clat24, clon24, fsec24, gpslat24, gpslon24,
     &   uwnd24, vwnd24
c
      common /levels/  wmo24, wban24, ratp24, dsig1_24, 
     &   r_yy24, r_mo24, r_dd24, r_hh24, rmin24, rsec24,
     &   clat24, clon24, hbmsl24, heit24, dsig2_24,
     &   f_yy24, f_mo24, f_dd24, f_hh24, fmin24, fsec24,
     &   gpslat24, latmq24, latck24, gpslon24, lonmq24, lonck24, 
     &   gps_ht24, htmq24, htck24,
     &   uwnd24, uwmq24, uwck24, vwnd24, vwmq24, vwck24
c
      CHARACTER*8  CVAL,PMISS                                            
      EQUIVALENCE  (RVAL,CVAL)
      REAL*8       RVAL,BMISS
      REAL*8       a24(37)
                                                                        
      DATA BMISS /   10E10  /                                                
      DATA PMISS /' MISSING'/                                            
                                                                        
C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C
      wmo24 = 99999
      wban24 = 99999
      ratp24 = -9
      dsig1_24 = -9
      r_yy24 = 9999
      r_mo24 = 99 
      r_dd24 = 99
      r_hh24 = 99
      rmin24 = 99
      rsec24 = 99.99
      clat24 = -99.99999
      clon24 = -999.99999
      hbmsl24 = 99999
      heit24 = 99999
      dsig2_24 = -9
      f_yy24 = 9999
      f_mo24 = 99
      f_dd24 = 99
      f_hh24 = 99
      fmin24 = 99
      fsec24 = 99.99
      gpslat24 = -99.99999
      latmq24 = -9
      latck24 = -9
      gpslon24 = -999.99999
      lonmq24 = -9
      lonck24 = -9
      gps_ht24 = 99999
      htmq24 = -9
      htck24 = -9
      uwnd24 = -999.9
      uwmq24 = -9
      uwck24 = -9
      vwnd24 = -999.9
      vwmq24 = -9
      vwck24 = -9
c
c       WMO Block & Station No.
      if (a24(1) .lt. bmiss .and. a24(2) .lt. bmiss) then
       wmo24 = int(a24(1)*1000) + int(a24(2))
      endif
c
c       WBAN No.
      if (a24(3) .lt. bmiss) wban24 = int(a24(3))
c
c       Radiosonde Type
      if (a24(4) .lt. bmiss) ratp24 = int(a24(4))
c
c       Data Significance - Balloon Launch Point
      if (a24(5) .lt. bmiss) dsig1_24 = int(a24(5))
c
c       Balloon Release Year, mon, day, hr, min, sec
      if (a24(6) .lt. bmiss) r_yy24 = int(a24(6))
      if (a24(7) .lt. bmiss) r_mo24 = int(a24(7))
      if (a24(8) .lt. bmiss) r_dd24 = int(a24(8))
      if (a24(9) .lt. bmiss) r_hh24 = int(a24(9))
      if (a24(10) .lt. bmiss) rmin24 = int(a24(10))
      if (a24(11) .lt. bmiss) rsec24 = a24(11)
c
c       Latitude / Longitude
      if (a24(12) .lt. bmiss) clat24 = a24(12)
      if (a24(13) .lt. bmiss) clon24 = a24(13)
c
c       Ht Barometer Above Mean Sea Lvl
c       Height
      if (a24(14) .lt. bmiss) hbmsl24 = int(a24(14))
      if (a24(15) .lt. bmiss) heit24 = int(a24(15))
c
c       Data Significance - Flight Level Observation
      if (a24(16) .lt. bmiss) dsig2_24 = int(a24(16))
c
c       Flight Level Year, mon, day, hr, min, sec
      if (a24(17) .lt. bmiss) f_yy24 = int(a24(17))
      if (a24(18) .lt. bmiss) f_mo24 = int(a24(18))
      if (a24(19) .lt. bmiss) f_dd24 = int(a24(19))
      if (a24(20) .lt. bmiss) f_hh24 = int(a24(20))
      if (a24(21) .lt. bmiss) fmin24 = int(a24(21))
      if (a24(22) .lt. bmiss) fsec24 = a24(22)
c
c       GPS Latitude
      if (a24(23) .lt. bmiss) gpslat24 = a24(23)
c
c       GPS Latitude Man/Auto QC, Data Quality-Check Ind.
      if (a24(24) .lt. bmiss) latmq24 = int(a24(24))
      if (a24(25) .lt. bmiss) latck24 = int(a24(25))
c
c       GPS Longitude
      if (a24(26) .lt. bmiss) gpslon24 = a24(26)
c
c       GPS Longitude Man/Auto QC, Data Quality-Check Ind.
      if (a24(27) .lt. bmiss) lonmq24 = int(a24(27))
      if (a24(28) .lt. bmiss) lonck24 = int(a24(28))
c
c       GPS Height   
      if (a24(29) .lt. bmiss) gps_ht24 = a24(29)
c
c       GPS Height Man/Auto QC, Data Quality-Check Ind.
      if (a24(30) .lt. bmiss) htmq24 = int(a24(30))
      if (a24(31) .lt. bmiss) htck24 = int(a24(31))
c
c       GPS u wind Component
      if (a24(32) .lt. bmiss) uwnd24 = a24(32)
c
c       GPS u wind Component Man/Auto QC, Data Quality-Check Ind.
      if (a24(33) .lt. bmiss) uwmq24 = int(a24(33))
      if (a24(34) .lt. bmiss) uwck24 = int(a24(34))
c
c       GPS v wind Component
      if (a24(35) .lt. bmiss) vwnd24 = a24(35)
c
c       GPS v wind Component Man/Auto QC, Data Quality-Check Ind.
      if (a24(36) .lt. bmiss) vwmq24 = int(a24(36))
      if (a24(37) .lt. bmiss) vwck24 = int(a24(37))
c
      write(24, 100) wmo24, wban24, ratp24, dsig1_24, 
     &   r_yy24, r_mo24, r_dd24, r_hh24, rmin24, rsec24,
     &   clat24, clon24, hbmsl24, heit24, dsig2_24,
     &   f_yy24, f_mo24, f_dd24, f_hh24, fmin24, fsec24,
     &   gpslat24, latmq24, latck24, gpslon24, lonmq24, lonck24, 
     &   gps_ht24, htmq24, htck24,
     &   uwnd24, uwmq24, uwck24, vwnd24, vwmq24, vwck24
 100  format(i5.5,1x,i5.5,1x,i2,1x,i2,
     &   1x,i4.4,4(1x,i2),1x,f5.2,
     &   1x,f9.5,1x,f10.5,1x,i5,1x,i5,1x,i2,
     &   1x,i4.4,4(1x,i2),1x,f5.2,
     &   1x,f9.5,1x,i2,1x,i2,1x,f10.5,1x,i2,1x,i2,
     &   1x,i5,1x,i2,1x,i2,
     &   1x,f6.1,1x,i2,1x,i2,1x,f6.1,1x,i2,1x,i2,1x,i3)
c
      RETURN                                                            
      END
C
C========================================================
C
      SUBROUTINE unpk_25(a25)                                    
      implicit none
                                                                        
      integer wmo25, wban25, ratp25, dsig1_25, r_yy25, r_mo25,
     &   r_dd25, r_hh25, rmin25, hbmsl25, heit25, dsig2_25,
     &   f_yy25, f_mo25, f_dd25, f_hh25, fmin25, lvlsig25, flpc25,
     &   pr_25, sirc25, gpht25, hght25, wdir25 

       real rsec25, clat25, clon25, fsec25, rh_25, td_25, tdp25,
     &   wspd25
c
      common /levels/  wmo25, wban25, ratp25, dsig1_25, 
     &   r_yy25, r_mo25, r_dd25, r_hh25, rmin25, rsec25,
     &   clat25, clon25, hbmsl25, heit25, dsig2_25,
     &   f_yy25, f_mo25, f_dd25, f_hh25, fmin25, fsec25,
     &   lvlsig25, flpc25, pr_25, rh_25, sirc25,
     &   td_25, tdp25, gpht25, hght25, 
     &   wspd25, wdir25
c
      CHARACTER*8  CVAL,PMISS                                            
      EQUIVALENCE  (RVAL,CVAL)
      REAL*8       RVAL,BMISS
      REAL*8       a25(33)
                                                                        
      DATA BMISS /   10E10  /                                                
      DATA PMISS /' MISSING'/                                            
                                                                        
C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C
      wmo25 = 99999
      wban25 = 99999
      ratp25 = -9
      dsig1_25 = -9
      r_yy25 = 9999
      r_mo25 = 99 
      r_dd25 = 99
      r_hh25 = 99
      rmin25 = 99
      rsec25 = 99.99
      clat25 = -99.99999
      clon25 = -999.99999
      hbmsl25 = 99999
      heit25 = 99999
      dsig2_25 = -9
      f_yy25 = 9999
      f_mo25 = 99
      f_dd25 = 99
      f_hh25 = 99
      fmin25 = 99
      fsec25 = 99.99
      lvlsig25 = -9
      flpc25 = 999
      pr_25 = 999999
      rh_25 = -99.9
      sirc25 = -9
      td_25 = 999.9
      tdp25 = 999.9
      gpht25 = 99999
      hght25 = 99999
      wspd25 = 9999.9
      wdir25 = 999
c
c       WMO Block & Station No.
      if (a25(1) .lt. bmiss .and. a25(2) .lt. bmiss) then
       wmo25 = int(a25(1)*1000) + int(a25(2))
      endif
c
c       WBAN No.
      if (a25(3) .lt. bmiss) wban25 = int(a25(3))
c
c       Radiosonde Type
      if (a25(4) .lt. bmiss) ratp25 = int(a25(4))
c
c       Data Significance
      if (a25(5) .lt. bmiss) dsig1_25 = int(a25(5))
c
c       Balloon Release Year, mon, day, hr, min, sec
      if (a25(6) .lt. bmiss) r_yy25 = int(a25(6))
      if (a25(7) .lt. bmiss) r_mo25 = int(a25(7))
      if (a25(8) .lt. bmiss) r_dd25 = int(a25(8))
      if (a25(9) .lt. bmiss) r_hh25 = int(a25(9))
      if (a25(10) .lt. bmiss) rmin25 = int(a25(10))
      if (a25(11) .lt. bmiss) rsec25 = a25(11)
c
c       Latitude / Longitude
      if (a25(12) .lt. bmiss) clat25 = a25(12)
      if (a25(13) .lt. bmiss) clon25 = a25(13)
c
c       Ht Barometer Above Mean Sea Lvl
c       Height
      if (a25(14) .lt. bmiss) hbmsl25 = int(a25(14))
      if (a25(15) .lt. bmiss) heit25 = int(a25(15))
c
c       Data Significance
      if (a25(16) .lt. bmiss) dsig2_25 = int(a25(16))
c
c       Flight Level Year, mon, day, hr, min, sec
      if (a25(17) .lt. bmiss) f_yy25 = int(a25(17))
      if (a25(18) .lt. bmiss) f_mo25 = int(a25(18))
      if (a25(19) .lt. bmiss) f_dd25 = int(a25(19))
      if (a25(20) .lt. bmiss) f_hh25 = int(a25(20))
      if (a25(21) .lt. bmiss) fmin25 = int(a25(21))
      if (a25(22) .lt. bmiss) fsec25 = a25(22)
c
c       Level Significance
      if (a25(23) .lt. bmiss) lvlsig25 = int(a25(23))
c
c       Flt level Pressure Corr.
      if (a25(24) .lt. bmiss) flpc25 = int(a25(24))
c
c       Pressure 
      if (a25(25) .lt. bmiss) pr_25 = int(a25(25))
c
c       Relative Humidity
      if (a25(26) .lt. bmiss) rh_25 = int(a25(26))
c
c       Solar & Infrared Rad. Corr.
      if (a25(27) .lt. bmiss) sirc25 = int(a25(27))
c
c       Temperature                
      if (a25(28) .lt. bmiss) td_25 = a25(28)
c
c       Dew Pt Temperature                
      if (a25(29) .lt. bmiss) tdp25 = a25(29)
c
c       Geopotential Ht.                       
      if (a25(30) .lt. bmiss) gpht25 = int(a25(30))
c
c       Height                    
      if (a25(31) .lt. bmiss) hght25 = int(a25(31))
c
c       Wind Speed                
      if (a25(32) .lt. bmiss) wspd25 = a25(32)
c
c       Height                    
      if (a25(33) .lt. bmiss) wdir25 = int(a25(33))
c
      write(25, 100) wmo25, wban25, ratp25, dsig1_25, 
     &   r_yy25, r_mo25, r_dd25, r_hh25, rmin25, rsec25,
     &   clat25, clon25, hbmsl25, heit25, dsig2_25,
     &   f_yy25, f_mo25, f_dd25, f_hh25, fmin25, fsec25,
     &   lvlsig25, flpc25, pr_25, rh_25, sirc25,
     &   td_25, tdp25, gpht25, hght25, 
     &   wspd25, wdir25
 100  format(i5.5,1x,i5.5,1x,i2,1x,i2,
     &   1x,i4.4,4(1x,i2),1x,f5.2,
     &   1x,f9.5,1x,f10.5,1x,i5,1x,i5,1x,i2,
     &   1x,i4.4,4(1x,i2),1x,f5.2,
     &   1x,i2,1x,i4,1x,i6,1x,f5.1,1x,i2,
     &   1x,f5.1,1x,f5.1,1x,i5,1x,i5,
     &   1x,f6.1,1x,i3)
c
      RETURN                                                            
      END                                                               
