c 
c include file "ebufr_data.inc"
c
c***********************************************************************
c  This file contains the data definitions needed by
c  the E-BUFR read routines.  This file should be included in all
c  main programs that use the E-BUFR library.
c
c  This include file should be included in the main program 
c  after all variable declarations and before any local data
c  statements.
c
c***********************************************************************
c    Initialize descriptors that must be known by an E-BUFR program
c***********************************************************************
      data curdes/19/
      data (xtab(i),i=1,19)/17*0,5,6/
      data (ytab(i),i=1,19)/010,011,012,013,014,015,016,017,018,019,020,
     +          255,254,253,252,251,250,1,1/
      data (scale(i),i=1,19)/17*0,5,5/
      data (ref(i),i=1,19)/17*0,-9000000,-18000000/
      data (name1(i),i=1,19)/
     +     'F of descriptor','X of descriptor','Y of descriptor',
     +     'Element name line 1','Element name line 2',
     +     'Units name','Units scale sign','Units scale',
     +     'Units reference sign','Units reference value',
     +     'Element data width',
     +     'Code figure/flag','Code/flag desc. 1',
     +     'Code/flag desc. 2','Code/flag desc. 3',
     +     'Code/flag desc. 4','Code/flag desc. 5',
     +     'Latitude (high accuracy)','Longitude (high accuracy)'/
      data (name2(i),i=1,19)/19*' '/
      data (nbits(i),i=1,19)/8,16,24,256,256,192,8,24,8,80,24,
     +     24,256,256,256,256,256,25,26/
      data (units(i),i=1,19)/17*'CCITT IA5',2*'Degrees'/
c***********************************************************************
c   End of Include file "ebufr_data.inc 
c***********************************************************************
c
