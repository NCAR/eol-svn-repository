C structure for configuration input file (for variance limits, etc.)
      real varb(NQCPARMS)	!variance amounts for bad qc flag
      real varq(NQCPARMS)	!variance amounts for questionable qc flag
      real varo(NQCPARMS)	!offset for variance file parm values
      real rocb(NQCPARMS)	!rate of change amounts for bad qc flag
      real rocq(NQCPARMS)	!rate of change amounts for questionable qc flag
      character*8 day1		!day 1 for dates in variance file
	 integer*2 iday1
      common /configcom/ varb,varq,varo,rocb,rocq,day1,iday1
