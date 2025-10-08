C array containing summary statistics for all qc'd fields
      integer stats(NQCPARMS,NQCFLAGS)
      real dev(NANALPARMS+1,20)
      integer tosscnt(68,2)
      common /qccom/ stats,dev,tosscnt

C statistics array stats(x,y):
C
C    x = 1 to NQCPARMS : one row for each parameter
C
C    y = 1 to NQCFLAGS : one col for each qc flag
C
C      = 1 : for 'G' qc flag
C      = 2 : for 'B' qc flag
C      = 3 : for 'D' qc flag
C      = 4 : for 'M' qc flag
C      = 5 : for 'N' qc flag
C      = 6 : for 'X' qc flag
C      = 7 : for 'U' qc flag
C      = 8 : for 'E' qc flag
C      = 9 : for 'I' qc flag

C deviation array dev(x,y):
C
C    x = 1 to NANALPARMS+1 : one row for each parameter
C                            (station pressure through precip)
C
C    y = 1 to 20 : one col for each variance sum
C
C      = 1 : holds sum of abs of variance for all records
C      = 2 : holds sum of abs of variance for records flagged good
C      = 3 : holds sum of abs of variance for records flagged bad
C      = 4 : holds sum of abs of variance for records flagged questionable
C      = 5 : holds maximum variance for all records
C      = 6 : holds maximum variance for records flagged good
C      = 7 : holds maximum variance for records flagged bad
C      = 8 : holds maximum variance for records flagged questionable
C      = 9 : holds sum of variance**2 for all records
C      = 10 : holds sum of variance**2 for records flagged good
C      = 11 : holds sum of variance**2 for records flagged bad
C      = 12 : holds sum of variance**2 for records flagged questionable
C      = 13 : holds sum of negative variances for all records
C      = 14 : holds sum of negative variances for records flagged good
C      = 15 : holds sum of negative variances for records flagged bad
C      = 16 : holds sum of negative variances for records flagged questionable
C      = 17 : holds sum of positive variances for all records
C      = 18 : holds sum of positive variances for records flagged good
C      = 19 : holds sum of positive variances for records flagged bad
C      = 20 : holds sum of positive variances for records flagged questionable

C toss count totals array tosscnt(x,y):
C
C    x = 1 to 68 : one row for each possible qc toss file code
C
C      = 1-17 : value compared to maps (one for each parm)
C      = 18-34 : value compared to rate of change check (one for each parm)
C      = 35-51 : misc checks (one for each parm)
C      = 52-68 : misc checks (one for each parm)
C
C      used :
C      = 1-7 : value compared to maps for stn press through wind dir
C      = 8 : value compared to gross limits for precip
C      = 9 : value compared to gross limits for squall/gust wind speed
C      = 13 : for precip with cld amt 1 =0 (clear day)
C      = 18-24 : rate of change check for stn press through wind dir
C      = 25 : rate of change check for precip
C      = 39 : for dew point > temperature
C      = 40 : for negative wind speed
C      = 41 : for wind dir < 0 or > 360
C      = 42 : for precip < 0
C      = 43 : for squall/gust wind speed < 0
C      = 58 : for wind dir flag set same as wind speed flag for calm winds
C
C    y = 1 to 2 : one col for bad and one for questionable
C      = 1 : for 'B' qc flag
C      = 2 : for 'D' qc flag
