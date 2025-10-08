#!/usr/bin/perl 

#-------------------------------------------------------------------
# reset_unheated_pcp.pl - This perl script/program reads QCF records
#   and determines if the current rec contains precip from an
#   unheated gage. If so, reset the precip QC flag to be Dubious.
#   For ESOP97, we assume the whole time period is winter.
# 
# 99 May lec
#   Created.
#------------------------------------------------------------------*/
$debug = 0;

#-----------------------------
# First get and print the time
#-----------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "\nreset_unheated_pcp.pl began at $year/$mon/$mday $hour:$min:$sec\n";

#---------------------------------------
# Open the input and output files.
# Update to read name from input 
# line and create output file name.
#
# Input names are ESOP97_HSC_YYMMDD.qcf.
# Output names should be unheated_YYMMDD.qcf.
#----------------------------------------
print "Processing input file: $ARGV[0]\n";

$YYMMDD = substr( $ARGV[0], 11, 10);
print "Output file name is: unheated_$YYMMDD\n";

open (INFILE, "$ARGV[0]") || die "Can NOT open input file for reading";
open (OUTFILE, ">unheated_$YYMMDD") || die "Can NOT open output file for reading";

$rec_count = 0;

#--------------------------------------
# For the open file, read all records.
# Fix and write to output file.
#--------------------------------------
while ($line = <INFILE>) 
   { 
   $rec_count++;

   #----------------------------------
   # Read a line from file. Parse it.
   #----------------------------------
   $network  = substr( $line, 30, 10);
   $stn      = substr( $line, 41, 15);

   $line_seg1     = substr( $line,   0, 170);
   $precip_QCflag = substr( $line, 170,   1);
   $line_seg2     = substr( $line, 171,  86);

   if ($debug)
      {  
      printf "\nRead line: $line\n";
      printf "network=: xxx%-sxxx\n", $network;
      printf "stn=: xxx%-sxxx\n", $stn;
      printf "\nline_seg1: xxx%-sxxx\n", $line_seg1;
      printf "\nprecip_QCflag: xxx%-sxxx\n", $precip_QCflag;
      printf "\nline_seg2: xxx%-sxxx\n", $line_seg2;
      }  

   if ($network eq "MOCAWS    " || $network eq "NSTL      " || $network eq "WI_AWON   ")
      {
      if ($precip_QCflag eq "U" || $precip_QCflag eq "G" || $precip_QCflag eq "T")
         {
         printf "Reset $network $stn precip QC flag ($precip_QCflag) to D at rec $rec_count.\n";

         $precip_QCflag = "D";
         }
      }

   printf OUTFILE "%-s%-s%-s", $line_seg1,$precip_QCflag,$line_seg2;

   }  # while data in file

printf "Total records processed: $rec_count\n";

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "reset_unheated_pcp.pl ended at $year/$mon/$mday $hour:$min:$sec\n";

close(INFILE);
close(OUTFILE);
