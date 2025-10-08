#!/usr/bin/perl 

#-------------------------------------------------------
# correct_lon.pl - This perl script/program reads QCF records
#   and determines if the station in the record should
#   have it's lon corrected to a negative number.
# 
# 98 July lec
#   Created.
#-------------------------------------------------------*/
$debug = 0;
   
#-----------------------------
# First get and print the time
#-----------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "\ncorrect_lon.pl began at $year/$mon/$mday $hour:$min:$sec\n";

#---------------------------------------
# Open the input and output files.
#
# Input names are YYMMDD.qcf.
# Output names will be 
# lon_YYMMDD.qcf. 
#----------------------------------------
print "Processing input file: $ARGV[0]\n";

$YYMMDD = substr( $ARGV[0], 0, 10);
print "Output file name is: lon_$YYMMDD\n";

open (INFILE, "$ARGV[0]") || die "Can NOT open input file for reading";
open (OUTFILE, ">lon_$YYMMDD") || die "Can NOT open  output file for reading";

#---------------------------------------------------------
# For the open file, read all records, determine 
# which records should have their lons corrected. Write all
# records to the output file. Output file sizes should
# be identical.
#---------------------------------------------------------
while ($line = <INFILE>) 
   { 
   #----------------------------------
   # Read a line from file. Parse it.
   #----------------------------------
   $stn_info   = substr ($line, 30,  53);
   $begin_line = substr ($line,  0,  70);
   $lon        = substr ($line, 70, 9);
   $end_line   = substr ($line, 79, 180);


   if ($debug)
      {  
      printf "\nRead line: $line";
      printf "stn_info=: xxx%sxxx\n", $stn_info; 
      printf "begin_line=: xxx%sxxx\n", $begin_line; 
      printf "end_line=: xxx%sxxx\n", $end_line; 
      }  

   if ( $stn_info eq "baker      princeton         45.52508    93.62237   0")
      {
      if ($debug)
         {
         printf "This stn needs lon corrected set.\n";
         }

      #------------------------
      # Set this station's lon. 
      #------------------------
      $lon = -93.62237;
      }

   if ($debug)
      {
      printf "%-s%1s%-s\n", $begin_line,$lon,$end_line;
      }
 
   printf OUTFILE "%-s%1s%-s", $begin_line,$lon,$end_line;

   }  # while data in file

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "correct_lon.pl ended at $year/$mon/$mday $hour:$min:$sec\n";

close(INFILE);
close(OUTFILE);
