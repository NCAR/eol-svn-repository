#!/usr/bin/perl 

#--------------------------------------------------------------
# summ_pcp_days.pl - This perl script/program reads Daily PQCF records
#   and summs the values to form monthly totals.
# 
# 99 Dec lec
#   Created.
#--------------------------------------------------------------*/
$debug = 1;

#-----------------------------
# First get and print the time
#-----------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "\nsumm_pcp_days.pl began at $year/$mon/$mday $hour:$min:$sec\n";

#---------------------------------------
# Open the input and output files.
# Update to read name from input 
# line and create output file name.
#----------------------------------------
print "Processing input file: $ARGV[0]\n";

print "Output file name is: $ARGV[0].summ\n";

open (INFILE, "$ARGV[0]") || die "Can NOT open input file for reading";
open (OUTFILE, ">$ARGV[0].summ") || die "Can NOT open output file for reading";

printf OUTFILE "Data from file: %-s\n", $ARGV[0];

#--------------------------------------
# Count negative zeroes that are found.
#--------------------------------------
$rec_count = 0;
$precip_monthly_total = 0.0;
$precip_TOI_total = 0.0;

#--------------------------------------
# For the open file, read all records.
# Fix and write to output file.
#--------------------------------------
while ($line = <INFILE>) 
   { 
   $rec_count++;

   if ($debug)
      {
      printf "\nRead line: $line\n";
      }

   #----------------------------------
   # Read a line from file. Parse it.
   #----------------------------------

   $date_YYYYDD = substr ($line, 0, 7);
   $day = 1;

   $precip_monthly_total = 0.0;

   for ($i=0;$i<31;$i++)
      {
      $precip = substr( $line, 62+$i*15, 7);
       
      if ($precip > -990.00)
         {
         $precip_monthly_total = $precip_monthly_total + $precip;
         $precip_TOI_total = $precip_TOI_total + $precip;
         }

      if ($debug)
         {
         printf "precip: $precip\n";
         printf "precip_TOI_total: $precip_TOI_total\n";
         printf "precip_monthly_total: $precip_monthly_total\n";
         }
      }

   printf OUTFILE "\nDate: %-s\n", $date_YYYYDD;

   printf OUTFILE "   Monthly Total (mm,in):%7.2f  %7.2f\n",
       $precip_monthly_total, ($precip_monthly_total/25.4);

   printf OUTFILE "   Time Of Interest Total (mm,in):%7.2f  %7.2f\n", 
       $precip_TOI_total, ($precip_TOI_total/25.4);

   }  # while data in file

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "summ_pcp_days.pl ended at $year/$mon/$mday $hour:$min:$sec\n";

close(INFILE);
close(OUTFILE);
