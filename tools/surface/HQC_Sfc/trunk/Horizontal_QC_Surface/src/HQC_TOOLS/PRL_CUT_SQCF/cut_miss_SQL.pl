#!/usr/bin/perl 

#-----------------------------------------------------------------
# cut_miss_SQL.pl - This perl script/program reads SQCF (streamflow
#   recs and strips requested stns/months.
# 
# 98 Oct lec
#   Created.
#-----------------------------------------------------------------*/
$debug = 0;

#-----------------------------
# First get and print the time
#-----------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "\ncut_miss_SQL.pl began at $year/$mon/$mday $hour:$min:$sec\n";

#---------------------------------------
# Open the input and output files.
# Update to read name from input 
# line and create output file name.
#----------------------------------------
print "Processing input file: $ARGV[0]\n";

print "Output file name is: $ARGV[0].strip\n";

open (INFILE, "$ARGV[0]") || die "Can NOT open input file for reading";
open (OUTFILE, ">$ARGV[0].strip") || die "Can NOT open output file for reading";

@d_per_mo = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);

$dropped_recs = 0;

#-------------------------------------
# For the open file, read all records.
#-------------------------------------
while ($line = <INFILE>) 
   { 
   #----------------------------------
   # Read a line from file. Parse it.
   # Only write out non-missing lines.
   #----------------------------------
   $year = substr( $line, 0, 4);
   $month = substr( $line, 5, 2);
   $stn = substr( $line, 19, 15);

   $miss_days = 0;

   $num_days_in_month = $d_per_mo[month-1];

   if ((year%4==0 && year%100 !=0 || year%400==0) && (month == 2))
      {
      $num_days_in_month++;
      }


   #--------------------------------------
   # Places for streamflow data are: 
   #     36,56,76,96,...,656  for 7 chars. 
   #--------------------------------------
   for ($place =36; $place < 670; $place +=20)
      {
      $day = substr( $line, $place, 7);

      if ($day <= -999.0)
         {
         $miss_days++;
         }
      }

   if ($miss_days != $num_days_in_month)
      {
      # At least one good day so print.
      print OUTFILE "$line";
      }
   else
      {
      # Drop this record since it has all missing days.
      $dropped_recs++;
      print "Drop record $year/$month for $stn\n";
      print "NOTE: WHOLE MONTH MISSING FOR: $year/$month $stn-> REMOVE FROM OUTPUT. \n"
      }

   }  # while data in file

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "cut_miss_SQL.pl ended at $year/$mon/$mday $hour:$min:$sec\n";

close(INFILE);
close(OUTFILE);
