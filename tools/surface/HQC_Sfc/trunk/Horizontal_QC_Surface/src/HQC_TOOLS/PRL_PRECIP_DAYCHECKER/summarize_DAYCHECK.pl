#!/usr/bin/perl 

#-------------------------------------------------------
# summarize_DAYCHECK.pl - This perl script/program reads
#   the output from check_PQC_DAY.pl and computes total
#   stats by day over every month included in output from
#   check_PQC_DAY.pl.
# 
# 99 Oct lec
#   Created.
#-------------------------------------------------------*/
$debug = 0;
   
#-----------------------------
# First get and print the time
#-----------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "\n-----summarize_DAYCHECK.pl began processing $ARGV[0] at $year/$mon/$mday $hour:$min:$sec\n";

#---------------------------------------
# Open the input and output files.
#----------------------------------------
open (INFILE, "$ARGV[0]") || die "Can NOT open input file for reading $ARGV[0]";

$number_lines_read = 0;

# Force to 0 
for ($ii=1; $ii<32; $ii++)
   {
   $tot_day_good[$ii] = 0;
   $tot_day_dubious[$ii] = 0;
   $tot_day_bad[$ii] = 0;
   $tot_day_missing[$ii] = 0;
   $tot_day_unchecked[$ii] = 0;
   }

#-------------------------------------------------
# Read all Daily precip records. For each day
# compute stats: number of G, D, B, U, max, min.
#-------------------------------------------------
while ($line = <INFILE>) 
   { 
   #----------------------------------
   # Read a line from file. Parse it.
   #----------------------------------
   $number_lines_read++;

   if ($debug)
      {
      printf "\nRead line: \n$line"; 
      }

   #----------------------------------
   # Read a line from file. Parse it.
   #----------------------------------
   $line_part1 = substr( $line, 0, 15);

   if ( $line_part1 ne    "-----check_PQC_"  &&
        $line_part1 ne    "Number of recor"  &&
        $line_part1 ne    "Day       Good "  &&
        $line_part1 ne    "---------------"
        )
      {
      #------------------------------
      # Split line. Order of split is
      # always: Day, Good, Dubious,
      # Bad, Missing, Unchecked.
      #------------------------------
      @counts = split(' ', $line);

      $ii = $counts[0];

      $tot_day_good[$ii]      += $counts[1];
      $tot_day_dubious[$ii]   += $counts[2];
      $tot_day_bad[$ii]       += $counts[3];
      $tot_day_missing[$ii]   += $counts[4];
      $tot_day_unchecked[$ii] += $counts[5];

      } # if line portion
   }  # while data in file



printf ("Number of records read: %d\n\n", $number_lines_read);

printf "Totals for All Days in File/time period.\n\n";

printf "Day   TOT_Good  TOT_Dubious   TOT_Bad  TOT_Missing  TOT_Unchecked\n";
printf "-----------------------------------------------------------------\n";

for ($ii=1; $ii<=31; $ii++)
   {
   printf "%3d %10d %10d %10d %10d %10d\n",
           $ii, $tot_day_good[$ii], $tot_day_dubious[$ii],
           $tot_day_bad[$ii], $tot_day_missing[$ii], 
           $tot_day_unchecked[$ii];
   }

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "-----summarize_DAYCHECK.pl ended processing $ARGV[0] at $year/$mon/$mday $hour:$min:$sec\n\n";

close(INFILE);
close(OUTFILE);
