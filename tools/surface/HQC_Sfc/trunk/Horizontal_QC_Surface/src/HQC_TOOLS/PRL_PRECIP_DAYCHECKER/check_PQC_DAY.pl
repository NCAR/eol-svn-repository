#!/usr/bin/perl 

#-------------------------------------------------------
# check_PQC_DAY.pl - This perl script/program reads
#   daily precip records and computes stats by day
#   within the specified file.
# 
# 99 Oct lec
#   Created.
#-------------------------------------------------------*/
$debug = 0;
   
#-----------------------------
# First get and print the time
#-----------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "\n-----check_PQC_DAY.pl began processing $ARGV[0] at $year/$mon/$mday $hour:$min:$sec\n";

#---------------------------------------
# Open the input and output files.
#----------------------------------------
open (INFILE, "$ARGV[0]") || die "Can NOT open input file for reading $ARGV[0]";

$number_lines_read = 0;

# Force to 0 
for ($ii=1; $ii<32; $ii++)
   {
   $day_good[$ii] = 0;
   $day_dubious[$ii] = 0;
   $day_bad[$ii] = 0;
   $day_missing[$ii] = 0;
   $day_unchecked[$ii] = 0;
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

   $i = 72; # Position of first QCflag

   for ($ii=1; $ii<32; $ii++) # Skip 0 position. Put cts in 1-31.
      {
      $QCflag = substr ($line, $i, 1);

      if ($debug)
         {
         printf "ii,i: $ii, $i\n";
         printf "QCflag: $QCflag\n";
         }

      if ($QCflag eq "G")
         {
         $day_good[$ii]++;
         }

      if ($QCflag eq "D")
         {
         $day_dubious[$ii]++;
         }

      if ($QCflag eq "B")
         {
         $day_bad[$ii]++;
         }

      if ($QCflag eq "M")
         {
         $day_missing[$ii]++;
         }

      if ($QCflag eq "U")
         {
         $day_unchecked[$ii]++;
         }


      $i= $i+15;

      if ($debug)
         {
         printf "(start at one)day_good[1]: $day_good[1]\n";
         printf "(start at one)day_dubious[1]: $day_dubious[1]\n";
         printf "(start at one)day_bad[1]: $day_bad[1]\n";
         printf "(start at one)day_missing[1]: $day_missing[1]\n";
         printf "(start at one)day_unchecked[1]: $day_unchecked[1]\n\n";

         printf "(start at one)day_good[2]: $day_good[2]\n";
         printf "(start at one)day_dubious[2]: $day_dubious[2]\n";
         printf "(start at one)day_bad[2]: $day_bad[2]\n";
         printf "(start at one)day_missing[2]: $day_missing[2]\n";
         printf "(start at one)day_unchecked[2]: $day_unchecked[2]\n\n";

         printf "(start at one)day_good[31]: $day_good[31]\n";
         printf "(start at one)day_dubious[31]: $day_dubious[31]\n";
         printf "(start at one)day_bad[31]: $day_bad[31]\n";
         printf "(start at one)day_missing[31]: $day_missing[31]\n";
         printf "(start at one)day_unchecked[31]: $day_unchecked[31]\n\n";
         }

       } # for each day

   }  # while data in file

printf ("Number of records read: %d\n\n", $number_lines_read);

printf "Day       Good      Dubious      Bad     Missing   Unchecked\n";
printf "------------------------------------------------------------\n";
for ($ii=1; $ii<=31; $ii++)
   {
   printf "%3d %10d %10d %10d %10d %10d\n",
           $ii, $day_good[$ii], $day_dubious[$ii],
           $day_bad[$ii], $day_missing[$ii], $day_unchecked[$ii];
   }

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "-----check_PQC_DAY.pl ended processing $ARGV[0] at $year/$mon/$mday $hour:$min:$sec\n\n";

close(INFILE);
close(OUTFILE);
