#!/usr/bin/perl 

#Try -w option after perl

#-----------------------------
# First get and print the time
#-----------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "Compave began at $year/$mon/$mday $hour:$min:$sec\n";

#---------------------------------
# Open the input and output files.
#---------------------------------
open(INFILE, "19941210000.sig") || die "Can NOT open input file for reading";

open(OUTFILE, ">aveVariances.out") || die "Can NOT open output file for reading";

#---------------------------------------------------------
# For each file opened, Read and sum all the variances for
# each parameter type. Then compute the average for each
# type and write to the output file for later plotting.
#---------------------------------------------------------
$numstns = <INFILE>;  # Does this line read all lines or just one?
++$numstns;
printf "%5d\n", $numstns;

#----------------------------
# Skip first numstns+1 lines.
#----------------------------
$j = 0;
while ($j < $numstns)
   {
   <INFILE>;
   ++$j;
   printf "(first loop) j: %d\n", $j;
   }
   
#-----------------------------
# Read second half of file in
# different format. Parse line
# and compute each parameter's
# average and std. The 7 parms
# are ordered:
# 0=stnPress, 1=SLP, 2=CSLP,
# 3=Temp, 4=DewPt, 5=WindSP,
# 6=WindDir.
#-----------------------------
@count = (0,0,0,0,0,0,0);
@parm_sums = (0.0,0.0,0.0,0.0,
              0.0,0.0,0.0);
@parm_ave  = (0.0,0.0,0.0,0.0,
              0.0,0.0,0.0);

while ($line = <INFILE>) 
   { 
   #----------------------------------
   # Read a line from file. Parse it.
   # Compute average of input numbers.
   #----------------------------------
#   chop($line);
   printf "Read line: $line\n";
   $in1 = substr( $line, 0, 5);
   $in2 = substr( $line, 10, 1);
   $in3 = substr( $line, 12, 7); # 11

   printf "in1=: %d\n", $in1;
   printf "in2=: %d\n", $in2;
   printf "in3=: %d\n", $in3;

   ++$count[$in2];
   $parm_sums[$in2] += $in3;

   printf "count[], parm_sums[]: %d %f\n",  $count[$in2], $parm_sums[$in2];
   }

#---------------------------------
# Print out the std to files.
#---------------------------------
$jj = 0;
for ($jj = 0; $jj<7; $jj++)
   {
   $parm_ave[$jj] = $parm_sum[$jj]/$count[$jj];

   printf "jj, parm_sum, parm_ave, std: %d %f %f %f\n", 
          $jj, $parm_sum[$jj], $parm_ave[$jj], sqrt($parm_ave[$jj]);
   printf OUTPUT "%d %f %f\n", 
          $jj, $parm_ave[$jj], sqrt($parm_ave[$jj]);
   }

printf "Compave ended at $year/$mon/$mday $hour:$min:$sec\n";

close(INFILE);
close(OUTFILE);
