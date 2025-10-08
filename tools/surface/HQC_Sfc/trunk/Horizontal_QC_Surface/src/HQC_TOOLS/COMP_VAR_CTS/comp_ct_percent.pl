#!/usr/bin/perl 

#Try -w option after perl

#---------------------------------------------
# comp_ct_percent.pl - this PERL script
#   takes the output from the compute_var_ct.c
#   program and helps to determine the
#   ranges that contain say 95% of
#   the ave sigmasq values in the 
#   sigmasq files used in the HQC processing.
#   This script could be improved. It was
#   created quickly to get results for
#   documentation and presentations. Currently,
#   the user MUST modify the ranges for each
#   parameter as required for the current
#   project. The original bin guesses below
#   were made after graphing the data in
#   histogram form and estimating the 95% ranges.
#
# input: var_ct.out with header removed and
#   renamed to var_ct.in. var_ct.out is
#   the output file from compute_var_ct.c.
#
# output: var_ct_percents.out.
#
# circa 1995 lec
#   Created.
#
# 22 May 98 lec
#   Added some documentation.
#---------------------------------------------

#-----------------------------
# First get and print the time
#-----------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "Comp_ct_percent began at $year/$mon/$mday $hour:$min:$sec\n";

#---------------------------------
# Open the input and output files.
#---------------------------------
open(INFILE, "var_ct.in") || die "Can NOT open input file for reading";

open(OUTFILE, ">var_ct_percents.out") || die "Can NOT open output file for reading";

#---------------------------------------------------------
# For each var in file opened, Read and sum all the variances cts
# each parameter type. Then compute the percent cts falling
# within each span.
#---------------------------------------------------------
# The 7 parms are numbered:
# are ordered:
# 0=stnPress, 1=SLP, 2=CSLP,
# 3=Temp, 4=DewPt, 5=WindSP,
# 6=WindDir.
#-----------------------------
@count = (0,0,0,0,0,0,0);
@parm_sum = (0,0,0,0,0,0,0);
@parm_percent  = (0.0,0.0,0.0,0.0,
                  0.0,0.0,0.0);

while ($line = <INFILE>) 
   { 
   #----------------------------------
   # Read a line from file. Parse it.
   #----------------------------------
#   chop($line);
   printf "Read line: $line\n";
   $datatype = substr( $line, 0, 7);
   $bin_num  = substr( $line, 8, 8);
   $bin_range = substr( $line, 15, 8);
   $cts = substr( $line, 24, 9);

   printf "datatype=: %d\n", $datatype;
   printf "bin_num=: %d\n", $bin_num;
   printf "bin_range=: %d\n", $bin_range;
   printf "cts=: %d\n", $cts;


   if ($datatype == 0) #Stn Pressure
      {
      printf "datatype=0\n";

      $count[$datatype] = $count[$datatype]+ $cts;

      if ($bin_range >= 15 && $bin_range <=60)
         {
         $parm_sum[$datatype] = $parm_sum[$data_type] + $cts;
         printf "Data in range for datatype = 0, parm_sum[$data_type]= %d\n",
                 $parm_sum[$data_type];
         }
      else
         {
         printf "Data NOT in range for datatype = 0, parm_sum[%d]= %d\n",
                $data_type, $parm_sum[$data_type];
         }
      }
   elsif ($datatype ==1) # SLP
      {
      printf "datatype=1";
      $count[$datatype] = $count[$datatype]+ $cts; 
      if ($bin_range >= 10 && $bin_range <=75) #10-75=97%,10-70=85%
         {
         $parm_sum[$datatype] += $cts; 
         } 

      }
   elsif ($datatype ==2)  # CSLP
      {
      $count[$datatype] = $count[$datatype]+ $cts; 
      if ($bin_range >= 5 && $bin_range <=95) # 5-85=93%,5-90=93%,5-95=93%,5-100=99%
         {
         $parm_sum[$datatype] += $cts; 
         } 

      }
   elsif ($datatype ==3)  # temp
      {
      $count[$datatype] = $count[$datatype]+ $cts; 
      if ($bin_range >= 10 && $bin_range <=35) #10-35 = 96.6%,10-30=91%
         {
         $parm_sum[$datatype] += $cts; 
         } 

      }
   elsif ($datatype ==4) # dew pt
      {
      $count[$datatype] = $count[$datatype]+ $cts; 
      if ($bin_range >= 10 && $bin_range <=40)
         {
         $parm_sum[$datatype] += $cts; 
         } 

      }
   elsif ($datatype ==5) # Wind Speed
      {
      $count[$datatype] = $count[$datatype]+ $cts; 
      if ($bin_range >= 2 && $bin_range <=11)
         {
         $parm_sum[$datatype] += $cts; 
         } 

      }
   elsif ($datatype == 6) #wind direction
      {
      $count[$datatype] = $count[$datatype]+ $cts; 
      if ($bin_range >= 6500 && $bin_range <=16500) #6500 = 94%,  6000=96.5%
         {
         $parm_sum[$datatype] += $cts; 
         } 

      }

   } # while INFILE

#----------------------------------------
# Print out the percentages of variances
# that fall within bin_range cutoffs
# hardcoded above.
#----------------------------------------
for ($jj = 0; $jj<7; $jj++)
   {
   if ($count[$jj] != 0)
      {
      $parm_percent[$jj] = $parm_sum[$jj]/$count[$jj];
#      printf "Do the division, parm_percent[%d]=%f\n", $jj, $parm_percent[$jj];
      }
   else
      {
#      printf "Don't do the division\n";
      }

   printf "Parm#, Total_ct, Parm_ct_in_range, Parm_%_in_range: %d %d %d %f\n",
          $jj,$count[$jj], $parm_sum[$jj], $parm_percent[$jj];

   printf OUTFILE "Parm#, Total_ct, Parm_ct_in_range, Parm_%_in_range: %d %d %d %f\n", 
          $jj, $count[$jj], $parm_sum[$jj], $parm_percent[$jj]; 
   }

printf "Comp_ct_percent ended at $year/$mon/$mday $hour:$min:$sec\n";

close(INFILE);
close(OUTFILE);
