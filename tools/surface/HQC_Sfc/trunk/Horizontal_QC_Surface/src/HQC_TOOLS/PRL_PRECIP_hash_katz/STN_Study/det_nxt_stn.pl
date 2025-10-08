#!/usr/bin/perl 

#-------------------------------------------------------
# det_nxt_stn.pl - This perl script/program takes
#   a list of input stations and determines which of
#   those stations is NOT in a given station list.
#   Crude searching, but it works.
# 
# 98 Jul lec
#   Created.
#-------------------------------------------------------*/
$debug = 0;
$debug1 = 0;

#-----------------------------
# First get and print the time
#-----------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "\ndet_nxt_stn.pl began at $year/$mon/$mday $hour:$min:$sec\n";

#---------------------------------------
# Open the input and output files.
# Update to read name from input 
# line and create output file name.
#----------------------------------------
print "Processing input file: nwsli.uniq\n";

open (INSTNFILE, "nwsli.uniq") || die "Can NOT open file hash.inp for reading";
open (INCHECKFILE, "stnids") || die "Can NOT open input file stnids for reading";
open (OUTFILE, ">stns_nxt_miss.out") || die "Can NOT open output file for reading";

#---------------------------------
# Read in the given station list.
# Fill the associative array.
#---------------------------------
$ii = 0;

while ($line = <INSTNFILE>)
   {
   chop ($line);

   @parts = split (" ", $line);

#   print "\nline:xxx", $line, "xxx\n";
#   print "parts[0]:xxx", $parts[0], "xxx\n";
#   print "parts[1]:xxx", $parts[1], "xxx\n";
#   print "parts[2]:xxx", $parts[2], "xxx\n";
#   print "parts[3]:xxx", $parts[3], "xxx\n";
#   print "parts[4]:xxx", $parts[4], "xxx\n";
#   print "parts[5]:xxx", $parts[5], "xxx\n";
#   print "parts[6]:xxx", $parts[6], "xxx\n";
#   print "parts[7]:xxx", $parts[7], "xxx\n";
#
#   Pick out the primary and secondary IDs.
#

   $nws_stns1[$ii] = $parts[0];
   $nws_stns2[$ii] = $parts[4];

   $ii++;

   } # Fill given station array

$nws_stn_ct = $ii;

print "nws_stn_ct = ", $nws_stn_ct, "\n";

#----------------------------------
# Read a line from file. Parse it.
# Only write out lines if stn is
# NOT in given list.
#----------------------------------
while ($line = <INCHECKFILE>) 
   { 
   $stn = substr( $line, 0, 9);
   chop ($stn);

   if ($debug)
      {  
      printf "Read line: $line\n";
      printf "stn=:xxx%sxxx\n", $stn; 
      printf "Begin searching for stn=:xxx%sxxx\n", $stn; 
      }

   $jj = 0;
   $stn_found = 0;

   while ($jj < $nws_stn_ct)
      {
      if ($debug)
         {
         print "COMPARE:: jj, nws_stns1, nws_stns2, stn: ", $jj," ", 
                $nws_stns1[$jj], " ",
             $nws_stns2[$jj], " ", $stn, "\n";
         }

      if ( $nws_stns1[$jj] eq $stn
           || $nws_stns2[$jj] eq $stn)
         {
         if ($debug1)
            {
            printf "Found Station (%-s) at line %d.\n", $stn, $jj;
            }

         $stn_found = 1;
         break;
         }

      $jj++;

      } # while jj

   if (!$stn_found)
     {
     if ($debug1)
        {
        printf "Station ( %-s) NOT found.\n", $stn;
        }

     # Write to output file.
     print OUTFILE "Station not in list: " , $stn, "\n";
     }
   else
     {
     if ($debug1)
        {
        printf "Station ( %-s) found.\n", $stn;
        }
     }

   }  # while data in file

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "det_nxt_stn.pl ended at $year/$mon/$mday $hour:$min:$sec\n";

close(INFILE);
close(OUTFILE);
