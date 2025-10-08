#!/usr/bin/perl 

#-------------------------------------------------------
# det_miss_stnids.pl - This perl script/program takes
#   a list of input stations and determines which of
#   those stations is NOT in a given station list.
#   (Hash table searching - appears to work and is
#   extremely fast.)
# 
# 98 Jul lec
#   Created.
#-------------------------------------------------------*/
$debug = 0;

#-----------------------------
# First get and print the time
#-----------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "\ndet_miss_stnids.pl began at $year/$mon/$mday $hour:$min:$sec\n";

#---------------------------------------
# Open the input and output files.
# Update to read name from input 
# line and create output file name.
#----------------------------------------
print "Processing input file.\n";

open (INSTNFILE, "nwsli.uniq") || die "Can NOT open file hash.inp for reading";
open (INCHECKFILE, "stnids") || die "Can NOT open input file stnids for reading";
open (OUTFILE, ">stns_not_in_list.out") || die "Can NOT open output file for reading";
open (OUTFILE2, ">stns_totalUniq_in_nwsli.out") || die "Can NOT open output2 file for reading";

#---------------------------------
# Read in the given station list.
# Fill the associative array.
#---------------------------------
%stnIDs = ();
%stnIDs_secondary = ();

$p0ct = 0;
$p4ct = 0;
$p4Dash = 0;

while ($line = <INSTNFILE>)
   {
   chop ($line);

#
#  Split fails if a field is missing
#  and have learned that there are missing
#  fields for some lines, so better to substr.
#
#  @parts = split (" ", $line);
#--------------------------------------------

   $junk1 = substr ($line, 0, 9);
   @parts0 = split (" ", $junk1);

   $junk2 = substr ($line, 32, 10); # many times missing (i.e., --')
   @parts4 = split (" ", $junk2);

   $parts7 = substr ($line, 52, 24);

#   print "\nline:xxx", $line, "xxx\n";
#   print "junk1:xxx", $junk1, "xxx\n";
#   print "junk2:xxx", $junk2, "xxx\n";
#
#   print "parts0[0]:xxx", $parts0[0], "xxx\n";
#   print "parts[1]:xxx", $parts[1], "xxx\n";
#   print "parts[2]:xxx", $parts[2], "xxx\n";
#   print "parts[3]:xxx", $parts[3], "xxx\n";
#   print "parts4[0]:xxx", $parts4[0], "xxx\n";
#   print "parts[5]:xxx", $parts[5], "xxx\n";
#   print "parts[6]:xxx", $parts[6], "xxx\n";
#   print "parts7:xxx", $parts7, "xxx\n";
#
#-----------------------------------------------
#   Pick out the primary and secondary IDs and
#   put them into hash tables (assoc. arrays).
#-----------------------------------------------

   print OUTFILE2 $parts0[0],"\n";
   print OUTFILE2 $parts4[0],"\n";

   if ($parts0[0] ne "--") # Never happens??
      {
      $p0ct++;

      $stnIDs{ $parts0[0] } = $parts7;
      }
   else
      {
      print "Found -- in parts[0]!!\n"
      }

   if ($parts4[0] ne "--") # Happens frequently.
      {
      $p4ct++;
      $stnIDs_secondary{ $parts4[0] } = $parts7;
      }
   else 
      {
#      print "Found -- in parts4[0]!!\n";
      $p4Dash++;
      }

   } # Fill associative array.


print "p0ct, p4ct, p4Dash = ", $p0ct,"  ", $p4ct,"  ", $p4Dash, "\n";


#----------------------------------
# Read a line from file. Parse it.
# Only write out lines if stn is
# NOT in given list.
#----------------------------------
$i = 0;
while ($line = <INCHECKFILE>) 
   { 
   $stn = substr( $line, 0, 9);
   chop ($stn);

   if ($debug)
      {  
      printf "Read line: $line\n";
      printf "stn=:xxx%sxxx\n", $stn; 
      }

   if ( length ( $stnIDs{ $stn } ) != 0 ||
        length ( $stnIDs_secondary{ $stn } ) != 0)
      {
      if ($debug)
         {
         printf "Found Station.\n";
         }
      }
   else
      {
      if ($debug)
         {
         printf "Station NOT found.\n";
         }

      # Write to output file.
      print OUTFILE "Station not in list: " , $stn, "\n"

      }

   }  # while data in file

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "det_miss_stnids.pl ended at $year/$mon/$mday $hour:$min:$sec\n";

close(INFILE);
close(OUTFILE);
