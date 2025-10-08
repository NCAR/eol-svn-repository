#!/usr/bin/perl 

#-------------------------------------------------------
# find_mobile_vars.pl - This perl script/program takes
#   input sigmasq files and determines if any mobile 
#   stations have legit var values.
# 
# 99 Mar lec
#   Created.
#-------------------------------------------------------*/
$debug = 0;

#-----------------------------
# First get and print the time
#-----------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "\nfind_mobile_vars.pl began at $year/$mon/$mday $hour:$min:$sec\n";

#---------------------------------------
# Open the input and output files.
# Update to read name from input 
# line and create output file name.
#----------------------------------------
print "Processing input file: $ARGV[0]\n";

system "gunzip $ARGV[0]";

$file_name = substr ($ARGV[0], 0, 15);

open (INFILE, "$file_name") || die "Can NOT open input file ($file_name) for reading";

#------------------------------------------------------
# Read in the given station list from the sigmasq file.
# Fill the associative array.
#------------------------------------------------------
%stnIDs = ();

$lines_read = 0;
$num_non_zero_mobile = 0;
$num_mobile_stns = 0;

while ($line = <INFILE>)
   {
   $lines_read++;
   chop ($line);

   if ($lines_read == 1)
      {
      # Get number of stns in this file.

      $num_stns = substr ($line, 0, 5);
      print "There are $num_stns stations in this sigmasq file.\n";
      }
   else
      {
      #---------------------------------------
      # Fill stn info into associative array.
      #---------------------------------------
      if ($lines_read <= $num_stns+1)
         {
         if ($debug)
            {
            print "Fill associative array\n";
            print "line: $line\n";
            }

         @parts = split (" ", $line);
         $stn_num = $parts[0]; # Stn number

         $network = substr ($line, 6, 8); # Network

         $stnIDs{ $stn_num } = $network;

         if ($debug)
            {
            print "stn_num:xxx", $stn_num,"xxx\n";
            print "network:xxx", $network, "xxx\n";
            print "stnIDs{ $stn_num } = ", $stnIDs{ $stn_num }, "\n";
            }

         } # Fill stn/network associative array.
      else
         {
         #-----------------------------------------------
         # See if there's any data for mobile networks.
         # Only DATSAV2M and GLERLM networks are mobile.
         # Suspect that for mobile stns that these sites
         # yield so few records that generally, the 
         # variances can NOT be computed.
         #-----------------------------------------------
         @parts = split (" ", $line);

         if ($debug)
            {
            print "parts of data: ", $parts[0]," ",$parts[1]," ", $parts[2],"\n";
            }

         if ($parts[0] ne "stn_no")  # skip the title line
            {
            $index = $parts[0];

         if ($debug) 
            { 
            print "index, stnIDs{ $index } = ", $index, "xxx", $stnIDs{ $index }, "xxx\n";
            }
           
            if ( ($stnIDs{ $index } eq "DATSAV2M") ||
                 ($stnIDs{ $index } eq "GLERLM  ") )
               {
               if ($debug)
                  {
                  print "Found MOBILE stn: @parts\n";
                  }

               $num_mobile_stns++;

               if ($parts[2] > -888.000)
                  {
                  print "Found Mobile stn with non-missing value: ", $ARGV[0], " ", $stnIDs{$index}, 
                         " ", $parts[0]," ", $parts[1], " ", $parts[2], "\n";

                  $num_non_zero_mobile++;
                  }
               } # mobile stn
            } #skip title line - not title line
         else
            {
            if ($debug)
               {
               print "Skip stn_no parm_no sigma value line\n";
               }
            }
         } # Process second section containing data
      } # Not first line
   }  # while data in file

print "Total Lines read: $lines_read\n";
print "Total: num mobile records found = ", $num_mobile_stns, "\n";
print "Total: num non_zero mobile records found = ", $num_non_zero_mobile, "\n";

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "find_mobile_vars.pl ended at $year/$mon/$mday $hour:$min:$sec\n";

system "gzip $file_name";

close(INFILE);
close(OUTFILE);
