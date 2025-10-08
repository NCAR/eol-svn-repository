#!/usr/bin/perl 

#-------------------------------------------------------
# det_begin_end.pl - This perl script/program determines
#   the begin and end dates for all the stns in the
#   specified input files.
# 
# 99 Oct lec
#   Created.
#-------------------------------------------------------*/
$debug = 0;

#-----------------------------
# First get and print the time
#-----------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "\ndet_begin_end.pl began at $year/$mon/$mday $hour:$min:$sec\n";

#---------------------------------------
# Open the input and output files.
# Process all the files (of the specified
# type that are in the current directory.
#----------------------------------------
opendir current_dir,  "." or die "Can't open current dir: $!";
@current_files = grep !/^\./, readdir current_dir;
closedir current_dir;

print "There are $#current_files files in current directory.\n";

open (OUTFILE, ">begin_end_dates.out") ||
              die "Can NOT open output file for reading";

#---------------------------------------
# Process all the files in the directory
# that are of a valid type.
#---------------------------------------
%stnBeg = ();
%stnEnd = ();

foreach $currentfile (@current_files)
   {
   #-------------------------------------
   # Only process the Daily precip files
   #-------------------------------------
   if ($currentfile =~ m/dqcf/)
      {
      open (INFILE, $currentfile) || 
                    die "Can NOT open $currentfile file for reading";

      print "\n\n------------------------------------------\n";
      print "Processing $currentfile\n";

      #---------------------------------
      # Fill the associative array with
      # all the uniq network/stnBegEnd and
      # initialize begin and end dates.
      # Add new stns as they appear. 
      #---------------------------------
      while ($line = <INFILE>)
         {
         chop ($line);

         $stnID = substr($line, 19, 15);

         $date  = substr($line, 0, 7);
         @parts = split ("/", $date);
         $date = $parts[0].$parts[1];   # Want YYYYMM

         if ($debug)
            {
            print "\nline:xxx", $line, "xxx\n";
            print "stnID:xxx", $stnID, "xxx\n";
            print "date:xxx", $date, "xxx\n";
            print "stnID: $stnID   stnBeg:$stnBeg{$stnID}  stnEnd:$stnEnd{$stnID}\n";
            }

         #--------------------------------------------------
         #   Update the begin and end dates for each stn and
         #   put them into hash tables (assoc. arrays).
         #--------------------------------------------------
         if (exists $stnBeg{$stnID} )
            {
            # This stn already exists in the hash.
            # Compare dates already in hash.

            if ($debug)
               {
               print "This stn EXISTS in the hash.\n";
               }

            if ($date < $stnBeg{ $stnID })
               {
               $stnBeg{ $stnID } = $date;
               }

            if ($date > $stnEnd{ $stnID })
               {
               $stnEnd{ $stnID } = $date;
               }

            if ($debug)
               {
               print "EXISTS: stnID: $stnID   stnBeg:$stnBeg{$stnID}  stnEnd:$stnEnd{$stnID}\n";
               }
            }
         else
            {
            # This stn is not in the hash, so add it.
            if ($debug)
               {
               print "This stn is NEW.\n";
               }
  
            $stnBeg{ $stnID } = $date;  #initialize value
            $stnEnd{ $stnID } = $date;  #initialize value

            if ($debug)
               {
               $len = length ($stnBeg{$stnID});
               print "After Add:length of $stnID value is: $len\n";
               print "NEW: stnID: $stnID   stnBeg:$stnBeg{$stnID}  stnEnd:$stnEnd{$stnID}\n";
               }

            }

         } # Fill/update associative array while in file

      } # Valid type of file

   } # For each file in the directory.

#--------------------------------------------------
# Print stnID with begin and End dates to output.
# The exact same stns should appear in both hashes.
#--------------------------------------------------
while (($stnID, $beg_date) = each (%stnBeg))
   {
   printf OUTFILE "$stnID   $beg_date    $stnEnd{$stnID}\n"
   }

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "det_begin_end.pl ended at $year/$mon/$mday $hour:$min:$sec\n";

close(INFILE);
close(OUTFILE);
