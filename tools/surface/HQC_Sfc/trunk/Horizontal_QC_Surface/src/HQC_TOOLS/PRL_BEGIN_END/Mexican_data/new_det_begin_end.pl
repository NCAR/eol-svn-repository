#!/usr/bin/perl 

#-------------------------------------------------------
# new_det_begin_end.pl - This perl script/program determines
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
printf "\nnew_det_begin_end.pl began at $year/$mon/$mday $hour:$min:$sec\n";

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
%stnBegEnd = ();

foreach $currentfile (@current_files)
   {
   #-------------------------------------
   # Only process the Daily precip files
   #-------------------------------------
   if ($currentfile =~ m/dqcf/)
      {
      open (INFILE, $currentfile) || 
                    die "Can NOT open $currentfile file for reading";

      print "------------------------------------------\n";
      print "Processing $currentfile\n\n";

      #---------------------------------
      # Fill the associative array with
      # all the uniq IDs/stnBegEnd and
      # initialize begin and end dates.
      # Add new stns as they appear. 
      #---------------------------------
      while ($line = <INFILE>)
         {
         chop ($line);

         $stnID = substr($line, 19, 15);

         $next_date  = substr($line, 0, 7);
         @parts = split ("/", $next_date);
         $next_date = $parts[0].$parts[1];   # Want YYYYMM

         if ($debug)
            {
            print "\nline:xxx", $line, "xxx\n";
            print "stnID:xxx", $stnID, "xxx\n";
            print "next_date:xxx", $next_date, "xxx\n";
            }

         #--------------------------------------------------
         #   Update the begin and end dates for each stn and
         #   put them into hash table (assoc. array).
         #--------------------------------------------------
         if (exists $stnBegEnd{$stnID} )
            {
            #-------------------------------------
            # This stn already exists in the hash.
            # Compare dates already in hash.
            #-------------------------------------
            @old_dates = @{ $stnBegEnd{ $stnID } };

            if ($debug)
               {
               print "This stn EXISTS in the hash.\n";
               print 
                 "old_dates[0]: $old_dates[0]  old_dates[1]: $old_dates[1]\n";
               }

            #-----------------
            # Check Begin date
            #-----------------
            if ($next_date < $old_dates[0])
               {
               $old_dates[0] = $next_date;

               if ($debug) {print "Reset begin date\n"};
               }

            #-----------------
            # Check End date
            #-----------------
            if ($next_date > $old_dates[1])
               {
               $old_dates[1] = $next_date;
               if ($debug) {print "Reset end date\n"};
               }

            #----------------------------
            # Delete old entry and update
            # with new dates. If just do
            # push then tags onto end of
            # hash rec. Is there another
            # way to update existing hash
            # record?
            #----------------------------
            delete ($stnBegEnd{$stnID});

            push (@{ $stnBegEnd{$stnID} }, @old_dates);

            if ($debug)
               {
               @hash_keys = keys (%stnBegEnd);
               @hash_values = values (%stnBegEnd);   # These are ptr values/refs

               print "hash_keys: @hash_keys\n";
               print "hash_values: @hash_values\n";

               print "EXISTS: stnID, begin, end: $stnID  $old_dates[0] $old_dates[1]\n";
               }
            }
         else
            {
            #----------------------------------------
            # This stn is not in the hash, so add it.
            #----------------------------------------
            if ($debug)
               {
               print "This stn is NEW.\n";
               }

            push (@{ $stnBegEnd{$stnID} }, $next_date); #initialize begin date
            push (@{ $stnBegEnd{$stnID} }, $next_date); #initialize end date

            @old_dates = @{ $stnBegEnd{ $stnID } };

            if ($debug)
               {
               print "Initialized: old_dates[0]: $old_dates[0]  old_dates[1]: $old_dates[1]\n
";
               }

            }  # New stn entry

         } # Fill/update associative array while in file

      } # Valid type of file

   } # For each file in the directory.

#--------------------------------------------------
# Print stnID with begin and End dates to output.
# The exact same stns should appear in both hashes.
#--------------------------------------------------
foreach $stnID (sort keys %stnBegEnd)
   {
   print "$stnID  @{$stnBegEnd{$stnID}} \n";
   }


($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "new_det_begin_end.pl ended at $year/$mon/$mday $hour:$min:$sec\n";

close(INFILE);
close(OUTFILE);
