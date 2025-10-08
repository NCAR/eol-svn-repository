#!/usr/bin/perl 

#-------------------------------------------------------------------
# stripLearFalcon.pl - This perl program strips the Lear and Falcon
#   soundings from the input composite sounding file. All other
#   soundings should be left intact.
#
#   Execute: stripLearFalcon.pl input_file_name
#
#   Required Inputs: One input parameter required as shown below:
#      stripLearFalcon.pl input_file_name 
#   where [input_file_name] is the name of the file to be stripped.
#
#   Output:
#    A file named input_file_name.retained_recs which has all the
#    Lear and Falcon soundings removed and a file named
#    input_file_name.dropped_recs which contains all the stripped
#    records.
#
#   Assumptions:
#    That the input file is in the standard ESC format. For info
#    on the ESC format see the EOL Plone sounding documentation
#    at www.eol.ucar.edu/dmg .
#--------------------------------------------------------------------
# Aug/Sep 2008 lec
#   Created Version 1.0.
#-------------------------------------------------------
##################################################
# stripLearFalcon.pl - MAIN processing.
##################################################
printf "\nstripLearFalcon.pl of file $ARGV[0] began on ";print scalar localtime; printf "\n";
$number_lines_processed = 0;
$number_lines_saved = 0;
$number_lines_dropped = 0;

#----------------------------------
# Set the output information level.
#----------------------------------
$debug=1;
if ($debug) {print "Input_file_name is $ARGV[0]\n";}

#--------------------------------------------------------
# Open the file to be checked and associated output file.
#--------------------------------------------------------
open (INPUT_FILE, "<$ARGV[0]") || die "Can NOT open first arg $ARGV[0] input file for reading";
open (OUTPUT_FILE, ">$ARGV[0].retained_recs") || die "Can NOT output file $ARGV[0].retained for writing";
open (DROPPED_OUTPUT_FILE, ">$ARGV[0].dropped_recs") || die "Can NOT output file $ARGV[0].dropped_recs for writing";

#-----------------------------------------------------------------------
# Check every line and strip Lear and Falcon
# sounding records only. Sample header lines are:
#
# Data Type:                         Sounding
# Project ID:                        IHOP High Res Class Format Sounding
# Release Site Type/Site ID:         Falcon 20, D-CMET
#-----------------------------------------------------------------------
$header_found = 0;
$keep_dropping = 0;
$number_lines_processed = 0;
$number_lines_dropped = 0;
$number_lines_retained = 0;

while ($input_line = <INPUT_FILE>)
   {
   $number_lines_processed++;
   @line_parts = split ' ', $input_line;

   if ($debug) {print "\n---Processing Line $number_lines_processed:: xxx $input_line xxx\n"; }
   if ($debug) {print "line_parts:: xxx @line_parts xxx\n"; }
   if ($debug) {print "line_parts[0]:: xxx $line_parts[0] xxx\n"; }

   if ($line_parts[0] eq "Data" && $line_parts[1] eq "Type:")
      {
      #------------------------------------------------------------------
      # Start saving lines until you know what type of sounding you have.
      # Read and save the next two lines then decide what to do with this 
      # sounding. Assume strict order of input records!!!
      #------------------------------------------------------------------
      $header_found = 1;
      $save_line[0] = $input_line;                               # Should be "Data Type" line
      $save_line[1] = <INPUT_FILE>; $number_lines_processed++;   # Should be "Project ID" line
      $save_line[2] = <INPUT_FILE>; $number_lines_processed++;   # Should be "Release Site Type/Site ID:" line
   
      if ($debug) {print "save_lines[0-2]::\n $save_line[0] $save_line[1] $save_line[2]"; }

      #------------------------------------------------------------------------------------------------
      # Found the Falcon or Lear sounding so dump to dropped file. Else keep and dump to retained file.
      #------------------------------------------------------------------------------------------------
      if ($save_line[2] eq "Release Site Type/Site ID:         FI Lear 36-016, N12FN              \n" ||
          $save_line[2] eq "Release Site Type/Site ID:         Falcon 20, D-CMET                  \n" ||
          $save_line[2] eq "Release Site Type/Site ID:         FI Lear 36-016, N12FN\n"               ||
          $save_line[2] eq "Release Site Type/Site ID:         Falcon 20, D-CMET\n"                     )
         {
         if ($debug) {print "Found Falcon or Lear Sounding. Drop line and rest of sounding!\n"};

         $keep_dropping = 1;
         $number_lines_dropped = $number_lines_dropped+3;

         print (DROPPED_OUTPUT_FILE $save_line[0],$save_line[1],$save_line[2]);
         }
      else
         {
         #----------------------------------------------------
         # Found Other sounding so write to saved record file.
         #----------------------------------------------------
         if ($debug) {print "Other sounding type. Save sounding!\n"};

         $keep_dropping = 0;
         $number_lines_retained = $number_lines_retained+3;

         print (OUTPUT_FILE $save_line[0],$save_line[1],$save_line[2]);

         } # Found Release type line.
      } # Found Data type line.

     #------------------------------------------------------------
     # Output the rest of the sounding to the correct output file.
     #------------------------------------------------------------
     if ($header_found)
        { $header_found = 0; }
     else
        {   
        if ($keep_dropping)
           { 
           if ($debug) {print "Keep Dropping\n-------------------------\n"};
           print (DROPPED_OUTPUT_FILE $input_line); 
           $number_lines_dropped++;
           }
        else
           { 
           if ($debug) {print "Save line.\n------------------------\n"};
           print (OUTPUT_FILE $input_line); 
           $number_lines_retained++;
           }
        } # header_found - Don't write twice!

   } # While  - process all records in input file..

close(INPUT_FILE);
close(OUTPUT_FILE);
close(DROPPED_OUTPUT_FILE);

print "\nTotal number lines processed in file $ARGV[0]: $number_lines_processed\n";
print "\nTotal number lines stripped: $number_lines_dropped\n";
print "\nTotal number lines retained: $number_lines_retained\n";

printf "\nstripLearFalcon.pl of file $ARGV[0] ended on ";print scalar localtime;
printf "\n";
