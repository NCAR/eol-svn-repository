#!/usr/bin/perl 

#-------------------------------------------------------------------
# BEWARE: This s/w needs more testing. It may fail in certain
#         cases and has not been completely tested!!!
#
# divide_comp.pl - This perl program divides a composite in ESC
#   format into individual soundings.
#
#   Execute: divide_comp.pl input_file_name
#
#   Required Inputs: One input parameter required as shown below:
#      divide_comp.pl input_file_name 
#   where [input_file_name] is the name of the file to be divided.
#
#   Output:
#    Individual sounding files.
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
# divide_comp.pl - MAIN processing.
##################################################
printf "\ndivide_comp.pl of file $ARGV[0] began on ";print scalar localtime; printf "\n";
$number_lines_processed = 0;
$fake_serial_no = 9000;

#----------------------------------
# Set the output information level.
#----------------------------------
$debug=0;
if ($debug) {print "Input_file_name is $ARGV[0]\n";}

#--------------------------------------------------------
# Open the file to be checked and associated output file.
#--------------------------------------------------------
open (INPUT_FILE, "<$ARGV[0]") || die "Can NOT open first arg $ARGV[0] input file for reading";

$header_found = 0;
$header_lines_read = 0;
$keep_dropping = 0;
$number_lines_processed = 0;

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
      # sounding.
      #------------------------------------------------------------------
      if ($debug) {print "Found a header.\n"; }

      $header_found = 1;
      $header_lines_read++;;
      $save_line[0] = $input_line;          # Should be "Data Type:" line

      $ii=0;
      $save_parts[0] = "yyy";
      $found_serialNo = 0;
      $found_UTCreleaseDate = 0;

      while ((!$found_serialNo || !$found_UTCreleaseDate) && !($save_parts[0] eq "------"))
         {
         $number_lines_processed++;
         $header_lines_read++;;
         $ii++;

         $save_line[$ii] = <INPUT_FILE>;
         @save_parts = split ' ', $save_line[$ii];

         if ($debug) {print "\nsave_line[$ii]:: ii = $ii xxx $save_line[$ii] xxx\n"; }
         if ($debug) {print "\nsave_parts: xxx @save_parts xxx \n"; }

         if ($save_parts[0] eq "Radiosonde" && $save_parts[1] eq "Serial")
            {
            # Found the serial number
            $serialNo = $save_parts[3];
            $found_serialNo = 1;
            }

         if ($save_parts[0] eq "UTC" && $save_parts[1] eq "Release")
            {
            # Found the UTC Release date
            chop($save_parts[4]);
            chop($save_parts[5]);
            chop($save_parts[6]);

            $UTCreleaseDate = $save_parts[4].$save_parts[5].$save_parts[6];
            $found_UTCreleaseDate = 1;
            }
         } # while searching for header info

      if (!$found_serialNo) {$serialNo = $fake_serial_no++;}  # Just assign a number
      if (!$found_UTCreleaseDate) {$$UTCreleaseDate = $fake_serial_no++;}  # Just assign a number

      if (!$found_serialNo || !$found_UTCreleaseDate && ($save_parts[0] eq "------"))
         { 
         print "BAD header: Found Data_Type line but no serialNo or releaseDate in header at line: $number_lines_processed.\n"; 
         }

      #------------------------------------
      # Have info to form output file name.
      #------------------------------------
      if ($debug) {print "First 7 Header lines::\n $save_line[0] $save_line[1] $save_line[2] $save_line[3] $save_line[4] $save_line[5] $save_line[6] $save_line[7]\n"; }

      $output_name = $UTCreleaseDate."_".$serialNo.".cls";

      if ($debug) {print "Open output file: $output_name\n"; }
      open (OUTPUT_FILE, ">$output_name") || die "Can NOT output file $output_name for writing";

      #--------------------------------------------------------
      # Write all header lines to output.
      # Can't just do "print (OUTPUT_FILE @save_line);"
      # because of left over lines from all types of soundings.
      #--------------------------------------------------------
      for $a (0 .. $header_lines_read)
         { print (OUTPUT_FILE $save_line[$a]); }

      if ($debug) {print "header_lines_read:: $header_lines_read\n";}
      if ($debug) {print "HEADER (before Null):: xxx\n @save_line xxx\n";}

      for $a (0 .. $#save_line)
         { $save_line[$a] = ""; }

      $#save_line = 0;

      if ($debug) {print "length of array save_line:: (1 + $#save_line)\n";}
      if ($debug) {print "HEADER (after Null):: xxx @save_line xxx\n";}

      } # Found Data type line. Process all header info.

      #------------------------------------------------------------
      # Output the rest of the sounding to the correct output file.
      #------------------------------------------------------------
      if ($header_found)
         { $header_found = 0; }
      else
         {   
         if ($debug) {print "Save line.\n------------------------\n"};
         print (OUTPUT_FILE $input_line); 
         } # header_found - Don't write twice!

   } # While  - process all records in input file..

close(INPUT_FILE);
close(OUTPUT_FILE);

print "\nTotal number lines processed in file $ARGV[0]: $number_lines_processed\n";
printf "\ndivide_comp.pl of file $ARGV[0] ended on ";print scalar localtime;
printf "\n";
