#! /usr/bin/perl -w
#--------------------------------------------------------------------------------
# UpdateNomTime.pl - This script updates the nominal time in a *.cls file to be
# one plus the UTC_release (actual) time of the sounding and the minutes and seconds are 
# zeroed out. If the UTC_release time is equal to the computed (1 + Actual Time Hour),
# then the nominal time is NOT updated.  This code was created to Post Process Norwegian
# data from the GTS BUFR processing for CAESAR 2024. 
#
# All the CAESAR data were from Norway sites. There were cases of computed 
# Nominal Times done during the basic conversion from BUFR to ESC (*.cls) that
# generated the same nominal times for multiple soundings from the same site.
# Per SL, the original processing to ESC format was technically is correct, 
# but it would be better for each output sounding for a single station to have
# different nominal times.  This may also be helpful to ensure that when/if 
# the soundings are previewed/displayed in the Field Data Archive (FDA) that
# the soundings will be displayed correctly and that all soundings will be listed
# for display. 
#
# Input file names are of the form: *.cls 
#
# Output file names are of the format: [input_file_name].cls.nomUpdated. Output
#   files will be place in the input directory. 
#--------------------------------------------------------------------------------
#
# Execute: 
#    UpdateNomTime.pl <input_dir> where the <input_dir> contains the *.cls files to be processed. 
#
# Examples: 
#    UpdateNomTime.pl ../input_dir
#
# Input: 
#    <input_dir>  where the <input_dir> contains the GTS_BUFR*_SONDE.cls files to be processed.
#
# Output: 
#    Note: All output will be put to the specified <input_dir>.
#
#    *.updated - Output files with Nominal Times updated, as required. Note that not all input
#          files will be modified.  
#
# Notes and Assumptions:
#
# 1. The user should search for HARDCODED, ASSUMPTIONS, BEWARE, WARNING, and ERROR in this code. 
#    The user should also search for "exit" in this code so that they know all the possible
#    places/reasons why this code will stop executing.
#
# 2. ASSUMPTION: That the input data are in expected GTS BUFR format (ESC = CLASS format)
#
# 3. ASSUMPTION: That input files are located in the input_dir and that the file names
#    are as specified above, are in ESC (*.cls) format.
#
# 4. IMPORTANT ASSUMPTION: That in every input file (ESC sounding) that the UTC Release time 
#    preceeds the Nominal Release time in the sounding header.
#
# Created: L. Cully - Aug 2024
#--------------------------------------------------------------------------------
use strict;
my ($WARN);

my $OUTFILE_UPDATED;
my $UpdatedRec = 0;
my $TotalRecProc = 0;

my $debug  = 0; # BEWARE: Generates a lot of debug!

&main();

#--------------------------------------------------------------
# void main()
# Run the scripts to update Nominal Times, if needed.
#--------------------------------------------------------------
sub main 
   {
   printf "\nUpdateNomTime.pl began on ";print scalar localtime;printf "\n";

   if ($debug) {print "Enter Main:: length ARGV = $#ARGV,  ARGV() = @ARGV\n";}

   if ($#ARGV < 0)
      { 
      print "Incorrect number of command line arguments!\n ARGV = @ARGV\n";
      print "Usage: UpdateNomTime.pl <input_dir>\n";
      exit(1);
      }

   #--------------------------------------------------------------------------------
   # This dir contains the data to be processed.
   # Examples:
   #   $INPUT_DIR_ARG = "../output_esc";
   #--------------------------------------------------------------------------------
   my $INPUT_DIR_ARG  = $ARGV[0];
   if ($debug) {print "INPUT_DIR_ARG = $INPUT_DIR_ARG \n";}

   #----------------------------------------------------------------
   # Read in the list of files to process.
   # The suffix is expected to be *.cls for input files. 
   #----------------------------------------------------------------
   printf "Opening INPUT_DIR_ARG:: $INPUT_DIR_ARG\n";
   opendir(my $INPUT_DIR, $INPUT_DIR_ARG) or die("Cannot open $INPUT_DIR_ARG\n");

   my @files = grep(/\.cls$/,readdir($INPUT_DIR));
   closedir($INPUT_DIR);

   printf "Files to process::\n @files \n";

   my $soundingCt = 1;

   #------------------------------------------------------------------------------------
   # Process every ESC file (*.cls) in the input directory.
   # Expected form of GTS BUFR in ESC format for Input file name: 
   #     GTS_BUFR_KSLC_202202132301_01_SONDE.cls
   #
   # Output file name: GTS_BUFR_KSLC_202202132301_01_SONDE.updated
   #------------------------------------------------------------------------------------
   foreach my $file (sort(@files)) 
       {
       my $InputFileToUpdate = sprintf("%s/%s", $INPUT_DIR_ARG, $file);

       printf "\n---------------------------Processing ESC formatted file: $InputFileToUpdate \n";

       my @name_parts = split (/.cls/, $file);    # HARDCODED
       if ($debug) {print "name_parts = @name_parts\n";}

       my $output_update_file = sprintf("%s/%s.updated", $INPUT_DIR_ARG, $name_parts[0]);  # HARDCODED
       print "output_update_file = $output_update_file\n";

       #-------------------------------------------------------------
       # Processing of Header lines in file. 
       #-------------------------------------------------------------
       #     1. Locate UTC Release Time and Nominal Release Time records.
       #
       #     2. Add one hour to the current UTC Release time and compare
       #        to current Nominal Release Time.
       #
       #     3. If times are same, leave Nominal Release Time "as is" and 
       #        write out file "as is".
       #
       #     4. If times are NOT same, update Nominal Release Time in 
       #        output file where the Nominal Release Time's HOUR
       #        is updated and the mins and secs are 00.
       #
       # ** WARNING: 
       # Potential issues:  If an hour is added to the UTC_release time
       # and it causes a rollover into the next day, month or year, 
       # then this needs to be handled.  For CAESAR 2024, this case
       # was not seen. 
       #-------------------------------------------------------------
       if ($debug) {print "Opening input and output files.\n";}

       open(my $INPUT_FILE,"<", $InputFileToUpdate) or die("Can't open file for reading: ".$InputFileToUpdate);
       open(my $OUTFILE_UPDATE,">", $output_update_file) or die("Can't open file for writing: ".$output_update_file);

       # -------------------------------------------------
       # Process all lines in input data file (ESC format)
       # -------------------------------------------------
       my @lines = <$INPUT_FILE>; 
       my $number_lines_in_file = $#lines+1;

       #------------------------------------------------------------
       # ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,
       #  $atime,$mtime,$ctime,$blksize,$blocks) = stat($filename);
       #------------------------------------------------------------
       # Stat array results::
       #  0 dev device number of filesystem
       #  1 ino inode number
       #  2 mode file mode (type and permissions)
       #  3 nlink number of (hard) links to the file
       #  4 uid numeric user ID of file's owner
       #  5 gid numeric group ID of file's owner
       #  6 rdev the device identifier (special files only)
       #  7 size total size of file, in bytes
       #  8 atime last access time in seconds since the epoch
       #  9 mtime last modify time in seconds since the epoch
       # 10 ctime inode change time in seconds since the epoch (*)
       # 11 blksize preferred block size for file system I/O
       # 12 blocks actual number of blocks allocated
       #------------------------------------------------------------
       my @file_info = stat $INPUT_FILE;

       if ($debug) { print "file_info:: @file_info\n"; print "number_lines_in_file:: $number_lines_in_file \n"; print "File size = $file_info[7]\n"; }

       close($INPUT_FILE);

       $TotalRecProc = 0;
       $UpdatedRec = 0;

       my $out_string = "";
       my $UTC_release_hour = "88";
       my $nominal_release_hour = "99";
       my $updated_nominal_release_hour = "77";

       # -----------------------
       # -----------------------
       # Loop through file lines
       # -----------------------
       # -----------------------
       foreach my $line (@lines)
         {
         $TotalRecProc++;
         if ($debug) {print "TotalRecProc = $TotalRecProc;  Orig Line: $line\n";}

         # -------------------------------------------------
         # Process the first 16 lines which are header lines.
	 # Output data lines "as is".
	 # -------------------------------------------------
         if ($TotalRecProc >= 15)  # HARDCODED
            {
            #----------------------------------------------------
            # It's a data line. Write it to the output "as is".
            #----------------------------------------------------
            if ($debug) { print "DATA LINE: Write record: TotalRecProc = $TotalRecProc . $line\n"; }
            print $OUTFILE_UPDATE $line;
            }
         else
            {
            # -------------------------------------------------
            # It's a header line. Grab "UTC Release Time" and 
	    # "Nominal Release Time" values when found. 
            # -------------------------------------------------
            if ($debug && $TotalRecProc < 15) { print "Header Rec: SEARCH for UTC Release and Nominal Release times.\n"; }

            my $header_line= substr($line, 0, 16);

            if ($header_line eq "UTC Release Time") # Actual sounding release time
	       {
               # Pick out UTC Release hour
               $UTC_release_hour = substr($line, 49, 2);
               if ($debug){ print "-------->>>>>>>>Found HEADER: UTC_release_hour: $UTC_release_hour\n";}
	       }
            elsif ( $header_line eq "Nominal Release ")
	       {
               # Pick out nominal_release_hour
               $nominal_release_hour = substr($line, 49, 2);

	       my $new_nominal_release_hour = $UTC_release_hour+1;
	       if ($new_nominal_release_hour == 24)
	          {
                  # WARNING: Not updated for CAESAR 2024, but this section may need updating to handle
		  # day, month, and year rollovers.  (HERE)
                  $new_nominal_release_hour = 0;
		  print"WARNING: Hour was 24 so DAY Rollover! Reset new_nominal_release_hour to Zero ($new_nominal_release_hour) \n";
                  }
               print "-------->>>>>>>>Compare new_nominal_release_hour ( $new_nominal_release_hour ) and Original nominal_release_hour ( $nominal_release_hour ).\n";

	       #-----------------------------------------------------------------------------------------------------
               # WARNING: Header order specifies Nominal Release time record is always after UTC_release time record. 
	       # If this is not true for the input data, then update this s/w to handle that case.
	       #-----------------------------------------------------------------------------------------------------
               if ( $new_nominal_release_hour == $nominal_release_hour)
                 {
                 if (1){ print "YYYY: new_nominal_release_hour EQUALS nominal_release_hour. PRINT OUT Header Line As Is. \n";} # HARDCODED

                 } # Times Match
              else
                 {
                 if (1){ print "NEW_nominal_release_hour ($new_nominal_release_hour) NOT EQUAL Original Nominal_release_hour( $nominal_release_hour ).\n";} # HARDCODED
                 if (1){ print "ZZZZ: Reset to use NEW Nominal Release hour and ensure reset minutes and seconds to zero.\n";} # HARDCODED

		 $UpdatedRec++;

                 my $part1 = substr($line, 0, 48); # Reset output line with new time
		 $line = sprintf("%s %02d:00:00\n", $part1, $new_nominal_release_hour);

       		 if ($debug){ print "Updated Line to Output:: $line\n";}
    	         } # Update nominal hour by one hour
	       } # Found Nominal Release time record
	    else 
	       {
               if ($debug){ print "Found HEADER but NOT TIME record. Print out As Is: $line\n";}
	       }

            if ($debug) {print "HEADER LINE: Write rec to output file: TotalRecProc = $TotalRecProc . $line\n"; }
            print $OUTFILE_UPDATE $line;
            } # Found header line

         } # end foreach all lines in the file

      close ($OUTFILE_UPDATE);

      print "Total Lines Processed from Input File: $TotalRecProc\n";
      print "Total UPDATED lines written to output file: $UpdatedRec\n";

      close ($INPUT_FILE);

      } # end foreach file in the input directory

   print "All lines processed in Input File\n";

   printf "\nUpdateNomTime.pl ended on ";print scalar localtime;printf "\n";
   exit(0); # Exit with Success Code
} main()
