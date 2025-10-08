#! /usr/bin/perl -w

#--------------------------------------------------------------------------------
# StripRecsMissTime.pl - This script strips data lines that have times that are 
# missing (e.g., -999.99). All stripped records are written to an output file 
# (i.e., *.cls.stripped) for each input file. All input data that does not 
# have missing times is written to a *.cls.clean file. The *.cls.clean files 
# can be used for final processed files to be format checked and autoQC'd.  
# This code was created to post process the NWS GTS BUFR data which contained 
# data recs (mostly missing data) but missing times that caused issues once 
# the data were sorted using the ant sort_esc build.xml command. 
#
# NOTE: This code ONLY strips out data records that have time equal to -999.0
# or smaller. Everything else, include lines with all missing data or lines
# that are BLANK are left "as is" in the output. 
#
# Although this software was created to run on the National Weather Service (NWS) GTS BUFR (binary)
# data as collected in the Field Data Archive (FDA) dataset 100.030, it should 
# run on any *.cls files in ESC format. 
#
# Input file names are of the form: GTS_BUFR_XXXX_YYYYMMDDhhmmss_JJ_SONDE.cls where
# XXXX is the stn name like KSLC; YYYYMMDDhhmmss is the year, month, day, hour,
# minute and second; JJ is the number of the sonde as extracted during the
# GTS BUFR preprocessing where there can be more than one sounding in a BUFR file.
# If so, then the number is such that the first sonde is "_01", next found in
# BUFR file is "_02" and so on.
#
#--------------------------------------------------------------------------------
#
# Execute: 
#    StripRecsMissTime.pl <input_dir>  where the <input_dir> contains the
#    GTS_BUFR*_SONDE.cls files to be processed. 
# Examples: 
#    StripRecsMissTime.pl ../output_esc
#
#    StripRecsMissTime.pl /net/work/Projects/CFACT/data_processing/upper_air/radiosonde/NWS_GTS_BUFR/software/xxx_test_Strip
#
# Input: 
#    <input_dir>  where the <input_dir> contains the GTS_BUFR*_SONDE.cls files to be processed.
#
# Output: 
#    Note: All output will be put to the specified <input_dir>.
#
#    *.cls.clean - Output files with all records that had times that are missing (-999.99) stripped.
#    *.cls.stripMissTimes - Output files containing all stripped records:w
#
# Notes and Assumptions:
#
# 1. The user should search for HARDCODED, ASSUMPTIONS, BEWARE, WARNING, and ERROR in this code. 
#    The user should also search for "exit" in this code so that they know all the possible
#    places/reasons why this code will stop executing.
#
# 2. ASSUMPTION: That the raw data are in expected NWS GTS BUFR format (ESC = CLASS format)
#
# 3. ASSUMPTION: That input files are located in the input_dir and that the file names
#    are as specified above, are in ESC (*.cls) format.
#
#
# Created: L. Cully April 2022
#
#--------------------------------------------------------------------------------
use strict;

my $debug  = 0; # BEWARE: Generates a lot of debug!

my $OUTFILE_STRIPPED;
my $StrippedRec = 0;

my $TotalRecProc = 0;
my $OutputRec = 0;

&main();

#--------------------------------------------------------------
# void main()
# Run the scripts to strip recs with missing times. 
#--------------------------------------------------------------
sub main 
   {
   printf "\nStripRecsMissTime.pl began on ";print scalar localtime;printf "\n";

   if ($debug) {print "Enter Main:: length ARGV = $#ARGV,  ARGV() = @ARGV\n";}

   if ($#ARGV < 0)
      { 
      print "Incorrect number of command line arguments!\n ARGV = @ARGV\n";
      print "Usage: StripRecsMissTime.pl <input_dir>\n";
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
   #
   # Expected form of NWS GTS BUFR in ESC format for Input file names: 
   #     GTS_BUFR_KSLC_202202132301_01_SONDE.cls
   #------------------------------------------------------------------------------------
   foreach my $file (sort(@files)) 
       {
       #--------------------------------------------------------------------------
       # Form of NWS GTS BUFR input file name: KSLC_202202150000_ius_sounding.bufr
       #--------------------------------------------------------------------------
       if ($debug) {print "-----------------------------\n";}
       my $InputFileToStrip = sprintf("%s/%s", $INPUT_DIR_ARG, $file);

       printf "\nprocessing ESC formatted file: $InputFileToStrip \n";

       my @name_parts = split (/.cls/, $file);    # HARDCODED
       if ($debug) {print "name_parts = @name_parts\n";}

       #----------------------------------------------------------------------------
       # output_clean_file   - Output file stripped of recs with missing times. 
       # output_stripped_lines_file - Output file with all lines that were stripped.
       #----------------------------------------------------------------------------
       my $output_clean_file = sprintf("%s/%s.clean", $INPUT_DIR_ARG, $name_parts[0]);  # HARDCODED
       my $output_stripped_lines_file = sprintf("%s/%s.stripped", $INPUT_DIR_ARG, $name_parts[0]); # HARDCODED

       if ($debug) {print "output_clean_file = $output_clean_file\n";}
       if ($debug) {print "output_stripped_lines_file = $output_stripped_lines_file\n";}


       #-------------------------------------------------------------
       # Process lines in file and strip lines with time = -999.99.
       #-------------------------------------------------------------
       print "Opening input and output files.\n";

       open(my $INPUT_FILE,"<", $InputFileToStrip) or die("Can't open file for reading: ".$InputFileToStrip);
       open(my $OUTFILE_CLEAN,">", $output_clean_file) or die("Can't open file for writing: ".$output_clean_file); #"cleaned" output files
       open(my $OUTFILE_STRIPPED_LINES,">", $output_stripped_lines_file) or die("Can't open file for writing: ".$output_stripped_lines_file); # Stripped lines


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
       $OutputRec = 0;
       $StrippedRec = 0;
       my $BlankRec = 0;

       my $out_string = "";

       # -----------------------
       # -----------------------
       # Loop through file lines
       # -----------------------
       # -----------------------
       foreach my $line (@lines)
         {
         $TotalRecProc++;

#        chomp ($line); #remove return \n

         if ($debug) {print "TotalRecProc = $TotalRecProc;  Orig Line: xxx $line xxx\n";}

         # -------------------------------------------------
         # Skip the first 16 lines which are header lines.
         # Put all header lines to the output file.  Don't
         # change anything in any line. Just strip lines
         # with missing Times.
         # -------------------------------------------------
         if ($TotalRecProc < 16)  # HARDCODED
            {
            #----------------------------------------------------
            # It's a header line. Write it to the output "as is".
            #----------------------------------------------------
            $OutputRec++;
            if ($debug) { print "Write record: TotalRecProc = $TotalRecProc . HEADER line. OutputREC = $OutputRec.\n"; }

            print $OUTFILE_CLEAN $line;
            }
         else
            {
            # -------------------------------------------------
            # NOT a header line.  Check for missing time value.
            # Write recs with valid times to output. Time is
            # valid if not -999.99
            # -------------------------------------------------
            # Pick time out of input line and see if missing. 
            # -------------------------------------------------
            if ($debug && $TotalRecProc< 17) { print "Header written to output.\n\n\n"; }

            my $time_value = substr($line, 0, 6);

            # --------------------------------------------------------------------
            # Check for Blank lines. Write to output clean file and issue warning. 
            # Proceed to next line.
            # --------------------------------------------------------------------
            if ($time_value eq "\n") 
               {
               $OutputRec++;
               $BlankRec++;

               if ($debug) { print "Write rec BLANK line output file: TotalRecProc = $TotalRecProc . This is a BLANK line OutputRec = $OutputRec;.\n"; }
               print "WARNING: BLANK Line Found at line $TotalRecProc. Print to cleaned output file and go to next line in file.\n";

               print $OUTFILE_CLEAN $line;

               next;
               }

            if ($debug) {print "time_value: xxx $time_value xxx\n";}

            if ($time_value < -999.0)
               {
               # This is a missing time. Put line to stripped output file.
               $StrippedRec++;
               if ($debug) { print "Write rec to STRIPPED output file: TotalRecProc = $TotalRecProc . This is a DATA line with MISSING time. StrippedRec = $StrippedRec.\n"; }

               printf ($OUTFILE_STRIPPED_LINES $line);
               }
            else
               {
               # This is a good time. Put line to cleaned output file.
               $OutputRec++;
               if ($debug) { print "Write rec to cleaned output file: TotalRecProc = $TotalRecProc . This is a DATA line with GOOD time. OutputREC = $OutputRec.\n"; }

               printf ($OUTFILE_CLEAN $line);
               }            

            if ($debug) { print "Write record: TotalRecProc = $TotalRecProc . This is a DATA line. OutputREC = $OutputRec.\n"; }

           } # Check for first 16 header lines

         } # end foreach all lines in the file

         close ($OUTFILE_STRIPPED_LINES);
         close ($OUTFILE_CLEAN);

         print "Total Lines Processed from Input File: $TotalRecProc\n";
         print "Total Good DATA  Lines written to output file.clean: $OutputRec\n";
         print "Total STRIPPED DATA written to output .stripped file: $StrippedRec\n";
         print "Total BLANK lines written to output .clean file: $BlankRec\n";

         my $percent = ($StrippedRec/$TotalRecProc)*100.0;
         my $result = sprintf("%s %d ( %.2f %s)\n", "Total Lines Stripped:", $StrippedRec, $percent, "\%");
         print $result;

         close ($INPUT_FILE);

       } # end foreach file in the input directory

    printf "\nStripRecsMissTime.pl ended on ";print scalar localtime;printf "\n";
    }
