#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The GTSBUFR_DiffClassOutput.pl script diffs the ESC (CLASS) output files
# when the preprocessing and subsequent GTSBUFR conversion software generates
# "duplicate" output ESC *.cls output.  These duplicate files may not be exact
# dups, so the scientific staff requests to know which files are exact dups,
# which are not, and what any diffs are.  The input file names are of the structure:
# GTS_BUFR_[siteName]_[YYYYMMDDhhmm]_[nn]_[SONDE or PIBAL].cls where YYYY is the
# year, MM is the month, DD is the day, hh is the hour, mm is the minute, and 
# nn is the duplicate sonde number. The duplicate sonde number is auto generated
# by the preprocessing software when it detects more than one sounding in the 
# binary input file. 
#
# Inputs: GTS BUFR ESC (*.cls) files.
#  User must specify input data directory and output data directory
#  on command line. (Note that input and output dirs must exist before running 
#  this software.)
#
# Execute command: 
#    GTSBUFR_DiffClassOutput.pl [input dir] [output dir]
#
# Examples:
#    GTSBUFR_DiffClassOutput.pl SOCRATES ../output/new_zealand ../output/new_zealand >& runDiffNZ.log &
#    GTSBUFR_DiffClassOutput.pl SOCRATES ../output/australia ../output/australia >& runDiffAUS.log &
#    GTSBUFR_DiffClassOutput.pl SOCRATES ../output/antarctica ../output/antarctica >& runDiffANT.log &
#
# Outputs: Log containing list of exact and non-exact duplicate ESC *.cls files found
#  in the input directory.
#
# Assumptions and Warnings:
#  0. User will search for all ASSUMPTIONS, WARNINGS, ERRORS, HARDCODED, PIBAL words.
#     Per the science staff requests, this s/w can issue a variety of warnings
#     and errors. 
#
#  1. User must create input and output directories before running code. 
#
# @author Linda Cully February 2019
# @version GTS BUFR Diff Sonde Files in ESC format  1.0
# Originally developed for SOCRATES 2018 GTS BUFR data.
#
##Module------------------------------------------------------------------------
package GTSBUFR_DiffClassOutput;
use strict;

printf "\nGTSBUFR_DiffClassOutput.pl began on ";print scalar localtime;printf "\n";
my $debug = 0; 

my $inputDir = "";

&main();
printf "\nGTSBUFR_DiffClassOutput.pl ended on ";print scalar localtime;printf "\n";

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the GTSBUFR radiosonde data by determining how many different versions
# (some unique some not) of each sounding at each specific date/time are found. </p>
##------------------------------------------------------------------------------
sub main 
   {
   my $differ = GTSBUFR_DiffClassOutput->new();
   $differ->diffESC();
   } #main()

##------------------------------------------------------------------------------
# @signature GTSBUFR_DiffClassOutput new()
# <p>Create a new instance of a GTSBUFR_DiffClassOutput.</p>
#
# @output $self A new GTSBUFR_DiffClassOutput object.
##------------------------------------------------------------------------------
sub new 
   {
   my $invocant = shift;
   my $self = {};
   my $class = ref($invocant) || $invocant;
   bless($self,$class);
   
   $self->{"RAW_DIR"} = $ARGV[0];     
   $inputDir = $ARGV[0];

   $self->{"OUTPUT_DIR"} = $ARGV[1]; 

   print "ARGV Values: Input RAW_DIR, OUTPUT_DIR: @ARGV\n\n";
   
   return $self;
   } # new()

##------------------------------------------------------------------------------
# @signature void diffESC()
# <p>Diff the class files in ESC format. Create the output dir if it does 
# not exist.</p>
##------------------------------------------------------------------------------
sub diffESC
   {
   my ($self) = @_;
    
   mkdir($self->{"OUTPUT_DIR"}) unless (-e $self->{"OUTPUT_DIR"});
    
   $self->diffDataFiles();
   } #diffESC()

                           
##------------------------------------------------------------------------------
# @signature void diffDataFiles()
# <p>Read in the files from the raw data directory and diff sets of files
# with same name with diff sounding numbers (in file name). All of these
# "same name" soundings were generated from a single raw BUFR file that had
# multiple soundings in it.</p>
##------------------------------------------------------------------------------
sub diffDataFiles 
   {
   my ($self) = @_;

   my %UNIQFileNames;
    
   opendir(my $RAW,$self->{"RAW_DIR"}) or die("Can't read raw directory ".$self->{"RAW_DIR"});
   my @files = grep(/.cls$/,sort(readdir($RAW)));   # HARDCODED - process only *.cls files
   closedir($RAW);

   if ($debug) {print "Input Files to Process: @files\n\n";}
   
 
   #----------------------------------------------------
   # Process all *.cls data files in specified directory.
   #----------------------------------------------------
   if ($debug) {printf("Input Dir read. Diff input files.\n");}

   my $diffExist = 0;

   # --------------------------------------------------------------------------------
   # ***** Determine if there are any files to diff or not in this directory. *******
   # Fill hash with list of uniq file names, then compare all *_01*.cls files with
   # all other versions of that file.
   # --------------------------------------------------------------------------------
   foreach my $file (@files) 
     { 
     #---------------------------------------------------------
     # Create a hash of unique file names from input directory.
     #---------------------------------------------------------
     if ($debug) {print "\nfile =  $file\n";}         # GTS_BUFR_Invercargill_201711190933_02_SONDE.cls

     if (index($file, "cls") == -1) 
        {
        print "WARNING: Found non-CLS file in directory. file = $file Skip to next file. Should NEVER HAPPEN since we only do *.cls files from input dir.\n"; 
        next;
        }
     else
        {
        if ($debug) {print "Found CLS file in directory. file = $file\n";}
        }


     my @clsSplit = split /\.cls/, $file;             # GTS_BUFR_Invercargill_201711190933_02_SONDE cls
     my @UScoreSplit = split /\_/, $clsSplit[0];      # GTS BUFR Invercargill 201711190933 02 SONDE
     my $uniqFileName = $UScoreSplit[0]."_".$UScoreSplit[1]."_".$UScoreSplit[2]."_".$UScoreSplit[3]; # GTS_BUFR_Invercargill_201711190933
     my $sondeNumber = $UScoreSplit[4];

     if ($debug) {print "uniqFileName = xxx $uniqFileName xxx, sondeNumber = $sondeNumber\n";} 

     if ( !exists($UNIQFileNames{$uniqFileName}) )
        {
        if ($debug) {print "Yes! Add to Hash? $uniqFileName \n";}
        $UNIQFileNames{$uniqFileName} = $sondeNumber;
        }
     else
        {
        if ($debug) {print "NO! Do NOT add to Hash? $uniqFileName.  Check sondeNumber.\n";}

        my $prev_sondeNumber =  $UNIQFileNames{$uniqFileName};
        if ( $prev_sondeNumber < $sondeNumber)
           {
           if ($debug) {print "Reset to new sonde number n hash. prev_sondeNumber = $prev_sondeNumber , sondeNumber = $sondeNumber .\n";}
           $diffExist = 1;

           $UNIQFileNames{$uniqFileName} = $sondeNumber;
           }
        else
           {
           if ($debug) {print "DO NOT Reset sonde number in hash. prev_sondeNumber = $prev_sondeNumber , sondeNumber = $sondeNumber .\n";}
           }
        } # file already in hash
     } # foreach file in input directory    

   if ($debug)
     {
     my @NamesInHash = keys %UNIQFileNames;
     print "\n\nNamesInHash: @NamesInHash \n\n";

     foreach my $UFN (keys %UNIQFileNames)
       {
       print "$UFN has sonde# = $UNIQFileNames{$UFN}\n";
       }

     } #debug

   #---------------------------------------------------
   # Diff all versions of file date/time and print diffs
   # to output file.
   #---------------------------------------------------
   if ($debug) {print "Need to diff any files? diffExist = $diffExist\n";}

   if ($diffExist == 0)
      {
      print "No diffs exist. Only single (_01) files in input_dir.\n";
      } 
   else
      {
      print "Diff versions exist in input_dir so diff all versions.\n";

      foreach my $UFN (keys %UNIQFileNames)
       {
       my $maxFiles = $UNIQFileNames{$UFN} * 1;

       print "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n";
       print "\nAny Diffs for this file? $UFN has sonde# = $UNIQFileNames{$UFN}. maxFiles = $maxFiles\n";

       if ($maxFiles > 1)
          {
          if ($debug) {print "Yes do Diffs of file versions. maxFiles = $maxFiles\n";}

          for (my $i=2; $i<=$maxFiles; $i++)
             {
             if ($debug) {print "Diff file. i=$i\n";}

             # Form and execute diff system commands
             my $cmd = sprintf("diff %s/%s_01_*.cls %s/%s_%02d_*.cls", $inputDir, $UFN, $inputDir, $UFN, $i );
             print "Execute: $cmd \n";

             system ($cmd);

             }

          } # maxFiles
       } # foreach UFN

       print "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n";

      } # diffExist


   } # diffDataFiles()
