#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The GTSBUFR_GetStats.pl script is used to gather stats from the *.preproc
# (preprocessed) GTS BUFR input files that have been converted to ASCII and then
# preprocessed into a cleaner (human readable) ASCII.</p>
#
# Inputs: Preprocessed GTS BUFR files converted from binary to ASCII (i.e.,
# *.preproc). See assumption #1 below.
#
# Outputs: Statistics and general info about each parameter found in the
#          preprocessed input files. Warning: depending on the number of
#          input files and stations being processed, this software can 
#          create numerous output files. It is wise to put the output files
#          in a separate output directory.
#
# Execute Cmd: GTSBUFR_GetStats.pl [input dir] [output dir] 
#   where "input dir" is the location of the *.preproc files to be processed
#   and "output dir" is the location where the (potentially numerous) output
#   stat files will be placed.
#
# Examples: 
#      GTSBUFR_GetStats.pl  ../orig_raw_input/antarctica ../output_stats/antarctica
#      GTSBUFR_GetStats.pl  ../orig_raw_input  ../output_stats
#
# WARNING: Do not place any *.out, *.out.s, *.txt, *.txt.s named files in the output
#   directory because they will be deleted by this software. Search for "WARNING REMOVAL" to
#   find the delete statement. This software creates and then deletes intermediate
#   *.out* and *.txt* files. The user may decided to COMMENT OUT the delete
#   command and do the cleanup manually.
#
# Assumptions:
#  0. User will search for all ASSUMPTIONS, WARNINGS, ERRORS, HARDCODED, PIBAL words.
#
#  1. That the input data has been pre-processed by the GTSBUFR preprocessing
#  software (preprocess_GTS_BUFR.pl) which converts incoming/raw GTS BUFR
#  binary data into ASCII using the bufr_dump (black box) software and then
#  cleans up the ASCII from bufr_dump into a more readable ASCII format. 
#
#  2. That the HARDCODED elements in this software have been updated for
#  the current project and data. 
#
#  3. That the only GTS BUFR parameters/elements that need to be processed
#  have been identified in the element hash tables below and that these
#  hash tables are correct. Warnings will be issued for incoming elements
#  that are not identified in these hash tables. See below. Search for
#  "Defined incoming header/data elements."
#
#  4.This s/w assumes that a set of data records starts with the "timePeriod"
#   rec. The next "timePeriod" rec found indicates the beginning of 
#   the next set of data for that next time. Note that for SOCRATES 2018
#   data, each timePeriod included 10 data elements/parameters including
#   the timePeriod parm.
#
#  5. This s/w assumes that the missing value for all input parms is "null".
#
#  6. User MUST select a set of sites to process. Search for phrase
#     "Select a set of Sites to process" or HARDCODED.
#
# @author Linda Cully December 2018
# @version GTS BUFR Sounding Get Statistics  1.0
# Originally developed for SOCRATES 2018 GTS BUFR data.
#
##Module------------------------------------------------------------------------
package GTSBUFR_GetStats;
use strict;

if (-e "/net/work") {
    use lib "/net/work/lib/perl/Utilities";
} else {
    use lib "/work/lib/perl/Utilities";
}
 
#use DpgCalculations;
#use DpgConversions;

printf "\nGTSBUFR_GetStats.pl began on ";print scalar localtime;printf "\n";

my $debug = 0;
my $debug2 = 0;

&main();
printf "\nGTSBUFR_GetStats.pl ended on ";print scalar localtime;printf "\n";

##------------------------------------------------------------------------------
# @signature void main()
# <p>Gather stats on the Preprocessed GTS BUFR *.preproc ASCII data files. </p>
##------------------------------------------------------------------------------
sub main 
   {
   my $converter = GTSBUFR_GetStats->new();
   $converter->getStats();
   } #main()

##------------------------------------------------------------------------------
# @signature GTSBUFR_GetStats new()
# <p>Create a new instance of a GTSBUFR_GetStats.</p>
#
# @output $self A new GTSBUFR_GetStats object.
##------------------------------------------------------------------------------
sub new 
   {
   my $invocant = shift;
   my $self = {};
   my $class = ref($invocant) || $invocant;
   bless($self,$class);
   
   $self->{"RAW_DIR"} = $ARGV[0];
   $self->{"OUTPUT_DIR"} = $ARGV[1];

   print "ARGV Values: Input RAW_DIR: $ARGV[0], OUTPUT_DIR: $ARGV[1] \n";

   return $self;
   } # new()

##------------------------------------------------------------------------------
# @signature void getStats()
# <p>Convert the raw data to the ESC format.</p>
##------------------------------------------------------------------------------
sub getStats
   {
   my ($self) = @_;
    
   mkdir($self->{"OUTPUT_DIR"}) unless (-e $self->{"OUTPUT_DIR"});
   $self->readDataFiles();

   $self->generateStats();

   } #getStats()

##------------------------------------------------------------------------------
# @signature void parseRawFiles(String file)
# <p>Read the data from the specified file and gather stats then print
#    to output file for further examination by science staff. </p>
#
# @input $file The name of the file to be parsed.
##------------------------------------------------------------------------------
sub parseRawFile 
   {
   my ($self,$file) = @_;

   if ($debug) {print "Enter parseRawFile: file = $file\n";}

   #--------------------------------
   # Form output file name and Open
   #--------------------------------
   my $outfile;

   my @filename_parts = split /\./, $file;
   $outfile = sprintf("%s.%s.stats", $filename_parts[0],$filename_parts[1]);

   printf("Input file name: %s ; Output file name is %s\n", $file, $outfile);

   open(my $OUT,">".$self->{"OUTPUT_DIR"}."/".$outfile)
        or die("Can't open output file for $file\n");

   #--------------------------------------------------------------------------------
   # Defined incoming header/data elements. HARDCODED
   #
   # All known possible header element types (in order as seen in initial dataset).
   # Note that some of these elements can be and are duplicated in the GTS headers.
   #
   # * The blockNumber added to beginning of stationNumber forms the WMO ID.
   #--------------------------------------------------------------------------------
   # Define the hash tables for used and not used elements/keys
   #--------------------------------------------------------------------------------
   my %headerElements_Used = (
     "typicalYear" => 1, "typicalMonth" => 1, "typicalDay" => 1,
     "typicalHour" => 1, "typicalMinute" => 1, "typicalSecond" => 1,
     "year" => 1, "month" => 1, "day" => 1,
     "hour" => 1, "minute" => 1, "second" => 1,
     "blockNumber" => 1,
     "stationNumber" => 1,
     "shipOrMobileLandStationIdentifier" => 1,
     "radiosondeType" => 1,
     "latitude" => 1,
     "longitude" => 1,
     "heightOfStationGroundAboveMeanSeaLevel" => 1,
     "height" => 1,
     "solarAndInfraredRadiationCorrection" => 1,
     "trackingTechniqueOrStatusOfSystem" => 1,
     "measuringEquipmentType" => 1,
     "cloudAmount" => 1,
     "heightOfBaseOfCloud" => 1,
     "cloudType" => 1,
     );

   my %headerElements_notUsed = (
     "bufrHeaderCentre" => 1,
     "bufrHeaderSubCentre" => 1,
     "dataCategory" => 1,
     "dataSubCategory" => 1,
     "internationalDataSubCategory" => 1,
     "masterTablesVersionNumber" => 1,
     "localTablesVersionNumber" => 1,
     "numberOfSubsets" => 1,
     "observedData" => 1,
     "compressedData" => 1,
     "unexpandedDescriptors" => 1,
     "subsetNumber" => 1,
     "timeSignificance" => 1,
     "heightOfBarometerAboveMeanSeaLevel" => 1,
     "stationElevationQualityMarkForMobileStations" => 1,
     "verticalSignificanceSurfaceObservations" => 1,
     "oceanographicWaterTemperature" => 1,
     "extendedDelayedDescriptorReplicationFactor" => 1,
     );

   my %footerElements_Used = (
     correctionAlgorithmsForHumidityMeasurements => 1,
     geopotentialHeightCalculation => 1,
     humiditySensorType => 1,
     pressureSensorType => 1,
     radiosondeOperatingFrequency => 1,
     radiosondeSerialNumber => 1,
     softwareVersionNumber => 1,
     temperatureSensorType => 1,
    );

   my %footerElements_notUsed = (
     absoluteWindShearIn1KmLayerAbove => 1,
     absoluteWindShearIn1KmLayerBelow => 1,
     radiosondeAscensionNumber => 1,
     delayedDescriptorReplicationFactor  => 1,
     operator => 1,
    );

  # --------------------------------------------------------------
  # Note that PIBALS have geopotentialHeight in every time period
  # but other soundings have nonCoordinateGeopotentialHeight.
  # Have not seen a sounding with both, but in that case code 
  # should default to the nonCoordinateGeopotentialHeight.
  # --------------------------------------------------------------
  my %dataElements_Used = (
     timePeriod => 1,
     pressure => 1,
     nonCoordinateGeopotentialHeight => 1,
     geopotentialHeight => 1,
     latitudeDisplacement => 1,
     longitudeDisplacement => 1,
     airTemperature => 1,
     dewpointTemperature => 1,
     windDirection => 1,
     windSpeed => 1,
     extendedVerticalSoundingSignificance => 1,
     );

  my %dataElements_notUsed = (
     );

  printf("\n----------\nProcessing file: %s\n",$file);

  open(my $FILE,$self->{"RAW_DIR"}."/".$file) or die("Can't open file: ".$file);

  my @lines = <$FILE>;
  close($FILE);

  # ---------------------------------------------
  # Go through all records in input file and
  # divide into Header, Footer, and data lines,
  # then call routines to specific stats.
  # Note that Footer recs are really header
  # recs that are at the bottom of the data file.
  # So, put footer recs into headerlines array.
  # ---------------------------------------------
  my %headerRecVal; 

  my @headerlines; 
  my @datalines; 

  my $ih = -1; my $total_ih = 0;   # Total count includes hdr and used footer recs
  my $id = -1; my $total_id = 0;
  my $total_if = 0;                # Total count is footer recs only
  my $totalRecs_all = 0;           # Total count of recs all kinds used and not used

  #--------------------------------------------------------
  # Loop through all the input lines and divide into types.
  #--------------------------------------------------------
  foreach my $line (@lines)
    {
    chomp ($line);
    if ($debug) {print "processing line: xxx $line xxx\n";}

    $totalRecs_all++;

    # ---------------------------------------
    # Skip any blank lines. 
    # ---------------------------------------
    # Shouldn't be any blank lines since they
    # should have been removed in preproc.
    # ---------------------------------------
    next if ($line =~ /^\s*$/);

    # -------------------------------------------------------------
    # Divide the input recs into HEADER and DATA arrays to process.
    # -------------------------------------------------------------
    my @record = split (/"/, $line);
    if ($debug2) {print "Split record/line: xxx @record xxx\n";}
     
    my $input_key = $record[3];
    if ($debug2) {print "Input Key: xxx $input_key xxx\n";}  # The key type is always 3rd element on split here.

    # only save comma separated element name and value
    my @record1 = split (/,/, $line);
    if ($debug2) {print "Split on comma record1: xxx @record1 xxx\n";}

    my @record2 = split (/:/, $record1[1]);
    if ($debug2) {print "Split record1[1] on colon. record2: xxx @record2 xxx\n";}

    my $value = $record2[1];
    if ($debug2) {print "Orig value: xxx $value xxx\n";}

    $value =~  s/}+$//g; # Strip off ending brace found on header recs.  HARDCODED
    if ($debug2) {print "value: xxx $value xxx\n";}

    # ------------------------------------------------------
    # Determine input element type and save to proper array
    # for later processing.
    # ------------------------------------------------------
    # This s/w assumes the hdr rec format is one of:
    #  { "key" : "typicalYear", "value" : 2018 },
    #  { "key" : "height", "value" : 41, "units" : "m" },
    #
    # Some elements have units but some do not.
    # ------------------------------------------------------
    if ( exists($headerElements_Used{$input_key}) )
       {
       if ($debug) {print "Known Header element type that is USED. $input_key\n"; }
       $total_ih++;
       $ih++;

       # Save off into header hash
       $headerRecVal{$input_key} = $value;
       if ($debug) {print "HASH headerRecVal(1): $input_key = $headerRecVal{$input_key}\n";}
  
       # Save off into header array. If missing/null, reset hdr elements to Unknown.
       if ($value eq "null") {$value = "Unknown";}

       $headerlines[$ih] = $input_key.",".$value;  # e.g., "typicalHour, 11"
       if ($debug) {print "ih, headerline: xxx $ih xxx $headerlines[$ih]xxx\n";}
       } 

    elsif ( exists($headerElements_notUsed{$input_key}) )
       {
       if ($debug) {print "Known HEADER element type but NOT USED. $input_key\n";}
       $total_ih++;
       }

    elsif ( exists($dataElements_Used{$input_key}) )
       {
       # ------------------------------------------------------------------------------
       # This s/w assumes this is the form of a data line.
       # Sample Data Line: { "key" : "windDirection", "value" : 44, "units" : "deg" },
       # ------------------------------------------------------------------------------
       if ($debug) {print "Known Data element type that is USED. $input_key\n"; }
       $total_id++;
       $id++;

       # Save off into data array
       $datalines[$id] =  $input_key.",".$value;  # e.g., "airTemperature, 265.85"
       if ($debug) {print "id, dataline: xxx $id xxx $datalines[$id]xxx\n";}

       } # Known Data Rec/Used

    elsif ( exists($dataElements_notUsed{$input_key}) )
       {
       if ($debug) {print "Known Data element type but NOT USED. $input_key\n";}
       $total_id++;
       }

    elsif ( exists($footerElements_Used{$input_key}) )
       {
       if ($debug) {print "Known Footer element type that is USED. $input_key\n"; }
       $total_if++;
       $ih++; #Add to hdr count

       # Save off into header/footer hash. Reset to Unknown if value is missing/null.
       if ($value eq "null") {$value = "Unknown";}

       $headerRecVal{$input_key} = $value;
       if ($debug) {print "HASH headerRecVal(2): $input_key = $headerRecVal{$input_key}\n";}

       # Save off into header array
       $headerlines[$ih] = $input_key.",".$value;  # e.g., "radiosondeSerialNumber, M1723487"
       if ($debug) {print "ih, headerline: xxx $ih xxx $headerlines[$ih]xxx\n";}
       }

    elsif ( exists($footerElements_notUsed{$input_key}) )
       {
       if ($debug) {print "Known Footer element type but NOT USED. $input_key\n"; }
       $total_if++;
       }

    else 
       {
       #-----------------------------------------------------------------------
       # Unknown element type. Not known Header, Footer, or Data type rec.
       # Don't really know what types of recs to expect in the future,
       # so warn if don't know the type. Might need to handle future rec types.
       #-----------------------------------------------------------------------
       print "WARNING: UNKNOWN input element type! $input_key, Input Line:$line\n\n";
       print $OUT  "WARNING: UNKNOWN input element type! $input_key, Input Line:$line\n\n";
       }

     if ($debug2) {print "ih (hdr+ftrUsed) = $ih,  id (data) = $id \n";}
     if ($debug2) {print "total_ih (hdr+(used)ftr) = $total_ih,  total_id (data) = $total_id, total_if (ftr only) = $total_if\n";}

     } # foreach on key type/line in raw data file


  if ($debug) {print "Header/Footer recs: @headerlines\n\n";}
  if ($debug) {print "\n\n Found: Hdr Recs= $total_ih, Ftr Recs= $total_if, Data Recs= $total_id\n";}

  print $OUT  "Total Recs All Types (includes blank): $totalRecs_all\n";
  print $OUT  "Total Header Recs: $total_ih, Total Footer Recs: $total_if, Total Data Recs: $total_id\n";

  #-----------------------------------------------------------------------------------
  #------ Process Header/Footer Lines ------------------------------------------------
  #-----------------------------------------------------------------------------------
  if ($debug) {print "Gather stats from HEADER lines and Write Stats to output.\n";}
  if ($debug) {print "Header/Footer recs: @headerlines\n\n";}

  my $allHeaderLines=0;

  foreach my $line (@headerlines)
     {
     $allHeaderLines++;
     # -------------------------------------------
     # Form of all expected DATA input lines:
     #     input_key , value
     # -------------------------------------------
     # Write Headerline stats to output file
     # -------------------------------------------
     my @record = split (/,/, $line);
     my $input_key = trim ($record[0]);
     my $value = trim ($record[1]);

     if ($debug) {print "HEADER Input Key: xxx $input_key xxx, value: xxx $value xxx\n";}

     if ( exists($headerElements_Used{$input_key}))
        {
        if ($value eq "null" )
           {print $OUT "USED Header Element: NULL $input_key\n"; }
        else
           {
           if ($input_key eq "radiosondeType")
              {
              #------------------------------------------------------------------
              # Set Radiosonde Type based on Code Table.
              #------------------------------------------------------------------
              # From ECMWF WMO code flag table 20 found at
              # https://confluence.ecmwf.int/display/ECC/WMO%3D20+code#-flag+table
              # Other pages on that wiki in many tables to use table 002011
              # for Radiosonde Type but updated tables can be
              # found in the doc at http://www.wmo.int/pages/prog/www/WMOCodes
              # /WMO306_vI1/Publications/2017update/WMO306_vI1_2011UP2017_en.pdf
              #------------------------------------------------------------------
              my %radiosondeType_CODE_TABLE = (
                  78 => "Vaisala RS90/Digicora III",
                  80 => "Vaisala RS92/Digicora III",
                  81 => "Vaisala RS92/Autosonde",
                 141 => "Vaisala RS41 with pressure derived from GPS height/DigiCORA MW41",
                 142 => "Vaisala RS41 with pressure derived from GPS height/AUTOSONDE ",
                 177 => "Modem GPSonde M10",
                 );

              my $radiosondeType = $headerRecVal{radiosondeType};
              $radiosondeType =~ s/^\s+//; # trim white space
              if ($debug) {print "radiosondeType = $radiosondeType\n"};

              my $sondeType = $radiosondeType_CODE_TABLE{eval $radiosondeType};
              $sondeType =~ s/^\s+//; # trim white space
              if ($debug) {print "sondeType = $sondeType\n"};

              print $OUT "USED Header Element non-null: $input_key = $value, sondeType = $sondeType \n";
              } 
           else # Not RadiosondeType
              { print $OUT "USED Header Element non-null: $input_key = $value\n"; }
           }   
        }
     elsif (exists($footerElements_Used{$input_key}))
        {
        if ($value eq "null" )
           {print $OUT "\nUSED Footer Element: NULL $input_key\n"; }
        else
           {print $OUT "\nUSED Footer Element non-null: $input_key = $value\n"; }
        }
     else
        {
        print $OUT "NOT USED Header/Footer Element: $input_key = $value\n";
        } # Header/Footer elements Used/Not Used

     } # foreach Header/Footer line

  # Write Headerline stats to output file

  #-----------------------------------------------------------------------------------
  #------Data Loop -------------------------------------------------------------------
  #-----------------------------------------------------------------------------------
  if ($debug) {print "Gather stats from DATA lines and Write Stats to output.\n";}
  if ($debug) {print "Data Lines:  @datalines\n";}

  my $pibal=1; # Reset to zero if find a temp or pressure. Pibals do not have these parms.

  my $allDataLines =0; my $allUnknownLines=0;

  my $GoodTimePeriod=0; my $nullTimePeriod=0; my $allTimePeriod=0;
  my $GoodWindSpeed=0; my $nullWindSpeed=0; my $allWindSpeed=0;
  my $GoodWindDirection=0; my $nullWindDirection=0; my $allWindDirection=0;
  my $GoodLatitudeDisplacement=0; my $nullLatitudeDisplacement=0; my $allLatitudeDisplacement=0;
  my $GoodLongitudeDisplacement=0; my $nullLongitudeDisplacement=0; my $allLongitudeDisplacement=0;
  my $GoodNonCoordinateGeopotentialHeight=0; my $nullNonCoordinateGeopotentialHeight=0; my $allNonCoordinateGeopotentialHeight=0;
  my $GoodGeopotentialHeight=0; my $nullGeopotentialHeight=0; my $allGeopotentialHeight=0;
  my $GoodPressure=0; my $nullPressure=0; my $allPressure=0;
  my $GoodAirTemperature=0; my $nullAirTemperature=0; my $allAirTemperature=0;
  my $GoodDewpointTemperature=0; my $nullDewpointTemperature=0; my $allDewpointTemperature=0;

  my $GoodExtendedVerticalSoundingSignificance=0; my $nullExtendedVerticalSoundingSignificance=0; 
  my $allExtendedVerticalSoundingSignificance=0; 
  my $ExtendedVerticalSoundingSignificanceVALUE65536=0; my $ExtendedVerticalSoundingSignificanceVALUEzero=0; 
  my $ExtendedVerticalSoundingSignificanceNULL=0;


  # ------------------------
  # Get Stats for Data Lines
  # ------------------------
  foreach my $line (@datalines)
     {         
     $allDataLines++;

     if ($debug) {print "\nProcess DATA line ($allDataLines): xxx $line xxx\n";}

     # -------------------------------------------
     # Form of all expected DATA input lines:
     #     input_key , value
     # -------------------------------------------
     my @record = split (/,/, $line);
     my $input_key = trim ($record[0]);
     my $value = trim ($record[1]);

     if ($debug) {print "DATA Input Key: xxx $input_key xxx, value: xxx $value xxx\n";}

     if ($input_key eq "timePeriod") 
        {
        if ($value eq "null" )
           {print "WARNING: Null timePeriod\n"; print $OUT "WARNING: Null timePeriod\n"; $nullTimePeriod++; $allTimePeriod++; }
        else
           {$GoodTimePeriod++; $allTimePeriod++;}
        }
     elsif ($input_key eq "windSpeed")
        {
        if ($value eq "null" )
           {print "WARNING: Null windSpeed\n"; print $OUT "WARNING: Null windSpeed\n"; $nullWindSpeed++; $allWindSpeed++; }
        else
           {$GoodWindSpeed++; $allWindSpeed++;}
        } # windSpeed
     elsif ($input_key eq "windDirection")
        {
        if ($value eq "null" )
           {print "WARNING: Null windDirection\n"; print $OUT "WARNING: Null windDirection\n"; $nullWindDirection++; $allWindDirection++; }
        else
           {$GoodWindDirection++; $allWindDirection++;}
        } # windDirection
     elsif ($input_key eq "latitudeDisplacement")
        {
        if ($value eq "null" )
           {print "WARNING: Null latitudeDisplacement\n"; print $OUT "WARNING: Null latitudeDisplacement\n"; $nullLatitudeDisplacement++; $allLatitudeDisplacement++; }
        else
           {$GoodLatitudeDisplacement++; $allLatitudeDisplacement++;}
        } # latitudeDisplacement
     elsif ($input_key eq "longitudeDisplacement")
        {
        if ($value eq "null" )
           {print "WARNING: Null longitudeDisplacement\n"; print $OUT "WARNING: Null longitudeDisplacement\n"; $nullLongitudeDisplacement++; $allLongitudeDisplacement++; }
        else
           {$GoodLongitudeDisplacement++; $allLongitudeDisplacement++;}
        } # longitudeDisplacement
     elsif ($input_key eq "nonCoordinateGeopotentialHeight")
        {
        if ($value eq "null" )
           {print "WARNING: Null nonCoordinateGeopotentialHeight\n"; print $OUT "WARNING: Null nonCoordinateGeopotentialHeight\n"; $nullNonCoordinateGeopotentialHeight++; $allNonCoordinateGeopotentialHeight++; }
        else
           {$GoodNonCoordinateGeopotentialHeight++; $allNonCoordinateGeopotentialHeight++;}
         } # nonCoordinateGeopotentialHeight
     elsif ($input_key eq "geopotentialHeight")
        {
        if ($value eq "null" )
           {print "WARNING: Null geopotentialHeight\n"; print $OUT "WARNING: Null geopotentialHeight\n"; $nullGeopotentialHeight++; $allGeopotentialHeight++; }
        else
           {$GoodGeopotentialHeight++; $allGeopotentialHeight++;}
        } # geopotentialHeight
     elsif ($input_key eq "pressure")
        {
        if ($value eq "null" )
           {print "WARNING: Null pressure\n"; print $OUT "WARNING: Null pressure\n"; $nullPressure++; $allPressure++; }
        else
           {$GoodPressure++; $allPressure++; $pibal = 0;}
        } # pressure
     elsif ($input_key eq "airTemperature")
        {
        if ($value eq "null" )
           {print "WARNING: Null airTemperature\n"; print $OUT "WARNING: Null airTemperature\n"; $nullAirTemperature++; $allAirTemperature++; }
        else
           {$GoodAirTemperature++; $allAirTemperature++; $pibal = 0;}
        } # airTemperature
     elsif ($input_key eq "dewpointTemperature")
        {
        if ($value eq "null" )
           {print "WARNING: Null dewpointTemperature\n"; print $OUT "WARNING: Null dewpointTemperature\n"; $nullDewpointTemperature++; $allDewpointTemperature++; }
        else
           {$GoodDewpointTemperature++; $allDewpointTemperature++;}
        } # dewpointTemperature 

     elsif ($input_key eq "extendedVerticalSoundingSignificance")
        {
        if ($value eq "null" )
           {
           print "WARNING: Null extendedVerticalSoundingSignificance\n"; print $OUT "WARNING: Null extendedVerticalSoundingSignificance\n"; $nullExtendedVerticalSoundingSignificance++; $allExtendedVerticalSoundingSignificance++;
           $ExtendedVerticalSoundingSignificanceNULL++;
           }
        else
           {
           $GoodExtendedVerticalSoundingSignificance++; $allExtendedVerticalSoundingSignificance++;
           if ($value == 65536) {$ExtendedVerticalSoundingSignificanceVALUE65536++;}
           if ($value == 0) {$ExtendedVerticalSoundingSignificanceVALUEzero++;}
           if (($value != 65536) && ($value != 0) && ($value ne "null" )) {print ($OUT "UNKNOWN ExtendedVerticalSoundingSignificance = $value\n")}

           }
        } # extendedVerticalSoundingSignificance
     else
        {
        print "WARNING: UNKNOWN DATA Record type = $input_key\n";
        print $OUT "WARNING: UNKNOWN DATA Record type = $input_key\n";
        $allUnknownLines++;
        } # unknown - issue warning!

     } # end foreach $line

  #--------------------------------------
  # Write Data stat output to output file 
  #--------------------------------------
  if ($pibal == 1)
     { print ($OUT "This sounding is a PIBAL. All Temps and Pressures are missing/null.\n") }
  else
     { print ($OUT "This sounding is NOT a PIBAL. Found at least one Temp or Pressure that is not missing.\n" )}

  print ($OUT "Data Stats::  ");
  print ($OUT "\n allDataLines= $allDataLines,\t\t allUnknownLines= $allUnknownLines\n GoodTimePeriod= $GoodTimePeriod,\t\t nullTimePeriod= $nullTimePeriod,\t\t allTimePeriod= $allTimePeriod\n GoodWindSpeed= $GoodWindSpeed,\t\t nullWindSpeed= $nullWindSpeed,\t\t allWindSpeed= $allWindSpeed\n GoodWindDirection= $GoodWindDirection,\t nullWindDirection= $nullWindDirection,\t\t allWindDirection= $allWindDirection\n GoodPressure= $GoodPressure,\t\t nullPressure= $nullPressure,\t\t allPressure= $allPressure\n GoodAirTemperature= $GoodAirTemperature,\t nullAirTemperature= $nullAirTemperature,\t allAirTemperature= $allAirTemperature\n GoodDewpointTemperature= $GoodDewpointTemperature,\t nullDewpointTemperature= $nullDewpointTemperature,\t allDewpointTemperature= $allDewpointTemperature\n\n GoodLatitudeDisplacement= $GoodLatitudeDisplacement, \t nullLatitudeDisplacement= $nullLatitudeDisplacement,\t\t allLatitudeDisplacement= $allLatitudeDisplacement\n GoodLongitudeDisplacement= $GoodLongitudeDisplacement,\t nullLongitudeDisplacement= $nullLongitudeDisplacement, \t\t allLongitudeDisplacement= $allLongitudeDisplacement\n GoodNonCoordinateGeopotentialHeight= $GoodNonCoordinateGeopotentialHeight, nullNonCoordinateGeopotentialHeight= $nullNonCoordinateGeopotentialHeight, allNonCoordinateGeopotentialHeight= $allNonCoordinateGeopotentialHeight\n GoodGeopotentialHeight= $GoodGeopotentialHeight, \t\t nullGeopotentialHeight= $nullGeopotentialHeight, \t\t allGeopotentialHeight= $allGeopotentialHeight\n GoodExtendedVerticalSoundingSignificance= $GoodExtendedVerticalSoundingSignificance, nullExtendedVerticalSoundingSignificance= $nullExtendedVerticalSoundingSignificance, allExtendedVerticalSoundingSignificance= $allExtendedVerticalSoundingSignificance, ExtendedVerticalSoundingSignificanceVALUE65536= $ExtendedVerticalSoundingSignificanceVALUE65536, ExtendedVerticalSoundingSignificanceVALUEzero= $ExtendedVerticalSoundingSignificanceVALUEzero \n" ); 

  if ($allExtendedVerticalSoundingSignificance != ($ExtendedVerticalSoundingSignificanceVALUE65536 + $ExtendedVerticalSoundingSignificanceVALUEzero+$ExtendedVerticalSoundingSignificanceNULL)) {print ($OUT "WARNING: There are ExtendedVerticalSoundingSignificance values that are NOT 65536, zero, nor Null!\n" );}

  if ($allExtendedVerticalSoundingSignificance == ($ExtendedVerticalSoundingSignificanceVALUE65536 + $ExtendedVerticalSoundingSignificanceVALUEzero+$ExtendedVerticalSoundingSignificanceNULL)) {print ($OUT "NOTE: All ExtendedVerticalSoundingSignificance values are either 65536, zero or Null!\n" );}

  if ( ($allNonCoordinateGeopotentialHeight != 0) && ($allGeopotentialHeight != 0) )
     { print ($OUT "WARNING: There are both NonCoordinateGeopotentialHeight and GeopotentialHeight in this sounding!\n"); }


  } # parseRawFile()

##------------------------------------------------------------------------------
# @signature void readDataFiles()
# <p>Read in the files from the raw data directory and gather stats. </p>
##------------------------------------------------------------------------------
sub readDataFiles 
   {
   my ($self) = @_;
    
   opendir(my $RAW,$self->{"RAW_DIR"}) or die("Can't read raw directory ".$self->{"RAW_DIR"});
   my @files = grep(/.preproc$/,sort(readdir($RAW)));   # HARDCODED
   closedir($RAW);

   if ($debug) {print "Input Files to Process: @files\n";}
    
   #----------------------------------------------------
   # Process all raw data files in specified directory.
   #----------------------------------------------------
   if ($debug) {printf("Raw Dir read. Now call parseRawFile() to process each input file.\n");}

   foreach my $file (@files) 
      { $self->parseRawFile($file); }
   
   } # readDataFiles()


##------------------------------------------------------------------------------
# @signature void generateStats()
# <p>Read in the files from the raw data directory and gather stats. </p>
##------------------------------------------------------------------------------
sub generateStats
   {
   my ($self) = @_;

   if ($debug) {print "Enter generateStats()\n";}

   #--------------------------------------------
   # Select a set of Sites to process. HARDCODED
   #--------------------------------------------
   #------------------  
   # Antarctica Sites:
   #------------------  
##   my @siteName = ( "casey", "davis", "mawson");  # HARDCODED

   #------------------  
   # Australia Sites:
   #------------------  
##   my @siteName = ( "adelaide", "albany", "ceduna", "esperance", "hobart", "kalgoorlie", "lord_howe_island", "macquarie_island", "melbourne", "perth", "sydney", "wagga_wagga", "williamtown", "woomera" );  # HARDCODED

   #------------------  
   # New Zealand Sites:
   #------------------  
##   my @siteName = ("invercargill", "paraparaumu");  # HARDCODED


   #------------------  
   # RELAMAPAGO Sites:
   #------------------  
   my @siteName = ("87155", "87244","87344","87418","87576","87623");  # HARDCODED


   # -------------------------------------------------------------------
   # Set parameter names to pull out from *.stats files then sort & uniq  HARDCODED
   # Note that Lat, Lon, Height parms done separately
   # -------------------------------------------------------------------
   my @parmNames = ("PIBAL", "blockNumber", "stationNumber", "shipOrMobileLandStationIdentifier", "radiosondeType", "sondeType", "solarAndInfraredRadiationCorrection", "trackingTechniqueOrStatusOfSystem", "measuringEquipmentType", "cloudAmount", "heightOfBaseOfCloud", "cloudType", "radiosondeSerialNumber", "radiosondeOperatingFrequency", "pressureSensorType", "temperatureSensorType", "humiditySensorType", "correctionAlgorithmsForHumidityMeasurements", "geopotentialHeightCalculation", "potentialHeight", "softwareVersionNumber");

   print "Output File Location:: $ARGV[1]\n";
   print "\n--------------------\nSites Processed:: siteName[] = @siteName\n";

   my $cmd = "";

   foreach my $i (0 .. $#siteName)
     { 
     my $site = $siteName[$i];

     # ----------------------------------------------------------------
     # latitude longitude heightOfStationGroundAboveMeanSeaLevel height
     # ----------------------------------------------------------------
     $cmd = sprintf ( "grep -h latitude  %s/*%s*.stats > %s/%s_llHt.out ", $self->{"OUTPUT_DIR"}, $site, $self->{"OUTPUT_DIR"}, $site);
     print "site = $site, cmd = $cmd\n";
     system ($cmd);

     $cmd = sprintf ( "grep -h longitude %s/*%s*.stats >> %s/%s_llHt.out ", $self->{"OUTPUT_DIR"}, $site, $self->{"OUTPUT_DIR"}, $site);
     print "site = $site, cmd = $cmd\n";
     system ($cmd);

     $cmd = sprintf ( "grep -h   heightOfStationGroundAboveMeanSeaLevel %s/*%s*.stats >> %s/%s_llHt.out ", $self->{"OUTPUT_DIR"}, $site, $self->{"OUTPUT_DIR"}, $site);
     print "site = $site, cmd = $cmd\n";
     system ($cmd);

     $cmd = sprintf ( "grep -h  height %s/*%s*.stats >> %s/%s_llHt.out ", $self->{"OUTPUT_DIR"}, $site, $self->{"OUTPUT_DIR"}, $site);
     print "site = $site, cmd = $cmd\n";
     system ($cmd);

     $cmd = sprintf ( "sort %s/%s_llHt.out > %s/%s_llHt.out.s; uniq %s/%s_llHt.out.s > %s/%s_llHt.out.su", $self->{"OUTPUT_DIR"}, $site, $self->{"OUTPUT_DIR"}, $site, $self->{"OUTPUT_DIR"}, $site, $self->{"OUTPUT_DIR"}, $site); print "site = $site, cmd = $cmd\n"; system ($cmd);


     # ------------
     # Other parms
     # ------------
     foreach my $i (0 .. $#parmNames)
       {
       my $parmID = $parmNames[$i];

       print "\nprocessing $site  parmID = $parmNames[$i]\n";

       $cmd = sprintf ( "grep -h  %s  %s/*%s*.stats > %s/%s_%s.out ",  $parmID, $self->{"OUTPUT_DIR"}, $site, $self->{"OUTPUT_DIR"}, $site, $parmID);

       print "site = $site, cmd = $cmd \n";
       system ($cmd);

       $cmd = sprintf ( "sort %s/%s_%s.out > %s/%s_%s.out.s; uniq %s/%s_%s.out.s > %s/%s_%s.out.su", $self->{"OUTPUT_DIR"}, $site, $parmID, $self->{"OUTPUT_DIR"}, $site, $parmID, $self->{"OUTPUT_DIR"}, $site, $parmID, $self->{"OUTPUT_DIR"}, $site, $parmID); 
       print "site = $site, cmd = $cmd \n"; 
       system ($cmd);

       } # for each parameter

     # Remove Extraneous *.out and sorted files. Only need Uniq'd files.
     $cmd = sprintf ( "/bin/rm -rf  %s/*%s*.out %s/*%s*.out.s",  $self->{"OUTPUT_DIR"}, $site, $self->{"OUTPUT_DIR"}, $site);

     print "WARNING: REMOVING *.out and *.out.s FILES from STATS OUTPUT DIRECTORY! site = $site, cmd = $cmd \n";
     system ($cmd);  # WARNING REMOVAL

     } # for each siteName


  # ----------------------------------------------
  # Generate overall stats for all sites combined. 
  # ----------------------------------------------
  foreach my $i (0 .. $#parmNames)
     {
     my $parmID = $parmNames[$i];
     print "\nprocessing All Sites  parmID = $parmNames[$i]\n";
 
     $cmd = sprintf ( "grep -h %s  %s/*_%s.out.su > %s/AllSites_%s.txt ",  $parmID, $self->{"OUTPUT_DIR"}, $parmID, $self->{"OUTPUT_DIR"}, $parmID);
     print "All Sites:  cmd = $cmd \n";
     system ($cmd);

     $cmd = sprintf ( "sort %s/AllSites_%s.txt > %s/AllSites_%s.txt.s; uniq %s/AllSites_%s.txt.s > %s/AllSites_%s.txt.su", $self->{"OUTPUT_DIR"}, $parmID, $self->{"OUTPUT_DIR"}, $parmID, $self->{"OUTPUT_DIR"}, $parmID, $self->{"OUTPUT_DIR"}, $parmID);
     print "AllSites:  cmd = $cmd \n";
     system ($cmd);

     } # for each parameter

  # Remove Extraneous AllSites_*.txt and sorted files. Only need Uniq'd files.
  $cmd = sprintf ( "/bin/rm -rf  %s/AllSites_*.txt %s/AllSites_*.txt.s",  $self->{"OUTPUT_DIR"}, $self->{"OUTPUT_DIR"});

  print "WARNING: REMOVING AllSites_*.txt and *.txt.s FILES from STATS OUTPUT DIRECTORY! cmd = $cmd \n";
  system ($cmd);  # WARNING REMOVAL

  } # readDataFiles()


##------------------------------------------------------------------------------
# @signature String trim(String line)
# <p>Remove all leading and trailing whitespace from the specified String.</p>
#
# @input $line The String to be trimmed.
# @output $line The trimmed String.
##------------------------------------------------------------------------------
sub trim {
    my ($line) = @_;
    return $line if (!defined($line));
    $line =~ s/^\s+//;
    $line =~ s/\s+$//;
    return $line;
} # trim()
