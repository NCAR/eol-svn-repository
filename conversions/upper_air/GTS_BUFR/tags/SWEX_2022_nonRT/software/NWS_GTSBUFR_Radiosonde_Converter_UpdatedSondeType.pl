#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The NWS_GTSBUFR_Radiosonde_Converter.pl script is used for converting 
# high resolution GTS BUFR "preprocessed" radiosonde data into ESC format.
# Note that this code only works on the preprocessed data that have been 
# converted using bufr_dump from binary bufr format into ascii that has been
# cleaned (e.g., blank lines removed, unnecessary chars removed, etc.) using
# the preprocessing software.  
#
# This s/w goes through the data twice. On the first pass, the header and
# data records are divided into arrays for processing. The data are studied
# per science staff request and messages/warnings may be issued.  The
# second pass converts the info in the previously created header and data
# arrays.  Note that the raw data has one element/parameter per line, where
# a "timePeriod" parameter divides the set of values for that time period.
# This s/w does not assume any order of the incoming records other than that
# "timePeriod" records separate each set of recs for that timePeriod. 
# The s/w recognizes and then handles each record per its type. It warns
# if it encounters an unrecognized record of any type. See a sample
# file for header and footer records. This s/w treats header and footer
# records as same since they both contain sounding information and not data.
#
# Here's a sample of a single timePeriod's records. 
#  { "key" : "timePeriod", "value" : 60, "units" : "s" },
#  { "key" : "extendedVerticalSoundingSignificance", "value" : 0, "units" : "FLAG TABLE" },
#  { "key" : "pressure", "value" : 98090, "units" : "Pa" },
#  { "key" : "nonCoordinateGeopotentialHeight", "value" : 304, "units" : "gpm" },
#  { "key" : "latitudeDisplacement", "value" : -0.00207, "units" : "deg" },
#  { "key" : "longitudeDisplacement", "value" : 0.00472, "units" : "deg" },
#  { "key" : "airTemperature", "value" : 286.97, "units" : "K" },
#  { "key" : "dewpointTemperature", "value" : 279.36, "units" : "K" },
#  { "key" : "windDirection", "value" : 285, "units" : "deg" },
#  { "key" : "windSpeed", "value" : 7.2, "units" : "m/s" }
# </p>
#
# Inputs: Preprocessed GTS BUFR files converted from binary to ASCII
#  and then converted to a more readable "preproc" ASCII format.
#  See assumption #1 below.  User must specify project, input data dir
#  and output data dir on command line. (Note that input and output dirs 
#  must exist before running this command.)
#
# Execute command: 
#    NWS_GTSBUFR_Radiosonde_Converter.pl [project] [input dir] [output dir] [Keep EVSS Recs Flag]
#
#    where "Keep EVSS Recs Flag" indicates to keep (1) or drop (0) incoming mandatory level data records.
#    Mandatory level data recs are indicated where the "extendedVerticalSoundingSignificance" has
#    a value of 65536.
#
# Examples:
#    NWS_GTSBUFR_Radiosonde_Converter.pl CFACT_2022 ../output_preproc_data ../output_esc 0 > & runCFACT.log &
#
# Outputs: GTS BUFR ASCII data files in ESC sounding format. The output
#  file names will contain "PIBAL" or "SONDE" depending on what this code
#  has determine the sonde type to be. Search for PIBAL to learn more.
#
# Assumptions and Warnings:
#  0. User will search for all ASSUMPTIONS, WARNINGS, ERRORS, HARDCODED, PIBAL words.
#     Per the science staff requests, this s/w can issue a variety of warnings
#     and errors.  This code is currently set to ONLY process data from the
#     United States. Search for United States in code. 
#
#  1. That the input data has been pre-processed by the GTSBUFR preprocessing
#  software (preprocess_GTS_BUFR.pl) which converts incoming/raw GTS BUFR
#  binary data into ASCII using the bufr_dump (black box) software and then
#  cleans up the ASCII from bufr_dump into a more readable ASCII format. 
#
#  2. That the HARDCODED elements in this software have been updated for
#  the current project and data, as needed.
#
#  3. That the only GTS BUFR parameters/elements that need to be processed
#  have been identified in the element hash tables below and that these
#  hash tables are correct. Warnings will be issued for incoming elements
#  that are not identified in these hash tables. See below. Search for
#  "Defined incoming header/data elements."
#
#  4.This s/w assumes that a set of data records starts with the "timePeriod"
#   record. The next "timePeriod" rec found indicates the beginning of 
#   the next set of data for that next time. Note that for SOCRATES 2018
#   data, each timePeriod included 10 data elements/parameters including
#   the timePeriod parm.
#
#  5. This s/w assumes that the missing value for all input parms is "null".
#
#  6. If the nonCoordinate geopotential height is available, this s/w sets
#    the output altitude to that. If that parm is not available, the s/w will use
#    the geopotential height. If neither are available in the input file
#    then the the missing altitude value is output. Expect that a single
#    sounding will have either one or the other geopotential height values
#    consistently throughout a single input file.  Probably will be consistent
#    for a single site throughout all that sites soundings. This s/w makes
#    a decision on which geopotential ht parm is available and uses that
#    throughout the data processing. It will *not* switch back and forth
#    between geoPotHt and nonCoordGeoPotHt. This is per science staff request.
#
#  7. In general, all input records are written to the output file. This is true 
#     whether or not there are duplications and/or records out of time order. Note
#     that all records are written out if s/w determines the sonde is a PIBAL
#     and this includes duplicate t=0 sec records.  Science staff has requested
#     that if input sonde is a regular sonde (has temp, press, and/or at least
#     one nonCoordGeoPotHt), then only output t=0 sec recs that have Temp/Press
#     values and are not Null/Missing.  This s/w does NOT sort the output recs.
#     Records are written out in the order they are read in.  To sort the output
#     files, see the build.xml file where we use the same sort s/w as used by
#     the NWS processing s/w. 
#
#  8. There are rare cases where this code will "exit"/quit. Search for "exit
#     to determine those cases.   
#
#  9. User must create input and output directories before running code. 
#
# 10. It is unknown whether or not this code will work for dropsondes.  It has
#     not been tested on dropsonde data as none were received for SOCRATES 2018.
#
# 11. This s/w only recognizes 4 countries/continents (Australia, New Zealand, 
#     Antarctica and the United States). User *MUST* search for and add countries to hash named 
#     "known_country". User will receive a warning for unknown country and 
#     that will be written into the output file.
#
# 12. User should note that a file named "warning.log" will be created in the
#     output directory specified by the user. Examine this log for additional
#     warnings and messages. 
#
# 13. User should have worked with science staff to determine if mandatory level
#     sounding records are to be included or not in the output. See KeepEVSSrecs flag.
#
# @author Linda Cully February 2019
# @version GTS BUFR Sounding  1.0
# Originally developed for SOCRATES 2018 GTS BUFR data.
#
# @version GTS BUFR Sounding  2.0
#  March 2019 -  LEC updated s/w per S. Loehrer's request as follows. If the key
#    data element is "extendedVerticalSoundingsignificance" and that element
#    has a value equal to "65536", then do NOT include that data record in the
#    *.cls output. User needs to be able to indicate via command line input
#    to drop or include records with this key and value. Per SL, soundings have
#    numerous duplicate data records. These are coming from the inclusion of the
#    mandatory level data in the BUFR files in addition to the routine 1 or 2
#    second resolution observations. If this key element is 65536, then the
#    data record is a mandatory level and not a 1 or 2 sec record. For SOCRATES,
#    we can exclude any records that have this value for the key. These records
#    are not "bad" in any sense of the word but do lead to us flagging other
#    data due to duplicate times and sometimes different data values.
#    BEWARE that the calc of the ascension rate is based on the current rec and
#    the previous rec, even in the previous rec is a "dropped" mandatory rec.
#    S. Loehrer is aware of this and says to leave code "as is".
#
# June 2019 - LEC added in radiosondeType = 177 = Modem GPSonde M10 (France)" to
#    process RELAMAPGO SMN BUFR data sites. Country Argentina ("ar") also added.
#
# March/April 2022 - LEC updated s/w to handle more radiosondeTypes and to bring code
#    updates for RELAMPAGO processing together into single code set. This includes
#    the EVSS handling software.  This code was used to processed CFACT 2022 and 
#    WINTRE-MIX 2022 NWS GTS BUFR data taken from the FDA dataset 100.030. Added
#    code to handle several new input BUFR elements. Added code to ingest a
#    specific (HARDCODED to be "NWS_GTS_BUFR_station.lst") station list file that 
#    has the form for each line of "Id;WBAN;WMO;Name;" where sample lines would be:
#
#    Id;WBAN;WMO;Name;
#    KAAA;99999;99999;Anytown, NM;
#    KABQ;23050;72365;Albuquerque, NM;
#    KABR;14929;72659;Aberdeen, SD;
#    KALB;54775;72518;Albany, NY;
#
#    IMPORTANT: The NWS_GTS_BUFR_station.lst file must be located in the same dir
#    as where this conversion software is executed. 
#
##Module------------------------------------------------------------------------
package NWS_GTSBUFR_Radiosonde_Converter;
use strict;

if (-e "/net/work") {
    use lib "/net/work/lib/perl/Utilities";
    use lib "/net/work/lib/perl/UpperAir";
    use lib "/net/work/lib/perl/Station";
} else {
    use lib "/work/lib/perl/Utilities";
    use lib "/work/lib/perl/UpperAir";
    use lib "/work/lib/perl/Station";
}
 
use SimpleStationMap;
use Station;
use ClassConstants qw(:DEFAULT);
use ClassHeader;
use ClassRecord;
use DpgCalculations;
use DpgConversions;
my ($WARN);

printf "\nNWS_GTSBUFR_Radiosonde_Converter.pl began on ";print scalar localtime;printf "\n";

my $debug = 0; 
my $debug2 = 0; # detailed debug

my $KeepEVSSrecs = 1; # User defined Command Line Flag. Initialize to keep mandatory recs.

# -----------------------------------------------------------------------------------
# Name of input file that contains a complete list of the NWS station/site infol
# The form of the each line of the file is: Id;WBAN;WMO;Name; Here are a few sample
# lines from the (Hand Created/HARDCODED) NWS_GTS_BUFR_station.lst input file:
#
#    KABR;14929;72659;Aberdeen, SD;
#    KALB;54775;72518;Albany, NY;
#    KAMA;23047;72363;Amarillo, TX;
#    KAPX;04837;72634;Gaylord, MI;
# -----------------------------------------------------------------------------------
my $Input_Station_Info_File= "NWS_GTS_BUFR_station.lst"; # HARDCODED
my %site_info;

printf "\nHARDCODED:: Input Station Info File: %s \n", $Input_Station_Info_File;

&main();
printf "\nNWS_GTSBUFR_Radiosonde_Converter.pl ended on ";print scalar localtime;printf "\n";

#-------------------------------------------------
# CODE TABLE Translations for recognized data elements.
# Only include for those elements that are used.
#
# This may need to be changed/updated per
# the data being processed. Not all elements
# are translated. 
#-----------------------------------------------
# List of found elements with CODEs:
#-----------------------------------------------
# "cloudAmount", "cloudType", 
# "correctionAlgorithmsForHumidityMeasurements"
# "geopotentialHeightCalculation"
# "humiditySensorType", "measuringEquipmentType"
# "pressureSensorType", "radiosondeType"
# "solarAndInfraredRadiationCorrection"
# "stationElevationQualityMarkForMobileStations"
# "temperatureSensorType", "timeSignificance"
# "trackingTechniqueOrStatusOfSystem"
# "verticalSignificanceSurfaceObservations"
#------------------------------------------------------------------

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the GTSBUFR radiosonde data by converting it from 
# the preprocessed ASCII format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main 
   {
   my $converter = NWS_GTSBUFR_Radiosonde_Converter->new();
   $converter->convert();
   } #main()

##------------------------------------------------------------------------------
# @signature NWS_GTSBUFR_Radiosonde_Converter new()
# <p>Create a new instance of a NWS_GTSBUFR_Radiosonde_Converter.</p>
#
# @output $self A new NWS_GTSBUFR_Radiosonde_Converter object.
##------------------------------------------------------------------------------
sub new 
   {
   my $invocant = shift;
   my $self = {};
   my $class = ref($invocant) || $invocant;
   bless($self,$class);
   
   $self->{"stations"} = SimpleStationMap->new();

   $self->{"PROJECT"} = $ARGV[0]; 
   $self->{"NETWORK"} = "GTS BUFR Sounding Data/Ascending"; # HARDCODED
   
   $self->{"STNINFO_DIR"} = "./"; # HARDCODED - Expect stn info file to be in dir where code is executed.

   $self->{"RAW_DIR"} = $ARGV[1];     
   $self->{"OUTPUT_DIR"} = $ARGV[2]; 
   $KeepEVSSrecs = $ARGV[3];  # "extendedVerticalSoundingsignificance" record inclusion flag  (1=keep, 0=drop)

   print "ARGV Values: PROJECT: $ARGV[0], Input RAW_DIR: $ARGV[1], OUTPUT_DIR: $ARGV[2],  KeepEVSSrecs:  $ARGV[3]\n";

   $self->{"STATION_FILE"} = sprintf("%s/%s_sounding_stationCD.out",$self->{"OUTPUT_DIR"},
                 $self->cleanForFileName($self->{"PROJECT"}));
   $self->{"WARN_LOG"} = $self->{"OUTPUT_DIR"}."/warning.log";

   return $self;
   } # new()

##------------------------------------------------------------------------------
# @signature Station buildDefaultStation(String station_id, String network)
# <p>Create a default station for the GTSBUFR network using the 
# specified station_id and network.</p>
#
# @input $station_id The identifier of the station to be created.
# @input $network The network the station belongs to.
# @return The new station object with the default values for the network.
##------------------------------------------------------------------------------
sub buildDefaultStation 
   {
   my ($self,$station_id,$network) = @_;
   my $station = Station->new($station_id,$network);

   # --------------------------------------------------------------------------------------
   # Following info goes into the stationCD.out file. This file is not used at this time
   # but still generated.
   #
   # The items below should be updated with more precise info.  HARDCODED - FIX when/if known.
   # Note that since this code may process anything coming off the GTS, these might 
   # vary by project and sonde type. :w
   #
   # --------------------------------------------------------------------------------------
   $station->setStationName("GTSBUFR");  
   $station->setStateCode("99");         
   $station->setReportingFrequency("no set schedule"); 
   $station->setNetworkIdNumber("99"); 
   $station->setPlatformIdNumber(999); 
   $station->setMobilityFlag("f");   # Set to Fixed but could be "m" or mobile for some networks.

   return $station;
   } # buildDefaultStation()

##------------------------------------------------------------------------------
# @signature String buildLatLonFormat(String value)
# <p>Generate the decimal format for the specified value.</p>
#
# format length must be the same as the value length or
# convertLatLong will complain (see example below)
# base lat =   36.6100006103516 base lon =    -97.4899978637695
# Lat format = DDDDDDDDDDDDDDDD  Lon format = -DDDDDDDDDDDDDDDD  
#
# @input $value The value of the lat/lon being formatted.
# @output $fmt The format that corresponds the the value.
##------------------------------------------------------------------------------
sub buildLatLonFormat 
   {
   my ($self,$value) = @_;
    
   my $fmt = $value < 0 ? "-" : "";
   while (length($fmt) < length($value)) { $fmt .= "D"; }
   return $fmt;
   } # buildLatLonFormat()

##-------------------------------------------------------------------------
# @signature String cleanForFileName(String text)
# <p>Remove hyphens/convert spaces to underscores in a String so 
#    it can be used in a file name.</p>
#
# @input $text The String to be cleaned.
# @output $text The cleaned up String.
##-------------------------------------------------------------------------
sub cleanForFileName 
   {
   my ($self,$text) = @_;

   # Convert spaces to underscores.
   $text =~ s/\s+/_/g;

   # Remove all hyphens
   $text =~ s/\-//g;

   return $text;
   } # cleanForFileName()

##------------------------------------------------------------------------------
# @signature void convert()
# <p>Convert the raw data to the ESC format.</p>
##------------------------------------------------------------------------------
sub convert 
   {
   my ($self) = @_;
    
   mkdir($self->{"OUTPUT_DIR"}) unless (-e $self->{"OUTPUT_DIR"});
    
   $self->readDataFiles();
   $self->printStationFiles();
   } #convert()

##------------------------------------------------------------------------------------------
# @signature ClassHeader parseHeader(String file, Hash Rec=>value)
# <p>Parse the header lines from the specified file into the ESC header format.</p>
#
# @input $file The name of the file being parsed. 
# @input %headerRecVal Hash containing all header key_inputs and values for this sounding
#    file.
# @output $header The header data in ESC format.
#
# WAS: Assumption: Input file name form/example: 201803071200.casey_aq_sounding_1.preproc 
# NOW: Assumption: Input file name form/example: KSLC_202202130000_ius_sounding_1.preproc 
#
# Add in code tables for balloonManufacturer, balloonType, humiditySensorType. - Mar/Apr 2022
##-------------------------------------------------------------------------------------------
sub parseHeader 
   {
   my ($self,$file,%headerRecVal) = @_;
   my $header = ClassHeader->new();

   if ($debug)
     {
     print "\n------Enter ParseHeader()------------\nheaderRecVal Hash::\n";
     foreach my $key (keys %headerRecVal)
       {
       print "$key = $headerRecVal{$key} \n";
       }
     } #debug2


   # ------------------------------------------------------
   # Set the type of sounding
   # ------------------------------------------------------
   $header->setType("GTS BUFR Sounding Data"); # HARDCODED
   $header->setReleaseDirection("Ascending"); # HARDCODED
   $header->setProject($self->{"PROJECT"});   # HARDCODED

   # ------------------------------------------------------
   # The Id will be the prefix of the output file
   # and appears in the stationCD.out file
   # ------------------------------------------------------
   $header->setId("GTS");  #HARDCODED 

   # ------------------------------------------------------
   # Get Header/Footer Info for output sounding header.
   # Loc info from input file name
   # ------------------------------------------------------
   #--------------------------------------------------------
   #   Header Lines in header array are of the form:
   #     parm_name, parm_value
   #
   #  Can not assume the order in which the records will be
   #  in raw data.
   #
   #  Header/Footer Lines that are used:
   #
   #  blockNumber, stationNumber
   #  latitude, longitude, height
   #  radiosondeSerialNumber
   #  radiosondeType
   #  softwareVersionNumber
   #  radiosondeOperatingFrequency   - Drop per SL as of 22 March 2022
   #  typicalYear, typicalMonth, typicalDay, typicalHour, typicalMinute, typicalSecond  (Nominal date/time)
   #  year, month, day, hour, minute, second  (Actual date/time)
   #
   #  Added 23 Mar 2022:
   #  radiosondeAscensionNumber
   #  balloonManufacturer / balloonType
   #  weightOfBalloon
   #  humiditySensorType
   #  Ground Station Software
   #
   # --------------------------------------------------------
   # Following are not in header output:
   #  cloudAmount, heightOfBaseOfCloud, cloudType 
   #  heightOfStationGroundAboveMeanSeaLevel
   #  shipOrMobileLandStationIdentifier, 
   #  geopotentialHeight    - Can be in each time period/rec
   # --------------------------------------------------------
   #  Header/Footer Lines that need to be saved for data
   #  description document/readme:
   #     solarAndInfraredRadiationCorrection
   #     trackingTechniqueOrStatusOfSystem
   #     measuringEquipmentType
   #     pressureSensorType
   #     temperatureSensorType
   #     humiditySensorType   <<-------
   #     correctionAlgorithmsForHumidityMeasurements
   #     geopotentialHeightCalculation
   #--------------------------------------------------------
   # Set header Release Site Type/Site ID
   #--------------------------------------------------------
   if (($headerRecVal{blockNumber} > 0) && $headerRecVal{stationNumber} >0)
     {
     my $site = "";

     # ---------------------------------------------------------------------------------------------------------
     # WAS::
     # Extract site name from file name   ASSUMPTION file name format: 201803071200.casey_aq_sounding_1.preproc
     # Note that station name may be longer than single word so must count from the end of word to find all
     # site name parts.  E.g., 201712251200.macquarie_island_au_sounding_1.preproc, 
     # 201803071200.lord_howe_island_au_sounding_1.preproc .  See RELAMPAGO 2018-2019
     # GTSBUFR_Radiosonde_Converter.pl at line 1843 for the code to process those file names. 
     #
     # For NWS GTS BUFR data from data archive dataset 100.030:
     #     Input file name form KSLC_202202130000_ius_sounding_1.preproc so for site name there should no
     #     longer be issues with site names that are multiple works. We will be using the site IDs (e.g., KSLC)
     #     Note that there are *ius* and *iuk* files in dataset 100.030. Both should/could be processed. Work
     #     with the associate scientist to determine the files to process.
     #
     # ---------------------------------------------------------------------------------------------------------
     my @filename_parts = split /\./, $file;  # KSLC_202202130000_ius_sounding_1  
     my @filename_subparts = split /\_/, $filename_parts[0]; # KSLC 202202130000 ius sounding 1

     if ($debug) {print "\nInput file xxx $file xxx\n";}
     if ($debug) {print "\nfilename_parts xxx @filename_parts xxx\n";}
     if ($debug) {print "\nfilename_subparts xxx @filename_subparts xxx\n";}

     my $fileArrayLength = scalar (@filename_subparts);

     #---------------------------------------------------------------------------------------------
     # WAS: In the RELAMPAGO data, the country was in the file name:
     #      my $locCountry = $fileArrayLength-3;  # HARDCODED for international project BUFR sondes
     #      my $country = $filename_subparts[$locCountry];
     #      
     # March 2022: This code only processes NWS soundings in the United States so set country to US.
     #      The data files from dataset 100.030 do NOT have the country included.
     #---------------------------------------------------------------------------------------------
     my $country = "us";  # HARDCODED to only process NWS data from the United States.
     if ($debug) {print "\nInput file xxx $file xxx Raw Country = $country\n";}

     #--------------------------------------------------------------------
     # HARDCODED: Only recognized countries/continents. 
     #     User must add countries as new ones are encountered.
     #     March 2022: Add United States for NWS GTS BUFR data processing.
     #     The following section on Country is not used for NWS GTS BUFR.
     #--------------------------------------------------------------------
     my %known_country = (
        us => "United States",
        aq => "Antarctica",
        nz => "New Zealand",
        au => "Australia",
        );

     if ($debug2)
        {
        print "known_country Hash::\n";
        foreach my $key (keys %known_country)
          {
          print "$key = $known_country{$key}\n";
          }
        } #debug2

     if ( exists($known_country{$country}) )
        {
        $country = $known_country {$country};
        }
     else
        {
        print "WARNING: Unknown Country!\n";
        $country = "Unknown Country";
        }

     if ($debug2) {print "\nInput file xxx $file xxx Country = $country\n";}


     # ----------------------------------------------------------------------
     # For RELAMPAGO data: 
     #    Form Site name. Can be multiple words long. The
     #    following code was used:
     #     for my $i (0 .. $locCountry-1)
     #        {
     #        if ($debug) {print "\nForming site xxx $site xxx i = $i\n";}
     #
     #        $site = $site.$filename_subparts[$i]." ";
     #        $site =~ s/(\w+)/\u$1/g; # Capitalize first letter of each word
     #        }
     # ----------------------------------------------------------------------
     # ----------------------------------------------------------------------
     # For NWS GTS BUFR from dataset 100.030:
     #    Site name and ID will be 4 chars (e.g., KSLC). 
     # ----------------------------------------------------------------------
     $site = $filename_subparts[0];  # Use the NWS Site ID (KSGF) at front of file name.

     if ($debug) {print "From INPUT File Name: site = xxx $site xxx\n";}

     # Reform full site name but without country. e.g. "KBOI Boise, ID / 72681".

     # ------------------------------------------------------------
     # Form WBAN number from Block and Station Number in GTS header
     # and then form and set Site name/ID.
     # Was:     $site = $site.", ".$country." / ".$BNstr.$SNstr;
     # ------------------------------------------------------------
     my $BNstr =$headerRecVal{blockNumber};  $BNstr =~ s/^\s+//;
     my $SNstr =$headerRecVal{stationNumber}; $SNstr =~ s/^\s+//; #Fix First char to Upper Case

     my $site_name = "UnknownTown, UnknownState";

     if ( exists($site_info{$site}) ) # $site=KBOI
        {
        $site_name= $site_info{$site};  # "Boise, ID"
        }
     else
        {
        print "WARNING: Unknown NWS Site!\n";
        $site_name = "UnknownTown, UnknownState"; 
        }

     if ($debug2) {print "\nInput file xxx $file xxx Country = $country\n";}

     $site = $site." ".$site_name." / ".$BNstr.$SNstr;

     $header->setSite($site);

     if ($debug) {print "site = xxx $site xxx\n";}

     } # Have blockNumber and stationNumber

   # --------------------------
   # Form/output Lat/Lon/Elev
   # --------------------------
   my $sfc_elev = $headerRecVal{height};
   $header->setAltitude($sfc_elev,"m");
   if ($debug) {print "height = xxx $sfc_elev xxx\n";}

   my $sfc_lat = $headerRecVal{latitude} + 0.0;
   my $sfc_lon = $headerRecVal{longitude} + 0.0;

   $header->setLatitude($sfc_lat, $self->buildLatLonFormat($sfc_lat));
   $header->setLongitude($sfc_lon, $self->buildLatLonFormat($sfc_lon)); 
   if ($debug) {print "sfc_lat, sfc_lon: $sfc_lat, $sfc_lon\n"};

   #------------------------------------------------------------------
   # Set Radiosonde Type based on Code Table.  HARDCODED
   #------------------------------------------------------------------
   # From ECMWF WMO code flag table 20 found at
   # https://confluence.ecmwf.int/display/ECC/WMO%3D20+code-flag+table
   # Other pages on that wiki in many tables to use table 002011
   # for Radiosonde Type but updated tables can be
   # found in the doc at http://www.wmo.int/pages/prog/www/WMOCodes
   # /WMO306_vI1/Publications/2017update/WMO306_vI1_2011UP2017_en.pdf
   #
   # Updated with several new sonde types taken from Java NWS RRS
   # data conversion software and RELAMPAGO type 177.
   # ------------------------------------------------------------------
   my %radiosondeType_CODE_TABLE = (
      51 => "VIZ-B2 (USA)",
      52 => "Vaisala RS80-57H",
      78 => "Vaisala RS90/Digicora III",
      80 => "Vaisala RS92/Digicora III",
      81 => "Vaisala RS92/Autosonde",
      87 => "Sippican Mark IIA with chip thermistor, pressure", 
     124 => "Vaisala RS41/AUTOSONDE ",
     141 => "Vaisala RS41 with pressure derived from GPS height/DigiCORA MW41",
     142 => "Vaisala RS41 with pressure derived from GPS height/AUTOSONDE ",
     152 => "Vaisala RS92-NGP/Intermet IMS-2000", 
     154 => "Graw DFM-17", 
     177 => "Modem GPSonde M10 (France)",
     182 => "Lockheed Martin Sippican LMS-6 GPS Radiosonde",
     255 => "Missing",
     );


   # Note humiditySensorType values 7-29 are "Reserved"
   my %humiditySensorType_CODE_TABLE = (
       0 => "VIZ Mark II Carbon Hygristor",
       1 => "VIZ B2 Hygristor",
       2 => "Vaisala A-Humicap",
       3 => "Vaisala H-Humicap",
       4 => "Capacitance sensor",
       5 => "Vaisala RS90",
       6 => "Sippican Mark IIA Carbon Hygristor",
       7 => "Twin alternatively-heated humicap capacitance sensor",
       8 => "Humicap capacitance sensor with active de-icing method",
       9 => "Carbon hygristor",
       10 => "Psychrometer",
       11 => "Capacitive (polymer)",
       12 => "Capacitive (ceramic, including metal oxide)",
       13 => "Resistive (generic)",
       14 => "Resistive (salt polymer)",
       15 => "Resistive (conductive polymer)",
       16 => "Thermal conductivity",
       17 => "Gravimetric",
       18 => "Paper-metal coil",
       19 => "Ordinary human hair",
       20 => "Rolled hair (torsion)",
       21 => "Goldbeater's skin",
       22 => "Chilled mirror hygrometer",
       23 => "Dew cell",
       24 => "Optical absorption sensor",
       30 => "Other",
       31 => "Missing value",
     );

   #------------------------------------
   # Determine/translate Radiosonde Type
   #------------------------------------
   if ($debug) {print "\nDetermine Radiosonde Type\n";}

   my $sondeType = "N/A"; 
   my $RadioSondeType = "N/A";

   if ( exists($headerRecVal{radiosondeType}))  # The Key exists and has value == True
      {
      $RadioSondeType = $headerRecVal{radiosondeType};
      $RadioSondeType =~ s/^\s+//; # trim white space

      if ( exists($radiosondeType_CODE_TABLE{eval $RadioSondeType}))
         {
         $sondeType = $radiosondeType_CODE_TABLE{eval $RadioSondeType};
         $sondeType =~ s/^\s+//; # trim white space
         }
      else
         {
         print "\nWARNING(1): Unknown radiosondeType or sondeType! \n";
         $sondeType = "N/A";
         }

      if ($debug) {print "Exists:: RadioSondeType = xxx $RadioSondeType xxx; sondeType = xxx $sondeType xxx\n";}
      }
   else
      {
      print "\nWARNING(2): Unknown radiosondeType or sondeType! \n";
      $sondeType = "N/A";
      }

   if ($debug) {print "RadioSondeType = xxx $RadioSondeType xxx; sondeType = xxx $sondeType xxx\n";}

   #-----------------------------------------
   # Determine/translate Humidity Sensor Type
   #-----------------------------------------
   if ($debug) {print "\nDetermine Humidity Sensor Type\n";}

   my $RH_Type = "N/A";
   my $RH_sensor_Type = "N/A";

   if ( exists($headerRecVal{humiditySensorType}))  # The Key exists and has value == True
      {
      $RH_sensor_Type = $headerRecVal{humiditySensorType};
      $RH_sensor_Type =~ s/^\s+//; # trim white space

      if ( exists($humiditySensorType_CODE_TABLE{eval $RH_sensor_Type}))
         {
         $RH_Type = $humiditySensorType_CODE_TABLE{eval $RH_sensor_Type};
         $RH_Type =~ s/^\s+//; # trim white space
         }
      else
         {
         print "\nWARNING(1): Unknown humiditySensorType or RH_Type!\n";
         $RH_Type = "N/A";
         }

      if ($debug) {print "Exists:: RH_sensor_Type = xxx $RH_sensor_Type xxx; RH_Type = xxx $RH_Type xxx\n";}
      }
   else
      {
      print "/n WARNING(2): Unknown humiditySensorType or RH_Type!\n";
      $RH_Type = "N/A";
      }

   if ($debug) {print "RH_sensor_Type = xxx $RH_sensor_Type xxx; RH_Type = xxx $RH_Type xxx\n";}

   $header->setLine(9,"Radiosonde Type/RH Sensor Type:", $sondeType." / ".$RH_Type);


   #------------------------------------
   # Note balloonManufacturer values 3-61 are "Reserved"
   #------------------------------------
   my %balloonManufacturer_CODE_TABLE = (
       0 => "Kaysam",
       1 => "Totex",
       2 => "KKS",
       3 => "Guangzhou Shuangyi (China)",
       4 => "ChemChina Zhuzhou (China)",
      62 => "Other",
      63 => "Missing value",
     );

   #------------------------------------
   # Note balloonType values 7-29 are "Reserved"
   #------------------------------------
   my %balloonType_CODE_TABLE = (
       0 => "GP26",
       1 => "GP28",
       2 => "GP30",
       3 => "HM26",
       4 => "HM28",
       5 => "HM30",
       6 => "SV16",
       7 => "Totex TA type balloons",
       8 => "Totex TX type balloons",
      30 => "Other",
      31 => "Missing value",
     );


   # ---------------------------------------------
   # Determine/translate balloonManufacturer
   # ---------------------------------------------
   my $balloonManufacturer = "N/A";
   my $balloonMftr = "N/A";

   if ($debug) {print "\n\nxxxxxxxxx\nxxx Determine balloonManufacturer balloonManufacturer = xxx $balloonManufacturer xxx.\n";}

   if ( exists($headerRecVal{balloonManufacturer}))  # The Key exists and has value == True
      {
      $balloonManufacturer = $headerRecVal{balloonManufacturer};
      $balloonManufacturer =~ s/^\s+//; # trim white space

      if ($debug) {print "\nxxx Value Pulled from headerRecVal: balloonManufacturer = xxx $balloonManufacturer xxx\n";}

      if ($balloonManufacturer eq "null") 
         {
         $balloonManufacturer = "N/A"; # Reset "null" value to N/A
         print "\nxxx Value Pulled from headerRecVal RESET: balloonManufacturer = xxx $balloonManufacturer xxx\n";
         }
      else 
         {
         if ($debug) {print "\nxxx Have non null value so translate: balloonManufacturer = xxx $balloonManufacturer xxx\n";}

         if ( exists($balloonManufacturer_CODE_TABLE{eval $balloonManufacturer} ))
            {
            $balloonMftr = $balloonManufacturer_CODE_TABLE{eval $balloonManufacturer};
            $balloonMftr =~ s/^\s+//; # trim white space

            if ($debug) {print "\nxxx Translate value to Code Value: balloonMftr = xxx $balloonMftr xxx\n";}

            if ($debug) {print "xxx EXISTS balloonManufacturer_CODE_TABLE:: balloonManufacturer = xxx $balloonManufacturer xxx; balloonMftr = xxx $balloonMftr xxx\n";}
            }
         else
            {
            print "\nxxx WARNING(1): Unknown balloonManufacturer! NO EXISTS in balloonManufacturer_CODE_TABLE! Reset to N/A.\n";
            $balloonMftr = "N/A";

            if ($debug) {print "xxx Does NOT EXIST balloonManufacturer_CODE_TABLE:: balloonManufacturer = xxx $balloonManufacturer xxx; balloonMftr = xxx $balloonMftr xxx\n";}
            }
         } # Not null

      if ($debug) {print "xxx balloonManufacturer = xxx $balloonManufacturer xxx; balloonMftr = xxx $balloonMftr xxx\n";}

      } # exists headerRecVal() check
   else
      {
      print "xxx WARNING(2): balloonManufacturer DOES NOT EXIST or have value in headerRecVal(). Reset to N/A.\n";
      $balloonMftr = "N/A";
      } # No exist in headerRecVal() check

   if ($debug) {print "AFTER ALL: balloonManufacturer = xxx $balloonManufacturer xxx; balloonMftr = xxx $balloonMftr xxx\n\n";}


   #------------------------------------
   # Translate Balloon Type code
   #------------------------------------
   if ($debug) {print "\nDetermine Balloon Type\n";}

   my $BType_info = "N/A";
   my $balloon_type = "N/A";

   if ( exists($headerRecVal{balloonType}))  # The Key exists and has value == True
      {
      $BType_info = $headerRecVal{balloonType};
      $BType_info =~ s/^\s+//; # trim white space

      if ($BType_info eq "null")
         {
         $balloonManufacturer = "N/A"; # Reset "null" value to N/A
         }
      else
         {
         if ( exists($balloonType_CODE_TABLE{eval $BType_info}))
            {
            $balloon_type = $balloonType_CODE_TABLE{eval $BType_info};
            $balloon_type =~ s/^\s+//; # trim white space
            }
         else
            {
            print "WARNING(1): Unknown BType_info or balloon_type!\n";
            $balloon_type = "N/A";
            }
         } # Not null

      if ($debug) {print "Exists:: BType_info = xxx $BType_info xxx; balloon_type = xxx $balloon_type xxx\n";}
      } # exists headerRecVal() check
   else
      {
      print "WARNING(2): Unknown BType_info or balloon_type!\n";
      $balloon_type = "N/A";
      } # No exist in headerRecVal() check

   if ($debug) {print "BType_info = xxx $BType_info xxx; balloon_type = xxx $balloon_type xxx\n";}

   my $balloon_info = $balloonMftr." / ".$balloon_type;
   $header->setLine(7,"Balloon Manufacturer/Type:", $balloon_info);


   #------------------------------------
   # Translate Balloon Weight code
   #------------------------------------
   if ($debug) {print "\nDetermine Balloon Weight\n";}
   my $balloonWeight = "N/A";

   if ( exists($headerRecVal{weightOfBalloon}))  # The Key exists and has value == True
      {
      $balloonWeight = $headerRecVal{weightOfBalloon};
      $balloonWeight =~ s/^\s+//; # trim white space

      if ($debug) {print "Exists:: balloonWeight = $balloonWeight\n";}
      if ( $balloonWeight eq "null") 
         {
         $balloonWeight = "N/A";
         }
      }
   else
      {
      print "WARNING(2): Unknown balloonWeight! balloonWeight = xxx $balloonWeight xxx\n";
      $balloonWeight = "N/A";
      }

   if ($debug) {print "balloonWeight = xxx $balloonWeight xxx\n";}

   $header->setLine(8,"Balloon Weight:", $balloonWeight);
   if ($debug) {print "balloon Weight = $balloonWeight\n";}



   #------------------------------------
   # Set radiosondeAscensionNumber
   #------------------------------------
   if ($debug) {print "\nDetermine AscensionNumber\n";}
   my $ascensionNum = "N/A";

   if (exists($headerRecVal{radiosondeAscensionNumber}))
      {
      $ascensionNum = $headerRecVal{radiosondeAscensionNumber};
      if ($debug) {print "Exists: ascensionNum = $ascensionNum\n";}
      }
   else
      {
      $ascensionNum = "N/A";
      print "WARNING: Missing AscensionNumber\n";
      }

   $header->setLine(5,"Ascension Number:", $ascensionNum);
   if ($debug) {print "ascensionNum = $ascensionNum\n\n";}


   # --------------------------------------------------------------------
   # Set Ground Station s/w, Radiosonde Freq, Serial Number, if not null.
   # add 2 slash lines to hdr
   # --------------------------------------------------------------------
   if (exists($headerRecVal{radiosondeSerialNumber}))
      {
      my $radiosondeSerialNumber = trim (eval($headerRecVal{radiosondeSerialNumber}) );
      $header->setLine(6,"Radiosonde Serial Number:", $radiosondeSerialNumber);
      if ($debug) {print "Output Line 6:: Radiosonde Serial Number: $radiosondeSerialNumber\n";}
      }
   else
      {
      $header->setLine(6,"Radiosonde Serial Number:", "Unknown");
      if ($debug) {print "Output Line 6:: Radiosonde Serial Number: Unknown\n";}
      }

   if (exists($headerRecVal{softwareVersionNumber}))
     {
     my $groundStationSoftware = trim (eval($headerRecVal{softwareVersionNumber}) );
     $header->setLine(10,"Ground Station Software:", $groundStationSoftware);
     if ($debug) {print "Output Line 8:: groundStationSoftware: $groundStationSoftware\n";}
     }
   else
     {
     $header->setLine(10,"Ground Station Software:", "Unknown");
     if ($debug) {print "Output Line 8:: groundStationSoftware: Unknown\n";}
     }
    

   #-------------------------------------------------
   # As of 23 March 2022, assoc. sci. says to drop 
   # the Radiosonde Frequency. Not enough room in the
   # CLASS/ESC file header. Do NOT set line in 
   # Header for radio frequency for GTS BUFR sondes. 
   #
   # WAS: Originally for processing non-NWS data here's
   # the code to set the header lines:
   #   $header->setLine(8,"Radiosonde Frequency:", $radiosondeFrequency);
   #   $header->setLine(8,"Radiosonde Frequency:", "Unknown");
   #
   #-------------------------------------------------
   if (exists($headerRecVal{radiosondeOperatingFrequency}) )
      {
      my $radiosondeFrequency = trim (eval($headerRecVal{radiosondeOperatingFrequency} ) );
      if ($debug) {print "\nDROPPED - No Room in HDR:: Output Line 9:: Radiosonde Frequency: $radiosondeFrequency\n";}
      }
   else
      {
      if ($debug) {print "Output Line 9:: Radiosonde Frequency: Unknown\n";}
      }


   #-------------------------------------------------------------------
   # As of 23 March 2022, assoc. sci. has added several other BUFR
   # elements to output header so do NOT add 2 lines with slashes.
   # Keep code for reference. 
   #
   #    $header->setLine(9,"/", " ");
   #    $header->setLine(10,"/", " ");
   #    if ($debug) {print "Output Lines 10 and 11:: as slashes.\n";}
   #-------------------------------------------------------------------
   
   # -----------------------------------------------------
   # Set Date/Time in line 12 of the header. This is the 
   # Nominal Release Time (not Actual Release Time).
   # Note that date/time must be in a specific format.
   # -----------------------------------------------------
   # Set Nominal Date
   # ------------------
   my $date_Year = trim($headerRecVal{typicalYear});
   my $date_Month = trim($headerRecVal{typicalMonth});
   my $date_Day = trim($headerRecVal{typicalDay});
   my $date = sprintf("%04d, %02d, %02d", $date_Year, $date_Month, $date_Day);
   if ($debug) {print "\nNominal date: $date; $date_Year $date_Month $date_Day \n";}

   # ----------------
   # Set Nominal Time
   # ----------------
   my $time_Hour = trim($headerRecVal{typicalHour});
   my $time_Minute = trim($headerRecVal{typicalMinute}); 
   my $time_Second = trim($headerRecVal{typicalSecond});
   my $time = sprintf("%02d:%02d:%02d", $time_Hour, $time_Minute, $time_Second);
   if ($debug) {print "Nominal time: $time, $time_Hour $time_Minute $time_Second \n\n";}

   $header->setNominalRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);

   # -------------------------------
   # Set actual date/time for header
   # The actual time should also be
   # put in the output file name.
   # -------------------------------
   $date_Year = trim($headerRecVal{year});
   $date_Month = trim($headerRecVal{month});
   $date_Day = trim($headerRecVal{day});
   $date = sprintf("%04d, %02d, %02d", $date_Year, $date_Month, $date_Day);
   if ($debug) {print "Actual date: $date; $date_Year $date_Month $date_Day \n";}   

   $time_Hour = trim($headerRecVal{hour});
   $time_Minute = trim($headerRecVal{minute});
   $time_Second = trim($headerRecVal{second});
   $time = sprintf("%02d:%02d:%02d", $time_Hour, $time_Minute, $time_Second);
   if ($debug) {print "Actual time: $time, $time_Hour $time_Minute $time_Second \n";}

   $header->setActualRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);

   #-----------------------------------------------------
   # Example of how to add coln headers:
   # $header->setVariableParameter(2, "MixR","g/kg");
   #-----------------------------------------------------

   return $header;
   } # parseHeader()
                           
##------------------------------------------------------------------------------
# @signature void parseRawFiles(String file)
# <p>Read the data from the specified file and convert it to the ESC format.</p>
#  Note: In some cases, have found First occurance of timePeriod has null values 
#  and t=0 for sfc. Then second occurance of timePeriod also values but also 
#  has t=0 for sfc and no displacement.  All lines before timePeriod's first 
#  occurance are header lines. 
#  
# @input $file The name of the file to be parsed.
##------------------------------------------------------------------------------
sub parseRawFile 
   {
   my ($self,$file) = @_;

   if ($debug) {print "Enter parseRawFile: file = $file\n";}

   #--------------------------------------------------------------------------------
   # Defined incoming header/data elements. HARDCODED
   #
   # All known possible header element types (in order as seen in initial dataset).
   # Note that some of these elements can be and are duplicated in the GTS headers.
   #
   # * The blockNumber added to beginning of stationNumber forms the WMO ID.
   # * The typical"date/time" forms the nominal date/time.
   # * Other "date/time" forms the actual date/time.
   # * Sci Staff are unsure whether use height or heightOfStationGroundAboveMeanSeaLevel.
   # * Info on Clouds may be used in hdr. Sci Staff unsure on this.
   #
   #--------------------------------------------------------------------------------
   # Define the hash tables for used and not used elements/keys
   #----------------------------------------------------
   # These header elements are recognized AND are used.
   # Migrated several elements from notUsed to Used sections. 
   #----------------------------------------------------
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
     "balloonManufacturer" => 1,
     "balloonType" => 1,
     "weightOfBalloon" => 1,
     );

   # ---------------------------------------------------------------------------
   # NWS "New" header element types start at amountOfGasUsedInBalloon below and
   # "New" footer element types start after operator type in that section below.
   # Migrated several elements from notUsed to Used sections. 
   # ---------------------------------------------------------------------------
   my %headerElements_notUsed = (
     "bufrHeaderCentre" => 1,
     "bufrHeaderSubCentre" => 1,
     "compressedData" => 1,
     "dataCategory" => 1,
     "dataSubCategory" => 1,
     "internationalDataSubCategory" => 1,
     "localTablesVersionNumber" => 1,
     "masterTablesVersionNumber" => 1,
     "numberOfSubsets" => 1,
     "observedData" => 1,
     "unexpandedDescriptors" => 1,
     "subsetNumber" => 1,
     "timeSignificance" => 1,
     "heightOfBarometerAboveMeanSeaLevel" => 1,
     "stationElevationQualityMarkForMobileStations" => 1,
     "verticalSignificanceSurfaceObservations" => 1,
     "oceanographicWaterTemperature" => 1,
     "extendedDelayedDescriptorReplicationFactor" => 1,
     "amountOfGasUsedInBalloon" => 1,
     "balloonFlightTrainLength" => 1,
     "balloonShelterType" => 1,
     "edition" => 1,
     "masterTableNumber" => 1,
     "observerIdentification" => 1,
     "radiosondeCompleteness" => 1,
     "radiosondeConfiguration" => 1,
     "radiosondeGroundReceivingSystem" => 1,
     "radiosondeReleaseNumber" => 1,
     "radome" => 1,
     "reasonForTermination" => 1,
     "typeOfGasUsedInBalloon" => 1,
     "typicalDate" => 1,
     "typicalTime" => 1,
     "updateSequenceNumber" => 1,
     );

   my %footerElements_Used = (
     correctionAlgorithmsForHumidityMeasurements => 1,
     geopotentialHeightCalculation => 1,
     humiditySensorType => 1,
     pressureSensorType => 1,
     radiosondeAscensionNumber => 1,
     radiosondeOperatingFrequency => 1,
     radiosondeSerialNumber => 1,
     softwareVersionNumber => 1,
     temperatureSensorType => 1,
    );

   my %footerElements_notUsed = (
     absoluteWindShearIn1KmLayerAbove => 1,
     absoluteWindShearIn1KmLayerBelow => 1,
     delayedDescriptorReplicationFactor  => 1,
     operator => 1,
     meanWindDirectionFor1500To3000M => 1,
     meanWindDirectionForSurfaceTo1500M => 1,
     meanWindSpeedFor1500To3000M => 1,
     meanWindSpeedForSurfaceTo1500M => 1,
     modifiedShowalterStabilityIndex => 1,
     text => 1,
    );

  # --------------------------------------------------------------
  # Note that PIBALS have geopotentialHeight in every time period
  # but "regular" soundings have nonCoordinateGeopotentialHeight.
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
     );

  # WAS Following commented out in RELAMPAGO processing
  my %dataElements_notUsed = (
     extendedVerticalSoundingSignificance => 1,
     );

  if ($debug2)
     {
     print "headerElements_Used Hash::\n";
     foreach my $key (keys %headerElements_Used)
       {
       print "$key = $headerElements_Used{$key}\n";
       }
     } #debug2

   printf("\n----------\nProcessing file: %s\n",$file);

   open(my $FILE,$self->{"RAW_DIR"}."/".$file) or die("Can't open file: ".$file);

   my @lines = <$FILE>;
   close($FILE);

   # ---------------------------------------------
   # Go through all records in input file and 
   # divide into Header, Footer, and data lines,
   # then call routines to parse each type.
   # Note that Footer recs are really header
   # recs that are not at the top of the file.
   # So, put footer recs into headerlines array.
   # ---------------------------------------------
   my %headerRecVal; 

   my @headerlines; 
   my @datalines; 

   my $totalRecs_all = 0; #Total count of recs all kinds used and not used

   my $ih = -1; my $total_ih = 0;   # Total count includes hdr and used footer recs
   my $id = -1; my $total_id = 0;
   my $total_if = 0;   # Total count is footer recs only

   # -----------------------------------
   # Start out assuming this is a PIBAL.
   # -----------------------------------
   my $pibal = 1; # Determine if sounding is a PIBAL. See where reset to 0, if find T or P not missing.
   my $Use_nonCoordGeoPotHt = 0; # Only Regular Soundings have nonCorrdGeoPotHt values. PIBALS do not.

   my $prev_timePeriod = -99;

   # **********************************
   #-----------------------------------
   # Loop through all the input lines.
   #-----------------------------------
   # **********************************
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
     # This is the FIRST LOOP THROUGH THE INPUT DATA. The code first
     # splits the data input the header and data types and then 
     # processes those arrays next. Since we are going through the
     # data, the data is simplied to be just the input_key and the
     # value for the parm.   Combo the header and footer info into
     # the header array.
     #
     # HERE is where CAPTURE and PROCESS ANY ADDITIONAL FLAGS.
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

     $value =~  s/}+$//g; # Strip off ending brace found on header recs.
     $value =  trim ($value); # Strip off spaces. 
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


        # ------------------------------------------------------------------
        # See if time periods are in increasing order. If not, issue warning.
        # Beware Null time period value. Reset to odd missing value so 
        # obvious in output.
        # ------------------------------------------------------------------
        if ($input_key eq "timePeriod")
          {

          if ($debug2) {print "1. input_key = $input_key  , value = $value\n";}

          if ( ($value ne "null" ) && ($value ne "Null") )
             {
             if ($value < $prev_timePeriod)
                { 
                print "WARNING: ParseRawFile( $file) timePeriods out of order! prev_timePeriod = $prev_timePeriod, current timePeriod = $value\n"; 
                }

             if ($value == $prev_timePeriod)
                {
                if ($debug) {print "WARNING: ParseRawFile( $file) DUPLICATE timePeriods found (in time order)! prev_timePeriod = $prev_timePeriod, current timePeriod = $value\n";}
                }
             } 
          else  # timePeriod is NULL
             {
             if ($debug2) {print "2. RESET Null time to -999.9 ,  input_key = $input_key  , value = $value\n";}


             print "     ERROR: ParseRawFile( $file)  Null timePeriod Value! input_key = $input_key, value = $value\n"; 
             $value = -999.9;  

             print "WARNING: ParseRawFile( $file) Resetting NULL time to -999.9 ,  input_key = $input_key  , value = $value , prev_timePeriod = $prev_timePeriod\n";
             }

          $prev_timePeriod = $value;

          } # timePeriod sort order check

        #-----------------------------------------------------------------------------
        # Determine if this is a PIBAL. Pibals do not have pressures or temperatures.
        # PIBALs also do NOT have the temp correction parameter but they do have the
        # geopotential height parm. Sci staff does NOT expect that PIBALs have the
        # noncoordinated geopotential height.   
        #
        # If find the geopotential height in any rec, then assume this is a PIBAL
        # and only use the geopot ht in the output. If find the nonCoordinateGeopotentialHeight,
        # assume this is a regular sounding and only use the noncoordinated
        # height throughout the sounding output.  If find both in a sounding, this is
        # likely an ERROR so issue a warning for science staff to review.                  
        #
        # This is important for properly handling the first time = 0.0 sec record.
        # Pibals are handled differently and must keep the 0.0 rec. Not so for other
        # soundings. We drop the 0.0 sec record for other soundings (non-Pibals), if
        # the 0.0 sec record does not have pressure or temperature. Search for PIBAL.
        #-----------------------------------------------------------------------------
        # If ever find the presence of Temp, P, or nonCoordHt parms (even if null value),
        # then assume this is a regular sounding and NOT a PIBAL.  We assume that only
        # PIBALS have the geopotentialHeight parmeter. Regular soundings only have the
        # nonCoordinateGeopotentialHeight parmeter....per science staff. Statements below
        # may cause repeated resetting of pibal parm = OK for now to help determine if
        # these checks are what is needed. 
        #-------------------------------------------------------------------------------
        if (($input_key eq "airTemperature")||($input_key eq "pressure")
             || ($input_key eq "nonCoordinateGeopotentialHeight") )
           {
           if ($debug) {print "This is a Regular Sounding. NOT a PIBAL. Found either AirTemp, Press, or nonCoordGeoPotHt parm record!\n";}
           $pibal = 0;   # This resets the default of PIBAL.

           # Use nonCoordGeoPotHt for Altitude of Regular Soundings.
           $Use_nonCoordGeoPotHt = 1;
           }

        if ($input_key eq "geopotentialHeight")
           {
           # PIBALs have geoPotHts. Default is PIBAL unless reset in prev check.
           if ($debug) {print "This is a PIBAL. Found geopotentialHeight parm record! pibal = $pibal. Use_nonCoordGeoPotHt = $Use_nonCoordGeoPotHt\n";}
           }

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
        print "WARNING: ParseRawFile( $file ) UNKNOWN input element type! input_key is unknown for input Line:$line\n";
        }

      if ($debug2) {print "ih (hdr+ftrUsed) = $ih,  id (data) = $id \n";}
      if ($debug2) {print "total_ih (hdr+(used)ftr) = $total_ih,  total_id (data) = $total_id, total_if (ftr only) = $total_if\n";}

   } # foreach on key type/line in raw data file

  if ($pibal == 1) 
    { print "ParseRawFile( $file) Found a PIBAL. The airTemp, Press, and nonCoordgeopotHt were NOT found or were all Null in this sounding.\n"}
  else
    { print "ParseRawFile( $file) Found Regular Sounding. This is NOT a PIBAL. Found at least one Temp, Press, or nonCoordGeoPotHt.\n"}


  if ($debug) {print "Header/Footer recs: @headerlines\n\n"; print "Data recs: @datalines\n\n"; print "\n\n Found: Hdr Recs= $total_ih, Ftr Recs= $total_if, Data Recs= $total_id\n"; print "Total Recs All Types (includes blank): $totalRecs_all\n\n";}

  if ($debug)
     {
     print "parseRawFile(): HASH headerRecVal(3)::\n";
     foreach my $key (keys %headerRecVal)
       { print "(2) $key = $headerRecVal{$key}\n"; }
     } #debug


   # -----------------------------
   # -----------------------------
   # Generate the sounding header.
   # -----------------------------
   # -----------------------------
   if ($debug) {print "Generate Sounding Header. Call parseHeader().\n";}
   my $header = $self->parseHeader($file, %headerRecVal);
   if ($debug) {print "Returned from parseHeader().\n";}
    
   # ----------------------------------------------------------
   # Only continue processing the file if a header was created.
   # ----------------------------------------------------------
   if (defined($header)) 
      {
      if ($debug) {print "Header Created. Conti Processing.\n";}
      # -----------------------------------------------------
      # Determine the station the sounding was released from
      # and keep a list of all stations found for station
      # output file. 
      # -----------------------------------------------------
      my $station = $self->{"stations"}->getStation($header->getId(),$self->{"NETWORK"});

      if (!defined($station)) 
         {
         $station = $self->buildDefaultStation($header->getId(),$self->{"NETWORK"});
         $self->{"stations"}->addStation($station);
         }

      $station->insertDate($header->getNominalDate(),"YYYY, MM, DD");

      # -----------------------------------------------------
      # Create the output file name and open the output file.
      # Beware that output file names must contain site name
      # to be unique. Also some incoming files contained more
      # than one sounding so preprocessor splits those into
      # multiple sounding files with "_#.cls" for suffix.
      #
      # Have confirmed that we always use the Actual Time
      # in the output file name.
      #
      # Put "PIBAL" or "SONDE" in output file name so can
      # tell which is which in output. Some sites will have
      # both types.
      #
      # Sample Input File Name:
      #   SOCRATES 2018 E.G.: 201710220000.lord_howe_island_au_sounding_1.preproc
      #    (Note that the SOCRATES data was not NWS.)
      #
      #   CFACT 2022 E.G.: KSLC_202202221200_ius_sounding_1.preproc
      #    (Note that the CFACT data is NWS only.)
      #
      # HERE is the section on forming output file name. Note
      # that the NWS input file names are very different from
      # the previous input files this code was applied to. 
      #
      # WAS:my @nameSplit2 = split (/,/,$nameSplit1[0]);
      # -----------------------------------------------------
      my $outfile;
      my ($hour, $min, $sec) = split (/:/, $header->getActualTime());

      my @filename_parts = split /\./, $file;  # SOCRATES 2018 E.G.: 201710220000.lord_howe_island_au_sounding_1.preproc
                                               # NWS Input File Name CFACT 2022 E.G.: KSLC_202202221200_ius_sounding_1.preproc
                                              
      my @filename_subparts = split /\_/, $filename_parts[0]; # SOCRATES: lord, howe, island, au, sounding, 1 (Use filename_parts[0] for SOCRATES.
                                                              # NWS CFACT: KSLC, 202202221200, ius, sounding, 1

      if ($debug) {print "\nfilename_subparts xxx @filename_subparts xxx\n";} 

      my $fileArrayLength = scalar (@filename_subparts);
      my $fileSuffixCt = $filename_subparts[$fileArrayLength-1];

      if ($debug) {print "fileSuffixCt = $fileSuffixCt\n";}

      my @nameSplit1 = split (/\//,$header->getSite());

      my @nameSplit2 = split (/ /,$nameSplit1[0]);

      if ($debug) {print "nameSplit1 = @nameSplit1\n"; print "nameSplit2 = @nameSplit2\n";}

      my $NS = $nameSplit2[0]; 
      $NS  =~ tr/ //ds; # remove all white space

      if ($pibal)
         {
         $outfile = sprintf("GTS_BUFR_%s_%04d%02d%02d%02d%02d_%02d_PIBAL.cls",
                      $NS,
                      split(/,/,$header->getActualDate()),
                      $hour, $min, $fileSuffixCt);
         }
      else
         {
         $outfile = sprintf("GTS_BUFR_%s_%04d%02d%02d%02d%02d_%02d_SONDE.cls",
                      $NS,
                      split(/,/,$header->getActualDate()),
                      $hour, $min, $fileSuffixCt);
         }

      printf("Input file name: %s ; Output file name is %s\n", $file, $outfile);

      open(my $OUT,">".$self->{"OUTPUT_DIR"}."/".$outfile)
        or die("Can't open output file for $file\n");

      # ------------------------------------
      # Write sounding header to output file
      # ------------------------------------
      print($OUT $header->toString());

      if ($debug) {print "\n--------- End Header. Skip Hdr Lines. Begin Data ------------\n";}
   
      #######################################################################
      # ----------------------------------------
      # Needed for code to derive ascension rate
      # ----------------------------------------
      # Initial Altitude will be the surface 
      # altitude from header.  
      # ----------------------------------------
      my $prev_time = 9999;                      # HARDCODED initialization
      my $prev_alt = $header->getAltitude();
      my $prev_uwind = -999.9;
      my $prev_vwind = -999.9;

      # ----------------------------------------
      # Parse the data portion of the input file
      # ----------------------------------------
      my $DataRecCt = 0;

      # ---------------------------------------------------
      # Save off the header reference lat/lon as data recs
      # have lat/lon displacement values only. Must calc
      # actual lat/lons for each data rec.
      # ---------------------------------------------------
      my $headerLat = $headerRecVal{latitude};
      my $headerLon = $headerRecVal{longitude};

      if ($debug) {print "\nheaderLat = $headerLat,headerLon = $headerLon\n";}

      # --------------------------------------------------------------------------------------
      # Note that the data records are one element/parameter per input lines. 
      # The data for a time period is in a set of lines separated by the "timePeriod"
      # record/parameter.  Must collect data until hit the next "timePeriod" then write
      # out the previous set of collected data.  Software is writing out the PREVIOUSLY
      # collected timePeriod because rec types can come in any order and only know
      # that all of a timePeriod's recs have been collected when s/w hits the next
      # timePeriod record.
      # --------------------------------------------------------------------------------------
      # For the sample of data files provided for SOCRATES 2018, the set appears to have 
      # 10 elements/parms and to start with the "timePeriod" rec and end with 
      # the "windSpeed" rec BUT we can not assume that in the future that the set of data
      # recs will be in that order. Right now, we ASSUME that they start with the "timePeriod"
      # rec. It's possible that data recs could have more or less elements/parameters included. 
      # ---------------------------------------------------------------------------------------
      # -----------------------------------------------------------
      # Process each line. Expect one parameter and value per line.
      # -----------------------------------------------------------

      # Set to write first rec to output
      my $writePrevRecToOutput = 1;

      # Create the first DATA record
      my $recordOut = ClassRecord->new($WARN,$file);

      # Initialize the current time
      my $current_time = -999.9;

      if ($debug) {print "\n******Begin process only datalines array.*****\n";}
      foreach my $line (@datalines)   # Only process datalines in this loop.
         {         
         if ($debug) {print "\nLOOP TOP: Process DATA line: xxx $line xxx\n";}

         # --------------------------------------
         # Skip any blank lines. Shouldn't be any
         # blank lines, since they were removed
         # during preprocessing. 
         # --------------------------------------
         next if ($line =~ /^\s*$/);

         # -------------------------------------------
         # Form of all expected DATA input lines:
         #     input_key , value
         # -------------------------------------------
         my @record = split (/,/, $line);
         my $input_key = trim ($record[0]);
         my $value = trim ($record[1]);

         if ($debug) {print "DATA Input Key: xxx $input_key xxx, value: xxx $value xxx\n";}
         if (($debug) && ($input_key eq "timePeriod") ) {print "\n-------- DATA - Next timePeriod Found------- value = $value --\n";}

         #---------------------------------------------------
         # Next check of timePeriod has null value may/should
         # never happen, but this check is to make sure.
         # If Null then reset to odd missing value that will
         # be obvious in the output.
         # --------------------------------------------------
         if (($input_key eq "timePeriod") && ($value eq "null" )) 
            {
            print "     ERROR: ParseRawFile( $file)  Null timePeriod Value! input_key = $input_key, value = $value, prev_time = $prev_time\n"; 
            print "WARNING: ParseRawFile( $file) Resetting NULL time to -999.9 ,  input_key = $input_key  , value = $value , prev_time = $prev_time\n";

            $value = -999.9    # Reset time to odd missing value
            }

         #-----------------------------------------------------------------------
         # Found next set of DATA records to process and then write to output.
         # In SOCRATES 2018 data only 10 diff DATA recs found and handled by this
         # software. WARNINGS are issued for any other type of rec. 
         #-----------------------------------------------------------------------
         if ($input_key eq "timePeriod")  # Each data rec set starts with "timePeriod". ASSUMPTION. 
            {
            # -----------------------------------------------------------------------
            # If not first data rec found, then write previous time period's
            # saved values to output file. Only write out a zero time period record 
            # if this is a PIBAL. PIBALs do not have airTemp, Press but do have
            # geopotentialht values. They do not have nonCoordinateGeopotentialHeight
            # parameters.   ASSUMPTION.
            # -----------------------------------------------------------------------
            $current_time = $value;

            if ($DataRecCt >0)   # Not first record, so not first timePeriod.
               {      
               if ($debug) {print "Not first timePeriod. WRITE Previous timePeriod data to OUTPUT and start next timePeriods data collection.\n";}

               #-----------------------------------------
               # Calc all non provided values for previous
               # timePeriod. At this point, should have all
               # possible values for prev timePeriod.
               #-----------------------------------------
               #--------------------
               # Calc Ascension Rate
               #--------------------
               my $getTime = $recordOut->getTime();
               my $getAlt = $recordOut->getAltitude();

               if ($debug) {print "\nFor Prev Rec -- Try to Calc AscRate:  getTime= $getTime, prev_time= $prev_time,  getAlt= $getAlt, prev_alt = $prev_alt\n";}

               if ($prev_time != 9999   && $getTime != 9999   &&
                   $prev_time != -999.9 && $getTime != -999.9 &&
                   $prev_alt  != 99999  && $getAlt  != 99999  &&
                   $prev_time != $getTime )
                  {

                  $recordOut->setAscensionRate( ($getAlt  - $prev_alt) / ($getTime - $prev_time),"m/s");

                  if ($debug) 
                      { 
                      my $AscRate = (($getAlt - $prev_alt) / ($getTime - $prev_time));
                      print "Calc Ascension Rate. AscensionRate = $AscRate\n"; 
                      }
                  }
               else
                  {
                  if ($debug) {print "WARNING: ParseRawFile( $file) Can not calc Ascension Rate at data record = $DataRecCt current_time= $current_time. prev_time= $prev_time, prev_alt= $prev_alt, getTime = $getTime, getAlt= $getAlt\n";} 
                  }

               #------------
               # Calc RH
               #------------
               my $t   = $recordOut->getTemperature();
               my $dpt = $recordOut->getDewPoint();

               if (($t != 999.0) && ($dpt != 999.0))
                  {
                  my $RH = calculateRelativeHumidity($t, $dpt);
                  $recordOut->setRelativeHumidity($RH);
                  }

               #--------------------------
               # Calc U, V Wind Components
               #--------------------------
               my $wspd = $recordOut->getWindSpeed();
               my $wdir = $recordOut->getWindDirection();
               my $uwind = 9999.0;
               my $vwind = 9999.0;
               
               if (($wspd != 999.0) && ($wdir != 999.0))
                  {
                  ($uwind, $vwind) =  calculateUVfromWind($wspd, $wdir);
                  $recordOut->setUWindComponent($uwind,"m/s");
                  $recordOut->setVWindComponent($vwind,"m/s");

                  if ( ($prev_uwind != 9999.0) && ($prev_uwind != 9999.0) )
                     {
                     if ( (($prev_uwind - $uwind) > 1.0) || (($prev_uwind - $uwind) > 1.0) )
                        {
                        printf "WARNING: ParseRawFile( %s) U V Wind components have change GREATER than 1.0! prev_uwind= %5.2f, uwind= %5.2f, prev_vwind= %5.2f, vwind= %5.2f at current_time= %4d, prev_time= %4d\n", 
                                $file,$prev_uwind, $uwind, $prev_vwind, $vwind, $getTime, $prev_time;
                        }
                     } # prev U V wind components not missing

                  } # Wind Speed & Wind Dir are not missing


               #--------------------------------------
               #**************************************
               # Write the previous rec to output file.
               #**************************************
               #--------------------------------------
               # ------------------------------------------------------------------------------------------------ 
               # Only output recs with time of 0.0 seconds if a PIBAL, but not for regular soundings.
               # PIBALS are a special case and do not have Temp or Press. Per Science Staff: Do not include 
               # any zero sec recs that do not have temp and press for regular soundings. 
               #
               # Note that in SOCRATES GTS BUFR data seems fairly common that 
               # there are 2 recs with time of 0.0 second. Drop the first "junk" one or multiple first junk recs.
               #
               # IMPORTANT/ASSUMPTIONS/Notes:
               # Remember that this code is writing the PREVIOUSLY collect timePeriod's data to the output and then
               # it starts collecting the data for the current timePeriod. In general,this code will write out 
               # all records, even the ones Out of Time Order. A warning would have been issued earlier in the code 
               # if a time was out of time order. Science staff requests that we then sort data (as for 
               # NWS soundings) after the data have been converted to ESC format.  This will cause duplicate records 
               # to be sorted next to each other and recs to be in increasing time sort order. This code will
               # NOT find duplicate time records that are out of time order in the input *.preproc file. Must
               # sort *.cls output to find those duplicate time records. 
               #
               # Set flag to write or not that will be checked at beginning of next pass.
               #
               # Consistently use one or the other of the geopotential heights (geo or nonCoord) in the output.
               # Do not switch between the two.
               # ------------------------------------------------------------------------------------------------ 
               my $press = $recordOut->getPressure;

               if ($debug) {print "Is timePeriod rec. Not first Rec. Check Time if zero. timePeriod = $current_time. PIBAL = $pibal\n";}

               # ---------------------------------------------------------------------------------------
               # Not first rec but timePeriod=0 so this is likely second rec or dup of t=0. In SOCRATES
               # data have seen dup t=0 recs but first rec in regular soundings had missing T, P and so
               # sci staff says to drop.  This code allows multiple zero sec recs and will output all
               # t=0 recs that have non-null T and P.
               # ---------------------------------------------------------------------------------------
               if ($current_time == 0.0) 
                  {
                  if ($debug) {print "Not first rec. TimePeriod is zero. timePeriod = $current_time. PIBAL = $pibal\n";}

                  if ($pibal == 1) # Found PIBAL. Output all PIBAL recs.
                     {
                     if ($debug) {print "TimePeriod is zero. Write prev rec of PIBAL to output.  PIBAL = $pibal\n";}
                     if ($writePrevRecToOutput) {printf($OUT $recordOut->toString())}; # Write to output file
                     }
                  else
                     {
                     if ($debug) {print "NOT PIBAL. Temp ($t) and Press($press). Write to output if P, T non-missing.\n";}

                     if (($t !=999.0) && ($press !=999.0))   # This also indicates this is regular sounding type. Not Pibal.
                        {
                        if ($debug) {print "Temp ($t) and Press($press) are NOT missing. Write Regular Sounding to output if true to writePrevRecToOutput = $writePrevRecToOutput\n";}

                        if ($writePrevRecToOutput) {printf($OUT $recordOut->toString())}; # Write previous rec to output
                        }
                     else
                        {
                        if ($debug) {print "Time is $current_time. Regular Sounding BUT either T or P is Missing/Null. Do NOT write to output. \n";}
                        }
                     } # Not PIBAL
                  } # timePeriod equals 0.0 but not first rec

               else # timePeriod ne 0.0 and not first rec
                  {
                  if ($debug) {print "Is timePeriod rec. Not first rec. timePeriod ne 0.0. twice-Previous Time = $prev_time; Current Time = $current_time. Output previous rec as is.  writePrevRecToOutput = $writePrevRecToOutput\n";}

                  if ($writePrevRecToOutput)   # All recs currently being written to output.
                     {
                     if ($debug) {print "Writing previous rec to output file for twice-prev_time = $prev_time; DataRecCt = $DataRecCt.\n";}

                     printf($OUT $recordOut->toString()); # Write to output file
                     }
                  else
                     {
                     print "WARNING: ParseRawFile( $file ) DROP REC: NOT writing prev rec to output at record = $DataRecCt. twice-prev_time = $prev_time (Dropping Rec between these two times.) Current Time= $current_time\n";
                     }                  

                  } # timePeriod ne 0.0 and not first rec

               #-------------------------------
               # Save off parms for next pass.
               #-------------------------------
               if ($debug) {print "Prior to Reset for next Loop Pass:  twice-prev_time = $prev_time, prev_alt= $prev_alt, prev_uwind= $prev_uwind, prev_vwind= $prev_vwind.\n";}

               $prev_time = $recordOut->getTime();     # Get time from "prev rec" just written out. Not current_time which is for next set of data.
               $prev_alt = $recordOut->getAltitude(); 

               $prev_uwind = $recordOut->getUWindComponent();
               $prev_vwind = $recordOut->getVWindComponent();

               if ($debug) {print "Reset Previous Time,Alt,Uwind,Vwind for current rec comparisons.  prev_time = $prev_time, prev_alt= $prev_alt, prev_uwind= $prev_uwind, prev_vwind= $prev_vwind.\n";}

               #####################################################
               #----------------------------------------------------
               # Start a next output Class sounding record
               # Prep for this new set of data elements.
               #----------------------------------------------------
               #####################################################
               $recordOut = ClassRecord->new($WARN,$file);  

               #-------------------------------------------------------------
               # TimePeriods should always be increasing, but may find dup or 
               # out of order recs at same time or at t=0.0. Have seen last rec
               # out of time order when there is already rec at same time. 
               # Output all records. Do post processing sort as for NWS processing
               # and sci staff will remove any out of place records.
               #
               # If find timePeriod that is out of time order then issue a warning 
               # but do write to output in received order.
               # --------------------------------------------------------------
               # Prep for writing current rec out on next pass through loop.
               # Just wrote out previous rec. Consider if should write out
               # the current on the next pass through loop with these 2 checks.
               # --------------------------------------------------------------
               # --------------------------------------------------------------
               # Save new time. We are in a timePeriod record.
               # --------------------------------------------------------------
               if ($current_time  ne "null") 
                  { 
                  if ($debug) {print "Not first record. Save off time. writePrevRecToOutput = $writePrevRecToOutput, DataRecCt = $DataRecCt.\n"; }

                  $recordOut->setTime($value);
                  $writePrevRecToOutput = 1;
                  } 
               else
                  { 
                  print "WARNING:  ParseRawFile ( $file) Time is Null at record = $DataRecCt. Setting time to -999.\n"; 
                  $recordOut->setTime(-999); 
                  $writePrevRecToOutput = 1; 
                  }

               # Time must be increasing else issue warning but print to output
               if ($debug) {print "Check for Out of Time Order rec. current time = $current_time,  prev_time= $prev_time\n";}

               if ($current_time < $prev_time)
                  {
                  print "WARNING: ParseRawFile( $file) Found Out of Time Order record. Write CURRENT/All recs to output.  current time = $current_time < prev_time= $prev_time\n";
                  $writePrevRecToOutput = 1;
                  }

               if ($current_time == $prev_time)
                  {
                  print "WARNING: ParseRawFile( $file) DUPLICATE timePeriods found (in time order)! prev_time = $prev_time, current timePeriod = $current_time\n";
                  }

               $DataRecCt++;
               if ($debug) {print "NOT first rec: Increment count of timePeriod recs found. DataRecCt = $DataRecCt \n";}

               } # NOT first Data Rec  *********************************************************


            else # Check on if found First Data Rec
               {
               #----------------------------------------------------------
               # Found first DATA record. This should be a timePeriod rec.
               #----------------------------------------------------------
               if ($debug) {print "Found FIRST data rec to process. DataRecCt = $DataRecCt.\n";}

               if ($input_key ne "timePeriod") 
                  {
                  print "     ERROR: ParseRawFile( $file )  FIRST DATA rec is NOT a timePeriod but should be! Exiting 3! input_key = $input_key , current_time = $current_time\n"; 
                  exit(1);
                  }

               if ($value ne "null") # timePeriod can not be null/missing
                  { 
                  $recordOut->setTime($current_time); 
                  $writePrevRecToOutput = 1; # Generally write out the first rec if it passes test above.

                  if ($debug) {print "Good (non-null) first rec found. DataRecCt = $DataRecCt. current_time = $current_time, writePrevRecToOutput = $writePrevRecToOutput.\n";}

                  } # ASSUMPTION: First data rec is timePeriod rec.
               else
                  { 
                  print "     ERROR: ParseRawFile( $file )  First record has timePeriod of Null. Expect timePeriod value >=0.0. Exiting 4!  current_time = $current_time\n";
                  exit(1); 
                  }

               $DataRecCt++;
               if ($debug) {print "End first rec found. DataRecCt = $DataRecCt\n";}
             
               } #First data rec


            } # timePeriod element found

         elsif ($input_key eq "windSpeed")
               {
               if ($value ne "null")  # zero wind dir is legit so check for null/missing
                  {
                  if ($debug) {print "Set wind speed = $value\n";}
                  $recordOut->setWindSpeed($value,"m/s");
                  }
               else
                  {
                  print "WARNING: ParseRawFile( $file) Wind Speed is Null! current_time = $current_time \n";
                  $recordOut->setWindSpeed(999.0,"m");   # *
                  }

               } # windSpeed

         elsif ($input_key eq "windDirection")
               {
               if ($value ne "null")  # zero wind dir is legit
                  {
                  if ($debug) {print "Set wind direction = $value\n";}
                  $recordOut->setWindDirection($value);
                  }
               else
                  {
                  print "WARNING: ParseRawFile( $file) Wind Dir is Null! current_time = $current_time \n";
                  $recordOut->setWindDirection(999.0);   #*
                  }

               } # windDirection

         elsif ($input_key eq "latitudeDisplacement")
               {
               #------------------------------------------------------------------------
               # Set Latitude 
               #------------------------------------------------------------------------
               # Must work in all hemispheres. All locations. Consider Latitude vs
               # equator rollover (0.0) and pole rollover (+/-90). For Longitude, 
               # consider International Dateline rollover (+/-180 vs 360). Can not
               # predict form of all data's lat/lons so issue warnings.  The lat/lon
               # displacements are always displacements from the origin/header lat/lon.
               #
               # Example displacement versus initial lat/lon
               # Initial Lat/Lon:
               #   { "key" : "latitude", "value" : -66.2827, "units" : "deg" },
               #   { "key" : "longitude", "value" : 110.523, "units" : "deg" },
               #
               # Displacement:
               # { "key" : "latitudeDisplacement", "value" : -0.00029, "units" : "deg" },
               # { "key" : "longitudeDisplacement", "value" : 0.00053, "units" : "deg" },
               #------------------------------------------------------------------------
               if ($value ne "null")  # zero lat is legit
                  {
                  my $lat = $value+ $headerLat; 
                  my $alat = abs($lat); 

                  if ($debug) {print "Set latitude = $lat, abs(lat) = $alat))\n";}

                  if ($alat > 90.0) 
                     { print "WARNING: ParseRawFile( $file) lat + latitudeDisplacement > +/-90 ( $lat )! Output As Is! current_time = $current_time \n"; }

                  $recordOut->setLatitude($lat, $self->buildLatLonFormat($lat));
                  } 
               else
                  {
                  print "WARNING: ParseRawFile( $file) Latitude is Null! current_time = $current_time\n";
                  my $lat = 999.0;
                  $recordOut->setLatitude($lat, $self->buildLatLonFormat($lat));
                  }

               } # latitudeDisplacement

         elsif ($input_key eq "longitudeDisplacement")
               {
               if ($value ne "null")   # zero lon is legit
                  {
                  #--------------
                  # Set Longitude 
                  #--------------
                  my $lon = $value+ $headerLon; 
                  my $alon=abs($lon);

                  if ($debug) {print "Set longitude = $lon, abs(lon) = $alon\n";}

                  if (($alon > 180.0) || ($alon > 360.0))
                     {print "WARNING: ParseRawFile( $file) lon + longitudeDisplacement > +/-180 OR +/-360 ( $lon )! Output As Is! current_time = $current_time\n"; }

                  $recordOut->setLongitude($lon, $self->buildLatLonFormat($lon));
                  }
               else
                  {
                  print "WARNING: ParseRawFile( $file) Longitude is Null! current_time = $current_time\n";
                  my $lon = 9999.0;
                  $recordOut->setLongitude($lon, $self->buildLatLonFormat($lon));
                  }

               } # longitudeDisplacement

         elsif ($input_key eq "nonCoordinateGeopotentialHeight")
               {
               # --------------------------------------------------------------------------
               # From the AMS Glossary for Geopotential Height ("gravity-adjusted height"):
               # --------------------------------------------------------------------------
               # The height of a given point in the atmosphere in units proportional 
               # to the potential energy of unit mass (geopotential) at this height 
               # relative to sea level.  The relation, in SI units, between the 
               # geopotential height Z and the geometric height z is 
               #     Z = (1/g0)* Intergral from zero to z of (gdz prime)
               # where g is the acceleration of gravity and g0 is the globally 
               # averaged acceleration of gravity at sea level (g0 = 9.806 65 m s−2), 
               # so that the **two heights are numerically interchangeable** for most 
               # meteorological purposes. Also, one geopotential meter is equal to 
               # 0.98 dynamic meter.  See dynamic height. From link at
               # http://glossary.ametsoc.org/wiki/Geopotential_height .
               # Geometric height = elevation above mean sea level.
               # --------------------------------------------------------------------------
               # A zero or negative geopotential ht is possible so these must be allowed.
               # Science staff says to leave "as is" for now and if significant issues
               # appear they will provide instructions on how to handle.
               #
               # Science staff says use "geopotentialHeight" for PIBALS and 
               # use nonCoordinateGeopotentialHeight for regular soundings.
               # Code set to keep whichever first type of geo pot ht is encounter
               # and stay with that consistently throughout the sounding output. 
               #
               # Do Not Mix geopotHt and nonCoordGeoPotHt in a single sounding.
               # --------------------------------------------------------------------------
               if ($value ne "null")   #ASSUMPTION: Geopot height can be negative or zero.
                  {
                  if ($debug) {print "Regular Sounding: Setting Altitude using nonCoordGeopotHt = $value.\n";}
                  if ($debug) {print "WARNING: ParseRawFile( $file) Regular Sounding: NonCoordGeoPotHt used in Altitude. current_time = $current_time\n";}

                  $recordOut->setAltitude($value,"m");
                  }
               else
                  { 
                  #----------------------------------------------------------------------------------------
                  # Extra Checks: 
                  # If Altitude already set by geopotentialHeight or dup nonCoordGeoPotHt, issue a warning.
                  # Should be a dup in a timePeriod set nor a combo of the two different Ht types.
                  #----------------------------------------------------------------------------------------
                  if ($recordOut->getAltitude() < 99999)
                     { if ($debug) {print "WARNING: ParseRawFile( $file )  Altitude previously set by either a duplicate nonCoord geoPot ht OR geopotentialHeight OR input nonCoordGeoPotHt is Null.  Set output Altitude as missing. current_time = $current_time\n";} }
                  else
                     { if ($debug) {print "Altitude NOT previously set with geopotentialHeight and nonCoord is null!\n";} }

                  #-------------
                  # Set Altitude
                  #-------------
                  if ($debug) {print "Regular Sounding: nonCoordGeopotHt = Null. Set Altitude to missing.\n";}
                  $recordOut->setAltitude(99999,"m");  # not needed
                  }
               } # Altitude/Height

         elsif ($input_key eq "geopotentialHeight")
               { 
               #-------------------------------------------------------
               # Code assumes that only PIBALS have geopotentialHeight.
               #-------------------------------------------------------
               if ($pibal)
                  { if ($debug) {print "This is a PIBAL and we are using the geopotentialHeight. pibal = $pibal\n";} }

               if ($Use_nonCoordGeoPotHt) # Prev set as Regular sounding!
                  { print "WARNING: ParseRawFile( $file) Sounding has geoPotHt and nonCoordGeoPotHt. NonCoordGeoPotHt used in Altitude. SKIP GeoPotHt rec. pibal = $pibal ,  current_time = $current_time \n"; }

               else
                  {
                  # ----------------------------------------------
                  # PIBAL so use geoPotHt. Do not mix the two diff
                  # geoPotHts and nonCoordGeoPotHts.
                  # ----------------------------------------------
                  if ($value ne "null")   #ASSUMPTION: Allow geoPot to be negative and zero
                     {
                     $recordOut->setAltitude($value,"m");

                     if ($debug) {print "Setting Altitude using geopotentialHeight!  Altitude = $value\n"; }
                     }
                  else
                     {
                     if ($debug) {print "WARNING ParseRawFile( $file )  geopotentialHeight is Null! Set Alt to missing. current_time = $current_time \n";}
                     $recordOut->setAltitude(99999,"m");
                     }

                  } # Have already set the Altitude using the nonCoordGeoPot height so DO NOT reset with geoPotHt
               } # geopotentialHeight

         elsif ($input_key eq "pressure")
            {
            if (($value ne "null") && ($value != 0.0))
               {
               $value = $value/100.0; # convert Pa to mb

               if ($debug) {print "Set pressure = $value\n";}
               $recordOut->setPressure($value,"mb");
               }
            else
               { 
               print "WARNING: ParseRawFile( $file) Pressure is zero or Null! current_time = $current_time \n";
               $recordOut->setPressure(9999,"mb"); 
               } 
            } # pressure

         elsif ($input_key eq "airTemperature")
            {
            if (($value ne "null") && ($value != 0.0))
               {
               #absolute zero (0 K) is equivalent to −273.15 °C (−459.67 °F), so zero not expected.
               $value = $value-273.15;

               if ($debug) {print "Set Temp = $value\n";}
               $recordOut->setTemperature($value,"C");
               } # Kelvin to deg C
            else
               { 
               print "WARNING: ParseRawFile( $file) airTemperature is zero Kelvin or NULL! current_time = $current_time\n";
               $recordOut->setTemperature(999,"C");  #*
               } 
            } #airTemperature

         elsif ($input_key eq "dewpointTemperature")
            {
            if (($value ne "null") && ($value != 0.0))  #ASSUMPTION: Temps are in Kelvin. Absolute value not expected.
               {
               $value = $value-273.15;

               if ($debug) {print "Set dew point = $value\n";}
               $recordOut->setDewPoint($value,"C"); #*
               } # dewpoint for RH calcs
            else
               { 
               print "WARNING: ParseRawFile( $file) dewpointTemperature is Null or zero Kelvin! current_time = $current_time\n";
               $recordOut->setDewPoint(999,"C");
               } 
            } #dewpointTemperature

         elsif ( $input_key eq "extendedVerticalSoundingSignificance")
            {
            # -----------------------------------------------------------------
            # WAS: skip this type of rec, but now handle it.
            # -----------------------------------------------------------------
            # if ($debug) { print "Skipping extendedVerticalSoundingSignificance type rec. Record Ignored.\n";}
            # -----------------------------------------------------------------
            # If Value of this parm is 65536, then this is a mandatory rec set.
            # Determine if user wants these records in output or not.
            # -----------------------------------------------------------------
            if ($debug) { print "Found extendedVerticalSoundingSignificance type rec. Value = $value\n";}

            if ($value == 65536)
              {
              if ($debug) {print "Key is extendedVerticalSoundingSignificance. Value ($value) is 65536! FOUND Mandatory Level Rec Set! KeepEVSSrecs = $KeepEVSSrecs .\n"; }

              if (!$KeepEVSSrecs) # Not keeping mandatory level recs
                 {
                 if (1) {print "Key is extendedVerticalSoundingSignificance. Value is 65536! DROP THIS Mandatory Level Rec! Reset writePrevRecToOuput to zero. DROP REC! current_time = $current_time\n"; }

                 $writePrevRecToOutput = 0;  # Do Not write this mandatory level rec to the output!  DROP REC!
                 }
              }
            else
              {
              if ($debug) {print "Key is extendedVerticalSoundingSignificance. Value ($value) is NOT 65536! NOT a Mandatory Level Rec Set! KEEP REC!\n"; }
              } # value check


            } # extendedVerticalSoundingSignificance

         else
            {
            print "WARNING: ParseRawFile( $file)  UNKNOWN DATA Record type = $input_key  current_time = $current_time \n";
            } # unknown - issue warning!

         } # end foreach $line

      #----------------------------------------------------------
      # Write out last record found at end of file.
      #-----------------------------------------------------------
      # Always (in code above) writing out previous record info to 
      # output if meets criteria. Here print out final record if 
      # meets criteria.  If it is the First and Only record 
      # (regardless if T & P are missing) in this file then write 
      # it out. Checks in code above should handle this. 
      #--------------------------------------------
      if ($debug) {print "FINAL/LAST rec in file found. Write to output. writePrevRecToOutput = $writePrevRecToOutput\n";}

      if ($writePrevRecToOutput) {printf($OUT $recordOut->toString())}; # Write to output file

      } # If header defined

   } # parseRawFile()

##------------------------------------------------------------------------------
# @signature void printStationFiles()
# <p>Generate the output files containing station information.</p>
##------------------------------------------------------------------------------
sub printStationFiles 
   {
   my ($self) = @_;

   open(my $STN, ">".$self->{"STATION_FILE"}) || die("Cannot create the ".$self->{"STATION_FILE"}." file\n");

   foreach my $station ($self->{"stations"}->getAllStations()) 
      { print($STN $station->toString()) if ($station->getBeginDate !~ /^9+$/); }

   close($STN);

   } # printStationFiles()

##------------------------------------------------------------------------------
# @signature void readDataFiles()
# <p>Read in the files from the raw data directory and convert each into an
# ESC formatted file.</p>
##------------------------------------------------------------------------------
sub readDataFiles 
   {
   my ($self) = @_;
    
   opendir(my $RAW,$self->{"RAW_DIR"}) or die("Can't read raw directory ".$self->{"RAW_DIR"});
   my @files = grep(/.preproc$/,sort(readdir($RAW)));   # HARDCODED - process only *.preproc files
   closedir($RAW);

   if ($debug) {print "Input Files to Process: @files\n";}
    
   open($WARN,">".$self->{"WARN_LOG"}) or die("Can't create ".$self->{"WARN_LOG"});


   #--------------------------------------------------------------------------
   # Read in the station/site info for all NWS sites and put into global hash. 
   # HERE is where processing site info into site_info hash. 
   #--------------------------------------------------------------------------
   if ($debug) {printf("Call readSiteInfo to fill site_info hash.\n");} 
   $self->readSiteInfo;

  if ($debug2)
     {
     print "(Print 1) site_info Hash::\n";
     foreach my $key (keys %site_info)
       {
       print "$key = $site_info{$key}\n";
       }
     } #debug


   #----------------------------------------------------
   # Process all raw data files in specified directory.
   #----------------------------------------------------
   if ($debug) {printf("Raw Dir read. Now call parseRawFile() to process each input file.\n");}

   foreach my $file (@files) 
      { 
      $self->parseRawFile($file); 
      }
   
   close($WARN);
   } # readDataFiles()


##-------------------------------------------------------------------------
## @signature void readSiteInfo()
## <p>Read in the file with the site information. This is HARDCODED with an 
## expected input file name. Note that the site info file MUST be located
## in the same dir as the raw data??????</p>
##  $site_info     Input File Name: NWS_GTS_BUFR_station.lst
##-------------------------------------------------------------------------
sub readSiteInfo
   {
   my ($self) = @_;

   # ---------------------------------------------------------------------------
   # Open station Info file, read contents and fill hash table with station information.
   # This information is used to fill station line in the header section. 
   # There should NOT be a header line in this file! The name of the station info 
   # file is HARDCODED at the top of this software file.  Note that a copy of 
   # the station info file MUST be in the RAW_DIR where all the raw data are located. 
   #
   # Id;WBAN;WMO;Name;    - NOT INCLUDED IN ACTUAL INPUT FILE.
   # KABR;14929;72659;Aberdeen, SD;
   #
   # ---------------------------------------------------------------------------
   printf("\n----------\nProcessing INPUT Station Info file: %s\n",$Input_Station_Info_File);

   open(my $FILE,$self->{"STNINFO_DIR"}."/".$Input_Station_Info_File) or die("Can't open file: ".$Input_Station_Info_File);

   if ($debug) {print "Input Station Information File to Process: $Input_Station_Info_File\n";}

   my @lines = <$FILE>;
   close($FILE);

   my $stn_lines=0;

   foreach my $line (@lines)
     {
     chomp ($line);
     if ($debug) {print "processing Station Info Line: xxx $line xxx\n";}

     $stn_lines++;

     my @record = split (/;/, $line);
     if ($debug2) {print "Split record/line: xxx @record xxx\n";}

     my $input_key = $record[0];  # KABR
     if ($debug2) {print "Input Key: xxx $input_key xxx\n";} 

     my $townState = $record[3];  # Aberdeen, SD
     if ($debug2) {print "townState: xxx $townState xxx\n";}

     # Put info into site_info hash where ID is the key to the site name info.
     $site_info{$input_key} = $townState;

     if ($debug) {print "HASH site_info(1): $input_key = $site_info{$input_key}\n";}
     }

  if ($debug) {print "Total Station Info Lines read: xxx $stn_lines xxx\n";}


  if ($debug2)
     {
     print "(Print 2) site_info Hash::\n";
     foreach my $key (keys %site_info)
       {
       print "$key = $site_info{$key}\n";
       }
     } #debug


   if ($debug) {printf("Stn Info File read and site_info in global hash. \n");}

   } # readSiteInfo()


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
