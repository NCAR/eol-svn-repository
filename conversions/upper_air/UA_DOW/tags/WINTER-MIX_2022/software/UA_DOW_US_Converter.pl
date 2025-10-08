#! /usr/bin/perl -w

##Module---------------------------------------------------------------------
# <p>The UA_DOW_US_Converter.pl script is used for converting WINTRE-MIX
# University of Albany (SUNY) US DOW sounding data from its raw
# ASCII Text format into the EOL Sounding Composite (ESC) format.</p>
#
# INPUT: UA_DOW_US raw data in the following format/order. See sample 
#   raw data line below. Here are the expected parameters in order along with
#   definitions.
#
#  1. UTC_Date: The Month/Day/Year at which the sounding was launched in UTC.
#  2. UTC_Time: The time at which the sounding was launched in UTC. HH:MM:SS
#  3. FltTime: The elapsed time in SECONDS since the sounding was launched
#  4. Ascent: The ascent rate of the sounding in METERS per MINUTE
#  5. GPM_AGL: The geopotential METERS of the sounding above ground level
#  6. GPM_MSL: The geopotential METERS above mean sea level
#  7. Alt_AGL: The altitude of the sounding in METERS above ground level
#  8. Alt_MSL: The altitude of the sounding in METERS above mean sea level
#  9. Press: The pressure measured by the radiosonde in hPa
#  10. Temp: The temperature measured by the radiosonde in degrees CELSIUS
#  11. RelHum: The relative humidity measured by the radiosonde in %
#  12. Mix_Rat: The mixing ratio measured by the radiosonde in GRAMS per kg 
#  13. DP: The dew point measured by the radiosonde in degrees CELSIUS
#  14. WSpeed: The wind speed measured by the radiosonde in METERS per SECOND
#  15. WDirn: The wind direction measured by the radiosonde in DEGREES (0 degrees
#      corresponds to a northerly wind).
#  16. Long/E: The longitudinal position of the sounding e.g., 073 31'05.7"W .
#  17. Lat/N: The latitudinal position of the sounding e.g., 44 41'08.8"N .
#
#   Raw data line: Note that the spaces/tabs shown in the next line are not accurate.
#  3/7/2022    10:01:59 PM            0.0              0            0.0          103.0
#  0.0          103.0         985.90          +2.00         100.00            4.5     
#  +2.0            0.7            290  073 31'05.7"W   44 41'08.8"N
#
# OUTPUT: Sounding data files in ESC format.
#
# Assumptions/Warnings:
#      - Search for "HARDCODED" to change project related constants.
#
# WARNING: 
#      1. Times have AM and PM in UTC time. Sample Date/Time: "3/7/2022  6:21:08 PM". 
#
#      2. The lat/lon/elev in the output header is set to be the same as the lat/lon/elev
#      on the first data record with time equal 0.0 seconds. Confirm True for UA DOW US.
#
# @author Linda Cully  31 October 2022
# @version WINTRE-MIX (2022)
#
##Module---------------------------------------------------------------------
package UA_DOW_US;
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
use ElevatedStationMap;
use Station;
use ClassConstants qw(:DEFAULT);
use ClassHeader;
use ClassRecord;
use DpgCalculations;
use DpgConversions;
use DpgDate qw(:DEFAULT);

my ($WARN);

my $debug = 0;

printf "\nUA_DOW_US_Converter.pl began on ";print scalar localtime;printf "\n";
&main();
printf "\n\nUA_DOW_US_Converter.pl ended on ";print scalar localtime;printf "\n";


##---------------------------------------------------------------------------
# @signature void main()
#
# <p>Execute the script to convert the data.</p>
##---------------------------------------------------------------------------
sub main 
   {
   my $converter = UA_DOW_US->new();
   $converter->convert();

   } # end main()

##---------------------------------------------------------------------------
# @signature void convert()
#
# <p>Convert the raw data into the CLASS format.</p>
##---------------------------------------------------------------------------
sub convert {
   my $self = shift;

   mkdir($self->{"OUTPUT_DIR"}) unless (-e $self->{"OUTPUT_DIR"});
   mkdir("../final") unless (-e "../final");

   open($WARN,">".$self->{"WARN_LOG"}) or die("Cannot open warning file.\n");

   $self->readRawDataFiles();
   $self->printStationFiles();

   close($WARN);

   } # convert()

##---------------------------------------------------------------------------
# @signature EOL_Dropsonde_Converter new()
#
# <p>Create a new converter object.</p>
#
# @output $self The new converter.
##---------------------------------------------------------------------------
sub new {
   my $invocant = shift;
   my $self = {};
   my $class = $invocant || ref($invocant);
   bless($self,$class);

   # ----------------------------------
   # HARDCODED project specific values
   # ----------------------------------
   $self->{"PROJECT"} = "WINTRE-MIX_2022"; 
   $self->{"NETWORK"} = "UA DOW US";
   
   $self->{"FINAL_DIR"} = "../final";
   $self->{"OUTPUT_DIR"} = "../output_esc";
   $self->{"RAW_DIR"} = "../raw_data";
  
   $self->{"STATION_FILE"} = sprintf("%s/%s_%s_sounding_stationCD.out",$self->{"FINAL_DIR"},
                                     $self->clean_for_file_name($self->{"NETWORK"}),
                                     $self->clean_for_file_name($self->{"PROJECT"}));

   $self->{"WARN_LOG"} = $self->{"OUTPUT_DIR"}."/warning.log";

   $self->{"SUMMARY"} = $self->{"OUTPUT_DIR"}."/station_summary.log";
   $self->{"stations"} = ElevatedStationMap->new();

   return $self;
   } # end new()


##---------------------------------------------------------------------------
# @signature void printSounding()
#
# <p>Generate the output (*.cls) file for the sounding.</p>
##---------------------------------------------------------------------------
sub printSounding 
   {
   if ($debug) {print "Enter printSounding()\n";}


   my ($self,$filename,$header,$records) = @_;

   my $station = $self->{"stations"}->getStation($header->getId(),$self->{"NETWORK"},$header->getLatitude(),$header->getLongitude(),$header->getAltitude());

   if (!defined($station)) 
      {
      $station = Station->new($header->getId(),$self->{"NETWORK"});
      $station->setStationName(sprintf("%s", $header->getSite()));
      $station->setLatitude($header->getLatitude(),$self->buildLatlonFormat($header->getLatitude()));
      $station->setLongitude($header->getLongitude(),$self->buildLatlonFormat($header->getLongitude()));
      $station->setElevation($header->getAltitude(),"m");

      $station->setNetworkIdNumber(99);
      $station->setPlatformIdNumber(99); # Unknown for UA DOW US
      $station->setReportingFrequency("1 second"); 
      $station->setLatLongAccuracy(2);

      $self->{"stations"}->addStation($station);
      }
    
   $station->insertDate($header->getNominalDate(),"YYYY, MM, DD");

   if ($debug) {print "Open output file for writing.\n";}

   open(my $OUT,sprintf(">%s/%s",$self->{"OUTPUT_DIR"},$filename)) or die("Can't write to $filename\n");

   # ----------------------------------------------------------------------
   # Write out the Header portion of the sounding to the *.cls output file.
   # ----------------------------------------------------------------------
   if ($debug) {print "Write Header to output file.\n";}
   print($OUT $header->toString());
   
   # ----------------------------------------------------
   # Write out the data records to the output *.cls file.
   # ----------------------------------------------------
   if ($debug) {print "Write Data Records to output file.\n";}
   foreach my $record (@{$records}) 
      {
      print($OUT $record->toString());
      }

   close($OUT);

   if ($debug) {print "Exit printSounding()\n";}

   } # end printSounding()

##---------------------------------------------------------------------------
# @signature void printStationFiles()
#
# <p>Generate the stationCD.out file and the station summary log for the stations
# in the conversion.
#
# NOTE: The station list output files contains one line for each station found
#       and processed in the input data. In the past, these station list output
#       files were required to be put into the data archive along with the data.
#       This is no longer required so (for now and although the code runs) any
#       code in this file that is created to generate the station list output
#       file runs but is ignored. 
# </p>
##---------------------------------------------------------------------------
sub printStationFiles 
   {
   my $self = shift;
   my ($STN, $SUMMARY);

   open($STN, ">".$self->{"STATION_FILE"}) || 
   die("Cannot create the ".$self->{"STATION_FILE"}." file\n");

   foreach my $station ($self->{"stations"}->getAllStations()) 
      {
      print($STN $station->toString()) if ($station->getBeginDate !~ /^9+$/);
      }

   close($STN);

   open($SUMMARY, ">".$self->{"SUMMARY"}) ||    die("Cannot create the ".$self->{"SUMMARY"}." file.\n");

   print($SUMMARY $self->{"stations"}->getStationSummary());

   close($SUMMARY);
   } # end printStationFiles()

##---------------------------------------------------------------------------
# @signature void readRawDataFiles()
#
# <p>Read in a list of all raw data files and process them one at a time.</p>
##---------------------------------------------------------------------------
sub readRawDataFiles 
   {
   my $self = shift;

   opendir(my $RAW,$self->{"RAW_DIR"}) or die("Can't open raw data directory\n");

   # ----------------------------------------------------------------------------------------
   # Expected Input File names are of the form: 
   #    "upperair.sounding.202202030100.Albany_DOW-US_Plattsburgh.txt" or 
   #    "upperair.sounding.202202101030.Albany_DOW-US_N.txt" or
   #    "upperair.sounding.202202250900.Albany-ESSX.txt" 
   #
   #    so generic form is:
   #            "upperair.sounding.YYYYMMDDhhmm.Albany*.txt"
   # ----------------------------------------------------------------------------------------
   my @files = grep(/.txt$/,readdir($RAW)); # Only process Albany data
   closedir($RAW);

   if ($debug) {print("Raw Files: @files\n\n\n");}
    
   #---------------------------------------------------
   # Open and read the contents of each data input file.
   #---------------------------------------------------
   foreach my $file (@files) 
      {
      printf("\nPROCESSING: %s\n",$file);

      open(my $FILE,sprintf("%s/%s",$self->{"RAW_DIR"},$file)) or die("Can't open file: $file\n"); 
   
      $self->readRawFile($FILE, $file);  

      close($FILE);
      } # foreach raw data file

   } # end readRawDataFiles()

##---------------------------------------------------------------------------------
# @signature void readRawFile(FileHandle FILE)
#
# <p>Read the data in the file handle, fill in header info, fill in data elements
# from raw data into output record, and call routine to print it to an 
# output (*.cls) file.</p>
#
# Form of input file name: "upperair.sounding.YYYYMMDDhhmm.Albany*.txt"
#
# @input $FILE The file handle holding the raw data and name of input file.
##---------------------------------------------------------------------------------
sub readRawFile {

   my ($self,$FILE,$filename) = @_;

   my ($header,$records,$windUnits);

   my $file_prefix = "UA_DOW_US_";  # prefix to output file names

   if ($debug) {print "\n\n-----------------\nEnter readRawFile(): Processing filename: $filename\n\n";}

   # ----------------------------------
   # ----------------------------------
   # Set up sounding header information
   # ----------------------------------
   # ----------------------------------

   #-------------------------------------------------
   # Create the header element to fill with metadata. 
   #-------------------------------------------------
   $header = ClassHeader->new($WARN);

   # ----------------------------------
   # HARDCODED project specific values
   # ----------------------------------
   $header->setType("UA DOW US Sounding Data ");
   $header->setReleaseDirection(" Ascending");
   $header->setProject($self->{"PROJECT"});

   $header->setLine(5,"Radiosonde Frequency:", "403MHz"); 
   $header->setLine(6,"Radiosonde Type/RH Sensor Type:", "iMet-4 radiosonde/thin-film capacitive polymer");

   if ($debug) {print "(1)filename = xxx $filename xxx\n";}

   my @filename_parts = split(/\./, $filename);

   if ($debug) {print "(2)filename = $filename, filename_parts = xxx @filename_parts xxx $filename_parts[0] \n\n";}

   # ------------------------------------------------------------------------------------------
   # Readme for 612.001 says that different iMet system used for different IOPs::
   #
   # "Soundings conducted by the research team from the University at Albany were performed with
   # the iMet-3050A sounding system for IOPs 1-4 and the iMet-3150 sounding system for IOPs 6-11,
   # with no soundings collected for IOP5 due to the failure of iMet-3050A sounding system
   #
   # 1. IOPs 1-4 ran from 2100 UTC 2 Feb through 1000 UTC 18 Feb 2022  (202202022100 - 202202181000)
   # 2. No soundings from UA DOW US for IOP5 so gap in this data collection.
   # 3. IOPs 6-11 ran from 0900 UTC 25 Feb through 0700 UTC 15 Mar 2022 (202202250900 - 202203150700)
   # ------------------------------------------------------------------------------------------
   if ($debug) {print "Determine which IOP: xxx $filename_parts[2] xxx 2022 02 02 2100 -> 2022 02 18 1000) = IOPs 1-4 \n";}
   
   if (($filename_parts[2] > 202202022000) && ($filename_parts[2] < 202202182359) ) #Catch times close but possibly outside exact IOP range. 
      {
      if ($debug) {print "In IOPs 1-4 \n";}
      $header->setLine(7,"Ground Station Software:", "iMet-3050A iMetOS-II software version 3.133.0C"); 
      }
   else # Assume collected in IOPs 6-11 (2022 02 25 0900 - 2022 03 15 0700)
      {
      if ($debug) {print "In IOPs 6-11 \n";}
      $header->setLine(7,"Ground Station Software:", "iMet-3150 iMetOS-II software version 3.133.0C"); 
      }

   # -----------------------------------------------------------------
   #  University at Albany 3 Sites:
   #     DOW-US-N (Champlain, NY): 44.9554328, -73.3878575, elev: 46 m
   #     DOW-US-Plattsburgh: 44.684823, -73.526291, elev: 109 m
   #     ESSX (Essex Farm): 44.308028, -73.374444, elev: 67 m
   #
   #  Set the lat, lon and elev in the header to that as specified in
   #  the readme document for dataset 612.001. 
   # -----------------------------------------------------------------
   my $site_name = $filename_parts[3];

   my $hdrLat = 0.0;  # decimal deg
   my $hdrLon = 0.0; # decimal deg
   my $hdrElev = 0.0; # meters

   if ($site_name eq "Albany_DOW-US_Plattsburgh") # HARDCODED
      {
      $site_name = "Plattsburgh, NY"; 

      $hdrLat = 44.684823;  # decimal deg
      $hdrLon =  -73.526291; # decimal deg
      $hdrElev = 109; # meters
      }
   elsif ($site_name eq "Albany_DOW-US_N")
      {
      $site_name = "Champlain, NY"; 

      $hdrLat = 44.9554328;  # decimal deg
      $hdrLon = -73.3878575; # decimal deg
      $hdrElev = 46; # meters
      }
   elsif ($site_name eq "Albany-ESSX")
      {
      $site_name = "Essex Farm, NY"; 

      $hdrLat = 44.308028;  # decimal deg
      $hdrLon = -73.374444; # decimal deg
      $hdrElev = 67; # meters
      }
   else
      {
      $site_name = "UNKNOWN Station"; 

      $hdrLat = 99.0;  # decimal deg
      $hdrLon = 999.0; # decimal deg
      $hdrElev = 99.0; # meters
      }

   $header->setLatitude($hdrLat,$self->buildLatlonFormat($hdrLat));
   $header->setLongitude($hdrLon,$self->buildLatlonFormat($hdrLon));
   $header->setAltitude($hdrElev,"m");


   # ---------------------------------------
   # Parse date/time out of input file name.
   # ---------------------------------------
   my $datetime = $filename_parts[2]; # YYYYMMDDhhmm
   my $dateVal = substr($datetime, 0, 8);
   my $timeVal = substr($datetime, 8, 4);

   if ($debug) {print "datetime = xxx $datetime xxx. dateVal = xxx $dateVal xxx. timeVal = xxx $timeVal xxx.\n";} 

   $header->setNominalRelease($dateVal,"YYYYMMDD", $timeVal,"HHMM",0);
   $header->setActualRelease($dateVal,"YYYYMMDD", $timeVal,"HHMM",0);

   # ------------------------------------------------------------------------
   # Three different sites. See lat/lon/elev from readme for dataset 612.001
   # above. 
   # ------------------------------------------------------------------------
   $header->setSite("UA DOW");
   $header->setId($site_name);
   $header->setSite(sprintf("%s %s", $header->getSite(), $header->getId()));

   # ----------------------------------------------------------
   # Output file names: "UA_DOW_US_YYYYMMDDHHMM.cls:"
   # ----------------------------------------------------------
   my $output_filename = sprintf("%s%s_%s.cls", $file_prefix, $filename_parts[3], $filename_parts[2]);
   if ($debug) {print "output_filename = xxx $output_filename xxx\n";}

   my $numline = 0; # Line count within input file

   #-------------------------------------------
   #-------------------------------------------
   # Process the data portion of the input file
   #-------------------------------------------
   #-------------------------------------------
   foreach my $line (<$FILE>) 
      {
      $numline++;
      if ($debug) {print "numline = $numline, Input line:\nxxx $line xxx\n";}
           
      #---------------------------------
      # Split line on spaces. HARDCODED.
      #---------------------------------
      my @data = split(' ',$line);

      #--------------------------------------------------------------
      # Skip the first 3 input head lines in the raw data. HARDCODED.
      #--------------------------------------------------------------
      if ( ($data[0] eq "UTC_Date") || ($data[0] eq "hr:min:s") || 
           ($data[0] eq "---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------") )
         {
         if ($debug) {print "SKIPPING Input line:\nxxx $line xxx\n";}
         next; # Skip to next iteration of this loop through data in input file
         }

      my $record = ClassRecord->new($WARN,$filename);  # Create a new output data record element. Fill with data.

      $record->setTime(trim($data[3]));  # Time from the data record since launch. Seconds since launch. FltTime.

      # ------------------------------------------------------------------
      # Parse the latitude and longitude values to output decimal lat/lon.
      # ------------------------------------------------------------------
      my @lat = split(//, $data[17]); # 44o41'08.8"N - Split into chars
      if ($debug) {print "array of split lat: xxx @lat xxx\n";}

      my $lat_deg = $lat[0].$lat[1]; my $lat_min = $lat[3].$lat[4]; my $lat_sec = $lat[6].$lat[7].$lat[8].$lat[9]; 
      my $lat_NS = $lat[11];

      if ($debug) {print "lat: deg,min,sec,N/S:: xxx $lat_deg xxx xxx $lat_min xxx xxx $lat_sec xxx xxx $lat_NS xxx\n";}

      my $lat_decimal = $lat_deg + $lat_min/60.0 + $lat_sec/3600.0;
      if ($lat_NS eq 'S') {$lat_decimal = -1.0 * $lat_decimal;}

      if ($debug) {print "lat: decimal deg:: xxx $lat_decimal xxx\n";}


      my @lon =  split(//, $data[16]); # 073o31'05.8"W - Split into chars
      if ($debug) {print "array of split lon: xxx @lon xxx\n";}

      my $lon_deg = $lon[0].$lon[1].$lon[2]; my $lon_min = $lon[4].$lon[5]; my $lon_sec = $lon[7].$lon[8].$lon[9].$lon[10]; 
      my $lon_EW = $lon[12];

      my $lon_decimal = $lon_deg + $lon_min/60.0 + $lon_sec/3600.0;
      if ($lon_EW eq 'W') {$lon_decimal = -1.0 * $lon_decimal;}

      if ($debug) {print "lon: deg,min,sec,N/S vs decimal deg:: xxx $lon_deg xxx, xxx $lon_min xxx, xxx $lon_sec xxx, xxx $lon_EW xxx vs xxx $lon_decimal xxx\n";}
      if ($debug) {print "lon: decimal deg:: xxx $lon_decimal xxx\n";}

      $record->setLatitude($lat_decimal,$self->buildLatlonFormat($lat_decimal));
      $record->setLongitude($lon_decimal,$self->buildLatlonFormat($lon_decimal));


      # Missing values for the following parameters are unknown.
      $record->setPressure(trim($data[9]),"mbar") unless (trim($data[9]) == 99999);
      $record->setAltitude(trim($data[6]),"m") unless (trim($data[6]) == 99999);    # GPM_MSL - Per SL

      $record->setTemperature(trim($data[10]),"C") unless (trim($data[10]) == 99999);
      $record->setDewPoint(trim($data[13]),"C") unless (trim($data[13]) == 99999);
      $record->setRelativeHumidity(trim($data[11])) unless (trim($data[11]) == 99999);


      #-------------------------------------------------------------------------------------
      # Set header lat/lon/elev to be the same as the lat/lon/elev on the first data record.
      # OR SHOULD THIS BE THE HARDCODED VALUES FROM THE 612.001 DOCUMENT!!!!!!
      #-------------------------------------------------------------------------------------
#      if ($numline == 4) # Found First Data Line
#         {
#         my $fixedLat = $record->getLatitude();  # decimal deg
#         my $fixedLon = $record->getLongitude(); # decimal deg
#         my $fixedElev = $record->getAltitude(); # meters 
#
#         if ($debug) {print "Set Lat/Lon/Elev to SAME as First data Rec:: lat/lon/elev: $fixedLat $fixedLon $fixedElev\n";}
#
#         $header->setLatitude($fixedLat,$self->buildLatlonFormat($fixedLat));
#         $header->setLongitude($fixedLon,$self->buildLatlonFormat($fixedLon));
#         $header->setAltitude($fixedElev,"m");
#
#         } # First data line - Set Header lat/lon/elev


      #-------------------------------------------------------------------------
      # Set the wind speed, wind direction and calculate the UV wind components.
      # Note that the missing values in the raw data are unknown. 
      #-------------------------------------------------------------------------
      $record->setWindSpeed(trim($data[14]),"m/s") unless (trim($data[14]) == 99999);
      $record->setWindDirection(trim($data[15])) unless (trim($data[15]) == 99999);

      if ( ($data[14] != 99999) && ($data[15] != 99999) )
         {
#         if ($debug) {print "Wind Speed/Wind Direction NOT missing so calcUV at numline = $numline .\n";}

         my ($uwind, $vwind) =  calculateUVfromWind(trim($data[14]), trim($data[15]));  # TRIM OUT THE SPACES

         $record->setUWindComponent($uwind,"m/s");
         $record->setVWindComponent($vwind,"m/s");
         } # If wind speed and wind dir are NOT reset to Missing above, calc UV winds
      else
         {
         if ($debug) {print "Either Wind Speed/Wind Direction ARE missing so DO NOT calcUV at numline = $numline .\n";}
         }

      # --------------------------------------------------------------------
      # Ascension rate is missing on all first data recs for WINTRE-MIX. 
      # Raw data indicates zero but must reset to missing. Not really zero.
      # --------------------------------------------------------------------
      if ($numline == 4)
            {
            print "WARNING: Always RESET Ascension Rate on first data line to MISSING! At numline = $numline .\n";
            $record->setAscensionRate(999.0,"m/s");
            }
      else
            {
            # Convert meters/minute to meters/second
            $record->setAscensionRate( (trim($data[4])/60.0),"m/s"); # Else set to incoming value in raw data
            }

      push(@{$records},$record);  # put this data record into the record array for output

      } # end foreach loop on data records

   #---------------------------------------
   # Write sounding to output (*.cls) file.
   #---------------------------------------
   if ($debug) {print "\t readRawFile(): write sounding to output file.\n";}

   $self->printSounding($output_filename,$header,$records);

   if ($debug) {print "\tExit readRawFile(): Processing FILE: $filename\n";}

   } # end readRawFile()

##------------------------------------------------------------------------------
# @signature String buildLatlonFormat(String value)
# <p>Generate the decimal format for the specified value.</p>
#
# format length must be the same as the value length or
# convertLatLong will complain (see example below)
# base lat = 36.6100006103516 base lon = -97.4899978637695
# Lat format = DDDDDDDDDDDDDDDD  Lon format = -DDDDDDDDDDDDDDDD  
#
# @input $value The value of the lat/lon being formatted.
# @output $fmt The format that corresponds the the value.
##------------------------------------------------------------------------------
sub buildLatlonFormat 
   {
   my ($self,$value) = @_;
    
   my $fmt = $value < 0 ? "-" : "";
   while (length($fmt) < length($value)) { $fmt .= "D"; }
   return $fmt;
   } # end buildLatlonFormat()

##-------------------------------------------------------------------------
# @signature String clean_for_file_name(String text)
# <p>Remove/translate characters in a String so it can be used in a file name.</p>
#
# @input $text The String to be cleaned.
# @output $text The cleaned up String.
##-------------------------------------------------------------------------
sub clean_for_file_name 
   {
   my ($self,$text) = @_;

   # Convert spaces to underscores.
   $text =~ s/\s+/_/g;

   # Remove all hyphens
   $text =~ s/\-//g;

   return $text;
   } # end clean_for_file_name()

##---------------------------------------------------------------------------
# @signature String trim(String line)
# <p>Remove surrounding white space of a String.</p>
# 
# @input $line The String to trim.
# @output $line The trimmed line.
##---------------------------------------------------------------------------
sub trim 
   {
   my $line = shift;
   $line =~ s/^\s+//g;
   $line =~ s/\s+$//g;
   return $line;
   } # end trim()
