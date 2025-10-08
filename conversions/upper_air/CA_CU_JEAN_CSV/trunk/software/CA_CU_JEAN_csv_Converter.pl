#! /usr/bin/perl -w

##Module---------------------------------------------------------------------
# <p>The CA_CU_JEAN_csv_Converter.pl script is used for converting WINTRE-MIX
# University of Colorado (CU) Canadian JEAN CSV formatted sounding data from 
# its raw ASCII Text format into the EOL Sounding Composite (ESC) format.</p>
#
# INPUT: CA_CU_JEAN_csv raw data in the following format/order. See sample 
#   raw data line below. Here are the expected parameters in order along with
#   definitions. Note that there only 1 Canadian CU JEAN site to process and
#   this is the csv format (not the txt format). The txt format is totally 
#   different and the processing done separately from this csv format processing. 
#
# Input data at each timestep are indicated by the rows within each CSV file, with the first row of
# data representing the initial conditions when the balloon was launched. The columns within each
# file correspond to the variables output by the Vaisala MW41 sounding software. These variables
# are as follows:
#
# 1.   /Row/@Altitude: The altitude of the radiosonde in meters above ground level - Not Used.
# 2.   /Row/@DataSrvTime: The data server timestamp in UTC - Not Used. 
# 3.   /Row/@Dropping: This column is set to 0 if the data is from an ascending radiosonde - Not Used.
# 4.   /Row/@East: The distance in meters that the radiosonde is to the east of the launch location - Not Used.
# 5.   /Row/@Height: The geopotential height of the radiosonde in meters - USED
# 6.   /Row/@Humidity: The relative humidity measured by the radiosonde in %. - USED
# 7.   /Row/@Latitude: The latitudinal position of the radiosonde  - USED
# 8.   /Row/@Longitude: The longitudinal position of the radiosonde - USED
# 9.   /Row/@North: The distance in meters that the radiosonde is to the north of the launch location - Not Used.
# 10.  /Row/@Pressure: The pressure measured by the radiosonde in hPa - USED
# 11.  /Row/@PtuStatus: PTU status flags for the launch: - Not Used.
#          1 = Pressure Interpolated
#          2 = Height Interpolated
#          4 = Temperature Interpolated
#          8 = Humidity Interpolated
#          16 = Telemetry Break
#          32 = Adiabatic Check Failed
#          64 = Pressure From Height - Interpolated
#
# 12. /Row/@RadioRxTimePk: Radio time in seconds - USED
# 13. /Row/@SoundingIdPk: Randomly generated sounding ID - USED
# 14. /Row/@Temperature: Temperature in Kelvin - USED
# 15. /Row/@Up: Radiosonde vertical distance in meters as measured relative to first time of valid sounding data - Not Used. 
# 16. /Row/@WindDir: Wind direction in degrees (0 degrees corresponds to a northerly wind) - USED
# 17. /Row/@WindEast: The zonal component of the wind in m/s - Not Used.
# 18. /Row/@WindInterpolated: A flag indicating whether the wind has been interpolated by the sounding software
# 19. /Row/@WindNorth: The meridional component of the wind in m/s
# 20. /Row/@WindSpeed: The wind speed in m/s
#
#   Raw data line: Note that the spaces/tabs shown in the next line are not accurate.
#
#   2.000129887,3/12/22 8:02,0,0,32.80930191,99,45.32475,-73.26658,0,1003.95154,0,
#   660.375,57f4cce8-0ea5-4026-9b8a-cc4719137c34,272.65,0,230,1.378879961,FALSE,
#   1.157017667,1.799999952
#
# OUTPUT: Sounding data files in ESC format.
#
# Assumptions/Warnings:
#      - Search for "HARDCODED" to change project related constants.
#      - Search for "SPECIAL CASES" to see changes required for two input files that had
#        different formats. This WINTRE-MIX CA CU DOW data had a total of three different
#        formats included.  Per science staff, the code assumes the frequency for these
#        two files is 1 second. 
#
# WARNING: From the WINTRE-MIX Field Collected Sounding dataset (612.001) readme document regarding
#      the Canadian CU DOW data, " "
#
# @author Linda Cully  2 November 2022
# @version WINTRE-MIX (2022)
#
##Module---------------------------------------------------------------------
package CA_CU_JEAN_csv;
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

my $debug = 1;
my $debug1 = 0;  # Lots of output

printf "\nCA_CU_JEAN_csv_Converter.pl began on ";print scalar localtime;printf "\n";
&main();
printf "\n\nCA_CU_JEAN_csv_Converter.pl ended on ";print scalar localtime;printf "\n";


##---------------------------------------------------------------------------
# @signature void main()
#
# <p>Execute the script to convert the data.</p>
##---------------------------------------------------------------------------
sub main 
   {
   my $converter = CA_CU_JEAN_csv->new();
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
   $self->{"NETWORK"} = "CA_CU_JEAN";
   
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
   if ($debug1) {print "Enter printSounding()\n";}


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
      $station->setPlatformIdNumber(99); # Unknown for CA CU JEAN csv format
      $station->setReportingFrequency("1 Second"); 
      $station->setLatLongAccuracy(2);

      $self->{"stations"}->addStation($station);
      }
    
   $station->insertDate($header->getNominalDate(),"YYYY, MM, DD");

   if ($debug1) {print "Open output file for writing.\n";}

   open(my $OUT,sprintf(">%s/%s",$self->{"OUTPUT_DIR"},$filename)) or die("Can't write to $filename\n");

   # ----------------------------------------------------------------------
   # Write out the Header portion of the sounding to the *.cls output file.
   # ----------------------------------------------------------------------
   if ($debug1) {print "Write Header to output file.\n";}
   print($OUT $header->toString());
   
   # ----------------------------------------------------
   # Write out the data records to the output *.cls file.
   # ----------------------------------------------------
   if ($debug1) {print "Write Data Records to output file.\n";}
   foreach my $record (@{$records}) 
      {
      print($OUT $record->toString());
      }

   close($OUT);

   if ($debug1) {print "Enter printSounding()\n";}

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

   # -------------------------------------------------------------------------------------
   # Expected Input File names are of the form: upperair.sounding.YYYYMMDDhhmm.CU_JEAN.csv
   # -------------------------------------------------------------------------------------
   my @files = grep(/\.csv$/,readdir($RAW)); 
   closedir($RAW);

   if ($debug1) {print "Raw Files: @files\n\n\n";}
    
   #---------------------------------------------------
   # Open and read the contents of each data input file.
   #---------------------------------------------------
   foreach my $file (@files) 
      {
      if ($debug1) {printf("\nPROCESSING: %s\n",$file); }

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
# Form of input file name: "upperair.sounding.YYYYMMDDhhmm.CU_JEAN.csv"
#
# @input $FILE The file handle holding the raw data and name of input file.
##---------------------------------------------------------------------------------
sub readRawFile {

   my ($self,$FILE,$filename) = @_;

   my ($header,$records,$windUnits);

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
   $header->setType("CU Jean Sounding Data");
   $header->setReleaseDirection("Ascending");
   $header->setProject($self->{"PROJECT"});

   $header->setId("CA_CU_JEAN");   # HARDCODED
   $header->setSite("St Jean-sur-Richelieu, QC");   # HARDCODED

   my $lat = 45.324; # HARDCODED Values from 612.001 readme
   my $lon = -73.266;
   my $alt = 37;  

   $header->setLatitude($lat,$self->buildLatlonFormat($lat));
   $header->setLongitude($lon,$self->buildLatlonFormat($lon));
   $header->setAltitude($alt,"m");

   $header->setLine(6,"Radiosonde Frequency:", "1 second");
   $header->setLine(7,"Radiosonde Type/RH Sensor Type:", "Vaisala RS41-SG/Thin Film Capacitor");
   $header->setLine(8,"Ground Station Software:", "Vaisala Sounding Processing System MW41 V2.11");


   #---------------------------------------------------------------------------------------
   # Sample input file name: upperair.sounding.202203120000.CU_JEAN.csv
   #
   # One site to process for the WINTRE-MIX 2022 project:
   #   CU JEAN - "St Jean-sur-Richelieu, QC" at 45.324880, - 73.266880 (elev: 37 m)
   #---------------------------------------------------------------------------------------
   if ($debug1) {print "(1)filename = xxx $filename xxx\n";}

   my @filename_parts = split(/\./, $filename); # upperair sounding 202203072200 CU_JEAN csv - Nominal time of sounding
   my $file_prefix = $filename_parts[3];  # prefix to output file names

   if ($debug1) {print "(2)filename = $filename, filename_parts = xxx @filename_parts xxx $filename_parts[0] xxx\n\n";}

   # -----------------------------------------------
   # Parse Nominal date/time out of input file name.
   # -----------------------------------------------
   my $Filedatetime = $filename_parts[2];     # YYYYMMDDhhmm - Nominal Date and Time from file name.
   my $dateVal = substr($Filedatetime, 0, 8); # YYYYMMDD   
   my $timeVal = substr($Filedatetime, 8, 4); # hhmm         

   if ($debug1) {print "Filedatetime = xxx $Filedatetime xxx. dateVal = xxx $dateVal xxx. timeVal = xxx $timeVal xxx.\n";} 

   $header->setNominalRelease($dateVal,"YYYYMMDD", $timeVal,"HHMM",0);

   $header->setActualRelease($dateVal,"YYYYMMDD", $timeVal,"HHMM",0); # Initialize with Nominal Time from file name. Reset Below.

   # ----------------------------------------------------------
   # Output file names: "CU_JEAN_csv_YYYYMMDDHHMM.cls:" 
   # ----------------------------------------------------------
   my $output_filename = sprintf("%s_%s.cls", "CU_JEAN_csv", $filename_parts[2]);
   if ($debug) {print "output_filename = xxx $output_filename xxx\n";}

   my $numline = 0;              # Line count within input file
   my $prev_RadioRxTimePk = 0.0; # Use to calc yyy
   my $prev_alt = 0;             # Use to calc ascension rate

   my $RadioRxTimePk = -1.0; # Current time on data line

   #-------------------------------------------
   #-------------------------------------------
   # Process the data portion of the input file
   #-------------------------------------------
   #-------------------------------------------
   foreach my $line (<$FILE>) 
      {
      $numline++;
      if ($debug1) {print "\n-----------------------------------------\nnumline = $numline, Input line:\nxxx $line xxx\n";}
           
      #---------------------------------
      # Split line on spaces. HARDCODED.
      #---------------------------------
      my @data = split(',',$line);

      if ($debug1) {print "data[0] = $data[0]; data Split Array:: xxx @data xxx\n ";}

      #--------------------------------------------------------------
      # Skip the first input header line in the raw data. HARDCODED.
      #--------------------------------------------------------------
      my $loc = index($data[0], "Row");  # Generally "Row" occurs many times in header line

      if ( $loc != -1 ) # "Row" not found in line
         {
         if ($debug1) {print "\nSKIPPING HEADER Input line: loc = $loc;\nxxx $line xxx\n";}
         next; # Skip to next iteration of this loop through data in input file
         }
      else
         {
         if ($debug1) {print "FOUND Data to Process:: Input line:\nxxx $line xxx\n";}
         }

      my $record = ClassRecord->new($WARN,$filename);  # Create a new output data record element. Fill with data.

      #---------------------------------------------------------------------------
      # Set the data record time.  Can NOT use the DataSrvTime because is stays
      # the same value for several data records. Must use RadioRxTimePk.
      #
      # Use the difference in the RadioRxTimePk parameter and diff between records
      # to get time that has passed.  RadioRxTimePk has values like:
      # 1400.862 then 1401.862, 1402.862, 1403.862, etc. So this is 1 second data.
      #----------------------------------------------------------------------------
      $RadioRxTimePk = trim($data[11]);

      if ($numline == 2) # First data line is second row in input file
         {
         $prev_RadioRxTimePk = $RadioRxTimePk; # Save off First recs time. Diff with all subsequent times.

         $record->setTime(0.0); # Set time on first data rec to be zero.
         }
      else
         {
         $record->setTime(trim( ($RadioRxTimePk - $prev_RadioRxTimePk) )); # Time passed between records in secs 'tween data recs. 
         }

      # --------------------------------------------------------------------------
      # Parse the latitude and longitude values to output decimal lat/lon.
      # --------------------------------------------------------------------------
      my $lat_decimal = 999.0; 
      my $lon_decimal = 9999.0;

      # ---------------------------------------------------
      # Set lat/lon on first rec to match header values. 
      # ---------------------------------------------------
      if ($numline == 2)
         {
         $lat_decimal = $header->getLatitude();
         $lon_decimal = $header->getLongitude();

         if ($debug1) {print "Odd lat: decimal deg:: xxx $lat_decimal xxx  lon: decimal deg:: xxx $lon_decimal xxx\n";}

         $record->setLatitude($lat_decimal,$self->buildLatlonFormat($lat_decimal));
         $record->setLongitude($lon_decimal,$self->buildLatlonFormat($lon_decimal));
         }
      else
         {
         $lat_decimal = $data[6];  # All Northern Hemisphere for WINTRE-MIX so Positive values.
         $lon_decimal = $data[7];  # All Western/Negative for WINTRE-MIX so Negative values.

         if ($debug1) {print "lat: decimal deg:: xxx $lat_decimal xxx  lon: decimal deg:: xxx $lon_decimal xxx\n";}
         }

      $record->setLatitude($lat_decimal,$self->buildLatlonFormat($lat_decimal));
      $record->setLongitude($lon_decimal,$self->buildLatlonFormat($lon_decimal));

      # --------------------------------------------------------------------
      # WARNING: Missing values for the following parameters are unknown so
      # keeping missing check values are 99999 but this may not be correct.
      # --------------------------------------------------------------------
      $record->setPressure(trim($data[9]),"mbar") unless (trim($data[9]) == 99999);

      # ---------------------------------------------------------------------------------------
      # Should we use "Height: The geopotential height of the radiosonde in meters" instead of
      # the "Altitude"? Choice is either "Altitude" above grnd lvl or "Height" Geopotential ht.
      # For other conversions, SL says use GPM_MSL.
      #
      # Using Geopotential Height in field #4.
      # ---------------------------------------------------------------------------------------
      $record->setAltitude(trim($data[4]),"m") unless (trim($data[4]) == 99999); # Geopotential "Height".

      # ----------------------------------------
      # Set Temp and RH. Calc Dew Point Temp.
      # ----------------------------------------
      $record->setRelativeHumidity(trim($data[5])) unless (trim($data[5]) == 99999);
      $record->setTemperature( (trim($data[13])-273.15) ,"C") unless (trim($data[13]) == 99999);

      my $dewPt = calculateDewPoint( (trim($data[13])-273.15), trim($data[5]));
      $record->setDewPoint (trim($dewPt),"C") unless (trim($dewPt) == 99999); 

      if ($debug1) {print "Temp, RH, Calc'd DewPt: $data[13]-273.15, $data[5], $dewPt \n"};

      #-------------------------------------------------------------------------
      # Set the wind speed, wind direction and calculate the UV wind components.
      #-------------------------------------------------------------------------
      my $windSpeed = 99999;
      my $windDirection = 99999;

      $windSpeed = trim($data[19]); 
      $windDirection = trim($data[15]);

      $record->setWindSpeed($windSpeed,"m/s") unless ($windSpeed == 99999);
      $record->setWindDirection($windDirection) unless ($windDirection == 99999);

      if ( ($windDirection != 99999) && ($windSpeed != 99999) )
         {
         if ($debug1) {print "Wind Speed/Wind Direction NOT missing so calcUV at numline = $numline .\n";}

         my ($uwind, $vwind) =  calculateUVfromWind($windSpeed, $windDirection);

         $record->setUWindComponent($uwind,"m/s");
         $record->setVWindComponent($vwind,"m/s");

         } # If wind speed and wind dir are NOT reset to Missing above, calc UV winds
      else
         {
         if ($debug1) {print "Either Wind Speed/Wind Direction ARE missing so DO NOT calcUV at numline = $numline .\n";}
         }


      #--------------------------------------------
      # Set a few values from the first data line. 
      #--------------------------------------------
      if ($numline == 2)
         {
         # -----------------------------------------------------------------------
         # Get radiosonde ID and only set once. ID is repeated on every data line.
         # -----------------------------------------------------------------------
         $header->setLine(5,"Radiosonde ID:", $data[12]);

         # -----------------------------------------------------------------------
         # Determine and set the Actual release time shown in header. This comes
         # from the first data line.     
         # -----------------------------------------------------------------------
         my $DataSrvTime = "99/99/9999 99:99"; # 2/3/22 4:02
         my $ActualdateVal = 99998877;
         my $ActualtimeVal = 6655;

         $DataSrvTime = $data[1]; # 2/3/22 4:02 - DataSrvTime of first Record

         my @DataSrvTime_parts = split(/ /, $DataSrvTime); # "2/3/22", "4:02"
         my @DTS_date_parts = split(/\//, $DataSrvTime_parts[0]); # 2 3 22
         my @DTS_time_parts = split(/:/, $DataSrvTime_parts[1]); # 4 02

         $ActualdateVal = sprintf("20%02d%02d%02d", $DTS_date_parts[2],$DTS_date_parts[0],$DTS_date_parts[1]); # YYYYMMDD - Year HARDCODED
         $ActualtimeVal = sprintf("%02d%02d", $DTS_time_parts[0], $DTS_time_parts[1]); # hhmm

         if ($debug) {print "DOW All Others:: DataSrvTime= xxx $DataSrvTime xxx ActualdateVal= xxx $ActualdateVal xxx ActualtimeVal = xxx $ActualtimeVal xxx\n";}

         $header->setActualRelease($ActualdateVal,"YYYYMMDD", $ActualtimeVal,"HHMM",0); 

         } # Set Actual Release Date & Time

      # --------------------------------------------------------------------
      # Ascension rate is missing on all first data recs for WINTRE-MIX. 
      # Raw data indicates zero but must reset to missing. Not really zero.
      # --------------------------------------------------------------------
      if ($numline == 2) # First data line is second row in input file
         {
         print "WARNING: Always RESET Ascension Rate on first data line to MISSING! At numline = $numline .\n";
         $record->setAscensionRate(999.0,"m/s");

         $prev_alt = $alt ;
         }
      else
         {
         # ----------------------------------------
         # Calculate and set the ascension rate.
         # The CA CU DOW data is 1 second data
         # so diff is distance traveled in 1 second.
         # -----------------------------------------
         my $ascenRate = $alt - $prev_alt;
         $prev_alt = $alt ;

         $record->setAscensionRate( $ascenRate, "m/s"); 
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
