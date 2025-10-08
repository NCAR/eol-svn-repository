#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The Indonesia_Palu_converter.pl script is used for converting 
# high resolution radiosonde data to the EOL Sounding Composite (ESC) format.
#
# @author L.E. Cully October 2012
# @version DYNAMO  Created to convert Indonesia Palu Sonde (Meisei) data.
#          Portions of this code are based on the T-PARC_2008 Japanese 
#          Radiosonde Converter. Note that preprocessing s/w was created to 
#          more closely examine the raw data for time gaps and duplicate records.
#          After S. Loehrer examined the results of the preprocessing,
#          he determined that the time gaps and duplicates are a result
#          of the internal clock being off and it getting reset at
#          every point where the GPS data returns after dropping out. 
#          Instructions from the scientific staff are to ignore all 
#          times in the Palu data and to assume that every record is 
#          one second once "acceptable" initial conditions (desccribed next)
#          have been met.    
#
#          This s/w first processes info from the data file header. The s/w then
#          examines raw data records until it finds the surface record. The
#          surface record is indicated when the third coln becomes "7". That
#          point is retained and written as the surface ("0 second") (T0) record.
#          To find the "1 second" (T1) and "2 second" (T2) records, the s/w
#          marches through the data records (in order) searching for two records
#          (no time gaps) where the pressure (P1) is less than or equal to 
#          the pressure at T0 and greater or equal to next record's pressure (P2)
#          (i.e., P0 >= P1 >= P2).  Once this condition is met, records with P1 and
#          P2 are retained and written to the output.  From P2 forward in the data 
#          file time is computed by simply incrementing time by 1 second through
#          to the end of the file. File times are ignored.
#
#          Also this s/w strips out the end of the sounding where the
#          pressures start dropping (sonde falling).
#
#          Note that a post processor was created to convert the ESC file (generated
#          by running this converter) from local to UTC times. 
#   -------------------------------------------------------------------------------
#   Notes and Assumptions:
#
#          - Search for HARDCODED to find project specific and other hard coded values. 
#          - This s/w assumes the data are in Meisei format and that the
#            raw data file names are of the form F*.CSV. Only *.CSV files
#            in the ../raw_data directory will be processed. 
#          - This s/w processes 
#          - This s/w assumes that the raw data files to process are located in ../raw_data.
#          - This s/w assumes the ../output directory exists. The software writes 
#            output ESC files to the ../output directory.
#          - This s/w assumes the ../final directory exists. The s/w writes the
#            Palu_DYNAMO_sounding_stationCD.out station file to the
#            ../final directory.
#          - This s/w processes data in Meisei format.
#          - This s/w computes the ascension rate using the same method as used
#            for the Ron Brown converter.
#          - This s/w expects the actual data to begin on line 8 of raw data file.
#          - This s/w ignores data records times. Times are computed from
#            the release point (T0) where the third coln equals "7". See description
#            above. 
#          - This s/w removes the descending data records (i.e., ascension rate is
#            negative) at the end of the file.
#          - For surface records with missing lat/lon/alt data, header
#            info is substituted in.
#
#
#  WARNING: Times in data files are local times, so output file
#           contains local times. See post processor to convert
#           local times to UTC which changes the output file name,
#           and two header lines (UTC Release and Nominal Release times).
#           Note that Palu's local time data is -8 hrs off from UTC. LT-8=UTC.
#
#  WARNING: To see debug output, set $debug = 1.
#
#  Execute:
#
# @use     Indonesia_Palu_converter.pl >& output.log
#
##Module------------------------------------------------------------------------
package Indonesia_Palu_converter;
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

use ElevatedStationMap;
use Station;
use ClassConstants qw(:DEFAULT);
use ClassHeader;
use ClassRecord;

my ($WARN);

printf "\nIndonesia_Palu_converter.pl began on ";print scalar localtime;printf "\n";
my $debug = 0;
my $debug_bad_asc = 0;

&main();
printf "\nIndonesia_Palu_converter.pl ended on ";print scalar localtime;printf "\n";

my $filename;
my $sounding = "";

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the Indonesia Palu radiosonde data by converting it from 
# the native ASCII format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = Indonesia_Palu_converter->new();
    $converter->convert();
} # main()

##------------------------------------------------------------------------------
# @signature Indonesia_Palu_converter new()
# <p>Create a new instance of a Indonesia_Palu_converter.</p>
#
# @output $self A new Indonesia_Palu_converter object.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);
    
    $self->{"stations"} = ElevatedStationMap->new();

    # HARDCODED
    $self->{"PROJECT"} = "DYNAMO";
    $self->{"NETWORK"} = "Indonesia Palu";
    
    $self->{"FINAL_DIR"} = "../final";
    $self->{"OUTPUT_DIR"} = "../output"; 
    $self->{"RAW_DIR"} = "../raw_data"; 
    
    $self->{"STATION_FILE"} = sprintf("%s/%s_%s_sounding_stationCD.out",$self->{"FINAL_DIR"},
				      $self->cleanForFileName($self->{"NETWORK"}),
				      $self->cleanForFileName($self->{"PROJECT"}));
    $self->{"WARN_LOG"} = $self->{"OUTPUT_DIR"}."/warning.log";

    return $self;
} # new()

##------------------------------------------------------------------------------
# @signature Station buildDefaultStation(String station_id, String network)
# <p>Create a default station for the Ishigaki-Jima network using the 
# specified station_id and network.</p>
#
# @input $station_id The identifier of the station to be created.
# @input $network The network the station belongs to.
# @return The new station object with the default values for the network.
##------------------------------------------------------------------------------
sub buildDefaultStation {
    my ($self,$station_id,$network) = @_;
    my $station = Station->new($station_id,$network);
    $station->setStationName($network);
    $station->setLatLongAccuracy(3);
    # HARDCODED
    # $station->setStateCode("99");
    $station->setCountry("Indonesia");
    $station->setReportingFrequency("no set schedule");
    $station->setNetworkIdNumber("99");
    # platform, 439, MEISEI Sounding
    $station->setPlatformIdNumber(439);

    return $station;

} # buildDefaultStation()

##------------------------------------------------------------------------------
# @signature String buildLatlonFormat(String value)
# <p>Generate the decimal format for the specified value.</p>
#
# <p>format length must be the same as the value length or
# convertLatLong will complain (see example below)<br />
# base lat = 36.6100006103516 base lon = -97.4899978637695<br />
# Lat format = DDDDDDDDDDDDDDDD  Lon format = -DDDDDDDDDDDDDDDD</p>
#
# @input $value The value of the lat/lon being formatted.
# @output $fmt The format that corresponds the the value.
##------------------------------------------------------------------------------
sub buildLatlonFormat {
    my ($self,$value) = @_;
    
    my $fmt = $value < 0 ? "-" : "";
    while (length($fmt) < length($value)) { $fmt .= "D"; }
    return $fmt;

} # buildLatlonFormat()

##-------------------------------------------------------------------------
# @signature String cleanForFileName(String text)
# <p>Remove/translate characters in a String so it can be used in a file name.</p>
#
# @input $text The String to be cleaned.
# @output $text The cleaned up String.
##-------------------------------------------------------------------------
sub cleanForFileName {
    my ($self,$text) = @_;

    # Convert spaces to underscores.
    $text =~ s/\s+/_/g;

    # Remove all hyphens
    $text =~ s/\-//g;

    return $text;

} #cleanForFileName()

##------------------------------------------------------------------------------
# @signature void convert()
# <p>Convert the raw data to the ESC format.</p>
##------------------------------------------------------------------------------
sub convert {
    my ($self) = @_;
    
    mkdir($self->{"OUTPUT_DIR"}) unless (-e $self->{"OUTPUT_DIR"});
    mkdir($self->{"FINAL_DIR"}) unless (-e $self->{"FINAL_DIR"});
    
    $self->readDataFiles();
    $self->printStationFiles();

} # convert()

##------------------------------------------------------------------------------
# @signature ClassHeader parseHeader(String file, String[] lines)
# <p>Parse the header lines from the specified file into the ESC header format.</p>
#
# @input $file The name of the file being parsed.
# @input $lines[] The header lines to be parsed.
# @output $header The header data in ESC format.
##------------------------------------------------------------------------------
sub parseHeader {
    my ($self,$file,@headerlines) = @_;
    my $header = ClassHeader->new();

    $filename = $file;
    if ($debug) {printf("parsing header for %s\n",$filename); }

    $header->setReleaseDirection("Ascending");   #HARDCODED

    #-------------------------------------------------
    # Set the sounding type for first header
    # line: "Data Type:  Palu/Ascending"
    #-------------------------------------------------
    $header->setType("BMKG Radiosonde");         #HARDCODED
    $header->setReleaseDirection("Ascending");   #HARDCODED
    $header->setProject($self->{"PROJECT"});   
    
    #-------------------------------------------------
    # HARDCODED
    # The Id will be the prefix of the output file
    #-------------------------------------------------
    $header->setId("Indonesia_Palu");        #HARDCODED

    #-------------------------------------------------
    # This info goes in the 
    # "Release Site Type/Site ID:" header line
    #-------------------------------------------------
    $header->setSite("Palu, Indonesia/97072");  #HARDCODED

    #-------------------------------------------------
    # Read through the file for additional header info
    #-------------------------------------------------
    foreach my $line (@headerlines) 
	{
	if ($line =~ /RS-06G/)       #HARDCODED
           {
           chomp($line);

           my @headerInfo = split(',', $headerlines[0]);
		    
           #-------------------------------------------------
	   # Set the radiosonde id header line info
           #-------------------------------------------------
           my @contents;
           my $label = "Sonde Id/Sonde Type";
           $contents[0] = $headerInfo[1];
           $contents[1] = join(" ", "Meisei", $headerInfo[0]);
           $header->setLine(5, trim($label).":",trim(join("/",@contents))); 

           #-------------------------------------------------
           # Set the release location info
           #-------------------------------------------------
           my $lat = trim($headerInfo[9]);
           my $lon = trim($headerInfo[10]);
           $lat =~ s/\+//g;
           $lon =~ s/\+//g;

           if ($debug) { print "\tHEADER LAT:  $lat  HEADER LON:  $lon\n"; }

           $header->setLatitude($lat,$self->buildLatlonFormat($lat));
           $header->setLongitude($lon,$self->buildLatlonFormat($lon));
           $header->setAltitude($headerInfo[12],"m"); 
    
           #-------------------------------------------------
           # Set the release date and time info
           #-------------------------------------------------
	   my $date = trim($headerInfo[4]);

           if ($date =~ /(\d{4})\/(\d{2})\/(\d{2})/)
              {
              my ($year, $month, $day) = ($1,$2,$3);
              $date = join ", ", $year, $month, $day;
              }
           else
              { print "WARNING: No date found in header info\n"; }
	        
           my $time = $headerInfo[5];

           $header->setActualRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
           $header->setNominalRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);

           } # if RS-06G

        } # foreach line

    return $header;

    } # ParseHeader{}
                           
##------------------------------------------------------------------------------
# @signature void parseRawFiles(String file)
# <p>Read the data from the specified file and convert it to the ESC format.</p>
#
# @input $file The name of the file to be parsed.
##------------------------------------------------------------------------------
sub parseRawFile {
   my ($self,$file) = @_;
    
   printf("\n+++++++++++++++++++++++++\nProcessing file: %s\n",$file);

   open(my $FILE,$self->{"RAW_DIR"}."/".$file) or die("Can't open file: ".$file);
   my @lines = <$FILE>;
   close($FILE);

   my $lines_in_file = $#lines +1;

   if ($debug) {print "Number of lines in file = $lines_in_file\n";}

    
   #------------------------------
   # Generate the sounding header.
   #------------------------------
   my @headerlines = @lines[0..1];
   my $header = $self->parseHeader($file,@headerlines);
    
   #-----------------------------------------------------------
   # Only continue processing the file if a header was created.
   #-----------------------------------------------------------
   if (!defined($header)) { print "WARNING: Unable to create header\n"; }

   #-----------------------------------------------------------
   # Determine the station the sounding was released from.
   #-----------------------------------------------------------
   my $station = $self->{"stations"}->getStation($header->getId(),$self->{"NETWORK"},
                    $header->getLatitude(),$header->getLongitude(),
                    $header->getAltitude());

   if (!defined($station)) 
      {
      $station = $self->buildDefaultStation($header->getId(),$self->{"NETWORK"});
      $station->setLatitude($header->getLatitude(),$self->buildLatlonFormat($header->getLatitude()));
      $station->setLongitude($header->getLongitude(),$self->buildLatlonFormat($header->getLongitude()));
      $station->setElevation($header->getAltitude(),"m");
      $self->{"stations"}->addStation($station);
      }

   $station->insertDate($header->getNominalDate(),"YYYY, MM, DD");

   # ----------------------------------------------------
   # Create the output file name and open the output file
   # ----------------------------------------------------
   my ($hour, $min, $sec) = split (/:/, $header->getActualTime());

   if ($debug) { print "FROM HEADER TIME:  $hour $min $sec\n";}

   my $outfile = sprintf("%s_%04d%02d%02d%02d%02d.cls", 
                         $header->getId(),
                         split(/,/,$header->getActualDate()),
                         $hour, $min);
 
   printf("\tOutput file name is %s\n", $outfile);


   open(my $OUT,">".$self->{"OUTPUT_DIR"}."/".$outfile)
                      or die("Can't open output file for $file\n");

   print($OUT $header->toString());
   
    
   # --------------------------------------------
   # Create an array to hold all of the data records.
   # This is required so additional processing can take
   # place to remove descending data records at the
   # end of the data files
   # --------------------------------------------
   my @record_list = ();

   # ----------------------------------------
   # Needed for code to derive ascension rate
   # ----------------------------------------
   my $prev_time = 9999.0;
   my $prev_alt = 99999.0;

   my $lat = -999.99; my $lon = -999.99; my $alt = -999.99;

   my $pressureZeroRec  = -999.99;
   my $pressureNextRec  = -888.88;
   my $pressureNextRec2 = -777.77;

   my $recordTime = -1;

   my $surfaceRecord = 0;
   my $realData = 0;
   my $first_valid_rec = 0;

   my $print_rec = 0;
   
   # ------------------------
   # Loop through all records
   # ------------------------
   my $index = 0;
   foreach my $line (@lines) 
      {
      if ($debug){ print "\n-----TOP FOREACH: index = $index ------\n"; }

      #----------------------------------------
      # Ignore/Skip the Eight (8) header lines.
      #----------------------------------------
      if ($index < 7) { $index++; next; }
    
      chomp($line);

      my @data    = split(',',$line);

      # ------------------------------------------------
      # For the Indonesia sites, the release point is 
      # when the third "data" coln is "7". So real data
      # starts at the "7" time.
      # ------------------------------------------------
      if ($debug) { print "Search for release pt: data[2] = $data[2]. If !7 then SKIP IT!\n"; }

      if ((!$realData) && ($data[2] == 7))     #HARDCODED
         {
         if ($debug) {print "FOUND release point first. data[2] = $data[2]! Save off the pressure!\n";}

         $realData = 1;       # Found first real data to check and process
         $surfaceRecord = 1;  # Found sfc record/Release point

         $pressureZeroRec  = $data[20] unless $data[20] =~ /-+$/;   # Release point pressure

         if ($debug) {print "pressureZeroRec = $pressureZeroRec\n";}

         if ($pressureZeroRec <= -999.00) # Must have legit/non-missing Release Point pressure
            {
            print "ERROR: Bad-Missing pressure on Release Point (0 second) record = $pressureZeroRec . Exit!";
            exit(1);
            }

         } # if found release point - !$realData

      
      if ($realData)
         {
         if ($surfaceRecord) # If surface rec, push to output. After that start searching for 2 good pressures
            {
            if ($debug) { print "This is the surface record! surfaceRecord = $surfaceRecord\n"; }

            $recordTime = 0.0;
            $print_rec = 1;
            $index --;  #decrement so we'll consider this rec again after pushing to output
            }
         else
            {
            # ---------------------------------------------------------------
            # There are issues in the lower altitude data, so the code must
            # locate the release point (3rd coln = 7) and then ignore records
            # until the pressure on the two subsequent records have pressures
            # less than that of the release point (P0). So, P0> P1 > P2.
            # The pressure must also decrease between P1 and P2. Once this
            # condition is found, accept records from that point forward.
            # Ignore time! Simple add one second for each subsequent record.
            # The pressures can all be equal. That would also be valid.
            # ---------------------------------------------------------------
            if ($debug) 
               { 
               print "RealData = $realData; surfaceRecord = $surfaceRecord;first_valid_rec = $first_valid_rec; lines_in_file = $lines_in_file\n"; 
               }

            # -----------------------------------------------------
            # Search for two valid pressures in a row. They must be
            # less than or equal to the surface pressure.
            # -----------------------------------------------------
            if (!$first_valid_rec)
               {
               $pressureNextRec  = 8888;
               $pressureNextRec2 = 8888;

               my $line1;   my $line2;
               my @dataNR;  my @dataNR2;

               if ( $index < ($lines_in_file-1))
                  {
                  $line1 = $lines[$index+1]; 
                  @dataNR  = split(',',$line1); # Need to examine next pressure = NR1
                  $pressureNextRec  = $dataNR[20]  unless $dataNR[20]  =~ /-+$/;
                  } 

               if ( $index < ($lines_in_file-2))
                  {
                  $line2 = $lines[$index+2]; 
                  @dataNR2 = split(',',$line2); # Need to examine second "next" pressure ahead = NR2
                  $pressureNextRec2 = $dataNR2[20] unless $dataNR2[20] =~ /-+$/;
                  }

               if ($debug) 
                 { 
                  print "Form::\nline1 = $line1\n line2 = $line2\n";
                  print "index=$index, pressureZeroRec = $pressureZeroRec; (data[20] = $data[20]), dataNR[20] = $dataNR[20] dataNR2[20] = $dataNR2[20]\n";
                 print "COMPARE:: pressureZeroRec = $pressureZeroRec; pressureNextRec = $pressureNextRec; pressureNextRec2 = $pressureNextRec2 ; first_valid_rec = $first_valid_rec \n"; 
                 print "Check first_valid_rec = $first_valid_rec\n";
                 }

               } # if first_valid_rec

            if ( $first_valid_rec || (($pressureZeroRec >= $pressureNextRec) && ($pressureNextRec >= $pressureNextRec2)) )
               {
               if ($debug) {print "FOUND:: first valid recs/start incrementing or continue time AND writing out recs! \n";}

               $first_valid_rec = 1;
               $recordTime++;
               $print_rec = 1;

               if ($debug) {print "Increment Time:: recordTime = $recordTime! print_rec = $print_rec\n";}
               }
            else
               {
               $print_rec = 0;

               if ($debug) {print "Not VALID record = Skip! Do not print!  Increment index = $index; print_rec = $print_rec\n";}
               }
            } # Not surface rec so search for valid pressures


         #--------------------------------------------
         # Only fill the rec if going to print the rec
         # Always print the surface rec/Release Point.
         #--------------------------------------------
         if ($print_rec)
            {
            # ---------------------------------------------------
            # Found first valid time. From this point forward,
            # assume that each record is one second off. Ignore
            # the times, time gaps, and duplicate times on the
            # records per S. Loehrer.  Create time by counting
            # sequentially.
            # ---------------------------------------------------
            my $record = ClassRecord->new($WARN,$file);

            $record->setTime($recordTime);  # Set time on rec

            # ------------------------------------------------
            # Else, we have a good data record, so process it. 
            # Units are HARDCODED.
            # ------------------------------------------------
            $record->setPressure($data[20],"mb") unless ($data[20] =~ /-+$/);
            $record->setTemperature($data[21],"C") unless ($data[21] =~ /-+$/);    
            $record->setRelativeHumidity($data[22]) unless ($data[22] =~ /-+$/);

            $record->setWindSpeed($data[10],"m/s") unless ($data[10] =~ /-+$/);
            $record->setWindDirection($data[9]) unless ($data[9] =~ /-+$/);

            $lat = trim($data[17]);
            $lon = trim($data[18]);
            $alt = trim($data[11]);

            # ---------------------------------------------
            # if the surface record is missing lat/lon/alt 
            # values, set to value in header
            # ---------------------------------------------
            if ($surfaceRecord)
               {
               if ($lat =~ /-+$/) { $lat = $header->getLatitude(); }
               if ($lon =~ /-+$/) { $lon = $header->getLongitude(); }
               if ($alt =~ /-+$/) { $alt = $header->getAltitude(); }

               $surfaceRecord = 0;

               $print_rec = 0;

               } # if surface rec

            $record->setLatitude($lat, $self->buildLatlonFormat($lat)) unless ($lat =~ /-+$/); 
            $record->setLongitude($lon,$self->buildLatlonFormat($lon)) unless ($lon =~ /-+$/);
            $record->setAltitude($alt,"m") unless ($alt =~ /-+$/);
       
            #-------------------------------------------------------
            # Calculate the ascension rate which is the difference
            # in altitudes divided by the change in time. Ascension
            # rates can be positive, zero, or negative. But the time
            # must always be increasing (the norm) and not missing.
            #
            # Only save the next non-missing values.
            # Ascension rates over spans of missing values are OK.
            #-------------------------------------------------------
            if ($debug) 
               { 
               my $time = $record->getTime(); 
               my $alt = $record->getAltitude(); 
               print "\nCurrent Line: Time $time, prev_time $prev_time, " . "Alt $alt, prev_alt $prev_alt\n"; 
               } # debug

            #-----------------------------------------------------------
            # If the current and previous times and altitudes are not
            # missing and both times are not the same (potential divide
            # by zero!), then calculate the ascension rate. 
            #-----------------------------------------------------------
            if ($prev_time != 9999  && $record->getTime()     != 9999  &&
                $prev_alt  != 99999 && $record->getAltitude() != 99999 &&
                $prev_time != $record->getTime() ) 
               { 
               # ---------------------------
               # Compute the ascension rate.
               # ---------------------------
               $record->setAscensionRate( ($record->getAltitude() - $prev_alt) /
                                          ($record->getTime() - $prev_time),"m/s");

               if ($debug) { print "Ascension Rate calculated and set.\n"; }
               } # if prev_time

              if ($debug)
                 {
                 my $asc1 = $record->getAscensionRate();
                 print "AscensionRate = $asc1 ; index = $index\n";
                 }

            if ($record->getTime() != 9999 && $record->getAltitude() != 99999)
               {
               $prev_time = $record->getTime();
               $prev_alt = $record->getAltitude();

               if ($debug) { print "Saved Current time/alt as Previous. prev_alt = $prev_alt\n"; }
               }

            # --------------------------------------------
            # Add each record to the record_list array
            # for further processing to remove descending
            # data before calling print toString
            # --------------------------------------------
            if ($debug) { print "PUSH rec onto list for writing to output.\n"; }

            push(@record_list, $record);

            } # end if print_rec or surfaceRecord

         } # end if real data

      $index++;

      if ($debug) { print "------End Foreach: index = $index-----\n"; }

      } # end For each line

   if ($debug) { print "Post End of Foreach line in file.\n\n"; }

   if (!$first_valid_rec) { print "WARNING:: No valid records beyond release point!\n"; }

   # --------------------------------------------------
   # Remove the last records in the file that are 
   # descending (ascent rate is negative or zero)
   # --------------------------------------------------
   if ($debug) { print "\nCheck for dropping sonde. first_valid_rec = $first_valid_rec\n"; }

   foreach my $record (reverse(@record_list))
      {
      if ($debug)
         {
         my $asc1 = $record->getAscensionRate();
         print "AscensionRate = $asc1 ; first_valid_rec = $first_valid_rec\n"; 
         }

      if (( ($record->getAscensionRate() <= 0.0) ||
            ($record->getAscensionRate() == 999.0)) && ($first_valid_rec))
         {
         if ($debug) { print "REMOVE descending  rec - undef! ascRate = $record->getAscensionRate()\n"; }
         undef($record);
         } 
      else 
         { last; }

      } # foreach
    
   #----------------------------------
   # Print the records to the file and
   # close the output file..
   #----------------------------------
   if ($debug) { print "Print records to output file.\n"; }
   foreach my $final_record(@record_list) 
      {
      if ($debug) { print "Printing record:: $final_record\n"; }
      print ($OUT $final_record->toString()) if (defined($final_record));
      }   
   
   close($OUT);

   if (!$first_valid_rec && $pressureNextRec == 8888 && $pressureNextRec2 == 8888)
      {
      print "WARNING: No valid data found in this file!\n";
      }

   if ($debug) { print "End parseRawFiles().\n"; }

   } # parseRawFiles() 

##------------------------------------------------------------------------------
# @signature void printStationFiles()
# <p>Generate the output files containing station information.</p>
##------------------------------------------------------------------------------
sub printStationFiles {
    my ($self) = @_;

    open(my $STN, ">".$self->{"STATION_FILE"}) || 
	die("Cannot create the ".$self->{"STATION_FILE"}." file\n");
    foreach my $station ($self->{"stations"}->getAllStations()) {
        print($STN $station->toString()) if ($station->getBeginDate !~ /^9+$/);
    }
    close($STN);
} # printStationFiles()

##------------------------------------------------------------------------------
# @signature void readDataFiles()
# <p>Read in the files from the raw data directory and convert each into an
# ESC formatted file.</p>
##------------------------------------------------------------------------------
sub readDataFiles {
    my ($self) = @_;
    
    opendir(my $RAW,$self->{"RAW_DIR"}) or die("Can't read raw directory ".$self->{"RAW_DIR"});
    my @files = grep(/\.CSV$/i,sort(readdir($RAW)));        #HARDCODED
    closedir($RAW);
    
    
    open($WARN,">".$self->{"WARN_LOG"}) or die("Can't create ".$self->{"WARN_LOG"});
    
    foreach my $file (@files) {
	$self->parseRawFile($file);
    }
    
    close($WARN);
} # readDataFiles()

##------------------------------------------------------------------------------
## @signature String trim(String line)
## <p>Remove all leading and trailing whitespace from the specified String.</p>
##
## @input $line The String to be trimmed.
## @output $line The trimmed String.
##------------------------------------------------------------------------------
sub trim {
    my ($line) = @_;
    return $line if (!defined($line));
    $line =~ s/^\s+//;
    $line =~ s/\s+$//;
    return $line;
} #trim()

