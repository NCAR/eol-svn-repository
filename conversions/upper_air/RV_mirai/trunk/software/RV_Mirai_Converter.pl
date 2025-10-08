#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The RV_Mirai_Converter.pl script is used for converting high
# resolution radiosonde data from ASCII TSV format to the EOL Sounding Composite 
# (ESC) format.</p> 
#
# @author L. Cully 2012-02-13
# @version DYNAMO 2011 for R/V Mirai
#   This code was created by copying the R/V Baruna Jaya III 
#   DYNAMO 2011 s/w and making the following mods to process the Mirai data. 
#    - Changed all references in code from Baruna Jaya to Mirai.
#    - IMPORTANT: The R/V Mirai data has an additional coln of Elevation.
#         This param comes just after the Azimuth param.
#    - Per SL's request, added header line for "Ground Station Software".
#
# This code makes the following assumptions:
#  - That the raw data file names shall be in the form
#        "FLEDT_yyyymmddhhmm.tsv" where yyyy = year, mm = month, dd = day, hh=hour,
#         mm = minute. JNSR is the call sign for the Research Vessel Mirai.
#  - Note that the data in these files is slightly different than that of the
#         Sagar Kanya. In the MiraiVaisala "Digicora 3" format, instead
#         of 1-39 header lines with the actual data starting on line 40, these
#         data files have header lines running from 1-40 with the actual data
#         starting on line 41.  There's one additional parameter included in 
#         these data files and that is the "EL" or elevation parameter. 
#  - That the call/site ID is JNSR. 
#  - That the radiosonde type is Vaisala RS92-SGPD. 
#  - That the ground station software is Digicora III/MW31/ver3.64.1
#  - That the full name of this ship platform is R/V Mirai
#
#  - User should Search for HARDCODED.
##Module------------------------------------------------------------------------
package RV_Mirai_Converter;
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

printf "\nRV_Mirai_Converter.pl began on ";print scalar localtime;printf "\n";
my $debug = 0;
&main();
printf "\nRV_Mirai_Converter.pl ended on ";print scalar localtime;printf "\n";

my $filename;

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the R/V Mirai radiosonde data by converting it from 
# the native ASCII format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = RV_Mirai_Converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature RV_Mirai_Converter new()
# <p>Create a new instance of a RV_Mirai_Converter.</p>
#
# @output $self A new RV_Mirai_Converter object.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);
    
    $self->{"stations"} = ElevatedStationMap->new();

    # HARDCODED
    $self->{"PROJECT"} = "DYNAMO";
    $self->{"NETWORK"} = "RV_Mirai";
    
    $self->{"FINAL_DIR"}  = "../final";
    $self->{"OUTPUT_DIR"} = "../output";
    $self->{"RAW_DIR"}    = "../raw_data";
    
    $self->{"STATION_FILE"} = sprintf("%s/%s_%s_sounding_stationCD.out",$self->{"FINAL_DIR"},
                                      $self->cleanForFileName($self->{"NETWORK"}),
                                      $self->cleanForFileName($self->{"PROJECT"}));
    $self->{"WARN_LOG"} = $self->{"OUTPUT_DIR"}."/warning.log";

    return $self;
}

##------------------------------------------------------------------------------
# @signature Station buildDefaultStation(String station_id, String network)
# <p>Create a default station for the RV Mirai network using the 
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
    $station->setStateCode("99"); 
    $station->setReportingFrequency("no set schedule");
    $station->setNetworkIdNumber("99");

    # platform, JNSR, RV_Mirai - unknown
    $station->setPlatformIdNumber(999); # Platform - ship/unknown in CODIAC

    return $station;
}

##------------------------------------------------------------------------------
# @signature String buildLatlongFormat(String value)
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
sub buildLatlongFormat {
    my ($self,$value) = @_;
    
    my $fmt = $value < 0 ? "-" : "";
    while (length($fmt) < length($value)) { $fmt .= "D"; }
    return $fmt;
}

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
}

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
}

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
    printf("parsing header for %s\n",$filename);
    $header->setReleaseDirection("Ascending");

    # Set the type of sounding
    $header->setType("RV Mirai");
    $header->setProject($self->{"PROJECT"});
    
    #----------------------------------------------
    # HARDCODED
    # The Id will be the prefix of the output file.
    #----------------------------------------------
    $header->setId("RV_Mirai");

    #---------------------------------------------
    # HARDCODED
    # "Release Site Type/Site ID:" header line
    #---------------------------------------------
    $header->setSite("JNSR");

    # ------------------------------------------------
    # Read through the file for additional header info
    # ------------------------------------------------
    my $index = 0;

    foreach my $line (@headerlines) 
      {
      #-------------------------------------------------------------
      # Add the non-predefined header lines to the header.
      # Changed $i to $i-1 to remove extra blank line from header. 
      # for (my $i = 6; $i < 11; $i++) 
      #-------------------------------------------------------------
      if (($index > 0) && ($index < 11))
         {
         if ($line !~ /^\s*\/\s*$/) 
            {
            if ($line =~ /RS-Number/i)
               {
               chomp ($line);
               chop ($line); # Chop off control M found in Mirai data
               my ($label,@contents) = split(/:/,$line);
               $label = "Sonde Id/Sonde Type";

               #-------------------------------------------------------------
               # HARDCODED
               # Add "Vaisala RS92-SGPD" after sonde ID (@contents) (per SL)
               #-------------------------------------------------------------
               $contents[1] = "Vaisala RS92-SGPD";
               $header->setLine(($index-1), trim($label).":",trim(join("/",@contents)));

              }  # RS-Number
           } # Rm blank lines
       } # index 0 to 11 

       #--------------------------------------------------------------
       # HARDCODED
       # Add header line (per SL) to include "Ground Station Software"
       #--------------------------------------------------------------
       $header->setLine((6), "Ground Station Software:",trim(join("/","Digicora III/MW31/ver3.64.1")));

       #-------------------------
       # Ignore the header lines.
       #-------------------------
       if ($index < 40) 
          { 
          $index++; 
          next; 
          }
       else
          {               
          #--------------------------------------------------------------
          # Find the lat/lon for the release location in the actual data.
          # For Mirai, the lat/lon are in the 15/16 colns.
          #--------------------------------------------------------------
          my @data = split(' ',$line);

          if (($data[15] > -32768) & ($data[16] > -32768))
             {
             #--------------------------------------------------------------
             # format length must be the same as the value length or
             # convertLatLong will complain (see example below)
             # base lat = 36.6100006103516 base lon = -97.4899978637695
             # Lat format = DDDDDDDDDDDDDDDD  Lon format = -DDDDDDDDDDDDDDDD  
             #--------------------------------------------------------------
             my $lon_fmt = $data[15] < 0 ? "-" : "";
             while (length($lon_fmt) < length($data[15])) { $lon_fmt .= "D"; }
             $header->setLongitude($data[15],$lon_fmt);

             my $lat_fmt = $data[16] < 0 ? "-" : "";
             while (length($lat_fmt) < length($data[16])) { $lat_fmt .= "D"; }
             $header->setLatitude($data[16],$lat_fmt);
 
             $header->setAltitude($data[6],"m"); 
             last;
             } # data 15,16 not +/-32768

          } # find lat/long release loc
      } # foreach $line in @headerlines

    # ----------------------------------------------------------
    # Extract the date and time information from the file name
    # BEWARE: Expects filenames in particular format.
    # ----------------------------------------------------------
    print "file name = $filename\n"; 

    my $date;
    my $time;

    if ($filename =~ /(\d{4})(\d{2})(\d{2})(\d{2})(\d{2})/)
       {
       my ($yearInfo, $monthInfo, $dayInfo, $hourInfo, $minInfo) = ($1,$2,$3,$4,$5);
       $date = join ", ", $yearInfo, $monthInfo, $dayInfo;
       print "date is $date\n";
       $time = join "", $hourInfo, ' ', $minInfo, ' 00';
       print "time is $time\n";
       }

    $header->setActualRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
    $header->setNominalRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);

    return $header;

} # parseHeader()
                           
##------------------------------------------------------------------------------
# @signature void parseRawFiles(String file)
# <p>Read the data from the specified file and convert it to the ESC format.</p>
#
# @input $file The name of the file to be parsed.
##------------------------------------------------------------------------------
sub parseRawFile {
    my ($self,$file) = @_;
    
    printf("\nProcessing file: %s\n",$file);

    open(my $FILE,$self->{"RAW_DIR"}."/".$file) or die("Can't open file: ".$file);

    my @lines = <$FILE>;

    close($FILE);

    #-----------------------------    
    # Generate the sounding header.
    #-----------------------------    
    my @headerlines = @lines;
    my $header = $self->parseHeader($file,@headerlines);
    
    #-----------------------------------------------------------
    # Only continue processing the file if a header was created.
    #-----------------------------------------------------------
    if (defined($header)) 
       {
       #-----------------------------------------------------------
       # Determine the station the sounding was released from.
       #-----------------------------------------------------------
       my $station = $self->{"stations"}->getStation($header->getId(),$self->{"NETWORK"},
                             $header->getLatitude(),$header->getLongitude(),
                             $header->getAltitude());

       if (!defined($station)) 
          {
          $station = $self->buildDefaultStation($header->getId(),$self->{"NETWORK"});
          $station->setLatitude($header->getLatitude(),$self->buildLatlongFormat($header->getLatitude()));
          $station->setLongitude($header->getLongitude(),$self->buildLatlongFormat($header->getLongitude()));
          $station->setElevation($header->getAltitude(),"m");
          $self->{"stations"}->addStation($station);
          }

       $station->insertDate($header->getNominalDate(),"YYYY, MM, DD");

       #-----------------------------------------------------
       # Create the output file name and open the output file
       #-----------------------------------------------------
       my $outfile;
       my ($hour, $min, $sec) = split (/:/, $header->getActualTime());

       $outfile = sprintf("%s_%04d%02d%02d%02d%02d.cls", 
                           $header->getId(),
                           split(/,/,$header->getActualDate()),
                           $hour, $min);
    
       printf("\tOutput file name is %s\n", $outfile);

       open(my $OUT,">".$self->{"OUTPUT_DIR"}."/".$outfile)
               or die("Can't open output file for $file\n");

       print($OUT $header->toString());
      
       #-----------------------------------------
       # Needed for code to derive ascension rate
       #-----------------------------------------
       my $prev_time = 9999.0;
       my $prev_alt = 99999.0;

       #--------------------------------------------
       # Parse the data portion of the input file
       #--------------------------------------------
       my $index = 0;

       foreach my $line (@lines) 
         {
         #-------------------------
         # Ignore the header lines.
         #-------------------------
         if ($index < 40) { $index++; next; }
          
         my @data = split(' ',$line);
         my $record = ClassRecord->new($WARN,$file);

         $record->setTime($data[0]);
         $record->setPressure($data[7],"mb") if ($data[7] != -32768);

         #----------------------------------------------------------------
         # Temp and Dewpt are in Kelvin.  C = K - 273.15
         # $record->setTemperature($data[2],"C") if ($data[2] != -32768);    
         #----------------------------------------------------------------
         $record->setTemperature(($data[2]-273.15),"C") if ($data[2] != -32768);    
         $record->setDewPoint(($data[8]-273.15),"C") if ($data[8] != -32768);
         $record->setRelativeHumidity($data[3]) if ($data[3] != -32768);
         $record->setUWindComponent($data[5],"m/s") if ($data[5] != -32768);
         $record->setVWindComponent($data[4],"m/s") if ($data[4] != -32768);
         $record->setWindSpeed($data[11],"m/s") if ($data[11] != -32768);
         $record->setWindDirection($data[10]) if ($data[10] != -32768);  

         #------------------------------
         # Get and set the longitude
         #------------------------------
         if ($data[15] != -32768) 
            {
            my $lon_fmt = $data[15] < 0 ? "-" : "";
            while (length($lon_fmt) < length($data[15])) { $lon_fmt .= "D"; }

            $record->setLongitude($data[15],$lon_fmt);
            }

         #------------------------------
         # Get and set the Latitude
         #------------------------------
         if ($data[16] != -32768) 
           {
            my $lat_fmt = $data[16] < 0 ? "-" : "";
            while (length($lat_fmt) < length($data[16])) { $lat_fmt .= "D"; }

            $record->setLatitude($data[16],$lat_fmt);
           }

         #-----------------------------------------------------------
         # Insert Ele (Elevation Angle) and Azi (Azimuth Angle) data
         # For setVariableValue(index, value):  
         # index (1) is Ele column, index (2) is Azi column.
         #-----------------------------------------------------------
         $record->setVariableValue(2, $data[12]) if ($data[12] != -32768); # Azimuth
         $record->setVariableValue(1, $data[13]) if ($data[13] != -32768); # Elevation

         #-------------
         # Set Altitude
         #-------------
         $record->setAltitude($data[6],"m") if ($data[6] != -32768);
                                                
         #-------------------------------------------------------
         # Calculate the ascension rate which is the difference
         # in altitudes divided by the change in time. Ascension
         # rates can be positive, zero, or negative. But the time
         # must always be increasing (the norm) and not missing.
         #
         # Only save off the next non-missing values.
         # Ascension rates over spans of missing values are OK.
         # This code originally from the UA Ron Brown Converter.
         #-------------------------------------------------------
         if ($debug) 
            { 
            my $time = $record->getTime(); my $alt = $record->getAltitude(); 
            print "\nNEXT Line: prev_time, rec Time, prev_alt, rec Alt:: $prev_time, $time, $prev_alt, $alt\n"; 
            }

         if ($prev_time != 9999  && $record->getTime()     != 9999  &&
             $prev_alt  != 99999 && $record->getAltitude() != 99999 &&
             $prev_time != $record->getTime() ) 
           {
           $record->setAscensionRate( ($record->getAltitude() - $prev_alt) /
                                      ($record->getTime() - $prev_time),"m/s");

           if ($debug) { print "Calc Ascension Rate.\n"; }
           }

         #-----------------------------------------------------
         # Only save off the next non-missing values. 
         # Ascension rates over spans of missing values are OK.
         #-----------------------------------------------------
         if ($debug) 
            { 
            my $rectime = $record->getTime(); my $recalt = $record->getAltitude();
            print "Try SAVE Line: rec Time, rec Alt:: $rectime, $recalt\n"; 
            }

         if ($record->getTime() != 9999 && $record->getAltitude() != 99999)
            {
            $prev_time = $record->getTime();
            $prev_alt = $record->getAltitude();

            if ($debug) { print "Current rec has valid Time and Alt. Save as previous.\n"; }
            }

         #------------------------------------
         # Completed the ascension rate data
         #------------------------------------
         printf($OUT $record->toString());
         } #foreach line
      } # if hdr defined
   else
      {
      printf("Unable to make a header\n");
      } # if hdr defined

} # End parseRawFiles()

##------------------------------------------------------------------------------
# @signature void printStationFiles()
# <p>Generate the output files containing station information.</p>
##------------------------------------------------------------------------------
sub printStationFiles {
    my ($self) = @_;

    open(my $STN, ">".$self->{"STATION_FILE"}) || 
             die("Cannot create the ".$self->{"STATION_FILE"}." file\n");

    foreach my $station ($self->{"stations"}->getAllStations()) 
       {
       print($STN $station->toString()) if ($station->getBeginDate !~ /^9+$/);
       } 

    close($STN);
}

##------------------------------------------------------------------------------
# @signature void readDataFiles()
# <p>Read in the files from the raw data directory and convert each into an
# ESC formatted file.</p>
##------------------------------------------------------------------------------
sub readDataFiles {
    my ($self) = @_;
    
    opendir(my $RAW,$self->{"RAW_DIR"}) or die("Can't read raw directory ".$self->{"RAW_DIR"});

    my @files = grep(/^FLEDT_\d{12}\.tsv/,sort(readdir($RAW)));
    closedir($RAW);
    
    open($WARN,">".$self->{"WARN_LOG"}) or die("Can't create ".$self->{"WARN_LOG"});
    
    if ($debug) { printf("Ready to read the files\n"); }

    foreach my $file (@files) 
       {
       $self->parseRawFile($file);
       }
    
    close($WARN);
}

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
}
