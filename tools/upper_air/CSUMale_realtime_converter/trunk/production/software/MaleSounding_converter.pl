#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The MaleSounding_converter.pl script is used for converting high
# resolution radiosonde data from ASCII to the modified EOL Sounding Composite (ESC) 
# format. The incoming format is Vaisala (Digicora 3). The output format is
# basically ESC, except the first header line of "Data Type:" is output 
# to be "CLASS,". This is so that the output format can be read by ASPEN
# as a class formatted sounding. ASPEN is then used to convert the CLASS/ESC
# output sounding into GTS format.
#</p> 
#
# @author Linda Cully 2011-10-13
# @version DYNAMO_2011 Updated to check number of lines in error file.
#    - Updated to check number of lines in error file. This helps
#      catch embedded junk such as binary or emails in data. For cron
#      scripts is it very important to completely spell out all directory
#      path names.
#
# @author Linda Cully 2011-09-29
# @version DYNAMO_2011 created from the DYNAMO_2011 RV Sagar Kanya converter.
#    - Changed all references in code from Sagar Kanya and Male (Maldives)
#      for the DYNAMO 2011 project.
#
# This code makes the following assumptions:
#
#  - That the raw data file names shall be in the form
#        "43555_yyyymmddhhmm.tsv" where yyyy = year, mm = month, dd = day, hh=hour,
#         mm = minute. 43555 is the call sign for Male Maldives.
#  - That the raw data is in the Vaisala "Digicora 3" format. The file contains
#         header info on lines 1-39. Actual data starts on line 40. 
#  - That the incoming raw data are located in the 
#         /net/iftp2/pub/incoming/dynamo/male directory.
#  - That the following directories exist: ../output, ../archive.
#
# Note the following:
#  - This code computes the ascension rate, even though this is not required. 
#  - Samples received did NOT contained a blank line at the end of the data. But
#      blank lines at end of file are ignored.
#
# BEWARE:  Search for "HARDCODED" to find project-specific items that may
#          need to updated.
#
# Cron info: This task will be run as a cron script. As of approx. 30 Sept 2011, 
#     SL will add to his merlot/local crontab to run this script frequently (every 10 mins)
#     To edit (using vi) the crontab do "crontab -e". Add in the following
#     single line to have this script run every 10 mins on the 10's (e.g., 01:10,
#     01:20, 01:40, 01:50, etc.).
#
#     */10 * * * * /net/work/Projects/dynamo/upper_air/Male_Maldives/production/
#     software/MaleSounding_converter.pl > /dev/null 2>&1   
#
#     To comment out a line in the crontab, place a # in front of the line.
#
#     The /dev/null 2>$1 is added to dump any output messages to the bit bucket.
#     Removing this will cause cron messaging to be active. 
#
#     Note that this executes the production version of the s/w which has the 
#     full path names for all dirs included. Not the test/processing version.
#
# BEWARE: This script runs under S. Loehrer's crontab. It can only be changed
#     through his login or by SIG. 
#
##Module------------------------------------------------------------------------
package MaleSounding_converter;
use strict;

use Mail::Mailer;

if (-e "/net/work") {
    use lib "/net/work/lib/perl/Utilities";
    use lib "/net/work/lib/perl/UpperAir";
} else {
    use lib "/work/lib/perl/Utilities";
    use lib "/work/lib/perl/UpperAir";
}
 
use ClassConstants qw(:DEFAULT);
use ClassHeader;
use ClassRecord;

my ($OUT);
my ($ERROUT);

my $filename;
my $errfileName;
my $outfileName;

my $errText= "";

my $rawDirName;
my $outputDirName;
my $archiveDirName;
my $gtsDirName;

my $TotalRecProc = 0;
my $dataRecProc = 0;

my $debug = 0;
my $debug2 = 0;

if ($debug2) {printf "\nMaleSounding_converter.pl began on ";print scalar localtime;printf "\n";}
&main();
if ($debug2) {printf "\nMaleSounding_converter.pl ended on ";print scalar localtime;printf "\n";}

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the Male Maldives radiosonde data by converting it from 
# the native ASCII format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = MaleSounding_converter->new();
    $converter->convert();
} # main

##------------------------------------------------------------------------------
# @signature MaleSounding_converter new()
# <p>Create a new instance of a MaleSounding_converter.</p>
#
# @output $self A new MaleSounding_converter object.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);
    
    # HARDCODED
    $self->{"PROJECT"} = "DYNAMO";
    $self->{"NETWORK"} = "43555";
   
    #----------------------------------------
    # Input/ingest directory. Data to convert
    # WAS:  $self->{"RAW_DIR"} = "../raw_data";
    #----------------------------------------
### $rawDirName = "/net/work/Projects/dynamo/upper_air/Male_Maldives/production/raw_data";      # test area
    $rawDirName = "/net/iftp2/pub/incoming/dynamo/male";

    $self->{"RAW_DIR"} = $rawDirName;

    #---------------------
    # Output directories. 
    #---------------------
    $outputDirName = "/net/work/Projects/dynamo/upper_air/Male_Maldives/production/output";     # working dir
    $archiveDirName = "/net/work/Projects/dynamo/upper_air/Male_Maldives/production/archive";   # archive copy of all CLS and log files

### $gtsDirName = "/net/work/Projects/dynamo/upper_air/Male_Maldives/production/gts";          # test area

    $gtsDirName =  "/h/eol/iss/project/dynamo/data/male/cls";  # Final GTS area to place converted *.cls file

    $self->{"OUTPUT_DIR"} = $outputDirName;
    $self->{"ARCHIVE_DIR"} = $archiveDirName;
    $self->{"GTS_DIR"} = $gtsDirName;

    return $self;
} # new


##------------------------------------------------------------------------------
# @signature void convert()
# <p>Convert the raw data to the ESC format.</p>
##------------------------------------------------------------------------------
sub convert {
    my ($self) = @_;
    
    mkdir($self->{"OUTPUT_DIR"}) unless (-e $self->{"OUTPUT_DIR"});

    $self->readDataFiles();
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
    if ($debug2) {printf("parsing header for %s\n",$filename);}
    $header->setReleaseDirection("Ascending");

    # Set the type of sounding
    $header->setType("CLASS");
    $header->setProject($self->{"PROJECT"});
    
    # HARDCODED
    # The Id will be the prefix of the output file
    $header->setId("43555");

    # Site info received from Scot
    # "Release Site Type/Site ID:" header line
    $header->setSite("43555;Male, Maldives");

    # ------------------------------------------------
    # Read through the file for additional header info
    # ------------------------------------------------
    my $index = 0;
    foreach my $line (@headerlines) 
       {
       if ($debug2) {print "parseHeader:: (index = $index); line: xxx $line xxx \n";}

       # -----------------------------------------------------------
       # Add the non-predefined header lines to the header.
       # Changed $i to $i-1 to remove extra blank line from header. 
       # for (my $i = 6; $i < 11; $i++) 
       # -----------------------------------------------------------
       if (($index > 0) && ($index < 11))
          {
          if ($line !~ /^\s*\/\s*$/) 
             {
             if ($line =~ /RS-Number/i)
                {
                chop ($line); chop ($line); # Trim control M/EOL char

                my ($label,@contents) = split(/:/,$line);
                $label = "Sonde Id/Sonde Type";
                $contents[1] = "Vaisala";

                $header->setLine(($index-1), trim($label).":",trim(join("/",@contents)));
                } # RS-Number
             } #
           } # index/line 1-10

       #--------------------------------------
       # Ignore the rest of the header lines.
       #--------------------------------------
       if ($index < 39) 
          { 
          if ($debug) {print "If index < 39...processed header line. index = $index\n"} 

          $index++; 
          next; 
          } # Header lines 1-39

       #--------------------------------------------------------------
       # Process the DATA lines starting at line 40 in the raw data.
       #--------------------------------------------------------------
       # Find the lat/lon for the release location in the actual data.
       #---------------------------------------------------------------
       else # Data lines
          {
          my @data = split(' ',$line);

          if ($debug2) {print "data: @data\n";}

          if (($data[14] > -32768) & ($data[15] > -32768)) 
             {
             #--------------------------------------------------------------
             # Format length must be the same as the value length or
             # convertLatLong will complain (see example below)
             #
             # base lat   = 36.6100006103516    base lon = -97.4899978637695
             # Lat format = DDDDDDDDDDDDDDDD  Lon format = -DDDDDDDDDDDDDDDD  
             #--------------------------------------------------------------
             #----------
             # Longitude
             #----------
             my $lon_fmt = $data[14] < 0 ? "-" : "";
             while (length($lon_fmt) < length($data[14])) 
                { 
                $lon_fmt .= "D"; 
                }

             if ($data[14] != -32768) {$header->setLongitude($data[14],$lon_fmt);}

             #----------
             # Latitude
             #----------
             my $lat_fmt = $data[15] < 0 ? "-" : "";
             while (length($lat_fmt) < length($data[15])) 
                { 
                $lat_fmt .= "D"; 
                }

             if ($data[15] != -32768) {$header->setLatitude($data[15],$lat_fmt);}
 
             #----------
             # Altitude
             #----------
             if ($data[6] != -32768) {$header->setAltitude($data[6],"m");} 

             last;
             } #data[14/15] > -32768 - Not MISSING

          } # Data lines
       } # foreach line

    # ----------------------------------------------------------
    # Extract the ACTUAL RELEASE date and time information from the file name
    # Expect file name structure: 43555_yyyymmddhhmm.tsv .                     HARDCODED
    # ----------------------------------------------------------
    if ($debug) { print "file name = $filename\n"; }

    my $date;
    my $time;

    if ($filename =~ /(\d{4})(\d{2})(\d{2})(\d{2})(\d{2})/)
      {
      my ($yearInfo, $monthInfo, $dayInfo, $hourInfo, $minInfo) = ($1,$2,$3,$4,$5);

      $date = join ", ", $yearInfo, $monthInfo, $dayInfo;
      $time = join "", $hourInfo, ' ', $minInfo, ' 00';

      if ($debug) {print "date is $date\n";print "time is $time\n";}

      } # Pull date and time from filename

    $header->setActualRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
    $header->setNominalRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);

    return $header;
} # parseHeader()
                           
##------------------------------------------------------------------------------
# @signature void parseRawFile(String file)
# <p>Read the data from the specified file and convert it to the ESC format.</p>
#
# @input $file The name of the file to be parsed.
##------------------------------------------------------------------------------
sub parseRawFile {
    my ($self,$file) = @_;

    my $altMissCt = 0;
    my $pressLess850 = 0;
    my $PNoMiss = 0;
   
    my $lonMiss = 0; my $latMiss = 0;
    my $TNoMiss = 0; my $TdNoMiss = 0; my $RHNoMiss = 0;
    my $UNoMiss = 0; my $VNoMiss = 0;
    my $WspNoMiss = 0; my $WdirNoMiss = 0;

    if ($debug2) {printf("\n********************\nProcessing file: %s\n",$rawDirName."/".$file); }

    open(my $FILE,$self->{"RAW_DIR"}."/".$file) or die("Can't open file: ".$file);
    my @lines = <$FILE>;
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
    my @file_info = stat $FILE;

    if ($debug2) 
       {
       print "file_info:: @file_info\n"; 
       print "number_lines_in_file:: $number_lines_in_file \n";
       print "File size = $file_info[7]\n";
       }

    close($FILE);

    #---------------------------------------------------
    # Open Error Log/Warning file. If there are any
    # errors, then the file being processed can not
    # be put on the GTS so don't move to ASPEN/GTS area.
    #----------------------------------------------------
    my @namepart = split(".tsv", $file);
    $errfileName = $namepart[0].".errlog";

    if ($debug2) {print "Error Log file name is $outputDirName/$errfileName\n"; }

    open($ERROUT,">".$self->{"OUTPUT_DIR"}."/".$errfileName) or 
             die("Can't open Error Log file $errfileName for input file $file\n");
    
    #------------------------------
    # Generate the sounding header.
    #------------------------------
    my @headerlines = @lines;

    my $header = $self->parseHeader($file,@headerlines);
    
    #-----------------------------------------------------------
    # Only continue processing the file if a header was created.
    #-----------------------------------------------------------
    if (defined($header)) 
       {
       # ----------------------------------------------------
       # Create the output file name and open the output file
       # ----------------------------------------------------
       my ($hour, $min, $sec) = split (/:/, $header->getActualTime());

       $outfileName = sprintf("%s_%04d%02d%02d%02d%02d.cls", 
                        $header->getId(),
                        split(/,/,$header->getActualDate()),
                        $hour, $min);
 
       if ($debug2) {printf("\tOutput file name is %s\n", $outputDirName."/".$outfileName); }

       open($OUT,">".$self->{"OUTPUT_DIR"}."/".$outfileName)
             or die("Can't open output file for $file\n");

       print($OUT $header->toString());
   
       # ----------------------------------------
       # Needed for code to derive ascension rate
       # ----------------------------------------
       my $prev_time = 9999.0;
       my $prev_alt = 99999.0;

       # ----------------------------------------------------
       # Parse the data portion of the input file
       # ----------------------------------------------------
       my $index = 0;

       foreach my $line (@lines)  
          {
          $lonMiss = 0;
          $latMiss = 0;
          $TotalRecProc++;

          if ($TotalRecProc >= 40)
             {
             $dataRecProc++;
             } # Keep track of data lines

          if ($debug2) {print "TotalRecProc: $TotalRecProc, dataRecProc: $dataRecProc\n";}

          #--------------------------------------------
          # Ignore the header lines and blank lines.
          # Note that there tends to be a blank line
          # at the end of the data. Skip that line, too.
          #---------------------------------------------
          if ($debug2) {print "line: $line\n";}

          if ($debug) {if ($line =~ /^\s*$/) {print "Line is empty. line = xxx $line xxx\n";} }

          if ($index < 39 || $line =~ /^\s*$/)   # HARDCODED to skip first 39 lines Plus any blank lines
             { 
             $index++; next; 
             }
       
          #-------------------------------------------------------------
          # Check for blank line particularly at end of file in Male data.
          # if missing or null or \n or line length not as expected....
          #-------------------------------------------------------------
          my @data = split(' ',$line);
          my $record = ClassRecord->new($ERROUT,$file);  

          #---------------------------------------------------
          # missing values are -32768 - Assumption! HARDCODED
          #---------------------------------------------------
          $record->setTime($data[0]);

          $record->setPressure($data[7],"mb") if ($data[7] != -32768);
          if (($data[7] <= 850.00) && ($data[7] != -32768) ) {$pressLess850++;}
          if ($data[7] != -32768) {$PNoMiss++;}

          #---------------------------------------------------------
          # $record->setTemperature($data[2],"C") if ($data[2] != -32768);    
          # Temp and Dewpt are in Kelvin.  C = K - 273.15
          #---------------------------------------------------------
          $record->setTemperature(($data[2]-273.15),"C") if ($data[2] != -32768);    
          if ($data[2] != -32768) {$TNoMiss++;};

          $record->setDewPoint(($data[8]-273.15),"C") if ($data[8] != -32768);
          if ($data[8] != -32768) {$TdNoMiss++;};

          $record->setRelativeHumidity($data[3]) if ($data[3] != -32768);
          if ($data[3] != -32768) {$RHNoMiss++;};

          $record->setUWindComponent($data[5],"m/s") if ($data[5] != -32768);
          if ($data[5] != -32768) {$UNoMiss++;};

          $record->setVWindComponent($data[4],"m/s") if ($data[4] != -32768);
          if ($data[4] != -32768) {$VNoMiss++;};

          $record->setWindSpeed($data[11],"m/s") if ($data[11] != -32768);
          if ($data[11] != -32768) {$WspNoMiss++;};

          $record->setWindDirection($data[10]) if ($data[10] != -32768);
          if ($data[10] != -32768) {$WdirNoMiss++;};

          #--------------------------------------------------
          # get the lat/lon data. MISSING value = -32768
          #--------------------------------------------------
          if ($data[14] != -32768) 
             {
             my $lon_fmt = $data[14] < 0 ? "-" : "";

             while (length($lon_fmt) < length($data[14])) { $lon_fmt .= "D"; }
             $record->setLongitude($data[14],$lon_fmt);
             } # if lon not missing
          else
             {
             $lonMiss = 1;
             }

          if ($data[15] != -32768) 
             {
             my $lat_fmt = $data[15] < 0 ? "-" : "";

             while (length($lat_fmt) < length($data[15])) { $lat_fmt .= "D"; }
             $record->setLatitude($data[15],$lat_fmt);
             } # if lat not missing
          else
             {
             $latMiss = 1;
             }

          if ($debug2) {print "Latitude:: $data[15] , Longitude:: $data[14], TotalRecProc: $TotalRecProc, dataRecProc:: $dataRecProc \n";}

          #----------------------------------------------------------
          # Insert Ele (Elevation Angle) and Azi (Azimuth Angle) data
          # For setVariableValue(index, value):  
          # index (1) is Ele column, index (2) is Azi column.
          #----------------------------------------------------------
          $record->setVariableValue(2, $data[12]) if ($data[12] != -32768);
          $record->setAltitude($data[6],"m") if ($data[6] != -32768);     # AKA Height in raw data.

          if ($data[6] == -32768.00)
             {
             $altMissCt++;
             if ($debug2) { print "Missing height, altMissCt:: $altMissCt\n"; }
             }
                                             
          #-------------------------------------------------------
          # Following calc of asc rate not required for Male Maldives,
          # but S. Loehrer says OK to leave in.
          #-------------------------------------------------------
          # Calculate the ascension rate which is the difference
          # in altitudes divided by the change in time. Ascension
          # rates can be positive, zero, or negative. But the time
          # must always be increasing (the norm) and not missing.
          #
          # Only save off the next non-missing values.
          # Ascension rates over spans of missing values are OK.
          #-------------------------------------------------------
          if ($debug) 
             { 
             my $time = $record->getTime(); 
             my $alt = $record->getAltitude(); 
             print "\nNEXT Line: prev_time, rec Time, prev_alt, rec Alt:: $prev_time, $time, $prev_alt, $alt\n";
             }

          if ($prev_time != 9999  && $record->getTime()     != 9999  &&
              $prev_alt  != 99999 && $record->getAltitude() != 99999 &&
              $prev_time != $record->getTime() ) 
             {
             $record->setAscensionRate( ($record->getAltitude() - $prev_alt) /
                                        ($record->getTime() - $prev_time),"m/s");

             if ($debug) { print "Calc Ascension Rate.\n"; }
             } # If input non-missing, calc asc. rate.

          #-----------------------------------------------------
          # Only save off the next non-missing values. 
          # Ascension rates over spans of missing values are OK.
          #-----------------------------------------------------
          if ($debug) 
             { 
             my $rectime = $record->getTime(); my $recalt = $record->getAltitude();
             if ($debug) {print "Try SAVE Line: rec Time, rec Alt:: $rectime, $recalt\n";  }
             }

          if ($record->getTime() != 9999 && $record->getAltitude() != 99999)
             {
             $prev_time = $record->getTime();
             $prev_alt = $record->getAltitude();

             if ($debug) { print "Current rec has valid Time and Alt. Save as previous.\n"; }
             } # save next non-missing vals

          #-----------------------------------
          # Completed the ascension rate data
          #-----------------------------------
          printf($OUT $record->toString());

          #------------------------------------------------------
          # If the lat and lon are missing from the first record
          # then this is a significant error so DO NOT send this
          # sounding on to GTS. That is, create and keep an error log.
          #------------------------------------------------------
          if ($debug2) {print "Latitude MISS:: $lonMiss , Longitude MISS:: $lonMiss, TotalRecProc: $TotalRecProc, dataRecProc:: $dataRecProc \n";}

          if (($TotalRecProc == 40) && ($lonMiss) && ($latMiss)) # HARDCODED - Data begins on line 40
             {
             if ($debug2) {print "ERROR:: Latitude and Longitude of first data record are MISSING!\n";}
             $errText =  $errText."ERROR:: Latitude and Longitude of first data record are MISSING!\n";

             print  $ERROUT "ERROR:: Latitude and Longitude of first data record are MISSING!\n";
             } # lat/lon missing from first data rec

          } # foreach data line

       if ($debug2) {print "Processing total, data line number:: $TotalRecProc, $dataRecProc\n";} 

       } # successfully made header, process data
   else
      {
      if ($debug2) {printf("Unable to make a header\n");}
      $errText =  $errText."ERROR:: Unable to make header.\n";

      print  $ERROUT "ERROR:: Unable to make header.\n";
      } # Could not make header


   #---------------------------------------------------
   # If all altitudes are missing in data, then write
   # error to error log and this sounding should not
   # be passed to GTS.
   #---------------------------------------------------
   if (($dataRecProc >= 0) && ($dataRecProc == $altMissCt))
      {
      if ($debug2) {printf("All Altitudes/Heights are MISSING in data records.\n");}
      $errText =  $errText."ERROR:: All Altitudes/Heights are MISSING in data records.\n";
      print  $ERROUT "ERROR:: All Altitudes/Heights are MISSING in data records.\n";
      }

   if ( ($TotalRecProc == 0))
      {
      if ($debug2) {printf("ERROR:: No header lines AND No data records. EMPTY FILE!\n");}
      $errText =  $errText."ERROR:: No header lines AND No data records. EMPTY FILE!\n";
      print  $ERROUT "ERROR:: No header lines AND No data records. EMPTY FILE!\n";
      }

   if ($dataRecProc == 0)
      {
      if ($debug2) {printf("ERROR:: No data records.\n");}
      $errText =  $errText."ERROR:: No data records.\n";
      print  $ERROUT "ERROR:: No data records.\n";
      }

   #---------------------------------------------------
   # If pressure is never less than or equal to 850MB,
   # then do not put this sounding on GTS.
   #---------------------------------------------------
   if (($pressLess850 < 1) && ($PNoMiss > 0))
      {
      if ($debug2) {print "ERROR:: Pressure is never less than 850.00 MB.\n";}
      $errText =  $errText."ERROR:: Pressure is never less than 850.00 MB.\n";
      print  $ERROUT "ERROR:: Pressure is never less than 850.00 MB.\n";
      }

   if ($PNoMiss < 1)
      {
      if ($debug2) {print "ERROR:: All Pressure values are MISSING.\n";}
      $errText =  $errText."ERROR:: All Pressure values are MISSING.\n";
      print  $ERROUT "ERROR:: All Pressure values are MISSING.\n";
      }

   #---------------------------------------------------
   # If  all the data are missing, this sounding should
   # not be put on the GTS.
   #---------------------------------------------------
   my $allData = $TNoMiss + $TdNoMiss + $RHNoMiss + $UNoMiss + $VNoMiss + $WspNoMiss + $WdirNoMiss;
   if ($debug) {print "allData count:: $allData\n";}

   if ($allData == 0)
      {
      if ($debug2) {printf("ERROR:: No valid data on any records. Possibly no header lines.\n");}
      $errText =  $errText."ERROR:: No valid data on any records. Possibly no header lines.\n";
      print  $ERROUT "ERROR:: No valid data on any records. Possibly no header lines.\n";
      }

    close($ERROUT);
    close($OUT);

   } # parseRawFile()


##------------------------------------------------------------------------------
# @signature void readDataFiles()
# <p>Read in the files from the raw data directory and convert each into an
# ESC formatted file.</p>
##------------------------------------------------------------------------------
sub readDataFiles {
    my ($self) = @_;
    
    my $cmd = "\n";

    opendir(my $RAW,$self->{"RAW_DIR"}) or die("Can't read raw directory ".$self->{"RAW_DIR"});

    #------------------------------------------------------------
    # Input file names must be of the form: 43555_yyyymmddhhmm.tsv         HARDCODED 
    # where 43555 is the call sign for the Male Maldives, yyyy = year,
    # mm = month, dd = day, hh = hour, mm = minute, and "tsv" is the
    # suffix. This is the exact form. All files with names of this
    # form will be processed.
    #------------------------------------------------------------
    my @files = grep(/^43555_\d{12}\.tsv/,sort(readdir($RAW)));

    closedir($RAW);
    
    foreach my $input_file (@files) 
       {
       if ($debug2) {print "Bef - TotalRecProc: $TotalRecProc, dataRecProc: $dataRecProc\n";}

       $self->parseRawFile($input_file);   # Process each data file!

       if ($debug2) {print "Aft - TotalRecProc: $TotalRecProc, dataRecProc: $dataRecProc\n";}

       #----------------------------------------------------
       # If there are any lines in the Error log then do NOT
       # copy the converted CLASS file to the GTS area.
       #
       # If the error log is empty, then copy the CLASS file
       # to the GTS area for ASPEN QC and GTS submittal.
       #
       # Either way, always move the original ingest file,
       # the *.cls file, and *.errlog file to the archive
       # directory.  Leave the ingest directory empty of
       # these files.
       #----------------------------------------------------
       open($ERROUT,"<".$self->{"OUTPUT_DIR"}."/".$errfileName) or
                die("Can't open Error Log file $errfileName\n");

       my @err_lines = <$ERROUT>;
       my $number_lines_in_ERROR_file = $#err_lines+1;

       #------------------------------------------------------------
       # ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,
       #  $atime,$mtime,$ctime,$blksize,$blocks) = stat($filename);
       #------------------------------------------------------------
       my @errfile_info = stat $ERROUT;
       close ($ERROUT);

       if ($debug2)
          {
          print "errfile_info:: @errfile_info\n"; 
          print "err_lines:: $#err_lines\n";
          print "number_lines_in_ERROR_file:: $number_lines_in_ERROR_file \n";
          print "ERROR File size = $errfile_info[7]\n";
          }

       if ($#err_lines < 0)   # NO errors during processing! Copy Class file to GTS.
          {
          if ($debug2) {print "NO errors, copy class file to GTS area.\n";}

          $cmd = sprintf ("cp  %s/%s %s/.", $outputDirName, $outfileName, $gtsDirName);
          if ($debug2) {print "COPY CLASS to GTS. Issue cmd:: $cmd\n";}

          system "$cmd ";

          }
       else
          { if ($debug2) {print "THERE ARE ERRORS, don't move class file to GTS area.\n";} } # Error during processing

       #----------------------------------------------------------
       # Move files to archive area to prep for next incoming file
       # Need to chmod on files??
       #----------------------------------------------------------
       $cmd = sprintf ("mv %s/%s %s/.", $rawDirName, $input_file, $archiveDirName);
       if ($debug2) {print "ARCHIVE(mv) RAW Input data. Issue cmd:: $cmd\n";}

       system "$cmd ";

       $cmd = sprintf ("mv %s/%s %s/.", $outputDirName, $outfileName, $archiveDirName);
       if ($debug2) {print "ARCHIVE(mv) CLASS file. Issue cmd:: $cmd\n";}

       system "$cmd ";

       $cmd = sprintf ("mv  %s/%s %s/.", $outputDirName, $errfileName, $archiveDirName);
       if ($debug2) {print "errfileName: $errfileName\n";}
       if ($debug2) {print "ARCHIVE(mv) ERROR Log. Issue cmd:: $cmd\n";}

       system "$cmd ";

       #------------------------------------------------------
       # Send email saying what has been processed.
       # 
       # Add sfw\@ucar.edu and loehrer\@ucar.edu to the 
       # "addressto" for production.
       #------------------------------------------------------
       my $addressfrom = sprintf "%s", "cully\@ucar.edu";                                   #HARDCODED EMAIL

       my $addressto = sprintf "%s", "cully\@ucar.edu, loehrer\@ucar.edu, sfw\@ucar.edu";   #HARDCODED EMAIL
##     my $addressto = sprintf "%s", "cully\@ucar.edu";                                     #HARDCODED EMAIL

       my $errlength = length($errText);
       my $message;

       if ($errlength > 0 || $number_lines_in_ERROR_file > 0)
          { $message = "Male, Maldives Sounding $input_file Processed - ERROR"; }
       else
          { $message = "Male, Maldives Sounding $input_file Processed"; }

       if ($debug2) {print "email (from;to):: $addressfrom ; $addressto. \n";}

       my $mailer = Mail::Mailer->new();
       $mailer->open({
           From => $addressfrom,
           To => $addressto,
           Subject => $message,
           })
           or die "Can't open $!\n";

           if ($debug2) {print "errlength:: $errlength. \n";}

           if ($errlength > 0 || $number_lines_in_ERROR_file > 0)
              {
              print $mailer "The following Male Maldives data file has been processed:\n $rawDirName/$input_file .\n\nThis file contained $TotalRecProc total records and $dataRecProc data records.\n\nAll files (*.tsv, *.cls, *.errlog) have been moved to the \narchive area at  $archiveDirName .\n\nERROR MESSAGES::\n$errText \nBeware that junky input files may cause unexpected results. Review the *.errlog file for more ERROR information.\n\nThere were errors creating the Class file, so the class file WAS NOT copied \nto the GTS directory at $gtsDirName\n";
              }
           else
              {
              print $mailer "The following Male Maldives data file has been processed:\n $rawDirName/$input_file .\n\nThis file contained $TotalRecProc total records and $dataRecProc data records.\n\nAll files (*.tsv, *.cls, *.errlog) have been moved to the \narchive area at  $archiveDirName .\n\n$errText \nClass file created and copied to GTS directory at $gtsDirName\n";
              }

       $mailer->close();

       $TotalRecProc = 0;
       $dataRecProc = 0;

       $errText = "";

       } # process each file 


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
