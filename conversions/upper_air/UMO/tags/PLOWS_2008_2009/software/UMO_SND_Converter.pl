#! /usr/bin/perl -w

##Module-------------------------------------------------------------------------
# <p>The UMO_SND_Converter script is used for converting radiosonde data
# from the University of Missouri into the EOL Sounding Composite (ESC) format.</p>
# This s/w computes the geopotential altitude.
#
# @author L. Cully
# @version PLOWS 2008-2009 This was originally created for the PLOWS 2008-2009 project.
#          This version 0.0 created June 2009 and finalized in August 2009 when complete
#          dataset was received from source.
#
# BEWARE: This s/w assumes the raw data will be in /raw_data/*.PYS  or *.pys files.
# BEWARE: Update all hardcoded values. Check the code carefully for these. Search
#         for the word "BEWARE" within the code.
##Module-------------------------------------------------------------------------
package UMO_SND_Converter;

use strict;

if (-e "/net/work/") {
    use lib "/net/work/lib/perl/Utilities";
    use lib "/net/work/lib/perl/Station";
    use lib "/net/work/lib/perl/UpperAir";
} else {
    use lib "/work/lib/perl/Utilities";
    use lib "/work/lib/perl/Station";
    use lib "/work/lib/perl/UpperAir";
}

use DpgDate qw(:DEFAULT);

use ClassHeader;
use ClassRecord;
use DpgCalculations;

use ElevatedStationMap; 
use Station;

$| = 1;

my ($WARN);
my $debug = 0;

&main();

#---------------------------------------------------------
# BEWARE: A collection of functions that contain constants
#---------------------------------------------------------
sub getNetworkName { return "UMO"; }
sub getOutputDirectory { return "../output"; }
sub getProjectName { return "PLOWS_2008-2009"; }
sub getRawDirectory { return "../raw_data"; }

sub getStationFile { return sprintf("../final/%s_%s_sounding_stationCD.out",
				    getNetworkName(),getProjectName()); }
sub getStationList { return "../docs/station.list"; }
sub getSummaryFile { return "../output/station_summary.log"; }
sub getWarningFile { return "../output/warning.log"; }

####################### MAIN ###################################################
##------------------------------------------------------------------------------
# @signature void main()
# <p>Execute the conversion of the data.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = UMO_SND_Converter->new();
    
    if ($debug){ print "Begin UMO_SND_Converter().\n";}
    $converter->convert();
} # main


##------------------------------------------------------------------------------
# @signature UMO_SND_Converter new()
# <p>Create a new UMO_SND_Converter instance.</p>
#
# @output $converter The new converter.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);

    $self->{"stations"} = ElevatedStationMap->new();

    return $self;

} # UMO_SND_Converter new()


##------------------------------------------------------------------------------
# @signature void convert()
# <p>Convert all of the raw data and create the output files for the conversion.</p>
##------------------------------------------------------------------------------
sub convert {
    my $self = shift;

    mkdir(getOutputDirectory()) unless (-e getOutputDirectory());
    mkdir("../final") unless (-e "../final");

    if ($debug){ print "readRawDataFiles().\n"; }
    $self->readRawDataFiles();

    if ($debug){ print "Convert: printStationFiles().\n"; }
    $self->printStationFiles();
} # convert()

######################### Functions ################################################

##---------------------------------------------------------------------------
# @signature void printStationFiles()
# <p>Generate the stationCD.out file and the station summary log for the stations
# in the conversion.</p>
##---------------------------------------------------------------------------
sub printStationFiles {
    my $self = shift;
    my ($STN, $SUMMARY);

    if ($debug){ print "Enter printStationFiles().\n"; }

    open($STN, ">".$self->getStationFile()) || die("Cannot create the ".$self->getStationFile()." file\n");

    foreach my $station ($self->{"stations"}->getAllStations()) {
        print($STN $station->toString()) if ($station->getBeginDate !~ /^9+$/);
    }

    close($STN);

    open($SUMMARY, ">".$self->getSummaryFile()) || die("Cannot create the ".$self->getSummaryFile()." file.\n");
    print($SUMMARY $self->{"stations"}->getStationSummary());
    close($SUMMARY);

} # printStationFiles()

##------------------------------------------------------------------------------
# @signature int getMonth(String abbr)
# <p>Get the number of the month from an abbreviation.</p>
#
# @input $abbr The month abbreviation.
# @output $mon The number of the month.
# @warning This function will die if the abbreviation is not known.
##------------------------------------------------------------------------------
sub getMonth {
    my $self = shift;
    my $month = shift;

    if ($month =~ /MAR/i) { return 3; }
    elsif ($month =~ /APR/i) { return 4; }
    elsif ($month =~ /MAY/i) { return 5; }
    elsif ($month =~ /JUN/i) { return 6; }
    elsif ($month =~ /JUL/i) { return 7; }
    elsif ($month =~ /AUG/i) { return 8; }
    elsif ($month =~ /SEP/i) { return 9; }
    elsif ($month =~ /OCT/i) { return 10; }
    else { die("Unknown month: $month\n"); }

} # getMonth()

##------------------------------------------------------------------------------
# @signature String trim(String line)
# <p>Remove the leading and trailing whitespace around a String.</p>
#
# @input $line The String to be trimmed.
# @output $line The trimmed line.
##------------------------------------------------------------------------------
sub trim {
    my $line = shift;
    $line =~ s/^\s+//g;
    $line =~ s/\s+$//g;
    return $line;
} # String trim()


######################### Read Files ###########################################

##------------------------------------------------------------------------------
# @signature void readRawDataFiles()
# <p>Determine all of the raw data files that need to be processed and then
# process them.</p>
##------------------------------------------------------------------------------
sub readRawDataFiles {
    my $self = shift;
    my $file_count = 0;

    opendir(my $RAW,getRawDirectory()) or die("Cannot read raw data directory.\n");
    my @files = grep(/\.PYS$/i,(sort(readdir($RAW))));
    closedir($RAW);

    if ($#files < 0) 
       { print "   WARNING: readRawDataFiles() -  No files in the input directory.\n"; } 

    open($WARN,">".$self->getWarningFile()) or die("Can't open warning file.\n");

    foreach my $file (@files) {
        if ($debug){ print "readRawDataFiles() - reading $file.\n";}
        $self->readRawFile($file);
    }

    close($WARN);

} # readRawDataFiles()


##------------------------------------------------------------------------------
# @signature void readRawFile(String file_name)
# <p>Convert the specified file into the CLASS format.</p>
#
# @input $file_name The name of the raw data file to be converted.
##------------------------------------------------------------------------------
sub readRawFile {
    my $self = shift;
    my $file_name = shift;
    my $file = sprintf("%s/%s",$self->getRawDirectory(),$file_name);
    my $previous_record;

    if ($debug){ printf("readRawFile() - Processing file: %s\n",$file_name); }
    printf("Processing file: %s\n",$file_name);

    open(my $FILE,$file) or die("Cannot open file: $file\n");

    #---------------------------------------------
    # Process the header and print to output file.
    #---------------------------------------------
    my $first_record = ClassRecord->new($WARN,$file_name,$previous_record);
    my $header = $self->read_header($FILE,$file_name, $first_record);

    return if (!defined($header));

    my $outfile = sprintf("%s/%s_%04d%02d%02d%02d%02d%02d.cls",$self->getOutputDirectory(),
                          "UMO",split(", ",$header->getActualDate()),
                          split(":",$header->getActualTime()));

    open(my $OUT,sprintf(">%s",$outfile)) or die("Cannot open $outfile\n");
    print($OUT $header->toString());

    #----------------------------------------------------
    # Print the First Record. Info from Header metadata.
    #----------------------------------------------------
    if ($debug){print "readRawFile: print first record\n"; }

    print($OUT $first_record->toString()) if defined($first_record);
    $previous_record = $first_record;


    #---------------------------------------------
    # Process all data record in the file and
    # write to output.
    #---------------------------------------------
    if ($debug){ print "readRawFile: process data records\n"; }

    while (my $line = <$FILE>)
        {
        if ($debug){ print "readRawFile:: File line: $line"; }

        chop($line);  chop($line);  # remove \r\n from each line.
        my $size = length($line);
        if ($debug){ print "readRawFile:: File line After chop: $line xxx length(line) = $size\n"; }
 
        if ( length ($line) > 0)
           {
           #-----------------------------------------
           # Do not include the raw rec with time of
           # 00:00:00 (or 0.0 secs). Instead include
           # info from header as the 0.0 time. Also,
           # skip junk header lines.
           #-----------------------------------------
           my @line_parts = split (" ", $line);
           my $record;

           if ($debug){ print "Line length is greater than zero = size. Try and create record.\n"; }
           if ($debug){ print "line_parts[0] = xxx $line_parts[0] xxx\n"; }

           if ($line_parts[0] ne "00:00:00" &&    # Skip time 0.0 sec, replace with hdr info for this time.
               $line_parts[0] ne "PHYSICAL"  &&
               $line_parts[0] ne "Time"  && 
               $line_parts[0] ne "Sec."  &&
               $line_parts[0] ne "--------------------------------------------------------------------------" )
              {
              if ($debug){ print "create record - process line.\n"; }

              $record = $self->create_record($line,$header,$file_name,$previous_record);
              print($OUT $record->toString()) if defined($record);

              #---------------------------------------------
              # Only save off current rec as previous rec
              # if not completely missing. This affects
              # the calculations of the geopotential height,
              # as the previous height must be non-missing. 
              # Create an "isMissingRec" fn and add to 
              # ClassRecord.pm library, then update following.
              #---------------------------------------------
              if ($debug) 
                 {
                 my $press = $record->getPressure(); my $temp = $record->getTemperature();
                 my $dewpt = $record->getDewPoint(); my $rh = $record->getRelativeHumidity();
                 my $uwind = $record->getUWindComponent(); my $vwind = $record->getVWindComponent();
                 my $wsp   = $record->getWindSpeed(); my $wdir = $record->getWindDirection();
                 my $ascrt = $record->getAscensionRate(); my $alt = $record->getAltitude();
                 my $lon   = $record->getLongitude(); my $lat = $record->getLatitude();

                 print "Current Rec: press = $press, temp = $temp, dewpt = $dewpt, rh = $rh, uwind = $uwind, vwind = $vwind, wsp = $wsp , wdir = $wdir, ascrt = $ascrt, alt = $alt, lon = $lon, lat = $lat\n";
                 }

              if (
                 $record->getPressure() < 9999.0       && $record->getTemperature() < 999.0      &&
                 $record->getDewPoint() <  999.0       && $record->getRelativeHumidity() < 999.0 &&
                 $record->getUWindComponent() < 9999.0 && $record->getVWindComponent() < 9999.0  &&
                 $record->getWindSpeed() < 999.0       && $record->getWindDirection() < 999.0    &&
                 $record->getAscensionRate() < 999.0   && $record->getAltitude() < 99999.0     )
                 {
                 if ($debug) { print "Move to next record! previous_record = record \n\n";}
                 $previous_record = $record;
                 }
              else
                 {
                 if ($debug) { print "Do NOT assign current record to previous_record! Current record is completely missing. Don't save as previous record.\n\n";}
                 }
              }
           else
              {
              if ($debug){ print "SKIP data line: xxx $line xxx\n"; }
              }

           } #line length

         } # While

    close($OUT);
    close($FILE);

} # readRawFile()


######################### read_header  ######################################

##---------------------------------------------------------------------------
# @signature ClassHeader read_header(FileHandle FILE, String filename)
# <p>Create the header portion of the class formatted file.</p>
#
# @input $FILE The file handle containing the raw data.
# @input $filename The name of the raw data file.
# @output $header The ClassHeader for the file.
##---------------------------------------------------------------------------
sub read_header {
    my ($self,$FILE,$filename,$first_record) = @_;

    my $previous_record;   # NOT USED!!!
    my $station = Station->new();
    my $header = ClassHeader->new($station);
    my $id;
    my $date;

    if ($debug){ print "Enter read_header().\n"; }

    #----------------------------------------
    # Set the hardcoded values in the header.
    #----------------------------------------
    $header->setProject($self->getProjectName());

    $station->setNetworkName($self->getProjectName());
    $station->setReportingFrequency("no set schedule");

    #---------------------------------------------------
    # This is a mobile station. It was in various states
    # on different dates for the PLOWS project.
    #---------------------------------------------------
    $station->setStateCode("99");  
    $station->setNetworkIdNumber(99);
    $station->setPlatformIdNumber(99);
    $station->setLatLongAccuracy(2);

    #----------------------------------------------
    # Parse out the header lines.
    # Note that SL says to form the first data 
    # line from this info with a time of 0.0 sec.
    # Skip the 0.0 rec in the raw data. - NEW req.
    #----------------------------------------------
    my $line = <$FILE>;

    if ($debug){ print "Before while\n"; }

    while ($line ne "--------------------------------------------------------------------------\r\n") {

        if ($line eq "\r\n" || $line eq "\n" || $line eq " " || $line eq "") 
          {
          if ($debug){ print "Skip BLANK line.\n"; }
          $line = <$FILE>;
          next;
          }

         chop ($line);

        my @line_parts = split (' ', $line);

        if ($debug){ print "line_parts:: xxx @line_parts xxx\n"; }

        if ($line_parts[0] eq "PHYSICAL")
           {
           my @date_parts = split ('/', $line_parts[3]); # Raw date is day/month/year
           if ($debug){ print "date_parts:: @date_parts\n"; }

           $date = $date_parts[2].$date_parts[1].$date_parts[0]; #YYYYMMDD
           if ($debug){ print "date:: $date\n"; }

           my $time = $line_parts[4]; #HH:MM:SS
           if ($debug){ print "time = $time\n"; }

           $header->setActualRelease($date,"YYYYMMDD", $time,"HH:MM:SS",0);
           $header->setNominalRelease($date,"YYYYMMDD", $time,"HH:MM:SS",0);

           $first_record->setTime(0.0, 0.0); 
            }
        elsif ($line_parts[0] eq "LOCATION:")
           {
           #---------------------------------------------------
           # Set the Latitude. Raw lat is Deg.min. Not decimal.
           #---------------------------------------------------
           my $lat = $line_parts[4];
           my @lat_parts = split ('\'', $lat);

           my $lat_fmt = "DD.";
           $lat = $lat_parts[0];

           if ($debug){ print "lat_fmt: $lat_fmt,  lat = $lat\n"; }

           while (length($lat_fmt) < length($lat)) { $lat_fmt .= "M"; }

           if ($lat_parts[1] eq "S")
              { $lat = "-".$lat; $lat_fmt = "-".$lat_fmt; }

           if ($debug){ print "lat_fmt: $lat_fmt, lat = $lat\n"; }

           $first_record->setLatitude($lat,$lat_fmt);
           $station->setLatitude($lat,$lat_fmt);
           $header->setLatitude($lat,$lat_fmt);

           #------------------
           # Set the Longitude
           #------------------
           my $lon = $line_parts[5];
           my @lon_parts = split ('\'', $lon);

           my $lon_fmt = "DD.";
           $lon = $lon_parts[0];

           if ($debug){ print "lon_fmt: $lon_fmt, lon = $lon\n"; }

           while (length($lon_fmt) < length($lon)) { $lon_fmt .= "M"; }

           if ($lon_parts[1] eq "W")
              { $lon = "-".$lon; $lon_fmt = "-".$lon_fmt; }

           if ($debug){ print "lon_fmt: $lon_fmt, lon = $lon\n"; }

           $station->setLongitude($lon,$lon_fmt);
           $header->setLongitude($lon,$lon_fmt);
           $first_record->setLongitude($lon,$lon_fmt);
            }
        elsif ($line_parts[0] eq "PRESSURE")
           {
           $first_record->setPressure($line_parts[2],"hPa");
            }
        elsif ($line_parts[0] eq "HEIGHT")
           {
           $station->setElevation($line_parts[2],"m"); # raw in meters
           $header->setAltitude($line_parts[2],"m");
           $first_record->setAltitude($line_parts[2],"m");
            }
        elsif ($line_parts[0] eq "TEMPERATURE")
           {
           $header->setType("Univ. of Missouri Soundings"."/".$line_parts[7]." ".$line_parts[8]);
           $header->setReleaseDirection("Ascending");
	   $header->setLine(6, "Radiosonde Manufacturer:",$line_parts[7]." ".$line_parts[8]);
           $first_record->setTemperature($line_parts[2],"C");
            }
        elsif ($line_parts[0] eq "HUMIDITY")
           {
	   $header->setLine(5, "Radiosonde Serial Number:",$line_parts[7]);
           $first_record->setRelativeHumidity($line_parts[2]);
            }
        elsif ($line_parts[0] eq "WIND" && $line_parts[1] eq "DIR.")
           {
           $first_record->setWindDirection($line_parts[3]);
            }
        elsif ($line_parts[0] eq "WIND" && $line_parts[1] eq "VEL.")
           {
           $first_record->setWindSpeed($line_parts[3],"knot");
            }
        elsif ($line_parts[0] eq "CLOUD")
           {
	   $header->setLine(7, "Cloud Code:",$line_parts[3]);
            }
        elsif ($line_parts[0] eq "STATION" && $line_parts[1] eq "No.")
           {
           #----------------------------------------------------------
           # SL says don't put a station number anywhere in output. 
           # Just junk in the raw input data. Set to UMO-1 for
           # stationCD.out file. This does not affect the output data.
           #----------------------------------------------------------
           #            $id = trim($line_parts[3]);
           #            $id =~ s/\s+/_/g;
           #            $station->setStationId($id);
           #            $station->setStationId($id);
           #            $header->setId($id);
           #-------------------------------------------------------

           $station->setStationId("UMO-1");  # Set for StationCD.out file
           }
        elsif ($line_parts[0] eq "STATION" && $line_parts[1] eq "NAME")
           {
           #----------------------------------------------------
           # SL says don't put a station NAME anywhere in output.
           # Just junk in the raw input data.  Set to UMO.
           #----------------------------------------------------
           $station->setStationName("UMO");
           $header->setSite(sprintf("%s","UMO"));
           }
        else
           { 
           if ($debug){ print "Ignore line: $line\n"; }
           } # end IF

        if ($debug){ print "Read next line.\n"; }

	$line = <$FILE>;

    } # end While

   if ($debug){ print "End While\n"; }

    #-------------------------------------------
    # Get the station if it already was created.
    #-------------------------------------------
    if ($self->{"stations"}->hasStation($station->getStationId(),$station->getNetworkName(),
					$station->getLatitude(),$station->getLongitude(),$station->getElevation())) {

        if ($debug){print "Found existing station\n";}

	my $desc = $station->getStationName();
	$station = $self->{"stations"}->getStation($station->getStationId(),$station->getNetworkName(),
						   $station->getLatitude(),$station->getLongitude(),
						   $station->getElevation());
	$station->setStationName($desc) if ($station->getStationName() eq "Description");

    } else {
        if ($debug) {print "Add a new station\n";}
	$self->{"stations"}->addStation($station);
    }

    #-------------------------------------------------
    # Update the begin and end dates for this station.
    #-------------------------------------------------
    if ($debug){ print "Call station->insertDate(date, YYYYMMDD) with date = $date\n"; }
    $station->insertDate($date,"YYYYMMDD");     # Sets begin and end date in stationCD file.
    
    if ($debug){ print "Exit read_header().\n"; }
    return $header;

}# read_header()

######################### create_record  ####################################

##---------------------------------------------------------------------------
# @signature ClassRecord create_record(String line, ClassHeader header, String filename, ClassRecord previous_record)
# <p>Create a new ClassRecord from the data line.</p>
#
# @input $line The raw data line.
# @input $header The ClassHeader for the file.
# @input $filename The name of the raw data file.
# @input $previous_record The record that occurred in the raw file right before this file.
# @output $record The new record from the parsed data file/line.
##---------------------------------------------------------------------------
sub create_record {
    my ($self,undef(),$header,$filename,$previous_record) = @_;

    my @data = split(' ',$_[1]);

    my $record = ClassRecord->new($WARN,$filename,$previous_record);

    if ($debug){ print "create_record().\n"; }

    #-----------------------------------------------------------------
    # Set the Time in seconds for the record. Beware that this setTime
    # function can be called two different ways. Depends on number of
    # input parms. See library calls.
    #-----------------------------------------------------------------
    my @time_seconds = split (':', $data[0]);
    my $time_in_seconds = $time_seconds[0]*3600.0 + $time_seconds[1]*60.0 + $time_seconds[2];
    $record->setTime($time_in_seconds);

    #-----------------------------------------------------------------
    # BEWARE: Note the "hardcoded" error catches below, requested by
    #   SLoehrer for the 2008-2009 UMO data. Only valid for these
    #   specific years.
    #-----------------------------------------------------------------
    if ( $data[4] > 1800 )
       {
       print "   WARNING: Record with bad pressure at time = $data[0]. Set record to MISSING.\n";
       }
    elsif ($filename eq "research.UM_IMET.200902112352.radiosondePYS.txt.PYS" &&
            ($time_in_seconds == 10.0 || $time_in_seconds == 20.0 || 
             $time_in_seconds == 30.0 || $time_in_seconds == 40.0)  )
       {
       print "   WARNING: Found record at time $time_in_seconds for research.UM_IMET.200902112352.radiosondePYS.txt.PYS. Sonde sitting on surface! Set record to MISSING.\n";
       }

    else
       {
       #------------------------------------------------
       # BEWARE:
       # SLoehrer says there are issues with 2008-2009
       # raw data altitudes, so compute the geopotential
       # height/altitude and set for all UMO soundings
       # for this time period. Ignore the raw altitudes.
       # Note that the last three parms in calculateAltitude
       # are the pressure, temp, and dewpt for the current
       # record. To check the altitude calculations, see
       # the web interface tool at 
       #
       # http://dmg.eol.ucar.edu/cgi-bin/conversions/calculations/altitude
       #
       # To use the altitude from the raw data rec do:
       #     $record->setAltitude($data[6],"m");
       #------------------------------------------------
       my $geopotential_height; 

       if ($debug) 
          { 
          my $prev_press = $previous_record->getPressure(); 
          my $prev_temp = $previous_record->getTemperature(); 
          my $prev_dewpt = $previous_record->getDewPoint(); 
          my $prev_alt = $previous_record->getAltitude();

          print "\nCalc Geopotential Height from prev_press = $prev_press, 
                     prev_temp = $prev_temp, prev_dewpt = $prev_dewpt, 
                     prev_alt = $prev_alt, press = $data[4], temp = $data[1], dewpt = $data[2]\n"; 
          }

       if ($previous_record->getPressure() < 9990.0)
          {
          if ($debug){ print "prev_press < 9990.0 - NOT missing. Calc the geopotential height.\n"; }
 
          $geopotential_height = calculateAltitude($previous_record->getPressure(),
                                                   $previous_record->getTemperature(), $previous_record->getDewPoint(),
                                                   $previous_record->getAltitude(), $data[4], $data[1], $data[2]);
          }
       else
          {
          if ($debug){print "prev_press > 9990.0 - MISSING! Set geopot alt to missing.\n"; }

          $geopotential_height = 99999.0;
          }

       if ($debug) {print "geopotential_height = $geopotential_height\n"; }


       #-------------------------------
       # Set values for current record.
       #-------------------------------
       $record->setPressure($data[4],"hPa");
       $record->setTemperature($data[1],"C");
       $record->setRelativeHumidity($data[3]);
       $record->setDewPoint($data[2],"C");
       $record->setAltitude($geopotential_height,"m");

       $record->setWindDirection($data[9]);
       $record->setWindSpeed($data[10],"knot");
       }

    return $record;
} # create_record

#########################################################################################
