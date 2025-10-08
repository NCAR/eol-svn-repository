#! /usr/bin/perl -w

##Module-------------------------------------------------------------------------
# <p>The paposoSoundingConverter.pl script is used for converting the sounding data
# from the Paposo, Chile for VOCALS-REX 2008 .
#
#
# @usage paposoSoundingConverter.pl [--skip]
#        --skip   Skip the pre-processing steps to strip out blank lines; 
#                 default (if option not used) is false  
#
# @author Linda Echo-Hawk 2010-01-12
# @version VOCALS_2008 Paposo Lower Site Radiosonde (RAOBS) Data
#          - Changed to ElevatedStationMap (SimpleStationMap has no lat/lon/alt)
#          - Added command line switch to skip preprocessing steps
#          - Verified "missing" values (see warning below) for VOCALS_2008 data
#          - Added code to change surface ascent rate from 0.0 to missing 1/25/10
#          - Added Ground Check Humidity header line per Scot on 1/25/10
#
# @author L. Cully 
#  Oct/Nov 2008
# Began with Ron Brown and Le Moore conversions and created this Paposo converter. 
# Paposo is a Vaisala Variant as LeMoore was.
#
# WARNING: Not yet confident what MISSING values in raw data will be for 
#  each parm. This needs to be confirmed and code updated accordingly. WARNING!!!
#  (Search for the word "Verify".)
#
##Module-------------------------------------------------------------------------
package paposoSoundingConverter;
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

use DpgDate qw(:DEFAULT);
use ClassHeader;
use ClassRecord;
use ElevatedStationMap;
use Station;
 # import module to set up command line options
use Getopt::Long;  

my ($WARN);
                       
my $debug = 0;
my $debugHeader = 0;
my $debugData = 0;

printf "\npaposoSoundingConverter.pl began on ";print scalar localtime;printf "\n"; 
# read command line arguments 
my $result;   
# skip pre-processing steps for raw data files
my $skip;
# get the command line options
$result = GetOptions("skip" => \$skip);

if (!$skip)
{
   	printf("Perform pre-processing to strip blank lines.\n");
}
else
{
 	printf("Skip pre-processing steps to strip blank lines - already completed.\n");
}                                                     

&main();


printf "\npaposoSoundingConverter.pl ended on ";print scalar localtime;printf "\n";  

#-------------------------------------------------
# A collection of functions that contain constants
#-------------------------------------------------
sub getNetworkName { return "Paposo";  }
sub getOutputDirectory { return "../output"; }
# to be consistent with other VOCALS_2008 projects
sub getProjectName { return "VOCALS_2008"; }
sub getRawDirectory { return "../raw_data"; }
sub getStationFile { return sprintf("../final/%s_%s_stationCD.out",
                    getNetworkName(),getProjectName()); }
sub getSummaryFile { return "../output/station_summary.log"; }
sub getWarningFile { return "../output/warning.log"; }

##------------------------------------------------------------------------------
# @signature void main()
# <p>Execute the conversion of the data.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = paposoSoundingConverter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature void convert()
# <p>Convert all of the raw data and create the output files for the conversion.</p>
##------------------------------------------------------------------------------
sub convert {
    my $self = shift;

    mkdir(getOutputDirectory()) unless (-e getOutputDirectory());
    mkdir("../final") unless (-e "../final");

    $self->readRawDataFiles();
    $self->printStationFiles();
}

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

    if ($month =~ /JAN/i) { return 1; }
    elsif ($month =~ /FEB/i) { return 2; }
    elsif ($month =~ /MAR/i) { return 3; }
    elsif ($month =~ /APR/i) { return 4; }
    elsif ($month =~ /MAY/i) { return 5; }
    elsif ($month =~ /JUN/i) { return 6; }
    elsif ($month =~ /JUL/i) { return 7; }
    elsif ($month =~ /AUG/i) { return 8; }
    elsif ($month =~ /SEP/i) { return 9; }
    elsif ($month =~ /OCT/i) { return 10; }
    elsif ($month =~ /NOV/i) { return 11; }
    elsif ($month =~ /DEC/i) { return 12; }
    else { die("Unknown month: $month\n"); }
}

##------------------------------------------------------------------------------
# @signature paposoSoundingConverter new()
# <p>Create a new paposoSoundingConverter instance.</p>
#
# @output $converter The new converter.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);

    $self->{"stations"} = ElevatedStationMap->new();

    $self->{"FINAL_DIR"} = "../final";
    $self->{"NETWORK"} = getNetworkName();
    $self->{"PROJECT"} = getProjectName(); 

    $self->{"STATION_FILE"} = sprintf("%s/%s_%s_sounding_stationCD.out",$self->{"FINAL_DIR"},
                                      $self->clean_for_file_name($self->{"NETWORK"}),
                                      $self->clean_for_file_name($self->{"PROJECT"}));

    return $self;
}

##---------------------------------------------------------------------------
# @signature void printStationFiles()
# <p>Generate the stationCD.out file and the station summary log for the stations
# in the conversion.</p>
##---------------------------------------------------------------------------
sub printStationFiles {
    my $self = shift;
    my ($STN, $SUMMARY);
    
    open($STN, ">".$self->getStationFile()) || die("Cannot create the ".$self->getStationFile()." file\n");
    foreach my $station ($self->{"stations"}->getAllStations()) {
        print($STN $station->toString()) if ($station->getBeginDate !~ /^9+$/);
    }
    close($STN);

    open($SUMMARY, ">".$self->getSummaryFile()) || die("Cannot create the ".$self->getSummaryFile()." file.\n");
    print($SUMMARY $self->{"stations"}->getStationSummary());
    close($SUMMARY);
}

##------------------------------------------------------------------------------
# @signature void readRawDataFiles()
# <p>Determine all of the raw data files that need to be processed and then
# process them.</p>
##------------------------------------------------------------------------------
sub readRawDataFiles {
    my $self = shift;

    opendir(my $RAW,$self->getRawDirectory()) or die("Cannot open raw directory.\n");
    my @files = readdir($RAW);
    closedir($RAW);

    open($WARN,">".$self->getWarningFile()) or die("Can't open warning file.\n");

    foreach my $file (sort(@files)) {
    $self->readRawFile($file) if ($file =~ /\.TXT$/i);
    }

    close($WARN);
}

##------------------------------------------------------------------------------
# @signature void readRawFile(String file_name)
# <p>Convert the specified file into the CLASS format.</p>
#
# @input $file_name The name of the raw data file to be converted.
##------------------------------------------------------------------------------
sub readRawFile 
{
    my $self = shift;
    my $file_name = shift;
    my $file = sprintf("%s/%s",$self->getRawDirectory(),$file_name);

    my $cmd_perl = "perl -pe 's/^\\s\+\$//'";   # Strips blank lines
    my $cmd;
    my $string = "  ";
    my $number_recs_processed = 0;

    my $serial_number = "";

    my $release_lat = "";
    my $release_lon = "";
    my $release_alt = "";

    #----------------------------------------------------
    # Retain uniq of possible data termination indicators
    # 85575 = "fake" WMO stn ID, TTxx and IIxx are GTS
    # message indicators. SCQN is the Paposo "call sign".
    # Various spanish words: NOTA, velocidad, etc.
    # Translated GTS data hdrs: Time Hght, etc.
    #----------------------------------------------------
    my @stopData = ("TTAA", "TTBB", "TTCC", "TTDD",
                    "IIAA", "IIBB", "IICC", "IIDD",
                    "SCQN", "85575", "7487 SCQN", "7487",
                    "NOTA", "NOTA:", "velocodad", "velocidad",
                    "Time Hght", "Press Hght", "min s", "hPa gpm"
                    );

    my $stop_line_date_only = ""; my $stop_line_date_next_day = ""; 
    my $stop_line_date = "";      my $stop_line_date_colon = "";

    #-------------------------
    # List of lines to ignore .
    #-------------------------

    my @ignoreLines = ("Start Up",     "System test",  "STATION :" ,
                      "TEMP MOBIL",    "TEMP BASIC",   "Sounding program", 
                      "Time AscRate",  "Ground check",
                      );

    #----------------------------------------------------------------------
    # Preprocess each file by stripping all blank lines. 
    # Doing this because the input file formats particularly for VOCALS 2008
    # project seem to be varying and contain mixed in blank lines. Stripping
    # blank lines will add some consistency. (LEC)
    # E.g., command to strip blank lines: perl -pe 's/^\s+$//' infile > outfile
    #-----------------------------------------------------------------------
    #
    # Use the --skip cmd line option if this step has been performed already
    #-----------------------------------------------------------------------
	if (!$skip)
	{
		$cmd = sprintf "%s %s > %s.noBlanks", $cmd_perl, $file, $file;

        print "\nIssue the following command: $cmd\n";
        system $cmd;

        # Save the original input file in *.orig
        print "Executing: /bin/mv $file $file.orig \n"; 
		system "/bin/mv -f $file $file.orig";

        print "Executing: /bin/mv $file.noBlanks $file \n";
        system "/bin/mv -f $file.noBlanks $file";
    }

    printf("\nProcessing file: %s\n",$file_name);
    open(my $FILE,$file) or die("Cannot open file: $file\n");

    #----------------------------------------------------------------
    # Process the header. It's possible that header may be missing or
    # may find repeated header lines. 
    #----------------------------------------------------------------
    my $header = ClassHeader->new();
    $header->setProject($self->getProjectName());
    $header->setType("Paposo, Chile Soundings");
    $header->setReleaseDirection("Ascending");

    #-------------------------------------------------------------------
    # Sample Header and line of data after blank lines stripped.
    #-------------------------------------------------------------------
    # Skip everything until you hit these header records!!
    #
    # Sounding program REV 8.36 using GPS
    # Station  : 85575 SCQN
    # Location : 25.00 S  70.46 W     22 m
    # RS-number: 224507301
    # Ground check  :    Ref     RS   Corr
    #   Pressure    : 1014.5 1014.9   -0.4
    #   Temperature :   19.6   21.2   -1.6
    #   Humidity    :      0     -7      7
    # Started at:      21 OCT 08 13:43 UTC    
    #   Time AscRate Hgt/MSL Pressure   Temp  RH   Dewp  Dir Speed
    # min  s     m/s       m      hPa   degC   %   degC  deg   m/s
    #   0  0     0.0      22   1014.5   20.5  52   10.3  200   2.6
    #  78  0     4.9   19871     56.6  -67.8  //  /////  ///  ////
    #-------------------------------------------------------------------
    my $hdr_word = "";

    #--------------------------------------------------
    # Process lines until you find the data section.
    # If there are multiples of the same type of 
    # lines, the last one will be output. If there
    # are more than 30 lines found, s/w warns and exits.
    #---------------------------------------------------
    until ( $hdr_word =~ /min s/)
    {
        <$FILE> =~ /\s*(.+)\s*/; 
        if ($debug) {print "\nInput Line:: $1\n";}

        $number_recs_processed++;

        my @hdr_line = split(' ', trim($1));
        $hdr_word = $hdr_line[0]." ".$hdr_line[1];
        if ($debugHeader) {print "hdr_word:: $hdr_word\n";}

        #-------------------------------
        # Ignore several types of lines.
        #-------------------------------
        my @inIgnoreLines = grep {$hdr_word eq $_} @ignoreLines;
        if ($debug) 
		{
			print "ignoreLines:: @ignoreLines \n ";
			print "inIgnoreLines:: @inIgnoreLines, ";
			print "Array length:: scalar(@inIgnoreLines)\n";
		}

        next if ( scalar(@inIgnoreLines) > 0 );  # Go to next record

        #----------------------------------------------------
        # Process or warn about all other header lines found.
        #----------------------------------------------------
        SWITCH:  {

            #---------------
            # Station
            #---------------
            if ($hdr_word =~ /Station :/)
            { 
				my $station;
				my $site;
				if((defined($hdr_line[2])) && (defined($hdr_line[3])))
				{
					$hdr_line[3] = trim($hdr_line[3]);
					$hdr_line[2] = trim($hdr_line[2]);
					$station = join(" ", $hdr_line[3], $hdr_line[2]);
					$site = join("/", $hdr_line[3], $hdr_line[2]);
				}
				else
				{
					# print "Found an undefined value in header line\n";
					$station = "SCQN 85575";
					$site = "SCQN/85575";
				}
                if ($debugHeader) {print "   Processing Station:: $station\n";}

                $header->setId($station);  # For Station  :  "85575 SCQN".
                $header->setSite($site);

                last SWITCH;
            } # Station

            #-----------------------
            # Ground Check Pressure
            #-----------------------
            if ($hdr_word =~ /Pressure :/ )
            {
                #-------------------------------------------------------------
                # Convert "Pressure    : 1013.8 1013.6    0.2"     to
                # "Ground Check Pressure:    Ref 1013.8 Sonde 1013.6 Corr 0.2"
                #-------------------------------------------------------------
                my $GroundCheckPress = trim("Ref ". $hdr_line[2].
                       " Sonde ".$hdr_line[3]." Corr ".$hdr_line[4]);
                if ($debugHeader) {print "   Ground Check Pressure:: $GroundCheckPress\n";}

                $header->setLine(6,"Ground Check Pressure:    ", $GroundCheckPress);

                last SWITCH;
            } # Pressure

            #-------------------------
            # Ground Check Temperature
            #-------------------------
            if ($hdr_word =~ /Temperature :/ )
            {
                #-----------------------------------------------------------
                # Convert "Temperature :   19.4   19.9   -0.5"     to
                # "Ground Check Temperature: Ref 19.4 Sonde 19.9 Corr -0.5"
                #-----------------------------------------------------------
                my $GroundCheckTemp = trim("Ref ". $hdr_line[2]." Sonde ".
                              $hdr_line[3]." Corr ".$hdr_line[4]);
                if ($debugHeader) {print "   Ground Check Temperature:: $GroundCheckTemp\n";}

                $header->setLine(7,"Ground Check Temperature: ", $GroundCheckTemp);

                last SWITCH;
            } # Temperature

            #-------------------------
            # Ground Check Humidity
            #-------------------------
            if ($hdr_word =~ /Humidity :/ )
            {
                #-----------------------------------------------------------
                # Convert "Humidity :   0   -4   4"     to
                # "Ground Check Humidity: Ref 0 Sonde -4 Corr 4"
                #-----------------------------------------------------------
                my $GroundCheckHumidity = trim("Ref ". $hdr_line[2]." Sonde ".
                              $hdr_line[3]." Corr ".$hdr_line[4]);
                if ($debugHeader) {print "   Ground Check Humidity:: $GroundCheckHumidity\n";}

                $header->setLine(8,"Ground Check Humidity: ", $GroundCheckHumidity);

                last SWITCH;
            } # Humidity
  
            #---------------
            # Location
            #---------------
            if ($hdr_word =~ /Location :/ ) 
            {
                #---------------------------------------------------------------------------------------
                # Set the Release Location.
                #
                # Convert "Location : 25.00 S  70.46 W     22 m" to
                # "Release Location (lon,lat,alt):    000 27.60'E, 05 00.00'S, -0.460, -5.000, 22.0"
                #---------------------------------------------------------------------------------------
                if ($debugHeader) {print "   Processing Location\n";}

                shift(@hdr_line); shift(@hdr_line);
                my @loc = @hdr_line;
                
                if ($debugHeader) 
				{
					print "   loc array:: @loc ;\n"; # loc array:: 25.00 S 70.46 W 22 m  
				}

                if ($loc[1] eq 'S') {$loc[0]= -1.00 * $loc[0]; }
                if ($loc[3] eq 'W') {$loc[2]= -1.00 * $loc[2]; }
                
                $release_lat = $loc[0];
                $release_lon = $loc[2];
				$release_alt = $loc[4];

                $header->setLatitude($release_lat, $self->buildLatlongFormat($release_lat));
                $header->setLongitude($release_lon, $self->buildLatlongFormat($release_lon));
                
				$header->setAltitude($loc[4], "m");

                last SWITCH;
            } # Location

            #---------------
            # Serial Number
            #---------------
            if ($hdr_line[0] =~ /RS-number:/i)
            {
                $serial_number = $hdr_line[1];
                push @stopData, $serial_number;

                if ($debugHeader) 
				{
					print "   Processing (Serial) RS-number:: $serial_number\n  ";
					print "   Add to stopData Array: @stopData\n";
				}
                $header->setLine(5,"Radiosonde Serial Number:", trim($serial_number));

                last SWITCH;
            } # Release Number 

            #----------------------------------------------------------
            # Started at line.
            # Convert "21 OCT 08 13:43 UTC" to "2008, 10, 21, 13:43:00"
            #----------------------------------------------------------
            if ($hdr_word =~ /Started at:/i)
            {
                shift(@hdr_line); shift(@hdr_line);
                my @date_time = @hdr_line;

                if ($debug) 
                {
                    print "   Started at:: @date_time\n";
                    print "date_time:: @date_time\n";  #date_time:: 21 OCT 08 13:43 UTC
                    print "date_time[0], date_time[1], date_time[2], date_time[3]:: ";
					print "$date_time[0], $date_time[1], $date_time[2], $date_time[3]\n"; # 21, OCT, 08, 13:43
                }
				my @time = split(":", $date_time[3]);
                my $padded_time = sprintf("%02d%02d", $time[0], $time[1]); # 1343
                my $padded_time_colon = sprintf("%02d:%02d", $time[0], $time[1]); #13:43
                my $date = sprintf("20%02d, %02d, %02d", $date_time[2], 
								$self->getMonth($date_time[1]), $date_time[0]); # date:: 2008, 10, 19
				# ---------------------------------------------------------
				# Some files have the date as the next line after the data
				# records, so add this date info to the stopData array
				# ---------------------------------------------------------
                $stop_line_date_only = sprintf("%02d%02d%02d", $date_time[2], 
								$self->getMonth($date_time[1]),$date_time[0]); # 081019=YYMMDD

                #--------------------------------------------------
                # If start at time is > 23:00, then src may output 
                # stop date equal to next day with 00:00.
                #--------------------------------------------------
                # QUICK FIX: need to account for last day of month,
                # in this case October 31 for paposa VOCALS_2008 data
                my $next_day = $date_time[0] +1;     # next_day:: 20
				if ($next_day == 32)
				{
					$next_day = 1;
                    my $next_month = "NOV";
					$stop_line_date_next_day = sprintf("%02d%02d%02d", $date_time[2], 
							$self->getMonth($next_month),$next_day); # 081031 becomes 081101       
				}
				else
				{
					$stop_line_date_next_day = sprintf("%02d%02d%02d", $date_time[2], 
							$self->getMonth($date_time[1]),$next_day); # 081020
				}

                $stop_line_date = sprintf("%02d%02d%02d %02d%02d", $date_time[2], 
							$self->getMonth($date_time[1]),$date_time[0], $time[0],$time[1]); # 081019 2348
                $stop_line_date_colon = sprintf("%02d%02d%02d %02d:%02d", $date_time[2], 
							$self->getMonth($date_time[1]),$date_time[0],$time[0],$time[1]);  # 081019 23:48 

                push @stopData, $stop_line_date_only,$stop_line_date_next_day, $stop_line_date, $stop_line_date_colon;

                if ($debug) {print "\nAdd stop dates to stopData:: @stopData\n";}

                if ($debug)
                {
                    print "   date:: $date\n";                                       # date:: 2008, 10, 21
                    print "   next_day:: $next_day\n";                               # form "YYMMDD" 
                    print "   stop_line_date_only:: $stop_line_date_only\n";         # form "YYMMDD" eg. "081017"
                    print "   stop_line_date_next_day:: $stop_line_date_next_day\n"; # form "YYMMDD" eg. "081018"
                    print "   stop_line_date:: $stop_line_date\n";                   # form "YYMMDD HHMM" eg. "081017 1200"
                    print "   stop_line_date_colon:: $stop_line_date_colon\n";       # form "YYMMDD HHMM" eg. "081017 12:00"
                }

                # Set output Actual Release date/time = "Start at" date/time 
				$header->setActualRelease($date, "YYYY, MM, DD", $padded_time_colon, "HH:MM", 0);  


                #----------------------------------------------
                # The Nominal Release time is in the file name. 
                # Not in the file. Assume input file name form.
                # Example: SCQN_20OCT2008_0000UTC.TXT
                #----------------------------------------------
                my @file_name_parts = split("_", $file_name);
                if ($debug) {print "file_name_parts:: @file_name_parts \n";}

                my $file_name_yr  = substr($file_name_parts[1],5,4); # YYYY
                my $file_name_mo  = substr($file_name_parts[1],2,3); # OCT
                my $file_name_day = substr($file_name_parts[1],0,2); # dd
                my $file_name_hr  = substr($file_name_parts[2],0,2); # HH
                my $file_name_min = substr($file_name_parts[2],2,2); # MM 

                if ($debug) 
				{
					print "file_name_[yr,mo,day,hr,min]:: $file_name_yr $file_name_mo ";
					print "$file_name_day $file_name_hr $file_name_min\n";
				}

                my $file_name_date = sprintf("%04d, %02d, %02d", $file_name_yr, 
										$self->getMonth($file_name_mo), $file_name_day);
                my $file_name_time = sprintf("%02d:%02d", $file_name_hr, $file_name_min);

                $header->setNominalRelease($file_name_date, "YYYY, MM, DD", $file_name_time, "HH:MM", 0);


                last SWITCH;

            }

            #------------------
            # Last Header Line
            #------------------
            # if ($hdr_line[0].$hdr_line[1].$hdr_line[2] eq "minsm/s")
            if ($hdr_line[0].$hdr_line[1].$hdr_line[2] =~ /minsm\/s/)
            {
                if ($debug) {print "   Found Last Header Line. Expect Data Next!\n";}
                last SWITCH;
            } # Last Header Line

            print "ERROR: Unknown Header Line at line $number_recs_processed. Found:: @hdr_line! Check $file_name.\n";
            if ($debugHeader) {print "Unknown Header Line\n";}

        } # SWITCH ends

        if ($number_recs_processed > 30) 
        {
            print "ERROR: Too many header lines. Could Not find header info within first 30 lines! Check $file_name.\n";
            exit(0);
        }

    } # "Until Loop" - end of looking for header lines

    if ($debugHeader) {print "Total Number of Header Lines Processed: $number_recs_processed;\n";}

    #-----------------------------------------------------------------
    # All soundings come from the Paposo, Chile Surface site. Since the
    # Paposo is a stationary site, the lat/lons will be set
    # in the stationCD.out file and the type will be 'f' (fixed).
    #----------------------------------------------------------------- Verify HERE
    my $station = $self->{"stations"}->getStation($header->getId(),$self->{"NETWORK"},
   				               $header->getLatitude(),$header->getLongitude(),
     			               $header->getAltitude());   
    if (!defined($station)) 
	{
 	    $station = $self->build_default_station($header->getId(),$self->{"NETWORK"});
        $station->setLatitude($header->getLatitude(),$self->buildLatlongFormat($header->getLatitude()));
	    $station->setLongitude($header->getLongitude(),$self->buildLatlongFormat($header->getLongitude()));
	    $station->setElevation($header->getAltitude(),"m");
	    $self->{"stations"}->addStation($station);      
	}
    $station->insertDate($header->getNominalDate(),"YYYY, MM, DD");
    $station->setUTC_Offset (0);
    $station->setCountry("CI"); # Paposo, Chile

    #-------------------------------------------------
    # Open the output file in the ../output directory.
    # The output name should have the actual date/time
    # not the nominal time because there can be more
    # than one actual time within a nominal time.
    #-------------------------------------------------
	my @siteInfo = split(' ',$header->getId());
    my $filename = sprintf("%s_%s_%s_%04d%02d%02d%02d%02d.cls",
	           getNetworkName(), $siteInfo[0], $siteInfo[1],
               split(/, /,$header->getActualDate()),
               split(/:/,$header->getActualTime()));

    open(my $OUT,sprintf(">%s/%s",getOutputDirectory(),$filename)) or 
    die("Cannot open output file\n");
    
    #-----------------------------
    # Print Header to output file.
    #-----------------------------
    print($OUT $header->toString());

    if ($debugHeader) {print "File: $file_name. Found $number_recs_processed HEADER lines.\n";}

    #---------------------------------------------------------
    # Process Data Section
    #---------------------------------------------------------
    # Note that these files have GTS/info records beyond the 
    # end of the data lines, but do NOT process them.
    # Must verify how to detect end of sounding. 
    #---------------------------------------------------------
    #---------------------------------------------------------
    my @lines = <$FILE>;   # Grab all the remaining data lines
    if ($debugData) { print "input file lines::\n @lines\n\n"; }

    my $prev_record;
    my $first_data_rec = 1;
    #------------------------------------------------------
    # Process each of the remaining data lines.
    # Note that $WARN is the ../output/warning.log file.
    #
    # Sample Data lines:
    #   Time AscRate Hgt/MSL Pressure   Temp  RH   Dewp  Dir Speed
    # min  s     m/s       m      hPa   degC   %   degC  deg   m/s
    #
    #   0  0     0.0      22   1014.5   20.5  52   10.3  200   2.6
    #   0 10     4.2      64   1009.5   17.1  61    9.5  ///  ////
    #   0 20     3.7      95   1005.8   14.4  64    7.7  ///  ////
    #  15  0     4.6    4329    603.7   -0.4   8  -30.8  242   2.9
    #  16  0     4.2    4583    584.7   -2.2   7  -33.5  336   3.7
    #  17  0     4.8    4855    564 <<-- Cut off line
    #
    #-----------------------------------------------------------------------
    # NOTE: Assume slashes are missing. VERIFY Number of slashes for each parameter!!!
    # VERIFIED NUM SLASHES FOR MISSING: Ascent rate: 5; min: 5; sec: 5; pressure: 5; 
	# temp: 4; rh: 5; height: 5; dewpt: 5.  - VERIFIED 1/13/2010 echohawk
    # NOTE: Use pattern matching to look for one or more slashes 
    #       rather than an exact count for individual variables.
    #-----------------------------------------------------------------------
    foreach my $line (@lines) 
	{
        my @data = split(' ', trim($line));
        my $data_word = $data[0].' '.$data[1];

        my @inStopSet = grep {$data[0] eq $_} @stopData;   
        my @inStopSetWord = grep {$data_word eq $_} @stopData;   

        if ($debugData) {print "Processing data Line: $line\n data_word = $data_word\n";}
        if ($debugData) {print "inStopSet:: @inStopSet ; inStopSetWord:: @inStopSetWord\n";}

        if (  
           (scalar(@data) < 10 )        ||   # Incomplete data line
           (scalar(@inStopSet) > 0)     ||   # Found in list of "stop" words/phrases
           (scalar(@inStopSetWord) > 0)     
           ) 
        {
            #--------------------------------------------------
            # Found end of "regular" sounding. Stop processing.
            #--------------------------------------------------
            if ($debugData) {print "Found end of data, exit.\n";}
            last; 
        }
        
        my $rec = ClassRecord->new($WARN, $filename, $prev_record);
        $number_recs_processed++;


        $rec->setTime($data[0],$data[1]) unless (($data[0] =~ /\/+/) || ($data[1] =~ /\/+/));
        
		# $rec->setAscensionRate($data[2], "m/s") unless ($data[2] =~ /\/+/); 
        $rec->setAltitude($data[3], "m") unless ($data[3] =~ /\/+/); 
        $rec->setPressure($data[4], "hPa")  unless ($data[4] =~ /\/+/); 
        $rec->setTemperature($data[5], "C") unless ($data[5] =~ /\/+/); 
        $rec->setRelativeHumidity($data[6]) unless ($data[6] =~ /\/+/);
        
        #---------------------------------------------------------------
        # Set the first data rec's lat/lon to the release lat/lon, but
        # only if the time and rate are zero and the Hgt above MSL is
        # same as release site alt/elev. Else there's something wrong
        # with the sounding. Here's a sample first rec::
        #
        #   Time AscRate Hgt/MSL Pressure   Temp  RH   Dewp  Dir Speed
        # min  s     m/s       m      hPa   degC   %   degC  deg   m/s
        #   0  0     0.0      22   1014.7   25.5  43   12.0  270   2.6
        #---------------------------------------------------------------
        if ($debugData) 
		{
			print "first_data_rec: $first_data_rec, min (s/b 0) = $data[0], ";
			print "dsec (s/b 0) = $data[1], ascent rate (s/b 0) = $data[2], alt = $data[3], "; 
			print "release_alt = $release_alt\n";
		}

        if ($first_data_rec == 1 && $data[0] == 0 && $data[1] == 0 &&
            $data[2] == 0.0 && $data[3] == $release_alt)
        {
            $rec->setLatitude($release_lat, $self->buildLatlongFormat($release_lat)) 
								unless ($release_lat =~ /\/+/); # Missing Value 
            $rec->setLongitude($release_lon, $self->buildLatlongFormat($release_lon)) 
								unless ($release_lon =~ /\/+/); # Missing Valuea
            # The raw data has the first (surface) ascent rate 
			# as "0.0" but we want to use a "missing" value 
            # since the ascent rate is undefined at that point
			$data[2] = "-999.0";
		    $rec->setAscensionRate($data[2], "m/s") unless ($data[2] =~ /-999.0/); 
            $first_data_rec = 0;
        }
		else
		{
		    $rec->setAscensionRate($data[2], "m/s") unless ($data[2] =~ /\/+/); 
		}

        #--------------------------------------------------------------------
        # If T and RH are missing, then the Td should also be set to Missing.
        # Remember there isn't a Td flag to set! 
        #---------------------------------------------------------------
        # BEWARE: the getDewPoint fn attempts to calculate the DewPoint
        #         and returns that value.
        # It does not simply grab the current DewPt value and return it!
        # So if the raw input Td (data[7]) is missing, but the Temp and
        # RH are not, then the calc and reset the output Td using T, RH.
        #---------------------------------------------------------------
        if ($debugData) 
		{
			my $T = $rec->getTemperature(); 
			my $RH = $rec->getRelativeHumidity(); 
			print "Temp, RH:: $T , $RH\n";
		}

        if ($rec->getRelativeHumidity() == 999.0 && $rec->getTemperature() == 999.0)
        {
			$rec->setDewPoint("999.0", "C"); # 999.0 M  
		}  
        else
        { 
            if ($data[7] !~ /\/+/)
            { 
                $rec->setDewPoint($data[7], "C"); 
            } 
            else   # Calc DewPt from Temp and RH
            { 
                my $Td = $rec->getDewPoint();
                $rec->setDewPoint($Td, "C");
            } 
        } # The second parm indicates the "Units" for Celcius!

        if ($debugData) 
		{
			my $Td = $rec->getDewPoint(); 
			print "data[7] versus DewPt:: $data[7] $Td \n";
		}


        #---------------
        # Set Wind parms
        #---------------
        $rec->setWindSpeed($data[9], "m/s") unless ($data[9] =~ /\/+/);
        $rec->setWindDirection($data[8])    unless ($data[8] =~ /\/+/);

        #-------------------------------------------------------------
        # Convert ClassRecord to String. This call also checks values.
        # See ClassRecord.pm/check_values() for DewPt < -99.9 check.
        # Note that if Td < -99.9, warning message is written to 
        # ../output/warning.log file. Also, every parm is compared to
        # to its pre-known missing value and the allowed output format
        # size/length. The value is reset to its missing value if
        # too big for output format. Even Time is checked. A warning
        # message of "xxx value yyy is too big for the field at time zzz.
        # Setting to missing." is issued for each parm.
        #-------------------------------------------------------------
        if ($debugData) { print "Print record(toString)\n\n"; }
        print($OUT $rec->toString()); 

        $prev_record = $rec;

    } # Foreach my lines. - process all data lines.

    if ($debugData) 
	{ 
		print "readRawFile:: Total number_recs_processed = ";
		print "$number_recs_processed .\nNext File\n"; 
	}

    close($OUT);
}

##------------------------------------------------------------------------------
# @signature Station build_default_station(String station_id, String network)
# <p>Create a default station for the Paposo Ship using the specified
# station_id and network.</p>
#
# @input $station_id The identifier of the station to be created.
# @input $network The network the station belongs to.
# @return The new station object with the default values for the network.
##------------------------------------------------------------------------------
sub build_default_station {
    my ($self,$station_id,$network) = @_;
    my $station = Station->new($station_id,$network);

    $station->setStationName($network);
    $station->setLatLongAccuracy(3);         
    $station->setStateCode("99");
    $station->setReportingFrequency("6 hourly");
    $station->setNetworkIdNumber(99);
    $station->setPlatformIdNumber(999);
    $station->setMobilityFlag("f");
    return $station;
}

##------------------------------------------------------------------------------
# @signature String buildLatlongFormat(String value)
# <p>Generate the decimal format for the specified value.</p>
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
# @signature String clean_for_file_name(String text)
# <p>Remove/translate characters in a String so it can be used in a file name.</p>
#
# @input $text The String to be cleaned.
# @output $text The cleaned up String.
##-------------------------------------------------------------------------
sub clean_for_file_name {
    my ($self,$text) = @_;

    # Convert spaces to underscores.
    $text =~ s/\s+/_/g;

    # Remove all hyphens
    $text =~ s/\-//g;

    return $text;
}
  
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
}
