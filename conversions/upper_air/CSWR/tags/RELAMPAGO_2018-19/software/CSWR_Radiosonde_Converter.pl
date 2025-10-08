#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The CSWR_Radiosonde_Converter.pl script is used for converting 
# high resolution radiosonde data from the SHARPpy ascii format to the 
# EOL Sounding Composite (ESC) format.</p> 
#
#
# @author Linda Echo-Hawk, August 2019
# @version RELAMPAGO CSWR Radiosonde Data
#
#          - There are soundings from the following vehicles:
#            DOW8
#            SCOUT1
#            SCOUT2
#            SCOUT3
#            UIUC1
#            UIUC2
#            
#          - The files to use for the processing are the Hgt* files in the L2 directories, 
#            the dir tree is like:
#            YYYYMMDD-IOPXX/VECHICLE/soundings/L2/Hgt_YYYYMMDD_VEHICLE_hhmm
#            Copy these files into the raw_data directory for processing
# 
#          - These data are at routine height intervals rather than time intervals, but 
#            we should make the assumption that the data are at one second intervals.
#
#          - The HT is the geopotential height and the winds are in m/s.
#          
#          - The missing value is -999.0*.
#          
#          - The "raw" data come with flags provided by the CSWR QC processing. We 
#            should incorporate those into our flags as follows:
#
#            QP -> qp
#            QT -> qt
#            QD -> qu and qv
#
#            Values of 2 and 3 from the raw data translate to 2.0 in our files.
#            Values of 4 and 5 from the raw data translate to 3.0 in our files.
#            Values of 6 and 7 from the raw data translate to 4.0 in our files.
#
#            For any other values we won't make any changes from our regular processing.
#
#          - For the headers:
#
#            Data Type: (CSWR or UIUC) Mobile Radiosonde Data/Ascending
#            Project ID: RELAMPAGO
#            Release Site Type/Site ID: Use the vehicle name from the file name (e.g. SCOUT3)
#            Release Location: Use the locations from the first data record
#            UTC Release Time: Grab from the file name
#            Radiosonde Type: GRAW DFM-09
#            Radiosonde System Software: GRAWMET 5.14
#
#          - For the Data Type use UIUC just for the UIUC1 and UIUC2 vehicles, 
#            the others are all CSWR.
#          
#          - Need to derive RH and wind components.
#          
#          - A "readSurfaceValuesFile" function exists to 
#            read in surface values from a separate file, 
#            but is not called since the values were provided 
#            in the raw data and no separate file was used. 
#          - Ascension rate is calculated by the converter.
#
#
##Module------------------------------------------------------------------------
package CSWR_Radiosonde_Converter;
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

printf "\nCSWR_Radiosonde_Converter.pl began on ";print scalar localtime;printf "\n";
my $debug = 0;

&main();
printf "\nCSWR_Radiosonde_Converter.pl ended on ";print scalar localtime;printf "\n";

my $filename;

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the CSWR radiosonde data by converting it from 
# the native ASCII format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = CSWR_Radiosonde_Converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature CSWR_Radiosonde_Converter new()
# <p>Create a new instance of a CSWR_Radiosonde_Converter.</p>
#
# @output $self A new CSWR_Radiosonde_Converter object.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);
    
    $self->{"stations"} = SimpleStationMap->new();

    # HARD-CODED
    $self->{"PROJECT"} = "RELAMPAGO";
    # HARD-CODED
    $self->{"NETWORK"} = "CSWR";
    
    $self->{"FINAL_DIR"} = "../final";
    $self->{"OUTPUT_DIR"} = "../output";

    $self->{"RAW_DIR"} = "../raw_data";
    
    $self->{"STATION_FILE"} = sprintf("%s/%s_%s_sounding_stationCD.out",$self->{"FINAL_DIR"},
				      $self->cleanForFileName($self->{"NETWORK"}),
				      $self->cleanForFileName($self->{"PROJECT"}));
    $self->{"WARN_LOG"} = $self->{"OUTPUT_DIR"}."/warning.log";

    return $self;
}

##------------------------------------------------------------------------------
# @signature Station buildDefaultStation(String station_id, String network)
# <p>Create a default station for the ULM network using the 
# specified station_id and network.</p>
#
# @input $station_id The identifier of the station to be created.
# @input $network The network the station belongs to.
# @return The new station object with the default values for the network.
##------------------------------------------------------------------------------
sub buildDefaultStation {
    my ($self,$station_id,$network) = @_;
    my $station = Station->new($station_id,$network);
    # $station->setStationName($network);
	# info in 48-char field in stationCD.out file
    $station->setStationName("Meso");
    # HARD-CODED
    $station->setStateCode("99");
    $station->setReportingFrequency("no set schedule");
    $station->setNetworkIdNumber("99");
    # platform, 416	Radiosonde, GRAW DFM-09
    $station->setPlatformIdNumber(416);
	$station->setMobilityFlag("m");

    return $station;
}

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
sub buildLatLonFormat {
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

    # my $filename = $file;

    # HARD-CODED
    $header->setReleaseDirection("Ascending");
    $header->setProject($self->{"PROJECT"});
	# The Id will be the prefix of the output file
    # and appears in the stationCD.out file
    $header->setId("CSWR");
	# $header->setSite("NOAA ATDD Mobile");

    # ----------------------------------------------------------
    # Extract the date, time and vehicle information from the file name
	# Expects filename similar to:
	# Hgt_20181102_SCOUT3_2035  Hgt_20181102_UIUC2_1904  Hgt_20181106_SCOUT2_1455
	# Hgt_20181102_UIUC1_1929   Hgt_20181105_DOW8_1017   Hgt_20181110_SCOUT1_1504
    # ----------------------------------------------------------
	my ($hgt,$date,$vehicle,$time) = split("_", $file);

	if ($date =~ /(\d{4})(\d{2})(\d{2})/)
	{
		my ($year, $month, $day) = ($1,$2,$3);
		$date = join ", ", $year, $month, $day;
	}
	if ($time =~ /(\d{2})(\d{2})/)
	{
		my ($hour, $minute) = ($1, $2);
		$time = join ":", $hour,$minute,'00';
	}

    $header->setActualRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
    $header->setNominalRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);

    $header->setSite($vehicle);

    # Set the type of sounding
	if ($vehicle =~ /UIUC/)
	{
		$header->setType("UIUC Sounding Data");
	}
	else
	{
		$header->setType("CSWR Sounding Data");
	}
	

	my @headerData = split(' ',$headerlines[0]);
	
	# print "ALT: $headerData[1]\n";

	my $sfc_elev = $headerData[1];
	$header->setAltitude($sfc_elev,"m");
	
	# print "LAT: $headerData[12]\n";

	my $sfc_lat = $headerData[12];
    $header->setLatitude($sfc_lat, $self->buildLatLonFormat($sfc_lat));
	
    # print "LON: $headerData[11]\n";

	my $sfc_lon = $headerData[11];
	$header->setLongitude($sfc_lon, $self->buildLatLonFormat($sfc_lon)); 

    # -------------------------------------------------
   	# Other header info provided by Scot
	# -------------------------------------------------
    my $sondeType = "GRAW DFM-09";
	$header->setLine(5,"Radiosonde Type:", ($sondeType));
	my $groundStationSoftware = "GRAWMET 5.14";
	$header->setLine(6,"Radiosonde System Software:", ($groundStationSoftware));


    return $header;
}
                           
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

	my @headerlines = $lines[4];
	# print @headerlines;

                        
	# Generate the sounding header.
	my $header = $self->parseHeader($file, @headerlines);
    
    # Only continue processing the file if a header was created.
    if (defined($header)) {

	# Determine the station the sounding was released from.
	my $station = $self->{"stations"}->getStation($header->getId(),$self->{"NETWORK"});
	if (!defined($station)) {
	    $station = $self->buildDefaultStation($header->getId(),$self->{"NETWORK"});
	    $self->{"stations"}->addStation($station);
	}
	$station->insertDate($header->getNominalDate(),"YYYY, MM, DD");

    # ----------------------------------------------------
    # Create the output file name and open the output file
    # ----------------------------------------------------
    my $outfile;
	my ($hour, $min, $sec) = split (/:/, $header->getActualTime());
	my @location = split (" ", $header->getSite());
	my $citystate = join "_", @location;
	# print "citystate $citystate location @location\n";

   	$outfile = sprintf("%s_%s_%04d%02d%02d%02d%02d.cls", 
					   	   $header->getId(),
						   $citystate,
					   	   split(/,/,$header->getActualDate()),
					   	   $hour, $min);

    printf("\tOutput file name is %s\n", $outfile);


	open(my $OUT,">".$self->{"OUTPUT_DIR"}."/".$outfile)
	    or die("Can't open output file for $file\n");

	print($OUT $header->toString());
	
    # ----------------------------------------
    # Needed for code to derive ascension rate
    # ----------------------------------------
    my $prev_time = 0.0;
    my $prev_alt = $header->getAltitude();


    # ----------------------------------------------------
    # Parse the data portion of the input file
    # ----------------------------------------------------
	my $index = 0;
	my $fake_surface_time = 0;
	my $raw_data_time;
	my $fake_surface_data = 1;
    
    # Now grab the data from each line
	foreach my $line (@lines) 
	{
		my $record = ClassRecord->new($WARN,$file);
		
		if ($index >= 4)
		{
			if ($fake_surface_data)
			{
				# The raw data surface record does not
				# contain the time, so we assume the data
				# are at 1-second intervals.
			    $record->setTime($fake_surface_time);
	        	$record->setLatitude($header->getLatitude(),
			    	    $self->buildLatLonFormat($header->getLatitude()));
		        $record->setLongitude($header->getLongitude(),
				        $self->buildLatLonFormat($header->getLongitude()));
        	    $record->setAltitude($header->getAltitude(),"m");

				$fake_surface_data = 0;
			}
			
            #--------------------------------------------
			# RH and wind components must be calculated;
			# convert wind speed from knots to m/s
			#--------------------------------------------
			# $data[0]    P  Pressure
			# $data[1]    HT Geopotential Height
			# $data[2]    TC Temp
			# $data[3]    TD Dewpoint
			# $data[4]    Dir  Wind Direction
			# $data[5]    Spd  Wind Speed
			# $data[11]   Lon  Longitude
			# $data[12]   Lat  Latitude
			#
			#
		    #   P      HT      TC      TD     DIR     SPD  QP QH QT QD QW        LON       LAT
		    #  933.3   617.8   26.90   17.56   39.90    0.80   1  1  1  1  1  -64.50195 -32.41028
			#
			#
			#  QP -> qp  $data[6]  (pressure flag) setPressureFlag(float flag)
			#  QT -> qt  $data[8]  (temp flag)     setTemperatureFlag(float flag)
			#  QD -> qu and qv   $data[9]   (U and V wind flags) setUWindComponentFlag(float flag)
			#                                setVWindComponentFlag(float flag)
			#
			#  Values of 2 and 3 from the raw data translate to 2.0 in our files.
			#  Values of 4 and 5 from the raw data translate to 3.0 in our files.
			#  Values of 6 and 7 from the raw data translate to 4.0 in our files.
			#
			#  Any other values and we don't make any changes from our regular processing.

			my $temp;
			my $geopotential_height;
			my $dewpoint;
			my $pressure;
			my $wind_spd;
			my $wind_dir;
			
			chomp($line);
		    my @data = split(' ',$line);


			$pressure = trim($data[0]);
			$record->setPressure($pressure,"mb") if ($pressure !~ /^-999/);

			$geopotential_height = trim($data[1]);
			$record->setAltitude($geopotential_height,"m");

			$temp = trim($data[2]);
		   	$dewpoint = trim($data[3]);
			
			if (($dewpoint !~ /^-999/) && ($temp !~ /^-999/))
			{
                my $calculated_RH = calculateRelativeHumidity($temp, $dewpoint);
			
			    if ($calculated_RH)
			    {
				    $record->setRelativeHumidity($calculated_RH);
			    }
			    else
			    {
		            print "WARNING: RH could not be calculated\n";
			    }
			}

            $record->setTemperature($temp,"C") if ($temp !~ /^-999/);
			$record->setDewPoint($dewpoint,"C") if ($dewpoint !~ /^-999/);

			$wind_spd = trim($data[5]); # wind spd (m/s)
		    $record->setWindSpeed($wind_spd,"m/s") if ($wind_spd !~ /^-999/);
			
			$wind_dir = trim($data[4]); # wind dir (deg)
			$record->setWindDirection($wind_dir) if ($wind_dir !~ /^-999/);

			# -------------------------------------
			# Use raw data flags to set QC flags
			# -------------------------------------
			if (($data[6] == 2) || ($data[6] == 3))
			{
				$record->setPressureFlag(2.0);
				# print "Set Pressure Flag\n";
			}
			elsif (($data[6] == 4) || ($data[6] == 5))
			{
				$record->setPressureFlag(3.0);
				# print "Set Pressure Flag\n";
			}
			elsif (($data[6] == 6) || ($data[6] == 7))
			{
				$record->setPressureFlag(4.0);
				# print "Set Pressure Flag\n";
			}
 
			if (($data[8] == 2) || ($data[8] == 3))
			{
				$record->setTemperatureFlag(2.0);
				# print "Set Temp Flag\n";
			}
			elsif (($data[8] == 4) || ($data[8] == 5))
			{
				$record->setTemperatureFlag(3.0);
				# print "Set Temp Flag\n";
			}
			elsif (($data[8] == 6) || ($data[8] == 7))
			{
				$record->setTemperatureFlag(4.0);
				# print "Set Temp Flag\n";
			}

			if (($data[9] == 2) || ($data[9] == 3))
			{
			    $record->setUWindComponentFlag(2.0);
			    $record->setVWindComponentFlag(2.0);
				# print "Set Wind Flags\n";
			}
			elsif (($data[9] == 4) || ($data[9] == 5))
			{
				$record->setUWindComponentFlag(3.0);
			    $record->setVWindComponentFlag(3.0);
				# print "Set Wind Flags\n";
			}
			elsif (($data[9] == 6) || ($data[9] == 7))
			{
				$record->setUWindComponentFlag(4.0);
			    $record->setVWindComponentFlag(4.0);
				# print "Set Wind Flags\n";
			}
			# -------------------------------------
			# End set flags
			# -------------------------------------


	   		$record->setLatitude($data[12], $self->buildLatLonFormat($data[12]));
	   		$record->setLongitude($data[11], $self->buildLatLonFormat($data[11]));
		
        	#-------------------------------------
			# The first data line is index = 4.
			# Set initial time to zero, then 
			# use 5 second increments, so increment
			# $raw_data_time by five for lines
			# with $index > 4
			#-------------------------------------
			if ($index == 4)
			{                                                 
				$raw_data_time = 0;
		    	$record->setTime($raw_data_time);
				$raw_data_time += 1;         
			}
			elsif ($index > 4)
			{
				$record->setTime($raw_data_time);
				$raw_data_time += 1;
			}
			

    	    #-------------------------------------------------------
        	# this code from Ron Brown converter:
        	# Calculate the ascension rate which is the difference
	        # in altitudes divided by the change in time. Ascension
    	    # rates can be positive, zero, or negative. But the time
        	# must always be increasing (the norm) and not missing.
	        #
    	    # Only save off the next non-missing values.
        	# Ascension rates over spans of missing values are OK.
        	#-------------------------------------------------------
	        if ($index >= 4)
			{
			
			if ($debug) 
			{
				my $time = $record->getTime(); my $alt = $record->getAltitude(); 
            	# print "\nNEXT Line: prev_time, rec Time, prev_alt, rec Alt:: $prev_time, $time, $prev_alt, $alt\n"; 
				print "Gather ascension rate data for index $index:\n";
				print "\tPrevious Time $prev_time  altitude = $prev_alt\n";
				print "\tCurrent Time $time  altitude = $alt\n";
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
        	# Only save the next non-missing values. 
        	# Ascension rates over spans of missing values are OK.
        	#-----------------------------------------------------
	        if ($debug) 
			{ 
				my $rectime = $record->getTime(); my $recalt = $record->getAltitude();
            	print "Current record: Time $rectime  Altitude = $recalt "; 
			}

    	    if ($record->getTime() != 9999 && $record->getAltitude() != 99999)
        	{
            	$prev_time = $record->getTime();
	            $prev_alt = $record->getAltitude();
    	
        	    if ($debug) 
				{ 
					print " has valid Time and Alt.\n"; 
				}
    	    }
    		} # if ($index >= 3) calculate ascension rate
        	#-------------------------------------------------------
			# Completed the ascension rate data
    	    #-------------------------------------------------------

		    
		printf($OUT $record->toString());
		} # end if ($index >= 4)
			
	    $index++;
	} # end foreach $line

	} # end if (defined($header))
	else
	{
		printf("Unable to make a header\n");
	}
}

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
}

##------------------------------------------------------------------------------
# @signature void readDataFiles()
# <p>Read in the files from the raw data directory and convert each into an
# ESC formatted file.</p>
##------------------------------------------------------------------------------
sub readDataFiles {
    my ($self) = @_;
    
    opendir(my $RAW,$self->{"RAW_DIR"}) or die("Can't read raw directory ".$self->{"RAW_DIR"});
    # my @files = grep(/^2016.+txt$/,sort(readdir($RAW)));
    my @files = grep(/^Hgt/i,sort(readdir($RAW)));
    closedir($RAW);
    
    
    open($WARN,">".$self->{"WARN_LOG"}) or die("Can't create ".$self->{"WARN_LOG"});
    
	# printf("Ready to read the files\n");
    foreach my $file (@files) {
	$self->parseRawFile($file);
    }
    
    close($WARN);
}

##------------------------------------------------------------------------------
# @signature void readSurfaceValuesFile(file_name)
# <p>Read the contents of the file into an array.</p>
#
# @input $file_name The name of the raw data file to be read.
# @output array of surface values (lat/lon/elev)
##------------------------------------------------------------------------------
sub readSurfaceValuesFile {
    my $self = shift;

    open(my $FILE, sprintf("ULM_sfc_alt.txt")) or die("Can't read file into array\n");
    my @surface_data = <$FILE>;
    close ($FILE);

    return @surface_data;
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
