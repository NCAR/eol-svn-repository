#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The NOAA_ATDD_Mobile_Radiosonde_Converter.pl script is used for converting 
# high resolution radiosonde data from a csv format to the EOL Sounding 
# Composite (ESC) format.</p> 
#
#
# @author Linda Echo-Hawk 7 July 2016
# @version VORTEX-SE_2016 based on DYNAMO Singapore csv converter
#          - Scot provided a list of locations and their surface
#            lat/lon/elev values. This was put into a separate 
#            text file and read into the converter.
#          - The raw data did not contain any surface data. Surface
#            values were set from the data provided by Scot or set
#            to missing. This was the time = 0 line.
#          - The location from the text file served as the Site
#            value for the header and in the output file name.
#          - Two files had start times that were not "1". The 
#            converter had line 2 start with the time from the
#            raw data. In other words, two files had missing time
#            after t = 0.
#          - RH and dewpoint had to be calculated using equations
#            provided by Scot.
#          - The code used "setVariableValue" to include a column
#            for the mixing ratio values.
#          - Geopotential height is calculated by the converter.
#          - Ascension rate is calculated by the converter.
#
#
# @author Linda Echo-Hawk 2012-05-18
# @version DYNAMO  Created based on VORTEX2 SUNY_Radiosonde_Converter.pl.
#          - Raw data files are csv format and have *.csv extension
#            (first run dos2unix)
#          - Converter expects the actual data to begin
#            after the header lines (line number varies for
#            some files).  
#          - Header lat/lon/alt is hard-coded from csv file
#          - Release time is obtained from the file name.
#          - Some files contain a "missing" time last line.
#            Code was added so that these are not printed (set toString).
#          - Some files contain missing wind speed and/or direction on
#            the last data line.  Code was added to pop the last line
#            off of the data lines array and examine it.  If the data
#            is present the line is pushed back onto the array for
#            processing. Otherwise it is left off of the array.
#
#
##Module------------------------------------------------------------------------
package NOAA_ATDD_Mobile_Radiosonde_Converter;
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

printf "\nNOAA_ATDD_Mobile_Radiosonde_Converter.pl began on ";print scalar localtime;printf "\n";
my $debug = 0;
my $debug_geopotential_height = 0;

&main();
printf "\nNOAA_ATDD_Mobile_Radiosonde_Converter.pl ended on ";print scalar localtime;printf "\n";

my $filename;

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the NOAA ATDD radiosonde data by converting it from 
# the native ASCII format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = NOAA_ATDD_Mobile_Radiosonde_Converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature NOAA_ATDD_Mobile_Radiosonde_Converter new()
# <p>Create a new instance of a NOAA_ATDD_Mobile_Radiosonde_Converter.</p>
#
# @output $self A new NOAA_ATDD_Mobile_Radiosonde_Converter object.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);
    
    $self->{"stations"} = SimpleStationMap->new();

    # HARD-CODED
    $self->{"PROJECT"} = "VORTEX-SE_2016";
    # HARD-CODED
    $self->{"NETWORK"} = "NOAA_ATDD";
    
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
# <p>Create a default station for the SUNY Oswego network using the 
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
    $station->setStationName("NOAA_ATDD");
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
    my ($self,$file) = @_;
    my $header = ClassHeader->new();

    $filename = $file;

    # HARD-CODED
    # Set the type of sounding
    $header->setType("NOAA ATDD Mobile Sounding Data");
    $header->setReleaseDirection("Ascending");
    $header->setProject($self->{"PROJECT"});
	# The Id will be the prefix of the output file
    # and appears in the stationCD.out file
    $header->setId("ATDD");
	# $header->setSite("NOAA ATDD Mobile");


    # ------------------------------------------------------
	# Read in the file of surface values provided by Scot
	# Note that St. Florian, AL; 34.894121; -87.608493; 207
	# is the same as FlorenceAL and 
	# UAH SWIRLL, AL is the UAH file
	# ------------------------------------------------------
	my @surfaceValues = &readSurfaceValuesFile();
	# print Dumper(@surfaceValues);
    
	# my $filename; # this will be the city in the file name
	# file contents
	my $location;
	my $sfc_lat;
	my $sfc_lon;
	my $sfc_elev;

    # get the city from the input file name
	# file name example: 20160401_0100Z_ATDD_BelleMinaAL.txt
	# file example: Belle Mina, AL; 34.689060; -86.883967; 189
    my @file_match_name = split("_", $file);
	my $city = $file_match_name[3];
	$city =~ s/.txt//g;
	
	foreach my $line(@surfaceValues)
	{
        chomp($line);

		my (@location_data) = split(";", $line);

        if ($city =~ /$location_data[0]/)
        {
            print "FOUND MATCHING FILE: $city MATCHES $location_data[0]\n";
			print "LOCATION DATA: @location_data\n";
			$sfc_lat = trim($location_data[1]);
			$sfc_lon = trim($location_data[2]);
			$sfc_elev = trim($location_data[3]);

            last;
		}
    }

    if ($city =~ /AthensAL/i)
	{
		$city = "Athens, AL";
	}
	elsif ($city =~ /BelleMinaAL/i)
	{
		$city = "Belle Mina, AL";
	}
	elsif ($city =~ /CullmanAL/i)
	{
		$city = "Cullman, AL";
	}
	elsif ($city =~ /DecaturAL/i)
	{
		$city = "Decatur, AL";
	}
	elsif ($city =~ /JasperAL/i)
	{
		$city = "Jasper, AL";
	}
	elsif ($city =~ /FlorenceAL/i)
	{
		$city = "St. Florian, AL";
	}
	elsif ($city =~ /UAH/i)
	{
		$city = "UAH SWIRLL, AL";
	}

	$header->setSite($city);

    $header->setLatitude($sfc_lat, $self->buildLatLonFormat($sfc_lat));
	$header->setLongitude($sfc_lon, $self->buildLatLonFormat($sfc_lon)); 
    $header->setAltitude($sfc_elev,"m");

    my $sondeType = "GRAW DFM-09";
	$header->setLine(5,"Radiosonde Type:", ($sondeType));
	my $groundStationSoftware = "Version 5.9.3.10";
	$header->setLine(6,"Ground Station Software:", ($groundStationSoftware));
	# $header->setVariableParameter(1, "Alt","mAGL");
	$header->setVariableParameter(2, "MixR","g/kg");

    # ----------------------------------------------------------
    # Extract the date and time information from the file name
	# Expects filename similar to 2011100100Z.csv (2011-10-01 00 UTC)
	# 20160324_1700Z_ATDD_BelleMinaAL.txt 
    # ----------------------------------------------------------
    # print "file name = $filename\n"; 
    my $date;
	my $time;

	if ($filename =~ /(\d{4})(\d{2})(\d{2})_(\d{2})(\d{2})Z/)
	{
		my ($year, $month, $day, $hour, $minute) = ($1,$2,$3,$4,$5);
	    $date = join ", ", $year, $month, $day;
	    $time = join ":", $hour,$minute,'00';

        $header->setActualRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
        $header->setNominalRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
	}

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

    
	# Generate the sounding header.
	my $header = $self->parseHeader($file);
    
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
	my @file_match = split("_", $file);
	my $locReference = $file_match[3];
	$locReference =~ s/.txt//g;

    my $outfile;
	my ($hour, $min, $sec) = split (/:/, $header->getActualTime());

   	$outfile = sprintf("%s_%s_%04d%02d%02d%02d%02d.cls", 
					   	   $header->getId(),$locReference,
					   	   split(/,/,$header->getActualDate()),
					   	   $hour, $min);
 
    printf("\tOutput file name is %s\n", $outfile);


	open(my $OUT,">".$self->{"OUTPUT_DIR"}."/".$outfile)
	    or die("Can't open output file for $file\n");

	print($OUT $header->toString());
	
    # ----------------------------------------
    # Needed for code to derive ascension rate
    # ----------------------------------------
    # my $prev_time = 9999.0;
    # my $prev_alt = 99999.0;
	# Initial values will be the surface altitude
	# provided by Scot for time = 0
    my $prev_time = 0.0;
    my $prev_alt = $header->getAltitude();

    # ---------------------------------------------
    # Needed for code to derive geopotential height
    # ---------------------------------------------
    my $previous_record;
    my $geopotential_height;

    # ----------------------------------------------------
    # Parse the data portion of the input file
    # ----------------------------------------------------
	my $index = 0;
	my $fake_surface_time = 0;
	my $raw_data_time;
	my $fake_surface_data = 1;
    
    # Now grab the data from each lines
	foreach my $line (@lines) 
	{
        # Skip any blank lines.
		next if ($line =~ /^\s*$/);
        
		my $record = ClassRecord->new($WARN,$file);
		
		# if ($index == 3)
		if ($index >= 3)
		{
			if ($fake_surface_data)
			{
	            # set surface values provided by Scot here
				# all other values set to missing
			    $record->setTime($fake_surface_time);
	        	$record->setLatitude($header->getLatitude(),
			    	    $self->buildLatLonFormat($header->getLatitude()));
		        $record->setLongitude($header->getLongitude(),
				        $self->buildLatLonFormat($header->getLongitude()));
        	    $record->setAltitude($header->getAltitude(),"m");

				printf($OUT $record->toString());
				$fake_surface_data = 0;
			}
			
			
			chomp($line);
		    my @data = split(',',$line);
	    	
			$data[0] = trim($data[0]); # lat
		    $data[1] = trim($data[1]); # lon
	    	$data[2] = trim($data[2]); # time
			$data[2] =~ s/^\d\d\d\d//g; #strip off the HHMM digits
		    $data[3] = trim($data[3]); # height (m)
	    	$data[4] = trim($data[4]); # press (mb)
		    $data[5] = trim($data[5]); # temp (deg C)
		    $data[6] = trim($data[6]); # mixing ratio (g/kg)
			$data[7] = trim($data[7]); # wind spd (m/s)
			$data[8] = trim($data[8]); # wind dir (deg)

            #-----------------------------------------
			# RH and dewpoint must be calculated
			#-----------------------------------------
			my $sat_vapor_press;
			my $temp;
			my $calculated_RH;
			my $dewpoint;
			my $mixing_ratio;
			my $saturated_mixing_ratio;
		
        	#-------------------------------------
			# The first data line is index = 3.
			# We need to get that time value for
			# our "real" start time, then assume
			# 1 second increments, so increment
			# $raw_data_time by one for lines
			# with $index > 3
			#-------------------------------------
			if ($index == 3)
			{                                                 
				$raw_data_time = $data[2];
		    	$record->setTime($raw_data_time);
				$raw_data_time++;
			}
			elsif ($index > 3)
			{
				$record->setTime($raw_data_time);
				$raw_data_time++;
			}
		    $record->setPressure($data[4],"mb") if ($data[4] !~ /32768/);
		    $record->setTemperature($data[5],"C") if ($data[5] !~ /32768/);
			
			#------------------------------------
			# Must calculate RH and dewpoint
			#------------------------------------
			if ($data[5] !~ /^999/)
			{
            	# first calculate saturated vapor 
				$temp = $data[5];
				$mixing_ratio = $data[6];
			 	$sat_vapor_press = 6.112 * exp((17.62 * $temp)/(243.12 + $temp)); # in mb/hPa
                # derive the saturated mixing ratio Ws ($data[4] is pressure)
				$saturated_mixing_ratio = 621.97 * ($sat_vapor_press/($data[4]-$sat_vapor_press));
			   	$calculated_RH = 100 * ($mixing_ratio/$saturated_mixing_ratio);
			   	# print "Calculated RH = $calculated_RH\n";
		    	$record->setRelativeHumidity($calculated_RH);
			}
			else
			{
			   	print "WARNING: Cannot calculate RH because temperature is missing value\n";
			}
            # Calculated RH should never be "missing" since we check
			# the factors in the calculation to make sure they
			# are not missing.
			if (($temp =~ /^999/) || ($calculated_RH =~ /^-999/))
			{
				print "Unable to calculate DewPoint due to missing temp or RH\n";
			}
			else
			{
				$dewpoint = calculateDewPoint($temp, $calculated_RH); #Dewpoint Temp (C)
	    		$record->setDewPoint($dewpoint,"C");    
			}

			$record->setWindSpeed($data[7],"m/s") if ($data[7] !~ /32768/);
			$record->setWindDirection($data[8]) if ($data[8] !~ /32768/);

			# Insert Ele (Elevation Angle) and Azi (Azimuth Angle) data
			# # For setVariableValue(index, value):
			# # index (1) is Ele column, index (2) is Azi column.
			# Variable 1 is Height (above ground)- this turned out to be too large 
			# to fit the column, so we did not use this data
			# Variable 2 is Mixing Ratio
			# THIS IS FOR THE MIXING RATIO
			# See header->setVariableParameter to change column header
			# $record->setVariableValue(1, $data[3]) unless ($data[3] =~ /\/+/);
			$record->setVariableValue(2, $data[6]) unless ($data[6] =~ /\/+/);

	        $record->setLatitude($data[0], $self->buildLatLonFormat($data[0]));
		    $record->setLongitude($data[1], $self->buildLatLonFormat($data[1]));

            #-------------------------------------------------------------
			# Calculate geopotential height (this code from VORTEX2 SUNY)
            #
            #-------------------------------------------------------------
            # BEWARE:  For VORTEX2 (2009) SLoehrer says there are issues 
            # with the raw data altitudes, so compute the geopotential 
			# height/altitude and insert for all other than surface record.
            # call calculateAltitude(last_press,last_temp,last_dewpt,last_alt,
		    #                        this_press,this_temp,this_dewpt,1)
			# Note that the last three parms in calculateAltitude
            # are the pressure, temp, and dewpt (undefined for this dataset)
            # for the current record. To check the altitude calculations, see
            # the web interface tool at 
            #
            # http://dmg.eol.ucar.edu/cgi-bin/conversions/calculations/altitude
            #------------------------------------------------

            if ($index == 3)
			{
				# altitude = height $data[3] + surface value 
				my $oneSecAlt = $data[3] + $header->getAltitude();
	    	    $record->setAltitude($oneSecAlt,"m");

			}
			elsif ($index > 3)
			{
	            if ($debug) 
    	        { 
					my $prev_press = $previous_record->getPressure(); 
            		my $prev_temp = $previous_record->getTemperature(); 
                	my $prev_alt = $previous_record->getAltitude();
	
    	            print "\nCalc Geopotential Height from previous press = $prev_press, temp = $prev_temp, alt = $prev_alt,\n";
					print "and current press = $data[4] and temp = $data[5]\n"; 
            	}
                # print "INDEX: $index\n";
	            if ($previous_record->getPressure() < 9990.0)
    	        {
        	        if ($debug){ print "prev_press < 9990.0 - NOT missing so calculate the geopotential height.\n"; }
 
            	    $geopotential_height = calculateAltitude($previous_record->getPressure(),
                	                                         $previous_record->getTemperature(), 
															 undef, $previous_record->getAltitude(), 
															 $data[4], $data[5], undef, 1);
	                if (defined($geopotential_height))
					{
	    	            $record->setAltitude($geopotential_height,"m");
					}
					else
					{
						print "WARNING: Was not able to calculate geopotential height\n";
						$geopotential_height = 99999.0;
					}
	            }
    	        else
        	    {
            	    if ($debug_geopotential_height){print "WARNING: prev_press > 9990.0 - MISSING! Set geopot alt to missing.\n"; }
	                $geopotential_height = 99999.0;
    	        }
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
	        if ($index >= 3)
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

        	#---------------------------------------------
	        # Only save off current rec as previous rec
    	    # if not completely missing. This affects
        	# the calculations of the geopotential height,
	        # as the previous height must be non-missing. 
    	    #---------------------------------------------
        	if ($debug) 
			{
				my $press = $record->getPressure(); 
				my $temp = $record->getTemperature();
            	my $alt = $record->getAltitude();
	            print "Current Rec: press = $press, temp = $temp, alt = $alt\n";
    	    }
        	if ( ($record->getPressure() < 9999.0)  && ($record->getTemperature() < 999.0)
	             && ($record->getAltitude() < 99999.0) )
			{
				if ($debug) { print "Set previous_record = record and move to next record.\n\n"; }
            	$previous_record = $record;
	        } 
			else 
			{
				if ($debug) 
				{
					print "Do NOT assign current record to previous_record! ";
					print "Current record has missing values.\n\n";
		    	}
	        }
		    
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
    my @files = grep(/.txt$/,sort(readdir($RAW)));
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

    open(my $FILE, sprintf("lat-lon-elev.txt")) or die("Can't read file into array\n");
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
