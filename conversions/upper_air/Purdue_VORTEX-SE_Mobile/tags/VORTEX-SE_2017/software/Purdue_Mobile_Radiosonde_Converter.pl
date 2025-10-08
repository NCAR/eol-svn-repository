#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The Purdue_Mobile_Radiosonde_Converter.pl script is used for converting 
# high resolution radiosonde data from a csv format to the EOL Sounding 
# Composite (ESC) format.</p> 
#
# INPUTS: Sounding files that have an expected name of the following:
# 		2017-03-25_1557.template.csv
#	  These files are in a columnar ASCII format with header lines 1-2
#	  The data will start on line 3
#	  BEWARE: Since these files have a *.csv extention, dos2unix is ran first
#
# IMPORTANT NOTES: 
# 	HEADER NOTES:
# 	1. We will use the UTC time from the first data record as our release time
# 	2. The lat/lon for the header will come from the first data record
# 	3. The header elevation comes from a separate file: Purdue_VSE2017_locations.txt
# 		This file is copied over to the software directory - the same one as this script
# 	
# 	FILE NOTES:
# 	1. There is no surface data for these soundings. The zero-second will be taken from the first record
# 	2. The altitude for the zero-second record should be the first altitude value - this will be DIFFERENT from the header
# 	3. The UTC time column will be used to derive our seconds from release
# 	4. Geopotential height and ascention rate are to be calculated
#	5. Search for HARDCODED - this code will contain hard coded values that will need to be changed based on project specs
#	6. The UTC time column has the next day processing in a few of the files. There is code to ask if the day is the next day.
#
#
# OUTPUTS: The soundings in the EOL Sounding Composite (ESC) format.
#
# LOOK FOR HARDCODED to change for project specs
#
# @author Alley Robinson 2/14/2018
# @version VORTEX-SE 2017 Purdue Mobile Soundings
# 	- These soundings are in a new columar ASCII format and have different conversion requirements, so this code will be completely updated
# 	(see notes above for details)
#
# @author Linda Echo-Hawk 4 Nov 2016
# @version VORTEX-SE_2016 Purdue Mobile Soundings
#          based on the MSU_WindSond_Mobile_Converter.pl
#          - A separate text file (Purdue_elev.txt) is read in to
#            find the surface elevation for each raw data file
#          - From Scot: Don't assume 10 second intervals, use their 
#            UTC time from launch field to derive our time field.  Note 
#            that their field is not actually time from launch, but 
#            rather actual UTC time at each level, so our time field 
#            will need to be derived. Code is included to derive the time.
#          - For the release location, the converter uses the lat/lon from
#            the first data record in the raw files and the elevation
#            from the Purdue_elev.txt file. 
#          - The release time is obtained from the file name.
#          - The station is taken from the file name (as read in the
#            Purdue_elev.txt file).
#          - RH and dewpoint had to be calculated from the 
#            mixing ratio using equations provided by Scot.
#          - The code used "setVariableValue" to include a column
#            for the mixing ratio values.
#          - Geopotential height is calculated by the converter.
#          - Ascension rate is calculated by the converter.
#          - The location from the text file served as the Site
#            value for the header.
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
package Purdue_Mobile_Radiosonde_Converter;
use strict;

if (-e "/net/work") 
{
    use lib "/net/work/lib/perl/Utilities";
    use lib "/net/work/lib/perl/UpperAir";
    use lib "/net/work/lib/perl/Station";
}

else 
{
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

printf "\nPurdue_Mobile_Radiosonde_Converter.pl began on ";print scalar localtime;printf "\n";

# Debug Flags
my $debug                     = 0;
my $debug_geopotential_height = 0;
my $debug_details             = 0;

&main();

printf "\nPurdue_Mobile_Radiosonde_Converter.pl ended on ";print scalar localtime;printf "\n";

my $filename;

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the Purdue Mobile radiosonde data by converting it from 
# the native ASCII format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main 
{
    my $converter = Purdue_Mobile_Radiosonde_Converter->new();
    $converter->convert();
}


##------------------------------------------------------------------------------
# @signature Purdue_Mobile_Radiosonde_Converter new()
# <p>Create a new instance of a Purdue_Mobile_Radiosonde_Converter.</p>
#
# @output $self A new Purdue_Mobile_Radiosonde_Converter object.
##------------------------------------------------------------------------------
sub new 
{
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);
    
    $self->{"stations"} = SimpleStationMap->new();

    # HARDCODED Section below:
    $self->{"PROJECT"} = "VORTEX-SE_2017";
    $self->{"NETWORK"} = "Purdue";
    
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
# @signature void convert()
# <p>Convert the raw data to the ESC format.</p>
##------------------------------------------------------------------------------
sub convert 
{
    my ($self) = @_;
    
    mkdir($self->{"OUTPUT_DIR"}) unless (-e $self->{"OUTPUT_DIR"});
    mkdir($self->{"FINAL_DIR"}) unless (-e $self->{"FINAL_DIR"});
    
    $self->readDataFiles();
    $self->printStationFiles();
}

##------------------------------------------------------------------------------
# @signature ClassHeader parseHeader(String file, String[] lines)
# <p>Parse the header lines from/ the specified file into the ESC header format.</p>
#
# @input $file The name of the file being parsed.
# @input $lines[] The header lines to be parsed.
# @output $header The header data in ESC format.
##------------------------------------------------------------------------------
sub parseHeader 
{
	my ($self,$file,@headerlines) = @_;
	my $header = ClassHeader->new();

	# HARDCODED Section Below:
	
	# Sounding Type: 
	$header->setType("Purdue Mobile Sounding Data");
	$header->setReleaseDirection("Ascending");
	$header->setProject($self->{"PROJECT"});

	# The Id will be the prefix of the output file 
	$header->setId("Purdue");


	# Read in the locations file for the header elevation
	my @surfaceValues = &readHeaderElevationValuesFile();

	my $citystate;
	my $sfc_elev;
	my $sfc_lat;
	my $sfc_lon;

	foreach my $line(@surfaceValues)
	{
		
		# find out if the line in the surface values file applies to what you are processing
		chomp($line);
		my $string_file_name = substr $line, 1, 28;
		my $file_match_name  = trim($string_file_name);
		
		# City and State for the header:		

		if ($file =~ /$file_match_name/i)
                {
                        print "\tFILE $file matches FILE_MATCH_NAME $file_match_name\n";

			#-------------------------------------------------------------------------------------
			# $line:
			# 2017-03-25_1557.template.csv 34.480122   -87.306031 192 Moulton, AL
			#-------------------------------------------------------------------------------------

			# HARDCODED - substring to get the city and state
			my $string_city_state = substr $line, 55;
			if ($debug) { print "City State String: $string_city_state \n"; } 
	
			my @city_state_data = split(',', $string_city_state);
			my $city = trim($city_state_data[0]);
			my $state = trim($city_state_data[1]);
		
			if ($debug) { print "City: $city_state_data[0] State: $city_state_data[1] \n"; } 
	
			$citystate = join ", ", $city, $state; 

			# Elevation for the header:
			$sfc_elev = substr $line, 51, 4;
	
			if ($debug) { print "CityState: $citystate \n Alt: $sfc_elev \n"; }
			last;
		}
	}

	# Site:
	$header->setSite($citystate);

	# Elevation:
	$header->setAltitude($sfc_elev, "m");	

	# HARDCODED - Latitude and Longitude for the Header: 
        my @headerData = split(" ",$headerlines[2]);
       
        if($debug) { print "Header Data Line: @headerData \n"; }

	my $string_lat_lon = substr $headerlines[2], 343; 
	if ($debug) { print "String Lat Lon: $string_lat_lon \n"; }

	my @lat_lon_data = split(' ', $string_lat_lon);

	# Latitude:
	if($lat_lon_data[0])
	{
		$sfc_lat = trim($lat_lon_data[0]);
        }
	else 
	{
		$sfc_lat = 999;
	}

	$header->setLatitude($sfc_lat, $self->buildLatLonFormat($sfc_lat));        

	#Longitude:
	if ($lat_lon_data[1])
	{
		$sfc_lon = trim($lat_lon_data[1]);
        }
	else
	{
		$sfc_lon = 999;
	}

	$header->setLongitude($sfc_lon, $self->buildLatLonFormat($sfc_lon));

	print "Lat: $sfc_lat Lon: $sfc_lon \n";

	# ----------------------------------------------------------
	# Extract the date from the file name
	# Expects filename similar to: 2017-03-25_1557.template.csv
	# The time header will come from the UTC time of the first record
	# ----------------------------------------------------------
	my $date;
	my $sfc_time;

	# Date and Time:
	if ($file =~ /(\d{4})-(\d{2})-(\d{2})/)
	{
		my ($year, $month, $day) = ($1,$2,$3);
		$date = join ", ", $year, $month, $day;
		
		$sfc_time = trim($headerData[0]);
		
		$header->setActualRelease($date,"YYYY, MM, DD",$sfc_time,"HH:MM:SS",0);
		$header->setNominalRelease($date,"YYYY, MM, DD",$sfc_time,"HH:MM:SS",0);
	}


	# -------------------------------------------------
	# Other header info provided by Scot
	# -------------------------------------------------
	
	# HARDCODED Section below:
	
	# Radiosode Type:
	my $sondeType = "Windsond S1H3-S";
	$header->setLine(5,"Radiosonde Type:", ($sondeType));

	# Gound Station Software:
	my $groundStationSoftware = "RR1 Radio Receiver Software Version 2.80";
	$header->setLine(6,"Ground Station Software:", ($groundStationSoftware));

	# Mixing Ratio:
	$header->setVariableParameter(2, "MixR","g/kg");


	return $header;
} # end parseHeader
                           
##------------------------------------------------------------------------------
# @signature void parseRawFiles(String file)
# <p>Read the data from the specified file and convert it to the ESC format.</p>
#
# @input $file The name of the file to be parsed.
##------------------------------------------------------------------------------
sub parseRawFile {
	my ($self,$file) = @_;

	printf("\nProcessing file: %s\n",$file);

	# Open the file: 
	open(my $FILE,$self->{"RAW_DIR"}."/".$file) or die("Can't open file: ".$file);
	my @lines = <$FILE>;
	close($FILE);

	my @headerlines = @lines[0..5];

	# Generate the sounding header.
	my $header = $self->parseHeader($file, @headerlines);

	# Only continue processing the file if a header was created.
	if (defined($header)) 
	{
		# Determine the station the sounding was released from.
		my $station = $self->{"stations"}->getStation($header->getId(),$self->{"NETWORK"});

		if (!defined($station)) 
		{
			$station = $self->buildDefaultStation($header->getId(),$self->{"NETWORK"});
			$self->{"stations"}->addStation($station);
		}

		$station->insertDate($header->getNominalDate(),"YYYY, MM, DD");


		# Create the output file name and open the output file
		my $outfile;
		my ($hour, $min, $sec) = split (/:/, $header->getActualTime());

		$outfile = sprintf("%s_%04d%02d%02d%02d%02d.cls", 
				   $header->getId(),
				   split(/,/,$header->getActualDate()),
				   $hour, $min);

		printf("\tOutput file name is %s\n", $outfile);

		# Open the outfile:
		open(my $OUT,">".$self->{"OUTPUT_DIR"}."/".$outfile)
			or die("Can't open output file for $file\n");

		print($OUT $header->toString());

		
		# Set up to derive ascention rate
		# Initial values will be the surface altitude provided for time = 0
		my $prev_time = 0.0;
		my $prev_alt = $header->getAltitude();

		# Set up to derive geopotential height
		my $previous_record;
		my $geopotential_height;

		# ----------------------------------------------------
		# Parse the data portion of the input file
		# ----------------------------------------------------
		my $index = 0;
		my $fake_surface_time = 0;
		my $raw_data_time;
		my $prev_raw_data_time;
		my $derived_time;
		my $surfaceRecord = 1;
		

		# Now grab the data from each line
		foreach my $line (@lines) 
		{
			
			# Skip any blank lines.
			next if ($line =~ /^\s*$/);

			my $record = ClassRecord->new($WARN,$file);
			
			# Initializing variables for missing values:
			my $windDir;
			my $mixing_ratio;
				
		if($index >= 2)
		{
			chomp($line);
    			my @data = split(' ',$line);

						
				# Put the data into the array!! 			
				# ASSUMPTION: The data is split as followed 
				# $data[0]  UTC                16:55:28 
				# $data[1]  Local              12:55:28 
				# $data[2]  Pressure mb        421.69 
				# $data[3]  Pressure Pa        42168 
				# $data[4]  Temperature        -17.14 (C)
				# $data[5]  Relative Humidity  51.0   (%)
				# $data[6]  Wind Speed 	       24.42  (m/s)
				# $data[7]  Wind Direction     221.5  (True Degrees)
				# $data[8]  Wind Heading       41.5   (True Degrees)
				# $data[9]  Wind Heading       38.4   (magnetic degrees)
				# $data[10] Dew Point          -24.89(C)  
				# $data[11] Mixing ratio       1.10  (g/kg)
				# $data[12] Density            0.628 (kg/m3)
				# $data[13] Alt                7224   (m MSL)
				# $data[14] Height m AGL       7010 
				# $data[15] Height ft          23000 
				# $data[16] Lat                35.022426 
				# $data[17] Lon                -87.059009
				#
				# We will use the following data values for the output:
				# Pressure, Temperature, RH, Wind Speed, Wind Dir, Dew Point, Mixing Ratio, Lat, Lon
				# $data[2]  $data[4]  $data[5] $data[6]  $data[7]  $data[10]  $data[11]    $data[16] $data[17]				

		
				# Pressure
				if($data[2])
				{
					$data[2] = trim($data[2]);
				}
				else
				{
					$data[2] = 999;
				}
				
				$record->setPressure($data[2],"mb") if ($data[2] !~ /^999$/);
			
				# Mixing Ratio
				if($data[11])
				{
					$mixing_ratio = trim($data[11]); 
				}
				else
				{
					$mixing_ratio = 999;
				}		
				
				# Time: We will use the UTC time to calculate seconds from release
				$raw_data_time = $data[0];
	                        $raw_data_time =~ s/://g;
				my $time_diff;

				# This time calculation takes the next day into consideration
				if ($raw_data_time =~ /(\d{2})(\d{2})(\d{2})/)
				{
					$raw_data_time = ($1 * 3600)+($2 * 60)+($3); # conversion to seconds
				}		
				if ($index == 2)
				{
					$record->setTime($fake_surface_time); 
					$prev_raw_data_time = $raw_data_time; 
				}			
				elsif ($index > 2)
				{	
					if($prev_raw_data_time > $raw_data_time)
					{
						$time_diff = $raw_data_time + (86400 - $prev_raw_data_time);
					}
					else
					{
						$time_diff = $raw_data_time - $prev_raw_data_time;
					}
	
					$derived_time += $time_diff;

					$record->setTime($derived_time);
					$prev_raw_data_time = $raw_data_time;
					
					print "Derived time: $derived_time \n";
				}
				
				# Surface record:
				if ($surfaceRecord)
				{	#This file is different from the others:
					
					my $length = @data;
					if ($debug) { print "Length of array: $length \n"; }

					if ($file =~ /2017-03-25_1557.template.csv/)
					{
						print "$line \n";
						# Latitude for file:
						my $temp_lat = trim($data[7]);
						$record->setLatitude($temp_lat, $self->buildLatLonFormat($temp_lat));
						
						# Longitude for file:
						my $temp_lon = trim($data[8]);
						$record->setLongitude($temp_lon, $self->buildLatLonFormat($temp_lon));

						# Temperature for file:
						my $temp_temp = 23.00;
						$record->setTemperature($temp_temp,"C");

						# Relative Humidity for file:
						my $temp_rh = 48.5;
						$record->setRelativeHumidity($temp_rh);

						# Dew Point for file:
						my $temp_dewPt = 11.56;
						$record->setDewPoint($temp_dewPt,"C");
						
						# Wind Speed for file:
						my $temp_wind_spd = 999;
						$record->setWindSpeed($temp_wind_spd,"m/s");

						# Wind Direction for file:
						my $temp_wind_dir = 999;
						$record->setWindDirection($temp_wind_dir);

						# Altitude:
						my $alt = trim($data[4]);
						$record->setAltitude($alt ,"m");
					
					}

					elsif ($file =~ /2017-04-05_1853.template.csv/)
					{
						# Latitude for file:
						my $temp_lat2 = trim($data[11]);
						$record->setLatitude($temp_lat2, $self->buildLatLonFormat($temp_lat2));

						# Longitude for file:
						my $temp_lon2 = trim($data[12]);
						$record->setLongitude($temp_lon2, $self->buildLatLonFormat($temp_lon2));
						
						# Temperature for file:
						my $temp_temp2 = 20.71;
						$record->setTemperature($temp_temp2,"C");

						# Reletive Humidity for file:
						my $temp_rh2 = 68.9;
                                                $record->setRelativeHumidity($temp_rh2);
							
						# Dew Point:
						$data[5] = trim($data[5]);
						$record->setDewPoint($data[5],"C");

						# Mixing Ratio
						$mixing_ratio = trim($data[6]);

						# Wind Speed (missing):
					 	my $temp_wind_spd2 = 999;
                                                $record->setWindSpeed($temp_wind_spd2,"m/s");

						# Wind Direction (missing):
						my $temp_wind_dir2 = 999;
                                                $record->setWindDirection($temp_wind_dir2);

						# Altitude:
						my $alt2 = trim($data[8]);
						$record->setAltitude($alt2 ,"m");
					}

					else # if the file isn't that one
					{
					
						if ($length == 15) # wind direction/heading are missing
						{
							# Temperature:
							$data[4] = trim($data[4]);
							$record->setTemperature($data[4],"C") if ($data[4] !~ /^999$/);

							# Relative Humidity:
							$data[5] = trim($data[5]);
							$record->setRelativeHumidity($data[5]);

							# Wind Speed:
							my $missing_wind_speed = 999;
							$record->setWindSpeed($missing_wind_speed,"m/s");

							# Wind Direction:
							my $missing_wind_dir = 999;
							$record->setWindDirection($missing_wind_dir);

							# Mixing Ratio
							$mixing_ratio = trim($data[8]);
			
							#TODO Altitude
							$data[10] = trim($data[10]);
							$record->setAltitude($data[10] ,"m");

							# Latitude:
							if($data[13])
							{
								$data[13] = trim($data[13]);
							}
							else
							{
								$data[13] = 999;
							}
							$record->setLatitude($data[13], $self->buildLatLonFormat($data[13]));

							#Longitude:
							if($data[14])
							{
								$data[14] = trim($data[14]);
							}
							else
							{
								$data[14] = 999;
							}
							$record->setLongitude($data[14], $self->buildLatLonFormat($data[14]))

						}
						else
						{
							# Latitude:
							if($data[16])
							{
								$data[16] = trim($data[16]);
                                			}
							else
							{
								$data[16] = 999;
							}
	
							$record->setLatitude($data[16], $self->buildLatLonFormat($data[16]));
		
							# Longitude:
							if ($data[17])
							{
								$data[17] = trim($data[17]);
							}
							else
							{
								$data[17] = 999;
							}
	
							$record->setLongitude($data[17], $self->buildLatLonFormat($data[17]));

							# Temperature:
							$data[4] = trim($data[4]);
   		        	                        $record->setTemperature($data[4],"C") if ($data[4] !~ /^999$/);
		
							# Relative Humidity:				
							$data[5] = trim($data[5]);
		                	                $record->setRelativeHumidity($data[5]);

							# Dew Point:
							$data[10] = trim($data[10]);
		                       		        $record->setDewPoint($data[10],"C");
				
							# Wind Speed:
							if ($data[6])
							{
								$data[6] = trim($data[6]);
							}
							else
							{
								$data[6] = 999;
							}
		                       		         $record->setWindSpeed($data[6],"m/s") if ($data[6] !~ /^999$/);

							# Wind Direction: 
		                       		         if ($data[7])
        		               		         {
                        	        	        	$data[7] = trim($data[7]);
                                	 		        $windDir = $data[7]
                         		 	         }
							elsif ($data[6] = 0.0)
							{
								$windDir = 999;
							}
							else
							{
								$windDir = 999;
							}

                                			$record->setWindDirection($windDir);

							# Altitude:
							$data[13] = trim($data[13]);
                                        		$record->setAltitude($data[13] ,"m");

						} #end else
					}
					$surfaceRecord = 0;
				}
				else #if the record is not the surface record 
				{
					# Latitude:
					if ($data[16])
					{
						$data[16] = trim($data[16]);
                                        }
					else
					{
						$data[16] = 999;
					}

					$record->setLatitude($data[16], $self->buildLatLonFormat($data[16]));

					# Longitude
					if ($data[17])
					{
						$data[17] = trim($data[17]);
                                        }
					else
					{
						$data[17] = 999;
					}

					$record->setLongitude($data[17], $self->buildLatLonFormat($data[17]));

					# Temperature:
					$data[4] = trim($data[4]);
                                        $record->setTemperature($data[4],"C") if ($data[4] !~ /^999$/);
					 
					# Relative Humidity:
					$data[5] = trim($data[5]);
                                        $record->setRelativeHumidity($data[5]);

					# Dew Point
					$data[10] = trim($data[10]);
                                        $record->setDewPoint($data[10],"C");
					
					# Wind Speed:
					if($data[6])
					{
						$data[6]   = trim($data[6]);
	                                }
					else
					{
						$data[6] = 999;
					}

					$record->setWindSpeed($data[6],"m/s") if ($data[6] !~ /^999$/);

					# Wind Direction:
	                                if ($data[7])
        	                        {
                	                        $data[7] = trim($data[7]);
                        	                $windDir = $data[7]
                               		}
                                        elsif ($data[6] = 0.0)
                                        {
                                                $windDir = 999;
                                        }
                                        else
                                        {
                                                $windDir = 999;
                                        }

                                	$record->setWindDirection($windDir);
				
				}

				# Altitude for geopotential Height
				if($data[13])
				{
					$data[13] = trim($data[13]);
				}
				else
				{
					$data[13] = 999;
				}

				# Insert Ele (Elevation Angle) and Azi (Azimuth Angle) data
			#	$record->setVariableValue(1, $data[14]) unless ($data[14] =~ /\/+/);

				# Mixing ratio data:
				$record->setVariableValue(2, $mixing_ratio) unless ($mixing_ratio =~ /^999$/); 
					
			
				#-------------------------------------------------------------
				# Calculate geopotential height (this code from VORTEX2 SUNY) 
				#-------------------------------------------------------------

				# Note that the last three parms in calculateAltitude
				# are the pressure, temp, and dewpt (undefined for this dataset)
				# for the current record. To check the altitude calculations, see
				# the web interface tool at 
				# http://dmg.eol.ucar.edu/cgi-bin/conversions/calculations/altitude
				#-------------------------------------------------------------------
			
				if ($index > 2)
				{
				        if ($debug_geopotential_height) 
					{ 
						my $prev_time = $previous_record->getTime();
						my $prev_press = $previous_record->getPressure(); 
						my $prev_temp = $previous_record->getTemperature(); 
						my $prev_alt = $previous_record->getAltitude();

						if($debug_details) {    print "\nCalc Geopot. Height  from prev press = $prev_press, temp = $prev_temp, alt = $prev_alt,\n";
						print "and current press = $data[2] and temp = $data[4] at t = $prev_time\n"; }

					}	
				

					# NOTE: Do not calculate geopotential height without 
					# valid pressure. Scot says this is one of the most
					# important factors, so must not be "missing." More
					# discussion indicates that a check for valid previous 
					# altitude would also indicate a valid prev pressure.
					# Current pressure is also required.
					# --------------------------------------------------
			      		if (($previous_record->getPressure() < 999.0) && ($record->getPressure() < 999.0))
					{
						if ($debug_geopotential_height){ print "prev_press < 999.0 - NOT missing\n"; }
						$geopotential_height = calculateAltitude($previous_record->getPressure(),
													 $previous_record->getTemperature(), 
													 undef, $previous_record->getAltitude(), 
													 $data[2], $data[4], undef, 1);


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
				} # end elseif Geopotential height calculation		


				#-------------------------------------------------------
				# Calculate the ascention rate
				#-------------------------------------------------------
				# this code from Ron Brown converter:
				# Calculate the ascension rate which is the difference
				# in altitudes divided by the change in time. Ascension
				# rates can be positive, zero, or negative. But the time
				# must always be increasing (the norm) and not missing.
	
				# Only save off the next non-missing values.
				# Ascension rates over spans of missing values are OK.
				#-------------------------------------------------------
				if ($index >= 2)
				{
	
					if ($debug) 
					{
						my $time = $record->getTime(); my $alt = $record->getAltitude(); 
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

		   				if ($debug) { print " has valid Time and Alt.\n"; }
    					}		
				} # end if ($index >= 2) for the calculate ascension rate


				#---------------------------------------------
				# Only save current rec as previous rec
				# if not completely missing. This affects
				# the calculations of the geopotential height,
				# as the previous height must be non-missing. 
				#---------------------------------------------
				# NOTE that a more correct name for $previous_record
				# would be $last_valid_record.
				if ($debug_details) 
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
					if ($debug) { print "Do NOT assign current record to previous_record! ";}
				}
			
    
			printf($OUT $record->toString());
		} #end if index >= 2
			$index++;
		
		} # end foreach $line

	} # end if (defined($header))
	
	else
	{
		printf("Unable to make a header\n");
	}

} #end parseRawFile


##------------------------------------------------------------------------------
# @signature void readDataFiles()
# <p>Read in the files from the raw data directory and convert each into an
# ESC formatted file.</p>
##------------------------------------------------------------------------------
sub readDataFiles 
{
    my ($self) = @_;
    
    opendir(my $RAW,$self->{"RAW_DIR"}) or die("Can't read raw directory ".$self->{"RAW_DIR"});
    
    my @files = grep(/^2017.+csv$/,sort(readdir($RAW))); #HARDCODED year 2017 and .csv
    closedir($RAW);
        
    open($WARN,">".$self->{"WARN_LOG"}) or die("Can't create ".$self->{"WARN_LOG"});
    
    if ($debug) { printf("Ready to read the files\n"); }
    foreach my $file (@files) 
    {
	$self->parseRawFile($file);
    }
    
    close($WARN);
}

#------------------------------------------------------------------------
# @signature void readHeaderElevationValuesFile(file_name)
#<p>Read the contents of the file into an array.</p>
#
# This will give the location header elevation data from a separate file.
#
#@input $file_name The name of the data file to be read
#@output array of elevation header values (name, lat, lon, elev, location)
#------------------------------------------------------------------------
sub readHeaderElevationValuesFile 
{
    my $self = shift;

    open(my $FILE, sprintf("Purdue_VSE2017_locations.txt")) or die("Can't read file into array\n");
    my @header_elev_data = <$FILE>;
    close ($FILE);

    return @header_elev_data;
}


#-------------------------------
#*******************************
#    General Subroutines
#*******************************
#-------------------------------


##------------------------------------------------------------------------------
# @signature String trim(String line)
# <p>Remove all leading and trailing whitespace from the specified String.</p>
#
# @input $line The String to be trimmed.
# @output $line The trimmed String.
##------------------------------------------------------------------------------
sub trim 
{
    my ($line) = @_;
    return $line if (!defined($line));
    $line =~ s/^\s+//;
    $line =~ s/\s+$//;
    return $line;
}

##------------------------------------------------------------------------------
## @signature Station buildDefaultStation(String station_id, String network)
## <p>Create a default station for the Purdue network using the
## specified station_id and network.</p>
##
## @input $station_id The identifier of the station to be created.
## @input $network The network the station belongs to.
## @return The new station object with the default values for the network.
###------------------------------------------------------------------------------
sub buildDefaultStation 
{
	my ($self,$station_id,$network) = @_;
        my $station = Station->new($station_id,$network);
        # $station->setStationName($network);

        # info in 48-char field in stationCD.out file
        $station->setStationName("Purdue");
        $station->setStateCode("99");
        $station->setReportingFrequency("no set schedule");
	$station->setNetworkIdNumber("99");
	$station->setPlatformIdNumber(1231);
	$station->setMobilityFlag("m");

	return $station;
}

##------------------------------------------------------------------------------
# @signature void printStationFiles()
# <p>Generate the output files containing station information.</p>
##------------------------------------------------------------------------------
sub printStationFiles 
{
    my ($self) = @_;

    open(my $STN, ">".$self->{"STATION_FILE"}) || 
	die("Cannot create the ".$self->{"STATION_FILE"}." file\n");
    foreach my $station ($self->{"stations"}->getAllStations()) {
        print($STN $station->toString()) if ($station->getBeginDate !~ /^9+$/);
    }
    close($STN);
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
sub buildLatLonFormat 
{
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
sub cleanForFileName 
{
    my ($self,$text) = @_;

    # Convert spaces to underscores.
    $text =~ s/\s+/_/g;

    # Remove all hyphens
    $text =~ s/\-//g;

    return $text;
}
