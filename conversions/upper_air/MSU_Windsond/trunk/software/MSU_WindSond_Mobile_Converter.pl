#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The MSU_WindSond_Mobile_Radiosonde_Converter.pl script is used for converting 
# high resolution radiosonde data from a csv format to the EOL Sounding 
# Composite (ESC) format.</p> 
#
# @inputs Sounding files that have an expected name of the following:
# 		2017-03-25_1529z_MSU.csv
# 	  These files are in CSV format
# 	  Data starts on line 3 (index = 2)
#
# @outputs The sounding files in ESC format
#
# @author Alley Robinson 14 March 2018
# @version VORTEX-SE_2017 MSU Mobile Soundings
# 	IMPORTANT NOTES:
#
#		LOCATION FILE NOTES:
#		1. Had to run the formatFiles script before processing to remove spaces 
#
# 		HEADER NOTES:
#		1. Release site will come from a separate text file (msu_2017_locs.txt)
#		2. Lat/Lon in the header will also come from the above text file.
#		3. UTC release time will come from the time and date in the file name.
#
# 		FILE NOTES:
# 		1. Some files sit at the surface, so Scot provided the time to start the file.
# 		2. Some soundings do not have Temp/RH/wind data until a point, which will be excluded. 
# 			Scot will provide the start time of the files which will solve this issue
# 		3. Some soundings have falling data which will be excluded.
#
# NOTE: Look for HARDCODED and ASSUMPTION sections within the code
#
# @author Linda Echo-Hawk 25 October 2016
# @version VORTEX-SE_2016 MSU WindSond
#          based on the MSU_WindSond_Radiosonde_Converter.pl
#          - A separate text file (MSU_WindSond_locs.txt) provided a list
#            of locations and their surface lat/lon/elev values. This
#            file was read into the converter. 
#          - The lat/lon from the first data record and the elevation from 
#            the MSU_WS_locs.txt file were used for the release location.
#          - The actual release time is in the "UTC time from launch" parameter 
#            in the first data record.  The actual release date is typically 
#            that in the file name (or second header record).  There is one 
#            file 2016-03-13_0000z.txt where the actual release date is the 
#            previous day (due to the actual release time being 2359).
#          - Some files do not have lat/lon on the first record, so the first,
#            second and third records are checked until the values are found.
#          - The data are typically at one second intervals, although the lat/lon 
#            seem to be reported only every three seconds.
#          - There were two WindSond systems and the file names only report a 
#            nominal release time so that's why there are some file names with 
#            a "-2" in them. Code was added to include the system number in the
#            output file name so that some files would not get overwritten.
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
##Module------------------------------------------------------------------------
package MSU_WindSond_Mobile_Radiosonde_Converter;
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

printf "\nMSU_WindSond_Mobile_Radiosonde_Converter.pl began on ";print scalar localtime;printf "\n";

# Debug Flags:
my $debug       	      = 0;
my $debug_header 	      = 0;
my $debug_file_parser 	      = 0;
my $debug_geopotential_height = 0;
my $debug_dewpoint            = 0;
my $debug_ascent              = 0;

&main();
printf "\nMSU_WindSond_Mobile_Radiosonde_Converter.pl ended on ";print scalar localtime;printf "\n";

my $filename;

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the MSU_WindSond Mobile radiosonde data by converting it from 
# the native ASCII format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main 
{
    	my $converter = MSU_WindSond_Mobile_Radiosonde_Converter->new();
    	$converter->convert();
}

##------------------------------------------------------------------------------
# @signature MSU_WindSond_Mobile_Radiosonde_Converter new()
# <p>Create a new instance of a MSU_WindSond_Mobile_Radiosonde_Converter.</p>
#
# @output $self A new MSU_WindSond_Mobile_Radiosonde_Converter object.
##------------------------------------------------------------------------------
sub new 
{
	my $invocant = shift;
    	my $self = {};
    	my $class = ref($invocant) || $invocant;
    	bless($self,$class);
    
    	$self->{"stations"} = SimpleStationMap->new();

    	# HARDCODED section below:
    	$self->{"PROJECT"} = "VORTEX-SE_2017";
    	$self->{"NETWORK"} = "MSU";
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
# <p>Create a default station for the MSU_WindSond network using the 
# specified station_id and network.</p>
#
# @input $station_id The identifier of the station to be created.
# @input $network The network the station belongs to.
# @return The new station object with the default values for the network.
##------------------------------------------------------------------------------
sub buildDefaultStation 
{
 	my ($self,$station_id,$network) = @_;
    	my $station = Station->new($station_id,$network);
   
    	# HARDCODED section below:
    	$station->setStationName("MSU_WindSond");
    	$station->setStateCode("99");
    	$station->setReportingFrequency("no set schedule");
    	$station->setNetworkIdNumber("99");

    	# platform, 1229 Windsond S1H2
   	$station->setPlatformIdNumber(1229);
	$station->setMobilityFlag("m");

    	return $station;
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
# <p>Parse the header lines from the specified file into the ESC header format.</p>
#
# @input $file The name of the file being parsed.
# @input $lines[] The header lines to be parsed.
# @output $header The header data in ESC format.
##------------------------------------------------------------------------------
sub parseHeader 
{
	my ($self,$file,@headerlines) = @_;
    	my $header = ClassHeader->new();

     	# HARDCODED section below:

    	# Set the type of sounding
    	$header->setType("MSU WindSond Mobile Sounding Data");
    	$header->setReleaseDirection("Ascending");
    	$header->setProject($self->{"PROJECT"});
	
	# The Id will be the prefix of the output file
    	# and appears in the stationCD.out file
    	$header->setId("MSU");

    	# Site and Elevation for the header will come from the msu_2017_locs.txt file:
	my @surfaceValues = &readSurfaceValuesFile();	

	my $city;
	my $state;
	my $city_and_state;
	my $citystate;
	my $sfc_lat;
	my $sfc_lon;
	my $sfc_elev;

	foreach my $line(@surfaceValues)
	{
		# skip the first line in the file, it is a header
		next if ($line =~ /filename/);

		# find out if the line in the surface values file
		# applies to the $file you are processing
        	chomp($line);
	
		# Getting the Lat/Lon/Elevation from the surface values file:	   
		
		# ASSUMPTION: the data line looks like this
		# 2017-03-25_1541z_MSU.csv,34.288790, -87.598528,288, Haleyville AL 
		# $location_data[0] = 2017-03-25_1541z_MSU.csv
		# $location_data[1] = 34.288790
		# $location_data[2] = -87.598528
		# $location_data[3] = 288
		# $location_data[4] = HaleyvilleAL
	
		my @location_data = split(",", $line);
		my $file_match_name = trim($location_data[0]);
	
		if ($file =~ /$file_match_name/i)
		{
			print "\tFILE $file matches FILE_MATCH_NAME $file_match_name\n";
			
			# City and State: 
			$city_and_state = trim($location_data[4]);

			# HARDCODED: making city and state separate
			$city = substr $city_and_state, 0, -2;
			$state = substr $city_and_state, -2, 2;			

			$citystate = join ", ", $city, $state;

			# Elevation:
			$sfc_elev  = trim($location_data[3]);
		
			if ($debug_header) { print "CityState: $citystate \n Alt: $sfc_elev \n"; }		    	
			last;
		}
    	}

	# Site:
	$header->setSite($citystate);

	# Elevation:
    	$header->setAltitude($sfc_elev,"m");

	# Location will come from the first record:
	my @headerData = split(",",$headerlines[2]);

	$sfc_lat = $headerData[2];
        $sfc_lon = $headerData[3];

	if ($debug_header) { print "Lat: $sfc_lat Lon: $sfc_lon \n"; }

    	$header->setLatitude($sfc_lat, $self->buildLatLonFormat($sfc_lat));
        $header->setLongitude($sfc_lon, $self->buildLatLonFormat($sfc_lon));
   	
	# Release time will come from the name of the file:
	# ASSUMPTION: the file name is expected to be 2017-03-25_1541z_MSU.csv	
        if ($file =~ /(\d{4})-(\d{2})-(\d{2})_(\d{2})(\d{2})z/)
        {
                my ($year, $month, $day, $hour, $min) = ($1,$2,$3,$4,$5);
                my $date = join ", ", $year, $month, $day;
                my $time = join ":", $hour,$min, "00";

                if ($debug_header) { print "DATE:  $date   TIME:  $time\n"; }

                $header->setActualRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
                $header->setNominalRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
        }

    	# -------------------------------------------------
   	# Other header info provided by Scot
	# -------------------------------------------------
    	my $sondeType = "WindSond S1H2";
	$header->setLine(5,"Radiosonde Type:", ($sondeType));

	my $groundStationSoftware = "WS-250 receiver with software version 2.53";
	$header->setLine(6,"Ground Station Software:", ($groundStationSoftware));
	$header->setVariableParameter(2, "MixR","g/kg");


    	return $header;
}
                           
##------------------------------------------------------------------------------
# @signature void parseRawFiles(String file)
# <p>Read the data from the specified file and convert it to the ESC format.</p>
#
# @input $file The name of the file to be parsed.
##------------------------------------------------------------------------------
sub parseRawFile 
{
	my ($self,$file) = @_;
    
    	printf("\nProcessing file: %s\n",$file);

    	open(my $FILE,$self->{"RAW_DIR"}."/".$file) or die("Can't open file: ".$file);

    	my @lines = <$FILE>;
    	close($FILE);

	my @headerlines = @lines[0..2];
    
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
                
	    	# Needed for code to derive ascension rate
    		my $prev_time = 0.0;
    		my $prev_alt = $header->getAltitude();

		if ($debug_file_parser) { print "Prev Alt before it happens: $prev_alt \n"; }

      		# Needed for code to derive geopotential height
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
		my $fake_surface_data = 0;
		my $dewpoint_warnings = 0;
		my $do_skip_test = 1;    

   	 	# Now grab the data from each line
		foreach my $line (@lines) 
		{
			# Skip any blank lines.
			next if ($line =~ /^\s*$/);
        
			my $record = ClassRecord->new($WARN,$file);
		
			if ($index >= 2)
			{
         			my $sat_vapor_press;
				my $temp;
				my $dewpoint;
				my $mixing_ratio;
				my $saturated_mixing_ratio;
				my $pressure;
				my $rh;
				
				chomp($line);
		   	 	my @data = split(',',$line);
	    	
            			# Put the data into the array!
           	 		#----------------------------------------------------------------
				# ASSUMPTION: 
				# $data[0] = UTC time	$data[1] = height	$data[2] = lat
				# $data[3] = lon	$data[4] = pressure	$data[5] = temp
				# $data[6] = wind speed $data[7] = wind dir	$data[8] = RH 
				# $data[9] = mixing ratio
				#----------------------------------------------------------------
                             				
				# HARDCODED Hash for the One Second Records:
				my %oneSecondTimeHash =
				(
					"2017-03-25_1541z_MSU.csv" => "15:49:51",
					"2017-03-25_1601z_MSU.csv" => "16:04:10",
					"2017-03-25_1639z_MSU.csv" => "16:41:49",
					"2017-03-25_1659z_MSU.csv" => "17:05:22",
					"2017-03-25_1726z_MSU.csv" => "17:29:22",
					"2017-03-27_1159z_MSU.csv" => "12:04:55",
					"2017-03-27_1748z_MSU.csv" => "17:57:32",
					"2017-03-27_1804z_MSU.csv" => "18:09:29",
					"2017-03-27_1852z_MSU.csv" => "18:53:23",
					"2017-03-27_1854z_MSU.csv" => "18:58:48",
					"2017-03-27_1919z_MSU.csv" => "19:27:19",
					"2017-03-27_1924z_MSU.csv" => "19:26:25",
					"2017-03-27_1953z_MSU.csv" => "20:01:17",
					"2017-03-27_2005z_MSU.csv" => "20:06:29",
					"2017-03-27_2022z_MSU.csv" => "20:30:17",
					"2017-03-27_2025z_MSU.csv" => "20:26:16",
					"2017-03-27_2039z_MSU.csv" => "20:45:59",
					"2017-03-27_2040z_MSU.csv" => "20:40:50",
					"2017-03-27_2057z_MSU.csv" => "21:02:09",
					"2017-03-27_2058z_MSU.csv" => "20:58:51",
					"2017-03-27_2112z_MSU.csv" => "21:16:38",
					"2017-03-27_2130z_MSU.csv" => "21:31:13",
					"2017-03-27_2131z_MSU.csv" => "21:35:13",
					"2017-03-27_2141z_MSU.csv" => "21:42:50",
					"2017-03-27_2143z_MSU.csv" => "21:46:49",
					"2017-03-27_2159z_MSU.csv" => "22:07:13",
					"2017-03-27_2202z_MSU.csv" => "22:03:53",
					"2017-03-30_1547z_MSU.csv" => "15:49:43",
					"2017-03-30_1750z_MSU.csv" => "17:51:23",
					"2017-04-03_1701z_MSU.csv" => "17:04:28",
					"2017-04-03_1900z_MSU.csv" => "19:03:19",
					"2017-04-05_1111z_MSU.csv" => "11:12:35",
					"2017-04-05_1116z_MSU.csv" => "11:19:04",
					"2017-04-05_1350z_MSU.csv" => "13:53:43",
					"2017-04-05_1354z_MSU.csv" => "13:57:42",
					"2017-04-05_1451z_MSU.csv" => "14:54:47",
					"2017-04-05_1455z_MSU.csv" => "14:57:17",
					"2017-04-05_1556z_MSU.csv" => "15:57:32",
					"2017-04-05_1654z_MSU.csv" => "16:59:35",
					"2017-04-05_1704z_MSU.csv" => "17:07:34",
					"2017-04-05_1755z_MSU.csv" => "17:57:59",
					"2017-04-05_1855z_MSU.csv" => "19:00:30",
					"2017-04-05_1957z_MSU.csv" => "20:02:10",
					"2017-04-05_2015z_MSU.csv" => "20:16:32",
					"2017-04-05_2059z_MSU.csv" => "21:01:59",
					"2017-04-05_2139z_MSU.csv" => "21:41:19",
					"2017-04-26_2353z_MSU.csv" => "23:53:52",
					"2017-04-27_0012z_MSU.csv" => "00:14:35",
					"2017-04-27_0058z_MSU.csv" => "01:00:29",
					"2017-04-27_0059z_MSU.csv" => "01:00:02",
					"2017-04-27_0127z_MSU.csv" => "01:28:37",
					"2017-04-27_0210z_MSU.csv" => "02:13:28",
					"2017-04-27_0251z_MSU.csv" => "02:53:22",
					"2017-04-28_1543z_MSU.csv" => "15:46:26",
					"2017-04-28_1812z_MSU.csv" => "18:14:47",
					"2017-04-30_1503z_MSU.csv" => "15:04:56",
					"2017-04-30_1522z_MSU.csv" => "15:23:06",
					"2017-04-30_1556z_MSU.csv" => "15:57:33",
					"2017-04-30_1611z_MSU.csv" => "16:14:06",
					"2017-04-30_1615z_MSU.csv" => "16:16:35",
					"2017-04-30_1708z_MSU.csv" => "17:10:37",
					"2017-04-30_2034z_MSU.csv" => "20:37:53",
					"2017-04-30_2049z_MSU.csv" => "20:51:16",
					"2017-04-30_2059z_MSU.csv" => "21:01:14",
					"2017-04-30_2122z_MSU.csv" => "21:23:16",
					"2017-04-30_2127z_MSU.csv" => "21:30:30",
					"2017-04-30_2251z_MSU.csv" => "22:54:11",
					"2017-05-01_0015z_MSU.csv" => "00:17:14",
					"2017-05-01_0052z_MSU.csv" => "00:55:20",
					"2017-05-01_0153z_MSU.csv" => "01:55:21",
					"2017-05-01_0255z_MSU.csv" => "02:57:45"
				);
				
				my $oneSecondTime = $oneSecondTimeHash{$file};
		
				# Time: 
				# NOTE: The zero-second record is from the first record in the raw data
				#       The one-second record comes manually from Scot (in the hash above)

                                $raw_data_time = $data[0];
                                $raw_data_time =~ s/://g;

                                if($raw_data_time =~/(\d{2})(\d{2})(\d{2})/)
                                {
                                         $raw_data_time = ($1 * 3600)+($2 * 60)+($3);
                                }

                                if ($index == 2) # zero-second
                                {
                                        $record->setTime($fake_surface_time);
                                        $prev_raw_data_time = $raw_data_time;
                                }
                                elsif ($index > 2)
                                {
					if ($do_skip_test) # value should be 1 = true
					{
                                        	if($line =~ $oneSecondTime) # then we reach the line we want!
						{
							my $time_diff = $raw_data_time - $prev_raw_data_time;

                                        		$derived_time += ($raw_data_time - $prev_raw_data_time);

                                        		$record->setTime($derived_time);
                                       		 	$prev_raw_data_time = $raw_data_time;
							$do_skip_test = 0;
						} # then we are false because we don't want to skip any more lines
						else
						{
							next;
						}			
		
					}
					else # we want every line after that oneSecondTime
					{
						    my $time_diff = $raw_data_time - $prev_raw_data_time;

                                                        $derived_time += ($raw_data_time - $prev_raw_data_time);

                                                        $record->setTime($derived_time);
                                                        $prev_raw_data_time = $raw_data_time;
                                                        $do_skip_test = 0;
					}
		
                          	}

				# Latitude:
				if ($data[2]) 
				{
					$data[2] = trim($data[2]);
				}
				else
				{
					$data[2] = 999;
				}
				$record->setLatitude($data[2], $self->buildLatLonFormat($data[2]));		
	
				# Longitude:
				if ($data[3])
				{
					$data[3] = trim($data[3]);
				}
				else
				{
					$data[3] = 9999.000;
				}
				$record->setLongitude($data[3], $self->buildLatLonFormat($data[3]));
		
				# Pressure:
				if ($data[4])
				{
					$pressure = trim($data[4]);
				}
				else
				{
					$pressure = 999;
				}
				$record->setPressure($pressure,"mb") if ($pressure !~ /^999$/);

				# Temperature:
				if ($data[5])
				{
					$temp = trim($data[5]);
				}
				else
				{
					$temp = 999;
				}
				$record->setTemperature($temp,"C");

				# Wind Speed:
				if ($data[6])
				{
					$data[6] = trim($data[6]);
				}
				else
				{
					$data[6] = 999;
				}
				$record->setWindSpeed($data[6],"m/s");

				# Wind Direction:
				if ($data[7])
				{
					$data[7] = trim($data[7]);
				}
				else
				{
					$data[7] = 999;
				}
				$record->setWindDirection($data[7]);		

				# Relative Humidity:
				if ($data[8])
				{
					$rh = trim($data[8]);
				}
				else
				{
					$rh = 999;
				}
				$record->setRelativeHumidity($data[8]);
		
				# Mixing Ratio:
				if ($data[9])
				{
					$mixing_ratio = trim($data[9]);
				}
				else
				{
					$mixing_ratio = 999;
				}

		
				# Dewpoint:
				# ASSUMPTION: Dew Point must be calculated
				if ($mixing_ratio != 0) 
				{
				    if (($temp =~ /^999$/) || (($data[8]) && ($data[8] =~ /^999$/)))
				    {
						if($debug_dewpoint) { print "WARNING: Cannot calculate DewPoint due to missing temp or RH\n";}
						$dewpoint_warnings = 1;
				    }
				    elsif (($rh) && ($rh !~ /^-/))
				    {
						$dewpoint = calculateDewPoint($temp, $data[8]); #Dewpoint Temp (C)
						# Scot has indicated we should set RH and dewpoint to missing if MR <= 0.0
						$record->setDewPoint($dewpoint,"C") unless ($mixing_ratio <= 0.0);
				    }
				}
			
				# Insert Ele (Elevation Angle) and Azi (Azimuth Angle) data
				# # For setVariableValue(index, value):
				# # index (1) is Ele column, index (2) is Azi column.
				# Variable 1 is Height (above ground)- this turned out to be too large 
				# to fit the column, so we did not use this data
				# Variable 2 is Mixing Ratio
				# THIS IS FOR THE MIXING RATIO
				# See header->setVariableParameter to change column header
				# $record->setVariableValue(1, $data[3]) unless ($data[3] =~ /\/+/);
				
				# Mixing Ratio:
				$record->setVariableValue(2, $mixing_ratio) unless ($mixing_ratio =~ /^999$/); # ????
				

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

				# Altitude/Geopotential Height:
				
				#The surface record's altitude will come from the locations text file (aka the header):
				if ($index == 2)
				{ 
					my $zeroSecAlt = $header->getAltitude();
					$record->setAltitude($zeroSecAlt,"m");
				}

				# All other records will have the geopotential height calculation:
				elsif ($index > 2)
				{
					if ($debug_geopotential_height) 
					{ 
						my $prev_time = $previous_record->getTime();
						my $prev_press = $previous_record->getPressure(); 
						my $prev_temp = $previous_record->getTemperature(); 
						my $prev_alt = $previous_record->getAltitude();
		
						print "\nCalc Geopot. Height  from prev press = $prev_press, temp = $prev_temp, alt = $prev_alt,\n";
						print "and current press = $pressure and temp = $temp at t = $prev_time\n"; 
					}
			
					if (($previous_record->getPressure() < 9990.0) && ($record->getPressure() < 9990.0))
					{
						if ($debug_geopotential_height){ print "prev_press < 9990.0 - NOT missing\n"; }

						$geopotential_height = calculateAltitude($previous_record->getPressure(),
									$previous_record->getTemperature(), 
									 undef, $previous_record->getAltitude(), 
									 $pressure, $temp, undef, 1);


						if (defined($geopotential_height))
						{
							$record->setAltitude($geopotential_height,"m");
						}
						else
						{
							print "WARNING: Was not able to calculate geopotential height\n";
							$geopotential_height = 99999.0;
							# NOTE: Do not need to call SetAltitude with this value as
							# "not calling it" will automatically fill in a missing value.
						}
					}	
					else
					{
						if ($debug_geopotential_height){print "WARNING: prev_press > 9990.0 - MISSING! Set geopot alt to missing.\n"; }
						$geopotential_height = 99999.0;
						# NOTE: Do not need to call SetAltitude with this value as
						# "not calling it" will automatically fill in a missing value.
					}
				} # end elsif ($index > 2)

				
				# Ascention Rate:
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
				if ($index >= 2)
                                {

                                        if ($debug_ascent)
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

                                                if ($debug_ascent) { print "Calc Ascension Rate.\n"; }
                                        }

					if ($debug_ascent)
                                        {
                                                my $rectime = $record->getTime(); my $recalt = $record->getAltitude();
                                                print "Current record: Time $rectime  Altitude = $recalt ";
                                        }

                                        if ($record->getTime() != 9999 && $record->getAltitude() != 99999)
                                        {
                                                $prev_time = $record->getTime();
                                                $prev_alt = $record->getAltitude();

                                                if ($debug_ascent) { print " has valid Time and Alt.\n"; }
                                        }
                                } # end if $index >= 2 for ascension rate

                                if ($debug_file_parser)
                                {
                                        my $press = $record->getPressure();
                                        my $temp = $record->getTemperature();
                                        my $alt = $record->getAltitude();
                                        print "Current Rec: press = $press, temp = $temp, alt = $alt\n";
                                }

                                if ( ($record->getPressure() < 9999.0)  && ($record->getTemperature() < 999.0)
                                         && ($record->getAltitude() < 99999.0) )
                                {
                                        if ($debug_file_parser) { print "Set previous_record = record and move to next record.\n\n"; }

                                        $previous_record = $record;
                                }
                                else
                                {
                                        if ($debug_file_parser) { print "Do NOT assign current record to previous_record! ";}
                                }

			    
				printf($OUT $record->toString());
			} # end if ($index >= 2)
			
	    	$index++;
		} # end foreach $line

	}
	else
	{
		printf("Unable to make a header\n");
	}
} # end parseRawFile


#**********************************************
#	GENERAL SUBROUTINES		      *
#**********************************************

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
    my @files = grep(/^2017.+csv$/,sort(readdir($RAW)));
    closedir($RAW);
    
    
    open($WARN,">".$self->{"WARN_LOG"}) or die("Can't create ".$self->{"WARN_LOG"});
    
	 printf("Ready to read the files\n");
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
 
    open(my $FILE, sprintf("msu_2017_locs.txt")) or die("Can't read file into array\n");
    
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
