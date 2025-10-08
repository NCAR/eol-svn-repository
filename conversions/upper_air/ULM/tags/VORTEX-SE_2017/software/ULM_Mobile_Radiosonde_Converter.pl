#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The ULM_Mobile_Radiosonde_Converter.pl script is used for converting 
# high resolution radiosonde data from the University of Louisiana-Monroe
# from a csv format to the EOL Sounding Composite (ESC) format.</p> 
#
# @inputs raw_data files, ULM_sfc_alt_2017.txt  
# @outputs *.cls files in a one-to-one ratio with the raw_data files
#
# @author Alley Robinson 20 December 2017
# @version VORTEX-SE_2017 ULM Mobile Sounding
#	The differences between the 2016 and the 2017 data are:
#		1. The UTC time format is HH:MM:SS instead of HHMMSS
#		2. The values are separated by ', ' instead of ',' (spaces in the 2017 data)
#		3. The file names only have Z's in a few files. The rest did not.
#		4. RH and Dew point do not need to be calculated
#		5. formatFiles.sh needs to be ran before running this program
#			This script will format the files to remove spaces
# I had to remove spaces in the ULM_sfc_alt_2017.txt file for this program to work.
# I also had to rename the output files, since there are duplicate location data points. 
# Example: ULM_CrossvilleAL_201704051105.cls 
# Changed the Ground Station Software version. 
#
#
# @author Linda Echo-Hawk 20 September 2016
# @version VORTEX-SE_2016 ULM Mobile Sounding
#            based on V-SE NOAA ATDD converter (and 
#            DYNAMO Singapore converter
#          - A separate text file (ULM_sfc_alt.txt) provided a list 
#            of locations and their surface lat/lon/elev values. This
#            file was read into the converter. Only the elevation
#            value was used.
#          - The raw data does not contain a surface record
#		     i.e., a zero second record, so one is created
#		     with the header lat/lon/elev values. All other
#			 values in the 0-sec record are set to missing.
#          - Some files contained missing pressure, temperature and 
#            mixing ratio for some records. These were indicated by 
#            ,,, with no spaces between them (empty columns).
#          - RH and dewpoint had to be calculated from the 
#            mixing ratio using equations provided by Scot.
#          - The code used "setVariableValue" to include a column
#            for the mixing ratio values.
#          - Geopotential height is calculated by the converter.
#          - Ascension rate is calculated by the converter.
#          - The location from the text file served as the Site
#            value for the header.
#ULM_CrossvilleAL_201704051105.cls 
#
##Module------------------------------------------------------------------------
package ULM_Mobile_Radiosonde_Converter;
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

printf "\nULM_Mobile_Radiosonde_Converter.pl began on ";print scalar localtime;printf "\n";
my $debug = 0;
my $debug_geopotential_height = 0;

&main();
printf "\nULM_Mobile_Radiosonde_Converter.pl ended on ";print scalar localtime;printf "\n";

my $filename;

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the ULM Mobile radiosonde data by converting it from 
# the native ASCII format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = ULM_Mobile_Radiosonde_Converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature ULM_Mobile_Radiosonde_Converter new()
# <p>Create a new instance of a ULM_Mobile_Radiosonde_Converter.</p>
#
# @output $self A new ULM_Mobile_Radiosonde_Converter object.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);
    
    $self->{"stations"} = SimpleStationMap->new();

    # HARD-CODED
    $self->{"PROJECT"} = "VORTEX-SE_2017";
    # HARD-CODED
    $self->{"NETWORK"} = "ULM_mobile";
    
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
    $station->setStationName("ULM");
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
    # Set the type of sounding
    $header->setType("ULM Mobile Sounding Data");
    $header->setReleaseDirection("Ascending");
    $header->setProject($self->{"PROJECT"});
	# The Id will be the prefix of the output file
    # and appears in the stationCD.out file
    $header->setId("ULM");
	# $header->setSite("NOAA ATDD Mobile");


    # ------------------------------------------------------
	# Get the altitude from the file of surface values
	# ------------------------------------------------------
	my @surfaceValues = &readSurfaceValuesFile();

	my $citystate;
	my $sfc_elev;

	foreach my $line(@surfaceValues)
	{
		# skip the first line in the file, it is a header
		next if ($line =~ /filename/);
		# find out if the line in the surface values file
		# applies to the $file you are processing
        chomp($line);
	
		# raw data file name example: 20160501_2034Z_ULM_TannerAL.txt
		# sfc_alt ($line): 20160501_1644Z_ULM_TannerAL.txt,34.71229,-87.00117,187
		# $location_data[0] = 20160501_2034Z_ULM_TannerAL.txt
		# $location_data[1] = 34.71229
		# $location_data[2] = -87.00117
		# $location_data[3] = 187
	    
		my @location_data = split(",", $line);
		# print "FILE: $location_data[0]\n";
        my $file_match_name = trim($location_data[0]);
		# print "\tFILE_MATCH_NAME = $file_match_name and FILE = $file\n";
		
		if ($file =~ /$file_match_name/i)
		{
			print "\tFILE $file matches FILE_MATCH_NAME $file_match_name\n";

	    	my @file_location = split("_", $location_data[0]);
			my $city = $file_location[3];
			$city =~ s/.txt//g;
			my $state = substr($city,-2,2);
			print "\tSTATE = $state ";
			my @loc = split(//,$city);

			splice @loc, -2,2;
			$city = join "", @loc;
			print "CITY = $city ";
			$citystate = join ", ",$city, $state;
			print "CITYSTATE = $citystate\n";
			

			print "\tLOCATION DATA: @location_data\n";
			$sfc_elev = trim($location_data[3]);
			last;
		}
    }

	$header->setSite($citystate);

    $header->setAltitude($sfc_elev,"m");

    # -------------------------------------------------
   	# Get the header lat/lon data (from the surface record)
	# -------------------------------------------------
   	my $sfc_lat;
	my $sfc_lon;
	
	my @headerData = split(",",$headerlines[0]);
	$sfc_lat = $headerData[0];
	$sfc_lon = $headerData[1];

    $header->setLatitude($sfc_lat, $self->buildLatLonFormat($sfc_lat));
	$header->setLongitude($sfc_lon, $self->buildLatLonFormat($sfc_lon)); 


    # -------------------------------------------------
   	# Other header info provided by Scot
	# -------------------------------------------------
    my $sondeType = "iMet-1-ABxn";
	$header->setLine(5,"Radiosonde Type:", ($sondeType));
	my $groundStationSoftware = "iMetOS-II software version 03.90.0C.";
	$header->setLine(6,"Ground Station Software:", ($groundStationSoftware));
	$header->setVariableParameter(2, "MixR","g/kg");

    # ----------------------------------------------------------
    # Extract the date and time information from the file name
	# Expects filename similar to 20160501_2034Z_ULM_TannerAL.txt
	# 2017 Changes will reflect the filename similar to  ULM_CrossvilleAL_201704051105.cls 
    # ----------------------------------------------------------
    my $date;
	my $time;

	if ($file =~ /(\d{4})(\d{2})(\d{2})_(\d{2})(\d{2})Z/ || $file =~ /(\d{4})(\d{2})(\d{2})_(\d{2})(\d{2})/)
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

	my @headerlines = $lines[3];

    
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
    #
        my @file_match = split("_", $file);
        my $locReference = $file_match[3];
        $locReference =~ s/.txt//g;


        my $outfile;
	my ($hour, $min, $sec) = split (/:/, $header->getActualTime());

   	$outfile = sprintf("%s_%s_%04d%02d%02d%02d%02d.cls", 
					   	   $header->getId(),
						   $locReference,
					   	   split(/,/,$header->getActualDate()),
					   	   $hour, $min);

    printf("\tOutput file name is %s\n", $outfile);


	open(my $OUT,">".$self->{"OUTPUT_DIR"}."/".$outfile)
	    or die("Can't open output file for $file\n");

	print($OUT $header->toString());
	
    # ----------------------------------------
    # Needed for code to derive ascension rate
    # ----------------------------------------
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
		# These warnings were for calculating RH and Dewpoint, which are not needed anymore
		# They will always = 0.
	my $RH_warnings = 0;
	my $dewpoint_warnings = 0;
    
    # Now grab the data from each lines
	foreach my $line (@lines) 
	{
        # Skip any blank lines.
		next if ($line =~ /^\s*$/);
        
		my $record = ClassRecord->new($WARN,$file);
		
		if ($index >= 3)
		{
			if ($fake_surface_data)
			{
				# The raw data does not contain a surface record
				# i.e., a zero second record, so one is created
				# here with the header lat/lon/elev values.
				# All other values are set to missing.
			    $record->setTime($fake_surface_time);
	        	$record->setLatitude($header->getLatitude(),
			    	    $self->buildLatLonFormat($header->getLatitude()));
		        $record->setLongitude($header->getLongitude(),
				        $self->buildLatLonFormat($header->getLongitude()));
        	    $record->setAltitude($header->getAltitude(),"m");

				printf($OUT $record->toString());
				$fake_surface_data = 0;
			}
			
            #-----------------------------------------
			# RH and dewpoint do NOT need to be calculated
			#-----------------------------------------
			my $sat_vapor_press;
			my $temp;
			my $rh;
			my $dewpoint;
			my $pressure;
			my $windspeed;
			my $winddir;
			my $seconds;
        #               my $mixing_ratio;
        #               my $saturated_mixing_ratio;
        	
	
			chomp($line);
		    my @data = split(',',$line);
	    	
		    $data[0] = trim($data[0]); # lat
		    $data[1] = trim($data[1]); # lon

			#The time does not need to be stripped
			#We will grab SS values by splitting the value by :
		    $data[2] = trim($data[2]);
		    my @time = split(':',$data[2]);
		    $seconds = $time[2];	
	            

		    $data[3] = trim($data[3]); # height (m)

            # Four files have blank (nothing) for missing values
			# for pressure, temperature and mixing ratio, e.g., ,,,,
			# NOTE: It might be better to assign $data[4] to 
			# $pressure, then do an if defined on $pressure...
			if ($data[4])
			{
				$data[4] = trim($data[4]); # press (mb)
				$pressure = $data[4];
			}
			else
			{
				$pressure = 999;
			}
			if ($data[5])
			{
				$data[5] = trim($data[5]); # temp (deg C)
				$temp = $data[5];
			}
			else
			{
				$temp = 999;
			}
			if ($data[6])
			{
				$data[6] = trim($data[6]); # RH (%)
				$rh = $data[6];
			}
			else
			{
				$rh = 999;
			}
			if ($data[7])
			{
				$data[7] = trim($data[7]); # dewpoint (C)
				$dewpoint = $data[7];
			}
			else
			{
				$dewpoint = 999;
			}
			if ($data[8])
			{
				$data[8] = trim($data[8]); # wind speed (m/s)
				$windspeed = $data[8];
			}
			else
			{
				$windspeed = 32768;
			}
			if ($data[9])
			{
				$data[9] = trim($data[9]); # wind direction (deg)
				$winddir = $data[9];
			}
			else 
			{
				$winddir = 32768;
			} 
			
		
        	#-------------------------------------
			# The first data line is index = 3.
			# We need to get that time value for
			# our "real" start time, then assume
			# 5 second increments, so increment
			# $raw_data_time by five for lines
			# with $index > 3
			#-------------------------------------
			if ($index == 3)
			{                                                 
				$raw_data_time = $seconds;
		    	$record->setTime($raw_data_time);
				$raw_data_time += 5;         
			}
			elsif ($index > 3)
			{
				$record->setTime($raw_data_time);
				$raw_data_time += 5;
			}

			$record->setPressure($pressure,"mb") if ($pressure !~ /^999$/);
			$record->setTemperature($temp,"C") if ($temp !~ /^999$/);
		        $record->setRelativeHumidity($rh);
   	  	        $record->setDewPoint($dewpoint,"C");    

			# Insert Ele (Elevation Angle) and Azi (Azimuth Angle) data
			# # For setVariableValue(index, value):
			# # index (1) is Ele column, index (2) is Azi column.
			# Variable 1 is Height (above ground)- this turned out to be too large 
			# to fit the column, so we did not use this data
			# Variable 2 is Mixing Ratio
			# THIS IS FOR THE MIXING RATIO
			# See header->setVariableParameter to change column header
			# $record->setVariableValue(1, $data[3]) unless ($data[3] =~ /\/+/);
			#$record->setVariableValue(2, $mixing_ratio) unless ($mixing_ratio =~ /^999$/); # ????
			
			
			$record->setWindSpeed($windspeed,"m/s") if ($windspeed !~ /32768/);
			$record->setWindDirection($winddir) if ($winddir !~ /32768/);

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
	            if ($debug_geopotential_height) 
    	        { 
					my $prev_time = $previous_record->getTime();
					my $prev_press = $previous_record->getPressure(); 
            		my $prev_temp = $previous_record->getTemperature(); 
                	my $prev_alt = $previous_record->getAltitude();
	
    	            print "\nCalc Geopot. Height  from prev press = $prev_press, temp = $prev_temp, alt = $prev_alt,\n";
					print "and current press = $pressure and temp = $temp at t = $prev_time\n"; 
            	}
                # print "INDEX: $index\n";

				# NOTE: Do not calculate geopotential height without 
				# valid pressure. Scot says this is one of the most
				# important factors, so must not be "missing." More
				# discussion indicates that a check for valid previous 
				# altitude would also indicate a valid prev pressure.
				# Current pressure is also required.
				# --------------------------------------------------
	            # if ($previous_record->getPressure() < 9990.0)
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
	        # Only save current rec as previous rec
    	    # if not completely missing. This affects
        	# the calculations of the geopotential height,
	        # as the previous height must be non-missing. 
    	    #---------------------------------------------
			# NOTE that a more correct name for $previous_record
			# would be $last_valid_record.
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

	if ($RH_warnings)
	{
		print "WARNING: RH could not be calculated\n";
		$RH_warnings = 0;
	}
	if ($dewpoint_warnings)
	{
		print "WARNING: Dewpoint could not be calculated due to missing temp or RH\n";
	   	$dewpoint_warnings = 0;
   	}

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
    my @files = grep(/^2017.+txt$/,sort(readdir($RAW)));
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

    open(my $FILE, sprintf("ULM_sfc_alt_2017.txt")) or die("Can't read file into array\n");
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
