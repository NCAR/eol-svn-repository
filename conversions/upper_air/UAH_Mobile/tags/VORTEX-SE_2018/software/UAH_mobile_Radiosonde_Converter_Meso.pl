#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The UAH_Radiosonde_Converter.pl script is used for converting 
# high resolution radiosonde data from the SHARPpy ascii format to the 
# EOL Sounding Composite (ESC) format.</p> 
#
# @author Summer Stafford
# @version VORTEX-SE Meso 18-19 UAH Mobile Radiosonde Data
# 	   - Actual data starts on line 4.
# 	   - For the release times, use the time and date given by
# 	     the second record of the data files.
# 	   - Need to differentiate between two types of radiosondes using
# 	     column headers for the wind speed and direction columns (info
# 	     given by Scot L.)
# 	   - Assume data are at 5 sec. intervals for the iMet radiosonde and
# 	     at 3 sec. intervals for the Windsond radiosonde.
# 	   - For certain files specified by Scot L., the interval is 1 sec.
# 	   - Use the height data given by Scot L. and then calculate geopotential
# 	     height for all records above the surface.
# 	   - Convert wind speed from knots to m/s.
# 	   - Need to calculate wind components and ascension rate.
#
# @author Summer Stafford
# @version VORTEX-SE 2018 UAH Mobile Radiosonde Data
# 	   - Actual data starts on line 4.
# 	   - For the release times, use the time given by the second
# 	     record of the data files
# 	   - Need to differentiate between two types of radiosondes using
# 	     column headers for the wind speed and direction columns (info
# 	     given by Scot L)
# 	   - Lat/lon values given by the first data record of each raw data
# 	     file
# 	   - Assume data are at 5 sec. intervals for the iMet radiosonde and
# 	     at 3 sec. intervals for the Windsond radiosonde 
# 	   - Use the height data given by Scot L. and then calculate geopotential
# 	     height for all records above the surface
# 	   - Need to derive wind components
# 	   - Need to convert the wind speed to m/s
# 	   - Calculate ascension rate based off of geopotential height data
# 	   - For two files specified by Scot L. without surface data, use
# 	     height data from raw files
#
# @author Summer Stafford
# @version VORTEX-SE 2017 UAH Mobile Radiosonde Data
# 	   - Actual data starts on line 4.
# 	   - For the release times, use the time given by the second
# 	     record of the data files
# 	   - Need to differentiate between two types of radiosondes using
# 	     column headers for the wind speed and direction columns (info
# 	     given by Scot L)
# 	   - Lat/lon values given by the first data record of each raw data
# 	     file
# 	   - Assume data are at 1 sec. intervals for the iMet radiosonde and
# 	     at 3 sec. intervals for the Windsond radiosonde, and at 5 sec. 
# 	     intervals for files specified by Scot L.
# 	   - Use the height data given by Scot L. and then calculate geopotential
# 	     height for all records above the surface
# 	   - Need to derive wind components
# 	   - Need to convert the wind speed to m/s
#
# @author Linda Echo-Hawk
# @version Meso 2018-19 ULM Radiosonde Data
#          - The first record is the surface record.
#          - Header altitude is taken from the surface 
#            record HGHT (geopotential height) column 
#          - Lat/Lon values were provided for each
#            of the two locations (Monroe and 
#            Breaux Bridge LA) by Scot L.
#          - Assume the data are at 5-sec. intervals.
#          - Need to derive RH and wind components.
#          - A "readSurfaceValuesFile" function exists to 
#            read in surface values from a separate file, 
#            but is not called since the values were provided 
#            by Scot and no separate file was used. 
#          - Ascension rate is calculated by the converter.
#          - The file names differ in that some have SHARPPY 
#            all in upper case, while others use a lower case 
#            "py" (e.g., SHARPpy). The "readDataFiles" function 
#            uses a case-insensitive match to find all the files.
#
#
# @author Linda Echo-Hawk 19 October 2018
# @version Meso 2018-19 based on the VORTEX-SE 2016 ULM Mobile
#            Sounding Converter.
#          - NOTE that this converter was developed to test
#            our ability to convert SHARPpy data to the EOL format.
#          - Header info on lat/lon/alt as well as sonde info
#            would need to be provided by the source. These
#            values are hard-coded into this test converter.
#          - Need to derive RH and wind components.
#          - Height parameter is MSL so we need to derive 
#            the geopotential height. Scot is confirming
#            with source that first record is at surface.
#          - Scot is confirming with source the standard
#            time interval. For now, use 5 seconds.
#          - I left code in from the ULM conversion that
#            read in surface values from a separate file.
#            This code is currently commented out, but
#            since this info is not provided in the raw
#            data, we may have to use this approach for
#            future SHARPpy data conversions.
#
#
##Module------------------------------------------------------------------------
package UAH_mobile_Radiosonde_Converter;
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

printf "\nUAH_mobile_Radiosonde_Converter.pl began on ";print scalar localtime;printf "\n";
my $debug = 0;
my $debug_geopotential_height = 0;

&main();
printf "\nUAH_mobile_Radiosonde_Converter.pl ended on ";print scalar localtime;printf "\n";

my $filename;
my $sondetype;
my $groundStationSoftware;
my $date;
my $time;
my $increment;

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the ULM radiosonde data by converting it from 
# the native ASCII format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = UAH_mobile_Radiosonde_Converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature UAH_mobile_Radiosonde_Converter new()
# <p>Create a new instance of a UAH_mobile_Radiosonde_Converter.</p>
#
# @output $self A new UAH_mobile_Radiosonde_Converter object.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);
    
    $self->{"stations"} = SimpleStationMap->new();

    # HARD-CODED
    $self->{"PROJECT"} = "VORTEX-SE Meso18-19";
    # HARD-CODED
    $self->{"NETWORK"} = "UAH";
    
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
    $station->setStationName("UAH");
    # HARD-CODED
    $station->setStateCode("99");
    $station->setReportingFrequency("no set schedule");
    $station->setNetworkIdNumber("99");
    # platform, 591	Radiosonde, iMet-1
    # NOTE: Need to set platform id number according to radiosonde type
    # $station->setPlatformIdNumber(591);
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
    $header->setType("UAH Sounding Data");
    $header->setReleaseDirection("Ascending");
    $header->setProject($self->{"PROJECT"});
	# The Id will be the prefix of the output file
    # and appears in the stationCD.out file
    $header->setId("UAH");
	# $header->setSite("NOAA ATDD Mobile");

	my $sfc_elev;

	if ($file =~ /Courtland/)
	{
		$header->setSite("Courtland AL");
		$sfc_elev = 175;
	}
	elsif ($file =~ /Huntsville/)
	{
		$header->setSite("Huntsville AL");
		$sfc_elev = 207;
	}
	elsif ($file =~ /Scottsborro/)
	{
		$header->setSite("Scottsborro AL");
		$sfc_elev = 183;
	}
	elsif ($file =~ /Good_Hope/)
	{
		$header->setSite("Good Hope AL");
		$sfc_elev = 236;
	}
	elsif ($file =~ /Trinity/)
	{
		$header->setSite("Trinity AL");
		$sfc_elev = 195;
	}
	elsif ($file =~ /Priceville/)
	{
		$header->setSite("Priceville AL");
		$sfc_elev = 247;
	}
	elsif ($file =~ /Cullman/)
	{
		$header->setSite("Cullman AL");
		$sfc_elev = 228;
	}
	elsif ($file =~ /Florence/)
	{
		$header->setSite("Florence AL");
		$sfc_elev = 173;
		if ($file =~ /201903091800/){
			$increment= 1;
		}
		elsif ($file =~ /201903092100/){
			$increment = 1;
		}
		elsif ($file =~ /201903092200/){
			$increment = 1;
		}
		elsif ($file =~ /201903092300/){
			$increment = 1;
		}
		elsif ($file =~ /201903100000/){
			$increment = 1;
		}
	}
	elsif ($file =~ /Hackleburg/)
	{
		$header->setSite("Hackleburg AL");
		$sfc_elev = 286;
	}
	elsif ($file =~ /Iuka/)
	{
		$header->setSite("Iuka MS");
		$sfc_elev = 190;
	}
	elsif ($file =~ /Dodge_City/)
	{
		$header->setSite("Dodge City AL");
		$sfc_elev = 200;
	}
	elsif ($file =~ /Corinth_North/)
	{
		$header->setSite("Corinth North AL");
		$sfc_elev = 156;
	}
	elsif ($file =~ /Corinth_West/){
		$header->setSite("Corinth West AL");
		$sfc_elev = 125;
	}
	elsif ($file =~ /Burnsville/) {
		$header->setSite("Burnsville MS");
		$sfc_elev = 155;
	}
	elsif ($file =~ /Hamilton_South/)
	{
		$header->setSite("Hamilton South AL");
		$sfc_elev = 131;
	}
	elsif ($file =~ /Hamilton_West/)
	{
		$header->setSite("Hamilton West AL");
		$sfc_elev = 169;
	}
	elsif ($file =~ /Hamilton/){
		$header->setSite("Hamilton AL");
		$sfc_elev = 150;
	}
	elsif ($file =~ /Moulton/){
		$header->setSite("Moulton AL");
		$sfc_elev = 198;
	}
	elsif ($file =~ /Russellville1/)
	{
		$header->setSite("Russellville1 AL");
		$sfc_elev = 230;
	}
	elsif ($file =~ /Russellville2/)
	{
		$header->setSite("Russellville2 AL");
		$sfc_elev = 284;
	}
	elsif ($file =~ /Russellville3/)
	{
		$header->setSite("Russellville3 AL");
		$sfc_elev = 272;
	}
	elsif ($file =~ /Russellville4/)
	{
		$header->setSite("Russellville4 AL");
		$sfc_elev = 209;
	}
	elsif ($file =~ /Blackland/){
		$header->setSite("Blackland MS");
		$sfc_elev =112;
	}
	elsif ($file =~ /Red_Bay/)
	{
		$header->setSite("Red Bay AL");
		$sfc_elev = 195;
	}
	elsif ($file =~ /Decatur/)
	{
		$header->setSite("Decatur AL");
		$sfc_elev = 171;
	}
	elsif ($file =~ /Armory/)
	{
		$header->setSite("Armory MS");
		$sfc_elev = 67;
	}
	elsif ($file =~ /Natural_Bridge/)
	{
		$header->setSite("Natural Bridge AL");
		$sfc_elev = 232;
	}
	elsif ($file =~ /Fayette/)
	{
		$header->setSite("Fayette AL");
		$sfc_elev = 116;
	}
	else
	{
		print "WARNING: Unrecognized location\n";
	}

	
	my @headerData = split(",",$headerlines[0]);
	my $sfc_lat = trim($headerData[0]);
	my $sfc_lon = trim($headerData[1]);

	$header->setAltitude($sfc_elev, "m");	
	
	$header->setLatitude($sfc_lat, $self->buildLatLonFormat($sfc_lat));
	$header->setLongitude($sfc_lon, $self->buildLatLonFormat($sfc_lon)); 

    # -------------------------------------------------
   	# Other header info provided by Scot
   	# NOTE: Radiosonde Type and Ground Station Software
   	# 	specified in parseRawFiles() based on column
   	# 	headers
	# -------------------------------------------------
	$header->setLine(5,"Radiosonde Type:", ($sondetype));
	$header->setLine(6,"Ground Station Software:", ($groundStationSoftware));

	$header->setActualRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
	$header->setNominalRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
       
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
    my @columnHeaders = split(",", $lines[2]);
    if ($columnHeaders[8] =~ /Calculated/i){
	$sondetype = "Windsond S1H";
	$groundStationSoftware = "Windsond WS-250";
	$increment = 3;
    }
    else {
	$sondetype = "iMet-1-ABxn";
	$groundStationSoftware = "iMet-OS-II";
	$increment = 5;
    }
   # ----------------------------------------------------------
   # Extract the date and time informtaion from the second record
   # NOTE: Need to check all files for accurate time information
   # 	  - Time information in second line is date and hour and minute(?)
   # ----------------------------------------------------------
    my @secondline = split(",", $lines[1]);
    $date = $secondline[0];
    $time = $secondline[1]; 
    if ($date =~ /# /){
	$date =~ s/# //g;
    }
    if ($date =~ /(\d{4})-(\d{2})-(\d{2})/){
	my ($year, $month, $day) = ($1,$2,$3);
	$date = join ", ", $year, $month, $day;
    }
    elsif ($date =~ /(\d{4})(\d{2})(\d{2})/){
	my ($year, $month, $day) = ($1,$2,$3);
	$date = join ", ", $year, $month, $day;
    }
    if ($time =~ /(\d{2})(\d{2})( UTC)/){
	my ($hour, $minute) = ($1,$2);
	$time = join ":", $hour,$minute,'00';
    }
    close($FILE);

    my @headerlines = $lines[3];
	print @headerlines;
                        
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

    #-----------------------------------------------
    # Needed for code to derive geopotential height
    #-----------------------------------------------
    my $previous_record;
    my $geopotential_height;

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
		
		if ($index >= 3)
		{
			if ($fake_surface_data)
			{
				# The raw data surface record does not
				# contain the time, lat or lon values,
				# so the header lat/lon values are used.
			    $record->setTime($fake_surface_time);
	        	$record->setLatitude($header->getLatitude(),
			    	    $self->buildLatLonFormat($header->getLatitude()));
		        $record->setLongitude($header->getLongitude(),
				        $self->buildLatLonFormat($header->getLongitude()));
        	    $record->setAltitude($header->getAltitude(),"m");

				$fake_surface_data = 0;
			}
			
            #--------------------------------------------
			# Wind components must be calculated;
			# convert wind speed from knots to m/s
			#--------------------------------------------
			my $temp;
			my $height;
			my $dewpoint;
			my $pressure;
			my $wind_spd;
			my $wind_dir;
			my $my_RH;
			
			# Valid data flags
			my $valid_pressure;
			my $valid_temp;
			my $valid_dewpt;
			my $valid_wdir;
			my $valid_wspd;
		
			chomp($line);
		    my @data = split(',',$line);

			if ($data[0] =~ /%END%/)
			{
				last;
			}
	    	

			$pressure = trim($data[4]);
			if ($pressure !~ /^999$/){
				$record->setPressure($pressure,"mb");
			} 

			$height = trim($data[3]);
			$record->setAltitude($geopotential_height,"m");

			$temp = trim($data[5]);
			if ($temp !~ /^-9999/){
				$record->setTemperature($temp,"C");
			}
			
			$my_RH = trim($data[6]);
			if($my_RH !~ /^-9999/){
				$record->setRelativeHumidity($my_RH);
			}

		   	$dewpoint = trim($data[7]);
			if ($dewpoint !~ /^-9999/){
				$record->setDewPoint($dewpoint,"C");
			} 
			

			$wind_spd = trim($data[8]); # wind spd (m/s)
			
			$wind_dir = trim($data[9]); # wind dir (deg)
			if (($wind_dir !~ /999/) && ($wind_dir < 360)){
				$valid_wdir = 1;
			} else {$valid_wdir = 0;}

			if (($wind_spd =~ /^0.0/) && ($index == 3) && $valid_wdir)
			{
				$record->setWindDirection($wind_dir);	
				$record->setWindSpeed($wind_spd,"m/s");
			}
			else
			{
				my $convertedWindSpeed = convertVelocity($wind_spd,"knot", "m/s");
				if ($convertedWindSpeed && ($convertedWindSpeed >= 0) && ($wind_spd !~ /^-999/)){
					$valid_wspd = 1;
				} else {$valid_wspd = 0;}
				if ($valid_wdir && $valid_wspd)
				{
					$record->setWindDirection($wind_dir);
					$record->setWindSpeed($convertedWindSpeed,"m/s");
				}
			}
			
		
        	#-------------------------------------
			# The first data line is index = 3.
			# Set initial time to zero, then 
			# use 5 or 3 second increments, 
			# depending on the type of radiosonde,
			# so increment $raw_data_time by 5 or 3
			# seconds for lines with $index > 3
			#-------------------------------------
			if ($index == 3)
			{                                                 
				$raw_data_time = 0;
		    	$record->setTime($raw_data_time);
				$raw_data_time += $increment;
			}
			elsif ($index > 3)
			{
				$record->setTime($raw_data_time);
				$raw_data_time += $increment;
			}
			
	    #-------------------------------------------------------
	    # Calculate geopotential height (this code from VORTEX2 SUNY)
	    #
	    # -----------------------------------------------------
	    # BEWARE: For VORTEX2 (2009) Sloehrer says there are issues
	    # with the raw data altitudes, so compute the geopotential
	    # height/ altitude and insert for all other than surface record.
	    # call calculateAltitude(last_press, last_temp, last_dewpt, last_alt,
	    # 			     this_press, this_temp, this_dewpt, 1)
	    # Note that the last three parms in calculateAltitude
	    # are the pressure, temp, and dewpt (undefined for this dataset)
	    # for the current record. To check the altitude calculations, see
	    # the web interface tool at
	    #
	    # http://dmg.eol.ucar.edu/cgi-bin/conversions/calculations/altitude
	    # -------------------------------------------------------
	    if ($index == 3) {
		my $oneSecAlt = $header->getAltitude();
		$record->setAltitude($oneSecAlt, "m");
    	    }
	    elsif ($index > 3){
		if ($debug_geopotential_height) {
			my $prev_time = $previous_record->getTime();
			my $prev_press = $previous_record->getPressure();
			my $prev_temp = $previous_record->getTemperature();
			my $prev_alt = $previous_record->getAltitude();

			print "\nCalc Geopot. Height from prev press = $prev_press, temp = $prev_temp, alt = $prev_alt, \n";
			print "and current press = $pressure and temp = $temp at t = $prev_time\n";
		}		
	    #------------------------------------------------------------
	    # NOTE: Do not calculate geopotential height without
	    # valid pressure. Scot says this is one of the most important 
	    # factors, so must not be "missing." More discussion indicates
	    # that a check for valid previous altitude would also indicate
	    # a valid previous pressure. Current pressure is also required.
	    # -----------------------------------------------------------

	    if (($previous_record->getPressure() < 9990.0) && ($record->getPressure() < 9990.0)) {
		if ($debug_geopotential_height) { print "prev_press < 9990.0 - NOT missing\n"; }
		$geopotential_height = calculateAltitude($previous_record->getPressure(), $previous_record->getTemperature(), undef,
							 $previous_record->getAltitude(), $pressure, $temp, undef, 1);
		
		if (defined($geopotential_height)){
			$record->setAltitude($geopotential_height, "m");
		}	
		else {
			print "WARNING: Was not able to calculate geoptential height\n";
			$geopotential_height = 99999.0;
			#NOTE: Do not need to call SetAltitude with this value as
			# "not calling it" will automatically fill in a missing value
		}
 	    }
            else {
		if ($debug_geopotential_height) {print "WARNING: prev_press > 9990.0 - MISSING! Set geopot alt to missing.\n";}
		$geopotential_height = 99999.0;
		#NOTE: Do not need to call SetAltitude with this value as
		# "not calling it" will automatically fill in a missing value
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
                    if ($debug){
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
    my @files = grep(/.txt$/i,sort(readdir($RAW)));     
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
