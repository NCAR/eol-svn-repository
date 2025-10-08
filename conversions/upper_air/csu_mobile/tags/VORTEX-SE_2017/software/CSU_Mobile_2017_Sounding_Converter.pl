#! /usr/bin/perl -w
##Module------------------------------------------------------------------------
# <p>The CSU_Mobile_2017_Sounding_Converter.pl script is used for converting high
# resolution radiosonde data from ASCII formatted data to the EOL Sounding 
# Composite (ESC) format.</p> 
#
#
# Note:  LOOK FOR HARDCODED, ASSUMPTION 
#
# Sample Run Command: perl CSU_Mobile_2017_Sounding_Converter.pl
#
# @inputs Sounding files that look like the following: 
#			edt_20170321_2300.txt
#
# The files have the following columns:
# ASSUMPTION: The data array is split as followed:
# height $data[1]       RH        $data[5]      lat $data[9]
# press  $data[2]       windspeed $data[6]      lon $data[10]
# temp   $data[3]       winddir   $data[7]
# dewpt  $data[4]       ascent    $data[8]
#
#
# @outputs Processed sounding file in ESC format
#
#
# @author Alley Robinson
# @version VORTEX-SE 2017 for CSU Mobile sounding Data
#		- The file contains header info on lines 1-27, blank line on 28
#		- Actual data will start on line 29.
#		- We will only process the following parameters:
#			HeightMSL, 
#			Pressure,
#			Temp,
#			Dewpoint,
#			RH,
#			Wind Speed,
#			Wind Direction,
#			Ascention Rate,
#			Lat,
#			Lon
#		- Geopotential Height was a value that was collected and is in the raw data
#
#
#
# @author Linda Echo-Hawk
# @version PECAN 2015 for CSU Mobile, 
#          based on the DEEPWAVE Haast converter
#          - The converter expects filenames in the format:
#            edt_YYYYMMDD_HHmm.txt (e.g., edt_20150602_0303.txt)
#          - The file contains header info on lines 1-29. Actual data starts 
#            on line 33.
#          - The radiosonde ID is obtained from the header information.
#          - The lat/lon/alt header values are obtained from the
#            header information.
#          - Code was added to derive the geopotential height.
#          - The release date and time can be obtained from the file name
#            as well as the header information.
#          - Some raw data files have duplicate times that are really
#            separate 1-second records, so the time is set manually.
#          - One file (edt_20150611_0404.txt) had zero lat/lon in 
#            the header and surface record, so Scot provided the
#            correct release info and this was hard-coded into
#            the converter.
#
#
# @author Linda Echo-Hawk
# @version DEEPWAVE 2014 for Haast soundings
#          - The converter expects filenames in the format:
#            01-07-2014-release_0600Z-FLEDT.tsv
#          - see comments for Hobart BoM -- these all apply
#
# @author Linda Echo-Hawk
# @version DEEPWAVE 2014 for Hobart BoM
#          - The converter expects filenames in the following
#            format: 94975_YYYYMMDDHHmmss.tsv (e.g., 94975_20140721111731.tsv)
#          - The file contains header info on lines 1-39. Actual data starts 
#            on line 41. 
#          - The radiosonde ID is obtained from the header information.
#          - The lat/lon/alt header values are obtained from the surface
#            data record (t=0).
#          - Missing values are represented by "-32768.00" in the raw data.
#          - The release date and time and obtained from the file name.
#          - Temperature and dewpoint are in Kelvin and must be converted to 
#            Celsius by subtracting 273.15 or using the Perl Library function 
#            convertTemperature.
#
#
# @author Linda Echo-Hawk
# @version DYNAMO 2011 for Sipora Indonesia
#    This code was created by modifying the R/V Sagar Kanya converter.
#          - Header lat/lon/alt info is obtained from the data.  
#          - Release time is obtained from the file name.
#          - Search for "HARD-CODED" to find project-specific items that
#            may require changing.
# This code makes the following assumptions:
#  - That the raw data file names shall be in the form
#        "yymmddhhEDT.tsv" where yy = year, mm = month, dd = day, hh=hour. 
#  - That the raw data is in the Vaisala "Digicora 3" format. The file contains
#         header info on lines 1-39. Actual data starts on line 40. 
#
# Comment to see if the file will save remotely
##Module------------------------------------------------------------------------
package CSU_Mobile_2017_Sounding_Converter;
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
 
use ElevatedStationMap;
use Station;
use ClassConstants qw(:DEFAULT);
use ClassHeader;
use ClassRecord;
use DpgCalculations;

my ($WARN);

printf "\nCSU_Mobile_2017_Sounding_Converter.pl began on ";print scalar localtime;printf "\n";

my $debug            = 0;
my $debug_geo_height = 0;
&main();

printf "\nCSU_Mobile_2017_Sounding_Converter.pl ended on ";print scalar localtime;printf "\n";

##------------------------------------------------------------------------------
# @signature void main()
# nde serial number              N0540317"
#             #-----------------------------------------------------------
#
# <p>Process the CSU Mobile radiosonde data by converting it from 
# the native ASCII format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main 
{
    my $converter = CSU_Mobile_2017_Sounding_Converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature CSU_Mobile_2015_Sounding_Converter new()
# <p>Create a new instance of a CSU_Mobile_2015_Sounding_Converter.</p>
#
# @output $self A new CSU_Mobile_2017_Sounding_Converter object.
##------------------------------------------------------------------------------
sub new 
{
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);
    
    $self->{"stations"} = ElevatedStationMap->new();
	
    # **********************************
    # HARD-CODED
    # **********************************
    $self->{"PROJECT"} = "VORTEX-SE_2017"; 
    $self->{"NETWORK"} = "CSU_Mobile";
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
# <p>Create a default station for the West Texas Mesonetnetwork using the 
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
    $station->setStationName($network);
    $station->setLatLongAccuracy(3);
    
    # ******************************
    # HARD-CODED
    # ******************************
    $station->setCountry("99");
    $station->setReportingFrequency("no set schedule");
    $station->setNetworkIdNumber("99");
    $station->setPlatformIdNumber(415);
    $station->setMobilityFlag("m"); 

    return $station;
}

##------------------------------------------------------------------------------
# @signature String buildLatlonFormat(String value)
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
sub buildLatlonFormat 
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

    if($debug) {printf("parsing header for %s\n",$file);}

    # ********************************
    # HARD-CODED - for project CSU Mobile
    # ********************************

    # Set the type of sounding "Data Type:" header line
    $header->setType("CSU Mobile Sounding");
    $header->setReleaseDirection("Ascending");
    $header->setProject($self->{"PROJECT"});
    
    # The Id will be the prefix of the output file
    $header->setId("CSU_Mobile");

    # "Release Site Type/Site ID:" header line
    $header->setSite("Mobile/CSU_Mobile");


    # ------------------------------------------------
    # Read through the file for additional header info
    # ------------------------------------------------
	my $index = 0;

	foreach my $line (@headerlines) 
	{
            #-----------------------------------------------------------
            #  "Sonde type                        RS41-SGP"
            #-----------------------------------------------------------
	    if ($line =~ /Sonde type/i)
	    {
		chomp ($line);
		my (@sonde_type) = split(' ',$line);
		my $sonde_type_label = "Radiosonde Type";
	        $header->setLine(5, trim($sonde_type_label).":",trim($sonde_type[2]));
	    }

            # -----------------------------------------------------------
            #  "Sonde serial number              N0540317"
            #-----------------------------------------------------------
	    if ($line =~ /Sonde serial number/i)
	    {
		chomp ($line);
		my (@sonde_id) = split(' ',$line);
		my $sonde_id_label = "Radiosonde Serial Number";
	        $header->setLine(6, trim($sonde_id_label).":",trim($sonde_id[3]));
	    }

	    # --------------------------------------------------------
	    # Convert "Pressure: 1013.8 1013.6    0.2" to 
	    #         "Ground Check Pressure: Ref 1013.8 Sonde 1013.6 Corr 0.2"
	    # Using the P correction value in the data headers 
	    # --------------------------------------------------------
	    if ($line =~ /P correction/)
	    {
		chomp $line;
           	my @pres_values = split(' ', trim($line));
		my $GroundCheckPress = trim($pres_values[5]);
            
		if ($debug) {print "   Ground Check Pressure:: $GroundCheckPress\n";}
            
		$header->setLine(8,"Ground Check Pressure Corr:    ", "$GroundCheckPress hPa");
	    }
	
            #-----------------------------------------------------------
   	    # U correction (Uref - U1)                       0.4 %Rh
	    # Ground Check Humidity Correction: from U correction in the header info
            #-----------------------------------------------------------
   	    if ($line =~ /U correction/)
            {
   		chomp ($line);
        	my @humid_values = split(' ', trim($line));
		my $humid_correction = trim($humid_values[5]);
            
		$header->setLine(9,"Ground Check Humidity Corr:    ","$humid_correction %Rh");
   	    }

	    #----------------------------------------------------------------
	    # Release Point Height
	    #----------------------------------------------------------------
	    if ($line =~ /Release point height/)
	    {
		chomp ($line);
		my @altvalues = split(' ', trim($line));
                my $alt = $altvalues[6];

                $header->setAltitude($alt,"m"); 
		
		if ($debug) { print "Altitude: $alt"; }
	    }

	    # ----------------------------------------------
	    # Lat, Lon, and Alt - based on the surface record
	    # ASSUMPTION: $index == 30 is the surface record
	    # ----------------------------------------------
	    if ($index == 30)
	    {
            	my @data = split(' ',$line);
	
		#-----------------------------------------
		# ASSUMPTION: $data[10] = lon
		#             $data[9]  = lat 
		#-----------------------------------------
            	if (($data[10] !~ /-32768/) && ($data[9] !~ /-32768/))
            	{
			    if($file =~ /edt_20170321_2300.txt/)
    			    {
        			if($debug) { print "In the IF FILE matches statement\n"; }
        			my $manual_lat = 34.903;
        			my $manual_lon = -85.805;

        			$header->setLatitude($manual_lat,$self->buildLatlonFormat($manual_lat));
        			$header->setLongitude($manual_lon,$self->buildLatlonFormat($manual_lon));
    			    }

			    else 
			    {
				 $header->setLatitude($data[9],$self->buildLatlonFormat($data[9]));
				 $header->setLongitude($data[10],$self->buildLatlonFormat($data[10]));
			    }

		 }	
			   

	}
		$index++;

	}
	
	#-------------------------------------------------------------
	# Ground Station Software
	#-------------------------------------------------------------    	
	$header->setLine(7,"Ground Station Software: ", "Vaisala MW41");
   
    	# ----------------------------------------------------------
    	# Extract the date and time information from the file name
	# Expects filename similar to: edt_YYYYMMDD_HHMM.txt
	# e.g., edt_20150602_0303.txt
    	# ----------------------------------------------------------
    	if ($debug) { print "file name = $file\n"; } 

	if ($file =~ /(\d{4})(\d{2})(\d{2})_(\d{2})(\d{2})/)
	{
		my ($year, $month, $day, $hour, $min) = ($1,$2,$3,$4,$5);
        	my $date = join ", ", $year, $month, $day;
		my $time = join ":", $hour,$min, "00";
        	
		if ($debug) { print "DATE:  $date   TIME:  $time\n"; }

    		$header->setActualRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
		$header->setNominalRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
	}

    return $header;
} #end parseHeader()
                           
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

	#-------------------------------
	# Generate the sounding header. 
	# ASSUMPTION: Headers are from @lines 0-30
	# ------------------------------	      
	my @headerlines = @lines[0..30];
	my $header = $self->parseHeader($file,@headerlines);

	# Only continue processing the file if a header was created.
	if (defined($header)) 
	 {
		# Determine the station the sounding was released from.
		my $station = $self->{"stations"}->getStation($header->getId(),$self->{"NETWORK"},
							      $header->getLatitude(),$header->getLongitude(),
							      $header->getAltitude());

		if (!defined($station))
		{
			$station = $self->buildDefaultStation($header->getId(),$self->{"NETWORK"});
			$station->setLatitude($header->getLatitude(),$self->buildLatlonFormat($header->getLatitude()));
			$station->setLongitude($header->getLongitude(),$self->buildLatlonFormat($header->getLongitude()));
			$station->setElevation($header->getAltitude(),"m");
			$self->{"stations"}->addStation($station);
		}
	
		$station->insertDate($header->getNominalDate(),"YYYY, MM, DD");

		#Create the output file name and open it
		my $outfile;
		my @time = split(/:/, $header->getActualTime());
		my $hour = $time[0];
		my $min  = $time[1];
		my $sec  = $time[2];
	
		$outfile = sprintf("%s_%04d%02d%02d%02d%02d.cls",
							$header->getId(),
							split(/,/,$header->getActualDate()), $hour, $min);

		printf("\tOutput file name:    %s\n", $outfile);

		open(my $OUT,">".$self->{"OUTPUT_DIR"}."/".$outfile) or die("Can't open output file for $file\n");
		print($OUT $header->toString());

		# Needed for files with duplicate times
		my $current_time = 0;
		my $surfaceRecord = 1;
	
		# Parse the data portion of the input file:
	      	my $index = 0;
	      	foreach my $line (@lines)
	      	{
			#Ignore header lines and check for the last blank line
			if (($index < 30) || ($line =~ /^\s*$/)) 
			{	
				$index++;
				next;
			}
			
		
			my @data = split(' ',$line);
			my $record = ClassRecord->new($WARN,$file);

		#-------------------------------------------------------------
		# ASSUMPTION: The data array is split as followed:
		# height $data[1]	RH        $data[5]	lat $data[9]
		# press  $data[2]	windspeed $data[6]	lon $data[10]
		# temp   $data[3]	winddir   $data[7]
		# dewpt  $data[4]	ascent    $data[8]
		#-------------------------------------------------------------
		
			# Some raw data files will have duplicate times that are really 1sec difference. Manually set the time
			$record->setTime($current_time);
			$current_time++;

			#Set the rest of the data
                 	$record->setAltitude($data[1],"m") if ($data[1] !~ /-32768/);
		        $record->setPressure($data[2],"hPa") if ($data[2] !~ /-32768/);
                        $record->setTemperature(($data[3]),"C") if ($data[3] !~ /-32768/);
                        $record->setDewPoint(($data[4]),"C") if ($data[4] !~ /-32768/);
                        $record->setRelativeHumidity($data[5]) if ($data[5] !~ /-32768/);
                        $record->setWindSpeed($data[6],"m/s") if ($data[6] !~ /-32768/);
                        $record->setWindDirection($data[7]) if ($data[7] !~ /-32768/);
                #       $record->setAscensionRate($data[8],"m/s");

		#	$record->setLatitude($data[9],$self->buildLatlonFormat($data[9]));
		#	$record->setLongitude($data[10],$self->buildLatlonFormat($data[10]));


			# Scot wants the ascention rate missing in the surface record:
			if ($surfaceRecord)
			{
				# Ascention Rate:
				my $ascension_rate = 999.0;
				$record->setAscensionRate($ascension_rate,"m/s");
				
				if($file =~ /edt_20170321_2300.txt/)
				{
					if ($debug) { print "In the file matching of the surface record \n";}
					
					# Longitude:
					my $manual_lon_rec = -85.805;
					$record->setLongitude($manual_lon_rec,$self->buildLatlonFormat($manual_lon_rec));
					
					# Latitude:
					my $manual_lat_rec = 34.903;
					$record->setLatitude($manual_lat_rec,$self->buildLatlonFormat($manual_lat_rec));
				}				

				else #if the file isn't that particular one
				{
					$record->setLatitude($data[9],$self->buildLatlonFormat($data[9]));
					$record->setLongitude($data[10],$self->buildLatlonFormat($data[10]));
				}

				$surfaceRecord = 0;
			}
			else #if the record isn't the surface record
			{	
				$record->setAscensionRate($data[8],"m/s");
				$record->setLatitude($data[9],$self->buildLatlonFormat($data[9]));
				$record->setLongitude($data[10],$self->buildLatlonFormat($data[10]));
			}			
			
			printf($OUT $record->toString());

		$index++;
	     } #end for each loop
  } # end defined header

  else 
  {
	printf("Unable to make a header \n");
  }

} # end sub parseRawFile
 
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
	# HARD-CODED FILE NAME
    # my @files = grep(/^(d{2})-(d{2})-(\d{4})-release_(\d{4})Z-FLEDT\.tsv/,sort(readdir($RAW)));
    my @files = grep(/^edt.+\.txt$/,sort(readdir($RAW)));
    closedir($RAW);
    
    
    open($WARN,">".$self->{"WARN_LOG"}) or die("Can't create ".$self->{"WARN_LOG"});
    
	# printf("Ready to read the files\n");
    foreach my $file (@files) {
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
