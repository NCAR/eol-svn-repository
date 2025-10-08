#! /usr/bin/perl -w
##Module------------------------------------------------------------------------
# <p>The CSU_Mobile_2015_Sounding_Converter.pl script is used for converting high
# resolution radiosonde data from ASCII formatted data to the EOL Sounding 
# Composite (ESC) format.</p> 
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
#
##Module------------------------------------------------------------------------
package CSU_Mobile_2015_Sounding_Converter;
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
 
use ElevatedStationMap;
use Station;
use ClassConstants qw(:DEFAULT);
use ClassHeader;
use ClassRecord;
use DpgCalculations;

my ($WARN);

printf "\nCSU_Mobile_2015_Sounding_Converter.pl began on ";print scalar localtime;printf "\n";
my $debug = 0;
my $debug_geo_height = 0;
&main();
printf "\nCSU_Mobile_2015_Sounding_Converter.pl ended on ";print scalar localtime;printf "\n";

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the CSU Mobile radiosonde data by converting it from 
# the native ASCII format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = CSU_Mobile_2015_Sounding_Converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature CSU_Mobile_2015_Sounding_Converter new()
# <p>Create a new instance of a CSU_Mobile_2015_Sounding_Converter.</p>
#
# @output $self A new CSU_Mobile_2015_Sounding_Converter object.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);
    
    $self->{"stations"} = ElevatedStationMap->new();

    # HARD-CODED
    $self->{"PROJECT"} = "PECAN";
    # HARD-CODED
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
sub buildDefaultStation {
    my ($self,$station_id,$network) = @_;
    my $station = Station->new($station_id,$network);
    $station->setStationName($network);
    $station->setLatLongAccuracy(3);
    # HARD-CODED
	$station->setCountry("99");
    # $station->setStateCode("48");
    $station->setReportingFrequency("no set schedule");
    $station->setNetworkIdNumber("99");
    # platform, 415	Radiosonde, Vaisala RS92-SGP
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
sub buildLatlonFormat {
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

	# printf("parsing header for %s\n",$file);

    # Set the type of sounding "Data Type:" header line
    $header->setType("CSU Mobile Radiosonde");
    $header->setReleaseDirection("Ascending");

    $header->setProject($self->{"PROJECT"});
    
    # HARD-CODED
	# The Id will be the prefix of the output file
    $header->setId("CSU_Mobile");
	# "Release Site Type/Site ID:" header line
    $header->setSite("Mobile/CSU_Mobile");


    # ------------------------------------------------
    # Read through the file for additional header info
    # ------------------------------------------------
	my $u1;
	my $u2;
	my $index = 0;

	foreach my $line (@headerlines) 
	{
	    if ($line =~ /Sonde type/i)
	    {
			chomp ($line);
		    my (@sonde_type) = split(' ',$line);

            #-----------------------------------------------------------
            # "Sonde type                                      RS92-SGP"
            #-----------------------------------------------------------
			my $sonde_type_label = "Radiosonde Type";
	        $header->setLine(5, trim($sonde_type_label).":",trim($sonde_type[2]));
	    }

	    if ($line =~ /Sonde serial number/i)
	    {
			chomp ($line);
		    my (@sonde_id) = split(' ',$line);
            #-----------------------------------------------------------
            # "Sonde serial number                             K2333016"
            #-----------------------------------------------------------
			my $sonde_id_label = "Radiosonde Serial Number";
	        $header->setLine(6, trim($sonde_id_label).":",trim($sonde_id[3]));
	    }
		if ($line =~ /P correction/)
		{
			chomp $line;
            my @pres_values = split(' ', trim($line));

            #-------------------------------------------------------------
            # Convert "Pressure    : 1013.8 1013.6    0.2"     to
            # "Ground Check Pressure:    Ref 1013.8 Sonde 1013.6 Corr 0.2"
			# P correction (Pref - P)                         0.80 hPa
            #-------------------------------------------------------------
            my $GroundCheckPress = trim($pres_values[5]);
            if ($debug) {print "   Ground Check Pressure:: $GroundCheckPress\n";}
            $header->setLine(8,"Ground Check Pressure Corr:    ", $GroundCheckPress);
		}
		if ($line =~ /T correction/)
		{
   			chomp ($line);
            my @temp_values = split(' ', trim($line));

            #-----------------------------------------------------------
			# T correction (Tref - T)                         -0.05 ?C
            #-----------------------------------------------------------
            # my $GroundCheckTemp = trim("Ref ". $values[2]." Sonde ".
            #               $values[3]." Corr ".$values[4]);
            my $GroundCheckTemp = trim($temp_values[5]);
            if ($debug) {print "   Ground Check Temperature:: $GroundCheckTemp\n";}
            $header->setLine(9,"Ground Check Temperature Corr: ", $GroundCheckTemp);

  		}
   		if ($line =~ /U1 correction/)
   		{
   			chomp ($line);
            my @u1_values = split(' ', trim($line));
            $u1 = $u1_values[5];
            #-----------------------------------------------------------
   			# U1 correction (Uref - U1)                       -0.1 %Rh
			# Ground Check Humidity Correction: from U1 and U2 correction in raw headers show as U1/U2
			# U1: -0.1 / U2: -0.1
            #-----------------------------------------------------------

   		}
   		if ($line =~ /U2 correction/)
   		{
   			chomp ($line);
            my @u2_values = split(' ', trim($line));
            $u2 = $u2_values[5];

            #-----------------------------------------------------------
			# U2 correction (Uref - U2)                       -0.1 %Rh
            #-----------------------------------------------------------
   		}

		if ($line =~ /Release point height/)
		{
			chomp ($line);
			my @altvalues = split(' ', trim($line));
            my $alt = $altvalues[6];

            $header->setAltitude($alt,"m"); 
            #-----------------------------------------------------------
			# Release point height from sea level             1005 m
            #-----------------------------------------------------------
		}

		# ---------------------------------
		# If $index == 32 then this is
		# the surface data line
		# ---------------------------------
		if ($index == 32)
		{
            my @data = split(' ',$line);
			# $data[10] = lon, $data[9] = lat
            if (($data[10] !~ /-32768/) && ($data[9] !~ /-32768/))
            {

                # -----------------------------------------------
   				# for edt_20150611_0404.txt, the file with 0
				# lat/lon in the header info and surface record
				# # ---------------------------------------------
				if ($data[10] =~ /0.00/) # 40.3929  -97.183
				{
					$data[10] = -97.183;
				}
                $header->setLongitude($data[10],$self->buildLatlonFormat($data[10]));
				if ($data[9] =~ /0.00/)
				{
					$data[9] = 40.3929;

	                # ----------------------------------
					# for edt_20150611_0404.txt, the 
					# file with 0 lat/lon in the 
					# header info and surface
					# record, also fix the altitude
	                # ----------------------------------
					my $special_case_alt = 442;
					$header->setAltitude($special_case_alt,"m");
				}
                $header->setLatitude($data[9],$self->buildLatlonFormat($data[9]));
                # header altitude same as surface record altitude
                # $header->setAltitude($data[1],"m");
	            # last;
	        }
		}

		$index++;

	}

    $header->setLine(7,"Ground Station Software: ", "Digicora MW41 2.1.0");
    $header->setLine(10,"Ground Check Humidity Corr: ", "U1: $u1/U2: $u2");
    # ----------------------------------------------------------
    # Extract the date and time information from the file name
    # Expects filename similar to: edt_YYYYMMDD_HHMM.txt
	# e.g., edt_20150602_0303.txt
    # ----------------------------------------------------------
    # print "file name = $file\n"; 

	if ($file =~ /(\d{4})(\d{2})(\d{2})_(\d{2})(\d{2})/)
	{
		my ($year, $month, $day, $hour, $min) = ($1,$2,$3,$4,$5);
        my $date = join ", ", $year, $month, $day;
		my $time = join ":", $hour,$min, "00";
        # print "DATE:  $date   TIME:  $time\n";

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
	my @headerlines = @lines[0..32];
    my $header = $self->parseHeader($file,@headerlines);

    # Only continue processing the file if a header was created.
    if (defined($header)) {

	# Determine the station the sounding was released from.
	my $station = $self->{"stations"}->getStation($header->getId(),$self->{"NETWORK"},
						      $header->getLatitude(),$header->getLongitude(),
						      $header->getAltitude());
	if (!defined($station)) {
	    $station = $self->buildDefaultStation($header->getId(),$self->{"NETWORK"});
	    $station->setLatitude($header->getLatitude(),$self->buildLatlonFormat($header->getLatitude()));
	    $station->setLongitude($header->getLongitude(),$self->buildLatlonFormat($header->getLongitude()));
	    $station->setElevation($header->getAltitude(),"m");
	    $self->{"stations"}->addStation($station);
	}
	$station->insertDate($header->getNominalDate(),"YYYY, MM, DD");

    # ----------------------------------------------------
    # Create the output file name and open the output file
    # ----------------------------------------------------
    my $outfile;
	my ($hour, $min, $sec) = split (/:/, $header->getActualTime());

	$outfile = sprintf("%s_%04d%02d%02d%02d%02d.cls", 
  	 					   $header->getId(),
	   					   split(/,/,$header->getActualDate()),
	   					   $hour, $min);
 
    printf("\tOutput file name:  %s\n", $outfile);


	open(my $OUT,">".$self->{"OUTPUT_DIR"}."/".$outfile)
	    or die("Can't open output file for $file\n");

	print($OUT $header->toString());
	
    # ----------------------------------------
    # Needed for code to derive ascension rate
    # ----------------------------------------
    my $prev_time = 9999.0;
    my $prev_alt = 99999.0;

    # ---------------------------------------------
    # Needed for code to derive geopotential height
    # ---------------------------------------------
	my $previous_record;
	my $geopotential_height;
	my $surfaceRecord = 1;

    # -----------------------------------------
	# Needed for files with duplicate times
    # -----------------------------------------
    my $current_time = 0;

	# ----------------------------------------------------
    # Parse the data portion of the input file
    # ----------------------------------------------------
	my $index = 0;
	foreach my $line (@lines) {
	    # Ignore the header lines and
		# check for blank last line
	    if (($index < 32) || ($line =~ /^\s*$/)) { $index++; next; }
	    
	    my @data = split(' ',$line);
	    my $record = ClassRecord->new($WARN,$file);

		# -------------------------------------------------
		# height $data[1]       windspeed   $data[6]
		# press  $data[2]       wind dir    $data[7]
		# temp   $data[3]       ascent      $data[8]
		# dewpt  $data[4]       lat         $data[9]
		# RH     $data[5]       lon         $data[10]
		# -------------------------------------------------
        
		
		# -------------------------------------------
	    # Some raw data files have duplicate times
		# that are really separate 1-second records.
		# Manually set the time.
        # -------------------------------------------
	    $record->setTime($current_time);
		$current_time++;
		
	    $record->setPressure($data[2],"mb") if ($data[2] !~ /-32768/);
	    $record->setTemperature(($data[3]),"C") if ($data[3] !~ /-32768/);    
		$record->setDewPoint(($data[4]),"C") if ($data[4] !~ /-32768/);
	    $record->setRelativeHumidity($data[5]) if ($data[5] !~ /-32768/);
	    $record->setWindSpeed($data[6],"m/s") if ($data[6] !~ /-32768/);
	    $record->setWindDirection($data[7]) if ($data[7] !~ /-32768/);

		if ($surfaceRecord)
		{
			if ($data[10] =~ /0.00/) # 40.3929  -97.183
			{
				$data[10] = -97.183;
			}
			if ($data[9] =~ /0.00/)
			{
				$data[9] = 40.3929;

                # ----------------------------------
				# for edt_20150611_0404.txt, the 
				# file with 0 lat/lon in the 
				# header info and surface
				# record, also fix the altitude
                # ----------------------------------
				$data[1] = 442;
			}

            $record->setLongitude($data[10],$self->buildLatlonFormat($data[10]));
            $record->setLatitude($data[9],$self->buildLatlonFormat($data[9]));
			$record->setAltitude($data[1],"m");
			$surfaceRecord = 0;
        }

		else  # if (!$surfaceRecord)
		{
		    # get the lat/lon data 
		    if ($data[10] !~ /-32768/) {
			$record->setLongitude($data[10],$self->buildLatlonFormat($data[10]));
		    }
		    if ($data[9] !~ /-32768/) {
			$record->setLatitude($data[9],$self->buildLatlonFormat($data[9]));
		    }

			# ---------------------------------------------------------------
			# Calculate geopotential height
	        #-----------------------------------------------------------------
    	    # BEWARE:  For PECAN 2015 SLoehrer says there are issues 
           	# with the raw data altitudes, so compute the geopotential 
			# height/altitude and insert for all other than surface record.
        	# call calculateAltitude(last_press,last_temp,last_dewpt,last_alt,
			#                        this_press,this_temp,this_dewpt,this_alt,1)
			# Note that the last three parms in calculateAltitude
        	# are the pressure, temp, and dewpt for the current
	        # record. To check the altitude calculations, see
    	    # the web interface tool at 
        	#
	        # http://dmg.eol.ucar.edu/cgi-bin/conversions/calculations/altitude
			# NOTE: This code taken from VORTEX2 SUNY converter.
        	#-------------------------------------------------------------------
			if ($debug_geo_height) 
	        { 
    	        my $prev_press = $previous_record->getPressure(); 
        	    my $prev_temp = $previous_record->getTemperature(); 
            	my $prev_alt = $previous_record->getAltitude();
				my $prev_dewpt = $previous_record->getDewPoint();
	            print "\nCalc Geopotential Height from previous press = $prev_press, temp = $prev_temp, alt = $prev_alt,\n";
				print "and current press = $data[2] and temp = $data[3]\n"; 
        	}

	        if ($previous_record->getPressure() < 9990.0)
    	    {
        	    if ($debug){ print "prev_press < 9990.0 - NOT missing so calculate geopotential height.\n"; }
                # ---------------------------------------------------------
				# print "\tTHIS PRESSURE: $data[2]\n";
                # for 621_001.LOG, pressure is 0.000 and illegal divison
				# error occurs when calculateAltitude is called
                # ---------------------------------------------------------
				if ($data[2] != 0.000)
				{
					# print "Calculating geopotential height\n";
					$geopotential_height = calculateAltitude($previous_record->getPressure(),
                	                                 $previous_record->getTemperature(), 
													 $previous_record->getDewPoint(), 
													 $previous_record->getAltitude(), 
													 $data[2], $data[3], $data[4], 1);
				}
				else
				{
					print "Bad Pressure $data[2] -- Geopotential height not calculated\n";
					$geopotential_height = 99999.0;
				}
	            if (defined($geopotential_height))
				{
	    	        $record->setAltitude($geopotential_height,"m");
				}
				else
				{
					print "WARNING: Was not able to calculate geopotential height\n";
					$geopotential_height = 99999.0;
				}
    	    } # end calculate geopotential height
		} # end if !$surfaceRecord

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
        if ($debug) { my $time = $record->getTime(); my $alt = $record->getAltitude(); 
              print "\nNEXT Line: prev_time, rec Time, prev_alt, rec Alt:: $prev_time, $time, $prev_alt, $alt\n"; }

        if ($prev_time != 9999  && $record->getTime()     != 9999  &&
            $prev_alt  != 99999 && $record->getAltitude() != 99999 &&
            $prev_time != $record->getTime() ) 
        {
             $record->setAscensionRate( ($record->getAltitude() - $prev_alt) /
                                     ($record->getTime() - $prev_time),"m/s");

             if ($debug) { print "Calc Ascension Rate.\n"; }
        }

        #-----------------------------------------------------
        # Only save off the next non-missing values. 
        # Ascension rates over spans of missing values are OK.
        #-----------------------------------------------------
        if ($debug) { my $rectime = $record->getTime(); my $recalt = $record->getAltitude();
              print "Try SAVE Line: rec Time, rec Alt:: $rectime, $recalt\n"; }

        if ($record->getTime() != 9999 && $record->getAltitude() != 99999)
        {
            $prev_time = $record->getTime();
            $prev_alt = $record->getAltitude();

            if ($debug) { print "Current rec has valid Time and Alt. Save as previous.\n"; }
        }
        #-------------------------------------------------------
		# Completed the ascension rate data
        #-------------------------------------------------------

        #---------------------------------------------
        # Only save current record as previous record
        # if values not missing. This affects the
        # calculations of the geopotential height,
        # as the previous height must be non-missing. 
        #---------------------------------------------
        if ($debug_geo_height) 
		{
			my $press = $record->getPressure(); 
			my $temp = $record->getTemperature();
            my $alt = $record->getAltitude();
			my $dewpt = $record->getDewPoint();
            print "\tCurrent Rec: press = $press, temp = $temp, ";
			print "dewpt = $dewpt, alt = $alt\n";
        }
        if ( ($record->getPressure() < 9999.0)  && ($record->getTemperature() < 999.0)
             && ($record->getAltitude() < 99999.0) )
		{
			# if ($debug_geo_height) { print "\tAssign current record to previous_record \n\n"; }
            $previous_record = $record;
        } 
		else 
		{
			if ($debug_geo_height) 
			{
				print "\t\tDo NOT assign current record to previous_record! ";
				print "Current record has missing values.\n\n";
		    }
        }
	    
		printf($OUT $record->toString());
    }
	}
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
