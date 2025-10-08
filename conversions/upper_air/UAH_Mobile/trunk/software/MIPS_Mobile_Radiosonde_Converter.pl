#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The MIPS_Mobile_Radiosonde_Converter.pl script converts ASCII
# radiosonde data into the ESC format.  Raw data consists of *.LOG files (data)
# and *.STD files (header information).</p>
#
# @author Linda Echo-Hawk 7 Oct 2015
# @version PECAN 2015 (based on the DYNAMO Pontianak converter)
#          - The converter reads the *.STD files to get the header info 
#            when loadHeaderInfo is called. It reads the *.LOG to get the  
#            raw data records.
#          - The sounding (hash key) that each record belongs
#            to is the  raw data filename (e.g., 652_002). 
#          - The data records are stored in a hash based on the  
#            altitude data (height) (hash key).
#          - Code was added to calculate ascension rate and set the
#            ascent rate flag.
#          - Code was added from the VORTEX2 SUNY converter to 
#            calculate geopotential height.
#          - Wind speed was given in knots in the raw data and
#            was converted to m/s.
#          - A check was added to set all wind values to missing
#            if the winds was > 200 m/s.
#          - Wind direction was given in radians in the raw data 
#            and was converter to degrees.
#          - If the wind direction was equal to 360 degrees, the
#            value was changed to "0" degrees. A check had to be
#            added to guarantee that the current pressure was not
#            zero, else a divide-by-zero error occurs.
#          - Scot let me know that we need to change the surface 
#            lat/lon for all of the 14 July soundings. The correct 
#            release location for all of the 14 July soundings is:
#            38.454N and 99.898W. Use this value for both the header 
#            and the zero second record of the 14 July soundings.
#          - A post-processing step was used to remove descending
#            sonde data, using the script RemoveDescending.pl.
# 
#
# @author Linda Echo-Hawk 1 Oct 2012
# @version DYNAMO 2011-12 Created for Pontianak based on the T-PARC
#            JapaneseResearchVessel_Radiosonde_Converter.pl script.
#          - The converter reads the *.APA files to get the header info 
#            when loadHeaderInfo is called. It reads the *.AED to get the  
#            raw data records.
#          - The sounding (hash key) that each record belongs
#            to is the date portion of the raw data filenames. 
#          - The data records are stored in a hash based on the  
#            altitude data (height) (hash key).
#          - No lat/lon data is available except in the header, so this
#            was used for the surface data record (only) in the output   
#            file, per Scot's instructions.
#          - Code was added to calculate ascension rate and set the
#            ascent rate flag.
#          - Added code to get release time from raw data file name.
#          - I commented out the "addStation" call because of the error
#            "Station MIPS_Mobile at 38.64 -99.808 at height 694.5 in 
#            network MIPS Mobile is already in the StationMap" after
#            which the program dies (Perl Library code).
#
##Module------------------------------------------------------------------------
package MIPS_Mobile_Radiosonde_Converter;
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

use DpgConversions;
use DpgCalculations;
use ElevatedStationMap; 
# Station MIPS_Mobile at 38.64 -99.808 at height 694.5 in network MIPS Mobile is already in the StationMap
use Station;
use ClassConstants qw(:DEFAULT);
use ClassHeader;
use ClassRecord;
use Data::Dumper;

my $debug = 0;
my $debugHeader = 0;

printf "\nMIPS_Mobile_Radiosonde_Converter.pl began on ";print scalar localtime;printf "\n\n";  
&main();
printf "\nMIPS_Mobile_Radiosonde_Converter.pl ended on ";print scalar localtime;printf "\n";

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the MIPS_Mobile_Radiosonde sounding
# data by converting it from the native ASCII format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = MIPS_Mobile_Radiosonde_Converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature MIPS_Mobile_Radiosonde_Converter new()
# <p>Create a new instance of a MIPS_Mobile_Radiosonde_Converter.</p>
#
# @output $self A new MIPS_Mobile_Radiosonde_Converter object.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);

    $self->{"stations"} = ElevatedStationMap->new();

	# --------------------------------
    # HARD-CODED
    # --------------------------------
    $self->{"PROJECT"} = "PECAN";
    $self->{"NETWORK"} = "MIPS Mobile";
    # --------------------------------

    $self->{"FINAL_DIR"} = "../final";   
    $self->{"OUTPUT_DIR"} = "../output";
    $self->{"RAW_DIR"} = "../raw_data";

    $self->{"STATION_FILE"} = sprintf("%s/%s_%s_sounding_stationCD.out",
                                      $self->{"FINAL_DIR"},
                                      $self->cleanForFileName($self->{"NETWORK"}),
                                      $self->cleanForFileName($self->{"PROJECT"}));

    $self->{"SUMMARY"} = $self->{"OUTPUT_DIR"}."/station_summary.log";
    $self->{"WARN_LOG"} = $self->{"OUTPUT_DIR"}."/warning.log";

    return $self;
}


##------------------------------------------------------------------------------
# @signature Station buildDefaultStation(String station_id, String network)
# <p>Create a default station for the R/V Chofu Maru using the specified
# station_id and network.</p>
#
# @input $station_id The identifier of the station to be created.
# @input $network The network the station belongs to.
# @return The new station object with the default values for the network.
##------------------------------------------------------------------------------
sub buildDefaultStation {
    my ($self,$station_id,$network) = @_;

    # HARD-CODED
    my $station = Station->new("MIPS_Mobile",$self->{"NETWORK"});
    
	$station->setStationName("MIPS_Mobile");
	$station->setLatLongAccuracy(3);
    # $station->setStateCode("99");
    $station->setCountry("USA");
    $station->setReportingFrequency("12 hourly");
    $station->setNetworkIdNumber("99");
	# platform 591 Radiosonde, iMet-1
    $station->setPlatformIdNumber(591);
	$station->setMobilityFlag("m");
    return $station;
	
}


##------------------------------------------------------------------------------
# @signature String buildLatlonFormat(String value)
# <p>Generate the decimal format for the specified value.</p>
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

    # Convert spaces to underscores
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

    open(my $WARN,">".$self->{"WARN_LOG"}) or die("Can't create ".$self->{"WARN_LOG"});

    $self->loadHeaderInfo();
    $self->readDataFiles($WARN);
    $self->generateOutputFiles();
    $self->printStationFiles();

    close($WARN);
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

    if ($month =~ /JANUARY/i) { return 1; }
    elsif ($month =~ /FEBRUARY/i) { return 2; }
    elsif ($month =~ /MARCH/i) { return 3; }
    elsif ($month =~ /APRIL/i) { return 4; }
    elsif ($month =~ /MAY/i) { return 5; }
    elsif ($month =~ /JUNE/i) { return 6; }
    elsif ($month =~ /JULY/i) { return 7; }
    elsif ($month =~ /AUGUST/i) { return 8; }
    elsif ($month =~ /SEPTEMBER/i) { return 9; }
    elsif ($month =~ /OCTOBER/i) { return 10; }
    elsif ($month =~ /NOVEMBER/i) { return 11; }
    elsif ($month =~ /DECEMBER/i) { return 12; }
    else { die("Unknown month: $month\n"); }
}


##------------------------------------------------------------------------------
# @signature void generateOutputFiles()
# <p>Create the class files from the information stored in the data hash.</p>
##------------------------------------------------------------------------------
sub generateOutputFiles 
{
    my ($self) = @_;

    foreach my $key (keys(%{ $self->{"soundings"}})) 
	{
 		print "KEY: $key ";

        my $header = $self->{"soundings"}->{$key}->{"header"};

        # print Dumper($self);
		my $outfile = sprintf("%s_%04d%02d%02d%02d%02d.cls",
		                       $header->getId(),
							   split(/,/,$header->getActualDate()),
							   split(/:/,$header->getActualTime()));
		print "Output file: $outfile\n";

    	open(my $OUT,">".$self->{"OUTPUT_DIR"}."/".$outfile)
	        or die("Can't open output file\n");

        print($OUT $header->toString());
		# ------------------------------------------------------------------------
		# This shouldn't work anyway. We changed the sort from $alt to $time...
        foreach my $alt (sort {$a <=> $b} (keys(%{ $self->{"soundings"}->{$key}->{"records"}}))) {
             print($OUT $self->{"soundings"}->{$key}->{"records"}->{$alt}->toString());

        }

        close($OUT);
   }                         
}


##------------------------------------------------------------------------------
# @signature void loadHeaderInfo()
# <p>Create the headers for the class files from the information
# in the *.STD files.</p>
##------------------------------------------------------------------------------
sub loadHeaderInfo {
	my ($self) = @_;

    opendir(my $RAW,$self->{"RAW_DIR"}) 
		or die("Can't read raw directory ".$self->{"RAW_DIR"});
    # ---------------------------------------------------------------
    # Header info is contained in the *.STD files (e.g., 652_002.STD)
    # ---------------------------------------------------------------
    my @files = grep(/\d{3}_\d{3}\.STD$/i,sort(readdir($RAW)));
    foreach my $headerFile (@files)
	{
		printf("Processing header info file: %s\n",$headerFile);
        $headerFile =~ /(\d{3}_\d{3}\.STD)/i;

        open(my $FILE,sprintf("%s/%s",$self->{"RAW_DIR"},$headerFile)) 
			or die("Can't read $headerFile\n");

		my @header_lines = <$FILE>;
		my @lines = @header_lines[0..14];
	    close ($FILE);         

		my $sounding = $headerFile;
      	$sounding =~ /(\d{3}_\d{3})/;
	    $sounding = ($1);        
        print "\tSOUNDING KEY: $sounding\n";

        my $header = ClassHeader->new($self->{"WARN"});
   
	    # ---------------------------------------------
        # HARD-CODED
		# ---------------------------------------------
        $header->setReleaseDirection("Ascending");
        # Set the type of sounding
        $header->setType("UAH MIPS Mobile");
        $header->setProject($self->{"PROJECT"});
	    # The Id will be the prefix of the output file
        $header->setId("UAH_MIPS");
	    # "Release Site Type/Site ID:" header line
        $header->setSite("UAH MIPS Mobile");

        my $date;
		my $time;
		my $sonde_type;


    	# flag for special lat/lon case
		my $is_14july = 0;

        # Read the first lines of the file for additional header info
	    foreach my $line (@lines) 
	    {
            # Date: 16/07/2015  Time: 06:03 GMT   Ref No. : 652_002
			if ($line =~ /Date:/i)
			{
				if ($line =~ /14\/07\/2015/)
				{
					$is_14july = 1;
				}
				chomp ($line);
                my @release_info = split(' ', trim($line));
                # my @rel_date = split('\//', $release_info[3]);
                my @rel_date = split('/', $release_info[3]);
                my $rel_yr = $rel_date[2];
				my $rel_mo = $rel_date[1];
				my $rel_dy = $rel_date[0];
				$date = sprintf("%04d, %02d, %02d", $rel_yr, $rel_mo, $rel_dy);

				my @rel_time = split(':', $release_info[5]);
				my $rel_hr = $rel_time[0];
				my $rel_min = $rel_time[1];
				$time = sprintf("%02d:%02d:00", $rel_hr, $rel_min);

                $header->setActualRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
                $header->setNominalRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);

			    print "\tRELEASE:  $date    $time\n";

			}

            # Add the non-predefined header line to the header.
			# TEMPERATURE  :  0.0 Deg    SONDE TYPE : IMET1 AB
			if ($line =~ /SONDE TYPE/i)
			{
			    chomp ($line);
				my @sonde_info = split(' ', trim($line));
                $sonde_type = join(" ", $sonde_info[7], $sonde_info[8]);
				print "\tSONDE TYPE: $sonde_type\n";
				
   	            my $sonde_type_label = "Radiosonde Type";
			    $header->setLine(5, $sonde_type_label.":",trim($sonde_type)); 
			}
            # HUMIDITY   :   96.8 %   SONDE No.  : 34449
			if ($line =~ /SONDE No/i)
			{
				chomp ($line);
				my @id_info = split(' ', trim($line));
				# print "ID INFO: @id_info\n";
				my $sonde_id = trim($id_info[7]);
    	        my $sonde_id_label = "Radiosonde Serial Number";
				$header->setLine(6, $sonde_id_label.":",trim($sonde_id)); 
			}

			# LOCATION: OCT - 3 40.260N 96.712W
            if ($line =~ /LOCATION/i)
			{
			    chomp ($line);
			    my (@act_releaseLoc) = (split(' ',(split(/:/,$line))[1]));

			    my $hdr_lat = $act_releaseLoc[3];
				$hdr_lat =~ s/N//g;
				print "\t@act_releaseLoc\n";

			    my $hdr_lon = $act_releaseLoc[4];
                $hdr_lon =~ s/W//g;
				# since the longitude is west,
				# we want to add the minus sign
				my $minus = "-";
				$hdr_lon = $minus . $hdr_lon;

        		if ($is_14july)
				{
					$hdr_lat = 38.454;
					$hdr_lon = -99.898;
				}

                print "\tLAT: $hdr_lat  LON: $hdr_lon\n";

                $header->setLatitude($hdr_lat,$self->buildLatlonFormat($hdr_lat));
	            $header->setLongitude($hdr_lon,$self->buildLatlonFormat($hdr_lon)); 
			}
			# HEIGHT  :  395.8 Meter
			if ($line =~   /HEIGHT/i)
			{
				chomp ($line);
				my @alt_info = split(' ', trim($line));
				my $hdr_alt = $alt_info[2];
                $header->setAltitude($hdr_alt,"m");
			}

			my $ground_station = "iMET-3150";
			$header->setLine(7, "Ground Station Equipment:", ($ground_station));
		}
        
    	# rset flag for special lat/lon case
		$is_14july = 0;

        #-----------------------------------------------------------------
        # Set the station information
        #-----------------------------------------------------------------

        my $station = $self->{"stations"}->getStation($header->getId(),$self->{"NETWORK"}, 
                                  $header->getLatitude(),$header->getLongitude(),
		   	    		          $header->getAltitude());
        
        if (!defined($station)) {
			
            $station = $self->buildDefaultStation($header->getId(),$self->{"NETWORK"});
            $station->setLatitude($header->getLatitude(),$self->buildLatlonFormat($header->getLatitude()));
            $station->setLongitude($header->getLongitude(),$self->buildLatlonFormat($header->getLongitude()));
            $station->setElevation($header->getAltitude(),"m");
		    # $self->{"stations"}->addStation($station);
        }
        $station->insertDate($date,"YYYY, MM, DD");
		

        # --------------------------------------------------   
		# save this header to the hash for key = $sounding 
		# (date/time portion of filename)
        # --------------------------------------------------   
    	$self->{"soundings"}->{$sounding}->{"header"} = $header;    
	}
}


##------------------------------------------------------------------------------
# @signature void parseDataFile(FileHandle WARN, String file)
# <p>Parse the data values for the records in the file by altitude.</p>
#
# @input $WARN The file handle where warnings are to be stored.
# @input $file The name of the file to be parsed.
##------------------------------------------------------------------------------
sub parseDataFile 
{
    my ($self,$WARN,$file) = @_;

    printf("Processing data file: %s\n",$file);

    $file =~ /(\d{3}_\d{3}\.LOG)/i;
	my $sounding = $file;
 	$sounding =~ /(\d{3}_\d{3})/;
	$sounding = ($1);   
    print "\tSOUNDING KEY: $sounding\n";


    # flag for special lat/lon case
 	my $is_14july_data = 0;

    if (($sounding =~ /^646/) || ($sounding =~ /^647/) ||
		($sounding =~ /648_001/))
	{
		$is_14july_data = 1;
	}


    open(my $FILE,sprintf("%s/%s",$self->{"RAW_DIR"},$file)) or die("Can't read $file\n");
	my @lines = <$FILE>;
	close ($FILE);


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
	
    # ---------------------------------------------
    # No time in raw data, set for 1 sec resolution
    # ---------------------------------------------
	my $recordTime = 0;

    # time = 1 sec
    # FROM Purdue:                            FOR PECAN:
    # wind dir = $data[8] col 9            wnddir = $data[7] radians
	# wind spd = $data[9] col 1            wndspd = $data[8] knots
	# temp =     $data[1]                  temp  = $data[0]
	# RH =       $data[3]                  RH    = $data[2]
	# press =    $data[4]                  press = $data[3]
	# alt =      $data[5]                  alt =   $data[4]
	# lat =      $data[14] 12 or 17?       lat =   $data[11]
	# lon =      $data[16]                 lon =   $data[12]
	#
  
    foreach my $line (@lines) 
	{
		chomp($line);
		# Skip any blank lines.
		next if ($line =~ /^\s*$/);

		my @data = split(' ',$line);

		# create a $record object
        
		# sorting on altitude has problems because altitudes are not rising/descending linearly
		# my $record = $self->{"soundings"}->{$sounding}->{"records"}->{sprintf("%5.1f",$alt)};
		my $record = $self->{"soundings"}->{$sounding}->{"records"}->{sprintf("%5.1f",$recordTime)};
        $record = ClassRecord->new($WARN,$file);
		
		# need to manually set the time
	    $record->setTime($recordTime);
		
        # $record->setAltitude($data[4],"m") unless($data[4] =~ /-999/);
        $record->setPressure($data[3],"hPa") unless($data[3] =~ /-999/);
        $record->setTemperature($data[0],"C") unless($data[0] =~ /-999/);
        $record->setRelativeHumidity($data[2]) unless($data[2] =~ /-999/);

        # -------------------------------------------------
		# NOTE: Wind Speed is in knots, 
		# Wind Direction is in radians
		# Col 8: Wind direction (radians)(i.e., $data[7])
		# Col 9: Wind speed (kts)(i.e., $data[8])
        # -------------------------------------------------
		my $windDir = $data[7];
		my $windSpeed = $data[8];
       
		my $convertedWindSpeed = convertVelocity($windSpeed,"knot", "m/s");
		# print "\tWINDSPD: $windSpeed  CONVERTED WINDSPD: $convertedWindSpeed\n"; 

		if ($convertedWindSpeed <= 200)
		{
			$record->setWindSpeed($convertedWindSpeed,"m/s");

        	# The wind direction is in radians, so convert the values to degrees.
    	    # use convertAngle (initial angle, initial units, target units)
			my $convertedWindDir = convertAngle($windDir,"rad","deg");
			# print "\t INITIAL CONVERSION: $convertedWindDir  ";
			if ($convertedWindDir >= 360)
			{
				$convertedWindDir = 0;
			}
	        # print "WINDDIR: $windDir  FINAL CONVERTED WINDDIR: $convertedWindDir\n";
        	
			$record->setWindDirection($convertedWindDir);
		}

		# ----------------------------------------
		# End Wind Section
		# ----------------------------------------

	    # $record->setLongitude($data[12],$self->buildLatlonFormat($data[12])) if ($data[12] !~ /-999/);
	    # $record->setLatitude($data[11],$self->buildLatlonFormat($data[11])) if ($data[11] !~ /-999/);

		if ($surfaceRecord)
		{
			if ($is_14july_data)
			{
                # set special case lat/lon
				my $july14_lat = 38.454;
				my $july14_lon = -99.898;

	    		$record->setLongitude($july14_lon,$self->buildLatlonFormat($july14_lon));
			    $record->setLatitude($july14_lat,$self->buildLatlonFormat($july14_lat));
			}
			else
			{
	    		$record->setLongitude($data[12],$self->buildLatlonFormat($data[12])) if ($data[12] !~ /-999/);
			    $record->setLatitude($data[11],$self->buildLatlonFormat($data[11])) if ($data[11] !~ /-999/);
			}
			# for the surface record only (t=0) use the raw data altitude
	    	$record->setAltitude($data[4],"m");
			$surfaceRecord = 0;

		}
		else

        # if (!$surfaceRecord)
		{

	    	$record->setLongitude($data[12],$self->buildLatlonFormat($data[12])) if ($data[12] !~ /-999/);
	    	$record->setLatitude($data[11],$self->buildLatlonFormat($data[11])) if ($data[11] !~ /-999/);


	        #-----------------------------------------------------------------
    	    # BEWARE:  For PECAN 2015 SLoehrer says there are issues 
	        	# with the raw data altitudes, so compute the geopotential 
			# height/altitude and insert for all other than surface record.
        	# call calculateAltitude(last_press,last_temp,last_dewpt,last_alt,
			#                        this_press,this_temp,this_dewpt,this_alt,1)
			# Note that the last three parms in calculateAltitude
        	# are the pressure, temp, and dewpt (undefined for this dataset)
	        # for the current record. To check the altitude calculations, see
    	    # the web interface tool at 
        	#
	        # http://dmg.eol.ucar.edu/cgi-bin/conversions/calculations/altitude
			# NOTE: This code taken from VORTEX2 SUNY converter.
        	#-------------------------------------------------------------------
			if ($debug) 
	        { 
    	        my $prev_press = $previous_record->getPressure(); 
        	    my $prev_temp = $previous_record->getTemperature(); 
            	my $prev_alt = $previous_record->getAltitude();
	            print "\nCalc Geopotential Height from previous press = $prev_press, temp = $prev_temp, alt = $prev_alt,\n";
				print "and current press = $data[3] and temp = $data[0]\n"; 
        	}

	        if ($previous_record->getPressure() < 9990.0)
    	    {
        	    if ($debug){ print "prev_press < 9990.0 - NOT missing so calculate geopotential height.\n"; }
                # ---------------------------------------------------------
				# print "\tTHIS PRESSURE: $data[3]\n";
                # for 621_001.LOG, pressure is 0.000 and illegal divison
				# error occurs when calculateAltitude is called
                # ---------------------------------------------------------
				if ($data[3] != 0.000)
				{
					$geopotential_height = calculateAltitude($previous_record->getPressure(),
                	                                 $previous_record->getTemperature(), 
													 undef, $previous_record->getAltitude(), 
													 $data[3], $data[0], undef, 1);
				}
				else
				{
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
    	    }
		}

        #-------------------------------------------------------
        # Calculate the ascension rate which is the difference
        # in altitudes divided by the change in time. Ascension
        # rates can be positive, zero, or negative. But the time
        # must always be increasing (the norm) and not missing.
        #-------------------------------------------------------
        if ($debug) { my $time = $record->getTime(); my $alt = $record->getAltitude(); 
              print "\nNEXT: prev_time: $prev_time, current Time: $time, prev_alt: $prev_alt, current Alt: $alt\n"; }

        if ($prev_time != 9999  && $record->getTime()     != 9999  &&
            $prev_alt  != 99999 && $record->getAltitude() != 99999 &&
            $prev_time != $record->getTime() ) 
        {
			$record->setAscensionRate( ($record->getAltitude() - $prev_alt) /
                                    ($record->getTime() - $prev_time),"m/s");
			$record->setAscensionRateFlag("99.0");

            if ($debug) { print "Calc Ascension Rate.\n"; }
        }

        # Save the next non-missing values. 
        # Ascension rates over spans of missing values are OK.
        if ($debug) { my $rectime = $record->getTime(); my $recalt = $record->getAltitude();
              print "Try SAVE Line: rec Time, rec Alt:: $rectime, $recalt\n"; }

        if ($record->getTime() != 9999 && $record->getAltitude() != 99999)
        {
             $prev_time = $record->getTime();
             $prev_alt = $record->getAltitude();

             if ($debug) { print "Current rec has valid Time and Alt. Save as previous.\n"; }
        }
        #-------------------------------------------------------
        # End Calculate Ascension Rate
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
			if ($debug) { print "Move to next record! previous_record = record \n\n"; }
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

        # $self->{"soundings"}->{$sounding}->{"records"}->{sprintf("%5.1f",$alt)} = $record; 
        $self->{"soundings"}->{$sounding}->{"records"}->{sprintf("%5.1f",$recordTime)} = $record; 

	    $recordTime++;
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

    open(my $SUMMARY, ">".$self->{"SUMMARY"}) || die("Cannot create the ".$self->{"SUMMARY"}." file.\n");
    print($SUMMARY $self->{"stations"}->getStationSummary());
    close($SUMMARY);   
}


##------------------------------------------------------------------------------
# @signature void readDataFiles()
# <p>Read in the files from the raw data directory and convert each into an
# ESC formatted file.</p>
##------------------------------------------------------------------------------
sub readDataFiles {
    my ($self,$WARN) = @_;

    opendir(my $RAW,$self->{"RAW_DIR"}) or die("Can't read raw directory ".$self->{"RAW_DIR"});

    my @files = grep(/(\d{3}_\d{3}\.LOG$)/i,sort(readdir($RAW)));
    foreach my $datafile (@files) {
        $self->parseDataFile($WARN,$datafile);
    }
    rewinddir($RAW);
    

    closedir($RAW);
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

