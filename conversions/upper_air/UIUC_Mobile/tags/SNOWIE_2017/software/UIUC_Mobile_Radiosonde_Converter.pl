#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The UIUC_Mobile_Radiosonde_Converter.pl script is used for converting high 
# resolution radiosonde ascii data to the EOL Sounding Composite
# (ESC) format.</p> 
#
# @usage UIUC_Mobile_Radiosonde_Converter.pl >&! results.txt
#                       
# @author Linda Echo-Hawk 2017-12-18
# @version SNOWIE Created based on the IPC_Crouch_Radiosonde_Converter.pl and
#            OWLeS GRAW Converter scripts
#          - Original raw data is in NetCDF4 which we were unable to read
#            with the ARMnetCDF_to_ESC.pl script (reads NetCDF3) so see
#            the /convert_raw_data diretory README.txt file for instructions
#            to convert the files to ascii. We used a python netcdfreadout.py
#            script provided by the data source and modified by Janet Anstett 
#            to put the data in tabular form. Linda E-H ran ncdump -h to dump 
#            the NetCDF header info into files which were concatenated with
#            the data files to form the new ascii raw data.
#          - Surface lat/lon/alt values are taken from the NetCDF header info.
#          - Code was added to get the release site from the file name, and 
#            also to check and correct the March files that have incorrect
#            release sites in the file name (resinn should be Idaho Power
#            Company Garage) based on notes from Scot L.
#          - Code to calculated geopotential height is included.
#          - Surface lat/lon/alt values are from the global attributes
#            section of the NetCDF file.
#          - Code to put the records into an @record_list array for reversal 
#            was left in place (commented out) in case Scot L. determines
#            that there are descending records that need to be removed.
#          - Removed a command line switch that had been used to determine
#            if altitude needed to be converted from feet to meters
#
#
# @author Linda Echo-Hawk 2014-08-08
# @version OWLeS Revised for the Univ of Utah North Redfield soundings
#          - Raw data files were renamed to include the sonde id in the name
#          - Added code to calculate ascent rate
#          - Altitude was given in meters
#          - Removed check for "header" info at bottom of file
#          - Header lat/lon/alt obtained from surface data record.
#          - Removed conversion for Wind speed since it is given in m/s.
#          - Removed hard-coded corrections for surface altitudes.
#
#
# @author Linda Echo-Hawk 2012-05-18
# @version DYNAMO  Created based on VORTEX2 Singapore_Radiosonde_Converter.pl.
#          - Raw data files are ascii format and have *.txt extension
#            (first run dos2unix)
#          - Converter expects the actual data to begin
#            after the header lines (line number varies for
#            some files).  
#          - Header lat/lon/alt is hard-coded from csv file
#          - Release time is obtained from the file name.
#          - There is header information at the end of the file, so
#            there is a code check for "Tropopauses" to signal the
#            end of the data.
#          - Wind speed must be converted from knots to meters/second.
#          - Altitude must be converted from feet to meters.  NOTE that
#            some files have altitude in meters.  These files will be 
#            processed separately.  A command line switch has been added
#            so that the converter will know whether or not the altitude
#            needs to be converted from feet to meters.
#          - A correction factor was hard-coded in to handle two files
#            with incorrect surface altitudes. The default value of 
#            the correction factor is zero (for all other files).
#          - Code was added to remove the descending data after the 
#            sondes start to fall (ascent rate < 0).  
#
# @use     GRAW_Radiosonde_Converter.pl >&! results.txt
#
#         
##Module------------------------------------------------------------------------
package UIUC_Mobile_Radiosonde_Converter;
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

printf "\nUIUC_Mobile_Radiosonde_Converter.pl began on ";print scalar localtime;printf "\n";
my $debug = 0;
my $debug_geopotential_height = 0;

&main();
printf "\nUIUC_Mobile_Radiosonde_Converter.pl ended on ";print scalar localtime;printf "\n";

my $filename;

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the UIUC_Mobile radiosonde data by converting it from 
# the native ASCII format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = UIUC_Mobile_Radiosonde_Converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature UIUC_Mobile_Radiosonde_Converter new()
# <p>Create a new instance of a UIUC_Mobile_Radiosonde_Converter.</p>
#
# @output $self A new UIUC_Mobile_Radiosonde_Converter object.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);
    
    $self->{"stations"} = SimpleStationMap->new();

    # HARD-CODED
    $self->{"PROJECT"} = "SNOWIE";
    # HARD-CODED
    $self->{"NETWORK"} = "UIUC_Mobile";
    
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
    $station->setStationName("UIUC_Mobile");
    # HARD-CODED
    $station->setStateCode("99");
    $station->setReportingFrequency("no set schedule");
    $station->setNetworkIdNumber("99");
    # platform 416	Radiosonde, GRAW DFM-09
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
# base lat = 36.6100006103516 base lon = -97.4899978637695
# Lat format = DDDDDDDDDDDDDDDD  Lon format = -DDDDDDDDDDDDDDDD  
#
# @input $value The value of the lat/lon being formatted.
# @output $fmt The format that corresponds to the value.
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

    $filename = $file;

    # HARD-CODED
    # Set the type of sounding
    $header->setType("UIUC Mobile Sounding Data");
    $header->setReleaseDirection("Ascending");
    $header->setProject($self->{"PROJECT"});
	# The Id will be the prefix of the output file
    # and appears in the stationCD.out file
    $header->setId("UIUC_Mobile");

    # -------------------------------------------------
    # Get the header lat/lon data
	# string :location = "43.605 degrees north, 116.214 degrees west" ;
	# string :start_alt_amsl = "820 meters" ;
	# lines 34 and 37 (counting 0 to n-1)
    # -------------------------------------------------
	print "LAT/LON LINE: $headerlines[34]";
    my (@latlon_info) = split(" ",$headerlines[34]); 

	# all files in western hemisphere (negative lon)
	my $lon = trim($latlon_info[6]);
	$lon = "-".$lon;
	# remove the quotation mark
	my $lat = trim($latlon_info[3]);
	$lat =~ s/"//g;
	
	print "ALT LINE: $headerlines[37]";
    my (@alt_info) = split(" ",$headerlines[37]); 
	
	my $alt = trim($alt_info[3]);
	# remove the quotation mark
	$alt =~ s/"//g;
	
	print "LAT: $lat LON: $lon ALT: $alt\n";
    $header->setLatitude($lat, $self->buildLatLonFormat($lat));
	$header->setLongitude($lon, $self->buildLatLonFormat($lon)); 
    $header->setAltitude($alt,"m");

    my $sondeType = "GRAW DFM-09";
    $header->setLine(5,"Radiosonde Type:", $sondeType);
    $header->setLine(6,"Ground Station Software:", "Version 5.10.12.3");

	# $header->setVariableParameter(2,"SRng","m ");


    # ----------------------------------------------------------
    # Extract date, time, and location info from the file name
	# Expects filename similar to:         
	# 2017-01-08-0100z-resinn.txt
    # ----------------------------------------------------------
    # print "file name = $filename\n"; 
	my $date;
	my $time;
	my $facility_id;
	my $facility;
	my $release_month;

    if ($filename =~ /(\d{4})-(\d{2})-(\d{2})-(\d{2})(\d{2})z-(\w{6,9}).txt/)
	{
		my ($year, $month, $day, $hour, $min) = ($1,$2,$3,$4,$5);
	    $date = join ", ", $year, $month, $day;
	    $time = join ":", $hour,$min,'00';
		$facility_id = $6;
		$release_month = $month;

        $header->setActualRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
        $header->setNominalRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
	}
    if ($facility_id =~ /resinn/)
    {
        $facility = "Residence Inn Downtown, Boise, ID";
    }
    elsif ($facility_id =~ /ipcgarage/)
    {
        $facility = "Idaho Power Company Garage, Boise, ID";
    }
    elsif ($facility_id =~ /caldwell/)
    {
        $facility = "Caldwell, ID";
    }
	# ----------------------------------------------------------------
	#  The soundings from 4-9 March have an incorrect location						
	#  in the file name (the global attributes are correct)
	#  These are actually ipcgarage.
	# ----------------------------------------------------------------
    if (($facility =~ /^Residence/) && ($release_month =~ /03/))
	{
		$facility = "Idaho Power Company Garage, Boise, ID";
		print "Changed Residence Inn to Idaho Power\n";
	}

	$header->setSite($facility);

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
	my @headerlines = @lines[0..40];
	# print "HEADER: $headerlines[30]\n";
	my $header = $self->parseHeader($file,@headerlines);
    
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

   	$outfile = sprintf("%s_%04d%02d%02d%02d%02d.cls", 
					   	   $header->getId(),
					   	   split(/,/,$header->getActualDate()),
					   	   $hour, $min);
 
    printf("\tOutput file name is %s\n", $outfile);


	open(my $OUT,">".$self->{"OUTPUT_DIR"}."/".$outfile)
	    or die("Can't open output file for $file\n");

	print($OUT $header->toString());
	

    # ----------------------------------------------------
    # Parse the data portion of the input file
    # ----------------------------------------------------
	my $startData = 0;
	my $surfaceRecord = 0;

    # ---------------------------------------------
	# Needed for code to derive geopotential height
	# ---------------------------------------------
	my $previous_record;
	my $geopotential_height;                

    # ----------------------------------------
    # Needed for code to derive ascension rate
    # ----------------------------------------
    my $prev_time = 9999.0;
    my $prev_alt = 99999.0;   

	# --------------------------------------------
    # Create an array to hold all of the data records.
	# This is required so additional processing can take
    # place to remove descending data records at the
	# end of the data files NOTE: Not used for UIUC
	# --------------------------------------------
	my @record_list = ();
	# --------------------------------------------


    # Now grab the data from each line
	foreach my $line (@lines) 
	{
        # Skip any blank lines.
		next if ($line =~ /^\s*$/);
        
		chomp($line);
	    my @data = split(' ',$line);
		# identify the last header line
		# to determine where the data starts        
		if (trim($data[0]) =~ /Time/i)
		{
			$startData = 1;
			$surfaceRecord = 1;
			next;
		}
        
		if ($startData)
		{
			$data[0] = trim($data[0]); # time
		    $data[1] = trim($data[1]); # pressure
		    $data[2] = trim($data[2]); # Height AGL - DO NOT USE -Derive geopotential height
		    $data[3] = trim($data[3]); # temp
		    $data[4] = trim($data[4]); # relative humidity (RH)
		    $data[5] = trim($data[5]); # wind speed
		    $data[6] = trim($data[6]); # wind direction

	    	my $record = ClassRecord->new($WARN,$file);

        	# missing values were unknown, used "//"
			$record->setTime($data[0]);
		    $record->setPressure($data[1],"mb") if ($data[1] !~ /\/\//);
		    $record->setTemperature($data[3],"C") if ($data[3] !~ /\/\//);   
		    $record->setRelativeHumidity($data[4]) if ($data[4] !~ /\/\//);

			$record->setWindSpeed($data[5],"m/s") if ($data[5] !~ /\/\//);
			$record->setWindDirection($data[6]) if ($data[6] !~ /\/\//);

            # Insert Ele (Elevation Angle) and Azi (Azimuth Angle) data
			# # For setVariableValue(index, value):
			# # index (1) is Ele column, index (2) is Azi column.
			# Variable 1 is Elevation angle
			# Variable 2 is Slant Range instead of azimuth angle
			# See header->setVariableParameter to change column header
			# $record->setVariableValue(1, $data[11]) unless ($data[11] =~ /\/+/);
			# $record->setVariableValue(2, $data[10]) unless ($data[10] =~ /\/\//);


			if ($surfaceRecord)
			{
				my $surfaceLat = $header->getLatitude();
				my $surfaceLon = $header->getLongitude();
				my $surfaceAlt = $header->getAltitude();
				# print ("SURFACE VALUES: $surfaceLat  $surfaceLon  $surfaceAlt\n");
	            $record->setLatitude($surfaceLat, $self->buildLatLonFormat($surfaceLat));
	            $record->setLongitude($surfaceLon, $self->buildLatLonFormat($surfaceLon));
			    $record->setAltitude($surfaceAlt,"m");

			}

            #-------------------------------------------------------------
			# Calculate geopotential height (this code from VORTEX2 SUNY
            # and VORTEX-SE NOAA_ATDD_Mobile_Radiosonde_Converter.pl)
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
            if (! $surfaceRecord)
			{
	            if ($debug) 
    	        { 
					my $prev_press = $previous_record->getPressure(); 
            		my $prev_temp = $previous_record->getTemperature(); 
                	my $prev_alt = $previous_record->getAltitude();
	
    	            print "\nCalc Geopotential Height from previous press = $prev_press, temp = $prev_temp, alt = $prev_alt,\n";
					print "and current press = $data[1] and temp = $data[3]\n"; 
            	}
	            if ($previous_record->getPressure() < 9990.0)
    	        {
        	        if ($debug){ print "prev_press < 9990.0 - NOT missing so calculate the geopotential height.\n"; }
 
            	    $geopotential_height = calculateAltitude($previous_record->getPressure(),
                	                                         $previous_record->getTemperature(), 
															 undef, $previous_record->getAltitude(), 
															 $data[1], $data[3], undef, 1);
	                if (defined($geopotential_height))
					{
						if ($debug_geopotential_height) 
						{ 
							print "GPH: $geopotential_height \n"; 
						}
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
            	    if ($debug_geopotential_height)
					{
						print "WARNING: prev_press > 9990.0 - MISSING! Set geopot alt to missing.\n"; 
					}
	                $geopotential_height = 99999.0;
    	        }
            }
        	#-------------------------------------------------------
	        # Calculate the ascension rate which is the difference
    	    # in altitudes divided by the change in time. Ascension
        	# rates can be positive, zero, or negative. But the time
	        # must always be increasing (the norm) and not missing.
    	    #-------------------------------------------------------
        	if ($debug) 
			{ 
				my $time = $record->getTime(); 
				my $alt = $record->getAltitude(); 
            	print "\nprev_time: $prev_time, current Time: $time, prev_alt: $prev_alt, current Alt: $alt\n"; 	
			}

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
			# push(@record_list, $record);
            
			
			$surfaceRecord = 0;


		} # end if ($startData)
	} # end foreach $line
	}


    # --------------------------------------------------
	# Remove the last records in the file that are 
    # descending (ascent rate is negative)
	# --------------------------------------------------
#	foreach my $last_record (reverse(@record_list))
#	{
#		# if (($last_record->getPressure() == 9999.0) && 
#		# 	($last_record->getAltitude() == 99999.0))
#        if (($last_record->getAscensionRate() < 0.0) ||
#		    ($last_record->getAscensionRate() == 999.0))
#	    {
#            # ALL OUR ASCENT RATES ARE 999
#		    undef($last_record);
#	    } 
#	    else 
#	    {
#		    last;
#	    }
#	}
    #-------------------------------------------------------------
    # Print the records to the file.
	#-------------------------------------------------------------
#	foreach my $rec(@record_list) 
#	{
#	    print ($OUT $rec->toString()) if (defined($rec));
#	}	
#
#	} # end if (defined($header))
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
    my @files = grep(/^2017.+\.txt$/,sort(readdir($RAW)));
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
