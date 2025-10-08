#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The Ranai_Radiosonde_Converter.pl script is used for converting high
# resolution radiosonde data from ASCII formatted data to the EOL Sounding 
# Composite (ESC) format.</p> 
#
# @author Linda Echo-Hawk
# @version DYNAMO 2011-12 for Ranai
#    This code was created by modifying the Sipora converter.
#          - Header lat/lon/alt info is obtained from the data.  
#          - Release time is obtained from the file name.
#          - Search for "HARD-CODED" to find project-specific items that
#            may require changing.
#          - The raw data consists of *.cor files (data) and *.ref files 
#            (header information) but because there was not a corresponding
#            REF file for each COR file, and because we only needed two 
#            lines from the REF file, I used the ReadTwoWriteOne.pl
#            script to combine the two files, so the code will look
#            for raw data files in the form: MOddmmhh.combo where 
#            MO = MODEM sonde, dd = day, mm = month, and hh = hour.
#            So the raw data file name only supplied month day and hour
#            release information.
#          - The raw data "time" column (column 0) contained 6 digits
#            that represented UTC release time in seconds.  Scot asked
#            that we determine that minutes and seconds from this number.
#            Some files had beginning numbers larger than 86400 
#            (>24 hours) so we subtracted 86400 from the first time
#            and used the remainder.
#          - The perl Date::Calc library function Add_Delta_DHMS was 
#            used to convert the local release time to UTC time.
#          - The raw data file contains header info on lines 1-3. 
#            Actual data starts on line 4. 
#          - Some files end with incomplete lines or lines that 
#            contained only "999999" in col. 0 so checks were put
#            in to detect and ignore these lines.
#          - Many of the files had times that were "999999" and Scot
#            indicated that these appeared while the sondes were
#            falling, so code was added to remove this line and all
#            that came after.
#          - Added code to remove descending sonde data lines.
#          
##Module------------------------------------------------------------------------
package Ranai_Radiosonde_Converter;
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
use ElevatedStationMap;
use Station;
use ClassConstants qw(:DEFAULT);
use ClassHeader;
use ClassRecord;
use Date::Calc qw(Add_Delta_DHMS);

my ($WARN);
my $debug_conversion = 0;
my $debug_date = 0;

printf "\nRanai_Radiosonde_Converter.pl began on ";print scalar localtime;printf "\n";
my $debug = 0;
&main();
printf "\nRanai_Radiosonde_Converter.pl ended on ";print scalar localtime;printf "\n";

my $filename;

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the Ranai radiosonde data by converting it from 
# the native ASCII format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = Ranai_Radiosonde_Converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature Ranai_Radiosonde_Converter new()
# <p>Create a new instance of a Ranai_Radoisonde_Converter.</p>
#
# @output $self A new Ranai_Radiosonde_Converter object.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);
    
    $self->{"stations"} = ElevatedStationMap->new();

    # HARD-CODED
    $self->{"PROJECT"} = "DYNAMO";
    # HARD-CODED
    $self->{"NETWORK"} = "Ranai";
    
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
	$station->setCountry("Indonesia");
    # $station->setStateCode("48");
    $station->setReportingFrequency("12 hours");
    $station->setNetworkIdNumber("99");
    # platform, 441, Radiosonde, MODEM
    $station->setPlatformIdNumber(441);
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

    $filename = $file;
	# printf("parsing header for %s\n",$filename);
    $header->setReleaseDirection("Ascending");

    # Set the type of sounding
    $header->setType("BMKG Radiosonde");
    $header->setProject($self->{"PROJECT"});
    
    # HARD-CODED
	# The Id will be the prefix of the output filea
	# and appears in the stationCD.out file
    $header->setId("Ranai");
	# "Release Site Type/Site ID:" header line
    $header->setSite("Ranai, Indonesia/96147");

    # -------------------------------------------------
    # Get the header lat/lon data
    # -------------------------------------------------
    my $lat = 3.9122;
	my $lon = 108.393183;
	my $alt = 1.0;

    $header->setLatitude($lat,$self->buildLatlonFormat($lat));
	$header->setLongitude($lon,$self->buildLatlonFormat($lon)); 
    $header->setAltitude($alt,"m");


	# ------------------------------------------------------------
    # File name shows LOCAL RELEASE TIME
	# ------------------------------------------------------------
	# MO010107.cor and MO010107.ref
	# MO - modem sonde
    # 010107 - 01 day of month; 01 month; 07 local release hour
    # local time is UTC + 7 hours
	# The DYNAMO time period was approx Sept 2011 to Mar 31, 2012
	# ------------------------------------------------------------
	
    my $orig_year;
	my $orig_month;
	my $orig_day;
	my $orig_hour; 
	
	my $fileDate;
	my $fileTime;
	
	if ($filename =~ /(\d{2})(\d{2})(\d{2})/)
	{
		$orig_month = $2;
		$orig_day = $1;
		$orig_hour = $3;

		if (($orig_month =~ /01/) || ($orig_month =~ /02/) ||
			($orig_month =~ /03/))
		{
			$orig_year = 2012;
		}
		else
		{
			$orig_year = 2011;
		}
		
		$fileDate = sprintf("%04d, %02d, %02d", $orig_year, $orig_month, $orig_day);
        $fileTime = sprintf("%02d:00:00", $orig_hour);
	    print "LOCAL RELEASE FROM FILE NAME:  $fileDate    $fileTime\n";
	}

	# ---------------------------------------------
	# The first "Temps" value (col. 1) in the data
	# is the UTC release time in seconds.  Use this
	# to find the release minutes and second
	# ---------------------------------------------
	my $surface_data = $headerlines[3];

    chomp($surface_data);
	my @release = split(';',$surface_data);

	my $releaseTime = $release[0];
	if ($debug_date)
	{
		print "UTC RELEASE SECONDS = $releaseTime \n";
	}
	if ($releaseTime > 86400)
	{
		$releaseTime -= 86400;
	}
	if ($debug_date)
	{
		print "CORRECTED UTC RELEASE SECONDS = $releaseTime  ";
	}
    
    my $rel_hours = int ($releaseTime / 3600);
	$releaseTime = $releaseTime % 3600;

	my $rel_minutes = int ($releaseTime / 60);
	$releaseTime = $releaseTime % 60;

	my $rel_seconds = $releaseTime;

    if ($debug_date)
	{
		print "IN HH:MM:SS: $rel_hours : $rel_minutes : $rel_seconds\n";
	}
    
	my $days_offset = 0;
	my $hour_offset = -7;
	my $minute_offset = 0;
	my $second_offset = 0;
	my $orig_min = $rel_minutes;
	my $orig_sec = $rel_seconds;

    my $new_year  = 0; my $new_month = 0; my $new_day   = 0; 
    my $new_hour  = 0; my $new_min   = 0; my $new_sec   = 0;

    if ($debug_date)
	{
		print "INPUT DATE:  $orig_year  $orig_month  $orig_day  $orig_hour  $orig_min  $orig_sec\n";
	}

	# ---------------------------------------------
    # Convert the original local release time
	# to UTC time
	# ---------------------------------------------
    ($new_year, $new_month, $new_day, $new_hour, $new_min, $new_sec) = 
        Add_Delta_DHMS( $orig_year, $orig_month, $orig_day, 
		$orig_hour, $orig_min, $orig_sec, $days_offset, 
		$hour_offset, $minute_offset, $second_offset );

    if ($debug_date) 
	{
		print "From Add_Delta_DHMS():: \n";
		print "\tNEW UTC RELEASE: $new_year, $new_month, $new_day, ";
		print "$new_hour, $new_min, $new_sec\n"; 
	}

    my $newDate = sprintf("%04d%02d%02d%02d%02d", $new_year, $new_month, 
	    $new_day, $new_hour, $new_min);
	if ($debug_date)
	{
		print "\tNEW DATE: $newDate\n";
	}

	my $relDate = sprintf("%04d, %02d, %02d", $new_year, $new_month, $new_day);
    my $relTime = sprintf("%02d:%02d:%02d", $new_hour, $new_min, $new_sec);
	print "FINAL UTC RELEASE:  $relDate    $relTime\n";

    $header->setActualRelease($relDate,"YYYY, MM, DD",$relTime,"HH:MM:SS",0);
    $header->setNominalRelease($relDate,"YYYY, MM, DD",$relTime,"HH:MM:SS",0);


    # ------------------------------------------------
    # Read through the array for additional header info
    # ------------------------------------------------
	foreach my $line (@headerlines) 
	{
	    # Sonde Id/Sonde Type:   105 2 10004/MODEM M2K2DC
        # Add the non-predefined header line to the header.
		# In raw data: "NA Identification Sonde=105 2 10004"
		if ($line =~ /Identification Sonde/i)
		{
		    chomp ($line);
            my ($label,@contents) = split('=', trim($line));
            $label = "Sonde Id/Sonde Type";
            # $contents[0] contains sonde ID number
			$contents[1] = "MODEM M2K2DC";
			trim($contents[1]);
		    $header->setLine(5, $label.":",trim(join("/",@contents))); 
		}
        # Add the software version header line
		# In raw data: "Version logiciel station=V7.5.0.0 B7.12"
		if ($line =~ /Version logiciel/i)
		{
		  	chomp $line;
            my @values = split('=', trim($line));

            #-------------------------------------------------------------
            # "Ground Station Software Version:  V7.5.0.0 V7.12"
            #-------------------------------------------------------------
            $header->setLine(6,"Ground Station Software Version: ", $values[1]);
		}
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
	my @headerlines = @lines[0..3];
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

	# --------------------------------------------
    # Create an array to hold all of the data records.
	# This is required so additional processing can take
    # place to remove descending data records at the
	# end of the data files
	# --------------------------------------------
	my @record_list = ();
	# --------------------------------------------

    # ----------------------------------------
    # Needed for code to derive ascension rate
    # ----------------------------------------
    my $prev_time = 9999.0;
    my $prev_alt = 99999.0;

    # ----------------------------------------------------
    # Parse the data portion of the input file
    # ----------------------------------------------------
	my $surface_record = 1;
	my $init_time = 999;
	my $time = 0;
	my $index = 0;
	foreach my $line (@lines) 
	{
	    # Ignore the header lines.
	    if ($index < 3) { $index++; next; }

		# --------------------------------------------------------------
	    # Some files end with a line that contains only "999999"
		# and other files have a data line with time = "999999"
		# which usually occurs while the sonde is falling.
		# Remove this line and all that follow per Scot's instructions.
		# --------------------------------------------------------------
		if ($line =~ /999999/)
		{
			last;
		}
		
		chomp($line);
	    my @data = split(';',$line);
	    my $record = ClassRecord->new($WARN,$file);
              
		if ($surface_record)
		{
            $init_time = $data[0];
			$record->setTime($time);
			$surface_record = 0;
		}
		else
		{
			$time = $data[0] - $init_time;
			$record->setTime($time);
		}

        $record->setAltitude($data[1],"m") unless($data[1] =~ /\/+/);
        
		# --------------------------------------------------
		# Some files have incomplete final data lines.
		# If there is not a complete final data line, 
		# we need to skip that line altogether, so set
		# the $incomplete_line flag.
		# --------------------------------------------------
        my $incomplete_line = 0;
		if ($data[15])
		{
			$record->setPressure($data[15],"hPa") unless($data[15] =~ /\/+/);
		}
		 else
		{
			$incomplete_line = 1;
		}
		if ($data[12])
		{
			$record->setTemperature($data[12],"C") unless($data[12] =~ /\/+/);
		}
		else
		{
			$incomplete_line = 1;
		}
		if ($data[14])
		{
			$record->setRelativeHumidity($data[14]) unless($data[14] =~ /\/+/);
		}
		else
        {
			$incomplete_line = 1;
		}


	    $record->setUWindComponent($data[4],"m/s") unless($data[4] =~ /\/+/);
	    $record->setVWindComponent($data[5],"m/s") unless($data[5] =~ /\/+/);
	    $record->setWindSpeed($data[7],"m/s") unless($data[7] =~ /\/+/);
	    $record->setWindDirection($data[8]) unless($data[8] =~ /\/+/);

        #-------------------------------------------------
	    # Convert the lat/lon data from radians to degrees 
		# Values have + in front: +0.0683800
        #-------------------------------------------------
		my $lon;
		my $lat;
		
		if ($data[3] !~ /\/+/)
		{
			$lon = $data[3];
			$lon =~ s/\+//g;
			$lon = convertAngle($lon,"rad","deg");
			$record->setLongitude($lon,$self->buildLatlonFormat($lon));
		}
		if ($data[2] !~ /\/+/)
		{
			$lat = $data[2];
			$lat =~ s/\+//g;
			$lat = convertAngle($lat,"rad","deg");
			$record->setLatitude($lat,$self->buildLatlonFormat($lat));
		}
        
		if ($debug_conversion)
		{
			print "LON: $data[3] == $lon   LAT: $data[2] == $lat at $time\n";
		}
        
        #-------------------------------------------------------
        # this code from Ron Brown converter:
        # Calculate the ascension rate which is the difference
        # in altitudes divided by the change in time. Ascension
        # rates can be positive, zero, or negative. But the time
        # must always be increasing (the norm) and not missing.
        #
        # Only save the next non-missing values.
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
		# 
		# Add the record to the @record_list 
		# (rather than toString) in case it ends up being
		# an incomplete record and needs to be deleted
        #-------------------------------------------------------
        if (!$incomplete_line)
		{
			# printf($OUT $record->toString());
			push(@record_list, $record);
		}
    } # end foreach $line


    # --------------------------------------------------
	# Remove the last records in the file that are 
    # descending (ascent rate is negative)
	# --------------------------------------------------
	foreach my $last_record (reverse(@record_list))
	{
	    if (($last_record->getAscensionRate() < 0.0) ||
		    ($last_record->getAscensionRate() == 999.0))
	    {
		    undef($last_record);
	    } 
	    else 
	    {
		    last;
	    }
	}
    #-------------------------------------------------------------
    # Print the records to the file.
	#-------------------------------------------------------------
	foreach my $rec(@record_list) 
	{
	    print ($OUT $rec->toString()) if (defined($rec));
	}	

	} # end if (defined($header)
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
    my @files = grep(/^MO(\d{6})\.combo/,sort(readdir($RAW)));
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
