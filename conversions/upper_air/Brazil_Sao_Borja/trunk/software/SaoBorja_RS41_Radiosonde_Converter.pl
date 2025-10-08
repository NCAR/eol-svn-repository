#! /usr/bin/perl -w
##Module------------------------------------------------------------------------
# <p>The SaoBorja_RS41_Radiosonde_Converter.pl script is used for converting
# high resolution radiosonde data from ASCII formatted data to the EOL Sounding 
# Composite (ESC) format.</p> 
#
#
# @author Linda Echo-Hawk
# @version RELAMPAGO INPE Sounding RS41 Radiosonde
#          - The converter expects filenames in the following
#            format: SBSB_YYYYMMDD_HHmm.txt (e.g. SBSB_20181101_1147.txt)
#            where YYYY=year, MM=month, DD=day, HH=hour, and mm=minute.
#          - The file contains header info on lines 1-23. Actual data starts 
#            on line 24. 
#          - Data Type: INPE Sounding Data/Ascending
#          - Release Site Type/Site ID: Sao Borja, Brazil
#          - Release Location (lon,lat,alt): this is the same for all soundings, 
#            could either be hard coded: 28.642719S 55.988880W 92m or grabbed 
#            from the following header records: "Release point latitude", 
#            "Release point longitude", and "Release point height from sea level" 
#            in the header information
#          - UTC Release Time: Use "Balloon release date" and "Balloon release time".
#          - Radiosonde Serial Number:  Use the "Sonde serial number" in the 
#            header information.
#          - Radiosonde Type: Use the "Sonde type" in the header information.
#          - Ground Station Software: Use the "Sonde software version" in the 
#            header information but with MW41 in front (e.g. MW41 2.2.11).
#   	   - Two raw data files ended mid-record, so a check was added to 
#            confirm that all values in each record were complete.
#
#
#
##Module------------------------------------------------------------------------
package SaoBorja_RS41_Radiosonde_Converter;
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

printf "\nSaoBorja_RS41_Radiosonde_Converter.pl began on ";print scalar localtime;printf "\n";
my $debug = 0;

&main();
printf "\nSaoBorja_RS41_Radiosonde_Converter.pl ended on ";print scalar localtime;printf "\n";

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the Sao Borja RS41 radiosonde data by converting it from 
# the native ASCII format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = SaoBorja_RS41_Radiosonde_Converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature SaoBorja_RS41_Radiosonde_Converter new()
# <p>Create a new instance of a SaoBorja_RS41_Radiosonde_Converter.</p>
#
# @output $self A new SaoBorja_RS41_Radiosonde_Converter object.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);
    
    $self->{"stations"} = ElevatedStationMap->new();

    # HARD-CODED
    $self->{"PROJECT"} = "RELAMPAGO";
    # HARD-CODED
    $self->{"NETWORK"} = "Sao_Borja_RS41";
    
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
# <p>Create a default station for the Sao Borja network using the 
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
    # platform, 1179, Vaisala RS41-SGP
    $station->setPlatformIdNumber(1179);
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

    # Set the type of sounding "Data Type:" header line
    $header->setType("INPE Sounding Data");
    $header->setReleaseDirection("Ascending");

    $header->setProject($self->{"PROJECT"});
    
    # HARD-CODED
	# The Id will be the prefix of the output file
    $header->setId("Sao_Borja_RS41");
	# "Release Site Type/Site ID:" header line
    $header->setSite("Sao Borja, Brazil");

    # ------------------------------------------------
    # Read through the file for additional header info
    # ------------------------------------------------
	my $date;
	my $time;

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
	        $header->setLine(6, trim($sonde_type_label).":",trim($sonde_type[2]));
	    }

	    if ($line =~ /Sonde serial number/i)
	    {
			chomp ($line);
		    my (@sonde_id) = split(' ',$line);
            #-----------------------------------------------------------
            # "Sonde serial number                             P3410036"
            #-----------------------------------------------------------
			my $sonde_id_label = "Radiosonde Serial Number";
	        $header->setLine(5, trim($sonde_id_label).":",trim($sonde_id[3]));
	    }
	    
		if ($line =~ /Sonde software version/i)
	    {
			chomp ($line);
		    my (@sw_version) = split(' ',$line);
            #-----------------------------------------------------------
            # "Sonde software version                            2.2.14"
            #-----------------------------------------------------------
			my $version_label = "Ground Station Software";
			my $version = "MW41 " . trim($sw_version[3]); 
	        $header->setLine(7, trim($version_label).":",trim($version));
	    }
        
		if ($line =~ /Balloon release date/)
		{
			chomp ($line);
			my @date_values = split(' ', $line);
			my ($rel_day, $rel_month, $rel_yr) = split(/\//, $date_values[3]);
			$rel_yr = "20" . $rel_yr;
			$date = join ", ", $rel_yr, $rel_month, $rel_day;

            #-----------------------------------------------------------
			# Balloon release date                            01/11/18
			# where date is DDMMYY (day, month, year)
            #-----------------------------------------------------------
		}
		
		if ($line =~ /Balloon release time/)
		{
			chomp ($line);
			my @time_values = split(' ', trim($line));
			my @hms_vals = split(":", $time_values[3]);
            # hour = $hms_vals[0];
            # minute = $hms_vals[1];
            # second = $hms_vals[2];
            $time = join ":", $hms_vals[0], $hms_vals[1], $hms_vals[2];

            #-----------------------------------------------------------
			# Balloon release time                            11:47:40
            #-----------------------------------------------------------
		}

        # 28.642719S 55.988880W 92m (values from Scot L.)
		# NOTE: These were reversed and the header and surface lat/lon
		# had to be corrected on the *.qc files in the /final directory.
		# If this script is re-used, be sure to fix these values here.
		my $lon = -28.642719;
		my $lat = -55.988880;
		my $alt = 92;
        $header->setLongitude($lon,$self->buildLatlonFormat($lon));
        $header->setLatitude($lat,$self->buildLatlonFormat($lat));
        $header->setAltitude($alt,"m");
	}

    $header->setActualRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
	$header->setNominalRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);

    # column headings for the variable parameters (Ele and Azi columns)
	$header->setVariableParameter(1, "MixR", "g/kg");
	$header->setVariableParameter(2, "PTmp", "K ");

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
	my @headerlines = @lines[0..18];
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
	
	# ----------------------------------------------------
    # Parse the data portion of the input file
    # ----------------------------------------------------
	my $surfaceRecord = 1;
    my $current_time = 0;
	my $start_data = 0;
	my $index = 0;
	my $record;
	
	foreach my $line (@lines) {
	    # Ignore the header lines and
		# check for blank last line
	    if (($index < 20) || ($line =~ /^\s*$/)) { $index++; next; }
	    
	    my @data = split(' ',$line);
		if ($data[0] =~ /min/)
		{
			$start_data = 1;
			next;
		}

		if (!$start_data)
		{
			next;
		}
		else
		{
	        $record = ClassRecord->new($WARN,$file);
		    # -------------------------------------------------
		    # minutes     $data[0]       RH          $data[6]
		    # seconds     $data[1]       dewpt       $data[7]
		    # ascent      $data[2]       wind dir    $data[8]
		    # geoept alt  $data[3]       wind spd    $data[9]
		    # press       $data[4]       PotTemp     $data[10]
		    # temp        $data[5]       MixR        $data[11]
		    # -------------------------------------------------
		
		    # ----------------------------------------------
		    # set the time from the minutes/seconds columns
            # ----------------------------------------------
		    $current_time = ($data[0]*60) + $data[1];
	        $record->setTime($current_time);

	        $record->setPressure($data[4],"mb") if ($data[4] !~ /-32768/);
	        $record->setTemperature(($data[5]),"C") if ($data[5] !~ /-32768/);    
		    $record->setDewPoint(($data[7]),"C") if ($data[7] !~ /-32768/);
	        $record->setRelativeHumidity($data[6]) if ($data[6] !~ /-32768/);
	        $record->setWindSpeed($data[9],"m/s") if ($data[9] !~ /\/\//);
	        $record->setWindDirection($data[8]) if ($data[8] !~ /\/\//);

            # Insert Ele (Elevation Angle) and Azi (Azimuth Angle) data
			# For setVariableValue(index, value)
			# index (1) is Ele column, index (2) is Azi column.
			$record->setVariableValue(1, $data[11]) if ($data[11] !~ /-32768/);
			$record->setVariableValue(2, $data[10]) if ($data[10] !~ /-32768/);

		    if ($surfaceRecord)
		    {
                my $surfaceLat = $header->getLatitude();
			    my $surfaceLon = $header->getLongitude();
			    my $surfaceAlt = $header->getAltitude();
			    # print ("SURFACE VALUES: $surfaceLat  $surfaceLon  $surfaceAlt\n");
			    $record->setLatitude($surfaceLat, $self->buildLatlonFormat($surfaceLat));
			    $record->setLongitude($surfaceLon, $self->buildLatlonFormat($surfaceLon));
			    $record->setAltitude($surfaceAlt,"m") if ($surfaceAlt !~ /\/\//);

				# Ascension rate not set for surface record
			    
				$surfaceRecord = 0;
            }
		    else  # if (!$surfaceRecord)
		    {
			    $record->setAltitude($data[3],"m") if ($data[3] !~ /-32768/);
		        $record->setAscensionRate($data[2],"m/s") if ($data[2] !~ /-32768/);
		    } # end if !$surfaceRecord
		} # end if $start_data

		# Two raw data files ended mid-record, so this checks to see
		# that all values were complete
	    if ($data[11])
		{
			printf($OUT $record->toString());
		}
	}
	}# foreach my $line
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
    my @files = grep(/^SBSB.+\.txt$/,sort(readdir($RAW)));
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

