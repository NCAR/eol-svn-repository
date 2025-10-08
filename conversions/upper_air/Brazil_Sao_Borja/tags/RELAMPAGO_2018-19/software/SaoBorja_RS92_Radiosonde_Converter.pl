#! /usr/bin/perl -w
##Module------------------------------------------------------------------------
# <p>The SaoBorja_RS92_Radiosonde_Converter.pl script is used for converting
# high resolution radiosonde data from ASCII formatted data to the EOL Sounding 
# Composite (ESC) format.</p> 
#
#
# @author Linda Echo-Hawk August 2019
# @version RELAMPAGO INPE Sounding RS92 Radiosonde
#          - The converter expects filenames in the following
#            format: YYYYMMDD_HHmm.TXT (e.g. 20181102_1200.TXT)
#            where YYYY=year, MM=month, DD=day, HH=hour, and mm=minute.
#          - Not all files contain header info. For those files, use values
#            such as "Unknown" for Sonde Serial Number and get release time
#            from the file name. Scot provided release location lat/lon
#            values for files without header information. Ground Station
#            Software is "Unknown" for all files.
#          - Data Type: INPE Sounding Data/Ascending
#          - Release Site Type/Site ID: Sao Borja, Brazil
#          - Radiosonde Type: Vaisala RS92.
#   	   - Some (one) raw data files ended mid-record, so a check was added to 
#            confirm that all values in each record were complete before writing.
#          - Scot made some manual changes were made to the raw data files. See the IVEN
#            notes for details. I also made some changes to two files, adding in "///"
#            for missing values (formerly blank columns). A copy of the original
#            files is located at /net/work/Projects/RELAMPAGO/data_processing/
#            upper_air/Brazil_Sao_Borja/RS92/raw_data/originals.
#
#
#
#
##Module------------------------------------------------------------------------
package SaoBorja_RS92_Radiosonde_Converter;
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

printf "\nSaoBorja_RS92_Radiosonde_Converter.pl began on ";print scalar localtime;printf "\n";
my $debug = 0;

&main();
printf "\nSaoBorja_RS92_Radiosonde_Converter.pl ended on ";print scalar localtime;printf "\n";

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the Sao Borja RS92 radiosonde data by converting it from 
# the native ASCII format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = SaoBorja_RS92_Radiosonde_Converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature SaoBorja_RS92_Radiosonde_Converter new()
# <p>Create a new instance of a SaoBorja_RS92_Radiosonde_Converter.</p>
#
# @output $self A new SaoBorja_RS92_Radiosonde_Converter object.
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
    $self->{"NETWORK"} = "Sao_Borja_RS92";
    
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
    # platform, 944	Radiosonde, Vaisala RS92
    $station->setPlatformIdNumber(944);
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
    $header->setId("Sao_Borja_RS92");
	# "Release Site Type/Site ID:" header line
    $header->setSite("Sao Borja, Brazil");


    # ----------------------------------------------------------
    # Set default values, then read through the file and
	# change those that have actual header information
	#
    # ----------------------------------------------------------
    # Extract the date and time information from the file name
	# Expects filename similar to 20181102_1200.TXT
    # ----------------------------------------------------------
    my $date;
	my $time;
	my $hour;
	my $minute;

	if ($file =~ /(\d{4})(\d{2})(\d{2})_(\d{2})(\d{2})/)
	{
		my ($year, $month, $day, $hour, $minute) = ($1,$2,$3,$4,$5);
	    $date = join ", ", $year, $month, $day;
	    $time = join ":", $hour,$minute,'00';
        
	}

    # ------------------------------------------------
    # Read through the file for additional header info
    # ------------------------------------------------
	my $lat = -28.64;
	my $lon = -55.99;
	my $alt = 80;
	

	my $sonde_id_label = "Radiosonde Serial Number";
	$header->setLine(5, trim($sonde_id_label).":", "Unknown");

	foreach my $line (@headerlines) 
	{
	    if ($line =~ /RS-number/i)
	    {
			chomp ($line);
		    my (@sonde_id) = split(' ',$line);
            #-----------------------------------------------------------
            # "RS-number:  G1613249"
            #-----------------------------------------------------------
			$sonde_id_label = "Radiosonde Serial Number";
	        $header->setLine(5, trim($sonde_id_label).":",trim($sonde_id[1]));
	    }
	    
        
		if ($line =~ /Started at:/)
		{
			chomp ($line);
			my @release_values = split(' ', $line);
			# my ($rel_day, $rel_month, $rel_yr, $rel_time) = split(' ', $release_values[3]);
			# my $rel_day = $release_values[2];
			my $rel_day = sprintf("%02d", $release_values[2]);
            my $rel_month = $release_values[3];
			my $rel_yr = $release_values[4];
			if ($rel_month =~ /NOV/i)
			{
				$rel_month = 11;
			}
			else
			{
				$rel_month = 12;
			}
			if ($rel_yr =~ /18/)
			{
				$rel_yr = 2018;
			}
			else
			{
				print "Unknown year found in 'Started at' line\n";
			}

			$date = join ", ", $rel_yr, $rel_month, $rel_day;

			my @time_values = split(":", $release_values[5]);
			$hour = sprintf("%02d", $time_values[0]);
			$minute = $time_values[1];
            $time = join ":", $hour,$minute,'00';
            
            #-----------------------------------------------------------
			# "Started at:       3 NOV 18 12:03 UTC"
            #-----------------------------------------------------------
			
		}

		if ($line =~ /Location/)
		{
			chomp ($line);
			my @loc_values = split(' ', $line);
            $lat = "-".($loc_values[2]);
			$lon = "-".($loc_values[4]);
			$alt = $loc_values[6];

            #-----------------------------------------------------------
		    # Location : 28.64 S  55.98 W     80 m
		    #-----------------------------------------------------------
		}

	}

    $header->setLongitude($lon,$self->buildLatlonFormat($lon));
    $header->setLatitude($lat,$self->buildLatlonFormat($lat));
    $header->setAltitude($alt,"m");

    $header->setActualRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
	$header->setNominalRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);

	# provided by Scot
	my $sonde_type_label = "Radiosonde Type";
	my $sonde_type = "Vaisala RS92";
	$header->setLine(6, trim($sonde_type_label).":",$sonde_type);

	my $version_label = "Ground Station Software";
	my $version = "Unknown"; 
	$header->setLine(7, trim($version_label).":",$version);


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
	my @headerlines = @lines[0..20];
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
	my $record;
	
	foreach my $line (@lines) {
	    # Ignore the header lines and
		# check for blank last line
	    if ($line =~ /^\s*$/) { next; }
	    

		chomp ($line);

	    my @data = split(' ',$line);

	    if (($data[11]) && (($data[0] =~ /^0$/) && ($data[1] =~ /^0$/)))
		{
			$start_data = 1;
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
		    # geopot alt  $data[3]       wind spd    $data[9]
		    # press       $data[4]       PotTemp     $data[10]
		    # temp        $data[5]       MixR        $data[11]
		    # -------------------------------------------------
		
		    # ----------------------------------------------
		    # set the time from the minutes/seconds columns
            # ----------------------------------------------
		    $current_time = ($data[0]*60) + $data[1];
	        $record->setTime($current_time);

	        if ($data[4])
			{
				$record->setPressure($data[4],"mb") if ($data[4] !~ /\/\//);
			}
			if ($data[5])
			{
				$record->setTemperature(($data[5]),"C") if ($data[5] !~ /\/\//);    
			}
			if ($data[7])
			{
				$record->setDewPoint(($data[7]),"C") if ($data[7] !~ /\/\//);
			}
			if ($data[6])
			{
				$record->setRelativeHumidity($data[6]) if ($data[6] !~ /\/\//);
			}
			if ($data[9])
			{
				$record->setWindSpeed($data[9],"m/s") if ($data[9] !~ /\/\//);
			}
			if ($data[8])
			{
				$record->setWindDirection($data[8]) if ($data[8] !~ /\/\//);
			}

            # Insert Ele (Elevation Angle) and Azi (Azimuth Angle) data
			# MixR  PTmp instead of ele, azi for Sao Borja RS92
			# For setVariableValue(index, value)
			# index (1) is Ele column, index (2) is Azi column.
			if ($data[11])
			{
				$record->setVariableValue(1, $data[11]) if ($data[11] !~ /\/\//);
			}
			if ($data[10])
			{
				$record->setVariableValue(2, $data[10]) if ($data[10] !~ /\/\//);
			}

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
			    if ($data[3])
				{
					$record->setAltitude($data[3],"m") if ($data[3] !~ /\/\//);
				}
				if ($data[2])
				{
					$record->setAscensionRate($data[2],"m/s") if ($data[2] !~ /\/\//);
				}
		    } # end if !$surfaceRecord
		} # end if $start_data

		# Some raw data files ended mid-record, so this checks to see
		# that all values were complete in the record before writing it out
	    if ($data[11])
		{
			printf($OUT $record->toString());
		}
		else
		{
			print "Don't print partial last record\n";
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
    my @files = grep(/TXT$/,sort(readdir($RAW)));
    closedir($RAW);
    
    
    open($WARN,">".$self->{"WARN_LOG"}) or die("Can't create ".$self->{"WARN_LOG"});
    
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

