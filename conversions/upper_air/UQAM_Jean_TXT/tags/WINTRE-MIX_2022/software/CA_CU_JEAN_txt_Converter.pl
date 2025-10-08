#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The CA_UQAM_JEAN_txt_Converter script is used for converting Canadian
# UQAM Jean site radiosonde data into the EOL Sounding Composite (ESC)
# format(ESC).</p>
#
# INPUT: UQAM_JEAN_txt raw data in the following format/order. See sample
#   raw data line below. Here are the expected parameters in order along with
#   definitions.
#
# FltTime: The elapsed time in seconds since the sounding was launched
# Press: The pressure measured by the radiosonde in hPa
# Temp: The temperature measured by the radiosonde in degrees Celsius
# RelHum: The relative humidity measured by the radiosonde in %
# WSpeed: The wind speed measured by the radiosonde in m sâ€“1
# WDirn: The wind direction measured by the radiosonde in degrees (0 degrees
# corresponds to a northerly wind).
# 13
# UTC_Date: The Month/Day/Year at which the sounding was launched in UTC
# UTC_Time: The time at which the sounding was launched in UTC. For Trois-RiviÃ¨res, the time
# in UTC is given in AM or PM, rather than in a military time format.
# Long/E: The longitudinal position of the sounding
# Lat/N: The latitudinal position of the sounding
# GPM_MSL: The geopotential meters above mean sea level
#
# Raw data line: Note that the spaces/tabs shown in the next line are not accurate.
# The strange character is the degree symbol.
#
# 0.0 1018.95 +5.10 58.30 2.0 0 2022-03-15 00:11:16 073â15'58.5"W 45â19'28.8"N 37.0
#
# OUTPUT: Sounding data files in ESC format.
#
# Assumptions/Warnings:
#      - Search for "HARD-CODED" to change project related constants.
#
#
#
# @author  Daniel Choi 21 September 2022
# @version WINTRE-MIX 2002
#
##Module------------------------------------------------------------------------
package CA_UQAM_JEAN_txt_Converter;
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
use ElevatedStationMap;
use Station;
use ClassConstants qw(:DEFAULT);
use ClassHeader;
use ClassRecord;
use DpgCalculations;
use DpgConversions;

my ($WARN);


printf "\nCA_UQAM_JEAN_txt_Converter.pl began on ";print scalar localtime;printf "\n\n";  
&main();
printf "\nCA_UQAM_JEAN_txt_Converter.pl ended on ";print scalar localtime;printf "\n"; 
##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the ISS radiosonde data by converting it from the native ASCII
# format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = CA_UQAM_JEAN_txt_Converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature CA_UQAM_Sorel_Converter new()
# <p>Create a new instance of a CA_UQAM_Sorel_Converter.</p>
#
# @output $self A new CA_UQAM_Sorel_Converter object.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);
    
    $self->{"stations"} = ElevatedStationMap->new();

    # HARD-CODED
    $self->{"PROJECT"} = "WINTRE-MIX_2022";
    # HARD-CODED
    $self->{"NETWORK"} = "CA_UQAM_JEAN";
    
    $self->{"FINAL_DIR"} = "../final";
    $self->{"OUTPUT_DIR"} = "../output_esc";
    $self->{"RAW_DIR"} = "../cleaned_raw_files";
    
    $self->{"STATION_FILE"} = sprintf("%s/%s_%s_sounding_stationCD.out",$self->{"FINAL_DIR"},
				      $self->clean_for_file_name($self->{"NETWORK"}),
				      $self->clean_for_file_name($self->{"PROJECT"}));
    $self->{"WARN_LOG"} = $self->{"OUTPUT_DIR"}."/warning.log";
    
    return $self;
}

##------------------------------------------------------------------------------
# @signature Station build_default_station(String station_id, String network)
# <p>Create a default station for the ISS network using the specified 
# station_id and network.</p>
#
# @input $station_id The identifier of the station to be created.
# @input $network The network the station belongs to.
# @return The new station object with the default values for the network.
##------------------------------------------------------------------------------
sub build_default_station {
    my ($self,$station_id,$network) = @_;
    my $station = Station->new($station_id,$network);
    $station->setStationName($network);
    $station->setLatLongAccuracy(3);
    # HARD-CODED
    $station->setStateCode("99");
    $station->setReportingFrequency("no set schedule");
    $station->setNetworkIdNumber(99);
    $station->setPlatformIdNumber(121);

    return $station;
}

##------------------------------------------------------------------------------
# @signature String build_latlong_format(String value)
# <p>Generate the decimal format for the specified value.</p>
#
# @input $value The value of the lat/lon being formatted.
# @output $fmt The format that corresponds the the value.
##------------------------------------------------------------------------------
sub build_latlong_format {
    my ($self,$value) = @_;
    
    my $fmt = $value < 0 ? "-" : "";
    while (length($fmt) < length($value)) { $fmt .= "D"; }
    return $fmt;
}

##-------------------------------------------------------------------------
# @signature String clean_for_file_name(String text)
# <p>Remove/translate characters in a String so it can be used in a file name.</p>
#
# @input $text The String to be cleaned.
# @output $text The cleaned up String.
##-------------------------------------------------------------------------
sub clean_for_file_name {
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
    
    $self->read_data_files();
    $self->print_station_files();
}

##------------------------------------------------------------------------------
# @signature ClassHeader parse_header(String file, String[] lines)
# <p>Parse the header lines from the specified file into the ESC header format.</p>
#
# @input $file The name of the file being parsed.
# @input $lines[] The header lines to be parsed.
# @output $header The header data in ESC format.
##------------------------------------------------------------------------------
sub parse_header {
    my ($self,$file,@lines) = @_;
    my $header = ClassHeader->new();

    $header->setReleaseDirection(" Ascending");
    $header->setType("UQAM Jean Sounding Data ");   # HARDCODED   - Get from doc for 612.001
    $header->setProject($self->{"PROJECT"});
    $header->setId("CA_UQAM_JEAN");   # HARDCODED
    $header->setSite("St Jean-sur-Richelieu, QC");   # HARDCODED
  
    my $lat = 45.324;
    my $lon = -73.266;
    my $alt = 37;  # Should be in meters (not feet), get from doc 612.001

    $header->setLatitude($lat,$self->build_latlong_format($lat));
    $header->setLongitude($lon,$self->build_latlong_format($lon));
    $header->setAltitude($alt,"m");
    
    # Split filename by "."

    my @datetime = split('\.', $file);
 
    # Get date and time from file name.   
 
    my $date = substr($datetime[2], 0, 4) . ", " . substr($datetime[2], 4, 2) . ", " . substr($datetime[2], 6, 2);
    my $time = substr($datetime[2], 8, 2) . ":" . substr($datetime[2], 10, 2) . ":00";
    $header->setActualRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
    $header->setNominalRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);

    return $header;
} #  End parse_header()

##------------------------------------------------------------------------------
# @signature void parse_raw_files(String file)
# <p>Read the data from the specified file and convert it to the ESC format.</p>
#
# @input $file The name of the file to be parsed.
##------------------------------------------------------------------------------
sub parse_raw_file {
    my ($self,$file) = @_;
 
    printf("parse_raw_file(): Processing file: %s\n",$file);
 
    open(my $FILE,$self->{"RAW_DIR"}."/".$file) or die("Can't open file: ".$file);
    my @lines = <$FILE>;
    close($FILE);
    
# Generate the sounding header.
    my $header = $self->parse_header($file,@lines[0..2]);


    # Only continue processing the file if a header was created.
    if (defined($header)) {

        #$header->setLine(5,"Radiosonde Type/RH Sensor Type:", "RS41-SG Radiosondes");
        #$header->setLine(8,"Balloon Manufacturer/Type:", "Kaymont 200-g Sounding Balloons");
        #$header->setLine(6,"Radiosonde Frequency:", "403MHz");
        #$header->setLine(6,"Ground Station Software:", "iMet-3050A iMetOS-II software version 3.133.0");
        $header->setLine(6,"Radiosonde Frequency:", "403 MHz"); # HERE Correct?
        $header->setLine(7,"Radiosonde Type/RH Sensor Type:", "iMet-4 radiosonde/thin-film capacitive polymer");
        $header->setLine(8,"Ground Station Software:", "iMet-3050A iMetOS-II software version 3.133.0");

        # Determine the station the sounding was released from.
	my $station = $self->{"stations"}->getStation($header->getId(),$self->{"NETWORK"},
						      $header->getLatitude(),$header->getLongitude(),
						      $header->getAltitude());
	if (!defined($station)) {
	    $station = $self->build_default_station($header->getId(),$self->{"NETWORK"});
	    $station->setLatitude($header->getLatitude(),$self->build_latlong_format($header->getLatitude()));
	    $station->setLongitude($header->getLongitude(),$self->build_latlong_format($header->getLongitude()));
	    $station->setElevation($header->getAltitude(),"m");
	    $self->{"stations"}->addStation($station);
	}
	$station->insertDate($header->getNominalDate(),"YYYY, MM, DD");
	
	my $outfile = sprintf("%s_%04d%02d%02d%02d%02d%02d.cls",
							   $header->getId(),
							   split(/,/,$header->getActualDate()),
							   split(/:/,$header->getActualTime()));


	open(my $OUT,">".$self->{"OUTPUT_DIR"}."/".$outfile)
 	    or die("Can't open output file for $file\n");   
        print ("\tOUTPUT FILE $outfile\n\n");

	
	print($OUT $header->toString());
	
	
	my $index = 0;
        my $asc_ind = 0;
        my @previous;
	foreach my $line (@lines) {
	    # Ignore the header lines.
	    if ($index < 3) { $index++; next; }
	    
	    my @data = split(' ',$line);
	    my $record = ClassRecord->new($WARN,$file);
	    $record->setTime($data[0]);
	    $record->setPressure($data[1],"mb") if ($data[1] != -999);
	    $record->setTemperature($data[2],"C") if ($data[2] != -999);
	    $record->setRelativeHumidity($data[3]) if ($data[3] != -999);
	    $record->setWindSpeed($data[4],"m/s") if ($data[4] != -999);
	    $record->setWindDirection($data[5]) if ($data[5] != -999);
           
            # Set ascension rate
 
            if ($asc_ind == 0) {
	        $record->setAscensionRate(999.0,"m/s") if ($data[0] != 999.0);
                @previous = @data;
            }

            else {
                if ($data[0]-$previous[0] <= 0) {
                    $record->setAscensionRate(999.0, "m/s") if ($data[0] != 999.0);
                    print "WARNING: Zero or negative time difference found in AscensionRate calculation! \n";
                }


                else {
                    $record->setAscensionRate(($data[10]-$previous[10])/($data[0]-$previous[0]), "m/s") if ($data[0] != 999.0);
                }
            }

            # Set UV wind

            my ($uwind, $vwind) =  calculateUVfromWind($data[4], $data[5]);

            $record->setUWindComponent($uwind,"m/s");
            $record->setVWindComponent($vwind,"m/s");

            # Set dewpoint temp

            my $dewpt = calculateDewPoint($data[2], $data[3]);
            $record->setDewPoint($dewpt,"C");
            
            # Set lat/lon

            my $longDecDeg;
            my $latDecDeg; 
            
            my $newlong = $data[8];
            $newlong =~ s/\D+//g;
            $longDecDeg = substr($newlong, 3, 2)/60 + substr($newlong, 5, 2)/3600 + substr($newlong, 7, 1)/36000 + substr($newlong, 0, 3);

            my $newlat = $data[9];
            $newlat =~ s/\D+//g;
            $latDecDeg = substr($newlat, 2, 2)/60 + substr($newlat, 4, 2)/3600 + substr($newlat, 6, 1)/36000 + substr($newlat, 0, 2);
            
            # If lon has W or if lat has S, set value to be negative

            if (index($data[8], "W") > 0) {
                $longDecDeg = -$longDecDeg;
            }


            if (index($data[9], "S") > 0) {
                $latDecDeg = -$latDecDeg;
            }

	    if ($longDecDeg != -999) {
		my $lon_fmt = $longDecDeg < 0 ? "-" : "";
		while (length($lon_fmt) < length($longDecDeg)) { $lon_fmt .= "D"; }
		$record->setLongitude($longDecDeg,$lon_fmt);
	    }
	    if ($latDecDeg != -999) {
		my $lat_fmt = $latDecDeg < 0 ? "-" : "";
		while (length($lat_fmt) < length($latDecDeg)) { $lat_fmt .= "D"; }
		$record->setLatitude($latDecDeg,$lat_fmt);
	    }

            # Set altitude
 
	    $record->setAltitude($data[10],"m") if ($data[10] != -999);
	 
            # Save info from current loop for ascension rate for next loop
   
            @previous = @data;
            $asc_ind++;
	    
	    printf($OUT $record->toString());
	}
    } 
}  # parse_raw_file()
#
##------------------------------------------------------------------------------
# @signature void print_station_files()
# <p>Generate the output files containing station information.</p>
##------------------------------------------------------------------------------
sub print_station_files {
    my ($self) = @_;

    open(my $STN, ">".$self->{"STATION_FILE"}) || 
	die("Cannot create the ".$self->{"STATION_FILE"}." file\n");
    foreach my $station ($self->{"stations"}->getAllStations()) {
        print($STN $station->toString()) if ($station->getBeginDate !~ /^9+$/);
    }
    close($STN);
}

##------------------------------------------------------------------------------
# @signature void read_data_files()
# <p>Read in the files from the raw data directory and convert each into an
# ESC formatted file.</p>
##------------------------------------------------------------------------------
sub read_data_files {
    my ($self) = @_;
    
 
    opendir(my $RAW,$self->{"RAW_DIR"}) or die("Can't read raw directory ".$self->{"RAW_DIR"});
    
    # Pattern match to find desired files

    my @files = grep(/^upperair.sounding/ ,sort(readdir($RAW)));

    closedir($RAW);
    
    
    open($WARN,">".$self->{"WARN_LOG"}) or die("Can't create ".$self->{"WARN_LOG"});
    


    foreach my $file (@files) {
       print "Processing file: $file\n";

       $self->parse_raw_file($file);    
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
