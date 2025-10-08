#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The HIAPER_Converter script is used for converting HIAPER dropsonde data
# in the EOL sounding format into the EOL Sounding Composite (ESC) format.</p>
#
# @author Joel Clawson
# @version 1.0 This conversion was originally created for T-REX.  It was adapted
# from the ISS EOL to ESC conversion script.
##Module------------------------------------------------------------------------
package HIAPER_Converter;
use strict;
if (-e "/net/work/") {
  use lib "/net/work/software/TREX/library/conversion_modules/Version6";
} else {
  use lib "/work/software/TREX/library/conversion_modules/Version6";
}

use Station::SimpleStationMap;
use Station::Station;
use Sounding::ClassConstants qw(:DEFAULT);
use Sounding::ClassHeader;
use Sounding::ClassRecord;

my ($WARN);

&main();

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the HIAPER dropsonde data by converting it from the native ASCII
# format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = HIAPER_Converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature HIAPER_Converter new()
# <p>Create a new instance of a HIAPER_Converter.</p>
#
# @output $self A new HIAPER_Converter object.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);
    
    $self->{"PROJECT"} = "T-REX";
    $self->{"NETWORK"} = "HIAPER";
    
    $self->{"FINAL_DIR"} = "../final";
    $self->{"OUTPUT_DIR"} = "../output";
    $self->{"RAW_DIR"} = "../raw_data";
    
    $self->{"STATION_FILE"} = sprintf("%s/%s_%s_sounding_stationCD.out",$self->{"FINAL_DIR"},
				      $self->clean_for_file_name($self->{"NETWORK"}),
				      $self->clean_for_file_name($self->{"PROJECT"}));
    $self->{"WARN_LOG"} = $self->{"OUTPUT_DIR"}."/warning.log";
    
    $self->{"stations"} = Station::SimpleStationMap->new();
    
    return $self;
}

##------------------------------------------------------------------------------
# @signature Station build_default_station(String station_id, String network)
# <p>Create a default station for the HIAPER network using the specified 
# station_id and network.</p>
#
# @input $station_id The identifier of the station to be created.
# @input $network The network the station belongs to.
# @return The new station object with the default values for the network.
##------------------------------------------------------------------------------
sub build_default_station {
    my ($self,$station_id,$network) = @_;
    
    my $station = Station::Station->new($station_id,$network);
    $station->setStationName($network);

    $station->setStateCode("CA");
    $station->setReportingFrequency("no set schedule");
    $station->setNetworkIdNumber(99);
    $station->setPlatformIdNumber(350);
    $station->setMobilityFlag("m");

    return $station;
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
    my $header = Sounding::ClassHeader->new();

    # Parse off the release direction of the sounding.
    my (undef(),$direction) = split(/\//,(split(/:/,$lines[0]))[1]);
    $header->setReleaseDirection(trim($direction));

    # Parse the general flight information
    my ($project,$flight,$platform,$id) = split(/[\/,]/,(split(/:/,$lines[2]))[1]);
    $flight = trim($flight);
    $id = trim($id);
    $platform = trim($platform);
    $header->setType(sprintf("%s/%s, %s Dropsonde",$flight,$platform,$id));
    $header->setProject($self->{"PROJECT"});
    $header->setId($id);
    $header->setSite(sprintf("%s, %s %s",$platform,$id,$flight));

    # Parse the location of the dropsonde.
    (split(/:/,$lines[4]))[1] =~ /\d+\s+[\d\.]+\'[WE],*\s+(\-?[\d\.]+)(\s+deg)?,\s+\d+\s+[\d\.]+\'[NS],*\s+(\-?[\d\.]+),?\s*(deg,*)?\s+([\d\.]+)/i;
    my ($lon,$lat,$alt) = ($1,$3,$5);

    my $lat_fmt = $lat < 0 ? "-" : "";
    while (length($lat_fmt) < length($lat)) { $lat_fmt .= "D"; }
    $header->setLatitude($lat,$lat_fmt);
    
    my $lon_fmt = $lon < 0 ? "-" : "";
    while (length($lon_fmt) < length($lon)) { $lon_fmt .= "D"; }
    $header->setLongitude($lon,$lon_fmt);
    
    $header->setAltitude($alt,"m");
    
    # Parse the date of the drop.
    $lines[5] =~ /(\d{4}, \d{2}, \d{2}), (\d{2}:\d{2}:\d{2})/;
    my ($date,$time) = ($1,$2);
    $header->setActualRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
    $header->setNominalRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);

    # Add all of the non-predefined header information to the header.
    for (my $i = 6; $i < 11; $i++) {
	if ($lines[$i] !~ /^\s*\/\s*$/) {
	    my ($label,@data) = split(/:/,$lines[$i]);
	    $header->setLine(trim($label).":",trim(join(":",@data)));
	}
    }
    
    return $header;
}

##------------------------------------------------------------------------------
# @signature void parse_raw_files(String file)
# <p>Read the data from the specified file and convert it to the ESC format.</p>
#
# @input $file The name of the file to be parsed.
##------------------------------------------------------------------------------
sub parse_raw_file {
    my ($self,$file) = @_;
    
    printf("Processing file: %s\n",$file);
    
    open(my $FILE,$self->{"RAW_DIR"}."/".$file) or die("Can't open file: ".$file);
    my @lines = <$FILE>;
    close($FILE);

    # Generated the header for the file.
    my $header = $self->parse_header($file,@lines[0..13]);
    
    # Only process the file if a header was generated.
    if (defined($header)) {

	# Determine the station that generated the sounding.
	my $station = $self->{"stations"}->getStation($header->getId(),$self->{"NETWORK"});
	if (!defined($station)) {
	    $station = $self->build_default_station($header->getId(),$self->{"NETWORK"});
	    $self->{"stations"}->addStation($station);
	}
	$station->insertDate($header->getNominalDate(),"YYYY, MM, DD");
	
	open(my $OUT,">".$self->{"OUTPUT_DIR"}."/".sprintf("%s_%04d%02d%02d%02d%02d%02d.cls",
							   $header->getId(),
							   split(/,/,$header->getActualDate()),
							   split(/:/,$header->getActualTime())))
	    or die("Can't open output file for $file\n");
	
	print($OUT $header->toString());
	
	# Read in the records
	my $index = @lines;
	foreach my $line (reverse(@lines)) {
            $index--;
	    # Ignore the header lines.
	    if ($index < 14) { last; }
	    

	    my @data = split(' ',$line);
	    my $record = Sounding::ClassRecord->new($WARN,$file);
	    $record->setTime($data[0]);
	    $record->setPressure($data[4],"mb") if ($data[4] != -999);
	    $record->setTemperature($data[5],"C") if ($data[5] != -999);
	    $record->setDewPoint($data[6],"C") if ($data[6] != -999);
	    $record->setRelativeHumidity($data[7]) if ($data[7] != -999);
	    $record->setUWindComponent($data[8],"m/s") if ($data[8] != -999);
	    $record->setVWindComponent($data[9],"m/s") if ($data[9] != -999);
	    $record->setWindSpeed($data[10],"m/s") if ($data[10] != -999);
	    $record->setWindDirection($data[11]) if ($data[11] != -999);
	    $record->setAscensionRate($data[12],"m/s") if ($data[12] != -999);
	    
	    if ($data[14] != -999) {
		my $lon_fmt = $data[14] < 0 ? "-" : "";
		while (length($lon_fmt) < length($data[14])) { $lon_fmt .= "D"; }
		$record->setLongitude($data[14],$lon_fmt);
	    }
	    if ($data[15] != -999) {
		my $lat_fmt = $data[15] < 0 ? "-" : "";
		while (length($lat_fmt) < length($data[15])) { $lat_fmt .= "D"; }
		$record->setLatitude($data[15],$lat_fmt);
	    }
	    $record->setAltitude($data[13],"m") if ($data[13] != -999);
	    
	    
	    printf($OUT $record->toString());
	}
    }
}

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
    my @files = grep(/^D\d{8}_\d{6}.+\.eol/,sort(readdir($RAW)));
    closedir($RAW);
    
    
    open($WARN,">".$self->{"WARN_LOG"}) or die("Can't create ".$self->{"WARN_LOG"});
    
    foreach my $file (@files) {
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
