#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The BAE146_Converter script is used for converting BAe-146 dropsonde data
# in the ESC variant format into the EOL Sounding Composite (ESC) format.</p>
#
# @author Joel Clawson
# @version 1.0 This conversion was originally created for T-REX.  It was adapted
# from the HIAPER conversion script.
#
# @author L. Cully
# @version Updated May 2008 by L. Cully to use latest sounding libraries.
#     Added some informational output statements.
# BEWARE: This s/w assumes the raw input data (*.cls) in /raw_data directories.
##Module------------------------------------------------------------------------
package BAE146_Converter;
use strict;

if (-e "/net/work/") {
    use lib "/net/work/lib/perl/Utilities";
    use lib "/net/work/lib/perl/Station";
    use lib "/net/work/lib/perl/UpperAir";
} else {
    use lib "/work/lib/perl/Utilities";
    use lib "/work/lib/perl/Station";
    use lib "/work/lib/perl/UpperAir";
}

use ClassConstants qw(:DEFAULT);
use ClassHeader;
use ClassRecord;

use SimpleStationMap;
use Station;

printf "\nBAE_146_Converter began on ";print scalar localtime;printf "\n";
&main();
printf "\nBAE_146_Converter ended on ";print scalar localtime;printf "\n";

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the BAE-146 dropsonde data by converting it from the native ASCII
# format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = BAE146_Converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature BAE146_Converter new()
# <p>Create a new instance of a BAE146_Converter.</p>
#
# @output $self A new BAE146_Converter object.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);
    
    $self->{"PROJECT"} = "T-REX";
    $self->{"NETWORK"} = "BAE146";

    $self->{"FINAL_DIR"} = "../final";
    $self->{"OUTPUT_DIR"} = "../output";
    $self->{"RAW_DIR"} = "../raw_data";

    $self->{"STATION_FILE"} = sprintf("%s/%s_%s_sounding_stationCD.out",$self->{"FINAL_DIR"},
                                      $self->clean_for_file_name($self->{"NETWORK"}),
                                      $self->clean_for_file_name($self->{"PROJECT"}));
    $self->{"WARN_LOG"} = $self->{"OUTPUT_DIR"}."/warning.log";

    $self->{"stations"} = SimpleStationMap->new();

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

    my $station = Station->new($station_id,$network);
    $station->setStationName($network);

    $station->setLatLongAccuracy(3);
    $station->setStateCode("CA");
    $station->setReportingFrequency("no set schedule");
    $station->setNetworkIdNumber(99);
    $station->setPlatformIdNumber(349);
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
# @signature String build_latlong_format(String value)
# <p>Generate the decimal format for the specified value.</p>
#
# @input $value The value of the lat/lon being formatted.
# @output $fmt The format that corresponds the the value.
##------------------------------------------------------------------------------
sub build_latlon_format {
    my ($self,$value) = @_;

    my $fmt = $value < 0 ? "-" : "";
    while (length($fmt) < length($value)) { $fmt .= "D"; }
    return $fmt;
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

    my ($projet,$flight) = split(/,/,(split(/:/,$lines[1]))[1]);
    my ($platform,$id) = split(/,/,(split(/:/,$lines[2]))[1]);
    $header->setType(sprintf("%s/%s, %s Dropsonde",trim($flight),trim($platform),trim($id)));
    $header->setProject($self->{"PROJECT"});
    $header->setReleaseDirection("Descending");
    $header->setId(trim($id));
    $header->setSite(sprintf("%s, %s %s",trim($platform),trim($id),trim($flight)));

    # Parse the location of the dropsonde.
    (split(/:/,$lines[3]))[1] =~ /\d+\s+[\d\.]+\'[WE],\s+\d+\s+[\d\.]+\'[NS],\s+(\-?[\d\.]+),?\s+(\-?[\d\.]+),?\s+([\d\.]+)/i;
    my ($lon,$lat,$alt) = ($1,$2,$3);
    $header->setLatitude($lat,$self->build_latlon_format($lat));
    $header->setLongitude($lon,$self->build_latlon_format($lon));
    $header->setAltitude($alt,"m");


    # Parse the release time from the header.
    $lines[4] =~ /(\d{4}, \d{2}, \d{2}), (\d{2}:\d{2}:\d{2})/;
    my ($date,$time) = ($1,$2);
    $header->setActualRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
    $header->setNominalRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);

    # Add all non-predefined header lines to the header.
    for (my $i = 5; $i < 11; $i++) {
        if ($lines[$i] !~ /^\s*\/\s*$/) {
            my ($label,@data) = split(/:/,$lines[$i]);
	    my $dataline = trim(join(":",@data));
	    unless ($dataline eq "" || $dataline eq ",") {
		$header->setLine($i, trim($label).":",$dataline);  ## LEC - added i
	    }
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
                                                           $self->clean_for_file_name($header->getId()),
                                                           split(/,/,$header->getActualDate()),
                                                           split(/:/,$header->getActualTime())))
            or die("Can't open output file for $file\n");

        print($OUT $header->toString());

        # Read in the records
        my $index = @lines;
        foreach my $line (reverse(@lines)) {
            $index--;
            # Ignore the header lines.
            if ($index < 15) { last; }

            my @data = split(' ',$line);
	    
	    my $record = ClassRecord->new($self->{"WARN"},$file);
	    $record->setTime($data[0]);
	    $record->setPressure($data[1],"mb") unless ($data[1] == 9999.0);
	    $record->setTemperature($data[2],"C") unless ($data[2] == 999.0);
	    $record->setDewPoint($data[3],"C") unless ($data[3] == 999.0);
	    $record->setRelativeHumidity($data[4]) unless ($data[4] == 999.0);
	    $record->setUWindComponent($data[5],"m/s") unless ($data[5] == 999.0);
	    $record->setVWindComponent($data[6],"m/s") unless ($data[6] == 999.0);
	    $record->setWindSpeed($data[7],"m/s") unless ($data[7] == 999.0);
	    $record->setWindDirection($data[8]) unless ($data[8] == 999.0);
	    $record->setAscensionRate($data[9],"m/s") unless ($data[9] == 99.0);
	    unless ($data[10] == 999.000) {
		$record->setLongitude($data[10],$self->build_latlon_format($data[10]));
	    }
	    unless ($data[11] == 999.000) {
		$record->setLatitude($data[11],$self->build_latlon_format($data[11]));
	    }
	    $record->setAltitude($data[12],"m") unless ($data[12] == 99999.0);

	    print($OUT $record->toString());
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
    my @files = grep(/^D\d{8}_\d{6}.+\.cls$/,sort(readdir($RAW)));
    closedir($RAW);


    open(my $WARN,">".$self->{"WARN_LOG"}) or die("Can't create ".$self->{"WARN_LOG"});

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
