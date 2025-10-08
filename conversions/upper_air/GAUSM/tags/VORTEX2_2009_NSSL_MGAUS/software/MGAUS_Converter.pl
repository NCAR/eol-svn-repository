#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The MGAUS_Converter script is used for converting Mobile GAUS radiosonde data
# in the EOL sounding format into the EOL Sounding Composite (ESC) format.</p>
#
# @author Linda Echo-Hawk
# @version VORTEX2 2009 Updated for the NSSL MGAUS and NCAR MGAUS Sounding Data
#           - Search on "HARD-CODED" to find project specific information
#             that may need to be updated.
#  BEWARE:  This s/w assumes the raw data will be in /raw_data/NSSL*/ dirs.
#
# @author L. Cully
# @version Updated May 2008 by L. Cully to use latest sounding libraries 
# (i.e., setLine fn, etc.) and added some documentation like the BEWARE below.
# Fixed bug in line split fed to setLine fn where splits on colon. Failed
# because incoming data included colons, so split was not proper.
#
# BEWARE: This s/w assumes the raw data will be in /raw_data/mobileX/ directories.
#
# @author Joel Clawson
# @version T-REX_2006 This was originally created for the T-REX project.  It
# was adapted from the GAUS Converter that was used for RICO. 
#
#
#
##Module------------------------------------------------------------------------
package MGAUS_Converter;
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
use ClassConstants qw(:DEFAULT);
use ClassHeader;
use ClassRecord;

use SimpleStationMap;
use Station;

my ($WARN);

printf "\nMGAUS_Converter.pl began on ";print scalar localtime;
&main();
printf "\nMGAUS_Converter.pl ended on ";print scalar localtime;printf "\n";

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the MGAUS radiosonde data by converting it from the native ASCII
# format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = MGAUS_Converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature ISS_Converter new()
# <p>Create a new instance of a ISS_Converter.</p>
#
# @output $self A new ISS_Converter object.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);
    
    $self->{"stations"} = SimpleStationMap->new();

    # HARD-CODED
    $self->{"PROJECT"} = "VORTEX2";
    $self->{"NETWORK"} = "MGAUS";
    
    $self->{"FINAL_DIR"} = "../final";
    $self->{"OUTPUT_DIR"} = "../output";
    $self->{"RAW_DIR"} = "../raw_data";
    
    $self->{"STATION_FILE"} = sprintf("%s/NSSL_%s_%s_sounding_stationCD.out",$self->{"FINAL_DIR"},
				      $self->clean_for_file_name($self->{"NETWORK"}),
				      $self->clean_for_file_name($self->{"PROJECT"}));
    $self->{"WARN_LOG"} = $self->{"OUTPUT_DIR"}."/warning.log";
    
    return $self;
}

##------------------------------------------------------------------------------
# @signature Station build_default_station(String station_id, String network)
# <p>Create a default station for the MGAUS network using the specified 
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
	# multiple states
    $station->setStateCode("99");
    $station->setReportingFrequency("no set schedule");
    $station->setNetworkIdNumber(99); 
    # platform is Rawinsonde, GAUS
    $station->setPlatformIdNumber(298);
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
    my ($self,$dir,$file,@lines) = @_;
    my $header = ClassHeader->new();

    # Parse the release direction from the header lines.
    my (undef(),$direction) = split(/\//,(split(/:/,$lines[0]))[1]);
    $header->setReleaseDirection(trim($direction)); 

    # Parse the type of sounding from the header lines.
    my ($project,$type) = split(/\//,(split(/:/,$lines[2]))[1]);
    $header->setType(trim($type));
    $header->setProject($self->{"PROJECT"});

    # Parse the site from the header lines.
    # RAW HDR 3: "Launch Site:            20090513" 
    # OUTPUT LINE:  "Release Site Type/Site ID:        20090513"
	my $site;
    my ($siteType) = (split(/:/,$lines[3]))[1];
	$siteType = trim($siteType); # may only contain white space
    # some files were missing Launch Site info
	if ($siteType)
	{
		$site =  join "/",$siteType, $dir;
	}
	else
	{
        $site = $dir;
	}
	# HARD-CODED project information
    $dir =~ /NSSL(\d+)/;
 	# The Id will be the prefix of the output file           
    $header->setId(sprintf("NSSL_MGAUS_%02d",$1));
    # "Release Site Type/Site ID:" header line  
    $header->setSite($site);
    
    # Parse the release location from the header lines.
    my (undef(),undef(),$lon,undef(),undef(),$lat,$alt) =
	split(' ',(split(/:/,$lines[4]))[1]);
    $lat = substr($lat,0,length($lat)-1);
    $lon = substr($lon,0,length($lon)-1);
    
    my $lat_fmt = $lat < 0 ? "-" : "";
    while (length($lat_fmt) < length($lat)) { $lat_fmt .= "D"; }
    $header->setLatitude($lat,$lat_fmt);
    
    my $lon_fmt = $lon < 0 ? "-" : "";
    while (length($lon_fmt) < length($lon)) { $lon_fmt .= "D"; }
    $header->setLongitude($lon,$lon_fmt);
    
    $header->setAltitude($alt,"m");
    
    $lines[5] =~ /(\d{4}, \d{2}, \d{2}), (\d{2}:\d{2}:\d{2})/;
    my ($date,$time) = ($1,$2);
    $header->setActualRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
    $header->setNominalRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
    
    # Add all of the non-predefined header information to the header.
    for (my $i = 6; $i < 10; $i++) {

	if ($lines[$i] !~ /^\s*\/$/) 
	{
		# my $label = "\n";

	    my ($label,$data) = split(/:/,$lines[$i]);

        if ($label eq "Reference Launch Data Source/Time")
		{
			my @line_split = split(/:/,$lines[$i]);
			$data = $line_split[1].":".$line_split[2].":".$line_split[3];
		}
           
	    $header->setLine($i-1, trim($label).":",trim($data));
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
    my ($self,$dir, $file) = @_;
    
    printf("\nProcessing file: %s/%s\n", $dir, $file);
    
    open(my $FILE,$self->{"RAW_DIR"}."/".$dir."/".$file) or die("Can't open file: ".$file);
    my @lines = <$FILE>;
    close($FILE);

    # Generate the ESC header
    my $header = $self->parse_header($dir,$file,@lines[0..13]);

    # Only process the sounding if a header was generated.
    if (defined($header)) {
	# Determine the station where the sounding was released.
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
	
	
	my $index = 0;
	foreach my $line (@lines) {
	    # Ignore the header lines.
	    if ($index < 14) { $index++; next; }
	    
	    my @data = split(' ',$line);
	    my $record = ClassRecord->new($WARN,$file);
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
    my @dirs = grep(/NSSL\d$/,sort(readdir($RAW)));
    closedir($RAW);

    open($WARN,">".$self->{"WARN_LOG"}) or die("Can't create ".$self->{"WARN_LOG"});

    foreach my $dir (@dirs) {
        opendir($RAW,sprintf("%s/%s",$self->{"RAW_DIR"},$dir)) or die("Can't read raw directory: ".$self->{"RAW_DIR"}."/".$dir);
        my @files = grep(/^D\d{8}_\d{6}.+\.eol/,sort(readdir($RAW)));
        closedir($RAW);
    
    
        foreach my $file (sort(@files)) { 
	    $self->parse_raw_file($dir, $file);
        }
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
    $line =~ s/^\s+//;
    $line =~ s/\s+$//;
    return $line;
}
