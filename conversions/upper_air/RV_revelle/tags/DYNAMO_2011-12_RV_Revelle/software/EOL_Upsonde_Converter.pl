#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The EOL_Upsonde_Converter.pl script is used for converting radiosonde data
# in the EOL sounding format into the EOL Sounding Composite (ESC) format.</p> 
#
# @author Linda Echo-Hawk 9 Nov 2012
# @version DYNAMO_2011-12  Modified for the R/V Revelle soundings
#          - Changed hard-coded values for R/V Revelle
#          - Code expects raw data files named *.eol.
#          - Run dos2unix on raw data files.
#
# @author Linda Echo-Hawk 2012-09-13
# @version DYNAMO_2011  Created for the Diego Garcia sounding processing by
#          modifying the GAUS_Converter.pl script
#          - Code was added to remove the last lines of a file if there were no 
#            valid altitude or pressure data (descending sondes).
#
#  The info below pertains to the GAUS_Converter.pl script
#
# @author Linda Echo-Hawk 2008-12-18
# @version VOCALS_2008  Updated based on the ISS_Converter.pl for PLOWS_2008-2009.
#          This was used for the Iquique_GAUS Radiosonde Data and the Olaya_GAUS 
#          Radiosonde Data.
#          - A change was made to pass the NON-truncated latitude/longitude 
#            into setLatitude/setLongitude in order for output header info to 
#            match raw data info for the Release (Launch) Location line.
#          - Code was added to remove the last lines of a file if there were no 
#            valid altitude or pressure data.
#          - Search for "HARD-CODED" to find areas requiring project-specific 
#            changes.
#
#                                                                           
# @author Linda Echo-Hawk
# @version RICO_2004 I believe this was created for RICO (only tagged version).
#
##Module------------------------------------------------------------------------
package EOL_Upsonde_Converter;
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

my ($WARN);

printf "\nEOL_Upsonde_Converter.pl began on ";print scalar localtime;printf "\n";
&main();
printf "\nEOL_Upsonde_Converter.pl ended on ";print scalar localtime;printf "\n";

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the radiosonde data by converting it from the EOL
# format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = EOL_Upsonde_Converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature EOL_Upsonde_Converter new()
# <p>Create a new instance of an EOL_Upsonde_Converter.</p>
#
# @output $self A new EOL_Upsonde_Converter object.
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
    $self->{"NETWORK"} = "RV_Revelle";
    
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
# <p>Create a default station for the GAUS network using the specified 
# station_id and network.</p>
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
    $station->setStateCode("99");
    $station->setReportingFrequency("12 hourly");
    $station->setNetworkIdNumber(99);
    # Platform = 415  Radiosonde, Vaisala RS92-SGP
    $station->setPlatformIdNumber(415);

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
    my ($self,$file,@lines) = @_;
    my $header = ClassHeader->new();

    # Parse off the release direction of the sounding.
    my (undef(),$direction) = split(/\//,(split(/:/,$lines[0]))[1]);
    $header->setReleaseDirection(trim($direction));

    # Parse the type of sounding
    my ($project,$type) = split(/\//,(split(/:/,$lines[2]))[1]);
    $header->setType("GAUS SOUNDING DATA");
    $header->setProject($self->{"PROJECT"});
    
	my $site = "R/V Revelle";
    # HARD-CODED
    $header->setId("RV_Revelle");
    $header->setSite(trim($site));

	# "Release Location:" Header
	my ($lon_deg,$lon_min,$lon,$lat_deg,$lat_min,$lat,$elev) = split(' ',(split(/:/,$lines[4]))[1]);
	# strip the commas off of these values  
	$lon_min =~ s/,//g;
	$lon =~ s/,//g;
	$lat =~ s/,//g;       
	$header->setLatitude($lat, $self->buildLatlonFormat($lat));
	$header->setLongitude($lon, $self->buildLatlonFormat($lon));
	$header->setAltitude($elev, "m");

# from Seychelles converter
    # -------------------------------------------------
	# Get nominal time from file name
	# Ex: D20111218_113106_P.1.PresCorrQC.eol
	# -------------------------------------------------
	my $fileDate;
	my $fileTime;
	if ($file =~ /(\d{4})(\d{2})(\d{2})_(\d{2})(\d{2})(\d{2})/)
	{
	    $fileDate = sprintf("%04d, %02d, %02d", $1, $2, $3);
		$fileTime = sprintf("%02d:%02d:%02d", $4, $5, $6);
		# print "NOM:  $fileDate   NOM:  $fileTime\n";
    }
	# print "NOM:  $fileDate   NOM:  $fileTime\n";
	$header->setNominalRelease($fileDate,"YYYY, MM, DD",$fileTime,"HH:MM:SS",0);
    $header->setActualRelease($fileDate,"YYYY, MM, DD",$fileTime,"HH:MM:SS",0);

    # -------------------------------------------------
	# This code works, but since there are two raw
	# data files with the same header release time
	# their output files overwrote each other
	# -------------------------------------------------
    # Parse the release time from the header.
    # $lines[5] =~ /(\d{4}, \d{2}, \d{2}), (\d{2}:\d{2}:\d{2})/;
    # my ($date,$time) = ($1,$2);
    # $header->setActualRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
    # $header->setNominalRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
    
    # Add all non-predefined header lines to the header.
    # Changed $i to $i-1 to remove extra blank line from header. 
    for (my $i = 6; $i < 11; $i++) {
	if ($lines[$i] !~ /^\s*\/\s*$/) {
        # NOTE: New line "Additonal Comments" did not have colon for later split
		if ($lines[$i] =~ /Additonal Comments/) # note typo in raw data
		{
			my (@text) = split(" ",$lines[$i]);
			$text[0] =~ s/Additonal/Additional/;
			$text[1] =~ s/Comments/Comments:/;
			$lines[$i] = join(" ",@text);
		}
        # print "$lines[$i]";
	    my ($label,@data) = split(/:/,$lines[$i]);
	    $header->setLine(($i-1), trim($label).":",trim(join(":",@data)));
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
    
    printf("Processing file: %s\n",$file);
    
    open(my $FILE,$self->{"RAW_DIR"}."/".$file) or die("Can't open file: ".$file);
    my @lines = <$FILE>;
    close($FILE);
    
    # Generate the sounding header.
    my $header = $self->parseHeader($file,@lines[0..13]);
    
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

	$outfile = sprintf("%s_%04d%02d%02d%02d%02d%02d.cls", 
  	 					   $header->getId(),
	   					   split(/,/,$header->getActualDate()),
	   					   split(/:/,$header->getActualTime()));
 
    printf("\tOutput file name:  %s\n", $outfile);


	open(my $OUT,">".$self->{"OUTPUT_DIR"}."/".$outfile)
	    or die("Can't open output file for $file\n");

	print($OUT $header->toString());
	
	my $index = 0;
	my @record_list = ();
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
	    
	    if ($data[14] !~ /-999.000000/) {
		my $lon_fmt = $data[14] < 0 ? "-" : "";
		while (length($lon_fmt) < length($data[14])) { $lon_fmt .= "D"; }
		$record->setLongitude($data[14],$lon_fmt);
	    }
	    if ($data[15] !~ /-999.000000/) {
		my $lat_fmt = $data[15] < 0 ? "-" : "";
		while (length($lat_fmt) < length($data[15])) { $lat_fmt .= "D"; }
		$record->setLatitude($data[15],$lat_fmt);
	    }
	    $record->setAltitude($data[13],"m") if ($data[13] !~ /-999.00/);

		push(@record_list, $record);
	}

    # Remove the last records in the file that are 
    # descending after the balloon burst.
	foreach my $record (reverse(@record_list)){
		if (($record->getPressure() == 9999.0) && ($record->getAltitude() == 99999.0)){
			undef($record);
		} else {
			last;
		}
	}
    # Print the records to the file.
	foreach my $record(@record_list) {
		print ($OUT $record->toString()) if (defined($record));
	}	
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
    my @files = grep(/^D\d{8}_\d{6}.+\.eol/,sort(readdir($RAW)));
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
