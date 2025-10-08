#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The UIllinois_Radiosonde_Converter.pl script is used for converting high 
# resolution radiosonde data from the University of Illinois to the EOL 
# Sounding Composite (ESC) format.</p> 
#
# @author Linda Echo-Hawk 2014-05-27
# @version OWLeS 2013-14 Created based on the SUNY_Radiosonde_Converter.pl
#          - The converter expects file names similar to:
#            data.UILL_sonde.201401212315.Cobourg_ON.txt
#            data.UILL_sonde.201401232315.Darlington_ProvPark_ON.txt
#          - Data values begin on the second line of the file
#          - There are two release sites, Cobourg and Darlington,
#            but Darlington actually has two sites. Scot has 
#            provided release altitude values based on the name
#            and longitude of the site.  See IVEN notes for detail.
#          - The provided surface elevation must be added to each
#            of the other elevation records in the raw data file
#            so it is the $alt_adjustment value.
#          - Wind speed is in knots and must be converted to m/s.
#          - Release time is obtained from the file name.
#          - Code was added from the Ron Brown Converter to derive
#            the ascension rate.
#
# @use UIllinois_Radiosonde_Converter.pl >&! results.txt
#
##Module------------------------------------------------------------------------
package UIllinois_Radiosonde_Converter;
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

use DpgDate qw(:DEFAULT);
use DpgConversions;
use SimpleStationMap;
use Station;
use ClassConstants qw(:DEFAULT);
use ClassHeader;
use ClassRecord;
use DpgCalculations;
my ($WARN);

printf "\nUIllinois_Radiosonde_Converter.pl began on ";print scalar localtime;printf "\n";
my $debug = 0;

&main();
printf "\nUIllinois_Radiosonde_Converter.pl ended on ";print scalar localtime;printf "\n";

my $filename;

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the UIllinois mobile radiosonde data by converting it from 
# the native ASCII format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = UIllinois_Radiosonde_Converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature UIllinois_Radiosonde_Converter new()
# <p>Create a new instance of a UIllinois_Radiosonde_Converter.</p>
#
# @output $self A new UIllinois_Radiosonde_Converter object.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);
    
    $self->{"stations"} = SimpleStationMap->new();

    # HARD-CODED
    $self->{"PROJECT"} = "OWLeS";
    # HARD-CODED
    $self->{"NETWORK"} = "UIllinois";
    
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
# <p>Create a default station for the UIllinois network using the 
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
    $station->setStationName("UIllinois");
    # HARD-CODED (NY for OWLeS)
    $station->setStateCode("36");
    $station->setReportingFrequency("no set schedule");
    $station->setNetworkIdNumber("99");
    # platform, Vaisala RS92-SGP 
    $station->setPlatformIdNumber(415);
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
# @output $fmt The format that corresponds the the value.
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
    $header->setType("University of Illinois Mobile Radiosonde");
    $header->setReleaseDirection("Ascending");
    $header->setProject($self->{"PROJECT"});
	# The Id will be the prefix of the output file
    # and appears in the stationCD.out file
    $header->setId("UIllinois");


    # -------------------------------------------------
    # Get the header lat/lon data
    # -------------------------------------------------
    
	my (@header_info) = split(" ",$headerlines[1]);
	my $lon = trim($header_info[6]);
	my $lat = trim($header_info[7]);
    my $alt;
    my $releaseSite;
    # -----------------------------------------------------
	# NOTE that there are two release sites, Cobourg and
	# Darlington, but Darlington actually has two sites.
	# Scot has provided the release altitude  values to be
	# used based on the name and longitude of the site.
	# See the IVEN notes for more detail.
	# -----------------------------------------------------
	if ($filename =~ /Cobourg/i)
	{
        $releaseSite = "Cobourg, Ontario";
		$alt = 75;
	}
	else
	{
		if ($lon =~ /-78.79/)
	    {
		    $releaseSite = "Darlington (Ontario) Provincial Park coastal site";
		    $alt = 78;
	    }
	    else
	    {
		    $releaseSite = "Darlington (Ontario) Provincial Park inland site";
		    $alt = 90;
	    }
	}
     
	# print "LAT: $lat LON: $lon\n";
	$header->setLongitude($lon,$self->buildLatLonFormat($lon));
	$header->setLatitude($lat,$self->buildLatLonFormat($lat));
	$header->setAltitude($alt,"m");
	$header->setSite($releaseSite);

    # HARD-CODED info provided by Scot
	my $surfaceDataSource = "Kestrel 4500 Pocket Weather Tracker";
	my $radiosondeType = "GRAW DFM-09";
	my $groundStnSoftware = "GRAWmet 5 version 5.9.2.4";

	$header->setLine(5,"Radiosonde Type:",$radiosondeType);
	$header->setLine(6,"Ground Station Software:",$groundStnSoftware);
	$header->setLine(7,"Surface Data Source:",$surfaceDataSource);

    # ----------------------------------------------------------
    # Extract the date and time information from the file name
	# data.UILL_sonde.201401212315.Cobourg_ON.txt
	# data.UILL_sonde.201401232315.Darlington_ProvPark_ON.txt
    # ----------------------------------------------------------
    # print "file name = $filename\n"; 
    my $date;
	my $time;

	if ($filename =~ /(\d{4})(\d{2})(\d{2})(\d{2})(\d{2})/)
	{
		my ($year, $month, $day, $hour, $min) = ($1,$2,$3,$4,$5);
	    $date = join ", ", $year, $month, $day;
	    $time = join ":", $hour,$min,'00';

        $header->setActualRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
        $header->setNominalRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
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
    
    # ------------------------------------------------
    # Generate the sounding header.
    # ------------------------------------------------
	my @headerlines = @lines[0..1];
	my $header = $self->parseHeader($file,@headerlines);
	# ------------------------------------------------
    
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

    # ---------------------------------------------
    # Needed to correct geopotential height
    # ---------------------------------------------
	my $alt_adjustment;

    # ----------------------------------------------------
    # Parse the data portion of the input file
    # ---------------------------------------------------
	my $index = 0;
	my $surfaceRecord = 1;
	foreach my $line (@lines) 
	{
        # Ignore the header line
		if ($index < 1) { $index++; next; }

		chomp($line);
        # Skip any blank lines.
		next if ($line =~ /^\s*$/);
		
	    my @data = split(' ',$line);
		$data[0] = trim($data[0]); # Time (s)
		$data[1] = trim($data[1]); # Pressure (hPa)
		$data[2] = trim($data[2]); # Temperature (deg C)
		$data[3] = trim($data[3]); # U (%) Relative Humidity (%)
		$data[4] = trim($data[4]); # Wind Speed (m/s)
		$data[5] = trim($data[5]); # Wind Direction (deg)

		$data[6] = trim($data[6]); # Lon (deg)
		$data[7] = trim($data[7]); # Lat (deg)
		$data[8] = trim($data[8]); # Height (m)

	    my $record = ClassRecord->new($WARN,$file);

        # missing values are -----
	    $record->setTime($data[0]);
	    $record->setPressure($data[1],"mb") if ($data[1] !~ /--+/);
	    $record->setTemperature($data[2],"C") if ($data[2] !~ /--+/);    
	    $record->setRelativeHumidity($data[3]) if ($data[3] !~ /--+/);

        # convert from knots to m/s
		if ($data[4] !~ /--+/)
		{
			my $windSpeed = $data[4];
			my $convertedWindSpeed = convertVelocity($windSpeed,"knot",
			"m/s");
			$record->setWindSpeed($convertedWindSpeed,"m/s");
		}
	    $record->setWindDirection($data[5]) if ($data[5] !~ /--+/);

	    $record->setLongitude($data[6],$self->buildLatLonFormat($data[6]));
	    $record->setLatitude($data[7],$self->buildLatLonFormat($data[7]));

		# ----------------------------------------------------
		# The surface elevation was provided by Scot since 
		# the 0 second record used a "0" elevation. The value
		# provided by Scot must be added to each of the other
		# elevation records.
		#
        # ----------------------------------------------------
		if ($surfaceRecord)
		{
		    $alt_adjustment = $header->getAltitude();
			print "ALT_ADJ $alt_adjustment\n";
            # NOTE - this adjustment has already been 
			# made to the header altitude
            $record->setAltitude($header->getAltitude(),"m");

			$surfaceRecord = 0;
		}
		
		# ----------------------------------------------------
		# Scot L. says: "add the surface value to every 
		# geopotential height in the sounding." 
		# NOTE that the "surface value" is the value 
		# provided by Scot and is the altitude 
		# adjustment calculated above
		# ----------------------------------------------------
		my $alt;
		if ($data[8] !~ /--+/)
		{
			$alt = ($data[8] + $alt_adjustment);
			$record->setAltitude($alt,"m");
		}

        #-------------------------------------------------------
        # this code from Ron Brown converter:
        # Calculate the ascension rate which is the difference
        # in altitudes divided by the change in time. Ascension
        # rates can be positive, zero, or negative. But the time
        # must always be increasing (the norm) and not missing.
        #
        # Only save off the next non-missing values.
        # Ascension rates over spans of missing values are OK.
        #-------------------------------------------------------
        if ($debug) 
		{
			my $time = $record->getTime(); my $alt = $record->getAltitude(); 
			print "Gather ascension rate data:\n";
			print "\tPrevious Time $prev_time  altitude = $prev_alt\n";
			print "\tCurrent Time $time  altitude = $alt\n";
		}

        if ($prev_time != 9999  && $record->getTime()     != 9999  &&
            $prev_alt  != 99999 && $record->getAltitude() != 99999 &&
            $prev_time != $record->getTime() ) 
        {
            $record->setAscensionRate( ($record->getAltitude() - $prev_alt) /
                                     ($record->getTime() - $prev_time),"m/s");

            if ($debug) { print "Calc Ascension Rate.\n"; }
        }

        #-----------------------------------------------------
        # Ascension rates over spans of missing values are OK.
        #-----------------------------------------------------
        if ($debug) 
		{ 
			my $rectime = $record->getTime(); my $recalt = $record->getAltitude();
            print "Current record: Time $rectime  Altitude = $recalt "; 
		}

        if ($record->getTime() != 9999 && $record->getAltitude() != 99999)
        {
            $prev_time = $record->getTime();
            $prev_alt = $record->getAltitude();

            if ($debug) 
			{ 
				print " has valid Time and Alt.\n"; 
			}
        }
        #-------------------------------------------------------
		# Completed the ascension rate data
        #-------------------------------------------------------

	    # printf($OUT $record->toString());
		push(@record_list, $record);
    } # end foreach my $line


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

	} # end if (defined($header)) 
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
    my @files = grep(/^data.+\.txt/,sort(readdir($RAW)));
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
