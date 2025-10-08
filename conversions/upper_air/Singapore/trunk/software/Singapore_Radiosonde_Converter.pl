#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The Singapore_Radiosonde_Converter.pl script is used for converting high 
# resolution radiosonde data from a csv format to the EOL Sounding Composite
# (ESC) format.</p> 
#
#
# @author Linda Echo-Hawk 2012-05-18
# @version DYNAMO  Created based on VORTEX2 SUNY_Radiosonde_Converter.pl.
#          - Raw data files are csv format and have *.csv extension
#            (first run dos2unix)
#          - Converter expects the actual data to begin
#            after the header lines (line number varies for
#            some files).  
#          - Header lat/lon/alt is hard-coded from csv file
#          - Release time is obtained from the file name.
#          - Some files contain a "missing" time last line.
#            Code was added so that these are not printed (set toString).
#          - Some files contain missing wind speed and/or direction on
#            the last data line.  Code was added to pop the last line
#            off of the data lines array and examine it.  If the data
#            is present the line is pushed back onto the array for
#            processing. Otherwise it is left off of the array.
#
#
##Module------------------------------------------------------------------------
package Singapore_Radiosonde_Converter;
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
my ($WARN);

printf "\nSingapore_Radiosonde_Converter.pl began on ";print scalar localtime;printf "\n";
my $debug = 0;
my $debug_geopotential_height = 0;

&main();
printf "\nSingapore_Radiosonde_Converter.pl ended on ";print scalar localtime;printf "\n";

my $filename;

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the Singapore radiosonde data by converting it from 
# the native ASCII format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = Singapore_Radiosonde_Converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature Singapore_Radiosonde_Converter new()
# <p>Create a new instance of a Singapore_Radiosonde_Converter.</p>
#
# @output $self A new Singapore_Radiosonde_Converter object.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);
    
    $self->{"stations"} = SimpleStationMap->new();

    # HARD-CODED
    $self->{"PROJECT"} = "DYNAMO";
    # HARD-CODED
    $self->{"NETWORK"} = "Singapore";
    
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
    $station->setStationName("Singapore");
    # HARD-CODED
    $station->setStateCode("99");
    $station->setReportingFrequency("no set schedule");
    $station->setNetworkIdNumber("99");
    # platform, 415, Radiosonde, Vaisala RS92-SGP
    $station->setPlatformIdNumber(415);
	# $station->setMobilityFlag("m");

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
    my ($self,$file) = @_;
    my $header = ClassHeader->new();

    $filename = $file;

    # HARD-CODED
    # Set the type of sounding
    $header->setType("Meteorological Service Singapore Radiosonde");
    $header->setReleaseDirection("Ascending");
    $header->setProject($self->{"PROJECT"});
	# The Id will be the prefix of the output file
    # and appears in the stationCD.out file
    $header->setId("Singapore");
	$header->setSite("WSSS Singapore/Changi / 48698");

    # -------------------------------------------------
    # Get the header lat/lon data
    # -------------------------------------------------

	my $lat = 1.3333;
	my $lon = 103.8;
	my $height = 14;

	# print "LAT: $lat LON: $lon\n";
    $header->setLatitude($lat, $self->buildLatLonFormat($lat));
	$header->setLongitude($lon, $self->buildLatLonFormat($lon)); 
    $header->setAltitude($height,"m");

    my $sondeType = "Vaisala RS92-SGP";
	$header->setLine(5,"Radiosonde Type:", ($sondeType));

    # ----------------------------------------------------------
    # Extract the date and time information from the file name
	# Expects filename similar to 2011100100Z.csv (2011-10-01 00 UTC)
    # ----------------------------------------------------------
    # print "file name = $filename\n"; 
    my $date;
	my $time;

	if ($filename =~ /(\d{4})(\d{2})(\d{2})(\d{2})Z/)
	{
		my ($year, $month, $day, $hour) = ($1,$2,$3,$4);
	    $date = join ", ", $year, $month, $day;
	    $time = join ":", $hour,'00','00';

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

    # Generate the sounding header.
	my $header = $self->parseHeader($file);
    
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
	
    # ----------------------------------------
    # Needed for code to derive ascension rate
    # ----------------------------------------
    my $prev_time = 9999.0;
    my $prev_alt = 99999.0;

    # ---------------------------------------------
    # Needed for code to derive geopotential height
    # ---------------------------------------------
    my $previous_record;
    my $geopotential_height;

    # ----------------------------------------------------
    # Parse the data portion of the input file
    # ----------------------------------------------------
	my $surfaceRecord = 1;
	my $startData = 0;

    #------------------------------------------------------
	# Examine the files to make sure that the last data
	# line is not missing any fields.
	# -----------------------------------------------------
	# Pop off the last line of each file (the @lines array).
    # For each of the last lines, if $data[7] is not present
	# delete the line from the array.  There should be six 
	# files with this problem. Else, push the line back onto
	# the end of the array.
	# -----------------------------------------------------
    my $last = pop@lines;
	my @last_data = split(',',$last);
	if ($last_data[7])
	{
		# print "LAST DATA 7 = $last_data[7]\n";
		push(@lines,$last);
	}
	else
	{
		print "WARN: Removing last data line, missing LAST data[7]\n";
	}
    
    # Now grab the data from each lines
	foreach my $line (@lines) 
	{
        # Skip any blank lines.
		next if ($line =~ /^\s*$/);
        
		chomp($line);
	    my @data = split(',',$line);
		# identify the last header line (has units in parens)
		# to determine where the data starts        
		if (trim($data[0]) =~ /\(s\)/i)
		{
			$startData = 1;
			next;
		}
        if ($startData)
		{
			$data[0] = trim($data[0]);
		    $data[1] = trim($data[1]);
		    $data[2] = trim($data[2]);
		    $data[3] = trim($data[3]);
		    $data[4] = trim($data[4]);
		    $data[5] = trim($data[5]);
		    $data[6] = trim($data[6]);
			$data[7] = trim($data[7]);

	    	my $record = ClassRecord->new($WARN,$file);

        	# missing values
		    $record->setTime($data[0]) if ($data[0] !~ /32768/);
		    $record->setPressure($data[1],"mb") if ($data[1] !~ /32768/);
		    $record->setTemperature($data[3],"C") if ($data[3] !~ /32768/);    
	    	$record->setDewPoint($data[4],"C") if ($data[4] !~ /32768/);    
		    $record->setRelativeHumidity($data[5]) if ($data[5] !~ /-32768/);
			$record->setWindSpeed($data[7],"m/s") if ($data[7] !~ /3276.8/);
			$record->setWindDirection($data[6]) if ($data[6] !~ /32768/);


			# ----------------------------------------------------
   			# get the lat/lon data for use in surface record only  
        	# ----------------------------------------------------
			if (!$surfaceRecord)
			{
				$record->setAltitude($data[2],"m") if ($data[2] !~ /\/+/);
			}
			else #(surface record)
			{
		    	# if surface record, use header lat/lon
	        	$record->setLatitude($header->getLatitude(),
			            $self->buildLatLonFormat($header->getLatitude()));
	        	$record->setLongitude($header->getLongitude(),
			            $self->buildLatLonFormat($header->getLongitude()));

				# for surface and header altitude, use value provided by Scot
            	$record->setAltitude($header->getAltitude(),"m");

				$surfaceRecord = 0;
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
            	# print "\nNEXT Line: prev_time, rec Time, prev_alt, rec Alt:: $prev_time, $time, $prev_alt, $alt\n"; 
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
        	# Only save the next non-missing values. 
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
    	    # for 29 files with last data lines with "missing" time, 
			# discard that last data line
		    printf($OUT $record->toString()) unless ($data[0] =~ /32768/);
			} # end if ($startData)
	    } # end foreach $line
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
    my @files = grep(/^2011.+\.csv/,sort(readdir($RAW)));
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
