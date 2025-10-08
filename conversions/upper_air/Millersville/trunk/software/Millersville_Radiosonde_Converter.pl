#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The Millersville_Radiosonde_Converter.pl script is used for converting high 
# resolution radiosonde ascii data to the EOL Sounding Composite
# (ESC) format.</p> 
#
# @usage Millersville_Radiosonde_Converter.pl [--convert_alt] [--rm_descending] 
#                                             [--SandyCreek]
#        --convert_alt  Convert the altitude values from feet to meters
#        --rm_descending  Remove data at end of file with negative ascension rates
#        --SandyCreek   Data from Sandy Creek Release Site; else it is Finger
#                       Lakes Technical Center
#        
#
# @author Linda Echo-Hawk 13 Aug 2014
# @version OWLeS Created for the Millersville University Radiosonde Data (revised 
#          the HWSmith_GRAW_Radiosonde_Converter.pl)
#          BEWARE: HARD_CODED information makes this converter difficult to reuse
#          - Sondes were released from two locations: Sandy Creek Central School and
#            Finger Lakes Technical Center. Release info for header and surface data
#            are dependent on this location. Raw data is placed in two separate 
#            directories: /FingerLakes_raw_data or /SandyCreek_raw_data and the 
#            command line switch above is used to determine which data should be
#            processed and what code should be used.
#          - Run the converter twice:  once with the --SandyCreek switch and
#            once without it (to process the Finger Lakes data)
#          - The last line of each file is blank, so code is included to skip
#            blank lines. Otherwise, missing and bogus values are put in.
#          - The surface lat/lon/elevation and release location was provided 
#            by Scot. The adjustment altitude is the header altitude provided
#            by Scot minus the surface record altitude. The adjustment altitude
#            must be added to each of the elevation records.
#          - Per the IVEN notes, there are special cases of files and times 
#            for which we do not set the lat/lon. Code is included to handle 
#            these special cases.
#          - Eight files were special cases which needed data removed from
#            the beginning of the file. See the IVEN notes for details. The
#            beginning records were manually removed from the raw data and 
#            code was added to detect if the surface record Time was not 
#            equal to zero. If that was the case, time was set to zero and
#            incremented as each record was read in.
#          - NOTE that the converter expects data to begin on line 16.
#          - Added code from the DYNAMO Singapore version of this 
#            converter to set Dewpt to missing if temp OR RH is missing
#          - Added switch to allow for removal of descending data at
#            the end of the file (i.e., ascension rates less than zero)
#
#
# @author Linda Echo-Hawk 2014-08-08
# @version OWLeS Revised for the Univ of Utah North Redfield soundings
#          - Raw data files were renamed to include the sonde id in the name
#          - Added code to calculate ascent rate
#          - Altitude was given in meters
#          - Removed check for "header" info at bottom of file
#          - Header lat/lon/alt obtained from surface data record.
#          - Removed conversion for Wind speed since it is given in m/s.
#          - Removed hard-coded corrections for surface altitudes.
#
#
# @author Linda Echo-Hawk 2012-05-18
# @version DYNAMO  Created based on VORTEX2 Singapore_Radiosonde_Converter.pl.
#          - Raw data files are ascii format and have *.txt extension
#            (first run dos2unix)
#          - Converter expects the actual data to begin
#            after the header lines (line number varies for
#            some files).  
#          - Header lat/lon/alt is hard-coded from csv file
#          - Release time is obtained from the file name.
#          - There is header information at the end of the file, so
#            there is a code check for "Tropopauses" to signal the
#            end of the data.
#          - Wind speed must be converted from knots to meters/second.
#          - Altitude must be converted from feet to meters.  NOTE that
#            some files have altitude in meters.  These files will be 
#            processed separately.  A command line switch has been added
#            so that the converter will know whether or not the altitude
#            needs to be converted from feet to meters.
#          - A correction factor was hard-coded in to handle two files
#            with incorrect surface altitudes. The default value of 
#            the correction factor is zero (for all other files).
#          - Code was added to remove the descending data after the 
#            sondes start to fall (ascent rate < 0).  
#
# @use     Millersville_Radiosonde_Converter.pl --SandyCreek >&! results.txt
#          Millersville_Radiosonde_Converter.pl >> & ! results.txt
#
#         
##Module------------------------------------------------------------------------
package Millersville_Radiosonde_Converter;
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
use DpgConversions;
# import module to set up command line options
use Getopt::Long;

my ($WARN);

printf "\nMillersville_Radiosonde_Converter.pl began on ";print scalar localtime;printf "\n";
my $debug = 0;


# read command line arguments 
my $result;   
# convert the altitude from feet to meters
my $convert_alt;
my $rm_descending;
my $sandyCreek;
# "limit:i" i is optional, "limit=i" i is required
$result = GetOptions("SandyCreek" => \$sandyCreek, "convert_alt" => \$convert_alt, "rm_descending" => \$rm_descending);

if ($sandyCreek)
{
	printf("Processing the Sandy Creek data.\n");
}
else
{
	printf("Processing the Finger Lakes data.\n");
}

if ($convert_alt)
{
	printf("Perform altitude conversion from feet to meters.\n");
}
else
{
	# printf("Do not perform altitude conversion.  Altitude given in meters.\n");
}
if ($rm_descending)
{
	printf("Removing descending data at end of file (negative ascension rate).\n");
}
&main();
printf "\nMillersville_Radiosonde_Converter.pl ended on ";print scalar localtime;printf "\n";

# my $filename;

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the Millersville Univ. radiosonde data by converting it from 
# the native ASCII format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = Millersville_Radiosonde_Converter->new();
    $converter->convert();
}
  
##------------------------------------------------------------------------------
# @signature Millersville_Radiosonde_Converter new()
# <p>Create a new instance of a Millersville_Radiosonde_Converter.</p>
#
# @output $self A new Millersville_Radiosonde_Converter object.
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
    $self->{"NETWORK"} = "Millersville";
    
    $self->{"FINAL_DIR"} = "../final";
    $self->{"OUTPUT_DIR"} = "../output";

    # $self->{"RAW_DIR"} = "../alt_in_meters/alt_in_meters_raw_data";
    if ($sandyCreek)
	{
		$self->{"RAW_DIR"} = "../SandyCreek_raw_data";
	}
	else
	{
		# if not Sandy Creek, this is Finger Lakes Technical Center
		$self->{"RAW_DIR"} = "../FingerLakes_raw_data";
	}
    
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
    $station->setStationName("Millersville");
    # HARD-CODED
    $station->setStateCode("99");
    $station->setReportingFrequency("no set schedule");
    $station->setNetworkIdNumber("99");
    # platform, 415, Radiosonde, Vaisala RS92-SGP
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

    # HARD-CODED
    # Set the type of sounding
    $header->setType("Millersville Radiosonde");
    $header->setReleaseDirection("Ascending");
    $header->setProject($self->{"PROJECT"});
	# The Id will be the prefix of the output file
    # and appears in the stationCD.out file
    $header->setId("Millersville");


    # -------------------------------------------------
    # Get the header lat/lon/alt data from the surface line
    # -------------------------------------------------
    my $lat;
	my $lon;
	my $alt;
	my $releaseSite;

    if ($sandyCreek)
	{
		$lat = 43.65;
		$lon = -76.07;
		$alt = 162;
		$releaseSite = "Sandy Creek Central School; Sandy Creek, NY";
	}
	else 
	{
        # if not Sandy Creek, this is Finger Lakes Technical Center
		$lat = 42.86;
		$lon = -77.11;
		$alt = 263;
		$releaseSite = "Finger Lakes Technical Center; Stanley, NY";
	}


	print "\tLAT: $lat LON: $lon ALT: $alt\n";
	print "\tRELEASE SITE: $releaseSite\n";

    $header->setLatitude($lat, $self->buildLatLonFormat($lat));
	$header->setLongitude($lon, $self->buildLatLonFormat($lon)); 
    $header->setAltitude($alt,"m");

	# "Release Site Type/Site ID:" header line
	$header->setSite($releaseSite);

    # ----------------------------------------------------------
    # Extract the date and time information from the file name
	# Expects filename similar to:
	# 1hzedt_20140128_2011.txt
    # ----------------------------------------------------------
    # print "file name = $filename\n"; 
	my $date;
	my $time;

    if ($file =~ /(\d{4})(\d{2})(\d{2})_(\d{2})(\d{2})/)
	{
		my ($year, $month, $day, $hour, $min) = ($1,$2,$3,$4,$5);
	    $date = join ", ", $year, $month, $day;
	    $time = join ":", $hour,$min,'00';

        $header->setActualRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
        $header->setNominalRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
	}

    # -------------------------------------------------
    # Get the sonde ID from the header lines
    # -------------------------------------------------
	my $sondeId;
	foreach my $headerline (@headerlines)
	{
		if ($headerline =~ /Sonde serial number/)
		{
			# print "\tHEADERLINES: $headerline\n";
    		my (@sonde_info) = split(" ",$headerline); 
			$sondeId = trim($sonde_info[3]);
			last;
		}
	}
    print "\tSonde ID: $sondeId\n";
    my $sondeType = "Vaisala RS92-SGP";
	$header->setLine(5,"Sonde Id/Sonde Type:", join("/",$sondeId, $sondeType));
    
	$header->setLine(6,"Surface Data Source: ","Kestrel");
	

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
	my @headerlines = @lines[0..2];
	# print "HEADER: $headerlines[0]\n";
	my $header = $self->parseHeader($file,@headerlines);
    
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
	

    # ----------------------------------------------------
    # Parse the data portion of the input file
    # ----------------------------------------------------

	# --------------------------------------------
    # Create an array to hold all of the data records.
	# This is required so additional processing can take
    # place to remove descending data records at the
	# end of the data files
	# --------------------------------------------
	my @record_list = ();
	# --------------------------------------------

    # ---------------------------------------------
    # Needed to correct geopotential height
    # ---------------------------------------------
	my $alt_adjustment;
	# --------------------------------------------
    my $useNewTimeValue = 0;
    my $recordTime = 0;
    my $surfaceRecord = 1;
	my $index = 0;
    # Now grab the data from each line
	foreach my $line (@lines) 
	{
        # Ignore the header lines
		if ($index < 17) { $index++; next; }
		
        # Skip any blank lines.
		# For Millersville, each file ended in a blank line.
		next if ($line =~ /^\s*$/);

		chomp($line);
	    my @data = split(' ',$line);

   		$data[0] = trim($data[0]); # Time
   	    $data[1] = trim($data[1]); # Height
   	    $data[2] = trim($data[2]); # Pressure
   	    $data[3] = trim($data[3]); # Temp (deg C)
   	    $data[4] = trim($data[4]); # RH
   		$data[5] = trim($data[5]); # dewpoint (deg C)
   		
   		$data[10] = trim($data[10]); # windspeed (m/s)
   		$data[11] = trim($data[11]); # Wind Dir (deg)
        $data[14] = trim($data[14]); # Ascension Rate
   		$data[15] = trim($data[15]); # lat (deg)
   	    $data[16] = trim($data[16]); # lon (deg)
	    	
   		my $record = ClassRecord->new($WARN,$file);

       	# missing values are ////

   		# --------------------------------------------
   		# for the 8 files whose surface values were
   		# much greater than zero, the beginning records
   		# were removed from the file and their data[0] 
   		# value will not be zero, so we will increment
   		# the time automatically with each record
   		# ---------------------------------------------
		if (($surfaceRecord) && ($data[0] !~ /^0$/))
		{
			$useNewTimeValue = 1;
		}
		if (!$useNewTimeValue)
		{
			$recordTime = $data[0];
		}

   	    $record->setTime($recordTime) if ($recordTime !~ /\/+/);
   	    $record->setPressure($data[2],"mb") if ($data[2] !~ /\/+/);
   	    $record->setTemperature($data[3],"C") if ($data[3] !~ /\/+/);    
   	    $record->setRelativeHumidity($data[4]) if ($data[4] !~ /\/+/);
	    	
   		# ---------------------------------------------------------
   		# setDewPoint unless temp is missing and if RH is missing
   		# handles the odd case where RH is "-----" and dewpt is
   		# invalid -- without this code RH gets set to "0"
       	# ---------------------------------------------------------
   		if (($data[3] !~ /\/+/) && ($data[4] !~ /\/+/))
   		{
   	    	$record->setDewPoint($data[5],"C") if ($data[5] !~ /\/+/);
   		}
			
   		$record->setWindSpeed($data[10],"m/s") if ($data[10] !~ /\/+/);
   		$record->setWindDirection($data[11]) if ($data[11] !~ /\/+/);
   		$record->setAltitude($data[1],"m");

   		$record->setAscensionRate($data[14],"m/s");

   		# ----------------------------------------------------
   		# The surface elevation was provided by Scot since 
   		# the surface values were incorect for several files.
		# The difference between the value provided by Scot 
		# and the surface elevation must be added to each of
   		# the other elevation records.
        # ----------------------------------------------------
		if ($surfaceRecord)
		{
			# --------------------------------------------
			# Calculate the altitude adjustment factor
			# --------------------------------------------
			if ($data[1] !~ /\/+/)
			{
	   	        # NOTE - the adjustment is the header altitude
				# provided by Scot minus the surface record altitude
				$alt_adjustment = ($header->getAltitude() - $data[1]);
			}
			else
			{
				print "Unable to calculate altitude adjustment\n";
			}
			print "\tALT_ADJ $alt_adjustment\n";
           	$record->setAltitude($header->getAltitude(),"m");

           	$record->setLatitude($header->getLatitude(), $self->buildLatLonFormat($header->getLatitude()));

           	$record->setLongitude($header->getLongitude(), $self->buildLatLonFormat($header->getLongitude()));
               
			$surfaceRecord = 0;
		}
   		else
		{
		
			# ----------------------------------------------------
	   		# Scot L. says: "add the surface value to every geopotential height 
   			# in the sounding."  NOTE that the "surface value" is the value provided 
   			# by Scot and is the altitude adjustment calculated above
	   		# ----------------------------------------------------
			if ($data[1] !~ /\/+/)
			{
				my $adjusted_alt = ($data[1] + $alt_adjustment);
				$record->setAltitude($adjusted_alt,"m");
			}
    		
	   		# ---------------------------------------------------------
	   		# Handle the special cases for latitude/longitude provided by Scot
   			# (see the IVEN notes)
   			# ---------------------------------------------------------
			if (($data[0] > 0) && ($data[0] < 7))
			{
   				# don't set the latitude or longitude
			}
		
   			elsif ((($file =~ /20140106_2315/) && (($data[0] >= 0) && ($data[0] < 14))) ||
   		    	(($file =~ /20140108_1552/) && (($data[0] >= 0) && ($data[0] < 91))))
   			{
   				# don't set the latitude or longitude
	   		}
   			else
   			{
           		$record->setLatitude($data[15], $self->buildLatLonFormat($data[15])) if ($data[15] !~ /\/+/);

	           	$record->setLongitude($data[16], $self->buildLatLonFormat($data[16])) if ($data[16] !~ /\/+/);
   			}
		}   	   
        	
        # increment the time if we are creating our own time values
		if ($useNewTimeValue)
		{
			$recordTime++;
		}
		# ----------------------------------------------------
		# Record assembled, now post process if needed,
		# or print to file
		# ----------------------------------------------------
			
		if ($rm_descending)
		{
           	push(@record_list, $record);
		}
		else
		{
			printf($OUT $record->toString());
		}

	} # end foreach $line

	if ($rm_descending)
	{
	    # --------------------------------------------------
		# Remove the last records in the file that are 
	    # descending (ascent rate is negative)
		# --------------------------------------------------
		foreach my $last_record (reverse(@record_list))
		{
	   		# if (($last_record->getPressure() == 9999.0) && 
			# 	($last_record->getAltitude() == 99999.0))
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
	} # end if ($rm_descending)

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
    my @files = grep(/^1hzedt.+\.txt/,sort(readdir($RAW)));
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
