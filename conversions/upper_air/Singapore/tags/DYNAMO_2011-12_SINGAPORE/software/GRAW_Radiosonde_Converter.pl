#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The GRAW_Radiosonde_Converter.pl script is used for converting high 
# resolution radiosonde ascii data to the EOL Sounding Composite
# (ESC) format.</p> 
#
# @usage GRAW_Radiosonde_Converter.pl [--convert_alt]
#        --convert_alt  Convert the altitude values from feet to meters
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
# @use     GRAW_Radiosonde_Converter.pl --convert_alt >&! results.txt
#
#         
##Module------------------------------------------------------------------------
package GRAW_Radiosonde_Converter;
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

printf "\nGRAW_Radiosonde_Converter.pl began on ";print scalar localtime;printf "\n";
my $debug = 0;


# read command line arguments 
my $result;   
# convert the altitude from feet to meters
my $convert_alt;
# "limit:i" i is optional, "limit=i" i is required
$result = GetOptions("convert_alt" => \$convert_alt);

if ($convert_alt)
{
	printf("Perform altitude conversion from feet to meters.\n");
}
else
{
	printf("Do not perform altitude conversion.  Altitude given in meters.\n");
}

&main();
printf "\nGRAW_Radiosonde_Converter.pl ended on ";print scalar localtime;printf "\n";

my $filename;

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the GRAW radiosonde data by converting it from 
# the native ASCII format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = GRAW_Radiosonde_Converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature GRAW_Radiosonde_Converter new()
# <p>Create a new instance of a GRAW_Radiosonde_Converter.</p>
#
# @output $self A new GRAW_Radiosonde_Converter object.
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

    # $self->{"RAW_DIR"} = "../alt_in_meters/alt_in_meters_raw_data";
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
    # platform, 416, Radiosonde, GRAW DFM-09
    $station->setPlatformIdNumber(416);
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


    # ----------------------------------------------------------
    # Extract the date and time information from the file name
	# Expects filename similar to 2011100100Z.csv (2011-10-01 00 UTC)
	# Filename: 201112250000107064_UPP_RAW_48698_2011122423.txt
    # ----------------------------------------------------------
    # print "file name = $filename\n"; 
    my $nomdate;
	my $nomtime;
	my $date;
	my $time;
    my $sondeId;

    if ($filename =~ /(\d{18})_UPP_RAW_(\d{5})_(\d{10})/)
	{
		my ($nom,$wmo,$release) = ($1,$2,$3);


	    if ($nom =~ /(\d{4})(\d{2})(\d{2})(\d{2})(\d{2})(\d{6})/)
	    {
		    # my ($year,$month,$day,$hour,$min,$sonde) = ($1,$2,$3,$4,$5,$6);
		    my ($nomyear,$nommonth,$nomday,$nomhour,$nommin) = ($1,$2,$3,$4,$5);
	        $nomdate = join ", ", $nomyear, $nommonth, $nomday;
	        $nomtime = join ":", $nomhour,$nommin,'00';
			$sondeId = $6;
		}
		if ($release =~ /(\d{4})(\d{2})(\d{2})(\d{2})/)
		{
			my ($year,$month,$day,$hour) = ($1,$2,$3,$4);
			$date = join ", ", $year, $month, $day;
			$time = join ":", $hour,'00','00';
		}
		if($debug)
		{
			printf("Nominal Date %s Time %s   Actual Date %s Time %s\n",
			       $nomdate, $nomtime, $date, $time);
		}
        
		$header->setActualRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
        $header->setNominalRelease($nomdate,"YYYY, MM, DD",$nomtime,"HH:MM:SS",0);

        my $sondeType = "GRAW DFM-09";
	    $header->setLine(5,"Sonde Id/Sonde Type:", join("/",$sondeId, $sondeType));

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
	

    # ----------------------------------------------------
    # Parse the data portion of the input file
    # ----------------------------------------------------
	my $surfaceRecord = 1;
	my $startData = 0;
	# --------------------------------------------
    # Create an array to hold all of the data records.
	# This is required so additional processing can take
    # place to remove descending data records at the
	# end of the data files
	# --------------------------------------------
	my @record_list = ();
	# --------------------------------------------

    # ----------------------------------------------------
    # Two files have incorrect surface altitudes
	# and the correction factor will be applied 
	# to each of the sonde's reported altitudes
    # ----------------------------------------------------
	my $corr_factor = 0;
	if ($file =~ /201201061100120451_UPP_RAW_48698_2012010610.txt/)
	{
	  	$corr_factor = 98;
	   	print "\tNOTE: $file has Correction Factor $corr_factor\n";
	}
	if ($file =~ /201201160000119931_UPP_RAW_48698_2012011523.txt/)
	{
	  	$corr_factor = 938;
	   	print "\tNOTE: $file has Correction Factor $corr_factor\n";
	}

    # Now grab the data from each lines
	foreach my $line (@lines) 
	{
        # Skip any blank lines.
		next if ($line =~ /^\s*$/);
        
		chomp($line);
	    my @data = split(' ',$line);
		# identify the last header line
		# to determine where the data starts        
		if (trim($data[0]) =~ /Time/i)
		{
			$startData = 1;
			next;
		}
        # Footer information starts here:
		if (($data[0] =~ /XXX/i) || ($data[0] =~ /Tropopauses/i))
		{
            $startData = 0;
		 	last;
		}
        
		if ($startData)
		{
			my $windspeed;
			my $alt;

			$data[0] = trim($data[0]); # Time
		    $data[1] = trim($data[1]); # Pres
		    $data[2] = trim($data[2]); # Temp
		    $data[3] = trim($data[3]); # U (%)= RH
		    $data[4] = trim($data[4]); # Wsp (kn)    need to convert to m/s
			if ($data[4] !~ /-----/)
			{
				$windspeed = convertVelocity($data[4],"knot","m/s");
			}
		    $data[5] = trim($data[5]); # wdir (deg)
			$data[7] = trim($data[7]); # GeoPot (ft) need to convert to m
			if ($data[7] !~ /-----/)
			{
				# $data[7] = $data[7] - $corr_factor;
				$alt = $data[7] - $corr_factor;
				if ($convert_alt)
				{
					$alt = convertLength($alt,"ft","m");
				}
			}
			# my $alt = $data[7] * .3048;
			$data[8] = trim($data[8]); # DewPoint (deg C)
			$data[9] = trim($data[9]); # ascent rate
			$data[11] = trim($data[11]); # azimuth
			$data[12] = trim($data[12]); # elevation


	    	my $record = ClassRecord->new($WARN,$file);

        	# missing values
		    $record->setTime($data[0]) if ($data[0] !~ /-----/);
		    $record->setPressure($data[1],"mb") if ($data[1] !~ /-----/);
		    $record->setTemperature($data[2],"C") if ($data[2] !~ /-----/);    
	    	# ---------------------------------------------------------
			# setDewPoint unless temp is missing or if RH is missing
	    	# ---------------------------------------------------------
			# $record->setDewPoint($data[8],"C") if ($data[8] !~ /-----/);    
			if (($data[2] !~ /-----/) && ($data[3] !~ /-----/))
			{
		    	$record->setDewPoint($data[8],"C") if ($data[8] !~ /-----/);
			}
		    $record->setRelativeHumidity($data[3]) if ($data[3] !~ /-----/);
			$record->setWindSpeed($windspeed,"m/s") if ($data[4] !~ /-----/);
			$record->setWindDirection($data[5]) if ($data[5] !~ /-----/);
            $record->setAltitude($alt,"m") if ($data[7] !~ /-----/);
    	    $record->setAscensionRate($data[9],"m/s") if ($data[9] !~ /-----/);
            # Insert Ele (Elevation Angle) and Azi (Azimuth Angle) data
		    # For setVariableValue(index, value):
		    # index (1) is Ele column, index (2) is Azi column.
		    $record->setVariableValue(1, $data[12]) if ($data[12] !~ /-----/);
		    $record->setVariableValue(2, $data[11]) if ($data[11] !~ /-----/);
		

			# ----------------------------------------------------
   			# get the lat/lon data for use in surface record only  
        	# ----------------------------------------------------
			if ($surfaceRecord)
			{
		    	# if surface record, use header lat/lon
	        	$record->setLatitude($header->getLatitude(),
			            $self->buildLatLonFormat($header->getLatitude()));
	        	$record->setLongitude($header->getLongitude(),
			            $self->buildLatLonFormat($header->getLongitude()));

				$surfaceRecord = 0;
			}

		    # printf($OUT $record->toString());
			push(@record_list, $record);

		} # end if ($startData)
	} # end foreach $line

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
    my @files = grep(/^201.+\.txt/,sort(readdir($RAW)));
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
