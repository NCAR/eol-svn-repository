#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The SUNY_Radiosonde_Converter.pl script is used for converting high 
# resolution radiosonde data to the EOL Sounding Composite (ESC) format.</p> 
#
# @author Linda Echo-Hawk 2014-05-27
# @version OWLeS 2013-14 Revisions
#          - Commented out @surfaceLocs array info from VORTEX2
#            but left this in for future reference. Removal would
#            be cleaner, but this info may be helpful later.
#          - Added large %surfaceLocs hash table of header info 
#            provided by the PI in the readme.
#          - For some raw data files, the surface altitude was 
#            different than the header altitude (provided by the PI 
#            in the readme file).  Code was added to determine the 
#            difference and use this number to adjust the altitude 
#            in the raw data file (per Scot L.).
#          - Some code is left in from 2009 and 2010. It could
#            probably be removed, but since I have already run 
#            the converter and created the output data, I don't
#            want to change the original code at this time. It 
#            might be good to go through and remove this before
#            the next revision.
#
# @author Linda Echo-Hawk 2010-10-06
# @version VORTEX2 2010 Revised based on the 2009 version
#          - Added @surfaceLocs array to hard-code the lat/lon/alt/release
#            info provided by Scot L.   
#          - The data began on Line 1 of the file.
#
# @author Linda Echo-Hawk 2010-02-18
# @version VORTEX2  Created based on T-PARC_2008 Minami Daito Jima. 
#          - Raw data files are Excel format converted to csv format
#            with .csv extensions (also run dos2unix)
#          - Converter expects the actual data to begin
#            line 4 of the raw data file.  
#          - Header altitude info is obtained from the data.  
#          - Surface lat/lon info is obtained from header info.  
#          - Release time is obtained from the file name.
#          - Code was added from the Ron Brown Converter to derive
#            the ascension rate.
##Module------------------------------------------------------------------------
package SUNY_Radiosonde_Converter;
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

printf "\nSUNY_Radiosonde_Converter.pl began on ";print scalar localtime;printf "\n";
my $debug = 0;

&main();
printf "\nSUNY_Radiosonde_Converter.pl ended on ";print scalar localtime;printf "\n";

my $filename;

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the SUNY Oswego radiosonde data by converting it from 
# the native ASCII format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = SUNY_Radiosonde_Converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature SUNY_Radiosonde_Converter new()
# <p>Create a new instance of a SUNY_Radiosonde_Converter.</p>
#
# @output $self A new SUNY_Radiosonde_Converter object.
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
    $self->{"NETWORK"} = "SUNY_Oswego";
    
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
    $station->setStationName("SUNY Oswego");
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
    my ($self,$file,$sounding) = @_;
    my $header = ClassHeader->new();

    $filename = $file;

    # HARD-CODED
    # Set the type of sounding
    $header->setType("SUNY Oswego Mobile Radiosonde");
    $header->setReleaseDirection("Ascending");
    $header->setProject($self->{"PROJECT"});
	# The Id will be the prefix of the output file
    # and appears in the stationCD.out file
    $header->setId("SUNY_Oswego");


    # ---------------------------------------------------------------
	# Hard-code 2013-2014 OWLeS surface data from the readme
	# lat, lon, alt, Release Site, Surface Data Source
    # ---------------------------------------------------------------
    my %surfaceLocs = (

	"edt_20131207_1723.txt" => [43.46, -76.54, 107, "Oswego, NY Shineman observation deck", "Dr Eugene Chermack surface weather station at SUNY Oswego" ],
    
	"edt_20131207_2011.txt" => [43.46, -76.54, 107, "Oswego, NY Shineman observation deck", "Dr Eugene Chermack surface weather station at SUNY Oswego" ],
    
	"edt_20131207_2315.txt" => [43.46, -76.54, 107, "Oswego, NY Shineman observation deck", "Dr Eugene Chermack surface weather station at SUNY Oswego" ],
    
	"edt_20131210_2308.txt" => [43.73, -76.13, 120, "Ellisburg, NY", "Kestrel 4500" ],
    
	"edt_20131211_0217.txt" => [43.73, -76.13, 120, "Ellisburg, NY", "Kestrel 4500" ],
    
	"edt_20131211_2018.txt" => [43.46, -76.54, 107, "Ellisburg, NY", "Kestrel 4500" ],
    
	"edt_20131211_2312.txt" => [43.46, -76.54, 107, "Ellisburg, NY", "Kestrel 4500" ],
    
	"edt_20131212_2116.txt" => [43.84, -76.30, 76, "Henderson, NY lighthouse", "Kestrel 4500" ],

    "edt_20131215_2315.txt" => [43.45, -76.51, 107, "Oswego, NY Shineman observation deck", "Dr Eugene Chermack surface weather station at SUNY Oswego" ],
    
	"edt_20131216_0215.txt" => [43.45, -76.51, 107, "Oswego, NY Shineman observation deck", "Dr Eugene Chermack surface weather station at SUNY Oswego" ],
    
	"edt_20131216_0459.txt" => [43.45, -76.51, 107, "Oswego, NY Shineman observation deck", "Dr Eugene Chermack surface weather station at SUNY Oswego" ],

    "edt_20131218_1707.txt" => [43.73, -76.13, 109, "North of Ellisburg, NY on Machold Rd.", "Kestrel 4500" ],

    "edt_20131218_2007.txt" => [43.73, -76.13, 109, "North of Ellisburg, NY on Machold Rd.", "Kestrel 4500" ],

    "edt_20131218_2308.txt" => [43.73, -76.13, 109, "North of Ellisburg, NY on Machold Rd.", "Kestrel 4500" ],

    "edt_20140106_1842.txt" => [43.27, -76.97, 78, "Sodus Point, NY", "Kestral 4500 except pressure from radiosonde and wind direction from phone compass" ],

    "edt_20140106_2013.txt" => [43.27, -76.97, 78, "Sodus Point, NY", "Kestral 4500 except pressure from radiosonde and wind direction from phone compass" ],

    "edt_20140106_2144.txt" => [43.27, -76.97, 78, "Sodus Point, NY", "Kestral 4500 except pressure from radiosonde and wind direction from phone compass" ],
 
    "edt_20140107_0212.txt" => [43.85, -76.21, 75, "Henderson Harbor, NY", "Kestrel 4500 except pressure from radiosonde and wind direction from phone compass" ],

    "edt_20140107_0513.txt" => [43.85, -76.21, 75, "Henderson Harbor, NY", "Kestrel 4500 except pressure from radiosonde and wind direction from phone compass" ],

    "edt_20140107_0816.txt" => [43.85, -76.21, 75, "Henderson Harbor, NY", "Fort Drum (KGTB) except pressure from radiosonde and wind direction from phone compass" ],

    "edt_20140107_1113.txt" => [43.85, -76.21, 75, "Henderson Harbor, NY", "Watertown (KART) except pressure from radiosonde and wind direction from phone compass" ],

    "edt_20140107_1419.txt" => [43.85, -76.21, 75, "Henderson Harbor, NY", "Watertown (KART) except RH from Kestrel,  pressure 	from radiosonde and wind direction from phone compass" ],

    "edt_20140107_1715.txt" => [43.85, -76.21, 75, "Henderson Harbor, NY", "Kestrel 4500" ], 

    "edt_20140109_1113.txt" => [43.45, -76.50, 107, "Oswego, NY Shineman observation deck", "Dr Eugene Chermack surface weather station at SUNY Oswego" ],

    "edt_20140118_1712.txt" => [43.27, -76.97, 78, "Sodus Point, NY", "DOW-7 weather station except wind direction was estimated using aerovane on DOW-7" ],

    "edt_20140118_2027.txt" => [43.27, -76.97, 78, "Sodus Point, NY", "DOW-7 weather station except wind direction was estimated using aerovane on DOW-7" ],

    "edt_20140118_2311.txt" => [43.27, -76.97, 78, "Sodus Point, NY", "DOW-7 weather station except wind direction was estimated using aerovane on DOW-7" ],
	
    "edt_20140119_2313.txt" => [43.46, -76.54, 107, "Oswego, NY Shineman observation deck", "Dr Eugene Chermack surface weather station at SUNY Oswego except Kestrel 4500 for pressure" ],

    "edt_20140120_0111.txt" => [43.46, -76.54, 107, "Oswego, NY Shineman observation deck", "Dr Eugene Chermack surface weather station at SUNY Oswego except Kestrel 4500 for pressure" ],

    "edt_20140120_1041.txt" => [43.46, -76.54, 107, "Oswego, NY Shineman observation deck", "Dr Eugene Chermack surface weather station at SUNY Oswego except Kestrel 4500 for pressure" ], 

    "edt_20140120_1232.txt" => [43.46, -76.54, 107, "Oswego, NY Shineman observation deck", "Dr Eugene Chermack surface weather station at SUNY Oswego except Kestrel 4500 for pressure" ],

    "edt_20140120_1910.txt" => [43.27, -76.97, 76, "Sodus Point, NY", "Kestrel 4500 except wind direction from phone compass" ],

    "edt_20140120_2111.txt" => [43.27, -76.97, 76, "Sodus Point, NY", "Kestrel 4500 except wind direction from phone compass" ],

    "edt_20140120_2325.txt" => [43.27, -76.97, 76, "Sodus Point, NY", "Kestrel 4500 except wind direction from phone compass" ],

    "edt_20140121_0212.txt" => [43.27, -76.97, 76, "Sodus Point, NY", "Kestrel 4500 except wind direction from phone compass" ],

    "edt_20140121_0520.txt" => [43.27, -76.97, 76, "Sodus Point, NY", "Kestrel 4500 except wind direction from phone compass" ],

    "edt_20140121_0811.txt" => [43.27, -76.97, 76, "Sodus Point, NY", "Kestrel 4500 except wind direction from phone compass" ],

    "edt_20140121_1109.txt" => [43.27, -76.97, 76, "Sodus Point, NY", "Kestrel 4500 except wind direction from phone compass" ],

    "edt_20140121_1412.txt" => [43.27, -76.97, 76, "Sodus Point, NY", "Kestrel 4500 except wind direction from phone compass" ],

    "edt_20140121_1732.txt" => [43.27, -76.97, 76, "Sodus Point, NY", "Kestrel 4500 except wind direction from phone compass" ],

    "edt_20140123_2314.txt" => [43.27, -76.97, 78, "Sodus Point, NY", "Kestrel 4500" ],

    "edt_20140124_0213.txt" => [43.27, -76.97, 78, "Sodus Point, NY", "Kestrel 4500" ], 

    "edt_20140126_1112.txt" => [43.45, -76.54, 107, "Oswego, NY Shineman observation deck", "Kestrel 4500" ],

    "edt_20140126_1411.txt" => [43.45, -76.54, 107, "Oswego, NY Shineman observation deck", "Kestrel 4500" ],

    "edt_20140127_1839.txt" => [43.45, -76.54, 107, "Oswego, NY Shineman observation deck", "Kestrel 4500" ],

    "edt_20140127_2141.txt" => [43.45, -76.54, 112, "Oswego, NY Shineman observation deck", "Kestrel 4500" ],

    "edt_20140127_2310.txt" => [43.45, -76.54, 112, "Oswego, NY Shineman observation deck", "Kestrel 4500" ],

    "edt_20140128_0210.txt" => [43.45, -76.54, 112, "Oswego, NY Shineman observation deck", "Kestrel 4500" ],

    "edt_20140128_0511.txt" => [43.45, -76.54, 112, "Oswego, NY Shineman observation deck", "Kestrel 4500" ],

    "edt_20140128_1715.txt" => [43.45, -76.54, 107, "Oswego, NY Shineman observation deck", "Kestrel 4500" ],

    "edt_20140128_2017.txt" => [43.45, -76.54, 107, "Oswego, NY Shineman observation deck", "Kestrel 4500" ]
);

    # -------------------------------------------------
    # Hard-code 2010 surface data provided by Scot L.
    # ------------------------------------------------
#    my %surfaceLocs = (
#        "201005241748Z_EDT.txt" => [ 41.1448, -101.129, 902.6, "Sutherland, NE", "edge of farm field, growing storm to our SW ~20 km" ],
#	    "201005251710Z_EDT.txt" => [ 38.4868, -100.926, 902.1, "Scott City, KS", "edge of farm field, stratocumulus deck overhead" ],
#        "201005282020Z_EDT.txt" => [ 44.5987, -104.724, 1280.6, "Devils Tower, WY", "in field in Devils Tower National Monument park" ],
#		"201005282315Z_EDT.txt" => [ 44.5987, -104.724, 1280.6, "Devils Tower, WY", "in field in Devils Tower National Monument park" ],
#        "201005292045Z_EDT.txt" => [ 42.7845, -100.448, 829.3, "Valentine, NE", "just off road, towering cumulus to the NW, cold front passed through ~5 minutes before launch"],
#        "201005311950Z_EDT.txt" => [ 40.3644, -100.647, 818.1, "McCook, NE", "Red Willow State Recreation Area, few cumulus fractus clouds, could see anvil from Baca Co., Colorado tornadic supercell well to our south (> 100 km)" ],
#		"201006011841Z_EDT.txt" => [ 40.8081, -98.3819, 573.4, "Grand Island, NE", "farm field, broken sky of growing cumulus congestus clouds" ]
#	); 

    # -------------------------------------------------
    # Get the header lat/lon data
    # -------------------------------------------------

	my $lat = $surfaceLocs{$filename}[0];
	my $lon = $surfaceLocs{$filename}[1];
	my $height = $surfaceLocs{$filename}[2];
	my $releaseSite = $surfaceLocs{$filename}[3];
	my $surfaceDataSource = $surfaceLocs{$filename}[4];
    # HARD-CODED info provided by Scot
	my $radiosondeType = "Vaisala RS92-SGP";
	my $groundStnSoftware = "MW41";

	# print "LAT: $lat LON: $lon\n";
    $header->setLatitude($lat, $self->buildLatLonFormat($lat));
	$header->setLongitude($lon, $self->buildLatLonFormat($lon)); 

	$header->setLine(5,"Radiosonde Type:",$radiosondeType);
	$header->setLine(6,"Ground Station Software:",$groundStnSoftware);
	$header->setLine(7,"Surface Data Source:",$surfaceDataSource);

	$header->setSite($releaseSite);
    

	# ------------------------------------------------
	# For SUNY Oswego data (10 total soundings)
	# ------------------------------------------------
	# Because header altitude values are hard-coded
	# into the @surfaceElevation array, the sounding
	# number is required to index into array
	# ------------------------------------------------
    # surface elevation data provided by Scot L.
	# ------------------------------------------------
	# This is the 2009 data surface elevation 
	# my @surfaceElevation = (844,314,345,381,338,336,656,1648,1235,363);
	# ------------------------------------------------
	# my @surfaceElevation = (902.6, 902.1, 1280.6, 1280.6, 829.3, 818.1, 573.4);
	# my $height = $surfaceElevation[$sounding];

	# print "HEIGHT: $height\n";
    # $header->setAltitude($height,"m"); 

    $header->setAltitude($height,"m");

    # ----------------------------------------------------------
    # Extract the date and time information from the file name
	# Expects 2013-2014 filename similar to: edt_20131207_1723.txt
    # Expects 2010 filename similar to: 201005241648Z_EDT.txt
    # Expects 2009 filename similar to: 20090607_0048Z_2s.csv 
    # ----------------------------------------------------------
    # print "file name = $filename\n"; 
    my $date;
	my $time;

	if ($filename =~ /(\d{4})(\d{2})(\d{2})_(\d{2})(\d{2})/)
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
    my ($self,$file,$sounding) = @_;
    
    printf("\nProcessing file: %s\n",$file);

     open(my $FILE,$self->{"RAW_DIR"}."/".$file) or die("Can't open file: ".$file);
    my @lines = <$FILE>;
    close($FILE);
    
	# print "LINE 5: $lines[4]\n";

    # Generate the sounding header.
    # ------------------------------------------------
	# 2009 ONLY: For SUNY Oswego data (10 total soundings)
	# Because header altitude values are hard-coded
	# into the @surfaceElevation array, the sounding
	# number is required to index into array
	# ------------------------------------------------
	my $header = $self->parseHeader($file,$sounding);
    
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
	my $alt_adjustment;

    # ----------------------------------------------------
    # Parse the data portion of the input file
    # ----------------------------------------------------
	my $surfaceRecord = 1;
	foreach my $line (@lines) 
	{

		chomp($line);
        # Skip any blank lines.
		next if ($line =~ /^\s*$/);
		
	    my @data = split(' ',$line);
		$data[0] = trim($data[0]); # Time (s)
		$data[1] = trim($data[1]); # Height (m)
		$data[2] = trim($data[2]); # Pressure (hPa)
		$data[3] = trim($data[3]); # Temperature (deg C)
		$data[4] = trim($data[4]); # Relative Humidity (%)
		$data[5] = trim($data[5]); # Wind Direction (deg)
		$data[6] = trim($data[6]); # Wind Speed (m/s)

	    my $record = ClassRecord->new($WARN,$file);

        # missing values are ///// 
	    $record->setTime($data[0]);
	    $record->setPressure($data[2],"mb") if ($data[2] !~ /\/+/);
	    $record->setTemperature($data[3],"C") if ($data[3] !~ /\/+/);    
	    $record->setRelativeHumidity($data[4]) if ($data[4] !~ /\/+/);
	    $record->setWindSpeed($data[6],"m/s") if ($data[6] !~ /\/+/);
	    $record->setWindDirection($data[5]) if ($data[5] !~ /\/+/);

		# ----------------------------------------------------
	    # get the lat/lon data for use in surface record only  
		#
		# if surface and header altitude differ, subtract the raw_data
		# value from the header altitude provided in the readme.  This 
		# diffrence will be the adjustment that must be subtracted from 
		# the raw data altitude values
        # ----------------------------------------------------
		if ($surfaceRecord)
		{
		    # if surface record, use header lat/lon
	        $record->setLatitude($header->getLatitude(),
			            $self->buildLatLonFormat($header->getLatitude()));
	        $record->setLongitude($header->getLongitude(),
			            $self->buildLatLonFormat($header->getLongitude()));

		    my $hdr_alt = $header->getAltitude();
        	$alt_adjustment = $data[1] - $hdr_alt;
			print "ALT_ADJ $alt_adjustment = SURFACE: $data[1] - HDR: $hdr_alt\n";

            $record->setAltitude($header->getAltitude(),"m");

			$surfaceRecord = 0;
		}
		
		# ----------------------------------------------------
		# Scot L. says:
		# For the soundings where the surface value in the data file is 
		# different we will need to adjust all of the geopotential 
		# altitude data in the file (we need to subtract the difference
		# from every geopotential altitude value in the file).
		# ----------------------------------------------------
		my $alt = ($data[1] - $alt_adjustment);

		$record->setAltitude($alt,"m");

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
        # Only save off the next non-missing values. 
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

	    printf($OUT $record->toString());
    }
	}
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
    my @files = grep(/^edt_.+\.txt/,sort(readdir($RAW)));
    closedir($RAW);
    
    
    open($WARN,">".$self->{"WARN_LOG"}) or die("Can't create ".$self->{"WARN_LOG"});
    
	# printf("Ready to read the files\n");
	# ------------------------------------------------
	# For SUNY Oswego data (2010 - 7 total soundings)
    # (2009 - 10 total soundings)
	# ------------------------------------------------
	# Because header altitude values are hard-coded
	# into the @surfaceElevation array, the sounding
	# number is required to index into array
	# ------------------------------------------------
	my $sounding = 0;
    foreach my $file (@files) {
	$self->parseRawFile($file,$sounding);
	$sounding++;
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
