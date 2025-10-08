#! /usr/bin/perl -w
##Module------------------------------------------------------------------------
# <p>The FP6_HESS_ESC_sounding_converter.pl script is used for converting high
# resolution radiosonde data from ASCII formatted data) to the EOL Sounding 
# Composite (ESC) format. The incoming format is an older Vaisala format.</p> 
#
# @author Linda Echo-Hawk
# @version PECAN 2015 
#          - The converter expects filenames in the following
#            format: sgpsonde-curC1.YYYYMMDD.HHmm.raw (e.g., 
#            sgpsonde-curC1.20150417.0514.raw)
#          - The file contains header info on lines 1-26. Actual data starts 
#            on line 28. 
#          - The radiosonde ID is obtained from the header information.
#          - No lat/lon values appear in the data. The lat/lon header values 
#            are used in the surface record (t=0). 
#          - Missing values are represented by "///" in the raw data.
#          - The release date and time and obtained from the header.
#          - Note: Some raw data files have last lines that are only 
#            partial lines and the missing values show up as "missing."
#            Scot contacted the PI but did not hear back about this.
#
# @use FP6_HESS_ESC_sounding_converter.pl >&! results.txt
#
##Module------------------------------------------------------------------------
package FP6_HESS_ESC_sounding_converter;
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
use ElevatedStationMap;
use Station;
use ClassConstants qw(:DEFAULT);
use ClassHeader;
use ClassRecord;

my ($WARN);

printf "\nFP6_HESS_ESC_sounding_converter.pl began on ";print scalar localtime;printf "\n";
my $debug = 0;
&main();
printf "\nFP6_HESS_ESC_sounding_converter.pl ended on ";print scalar localtime;printf "\n";

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the Hesston radiosonde data by converting it from 
# the native ASCII format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = FP6_HESS_ESC_sounding_converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature FP6_HESS_ESC_sounding_converter new()
# <p>Create a new instance of an FP6_HESS_ESC_sounding_converter.</p>
#
# @output $self A new FP6_HESS_ESC_sounding_converter object.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);
    
    $self->{"stations"} = ElevatedStationMap->new();

    # HARD-CODED
    $self->{"PROJECT"} = "PECAN";
    # HARD-CODED
    $self->{"NETWORK"} = "FP6_Hesston";
    
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
# <p>Create a default station for the West Texas Mesonetnetwork using the 
# specified station_id and network.</p>
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
	$station->setCountry("99");
    # $station->setStateCode("48");
    $station->setReportingFrequency("no set schedule");
    $station->setNetworkIdNumber("99");
    # platform, Radiosonde, Vaisala RS92
    $station->setPlatformIdNumber(944);
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
# @signature int getMonth(String abbr)
# <p>Get the number of the month from an abbreviation.</p>
#
# @input $abbr The month abbreviation.
# @output $mon The number of the month.
# @warning This function will die if the abbreviation is not known.
##------------------------------------------------------------------------------
sub getMonth {
    my $self = shift;
    my $month = shift;

    if ($month =~ /JAN/i) { return 1; }
    elsif ($month =~ /FEB/i) { return 2; }
    elsif ($month =~ /MAR/i) { return 3; }
    elsif ($month =~ /APR/i) { return 4; }
    elsif ($month =~ /MAY/i) { return 5; }
    elsif ($month =~ /JUN/i) { return 6; }
    elsif ($month =~ /JUL/i) { return 7; }
    elsif ($month =~ /AUG/i) { return 8; }
    elsif ($month =~ /SEP/i) { return 9; }
    elsif ($month =~ /OCT/i) { return 10; }
    elsif ($month =~ /NOV/i) { return 11; }
    elsif ($month =~ /DEC/i) { return 12; }
    else { die("Unknown month: $month\n"); }
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

	# printf("parsing header for %s\n",$file);

    # Set the type of sounding "Data Type:" header line
    $header->setType("ARM");
    $header->setReleaseDirection("Ascending");

    $header->setProject($self->{"PROJECT"});
    
    # HARD-CODED
	# The Id will be the prefix of the output file
    $header->setId("HESS");
	# "Release Site Type/Site ID:" header line
    $header->setSite("FP6 Hesston, KS/HESS");


    # ------------------------------------------------
    # Read through the file for additional header info
    # ------------------------------------------------
 	my $index = 0;

	foreach my $line (@headerlines) 
	{
        # skip over any blank lines (empty or contain white space only)
   	    if ($line =~ /RS-Number/i)
   	    {
   			chomp ($line);
   		    my ($label,@contents) = split(/:/,$line);
   			$label = "Sonde Id/Sonde Type";
			trim($contents[0]);
   			$contents[1] = "Vaisala RS92";
   	        $header->setLine(5, trim($label).":",trim(join("/",@contents)));
   	    }

		if ($line =~ /Pressure/)
		{
			chomp $line;
            my @values = split(' ', trim($line));

            #-------------------------------------------------------------
            # Convert "Pressure    : 1013.8 1013.6    0.2"     to
            # "Ground Check Pressure:    Ref 1013.8 Sonde 1013.6 Corr 0.2"
            #-------------------------------------------------------------
            my $GroundCheckPress = trim("Ref ". $values[2].
                   " Sonde ".$values[3]." Corr ".$values[4]);
            if ($debug) {print "   Ground Check Pressure:: $GroundCheckPress\n";}
            $header->setLine(7,"Ground Check Pressure:    ", $GroundCheckPress);
		}
		if ($line =~ /Temperature/)
		{
   			chomp ($line);
            my @values = split(' ', trim($line));

            #-----------------------------------------------------------
            # Convert "Temperature :   19.4   19.9   -0.5"     to
            # "Ground Check Temperature: Ref 19.4 Sonde 19.9 Corr -0.5"
            #-----------------------------------------------------------
            my $GroundCheckTemp = trim("Ref ". $values[2]." Sonde ".
                          $values[3]." Corr ".$values[4]);
            if ($debug) {print "   Ground Check Temperature:: $GroundCheckTemp\n";}
            $header->setLine(8,"Ground Check Temperature: ", $GroundCheckTemp);

  		}
   		if ($line =~ /Humidity1/)
   		{
   			chomp ($line);
            my @values = split(' ', trim($line));

            #-----------------------------------------------------------
  		    # Convert "Humidity: 
   			# 
            #-----------------------------------------------------------
            my $GroundCheckHumidity = trim("Ref ". $values[2]." Sonde ".
                          $values[3]." Corr ".$values[4]);
            if ($debug) {print "   Ground Check Humidity:: $GroundCheckHumidity\n";}
            $header->setLine(9,"Ground Check Humidity: ", $GroundCheckHumidity);

   		}

        if ($line =~ /Location/)
   		{
   		    chomp ($line);
   		    my (@act_releaseLoc) = (split(' ',(split(/:/,$line))[1]));
   		    my $lat = $act_releaseLoc[0];

   			my $lon = $act_releaseLoc[2];
			# print "@act_releaseLoc\n";
			if ($act_releaseLoc[3] =~ /W/)
			{
				$lon = "-".$lon;
			}
            my $alt = $act_releaseLoc[4];
            # print "LAT: $lat  LON: $lon  ALT: $alt\n";

	        $header->setLatitude($lat,$self->buildLatLonFormat($lat));
	        $header->setLongitude($lon,$self->buildLatLonFormat($lon));

           	$header->setAltitude($alt,"m");
   		}
   		# "Started at       1 August 2008 11:30 UTC" 
  		if ($line =~ /Started at/)
   		{
   			chomp ($line);
   		    my (@releaseTime) =  (split(' ',trim($line)));
   			my ($hour, $min) = (split(':',trim($releaseTime[5])));
   			# print "HOURS: $hour   MIN: $min\n";
   			my $time = sprintf("%02d:%02d:00", $hour, $min);
            my $date = sprintf("20%02d, %02d, %02d", $releaseTime[4],
   			           $self->getMonth($releaseTime[3]),$releaseTime[2]);
   		    # print "DATE: $date   TIME: $time\n";
            $header->setActualRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);

	    }


   		$index++;
	}

    $header->setLine(6,"Ground Station Equipment:    ", "MW31 V 3.64.1");

    # ----------------------------------------------------------
    # Extract the date and time information from the file name
    # Expects filename similar to: 94975_20140614231512.tsv
    # ----------------------------------------------------------
    # print "file name = $file\n"; 

	if ($file =~ /^sgpsonde-curC1.(\d{4})(\d{2})(\d{2}).(\d{2})(\d{2})/)
	{
		my ($year, $month, $day, $hour, $min) = ($1,$2,$3,$4,$5);
        my $date = join ", ", $year, $month, $day;
		my $time = join ":", $hour,$min,'00';
        print "FROM FILE: DATE:  $date   TIME:  $time\n";

    	# $header->setActualRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
	    # $header->setNominalRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
	}

    # Set nominal times. FROM CHINA LAKE TREX
    my $hour = substr($header->getActualTime(),0,2);
    my ($date,$time) = adjustDateTime($header->getActualDate(),"YYYY, MM, DD",
				      $header->getActualTime(),"HH:MM:SS",
				      0,$hour % 3 == 0 ? 0 : 3 - ($hour % 3),
				      -1 * substr($header->getActualTime(),3,2),0);
    $header->setNominalRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);

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
	my @headerlines = @lines[0..22];
    my $header = $self->parseHeader($file,@headerlines);

    # Only continue processing the file if a header was created.
    if (defined($header)) {

	# Determine the station the sounding was released from.
	my $station = $self->{"stations"}->getStation($header->getId(),$self->{"NETWORK"},
						      $header->getLatitude(),$header->getLongitude(),
						      $header->getAltitude());
	if (!defined($station)) {
	    $station = $self->buildDefaultStation($header->getId(),$self->{"NETWORK"});
	    $station->setLatitude($header->getLatitude(),$self->buildLatLonFormat($header->getLatitude()));
	    $station->setLongitude($header->getLongitude(),$self->buildLatLonFormat($header->getLongitude()));
	    $station->setElevation($header->getAltitude(),"m");
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
 
    printf("\tOutput file name:  %s\n", $outfile);


	open(my $OUT,">".$self->{"OUTPUT_DIR"}."/".$outfile)
	    or die("Can't open output file for $file\n");

	print($OUT $header->toString());
	
    # ----------------------------------------
    # Needed for code to derive ascension rate
    # ----------------------------------------
    my $prev_time = 9999.0;
    my $prev_alt = 99999.0;

    # ----------------------------------------------------
    # Parse the data portion of the input file
    # ----------------------------------------------------
	my $index = 0;
	my $surfaceRecord = 1;

	foreach my $line (@lines) {
	    # Ignore the header lines.
	    if ($index < 27) { $index++; next; }
	
        # if it is a blank line
        if ($line =~ /^\s*$/) { next; }

		# if it is a data record, create a record
		elsif ($line =~ /^\s*\d+\s+\d+\s+/) 
		{
	    	my @data = split(' ',$line);
		    my $record = ClassRecord->new($WARN,$file);

        	# min $data[0]		# temp [5]
			# sec $data[1]		# RH [6]
			# ascent rate [2]	# dewpt [7]
			# height [3]		# wind dir [8]
			# pressure [4]		# wind spd [9]

	    	$record->setTime($data[0],$data[1]);
	   		$record->setPressure($data[4],"mb") unless($data[4] =~ /\/+/);
	   		$record->setTemperature($data[5],"C") unless($data[5] =~ /\/+/);
	   		$record->setDewPoint($data[7],"C") unless ($data[7] =~ /\/+/);
	   		$record->setRelativeHumidity($data[6]) unless ($data[6] =~ /\/+/);
	   		$record->setWindSpeed($data[9],"m/s") unless ($data[9] =~ /\/+/);
	  		$record->setWindDirection($data[8]) unless ($data[8] =~ /\/+/);
	   		$record->setAscensionRate($data[2],"m/s") unless ($data[2] =~ /\/+/);
    	
			if ($surfaceRecord)
			{
				$record->setLatitude($header->getLatitude(), 
				                     $self->buildLatLonFormat($header->getLatitude()));
				$record->setLongitude($header->getLongitude(), 
			    	                 $self->buildLatLonFormat($header->getLongitude()));
				$surfaceRecord = 0;
			}
			
			if ($data[3])
			{
				$record->setAltitude($data[3],"m") unless($data[3] =~ /\/+/);
			}

			printf($OUT $record->toString());
    	}

    	# if it is not a data line
		else 
		{
			last;
		}

    } #foreach

	} #if $header
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
	# HARD-CODED FILE NAME
    my @files = grep(/^sgp.*\.raw/,sort(readdir($RAW)));
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
