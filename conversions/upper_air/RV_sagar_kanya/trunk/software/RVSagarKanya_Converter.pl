#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The RVSagarKanya_Converter.pl script is used for converting high
# resolution radiosonde data from ASCII formatted data) to the EOL Sounding 
# Composite (ESC) format. The incoming format is Vaisala (Digicora 3).</p> 
#
#
# @author Linda Echo-Hawk 2012-04-13
# @version DYNAMO 2011 for R/V Sagar Kanya - 10 additional soundings
#    - The converter expects raw data file names of the form 
#      "VTJR_yyyymmddhhmm.tsv" where yyyy = year, mm = month, 
#      dd = day, hh=hour, mm = minute. VTJR is the call number 
#      for the Research Vessel Sagar Kanya.
#    - Renamed the raw data files.  The 10 soundings had names in
#      the form EDT_12_10_2011_1651IST_1S.tsv, where 1S indicates
#      latitude 1 South, and IST is Indian Standard Time and 
#      IST is UTC + 5:30 (e.g., EDT_12_10_2011_1651IST_1S.tsv 
#      is from 1121 UTC on 12 October).  I calculated UTC and
#      renamed the files so that the existing RVSagarKanya_Converter.pl
#      script would recognize the files and be able to process them.
#    - Revised the code to calculate nominal time to differentiate
#      by minutes.  The original R/V Sagar Kanya soundings did not 
#      fall "on the hour" and so the code merely looked at the
#      "hour" portion of the release time. The nominal time
#      breaks indicated by Scot are:
#      actual -> nominal
#      2201-0400 -> 00
#      0401-1000 -> 06
#      1001-1600 -> 12
#      1601-2200 -> 18
#      BEWARE: Code is specific to R/V Sagar Kanya timeframe (Sept-Oct 2011)
#
# @author Linda Echo-Hawk 2012-02-08
# @version DYNAMO 2011 for R/V Sagar Kanya
#    This code was created by modifying the WestTexasMesonet_Converter.pl script.
#    - Changed all references in code from VORTEX2 and West Tx Mesonet 
#      to be DYNAMO 2011 project and for Research Vessel Sagar Kanya.
#    - Updated code to use buildLatlonFormat() 
#    - Added code to calculate nominal time as requested by Scot.  BEWARE: Code
#      is specific to R/V Sagar Kanya timeframe (Sept-Oct 2011)
# This code makes the following assumptions:
#  - That the raw data file names shall be in the form
#        "VTJR_yyyymmddhhmm.tsv" where yyyy = year, mm = month, dd = day, hh=hour,
#         mm = minute. VTJR is the call number for the Research Vessel Sagar Kanya.
#  - That the raw data is in the Vaisala "Digicora 3" format. The file contains
#         header info on lines 1-39. Actual data starts on line 40. 
#
# @author Linda Echo-Hawk 2010-10-07
# @version VORTEX2_2010 updated the 2009 converter
#          - notes from 2009 apply
#
# @author Linda Echo-Hawk 2009-12-4
# @version VORTEX2_2009 Created from the T-PARC Minami_Daito_Jima Converter 
#           for the West Texas Mesonet soundings (only two soundings).
#          - Converter expects the actual data to begin
#            line 40 of the raw data file.  
#          - Header lat/lon/alt info is obtained from the data.  
#          - Release time is obtained from the file name.
#            BEWARE:  The raw data files were named EDT.tsv, and there is
#            a corresponding EDT_MWListHdrs.tsv file which contains the
#            date and time, as well as radiosonde type information.  The
#            raw data files were renamed to include the date and time, 
#            e.g. EDT200905151816.tsv, to avoid overwriting, as well as
#            to make that info available for release time header info.
#          - Code was added from the Ron Brown Converter to derive
#            the ascension rate.
#          - Search for "HARD-CODED" to find project-specific items that
#            may require changing.
##Module------------------------------------------------------------------------
package RVSagarKanya_Converter;
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

printf "\nRVSagarKanya_Converter.pl began on ";print scalar localtime;printf "\n";
my $debug = 0;
&main();
printf "\nRVSagarKanya_Converter.pl ended on ";print scalar localtime;printf "\n";

my $filename;

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the R/V Sagar Kanya radiosonde data by converting it from 
# the native ASCII format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = RVSagarKanya_Converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature RVSagarKanya_Converter new()
# <p>Create a new instance of a RVSagarKanya_Converter.</p>
#
# @output $self A new RVSagarKanya_Converter object.
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
    $self->{"NETWORK"} = "SAGAR_KANYA";
    
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
    # platform, 235, Ship
    $station->setPlatformIdNumber(235);
    $station->setMobilityFlag("m"); 
    return $station;
}

##------------------------------------------------------------------------------
# @signature String buildLatlonFormat(String value)
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
    my ($self,$file,@headerlines) = @_;
    my $header = ClassHeader->new();

    $filename = $file;
	# printf("parsing header for %s\n",$filename);
    $header->setReleaseDirection("Ascending");

    # Set the type of sounding
    $header->setType("R/V Sagar Kanya");
    $header->setProject($self->{"PROJECT"});
    
    # HARD-CODED
	# The Id will be the prefix of the output file
    $header->setId("RVSagarKanya");
	# "Release Site Type/Site ID:" header line
    $header->setSite("R/V Sagar Kanya/VTJR");


    # ------------------------------------------------
    # Read through the file for additional header info
    # ------------------------------------------------
 	my $index = 0;
	foreach my $line (@headerlines) 
	{
        # Add the non-predefined header lines to the header.
		if (($index > 0) && ($index < 11))
	    {
		    if ($line !~ /^\s*\/\s*$/) 
		    {
			    if ($line =~ /RS-Number/i)
			    {
					chomp ($line);
				    my ($label,@contents) = split(/:/,$line);
					$label = "Sonde Id/Sonde Type";
					$contents[1] = "Vaisala RS92-SGPD with GPS windfinding";
			        $header->setLine(5, trim($label).":",trim(join("/",@contents)));
					$header->setLine(6,"Ground Station Software:", "Digicora III/MW31/ver3.61 or older");
		        }
	        }
	    } 

	    # Ignore the header lines.
	    if ($index < 39) { $index++; next; }
        # Find the lat/lon for the release location in the actual data.
		else
		{
			my @data = split(' ',$line);
			if (($data[14] > -32768) & ($data[15] > -32768))
			{
				$header->setLongitude($data[14],$self->buildLatlonFormat($data[14]));
				$header->setLatitude($data[15],$self->buildLatlonFormat($data[15]));
            	$header->setAltitude($data[6],"m"); 
				last;
			}
		}
	}


    # ----------------------------------------------------------
    # Extract the date and time information from the file name
    # BEWARE: Expects filename to be similar to: EDT200905151816.tsv 
    # ----------------------------------------------------------
    # print "file name = $filename\n"; 

    my $date;
	my $time;
	my $hour;
    my $min;

	if ($filename =~ /(\d{4})(\d{2})(\d{2})(\d{2})(\d{2})/)
	{
		my ($yearInfo, $monthInfo, $dayInfo, $hourInfo, $minInfo) = ($1,$2,$3,$4,$5);

		$hour = $hourInfo;
		$min = $minInfo;
	    $date = join ", ", $yearInfo, $monthInfo, $dayInfo;
	    print "date: $date   ";
	    $time = join "", $hourInfo, ' ', $minInfo, ' 00';
        print "time: $time\n";
	}

    $header->setActualRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
    
	
	# -----------------------------------------------------
	# Code modified from HachijoJima for T-PARC 2008
	# Adjust for nominal release time per Scot L.
	# BEWARE:  This code specific to DYNAMO R/V Sagar Kanya
	# -----------------------------------------------------
	# Soundings are taken four times per day, and
	# nominal times will be 00, 06, 12 or 18 hours.
	# -----------------------------------------------------
    my $tempDate = $date;
    my @dateInfo = (split(',',$tempDate));
   	# print "$dateInfo[0]   $dateInfo[1]   $dateInfo[2]\n";
    my $nomYear = $dateInfo[0];
  	my $nomMonth = $dateInfo[1];
   	my $nomDay = $dateInfo[2];
    my $nomHour = 0;

   	if (($hour == 0) || ($hour == 1) || (($hour == 2) && ($min =~ /00/)))
    {    $nomHour = 0; }     

    elsif (($hour == 2) || ($hour == 3) || ($hour == 4) || 
	       (($hour == 5) && ($min =~ /00/))) 
    {	$nomHour = 3;  }

    elsif (($hour == 5) || ($hour == 6) || ($hour == 7) || 
	       (($hour == 8) && ($min =~ /00/))) 
    {	$nomHour = 6;  }

    elsif (($hour == 8) || ($hour == 9) || ($hour == 10) || 
	       (($hour == 11) && ($min =~ /00/))) 
    {	$nomHour = 9; }

    elsif (($hour ==11) || ($hour == 12) || ($hour == 13) || 
	       (($hour == 14) && ($min =~ /00/))) 
    {	$nomHour = 12; }

    elsif (($hour ==14) || ($hour == 15) || ($hour == 16) || 
	       (($hour == 17) && ($min =~ /00/))) 
    {	$nomHour = 15; }

    elsif (($hour ==17) || ($hour == 18) || ($hour == 19) || 
	       (($hour == 20) && ($min =~ /00/))) 
    {	$nomHour = 18; }

    elsif (($hour ==20) || ($hour == 21) || ($hour == 22) || 
	       (($hour == 23) && ($min =~ /00/))) 
    {	$nomHour = 21; }
    
	elsif (($hour == 23))
   	{
        $nomHour = 0;
   	    # Adjust the nominal date to the next day for 23 hours
   	    # And to the first day of next month (if last day of month)
   	    if(($nomDay == 30) && (($nomMonth == 9) || ($nomMonth == 11)))
   		{
   			$nomDay = 1;
   			$nomMonth++;
   		}
   	    elsif(($nomDay == 31) && ($nomMonth == 10))
   		{
   			$nomDay = 1;
   			$nomMonth++;
   		}
   		else
   		{
   			$nomDay++;
   		}
	}

   	# print "NOMHOUR: $nomHour\n";               
   	my $nomTime = sprintf("%02d:00:00", $nomHour);
    my $nomDate = sprintf("%04d, %02d, %02d", $nomYear,$nomMonth,$nomDay);
   	print "NOMINAL:  $nomDate    $nomTime\n";
    $header->setNominalRelease($nomDate,"YYYY, MM, DD",$nomTime,"HH:MM:SS",0);


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
	my @headerlines = @lines;
    my $header = $self->parseHeader($file,@headerlines);
    
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
	foreach my $line (@lines) {
	    # Ignore the header lines.
	    if ($index < 39) { $index++; next; }
	    
	    my @data = split(' ',$line);
	    my $record = ClassRecord->new($WARN,$file);

	    $record->setTime($data[0]);
	    $record->setPressure($data[7],"mb") if ($data[7] != -32768);
        # Temp and Dewpt are in Kelvin.  C = K - 273.15
	    $record->setTemperature(($data[2]-273.15),"C") if ($data[2] != -32768);    
		$record->setDewPoint(($data[8]-273.15),"C") if ($data[8] != -32768);
	    $record->setRelativeHumidity($data[3]) if ($data[3] != -32768);
	    $record->setUWindComponent($data[5],"m/s") if ($data[5] != -32768);
	    $record->setVWindComponent($data[4],"m/s") if ($data[4] != -32768);
	    $record->setWindSpeed($data[11],"m/s") if ($data[11] != -32768);
	    $record->setWindDirection($data[10]) if ($data[10] != -32768);

	    # get the lat/lon data 
	    if ($data[14] != -32768) {
		$record->setLongitude($data[14],$self->buildLatlonFormat($data[14]));
	    }
	    if ($data[15] != -32768) {
		$record->setLatitude($data[15],$self->buildLatlonFormat($data[15]));
	    }
        # Insert Ele (Elevation Angle) and Azi (Azimuth Angle) data
		# For setVariableValue(index, value):  
		# index (1) is Ele column, index (2) is Azi column.
		$record->setVariableValue(2, $data[12]) if ($data[12] != -32768);
	    $record->setAltitude($data[6],"m") if ($data[6] != -32768);
	                                          
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
        if ($debug) { my $time = $record->getTime(); my $alt = $record->getAltitude(); 
              print "\nNEXT Line: prev_time, rec Time, prev_alt, rec Alt:: $prev_time, $time, $prev_alt, $alt\n"; }

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
        if ($debug) { my $rectime = $record->getTime(); my $recalt = $record->getAltitude();
              print "Try SAVE Line: rec Time, rec Alt:: $rectime, $recalt\n"; }

        if ($record->getTime() != 9999 && $record->getAltitude() != 99999)
        {
            $prev_time = $record->getTime();
            $prev_alt = $record->getAltitude();

            if ($debug) { print "Current rec has valid Time and Alt. Save as previous.\n"; }
        }
        #-------------------------------------------------------
		# Completed the ascension rate data
        #-------------------------------------------------------

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
	# HARD-CODED FILE NAME
	# for original Sagar Kanya raw data files
    # my @files = grep(/^VTJR_\d{12}\.tsv/,sort(readdir($RAW)));
	# for 83E raw data files
    my @files = grep(/^VTJR_\d{12}/,sort(readdir($RAW)));
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
