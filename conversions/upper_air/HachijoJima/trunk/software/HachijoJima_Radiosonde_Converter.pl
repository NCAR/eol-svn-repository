#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The HachijoJima_Radiosonde_Converter.pl script converts radiosonde
# and thermosonde data into the ESC format.  It merges txt PTU files
# and txt WND (wind) files into single records on altitude.</p>
#
# @author Linda Echo-Hawk 
# @version T-PARC_2008 (May 2010) Revised the converter to be more
#            efficient.  
#          - The parseWindFile function now reads the *.WND file into
#            an array, then passes a second array containing the first
#            lines of the wind file to the parseHeader function. 
#            After the parseHeader function returns, the data from
#            the wind file is read and stored in the ClassRecord
#            object.  The WND file is no longer opened and read 
#            two separate times.
#          - Found and fixed a bug in the nominal time code which 
#            did not check for hour=0.  Fortunately, the default
#            was "0" so even though uninitialized value warnings
#            occurred before, the results were not incorrect.
#
# @author Linda Echo-Hawk
# @version T-PARC_2008 Created for T-PARC based on AFRLConverter.pl for T-REX. 
#          - The converter reads the *.WND files to get the header info 
#            when parseHeader is called. Reading this file twice  
#            could probably be eliminated if the code was refactored, but
#            as it is, the header info is created in the hash before the
#            call for readDataFiles.  This is because the original code 
#            read only one file to get header info, rather than all the
#            raw data files.
#          - For T-PARC Hachijo Jima, the sounding (hash key) that each 
#            record belongs to is the date portion of the raw data filenames. 
#          - The data records are read in from the wind and ptu files, and 
#            each has altitude data (height), so the records are merged
#            together based on the altitude.
#          - No lat/lon data is available except in the header, so this
#            was used for the surface data record (only) in the output   
#            file, per Scot's instructions.
#          - Code was added to calculate ascension rate and set the
#            ascent rate flag.
#          - Added code to determine the nominal release time.
#
##Module------------------------------------------------------------------------
package HachijoJima_Radiosonde_Converter;
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
use Data::Dumper;

my $debug = 0;
my $debugHeader = 0;

 printf "\nHachijoJima_Radiosonde_Converter.pl began on ";print scalar localtime;printf "\n";  
&main();
 printf "\nHachijoJima_Radiosonde_Converter.pl ended on ";print scalar localtime;printf "\n";

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the HachijoJima radiosonde/thermosonde data by converting it
# from the native ASCII format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = HachijoJima_Radiosonde_Converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature HachijoJima_Radiosonde_Converter new()
# <p>Create a new instance of a HachijoJima_Radiosonde_Converter.</p>
#
# @output $self A new HachijoJima_Radiosonde_Converter object.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);

    $self->{"stations"} = ElevatedStationMap->new();

    $self->{"PROJECT"} = "T-PARC";
    $self->{"NETWORK"} = "HachijoJima";

    $self->{"FINAL_DIR"} = "../new_final";
    $self->{"OUTPUT_DIR"} = "../new_output";
    $self->{"RAW_DIR"} = "../raw_data";

    $self->{"STATION_FILE"} = sprintf("%s/%s_%s_sounding_stationCD.out",
                                      $self->{"FINAL_DIR"},
                                      $self->cleanForFileName($self->{"NETWORK"}),
                                      $self->cleanForFileName($self->{"PROJECT"}));
    $self->{"WARN_LOG"} = $self->{"OUTPUT_DIR"}."/warning.log";

    return $self;
}


##------------------------------------------------------------------------------
# @signature Station buildDefaultStation(String station_id, String network)
# <p>Create a default station using the specified station_id and network.</p>
#
# @input $station_id The identifier of the station to be created.
# @input $network The network the station belongs to.
# @return The new station object with the default values for the network.
##------------------------------------------------------------------------------
sub buildDefaultStation {
    my ($self,$station_id,$network) = @_;
    my $station = Station->new($station_id,$network);

    $station->setStationName("Hachijo Jim, Japan");
	$station->setLatLongAccuracy(3);
    $station->setStateCode("99");
    $station->setCountry("Japan");
    $station->setReportingFrequency("6 hourly");
    $station->setNetworkIdNumber("47678");
	# platform 87, Rawinsonde, Other
    $station->setPlatformIdNumber(87);
    return $station;
}


##------------------------------------------------------------------------------
# @signature String build_latlong_format(String value)
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

    # Convert spaces to underscores
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

    open(my $WARN,">".$self->{"WARN_LOG"}) or die("Can't create ".$self->{"WARN_LOG"});

    # $self->parseHeader();
    $self->readDataFiles($WARN);
    $self->generateOutputFiles();
    $self->print_station_files();

    close($WARN);
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

    if ($month =~ /JANUARY/i) { return 1; }
    elsif ($month =~ /FEBRUARY/i) { return 2; }
    elsif ($month =~ /MARCH/i) { return 3; }
    elsif ($month =~ /APRIL/i) { return 4; }
    elsif ($month =~ /MAY/i) { return 5; }
    elsif ($month =~ /JUNE/i) { return 6; }
    elsif ($month =~ /JULY/i) { return 7; }
    elsif ($month =~ /AUGUST/i) { return 8; }
    elsif ($month =~ /SEPTEMBER/i) { return 9; }
    elsif ($month =~ /OCTOBER/i) { return 10; }
    elsif ($month =~ /NOVEMBER/i) { return 11; }
    elsif ($month =~ /DECEMBER/i) { return 12; }
    else { die("Unknown month: $month\n"); }
}

##------------------------------------------------------------------------------
# @signature ClassRecord find_record(FILE* WARN, String sounding, float alt, String file)
# <p>Find a record for the specified sounding and altitude.</p>
#
# @input $WARN The FileHandle where warnings are to be stored.
# @input $sounding The sounding (hash key) that the record belongs to. For T-PARC
#        Hachijo Jima the key is the date portion of the raw data filenames.
# @input $alt The altitude of the record.
# @input $file The file that the record is being generated from.
# @output $record The record in the hash.
##------------------------------------------------------------------------------
sub find_record {
    my ($self,$WARN,$sounding,$alt,$file) = @_;

    my $record = $self->{"soundings"}->{$sounding}->{"records"}->{sprintf("%5.1f",$alt)};
    if (!defined($record)) {
        $record = ClassRecord->new($WARN,$file);
        $record->setAltitude($alt,"m");

        $self->{"soundings"}->{$sounding}->{"records"}->{sprintf("%5.1f",$alt)} = $record;
	}
    return $record;
}

##------------------------------------------------------------------------------
# @signature void generateOutputFiles()
# <p>Create the class files from the information stored in the data hash.</p>
##------------------------------------------------------------------------------
sub generateOutputFiles 
{
    my ($self) = @_;

    foreach my $key (keys(%{ $self->{"soundings"}})) 
	{
 		# print "KEY: $key\n";
        my $header = $self->{"soundings"}->{$key}->{"header"};

        # print Dumper($self);

        open(my $OUT,sprintf(">%s/%s_%04d%02d%02d%02d%02d.cls",$self->{"OUTPUT_DIR"},
                             $header->getId(),split(/,/,$header->getActualDate()),
                             split(/:/,$header->getActualTime()))) or die("Can't open output file!\n");
        print($OUT $header->toString());

        foreach my $alt (sort {$a <=> $b} (keys(%{ $self->{"soundings"}->{$key}->{"records"}}))) {
             print($OUT $self->{"soundings"}->{$key}->{"records"}->{$alt}->toString());
        }
        # $self->get_station()->insertDate($header->getNominalDate(),"YYYY, MM, DD");

        close($OUT);
   }                         
}

##------------------------------------------------------------------------------
# @signature String get_station()
# <p>Get the station where all of the soundings were released from.</p>
##------------------------------------------------------------------------------
sub get_station {
    my ($self) = @_;

    my $station = $self->{"stations"}->getStation("HachijoJima",$self->{"NETWORK"});
    if (!defined($station)) {
        $station = Station->new("HachijoJima",$self->{"NETWORK"});
        $station->setStationName("Hachijo Jima, Japan");
        $station->setLatitude(33.0017,"DDDDDDD");
        $station->setLongitude(139.013,"DDDDDDD");
        $station->setElevation(79,"m");
        $station->setLatLongAccuracy(3);
        $station->setStateCode("99");
        $station->setReportingFrequency("no set schedule");
        $station->setNetworkIdNumber(47678);
		# platform 87, Rawinsonde, Other
        $station->setPlatformIdNumber(87);
 
        $self->{"stations"}->addStation($station);
    }
    return $station;
}


##------------------------------------------------------------------------------
# @signature void parseHeader()
# <p>Create the headers for the class files from the information
# in the wind files.</p>
##------------------------------------------------------------------------------
sub parseHeader {
	my ($self,$file,@headerlines) = @_;

	# printf("Processing header info file: %s\n",$file);
    $file =~ /(\d{8}wnd)/i;
	my $sounding = $file;
    $sounding =~ /(\d{8})/;
	$sounding = uc($1);        

    my $header = ClassHeader->new($self->{"WARN"});
    
    # HARD-CODED
    $header->setReleaseDirection("Ascending");
    # Set the type of sounding
    $header->setType("Hachijo Jima Radiosonde (JMA)");
    $header->setProject($self->{"PROJECT"});
	# The Id will be the prefix of the output file
    $header->setId("HACHIJO_JIMA");
	# "Release Site Type/Site ID:" header line
    $header->setSite("Hachijo Jima, Japan/47678");

    # Read the first lines of the file for additional header info
	foreach my $line (@headerlines) 
	{
        # Add the non-predefined header line to the header.
	    if ($line =~ /RS-Number/i)
	    {
	   	    chomp ($line);
	        my ($label,@contents) = split(/:/,$line); 
            $label = "Sonde Id/Sonde Type";
	  	    # need to add "PTU GPS" after sonde ID (@contents)
	 	    $contents[1] = "PTU GPS";
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
            if ($debugHeader) {print "   Ground Check Pressure:: $GroundCheckPress\n";}
            $header->setLine(6,"Ground Check Pressure:    ", $GroundCheckPress);
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
            if ($debugHeader) {print "   Ground Check Temperature:: $GroundCheckTemp\n";}
            $header->setLine(7,"Ground Check Temperature: ", $GroundCheckTemp);

  		}
   		if ($line =~ /Humidity/)
   		{
   			chomp ($line);
            my @values = split(' ', trim($line));

            #-----------------------------------------------------------
  		    # Convert "Humidity: 
   			# 
            #-----------------------------------------------------------
            my $GroundCheckHumidity = trim("Ref ". $values[2]." Sonde ".
                          $values[3]." Corr ".$values[4]);
            if ($debugHeader) {print "   Ground Check Humidity:: $GroundCheckHumidity\n";}
            $header->setLine(8,"Ground Check Humidity: ", $GroundCheckHumidity);

   		}
        if ($line =~ /Location/)
   		{
   		    chomp ($line);
   		    my (@act_releaseLoc) = (split(' ',(split(/:/,$line))[1]));
   		    my $lat = $act_releaseLoc[0];
   			my $lon = $act_releaseLoc[2];
            my $alt = $act_releaseLoc[4];
            # print "LAT: $lat  LON: $lon  ALT: $alt\n";

   	    	# my $lon_fmt = $lon < 0 ? "-" : "";
   			# while (length($lon_fmt) < length($lon)) { $lon_fmt .= "D"; }
   			# $header->setLongitude($lon,$lon_fmt);

  			# my $lat_fmt = $lat < 0 ? "-" : "";
   			# while (length($lat_fmt) < length($lat)) { $lat_fmt .= "D"; }
   			# $header->setLatitude($lat,$lat_fmt);
 

	        $header->setLatitude($lat,$self->buildLatlonFormat($lat));
	        $header->setLongitude($lon,$self->buildLatlonFormat($lon));

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
            my $date = sprintf("%04d, %02d, %02d", $releaseTime[4],
   			           $self->getMonth($releaseTime[3]),$releaseTime[2]);
   		    # print "DATE: $date   TIME: $time\n";
            $header->setActualRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
   			# -------------------------------------------------
   			# Adjust for nominal release time
   			# BEWARE:  This code specific to T-PARC 2008
   			# -------------------------------------------------
   			# Soundings are taken four times per day, and
   			# nominal times will be 00, 06, 12 or 18 hours.
   			# -------------------------------------------------
            my $tempDate = $date;
  			my @dateInfo = (split(',',$tempDate));
   			# print "$dateInfo[0]   $dateInfo[1]   $dateInfo[2]\n";
            my $nomYear = $dateInfo[0];
  			my $nomMonth = $dateInfo[1];
   			my $nomDay = $dateInfo[2];
            my $nomHour = 0;

   			if (($hour == 0) || ($hour == 1) || ($hour == 2))
            {    $nomHour = 0; }     
            elsif (($hour == 3) || ($hour == 4) || ($hour == 5) || 
   	             ($hour == 6) || ($hour == 7) || ($hour == 8)) 
            {	$nomHour = 6;  }
            elsif (($hour == 9) || ($hour == 10) || ($hour == 11) || 
                   ($hour == 12) || ($hour == 13) || ($hour == 14))
            {	$nomHour = 12; }
            elsif (($hour == 15) || ($hour == 16) || ($hour == 17) || 
                   ($hour == 18) || ($hour == 19) || ($hour == 20))
            {	$nomHour = 18; }
            elsif (($hour == 21) || ($hour == 22) || ($hour == 23))
   			{
                $nomHour = 0;
   			    # Adjust the nominal date to the next day for 21-23 hours
   			    # And to the next month (if day is 31 and month is August) or
   			    # (if day is 30 and month is September) for TPARC 2008 
   				if(($nomDay == 30) && ($nomMonth == 9))
   				{
   					$nomDay = 1;
   					$nomMonth = 10;
   				}
   				elsif (($nomDay == 31) && ($nomMonth == 8))
   				{
   					$nomDay = 1;
   					$nomMonth = 9;
   				}
   				else
   				{
   					$nomDay++;
   				}
   			}
	        # print "NOMHOUR: $nomHour\n";               
   			my $nomTime = sprintf("%02d:00:00", $nomHour);
            my $nomDate = sprintf("%04d, %02d, %02d", $nomYear,$nomMonth,$nomDay);
   			# print "NOM:  $nomDate   NOM:  $nomTime\n";
            $header->setNominalRelease($nomDate,"YYYY, MM, DD",$nomTime,"HH:MM:SS",0);
	    }
		# save this header to the hash for key = $sounding (date/time portion of filename)
        $self->{"soundings"}->{$sounding}->{"header"} = $header;    
	}

    #-----------------------------------------------------------------
    # Set the station information
    #-----------------------------------------------------------------
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
}


##------------------------------------------------------------------------------
# @signature void parsePtuFile(FileHandle WARN, String file)
# <p>Parse the non-wind values from the file and add them to the record at the
# record's altitude. Also, set the time and calculate the ascension rate.</p>
#
# @input $WARN The FileHandle where warnings are to be stored.
# @input $file The name of the file to be parsed.
##------------------------------------------------------------------------------
sub parsePtuFile 
{
    my ($self,$WARN,$file) = @_;

    printf("Processing file: %s\n",$file);
    $file =~ /(\d{8}ptu)/i;
    my $sounding = $file;
	$sounding =~ /(\d{8})/;
	$sounding = uc($1);

    open(my $FILE,sprintf("%s/%s",$self->{"RAW_DIR"},$file)) or die("Can't read $file\n");
	my @lines = <$FILE>;
	close ($FILE);

    my $prev_time = 9999.0;
    my $prev_alt = 99999.0;   


	my $index = 0;
    foreach my $line (@lines) 
	{
		# Ignore the header lines. 
		if ($index < 49) { $index++; next; }

        chomp($line);
        # Skip any blank lines.
        next if ($line =~ /^\s*$/);

        my @data = split(' ',$line);

        # -------------------------------------------------------
        # Add the data to the record for this altitude ($data[3])
        # -------------------------------------------------------
        my $record = $self->find_record($WARN,$sounding,$data[3],$file);
		$record->setTime($data[0],$data[1]) unless($data[0] =~ /\/+/);
        $record->setPressure($data[2],"hPa") unless($data[2] =~ /\/+/);
        $record->setTemperature($data[4],"C") unless($data[4] =~ /\/+/);
        $record->setRelativeHumidity($data[5]) unless($data[5] =~ /\/+/);        
        $record->setDewPoint($data[6],"C") unless($data[6] =~ /\/+/);        

        #-------------------------------------------------------
        # Calculate the ascension rate which is the difference
        # in altitudes divided by the change in time. Ascension
        # rates can be positive, zero, or negative. But the time
        # must always be increasing (the norm) and not missing.
        #-------------------------------------------------------
        if ($debug) { my $time = $record->getTime(); my $alt = $record->getAltitude(); 
              print "\nNEXT: prev_time: $prev_time, current Time: $time, prev_alt: $prev_alt, current Alt: $alt\n"; }

        if ($prev_time != 9999  && $record->getTime()     != 9999  &&
            $prev_alt  != 99999 && $record->getAltitude() != 99999 &&
            $prev_time != $record->getTime() ) 
        {
			$record->setAscensionRate( ($record->getAltitude() - $prev_alt) /
                                    ($record->getTime() - $prev_time),"m/s");
			$record->setAscensionRateFlag("99.0");

            if ($debug) { print "Calc Ascension Rate.\n"; }
        }

        # Save the next non-missing values. 
        # Ascension rates over spans of missing values are OK.
        if ($debug) { my $rectime = $record->getTime(); my $recalt = $record->getAltitude();
              print "Try SAVE Line: rec Time, rec Alt:: $rectime, $recalt\n"; }

        if ($record->getTime() != 9999 && $record->getAltitude() != 99999)
        {
             $prev_time = $record->getTime();
             $prev_alt = $record->getAltitude();

             if ($debug) { print "Current rec has valid Time and Alt. Save as previous.\n"; }
        }
        # End Calculate Ascension Rate
        #-------------------------------------------------------
    }
}
                                            

##------------------------------------------------------------------------------
# @signature void parseWindFile(FileHandle WARN, String file)
# <p>Parse the wind values for the records in the file by altitude.</p>
#
# @input $WARN The file handle where warnings are to be stored.
# @input $file The name of the file to be parsed.
##------------------------------------------------------------------------------
sub parseWindFile 
{
    my ($self,$WARN,$file) = @_;

    printf("Processing file: %s\n",$file);
    $file =~ /(\d{8}wnd)/i;
	my $sounding = $file;
 	$sounding =~ /(\d{8})/;
	$sounding = uc($1);   

    open(my $FILE,sprintf("%s/%s",$self->{"RAW_DIR"},$file)) or die("Can't read $file\n");
	my @lines = <$FILE>;
	close ($FILE);

    # Generate the sounding header from the info in the wind file.
	my $header = $self->parseHeader($file, @lines[0..22]);
	if (!defined($header))
	{
		printf ("WARNING:  Unable to generate header information for %s\n", $file);
	}

    my $lon;
	my $lat;
	my $surfaceData = 1;

	my $index = 0;
    foreach my $line (@lines) 
	{
 		# Ignore the header lines.
		if ($index < 49) { $index++; next; }
		
		chomp($line);
		# Skip any blank lines.
		next if ($line =~ /^\s*$/);

		my @data = split(' ',$line);

        # -------------------------------------------------------
        # Add the data to the record for this altitude ($data[3])
        # -------------------------------------------------------
        my $record = $self->find_record($WARN,$sounding,$data[3],$file);
        $record->setWindSpeed($data[5],"m/s") unless ($data[5] =~ /\/+/);
        $record->setWindDirection($data[6]) unless ($data[6] =~ /\/+/);

	    # -----------------------------------------------------
        # Data contains no lat/lon info, so use header lat/lon
		# for surface data record only
	    # -----------------------------------------------------
		if ($surfaceData == 1)
		{
			# my $header = $self->{"soundings"}->{$sounding}->{"header"};  
		    # $lon = $header->getLongitude();
		    # $lat = $header->getLatitude();
   	        # my $lon_fmt = $lon < 0 ? "-" : "";
  		    # while (length($lon_fmt) < length($lon)) { $lon_fmt .= "D"; }
   		    # $record->setLongitude($lon,$lon_fmt);
  		    # my $lat_fmt = $lat < 0 ? "-" : "";
   		    # while (length($lat_fmt) < length($lat)) { $lat_fmt .= "D"; }
   		    # $record->setLatitude($lat,$lat_fmt); 
            
            my $header = $self->{"soundings"}->{$sounding}->{"header"};  
            $record->setLatitude($header->getLatitude(), 
			            $self->buildLatlonFormat($header->getLatitude()));
            $record->setLongitude($header->getLongitude(),
                        $self->buildLatlonFormat($header->getLongitude()));  
			
			$surfaceData = 0;
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
# @signature void readDataFiles()
# <p>Read in the files from the raw data directory and convert each into an
# ESC formatted file.</p>
##------------------------------------------------------------------------------
sub readDataFiles {
    my ($self,$WARN) = @_;

    opendir(my $RAW,$self->{"RAW_DIR"}) or die("Can't read raw directory ".$self->{"RAW_DIR"});

    my @files = grep(/\d{8}wnd\.txt$/i,sort(readdir($RAW)));
    foreach my $windfile (@files) {
        $self->parseWindFile($WARN,$windfile);
    }
    rewinddir($RAW);
    
    @files = grep(/\d{8}ptu\.txt$/i,sort(readdir($RAW)));
    foreach my $ptufile (@files) {
        $self->parsePtuFile($WARN,$ptufile);
    }
    rewinddir($RAW);

    closedir($RAW);
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
