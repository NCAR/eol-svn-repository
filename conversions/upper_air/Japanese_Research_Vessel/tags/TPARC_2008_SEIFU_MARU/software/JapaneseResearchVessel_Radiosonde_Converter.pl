#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The JapaneseResearchVessel_Radiosonde_Converter.pl script converts ASCII
# radiosonde data into the ESC format.  Raw data consists of *.AED files (data)
# and *.APA files (header information).</p>
#
# @author Linda Echo-Hawk
# @version T-PARC_2008 Created for T-PARC based on the 
#            HachijoJima_Radiosonde_Converter.pl for T-PARC.
#          - The converter reads the *.APA files to get the header info 
#            when loadHeaderInfo is called. It reads the *.AED to get the  
#            raw data records.
#          - For T-PARC Chofu Maru, the sounding (hash key) that each 
#            record belongs to is the date portion of the raw data filenames. 
#          - The data records are stored in a hash based on the  
#            altitude data (height) (hash key).
#          - No lat/lon data is available except in the header, so this
#            was used for the surface data record (only) in the output   
#            file, per Scot's instructions.
#          - Code was added to calculate ascension rate and set the
#            ascent rate flag.
#          - Added code to get nominal time from raw data file name.
#
##Module------------------------------------------------------------------------
package JapaneseResearchVessel_Radiosonde_Converter;
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

printf "\nJapaneseResearchVessel_Radiosonde_Converter.pl began on ";print scalar localtime;printf "\n\n";  
&main();
printf "\nJapaneseResearchVessel_Radiosonde_Converter.pl ended on ";print scalar localtime;printf "\n";

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the Japanese Research Vessel Chofu Maru radiosonde/thermosonde
# data by converting it from the native ASCII format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = JapaneseResearchVessel_Radiosonde_Converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature JapaneseResearchVessel_Radiosonde_Converter new()
# <p>Create a new instance of a JapaneseResearchVessel_Radiosonde_Converter.</p>
#
# @output $self A new JapaneseResearchVessel_Radiosonde_Converter object.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);

    $self->{"stations"} = ElevatedStationMap->new();

	# --------------------------------
    # HARD-CODED
    # --------------------------------
    $self->{"PROJECT"} = "T-PARC";
    $self->{"NETWORK"} = "ChofuMaru";
    # --------------------------------

    $self->{"FINAL_DIR"} = "../final";   
    $self->{"OUTPUT_DIR"} = "../output";
    $self->{"RAW_DIR"} = "../raw_data";

    $self->{"STATION_FILE"} = sprintf("%s/%s_%s_sounding_stationCD.out",
                                      $self->{"FINAL_DIR"},
                                      $self->cleanForFileName($self->{"NETWORK"}),
                                      $self->cleanForFileName($self->{"PROJECT"}));

    $self->{"SUMMARY"} = $self->{"OUTPUT_DIR"}."/station_summary.log";
    $self->{"WARN_LOG"} = $self->{"OUTPUT_DIR"}."/warning.log";

    return $self;
}


##------------------------------------------------------------------------------
# @signature Station buildDefaultStation(String station_id, String network)
# <p>Create a default station for the R/V Chofu Maru using the specified
# station_id and network.</p>
#
# @input $station_id The identifier of the station to be created.
# @input $network The network the station belongs to.
# @return The new station object with the default values for the network.
##------------------------------------------------------------------------------
sub buildDefaultStation {
    my ($self,$station_id,$network) = @_;

    # HARD-CODED
    my $station = Station->new("ChofuMaru",$self->{"NETWORK"});
    
	$station->setStationName("R/V Chofu Maru");
	$station->setLatLongAccuracy(3);
    # $station->setStateCode("99");
    $station->setCountry("Japan");
    $station->setReportingFrequency("12 hourly");
    $station->setNetworkIdNumber("99");
	# platform 87, Rawinsonde, Other
    $station->setPlatformIdNumber(87);
	$station->setMobilityFlag("m");
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

    $self->loadHeaderInfo();
    $self->readDataFiles($WARN);
    $self->generateOutputFiles();
    $self->printStationFiles();

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

        close($OUT);
   }                         
}


##------------------------------------------------------------------------------
# @signature void loadHeaderInfo()
# <p>Create the headers for the class files from the information
# in the wind files.</p>
##------------------------------------------------------------------------------
sub loadHeaderInfo {
	my ($self) = @_;

    opendir(my $RAW,$self->{"RAW_DIR"}) 
		or die("Can't read raw directory ".$self->{"RAW_DIR"});

    my @files = grep(/\d{8}\.APA$/i,sort(readdir($RAW)));
    foreach my $headerFile (@files)
	{
		printf("Processing header info file: %s\n",$headerFile);
        $headerFile =~ /(\d{8}\.APA)/i;

        open(my $FILE,sprintf("%s/%s",$self->{"RAW_DIR"},$headerFile)) 
			or die("Can't read $headerFile\n");

		my @lines = <$FILE>;
	    close ($FILE);         

		my $sounding = $headerFile;
      	$sounding =~ /(\d{8})/;
	    # $sounding = uc($1);        
	    $sounding = ($1);        
        # print "SOUNDING KEY: $sounding\n";

        my $header = ClassHeader->new($self->{"WARN"});
   
	    # ---------------------------------------------
        # HARD-CODED
		# ---------------------------------------------
        $header->setReleaseDirection("Ascending");
        # Set the type of sounding
        $header->setType("Chofu Maru Radiosonde (JMA)");
        $header->setProject($self->{"PROJECT"});
	    # The Id will be the prefix of the output file
        $header->setId("CHOFU_MARU");
	    # "Release Site Type/Site ID:" header line
        $header->setSite("R/V Chofu Maru/JCCX");

	    # ---------------------------------------------
        # File name shows nominal time
        # 08081600.APA
	    # ---------------------------------------------
	    $headerFile =~ /(\d{2})(\d{2})(\d{2})(\d{2})/;
        my $nomDate = sprintf("20%02d, %02d, %02d", $1, $2, $3);
        my $nomTime = sprintf("%02d:00:00", $4);
	    print "\tNOMINAL:  $nomDate    $nomTime\n";

        # Read the first lines of the file for additional header info
	    foreach my $line (@lines) 
	    {
            # Add the non-predefined header line to the header.
			# "Rs-number:             C0230792"
			if ($line =~ /Rs-number/i)
			{
			    chomp ($line);
			    my ($label,@contents) = split(/:/,$line); 
                $label = "Sonde Id";
                # $label = "Sonde Id/Sonde Type";
			    # need to add "PTU GPS" after sonde ID (@contents)
			  	# $contents[1] = "PTU GPS";
			    #$header->setLine(5, trim($label).":",trim(join("/",@contents))); 
			    $header->setLine(5, $label.":",trim(@contents)); 
		    }
			# "GC-corrections:       P   1.2 hPa, T  -0.2 C, U   0 %"
			if ($line =~ /GC-corrections/i)
			{
			  	chomp $line;
                my @values = split(' ', trim($line));

                #-------------------------------------------------------------
                # Convert "Pressure    : 1013.8 1013.6    0.2"     to
                # "Ground Check Pressure:    Ref 1013.8 Sonde 1013.6 Corr 0.2"
                #-------------------------------------------------------------
                my $GroundCheckPress = trim("Corr ".$values[2]);
                if ($debugHeader) {print "   Ground Check Pressure:: $GroundCheckPress\n";}
                $header->setLine(6,"Ground Check Pressure:    ", $GroundCheckPress);

                my $GroundCheckTemp = trim("Corr ".$values[5]);
                if ($debugHeader) {print "   Ground Check Temperature:: $GroundCheckTemp\n";}
                $header->setLine(7,"Ground Check Temperature: ", $GroundCheckTemp);

                my $GroundCheckHumidity = trim("Corr ".$values[8]);
                if ($debugHeader) {print "   Ground Check Humidity:: $GroundCheckHumidity\n";}
                $header->setLine(8,"Ground Check Humidity: ", $GroundCheckHumidity);
			}
			# "Station:              31.54 N 128.08 E    3 m from the sea level"
            if ($line =~ /Station/)
			{
			    chomp ($line);
			    my (@act_releaseLoc) = (split(' ',(split(/:/,$line))[1]));
			    my $lat = $act_releaseLoc[0];
			    my $lon = $act_releaseLoc[2];
                my $alt = $act_releaseLoc[4];
                print "\tLAT: $lat  LON: $lon  ALT: $alt\n";

                $header->setLatitude($lat,$self->buildLatlonFormat($lat));
	            $header->setLongitude($lon,$self->buildLatlonFormat($lon)); 
                $header->setAltitude($alt,"m");

                #-----------------------------------------------------------------
                # Set the station information
                #-----------------------------------------------------------------
                # HARD-CODED
                my $station = $self->{"stations"}->getStation("ChofuMaru",$self->{"NETWORK"}, 
	                                      $header->getLatitude(),$header->getLongitude(),
					    		          $header->getAltitude());
                # HARD-CODED
                if (!defined($station)) {
                    $station = $self->buildDefaultStation("ChofuMaru",$self->{"NETWORK"});

	                $station->setLatitude($header->getLatitude(),$self->buildLatlonFormat($header->getLatitude()));
	                $station->setLongitude($header->getLongitude(),$self->buildLatlonFormat($header->getLongitude()));
	                $station->setElevation($header->getAltitude(),"m");
		
                    $self->{"stations"}->addStation($station);
                }
                $station->insertDate($nomDate,"YYYY, MM, DD");     
			}

			# "Date:                 2008-08-15    Started at Gmt 23:30"
			if ($line =~ /Date:/)
			{
			  	chomp ($line);
			    my (@releaseTime) =  (split(' ',trim($line)));

			  	my ($year,$mon,$day) = (split('-',$releaseTime[1]));
                my $date = sprintf("%04d, %02d, %02d", $year, $mon, $day);

			  	my ($hour, $min) = (split(':',trim($releaseTime[5])));
			  	# print "HOURS: $hour   MIN: $min\n";
			   	my $time = sprintf("%02d:%02d:00", $hour, $min);

			    print "\tRELEASE:  $date    $time\n";
                $header->setActualRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);

                $header->setNominalRelease($nomDate,"YYYY, MM, DD",$nomTime,"HH:MM:SS",0);
	        }   
	    }
		# save this header to the hash for key = $sounding (date/time portion of filename)
        $self->{"soundings"}->{$sounding}->{"header"} = $header;    
	}
}


##------------------------------------------------------------------------------
# @signature void parseDataFile(FileHandle WARN, String file)
# <p>Parse the wind values for the records in the file by altitude.</p>
#
# @input $WARN The file handle where warnings are to be stored.
# @input $file The name of the file to be parsed.
##------------------------------------------------------------------------------
sub parseDataFile 
{
    my ($self,$WARN,$file) = @_;

    printf("Processing file: %s\n",$file);

    $file =~ /(\d{8}\.AED)/i;
	my $sounding = $file;
 	$sounding =~ /(\d{8})/;
	# $sounding = uc($1);   
	$sounding = ($1);   
    # print "SOUNDING KEY: $sounding\n";


    open(my $FILE,sprintf("%s/%s",$self->{"RAW_DIR"},$file)) or die("Can't read $file\n");
	my @lines = <$FILE>;
	close ($FILE);

    my $lon;
	my $lat;
	my $surfaceDataRecord = 1;
    my $prev_time = 9999.0;
    my $prev_alt = 99999.0;   
 
  
    foreach my $line (@lines) 
	{
		chomp($line);
		# Skip any blank lines.
		next if ($line =~ /^\s*$/);

		my @data = split(' ',$line);

        # CONFIRM MISSING VALUES
		# for Chofu Maru missing = //// or /////
		# create a $record object
		my $alt = $data[2];
		# print "ALT = $data[2]\n";
        
		my $record = $self->{"soundings"}->{$sounding}->{"records"}->{sprintf("%5.1f",$alt)};
        $record = ClassRecord->new($WARN,$file);
		
        $record->setAltitude($data[2],"m") unless($data[2] =~ /\/+/);
		$record->setTime($data[0],$data[1]) unless($data[0] =~ /\/+/);
        $record->setPressure($data[3],"hPa") unless($data[3] =~ /\/+/);
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
		
        $record->setWindSpeed($data[8],"m/s") unless ($data[8] =~ /\/+/);
        $record->setWindDirection($data[7]) unless ($data[7] =~ /\/+/);

	    # -----------------------------------------------------
        # Data contains no lat/lon info, so use header lat/lon
		# for surface data record only
	    # -----------------------------------------------------
		if ($surfaceDataRecord == 1)
		{
			my $header = $self->{"soundings"}->{$sounding}->{"header"};  

			$lon = $header->getLongitude();
		    $lat = $header->getLatitude();
            $record->setLatitude($lat,$self->buildLatlonFormat($lat));
            $record->setLongitude($lon,$self->buildLatlonFormat($lon));                            
			$surfaceDataRecord = 0;
		}

        $self->{"soundings"}->{$sounding}->{"records"}->{sprintf("%5.1f",$alt)} = $record; 
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

    open(my $SUMMARY, ">".$self->{"SUMMARY"}) || die("Cannot create the ".$self->{"SUMMARY"}." file.\n");
    print($SUMMARY $self->{"stations"}->getStationSummary());
    close($SUMMARY);   
}


##------------------------------------------------------------------------------
# @signature void readDataFiles()
# <p>Read in the files from the raw data directory and convert each into an
# ESC formatted file.</p>
##------------------------------------------------------------------------------
sub readDataFiles {
    my ($self,$WARN) = @_;

    opendir(my $RAW,$self->{"RAW_DIR"}) or die("Can't read raw directory ".$self->{"RAW_DIR"});

    my @files = grep(/\d{8}\.AED$/i,sort(readdir($RAW)));
    foreach my $datafile (@files) {
        $self->parseDataFile($WARN,$datafile);
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
