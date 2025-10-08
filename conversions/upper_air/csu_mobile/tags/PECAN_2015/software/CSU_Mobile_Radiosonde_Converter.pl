#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The CSU_Mobile_Radiosonde_Converter.pl script converts radiosonde
# and thermosonde data into the ESC format.  It merges txt PTU files
# and txt WND (wind) files into single records on keyed on altitude.</p>
#
# @author Linda Echo-Hawk, 19 Nov 2013
# @version MPEX CSU Mobile -- Created based on the T-PARC Gosan converter
#          - The raw data consists of EDT and *.tsv (containing lat/lon info)
#            data in two separate files per sounding. There were 55 EDT files 
#            and 54 *.tsv files, so the code must be able to work with a 
#            missing file.
#          - The converter looks for the data records on the next line 
#            after the column headers line of the EDT raw data file.
#          - The sounding (hash key) that each record belongs to is the 
#            date portion of the raw data file names.
#          - Since one sounding was missing a *.tsv file (where lat/lon 
#            info was stored), the header lat/lon data was used for the 
#            surface data record (only). The remainder of the data records 
#            for that sounding contained "missing" lat/lon values and the 
#            other soundings got the lat/lon info from the EDT file.
#
#
# @author Linda Echo-Hawk, 21 May 2010
# @version T-PARC_2008 Created for T-PARC based on the 
#            HachijoJima_Radiosonde_Converter.pl script for T-PARC. 
#          - Search for "HARD-CODED" to change project specific items
#          - The parseWindFile function reads the *.WND file into an
#            array, then passes a second array containing the first
#            14 lines of the wind file to the parseHeader function.
#            After the parseHeader function returns, the data from 
#            the wind file is read and stored in the ClassRecord
#            object.
#          - For T-PARC Gosan data, the sounding (hash key) that each 
#            record belongs to is the date portion of the raw data filenames. 
#          - The data records are read in from the wind and ptu files, and 
#            each has altitude data (height), so the records are merged
#            together based on the altitude.
#          - No lat/lon data is available except in the header, so this
#            was used for the surface data record (only) in the output   
#            file, per Scot's instructions.
#          - Code was added to calculate ascension rate and set the
#            ascent rate flag.
#          - Added code to get the nominal time from the raw data file name.
#
##Module------------------------------------------------------------------------
package CSU_Mobile_Radiosonde_Converter;
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
use DpgConversions;
use Station;
use ClassConstants qw(:DEFAULT);
use ClassHeader;
use ClassRecord;
use Data::Dumper;

my $debug = 0;
my $debugHeader = 0;

 printf "\nCSU_Mobile_Radiosonde_Converter.pl began on ";print scalar localtime;printf "\n";  
&main();
 printf "\nCSU_Mobile_Radiosonde_Converter.pl ended on ";print scalar localtime;printf "\n";

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the CSU_Mobile radiosonde/thermosonde data by converting it
# from the native ASCII format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = CSU_Mobile_Radiosonde_Converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature CSU_Mobile_Radiosonde_Converter new()
# <p>Create a new instance of a CSU_Mobile_Radiosonde_Converter.</p>
#
# @output $self A new CSU_Mobile_Radiosonde_Converter object.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);

    $self->{"stations"} = ElevatedStationMap->new();

    $self->{"PROJECT"} = "MPEX";
    $self->{"NETWORK"} = "CSU_Mobile";

    $self->{"FINAL_DIR"} = "../final";
    $self->{"OUTPUT_DIR"} = "../output";
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

    $station->setStationName("CSU_Mobile");
	$station->setLatLongAccuracy(3);
    $station->setStateCode("99");
    $station->setCountry("US");
    $station->setReportingFrequency("no set schedule");
    # Scot says that 72999 is not an Id number
    $station->setNetworkIdNumber("72999");
	# platform 415 	Radiosonde, Vaisala RS92-SGP
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
# @signature ClassRecord findRecord(FILE* WARN, String sounding, float alt, String file)
# <p>Find a record for the specified sounding and altitude.</p>
#
# @input $WARN The FileHandle where warnings are to be stored.
# @input $sounding The sounding (hash key) that the record belongs to. For T-PARC
#        CSU_Mobile data, the key is the date portion of the raw data filenames.
# @input $alt The altitude of the record.
# @input $file The file that the record is being generated from.
# @output $record The record in the hash.
##------------------------------------------------------------------------------
sub findRecord {
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
# @signature void generateOutputFile()
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
# @signature void parseHeader()
# <p>Create the headers for the class files from the information
# in the wind files.</p>
##------------------------------------------------------------------------------
sub parseHeader {
	my ($self,$file,@headerlines) = @_;

	printf("Processing header info file: %s\n",$file);
	my $sounding;

    if ($file =~ /EDT_(\d{6})_(\d{4})/)
	{
		my $date = ($1);
		my $time = ($2);
		$sounding = join ("", $date, $time);
		print "My EDT parseheader sounding = $sounding\n";
	}

    my $header = ClassHeader->new($self->{"WARN"});
    
    # HARD-CODED
    $header->setReleaseDirection("Ascending");
    # Set the type of sounding
    $header->setType("CSU Mobile Radiosonde");
    $header->setProject($self->{"PROJECT"});
	# The Id will be the prefix of the output file
    $header->setId("CSU_Mobile");
	# "Release Site Type/Site ID:" header line
	# originally used Site ID 72999, but this
	# was incorrect and Scot L. asked that it
	# be removed.
    # $header->setSite("CSU Mobile/72999");
    $header->setSite("CSU Mobile");

    my $index = 0;
    # Read the first lines of the file for additional header info
	foreach my $line (@headerlines) 
	{
        # Add the non-predefined header line to the header.
	    if ($line =~ /^RS/i)
	    {
	   	    chomp ($line);
	        # make an array of the characters on that line
			# remove the first two chars and rejoin
			my (@contents) = split(//,$line); 
            # removed two chars starting at position "0"
			splice (@contents, 0,2);
			my $newSondeId = join "", @contents;
			# print "Final result = $newSondeId\n";
            my $label = "Sonde Id/Sonde Type";
	   		# HARD-CODED
	  	    # Scot provided Radiosonde Type below
	   	    my $sondeType = "Vaisala RS-92 with GPS Windfinding";
	  		# Should be line 6 in output (line zero to n-1, so use 5)
	        # $header->setLine(5, trim($label).":",trim(join("/",@contents))); 
	        $header->setLine(5, trim($label).":",trim(join("/",$newSondeId, $sondeType))); 
	    }
        if ($line =~ /Lat/)
	    {
	   	    chomp ($line);
	  	    my (@act_releaseLoc) = (split(' ',$line));
	  	    my $lat = $act_releaseLoc[2];
	  	    my $lon = $act_releaseLoc[5];
            $header->setLatitude($lat,$self->buildLatlonFormat($lat));
	        $header->setLongitude($lon,$self->buildLatlonFormat($lon)); 
		}
					
		# -------------------------------------------------
		# Get release time from file name
		# EDT_130510_2333.txt
		# -------------------------------------------------
        # print "FILE NAME: $file\n";
		# my $fileInfo =~ /EDT_(\d{2})(\d{2})(\d{2})_(\d{2})(\d{2})/;
		if ($file =~ /EDT_(\d{2})(\d{2})(\d{2})_(\d{2})(\d{2})/)
		{
			my $date = sprintf("20%02d, %02d, %02d", $1, $2, $3);
			my $time = sprintf("%02d:%02d:00", $4, $5);      
			# print "NOM:  $date   NOM:  $time\n";
			$header->setNominalRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
			$header->setActualRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
		}
		
		$header->setLine(6,"Ground Station Software: ", "Digicora MW21 V3.64");
        $header->setLine(7,"Surface Data Source: ","Handheld weather station and/or most recent obs from nearest METAR station");
		
		if ($index == 9)
		{
            chomp ($line);
            my (@surfaceData) = (split(' ',$line));
			my $alt = $surfaceData[1];
			 print "EDT parseHeader ALT: $alt\n";
			$header->setAltitude($alt,"m");
		}
            
		$index++;
		# print "INDEX = $index\n";

	    
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
# @signature void parseEDTFile(FileHandle WARN, String file)
# <p>Parse the data values from the file and add them to the record at the
# record's altitude. Also, set the time and calculate the ascension rate.</p>
#
# NOTE: All data values except lat/lon are obtained from the EDT file.
#
# @input $WARN The FileHandle where warnings are to be stored.
# @input $file The name of the file to be parsed.
##------------------------------------------------------------------------------
sub parseEDTFile 
{
    my ($self,$WARN,$file) = @_;
	my $sounding;

    printf("\nProcessing EDT file: %s\n",$file);
    if ($file =~ /EDT_(\d{6})_(\d{4})/)
	{
		my $date = ($1);
		my $time = ($2);
		
		$sounding = join ("", $date, $time);
		# print "My EDT sounding = $sounding\n";
	}
	
    open(my $FILE,sprintf("%s/%s",$self->{"RAW_DIR"},$file)) or die("Can't read $file\n");
	my @lines = <$FILE>;
	close ($FILE);

    # Generate the sounding header from the info in the EDT_*.txt file.
	my $header = $self->parseHeader($file, @lines[0..9]);
	if (!defined($header))
	{
		printf ("WARNING:  Unable to generate header information for %s\n", $file);
	}
    
	my $prev_time = 9999.0;
    my $prev_alt = 99999.0;   

    my $lon;
	my $lat;
	my $surfaceData = 1;
    my $startData = 0;
    foreach my $line (@lines) 
	{
        chomp($line);
        # Skip any blank lines.
        next if ($line =~ /^\s*$/);

        my @data = split(' ',$line);
        
        # ----------------------------------------------------------------------
		# Look for the column headers
        #  Sec     mtrs       hPa     degC     degC    Pct      kts    deg
        # ----------------------------------------------------------------------
		if (trim($data[0]) eq "Sec")
		{
			$startData = 1;
			next;
		}
		if ($startData)
		{
            # -------------------------------------------------------
            # Add the data to the record for this altitude ($data[1])
            # -------------------------------------------------------
            my $record = $self->findRecord($WARN,$sounding,$data[1],$file);
		    $record->setTime($data[0]) unless($data[0] =~ /\/+/);
            $record->setPressure($data[2],"hPa") unless($data[2] =~ /\/+/);
            $record->setTemperature($data[3],"C") unless($data[3] =~ /\/+/);
            $record->setDewPoint($data[4],"C") unless($data[4] =~ /\/+/);        
            $record->setRelativeHumidity($data[5]) unless($data[5] =~ /\/+/);   

            # if the wind speed is not a "missing" value
            if ($data[6] !~ /\/+/)
			{
				my $windSpeed = $data[6];
				my $convertedWindSpeed = convertVelocity($windSpeed,"knot", "m/s");
				$record->setWindSpeed($convertedWindSpeed,"m/s");
			}
			$record->setWindDirection($data[7]) unless ($data[7] =~ /\/+/);

	        # -----------------------------------------------------
            # Data contains no lat/lon info, so use header lat/lon
		    # for surface data record only
	        # -----------------------------------------------------
		    if ($surfaceData == 1)
		    {
			    my $header = $self->{"soundings"}->{$sounding}->{"header"};  
                $record->setLatitude($header->getLatitude(), 
			                $self->buildLatlonFormat($header->getLatitude()));
                $record->setLongitude($header->getLongitude(),
                            $self->buildLatlonFormat($header->getLongitude()));

			    $surfaceData = 0;
			}

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
}
                                            

##------------------------------------------------------------------------------
# @signature void parseTSVFile(FileHandle WARN, String file)
# <p>Parse the TSV file for the lat/lon records in the file by altitude.</p>
#
# @input $WARN The file handle where warnings are to be stored.
# @input $file The name of the file to be parsed.
##------------------------------------------------------------------------------
sub parseTSVFile 
{
    my ($self,$WARN,$file) = @_;

    printf("Processing TSV file: %s\n",$file);
	my $sounding;
    if ($file =~ /72999_20(\d{10})/i)
    {
		$sounding = ($1);
	}

    open(my $FILE,sprintf("%s/%s",$self->{"RAW_DIR"},$file)) or die("Can't read $file\n");
	my @lines = <$FILE>;
	close ($FILE);

    my $lon;
	my $lat;
    my $index = 0;

	foreach my $line (@lines) 
	{
        # The data starts on line 40 of the file
		if ($index < 39) { $index++; next; }
		else
		{
			chomp($line);
			# Skip any blank lines.
			next if ($line =~ /^\s*$/);

			my @data = split(' ',$line);

			# print "TSV ALT: $data[6]\n";
            
			# -------------------------------------------------------
            # Add the data to the record for this altitude ($data[6])
            # -------------------------------------------------------
            my $record = $self->findRecord($WARN,$sounding,$data[6],$file);

			$record->setLatitude($data[15],$self->buildLatlonFormat($data[15])) unless($data[15] =~ /-32768/);
            $record->setLongitude($data[14],$self->buildLatlonFormat($data[14])) unless($data[14] =~ /-32768/);

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
    my ($self,$WARN) = @_;

    opendir(my $RAW,$self->{"RAW_DIR"}) or die("Can't read raw directory ".$self->{"RAW_DIR"});

    my @files = grep(/^72999/i,sort(readdir($RAW)));
    foreach my $tsvfile (@files) {
        $self->parseTSVFile($WARN,$tsvfile);
    }
    rewinddir($RAW);
    
    @files = grep(/^EDT/i,sort(readdir($RAW)));
    foreach my $edtfile (@files) {
        $self->parseEDTFile($WARN,$edtfile);
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
