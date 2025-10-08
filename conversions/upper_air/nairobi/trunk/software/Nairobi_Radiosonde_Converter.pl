#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The Nairobi_Radiosonde_Converter.pl script converts radiosonde
# and thermosonde data into the ESC format.  It merges txt PTU files
# and txt WND (wind) files into single records on keyed on altitude.</p>
#
# @author Linda Echo-Hawk, 17 Sept 2012
# @version DYNAMO_2011-12 Created for the Nairobi soundings based on
#             the T-PARC Gosan converter.
#           - Search for "HARD-CODED" to change project specific items
#           - The converter expects that the raw data files will be
#             named 11083122.PTU.TXT or 11083122.WND.TXT.  (The raw
#             data files were renamed to include the PTU or WND.)
#           - The sounding (hash key) that each record belongs to 
#             is the date portion of the raw data filenames. Because
#             one day had two soundings, there was a problem
#             using the 8 chars of the filenames as a unique key.  I 
#             appended "01" to each of the sounding keys except the
#             one exception which had "02" appended to the key.  This
#             allowed there to be a unqiue key for each sounding.
#           - The parseWindFile function reads the *.WND file into an
#             array, then passes a second array containing the first
#             14 lines of the wind file to the parseHeader function.
#             After the parseHeader function returns, the data from 
#             the wind file is read and stored in the ClassRecord
#             object.
#           - The data records are read in from the wind and ptu files, and 
#             each has altitude data (height), so the records are merged
#             together based on the altitude.
#           - No lat/lon data is available except in the header, so this
#             was used for the surface data record (only) in the output   
#             file, per Scot's instructions.
#           - Code was added to calculate ascension rate and set the
#             ascent rate flag.
#           - Added code to get the nominal time from the raw data file name.
#
##Module------------------------------------------------------------------------
package Nairobi_Radiosonde_Converter;
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

 printf "\nNairobi_Radiosonde_Converter.pl began on ";print scalar localtime;printf "\n";  
&main();
 printf "\nNairobi_Radiosonde_Converter.pl ended on ";print scalar localtime;printf "\n";

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the Nairobi radiosonde/thermosonde data by converting it
# from the native ASCII format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = Nairobi_Radiosonde_Converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature Nairobi_Radiosonde_Converter new()
# <p>Create a new instance of a Nairobi_Radiosonde_Converter.</p>
#
# @output $self A new Nairobi_Radiosonde_Converter object.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);

    $self->{"stations"} = ElevatedStationMap->new();

    $self->{"PROJECT"} = "DYNAMO";
    $self->{"NETWORK"} = "Nairobi";

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

    $station->setStationName("Nairobi, Kenya");
	$station->setLatLongAccuracy(3);
    $station->setStateCode("99");
    $station->setCountry("Kenya");
    $station->setReportingFrequency("12 hourly");
    $station->setNetworkIdNumber("99");
	# platform 415, Radiosonde, Vaisala RS92-SGP
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
#        Nairobi data, the key is the date portion of the raw data filenames.
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
 		print "KEY: $key\n";
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

	# printf("Processing header info file: %s\n",$file);
    
	my $sounding = 0;
    if ($file =~ /WND.TXT/i)
	{
		$sounding = $file;
 	    $sounding =~ /(\d{8})/;
	    if ($file =~ /(\d{8}).\d.WND.TXT/)
	    {
		    $sounding = $1."02";
	    }
	    else
	    {
		    $sounding = $1."01";
	    }
	}
    print "HDR KEY: $sounding\n";

    my $header = ClassHeader->new($self->{"WARN"});
    
    # HARD-CODED
    $header->setReleaseDirection("Ascending");
    # Set the type of sounding
    $header->setType("Nairobi Radiosonde");
    $header->setProject($self->{"PROJECT"});
	# The Id will be the prefix of the output file
    $header->setId("Nairobi");
	# "Release Site Type/Site ID:" header line
    $header->setSite("Nairobi, Kenya/63741");

    # Some raw data files have "Ground check omitted"
    my $groundCheck = 0;

    # Read the first lines of the file for additional header info
	foreach my $line (@headerlines) 
	{
        # Add the non-predefined header line to the header.
	    if ($line =~ /RS-Number/i)
	    {
	   	    chomp ($line);
	        my ($label,@contents) = split(/:/,$line); 
            $label = "Sonde Id/Sonde Type";
	  		# @contents[0] contains Sonde Id number
	   		# HARD-CODED
	  	    # Scot provided Radiosonde Type below
	   	    $contents[1] = "Vaisala RS92-SGPD with GPS Windfinding";
	  		# Should be line 6 in output (line zero to n-1, so use 5)
	        $header->setLine(5, trim($label).":",trim(join("/",@contents))); 
	    }
        if ($line =~ /Location/)
	    {
	   	    chomp ($line);
	  	    my (@act_releaseLoc) = (split(' ',(split(/:/,$line))[1]));
	  	    my $lat = $act_releaseLoc[0];
			if ($act_releaseLoc[1] eq "S")
			{
				$lat = "-".$lat;
			}
	  	    my $lon = $act_releaseLoc[2];
            my $alt = $act_releaseLoc[4];
            print "LAT: $lat  LON: $lon  ALT: $alt\n";
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
			# Get nominal time from file name
			# RS_WND_47185_2008100218_DIG2.txt
			# Soundings are taken twice per day, and
			# nominal times will be 06 or 18 hours.
			# -------------------------------------------------
            # print "FILE NAME: $file\n";
			# my @fileInfo = (split(/_/,$file));
			# $fileInfo[3] =~ /(\d{4})(\d{2})(\d{2})(\d{2})/;
			my $nomDate;
			my $nomTime; 

			if ($file =~ /(\d{2})(\d{2})(\d{2})(\d{2})/)
			{
				$nomDate = sprintf("20%02d, %02d, %02d", $1, $2, $3);
				$nomTime = sprintf("%02d:00:00", $4);
  
			    # print "NOM:  $nomDate   NOM:  $nomTime\n";
			}
	
    		# my $nomDate = sprintf("%04d, %02d, %02d", $1, $2, $3);
    		# my $nomTime = sprintf("%02d:00:00", $4);      
			# print "NOM:  $nomDate   NOM:  $nomTime\n";
            $header->setNominalRelease($nomDate,"YYYY, MM, DD",$nomTime,"HH:MM:SS",0);
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
            $header->setLine(7,"Ground Check Pressure:    ", $GroundCheckPress);
			$groundCheck = 1;
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
            $header->setLine(8,"Ground Check Temperature: ", $GroundCheckTemp);
			$groundCheck = 1;
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
            $header->setLine(9,"Ground Check Humidity: ", $GroundCheckHumidity);
			$groundCheck = 1;
		}
		if (!$groundCheck)
		{
			my $groundCheckValue = "omitted";
			$header->setLine(7,"Ground Check: ", $groundCheckValue);
		}
        # Add a header line for the ground station software (info from Scot)
		my $station_software = "Digicora II/MW15";
		$header->setLine(6,"Ground Station Software: ", $station_software);

	    
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
	my $sounding = 0;
    if ($file =~ /PTU.TXT$/i)
	{
		$sounding = $file;
		$sounding =~ /(\d{8})/;
		if ($file =~ /(\d{8}).\d.PTU.TXT/)
		{
			$sounding = $1."02";
		}
	    else
		{
			$sounding = $1."01";
		}
	}
	print "\tPTU KEY: $sounding\n";

    open(my $FILE,sprintf("%s/%s",$self->{"RAW_DIR"},$file)) or die("Can't read $file\n");
	my @lines = <$FILE>;
	close ($FILE);

    my $prev_time = 9999.0;
    my $prev_alt = 99999.0;   

    my $startData = 0;
    foreach my $line (@lines) 
	{
        chomp($line);
        # Skip any blank lines.
        next if ($line =~ /^\s*$/);

        my @data = split(' ',$line);
        
        # ----------------------------------------------------------------------
		# Look for the column headers
        #  min  s      hPa      gpm     deg C      %  deg C  Automatic  Operator
        # ----------------------------------------------------------------------
		if (trim($data[0]) eq "min")
		{
			$startData = 1;
			next;
		}
		if ($startData)
		{
            # -------------------------------------------------------
            # Add the data to the record for this altitude ($data[3])
            # -------------------------------------------------------
            my $record = $self->findRecord($WARN,$sounding,$data[3],$file);
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
	my $sounding = 0;
    if ($file =~ /WND.TXT/i)
	{
		$sounding = $file;
 	    $sounding =~ /(\d{8})/;
	    if ($file =~ /(\d{8}).\d.WND.TXT/)
	    {
		    $sounding = $1."02";
	    }
	    else
	    {
		    $sounding = $1."01";
	    }
	}
    print "\tWND KEY: $sounding   ";

    open(my $FILE,sprintf("%s/%s",$self->{"RAW_DIR"},$file)) or die("Can't read $file\n");
	my @lines = <$FILE>;
	close ($FILE);

    # Generate the sounding header from the info in the wind file.
	my $header = $self->parseHeader($file, @lines[0..21]);
	if (!defined($header))
	{
		printf ("WARNING:  Unable to generate header information for %s\n", $file);
	}

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
        #  min  s      hPa     gpm     m   m/s    deg  Automatic  Operator
        # ----------------------------------------------------------------------
        if (trim($data[0]) eq "min")
		{
			$startData = 1;
			next;
		}
		if ($startData)
		{
            # -------------------------------------------------------
            # Add the data to the record for this altitude ($data[3])
            # -------------------------------------------------------
            my $record = $self->findRecord($WARN,$sounding,$data[3],$file);
            $record->setWindSpeed($data[5],"m/s") unless ($data[5] =~ /\/+/);
            $record->setWindDirection($data[6]) unless ($data[6] =~ /\/+/);

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

    my @files = grep(/WND/i,sort(readdir($RAW)));
    foreach my $windfile (@files) {
        $self->parseWindFile($WARN,$windfile);
    }
    rewinddir($RAW);
    
    @files = grep(/PTU/i,sort(readdir($RAW)));
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
