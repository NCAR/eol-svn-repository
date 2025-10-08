#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The Japanese_Radiosonde_Converter.pl script is used for converting 
# high resolution radiosonde data to the EOL Sounding Composite (ESC) format.
# These data are very challenging and many hard-coded solutions have been
# use, which means this converter may not be a good choice for re-use.
# NOTE:  The Naze version has additional code not contained in the 
# converter used for Ishigaki Jima data.  See the TAGGED version for
# the Naze converter.</p> 
#
#
# @author Linda Echo-Hawk 2011-09-21
# @version T-PARC_2008  Created for T-PARC_2008 Ishigaki Jima and Naze data
#          based on the MinamiDaitoJima_Radiosonde_Converter.pl.
#          - Search for HARD-CODED to find project specific values.
#          - Added @record_list array: All data records are read in and then
#            processed in reverse order to remove the descending data that 
#            was included at the end of the file (i.e., ascension
#            rate was negative).
#          - For time values t=1 through t=10, set all values except
#            time to missing.
#          - For any negative altitude, sets all values except time to 
#            "missing" (per Scot L.)
#          - Converter expects the actual data to begin on line 8 of the 
#            raw data file, but we want to ignore all records until the 
#            release point, which is the point where the counter (column 2)
#            resets to 0.  The code looks for this point and sets that line
#            to be the surface record.
#            NOTE:  Added code to skip the first zero for three files,
#            skip the second zero for another file (per Scot)  HARD-CODED
#          - The time column shows actual HH:MM:SS info.  I was not able to
#            use the counter (column 2) which increments by 2, to determine
#            the time because most files had duplicate counter numbers.  
#            Instead I converted the time for each record to seconds and
#            compared it with the previous record.  If there were gaps, a
#            record with all missing values was inserted so the timing was
#            consecutive for this "1 second" data.
#          - Code was added from the Ron Brown Converter to derive
#            the ascension rate.
#
#          - NOTE:  After processing, it was noted that the times in the raw
#            data were local times.  A second post-processing converter was
#            written to convert the time to UTC time, change the output file
#            name, and change the two header lines (UTC Release and Nominal
#            Relese).  See ConvertToUTC.pl for details.
#
# @use     Japanese_Radiosonde_Converter.pl >& results.txt
##Module------------------------------------------------------------------------
package Japanese_Radiosonde_Converter;
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

printf "\nJapanese_Radiosonde_Converter.pl began on ";print scalar localtime;printf "\n";
my $debug = 0;
my $debug_releaseLoc = 0;
my $debug_missingRecords = 0;

&main();
printf "\nJapanese_Radiosonde_Converter.pl ended on ";print scalar localtime;printf "\n";

my $filename;
my $sounding = "";

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the Japanese radiosonde data by converting it from 
# the native ASCII format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = Japanese_Radiosonde_Converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature Japanese_Radiosonde_Converter new()
# <p>Create a new instance of a Japanese_Radiosonde_Converter.</p>
#
# @output $self A new Japanese_Radiosonde_Converter object.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);
    
    $self->{"stations"} = ElevatedStationMap->new();

    # HARD-CODED
    $self->{"PROJECT"} = "T-PARC";
    $self->{"NETWORK"} = "Ishigaki_Jima";
    
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
# <p>Create a default station for the Ishigaki-Jima network using the 
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
    # $station->setStateCode("99");
    $station->setCountry("Japan");
    $station->setReportingFrequency("no set schedule");
    $station->setNetworkIdNumber("99");
    # platform, 87, Rawinsonde, Other 
    $station->setPlatformIdNumber(87);

    return $station;
}

##------------------------------------------------------------------------------
# @signature String buildLatlonFormat(String value)
# <p>Generate the decimal format for the specified value.</p>
#
# <p>format length must be the same as the value length or
# convertLatLong will complain (see example below)<br />
# base lat = 36.6100006103516 base lon = -97.4899978637695<br />
# Lat format = DDDDDDDDDDDDDDDD  Lon format = -DDDDDDDDDDDDDDDD</p>
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

    # Set the sounding type for first header
    # line: "Data Type:  Ishigaki Jima/Ascending"
    $header->setType("Ishigaki Jima");
    $header->setReleaseDirection("Ascending");
    $header->setProject($self->{"PROJECT"});
    
    # HARD-CODED
	# The Id will be the prefix of the output file
    $header->setId("ISHIGAKI_JIMA");
	# This info goes in the 
	# "Release Site Type/Site ID:" header line
	# For site # see: www.radiometrics.com/ishihara.pdf  
    $header->setSite("Ishigaki Jima, Japan/47918");

    # Read through the file for additional header info
	foreach my $line (@headerlines) 
	{
		if ($line =~ /RS-01GM/)
		{
			chomp($line);

	        my @headerInfo = split(',', $headerlines[0]);
		    
			
		    # Set the radiosonde id header line info
			my @contents;
			my $label = "Sonde Id/Sonde Type";
			$contents[0] = $headerInfo[1];
		    $contents[1] = "Meisei RS-01GM";
		    $header->setLine(5, trim($label).":",trim(join("/",@contents))); 

            # Set the release location info
			my $lat = trim($headerInfo[9]);
			my $lon = trim($headerInfo[10]);
			$lat =~ s/\+//g;
			$lon =~ s/\+//g;
			if ($debug_releaseLoc)
			{
				print "\tHEADER LAT:  $lat  HEADER LON:  $lon\n";
			}
			$header->setLatitude($lat,$self->buildLatlonFormat($lat));
            $header->setLongitude($lon,$self->buildLatlonFormat($lon));
            $header->setAltitude($headerInfo[12],"m"); 
    
	        # Set the release date and time info
	        my $date = $headerInfo[4];
			if ($date =~ /(\d{4})\/(\d{2})\/(\d{2})/)
	        {
				my ($year, $month, $day) = ($1,$2,$3);
	            $date = join ", ", $year, $month, $day;
			}
			else
			{
				print "No date found in header info\n";
			}
	        
			my $time = $headerInfo[5];

            $header->setActualRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
            $header->setNominalRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
		}
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
	my @headerlines = @lines[0..1];
    my $header = $self->parseHeader($file,@headerlines);
    
    # Only continue processing the file if a header was created.
    if (!defined($header))
	{
		print "WARNING: Unable to create header\n";
	}

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
	my ($hour, $min, $sec) = split (/:/, $header->getActualTime());

	my $outfile = sprintf("%s_%04d%02d%02d%02d%02d.cls", 
  							   $header->getId(),
							   split(/,/,$header->getActualDate()),
							   $hour, $min);
 
    printf("\tOutput file name is %s\n", $outfile);


	open(my $OUT,">".$self->{"OUTPUT_DIR"}."/".$outfile)
	    or die("Can't open output file for $file\n");

	print($OUT $header->toString());
	
    
	# --------------------------------------------
    # Create an array to hold all of the data records.
	# This is required so additional processing can take
    # place to remove descending data records at the
	# end of the data files
	# --------------------------------------------
	my @record_list = ();
	# --------------------------------------------
	
	# ----------------------------------------
    # Needed for code to derive ascension rate
    # ----------------------------------------
    my $prev_time = 9999.0;
    my $prev_alt = 99999.0;

    # ----------------------------------------------------
    # Three files should start processing
    # on the second zero time; one file 
	# should start on the third zero time
	# HARD-CODED (per Scot L.)
    # ----------------------------------------------------
	my $skip_first_zero = 0;
	if (($file =~ /F2008090420S7800324.CSV/) ||
		($file =~ /F2008091008S7800344.CSV/) || 
		($file =~ /F2008092108S7800405.CSV/))
	{
		$skip_first_zero = 1;
		print "FILE SKIPS FIRST ZERO: $file\n";
	}
	my $skip_second_zero = 0;
	if ($file =~ /F2008080308S7712086.CSV/)
	{
		$skip_second_zero = 1;
		print "FILE SKIPS SECOND ZERO: $file\n";
	}
    # ----------------------------------------------------
    # Parse the data portion of the input file
    # ----------------------------------------------------
	my $index = 0;
	my $realData = 0;
	my $recordTime = 0;
	my $prev_seconds = 0;
	foreach my $line (@lines) 
	{
	    # Ignore the header lines.
	    if ($index < 8) { $index++; next; }
	    
		chomp($line);
	    my @data = split(',',$line);
		# ------------------------------------------------
	    # Ignore the data lines before the counter in
		# column 2 ($data[1]) resets to 0.  The record
        # when the counter=0 is the release surface record.
		# ------------------------------------------------
        		
		# --------------------------------------------------------        
	    # For Ishigaki Jima
		# Handle the four files which have more than one zero time
        # --------------------------------------------------------        
		if ((!$realData) && ($data[1] == 0))
		{ 
		    if ((!$skip_first_zero) && (!$skip_second_zero))
		    {
		        $realData = 1;
		    }
            elsif ($skip_first_zero)
			{
			    $skip_first_zero = 0;
				print "\t$file skips zero at $data[0]\n";
			}
            elsif ($skip_second_zero)
			{
				$skip_first_zero = 1;
				$skip_second_zero = 0;
				print "\t$file skips zero at $data[0]\n";
			}
		}
		
		if ($realData)
		{
			my $record = ClassRecord->new($WARN,$file);
		
            # ---------------------------------------------------
			# TIME GAP CODE
            # Raw data time is shown as HH:MM:SS (e.g., 08:30:52)
			# so we set the surface record to 0.0 seconds and
            # increment the time by 1 for each record. Because
            # there are >1 second gaps between some of the data
		    # records, convert the time to seconds and compare
		    # with the previous record to see if "missing"
			# records need to be inserted to fill the gaps.
            # ---------------------------------------------------
            my @timeinfo = split(":",$data[0]);
			my $seconds = (($timeinfo[0]*3600) + ($timeinfo[1]*60) + ($timeinfo[2]));

            if (($prev_seconds != 0) && (($seconds-$prev_seconds) > 1))  
			{
				if ($debug_missingRecords)
				{
					print "WARNING: Gap at time t=$recordTime  " .
					      "@timeinfo  SEC=$seconds PREV=$prev_seconds\n";
				}
				my $missing = ($seconds-$prev_seconds);
				$missing--; # 0 to n-1
				if ($debug_missingRecords)
				{
				    if ($missing > 1)
				    {
				    	print "MISSING > 1 (value is $missing)\n";
				    }
				}
				my $insertTime = $recordTime;
				for (my $i=0; $i<$missing; $i++)
				{
			        my $missing_record = ClassRecord->new($WARN,$file);
				 	$missing_record->setTime($insertTime);
					# --------------------------------------------
					# add each record to the record_list array
					# for further processing to remove descending
					# data before calling print toString
					# --------------------------------------------
					push(@record_list, $missing_record);
					if ($debug_missingRecords)
					{
						print "Pushed Record $insertTime  PREV=$prev_seconds\n";
					
					    if ($insertTime != $recordTime)
					    {
						    print "There is a diff! INSERT vs RECORD TIME\n";
					    }
					}

					$prev_seconds++;
					$recordTime++;
					$insertTime++;
				}
				if ($debug_missingRecords)
				{
					print "\tCompleted loop - Ready for " .
                          "Record $recordTime SEC=$seconds " .
						  "PREV=$prev_seconds\n\n";
				}
			}
            if ($debug_missingRecords)
			{
				print "Completed for loop for missing\n";
                print "\tRecord $recordTime SEC=$seconds PREV=$prev_seconds\n";
			}

            # -------------------------------------------------
			# TIME GAP CODE is completed.
			# Now start reading the data records.
            # -------------------------------------------------

            $record->setTime($recordTime);

			# Increment time for next data record
			$recordTime++;
			$prev_seconds = $seconds;
            
			# ---------------------------------------------------
			# We want to print the time zero data record as
			# is, then make times 1-10 "missing" values.  
			# (Per Scot: "the radiosonde is being acclimatized
		    # during this period and the data are not usable.") 
            # ---------------------------------------------------
			if (($recordTime > 1) && ($recordTime < 12))
			{
				# --------------------------------------------
				# add each record to the record_list array
				# for further processing to remove descending
				# data before calling print toString
				# --------------------------------------------
				push(@record_list, $record);
  
			}
            # ---------------------------------------------------
			# If the record has a negative altitude, set all
			# values except time to "missing" (per Scot L.).
			# NOTE:  This should not occur, the only files with
			# these values were moved to /unprocess_raw_data.
            # ---------------------------------------------------
			elsif ($data[11] < 0)
			{
				# print "\t Negative altitude in $file at @timeinfo\n";
				# --------------------------------------------
				# add each record to the record_list array
				# for further processing to remove descending
				# data before calling print toString
				# --------------------------------------------
				push(@record_list, $record);

			    # print "found negative altitude\n";
                # print "\tRecord $recordTime SEC=$seconds PREV=$prev_seconds\n";

			}
			else
			{
	            $record->setPressure($data[20],"mb") unless ($data[20] =~ /-+$/);
	            $record->setTemperature($data[21],"C") unless ($data[21] =~ /-+$/);    
	            $record->setRelativeHumidity($data[22]) unless ($data[22] =~ /-+$/);

	            $record->setWindSpeed($data[10],"m/s") unless ($data[10] =~ /-+$/);
	            $record->setWindDirection($data[9]) unless ($data[9] =~ /-+$/);
		        $record->setLatitude(trim($data[17]), $self->buildLatlonFormat(trim($data[17]))) 
			        					unless ($data[17] =~ /-+$/); 
		        $record->setLongitude(trim($data[18]),$self->buildLatlonFormat(trim($data[18]))) 
			             				unless ($data[18] =~ /-+$/);
	            $record->setAltitude($data[11],"m") unless ($data[11] =~ /-+$/);
			
	    
                #-------------------------------------------------------
                # this code from Ron Brown converter:
                # Calculate the ascension rate which is the difference
                # in altitudes divided by the change in time. Ascension
                # rates can be positive, zero, or negative. But the time
                # must always be increasing (the norm) and not missing.
                #
                # Only save the next non-missing values.
                # Ascension rates over spans of missing values are OK.
                #-------------------------------------------------------
                if ($debug) 
			    { 
				    my $time = $record->getTime(); 
				    my $alt = $record->getAltitude(); 
                    print "\nCurrent Line: Time $time, prev_time $prev_time, " .
					      "Alt $alt, prev_alt $prev_alt\n"; 
			    }

                if ($prev_time != 9999  && $record->getTime()     != 9999  &&
                    $prev_alt  != 99999 && $record->getAltitude() != 99999 &&
                    $prev_time != $record->getTime() ) 
                { 
                    $record->setAscensionRate( ($record->getAltitude() - $prev_alt) /
                                         ($record->getTime() - $prev_time),"m/s");

                    if ($debug) { print "Ascension Rate calculated and set.\n"; }
                }


                if ($record->getTime() != 9999 && $record->getAltitude() != 99999)
                {
                    $prev_time = $record->getTime();
                    $prev_alt = $record->getAltitude();

                    if ($debug) { print "Saved Current time/alt as Previous.\n"; }
                }
                #-------------------------------------------------------
		        # Completed the ascension rate data
                #-------------------------------------------------------

				# --------------------------------------------
				# add each record to the record_list array
				# for further processing to remove descending
				# data before calling print toString
				# --------------------------------------------
				push(@record_list, $record);
			}
        
		} # end if real data

    } # end for each line

	# --------------------------------------------------
	# Remove the last records in the file that are 
    # descending (ascent rate is negative or zero)
	# --------------------------------------------------
	foreach my $record (reverse(@record_list))
	{
	    if (($record->getAscensionRate() <= 0.0) ||
			($record->getAscensionRate() == 999.0))
		{
			undef($record);
		} 
		else 
		{
			last;
		}
	}
    
	#-------------------------------------------------------------
    # Print the records to the file.
	#-------------------------------------------------------------
	foreach my $record(@record_list) 
	{
		print ($OUT $record->toString()) if (defined($record));
	}	
	
    close($OUT);

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
    my @files = grep(/\.csv$/i,sort(readdir($RAW)));
    closedir($RAW);
    
    
    open($WARN,">".$self->{"WARN_LOG"}) or die("Can't create ".$self->{"WARN_LOG"});
    
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
