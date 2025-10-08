#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The Japanese_Radiosonde_Converter.pl script is used for converting 
# high resolution radiosonde data to the EOL Sounding Composite (ESC) format.
# These data are very challenging and many hard-coded solutions have been
# use, which means this converter may not be a good choice for re-use.
# NOTE:  The Naze converter contains additional code not needed for
# Ishigaki Jima.  See the TAGGED version for the Ishigaki Jima converter.</p> 
#
#
# @author Linda Echo-Hawk 2011-09-21
# @version T-PARC_2008  Created for T-PARC_2008 Ishigaki Jima and Naze data
#          based on the MinamiDaitoJima_Radiosonde_Converter.pl. SEE LAST
#          TWO NOTES BELOW FOR IMPORTANT INFORMATION.
#          - Search for HARD-CODED to find project specific values.
#          - Added @record_list array: All data records are read in and then
#            processed in reverse order to remove the descending data that 
#            was included at the end of the file (i.e., ascension
#            rate was negative).
#          - For time values t=1 through t=5, set all values except
#            time to missing. (NOTE: This is different from Ishigaki Jima)
#          - For any negative altitude, sets all values except time to 
#            "missing" (per Scot L.)
#          - Converter expects the actual data to begin on line 8 of the 
#            raw data file, but we want to ignore all records until the 
#            release point, which is the point where the counter (column 2)
#            resets to 0.  The code looks for this point and sets that line
#            to be the surface record.
#            NOTE:  Added code to skip the first zero for two files
#            (per Scot)  HARD-CODED
#          - For surface records with missing lat/lon/alt data, header
#            info is substituted in. (Added since Ishigaki Jima)
#          - The time column shows actual HH:MM:SS info.  I was not able to
#            use the counter (column 2) which increments by 2, to determine
#            the time because most files had duplicate counter numbers.  
#            Instead I converted the time for each record to seconds and
#            compared it with the previous record.  If there were gaps, a
#            record with all missing values was inserted so the timing was
#            consecutive for this "1 second" data.
#          - Code was added from the Ron Brown Converter to derive
#            the ascension rate.
#          - NOTE:  After reviewing the initial processing run, Scot L. asked
#            me to make the following revisions:
#            Leave the surface record as is, and set the first 5 seconds to
#            missing. For the next 50 seconds (t=6 - t=55), if the ascent rate is
#            less than 0 or greater than 10, set that line and all the previous
#            lines to missing. If ascent rate is still out of bounds at t=55,
#            continue checking until the first good ascent rate is found, then
#            stop checking. NOTE: These changes were not made for Ishigaki Jima.
#
#          - NOTE:  After processing, it was noted that the times in the raw
#            data were local times.  A second post-processing converter was
#            written to convert the time to UTC time, change the output file
#            name, and change the two header lines (UTC Release and Nominal
#            Relese).  See ConvertToUTC.pl for details.
#
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
my $debug_bad_asc = 1;

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
    $self->{"NETWORK"} = "Naze";
    
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
    # line: "Data Type:  Naze/Ascending"
    $header->setType("Naze");
    $header->setReleaseDirection("Ascending");
    $header->setProject($self->{"PROJECT"});
    
    # HARD-CODED
	# The Id will be the prefix of the output file
    $header->setId("NAZE");
	# This info goes in the 
	# "Release Site Type/Site ID:" header line
	# For site # see: www.radiometrics.com/ishihara.pdf  
    $header->setSite("Naze, Japan/47909");

    # Read through the file for additional header info
	foreach my $line (@headerlines) 
	{
		# if ($line =~ /RS-01GM/)  Ishigaki Jima
		if ($line =~ /RS-01G/)
		{
			chomp($line);

	        my @headerInfo = split(',', $headerlines[0]);
		    
			
		    # Set the radiosonde id header line info
			my @contents;
			my $label = "Sonde Id/Sonde Type";
			$contents[0] = $headerInfo[1];
		    $contents[1] = join(" ", "Meisei", $headerInfo[0]);
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
	        my $date = trim($headerInfo[4]);
			if ($date =~ /(\d{4})\/(\d{2})\/(\d{2})/)
	        {
				my ($year, $month, $day) = ($1,$2,$3);
	            $date = join ", ", $year, $month, $day;
			}
			else
			{
				print "WARNING: No date found in header info\n";
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

    # print "FROM HEADER TIME:  $hour $min $sec\n";

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
    # For Naze, two files should start processing
    # on the second zero time (per Scot L.) HARD-CODED 
    # ----------------------------------------------------
	my $skip_first_zero = 0;
	if (($file =~ /f2008091814s7800946.csv/) ||
		($file =~ /f2008092209s7800047.csv/))
	{
		$skip_first_zero = 1;
		print "FILE SKIPS FIRST ZERO: $file\n";
	}
	my $skip_second_zero = 0;
    # -------------------------------------------
    # No files for Naze were requested to skip
    # the second zero reset
    # -------------------------------------------
	# if ($file =~ /F2008080308S7712086.CSV/)
	# {
	# 	$skip_second_zero = 1;
	# 	print "FILE SKIPS SECOND ZERO: $file\n";
	# }
    # ----------------------------------------------------
    # Parse the data portion of the input file
    # ----------------------------------------------------
	my $index = 0;
	my $realData = 0;
	# after the first record change this to zero
	my $surfaceRecord = 1;
	my $lat;
	my $lon;
	my $alt;
	my $recordTime = 0;
	my $prev_seconds = 0;
	# -----------------------------------
	# variables for ascension rate checks
	my $last_bad_time = 0;
	my $keep_checking = 0;
	# -----------------------------------
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
	    # For Naze
		# Handle the two files which have more than one zero time
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
			# ----------------------------------------------
			# need a new variable for ascension rate checks
		    # since $recordTime will be incremented
            my $time_check = $recordTime;
			# ----------------------------------------------
			# Increment time for next data record
			$recordTime++;
			$prev_seconds = $seconds;
            
			# ---------------------------------------------------
			# We want to print the time zero data record as
			# is, then make times 1-5 "missing" values.  
			# (Per Scot: "the radiosonde is being acclimatized
		    # during this period and the data are not usable.").
		    # NOTE: $recordTime has been implemented, so add 1 
			# to the value you are checking for.
            # ---------------------------------------------------
			if (($recordTime > 1) && ($recordTime < 7))
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


			    $lat = trim($data[17]);
				$lon = trim($data[18]);
				$alt = trim($data[11]);
				# ---------------------------------------------
				# if the surface record is missing lat/lon/alt 
				# values, set to value in header
				# ---------------------------------------------
				if ($surfaceRecord)
				{
					if ($lat =~ /-+$/) { $lat = $header->getLatitude(); }
					if ($lon =~ /-+$/) { $lon = $header->getLongitude(); }
					if ($alt =~ /-+$/) { $alt = $header->getAltitude(); }
					$surfaceRecord = 0;
                    # set the flag to start checking ascension rates
					$keep_checking = 1;
				}
				$record->setLatitude($lat, $self->buildLatlonFormat($lat)) unless ($lat =~ /-+$/); 
		        $record->setLongitude($lon,$self->buildLatlonFormat($lon)) unless ($lon =~ /-+$/);
	            $record->setAltitude($alt,"m") unless ($alt =~ /-+$/);
	    
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
		        # Check for bad ascension rate data
                #-------------------------------------------------------
				if ($keep_checking)
				{
					my $asc_rate = $record->getAscensionRate();
					if ((($asc_rate < 0.0000) || ($asc_rate > 10)) && 
						($asc_rate != 999))
					{
						$last_bad_time = $time_check;
						if ($debug_bad_asc)
						{
							# print "BAD ASCENT RATE at TIME = $last_bad_time  ASC = $asc_rate\n";
							printf "BAD ASCENT RATE at TIME = %s  ASC = %5.1f \n", $last_bad_time,$asc_rate;
							if ($last_bad_time == 55)
							{
								print "\tBAD ASCENT RATE at TIME = $time_check, ";
								print "KEEP CHECKING = $keep_checking, ";
								print "LBT = $last_bad_time\n";
							}
						}
					}
					elsif ($time_check > $last_bad_time)
					{
					    # if you find a good ascent rate 
						# at or past time = 55, stop checking
                        if ($time_check >= 55)
						{
							$keep_checking = 0;
						}

						if ($debug_bad_asc)
						{
							printf "Found Good Ascent Rate at TIME = %s  ASC = %4.3f - ",
							        $time_check,$asc_rate;
						    print "KEEP CHECKING = $keep_checking, ";
						    print "LBT = $last_bad_time\n";
						}
					}
				}
				# end ascension rate check code
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

	
    # ----------------------------------------
	# For records with bad ascension rates:
   	# overwrite $record_list[1] through
   	# $record_list[$last_bad_time] with
   	# missing records
    # ----------------------------------------
   	if ($last_bad_time != 0)
   	{
		if ($debug_bad_asc)
		{
			print "Replacing records from 1 to Last Bad Time (LBT) $last_bad_time\n";
		}
		for (my $insert=1; $insert<=$last_bad_time; $insert++)
		{
		    my $replace_record = ClassRecord->new($WARN,$file);
			$replace_record->setTime($insert);
			# --------------------------------------------
			# overwrite existing record in record_array
			# --------------------------------------------
			$record_list[$insert] = $replace_record;
		}
   	}

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
	foreach my $final_record(@record_list) 
	{
		print ($OUT $final_record->toString()) if (defined($final_record));
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
