#! /usr/bin/perl -w

##Module-------------------------------------------------------------------------
# <p>The Howard_Univ_Radiosonde_Converter.pl script is used for converting 
# ascii columnar data from the Barbados and the Republic of Cape Verde
# Sites to the EOL Sounding Composite (ESC) format.</p>
#
# @use   Howard_Radiosonde_Converter.pl [--skip]
#        --skip   Skip the pre-processing steps to strip out blank lines.
#                 This step only needs to be done once.  The default is false.
#
# @author Linda Echo-Hawk 2011_10_21
# @VERSION PREDICT 2020 Adapted for Howard U. Cape Verde 2-second
#            resolution soundings.
#          - For Cape Verde, resolution is 2 seconds so time is incremented 
#            by 2.  For other soundings, this value may need to be changed
#          - Search for HARD-CODED to find values requiring project-specific
#            changes.
#
# @author Linda Echo-Hawk 2011_10_21
# @VERSION PREDICT 2010 Created for Howard U. Barbados soundings based on 
#            the Haenam_Radiosonde_Converter.pl script for T-PARC 2008
#          - General code cleanup including removing "functions that contain
#            constants" such as getNetworkName().  These are now part of the
#            new() function that creates a converter instance. 
#          - For Barbados, resolution is 4 seconds so time is incremented 
#            by 4.  For other soundings, this value may need to be changed
#          - Search for HARD-CODED to find values required project-specific
#            changes.
#          - Due to inconsistency in the files, lat/lon header info was
#            HARD-CODED.  Since the location changed halfway through the
#            dataset, code was added to look for the file which marked
#            the beginning of collection in the second location, and the
#            lat/lon info for that location was used.
#          - For Barbados soundings, if no actual time was available in the
#            raw data file, the nominal time was used.
#          - Nominal time was taken from the raw data file name.
#          - The "header" information was contained in a footer in the 
#            raw data files.  The file was read into an array and the 
#            footer lines were popped off to save the data for header info.
#          - No sonde type was given in the raw data.  It was known that 
#            sonde IDs beginning with "0" were Graw DFM-06 and sonde IDs 
#            beginning with "9" were Graw DFM-06.  There were also cases of
#            sonde IDs beginning with "8" or "XXXXXX".  Code was added to 
#            identify the sonde type, or mark it was "Unknown".
#
#            Adapted code from RonBrownSoundingConverter.pl to:
#          - Raw data wind speed is converted from knots to m/s.
#          - Ascent rate is derived in the code.  Raw data ascent rate with
#            units of meters/minute should not be used per Scot L.
#          - Command line options were added to allow the user to 
#            skip the pre-processing steps to strip out blank lines.  This
#            only needs to be done once.  Using the --skip option speeds 
#            up the converter.
#
##Module-------------------------------------------------------------------------
package Howard_Univ_Radiosonde_Converter;
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
use DpgConversions;
use ClassHeader;
use ClassRecord;
use ElevatedStationMap;
use Station;
# import module to set up command line options
use Getopt::Long;


my ($WARN);
my ($SUMMARY);

printf "\nHoward_Univ_Radiosonde_Converter.pl began on ";print scalar localtime;printf "\n";
my $debug = 0;
my $debug_ascent_rate = 0;
my $debug_release = 0;
my $debug_header = 0;


# read command line arguments 
my $result;   

# skip pre-processing steps for raw data files
my $skip;
# "limit:i" i is optional, "limit=i" i is required
$result = GetOptions("skip" => \$skip);

if ($skip)
{
 	printf("Skip pre-processing steps.\n");
}
else
{
   	printf("Perform pre-processing.\n");
}

&main();

printf "\nHoward_Univ_Radiosonde_Converter.pl ended on ";print scalar localtime;printf "\n";


##------------------------------------------------------------------------------
# @signature void main()
# <p>Execute the conversion of the data.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = Howard_Univ_Radiosonde_Converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature void convert()
# <p>Convert all of the raw data and create the output files for the conversion.</p>
##------------------------------------------------------------------------------
sub convert {
    my $self = shift;

    mkdir($self->{"OUTPUT_DIR"}) unless (-e $self->{"OUTPUT_DIR"});
    mkdir($self->{"FINAL_DIR"}) unless (-e $self->{"FINAL_DIR"});

    $self->readRawDataFiles();
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
# @signature Howard_Univ_Radiosonde_Converter new()
# <p>Create a new Howard_Univ_Radiosonde_Converter instance.</p>
#
# @output $converter The new converter.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);

    # ----------------------------------
    # HARD-CODED project specific values
    # ----------------------------------
    $self->{"PROJECT"} = "PREDICT"; 
    $self->{"NETWORK"} = "HOWARD_UNIV";
    
    $self->{"FINAL_DIR"} = "../final";
    $self->{"OUTPUT_DIR"} = "../output";
    $self->{"RAW_DIR"} = "../raw_data";
	
    $self->{"STATION_FILE"} = sprintf("%s/%s_%s_CapeVerde_sounding_stationCD.out",$self->{"FINAL_DIR"},
                                      $self->clean_for_file_name($self->{"NETWORK"}),
                                      $self->clean_for_file_name($self->{"PROJECT"}));

    $self->{"SUMMARY"} = $self->{"OUTPUT_DIR"}."/station_summary.log";
    $self->{"WARN_LOG"} = $self->{"OUTPUT_DIR"}."/warning.log";

	$self->{"stations"} = ElevatedStationMap->new();

    return $self;
}

##---------------------------------------------------------------------------
# @signature void printStationFiles()
# <p>Generate the stationCD.out file and the station summary log for the stations
# in the conversion.</p>
##---------------------------------------------------------------------------
sub printStationFiles {
    my $self = shift;
    # my ($STN, $SUMMARY);

    open(my $STN, ">".$self->{"STATION_FILE"}) || 
	    die("Cannot create the ".$self->{"STATION_FILE"}." file\n");
    foreach my $station ($self->{"stations"}->getAllStations()) {
        print($STN $station->toString()) if ($station->getBeginDate !~ /^9+$/);
    }
    close($STN);

    open($SUMMARY, ">".$self->{"SUMMARY"}) || die("Cannot create the ".$self->{SUMMARY}." file.\n");
    print($SUMMARY $self->{"stations"}->getStationSummary());
    close($SUMMARY);
}

##------------------------------------------------------------------------------
# @signature void readRawDataFiles()
# <p>Determine all of the raw data files that need to be processed and then
# process them.</p>
##------------------------------------------------------------------------------
sub readRawDataFiles {
    my $self = shift;

    opendir(my $RAW,$self->{"RAW_DIR"}) or die("Cannot open raw directory.\n");
    my @files = readdir($RAW);
    closedir($RAW);

    open($WARN,">".$self->{"WARN_LOG"}) or die("Can't open warning file.\n");

    foreach my $file (sort(@files)) {
	$self->readRawFile($file) if ($file =~ /\.txt$/);
    }

    close($WARN);
}

##------------------------------------------------------------------------------
# @signature void readRawFile(String file_name)
# <p>Convert the specified file into the CLASS format.</p>
#
# @input $file_name The name of the raw data file to be converted.
##------------------------------------------------------------------------------
sub readRawFile {
    my $self = shift;
    my $file_name = shift;
    my $file = sprintf("%s/%s",$self->{"RAW_DIR"},$file_name);

	printf("\n\tProcessing file: %s\n",$file_name);

    # use the --skip cmd line option if this step has been performed already
    if (!$skip)
	{
   	    my $cmd_perl = "perl -pe 's/^\\s\+\$//'";
   	    my $cmd;

        #----------------------------------------------------------------------
        # Preprocess each file by stripping all blank lines. 
        # Doing this because the input file formats particularly for VOCALS 2008
        # project seem to be varying and contain mixed in blank lines. Stripping
        # blank lines will add some consistency. (LEC)
        # E.g., command to strip blank lines: perl -pe 's/^\s+$//' infile > outfile
        #-----------------------------------------------------------------------
        $cmd = sprintf "%s %s > %s.noBlanks", $cmd_perl, $file, $file;

        print "\nIssue the following command: $cmd\n";
        system $cmd;

        print "Executing: /bin/mv $file $file.orig \n"; # Save the original input file in *.orig
        system "/bin/mv -f $file $file.orig";

        print "Executing: /bin/mv $file.noBlanks $file \n";
        system "/bin/mv -f $file.noBlanks $file";
    }
    
    open(my $FILE,$file) or die("Cannot open file: $file\n");

	my $header = ClassHeader->new($WARN);

    # ----------------------------------
    # HARD-CODED project specific values
    # ----------------------------------
    $header->setProject($self->{"PROJECT"});
    $header->setType("Howard University Radiosonde");
    $header->setReleaseDirection("Ascending");

    $header->setId("Howard_Univ");
    $header->setSite("CapeVerde");
    
	# --------------------------------------------------------------
	# for Howard Univ. there are 4 lines before the data; header info
    # is found at the bottom of the file
	# ---------------------------------------------------------------
	#    PRES     HGT     TEMP    DWPT    RELH    MIXR   WSPD   WDIR
	#     hPa      m        C      C      %      g kg-1  knot    deg
	# ---------------------------------------------------------------
	# 
    # File name shows nominal time
    # research.howard_univ.radiosonde_20100814_19Z_barbados.txt
	# NOTE: Wind speed in knots will require conversion 
	# ---------------------------------------------------------------
    my @fileInfo = (split(/_/,$file_name));
	
    if ($debug_header)
	{
		foreach my $f (@fileInfo)
        {
		    print "$f\n";
	    }

        print "FILEINFO: $fileInfo[2]\n";
	    print "FILETIME: $fileInfo[3]\n";
	}

	$fileInfo[2] =~ /(\d{4})(\d{2})(\d{2})/;
    my $nomDate = sprintf("%04d, %02d, %02d", $1, $2, $3);

    $fileInfo[3] =~/(\d{2})/;
    my $nomTime = sprintf("%02d:00:00", $1);

    if ($debug_header)
	{
		print "NOMDATE/TIME $nomDate $nomTime\n";
	}

    #-----------------------------------------------------------------------
    # skip the column headers
    <$FILE>;<$FILE>;<$FILE>;<$FILE>;
    #-----------------------------------------------------------------------

	my @lines = <$FILE>;   # Grab all the remaing lines

    # remove last lines of array and assign to header data line
	my $alt_line = pop@lines;
	my $lat_line = pop@lines;
	my $lon_line = pop@lines;
	pop@lines; # discard Groundstation line
	my $dateline = pop@lines;   
    
	if ($debug_header)
	{
		print "$alt_line\n";
	    print "$lat_line\n";
	    print "$lon_line\n";
	    print "$dateline\n";
	}

	# ----------------------------------
	#  Altitude [m]: 112.80
	# ----------------------------------
	chomp($alt_line);
	my (undef(),undef(),$alt) = split(' ',trim($alt_line));
	if ($debug_header)
	{
		print "ALT: $alt\n";
	}
	$header->setAltitude($alt, "m") unless ($alt == 999.0);
    
	# -----------------------------------------------------------
	# lat/lon will be hard-coded due to inconsistency in the
    # raw data files.   
	# -----------------------------------------------------------
    my $lat = 16.864;
	my $lon = -24.867;

    $header->setLatitude($lat, $self->buildLatlonFormat($lat));
    $header->setLongitude($lon, $self->buildLatlonFormat($lon));


    # ----------------------------------------------------------------
    #  Date: 16.08.2010 Start time: 04:18:52 Number of probe: 010520
    #  Date: 29.08.2010 Start time: xx:xx:xx Number of probe: XXXXXX
    # ----------------------------------------------------------------
    my @dateInfo = split(' ', trim($dateline));
	
	foreach my $d(@dateInfo)
	{
		print "$d  ";
	}
	print " \n";
	$dateInfo[1] =~ /(\d{2}).(\d{2}).(\d{4})/;
    my $date = sprintf("%04d, %02d, %02d", $3, $2, $1);
	my $releaseTime;
	if ($dateInfo[4] =~ /(\d{2}):(\d{2}):(\d{2})/)
	{
		$releaseTime = sprintf("%02d:%02d:%02d", $1, $2, $3);
	}
	else
	{
		$releaseTime = $nomTime;
	}
	
	my $probe = $dateInfo[8];
	my $sondeType = "";
    if ($debug_release)
	{
		print "date: $date   ";
		print "time: $releaseTime   ";
		print "probe: $probe\n";
	}

	if ($probe =~ /^0/)
	{
		$sondeType = "Graw DFM-06";
	}
	elsif ($probe =~ /^9/)
	{
		$sondeType = "Graw DFM-97";
	}
	elsif ($probe =~/^X/i)
	{
		$probe = "Unknown";
		$sondeType = "Unknown";
		print "WARNING: No sonde type match found for probe $probe \n";
	}
	else
	{
		$sondeType = "Unknown";
		print "WARNING: No match for sonde $sondeType found for probe $probe \n";
	}
    if ($probe)
	{
        $header->setLine(5,"Sonde Id/Sonde Type:", join ('/', $probe, $sondeType));
	}
    
	$header->setNominalRelease($nomDate, "YYYY, MM, DD", $nomTime, "HH:MM:SS", 0); 
    $header->setActualRelease($date, "YYYY, MM, DD", $releaseTime, "HH:MM:SS", 0);  

    #-----------------------------------------------------------------
    # Set the station information
    #-----------------------------------------------------------------
    my $station = $self->{"stations"}->getStation($header->getId(),$self->{"NETWORK"}, 
	                          $header->getLatitude(),$header->getLongitude(),
	  				          $header->getAltitude());
    if (!defined($station)) {
        $station = $self->build_default_station($header->getId(),$self->{"NETWORK"});

	    $station->setLatitude($header->getLatitude(),$self->buildLatlonFormat($header->getLatitude()));
	    $station->setLongitude($header->getLongitude(),$self->buildLatlonFormat($header->getLongitude()));
	    $station->setElevation($header->getAltitude(),"m");
		
        $self->{"stations"}->addStation($station);
    }
    $station->insertDate($header->getNominalDate(),"YYYY, MM, DD");     
    #-----------------------------------------------------------------

    #-------------------------------------------------
    # Open the output file in the ../output directory.
    #-------------------------------------------------
    my $filename = sprintf("%s_%s_%04d%02d%02d%02d%02d.cls",$header->getId(),
	           $header->getSite(),
			   split(/, /,$header->getActualDate()),
			   split(/:/,$header->getActualTime()));

    open(my $OUT,sprintf(">%s/%s",$self->{"OUTPUT_DIR"},$filename)) or 
			die("Cannot open output file\n");

	if (!defined($header))
	{
		print "WARNING: Header undefined\n";
	}
    # print header to output file
    print($OUT $header->toString());  
    
	my $first_line = 1;
    my $time = 0;
    my $prev_time = 9999.0;
    my $prev_alt = 99999.0;

    #------------------------------------------------------
    # Process each of the remaining data lines.
    # Note that $WARN is the ../output/warning.log file.
    #------------------------------------------------------
    foreach my $line (@lines) {
		
		if ($line !~ /^\s*\/\s*$/)
		{
			chomp($line);
	        my $rec = ClassRecord->new($WARN, $filename);
            my @data = split(' ', trim($line));

            $rec->setTime($time);

            # ----------------------------------
            # HARD-CODED value
            # ----------------------------------
			# Note: Cape Verde soundings are 2-second resolution
		    $time += 2;

            $rec->setPressure($data[0], "hPa")  unless ($data[0] == 999.0); 
            $rec->setTemperature($data[2], "C") unless ($data[2] == 999.0);
            $rec->setRelativeHumidity($data[4]) unless ($data[4] == 999.0); 

		    $rec->setDewPoint($data[3], "C") unless ($data[3] == 999.0); 
		
		    if ($data[6] != 999.0)
		    {
			    my $windSpd = convertVelocity($data[6],"knot", "m/s");
    	        $rec->setWindSpeed($windSpd, "m/s");
		    }

            $rec->setWindDirection($data[7])    unless ($data[7] == 999.0);
    	
		    $rec->setAltitude($data[1], "m") unless ($data[1] == 999.0); 
        
#            # -----------------------------
#            # Calculate ascension rate for all but first line (surface record)
#	        # -----------------------------
    	    if ($first_line) 
			{
	            $rec->setLatitude($header->getLatitude(),$self->buildLatlonFormat($header->getLatitude()));
	            $rec->setLongitude($header->getLongitude(),$self->buildLatlonFormat($header->getLongitude()));

                $prev_time = $rec->getTime();
                $prev_alt = $rec->getAltitude();
	            $first_line = 0;
   	        }   # end if($first_line)
            else
            { 
                #-------------------------------------------------------
                # Calculate the ascension rate which is the difference
                # in altitudes divided by the change in time. Ascension
                # rates can be positive, zero, or negative. But the time
                # must always be increasing (the norm) and not missing.
                #
                # Only save the next non-missing values.
                # Ascension rates over spans of missing values are OK.
                #-------------------------------------------------------
                if ($prev_time != 9999  && $rec->getTime()     != 9999  &&
                    $prev_alt  != 99999 && $rec->getAltitude() != 99999 &&
                    $prev_time != $rec->getTime() ) 
		        {
                    $rec->setAscensionRate( ($rec->getAltitude() - $prev_alt) /
                                            ($rec->getTime() - $prev_time),"m/s");
		        }        

                if ($rec->getTime() != 9999 && $rec->getAltitude() != 99999)
                {
                    $prev_time = $rec->getTime();
                    $prev_alt = $rec->getAltitude();

                    if ($debug_ascent_rate) 
					{
						print "Current record has valid Time and Alt. Save as previous.\n"; 
					}
                }
            } # Calc Ascension Rate if possible and save current values as previous
     
            print($OUT $rec->toString()); 
		} # if not blank line
    } # Foreach my lines. - process all data lines.

    close($OUT);
}

##------------------------------------------------------------------------------
# @signature Station build_default_station(String station_id, String network)
# <p>Create a default station using the specified station_id and network.</p>
#
# @input $station_id The identifier of the station to be created.
# @input $network The network the station belongs to.
# @return The new station object with the default values for the network.
##------------------------------------------------------------------------------
sub build_default_station {
    my ($self,$station_id,$network) = @_;
    my $station = Station->new($station_id,$network);

    $station->setStationName("Cape Verde");
	$station->setLatLongAccuracy(3);
	$station->setCountry("99");
    $station->setStateCode("99");
    $station->setReportingFrequency("6 hourly");
    $station->setNetworkIdNumber("99");
	# platform 87, Rawinsonde, Other
    $station->setPlatformIdNumber(87);
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
# @signature String clean_for_file_name(String text)
# <p>Remove/translate characters in a String so it can be used in a file name.</p>
#
# @input $text The String to be cleaned.
# @output $text The cleaned up String.
##-------------------------------------------------------------------------
sub clean_for_file_name {
    my ($self,$text) = @_;

    # Convert spaces to underscores.
    $text =~ s/\s+/_/g;

    # Remove all hyphens
    $text =~ s/\-//g;

    return $text;
}

##------------------------------------------------------------------------------
# @signature String trim(String line)
# <p>Remove the leading and trailing whitespace around a String.</p>
#
# @input $line The String to be trimmed.
# @output $line The trimmed line.
##------------------------------------------------------------------------------
sub trim {
    my $line = shift;
    $line =~ s/^\s+//g;
    $line =~ s/\s+$//g;
    return $line;
}






