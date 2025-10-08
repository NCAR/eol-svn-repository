#! /usr/bin/perl -w
##Module------------------------------------------------------------------------
# <p>The FP3_ELLIS_ESC_sounding_converter.pl script is used for converting high
# resolution radiosonde data from ASCII to the EOL Sounding Composite (ESC)
# format. PISA stands for the PECAN (2015) field project Sounding Array or
# "PECAN Integrated Sounding Array (PISA)". Note there there are 3 fixed PISA sites
# in the PECAN (Hays, KS) field project:  FP2 - GREEN;Greensburg, KS;
# FP3 - ELLIS;Ellis, KS; and FP6 - HESS;Hesston, KS.  This is the converter for
# the Ellis, KS data. FP3 stands for Fixed PISA site #3. SGP stands for Southern
# Great Plains.
#
# The incoming format is an older Vaisala format (Vaisala RS92).
#</p> 
#
# @author Linda Echo-Hawk
# @version PECAN 2015 
#          NOTE: Search on HARD-CODED to find values that may require 
#            updating.
#          - The converter expects filenames in the following format:
#            upperair.Millersville_FP3_radiosonde.YYYYMMDDHHmm.one_second.txt (e.g., 
#            upperair.Millersville_FP3_radiosonde.201507160600.one_second.txt)
#          - The file contains header info on lines 1-10. Actual data starts 
#            on line 12. 
#          - The radiosonde ID is obtained from the header information.
#          - The radiosonde type is hard-coded, although it is available
#            in the raw data header if needed.
#          - The header lat/lon/alt data were incorrect and we were 
#            advised to override these values by Scot L. The values are
#            hard-coded, and these values are also used in the surface 
#            data record. 
#          - Geopotential altitude is calculated. Ascent rate is also
#            calculated, even though most data files have an ascent
#            rate column.
#          - Missing values are represented by "/////" in the raw data.
#          - The release date and time are obtained from the raw data
#            file unless the file is determined to be empty, in which
#            case there is code to get that info from the file name.
#          - The first three raw data files have formats that are slightly
#            different than the rest of the data. These files were manually
#            changed to be in the same format (number and placement of
#            columns). See the original raw data files in the /orig_raw_data
#            directory.
#
##Module------------------------------------------------------------------------
package FP3_ELLIS_ESC_sounding_converter;
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
use DpgCalculations;

my ($WARN);

my $TotalRecProc = 0;
my $dataRecProc = 0;

#----------------------------------------------------------------
# Set either debug value to 1 for varying amounts of debug.
#-----------------------------------------------------------------
my $debug = 0; # higher level debug output
my $debug2 = 0;  # detailed debug output
my $debug_geo_height = 0; # geopotential height calculations


printf "\nFP3_ELLIS_ESC_sounding_converter.pl began on ";print scalar localtime;printf "\n";
&main();
printf "\nFP3_ELLIS_ESC_sounding_converter.pl ended on ";print scalar localtime;printf "\n";

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the FP3 ELLIS ESC radiosonde data by converting it from 
# the native ASCII format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = FP3_ELLIS_ESC_sounding_converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature FP3_ELLIS_ESC_radiosonde_converter new()
# <p>Create a new instance of a FP3 ELLIS ESC Converter.</p>
#
# @output $self A new FP3 ELLIS ESC Converter object.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);
    
    $self->{"stations"} = ElevatedStationMap->new();

    # HARD-CODED
    $self->{"PROJECT"} = "PECAN";
    $self->{"NETWORK"} = "Fixed_PISA";
    
    $self->{"FINAL_DIR"} = "../final";
    $self->{"OUTPUT_DIR"} = "../output";
    $self->{"RAW_DIR"} = "../raw_data";
    
    $self->{"STATION_FILE"} = sprintf("%s/%s_%s_sounding_stationCD.out",$self->{"FINAL_DIR"},
				      $self->cleanForFileName($self->{"NETWORK"}),
				      $self->cleanForFileName($self->{"PROJECT"}));
    $self->{"WARN_LOG"} = $self->{"OUTPUT_DIR"}."/warning.log";

    return $self;
} # new

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
    # platform, 1172	Vaisala RS41
    $station->setPlatformIdNumber(1172);
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
# @output $fmt The format that corresponds to the value.
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

	my $filename = $file; 
	# printf("parsing header for %s\n",$filename);


    # Set the type of sounding "Data Type:" header line
    $header->setType("Millersville");
    $header->setReleaseDirection("Ascending");

    $header->setProject($self->{"PROJECT"});
    
    # HARD-CODED
	# The Id will be the prefix of the output file
    $header->setId("ELLIS");
	# "Release Site Type/Site ID:" header line
    $header->setSite("FP3 Ellis, KS/ELLIS");


    # ------------------------------------------------
    # Read through the file for additional header info
    # ------------------------------------------------
 	my $index = 0;

	foreach my $line (@headerlines) 
	{

        # if there is a line and it is NOT a blank line
		if (($line) && ($line !~ /^\s*\/\s*$/))
		{
			if ($debug2) {print "parseHeader:: (index = $index); line: xxx $line xxx \n";}

	        # -----------------------------------------------------------
    	    # Add the non-predefined header lines to the header.
        	# skip over any blank lines (empty or contain white space only)
	        # -----------------------------------------------------------
			# Sonde serial number         L1420255
	        # -----------------------------------------------------------
   	    	if ($line =~ /Sonde serial number/i)
	   	    {
   				chomp ($line);
   			    my (@contents) = split(' ',$line);

				print "SONDE INFO: @contents\n"; # 34530 (PECAN_FP2)
				my $sonde_id = $contents[3];
                my $sonde_type = "Vaisala RS41-SGP";
                $header->setLine(5, "Radiosonde Type".":",trim($sonde_type));
                $header->setLine(6, "Radiosonde Serial Number".":",trim($sonde_id));
	   	    }

	   		# "Balloon release date and time     2015-05-16T17:52:40" 
  			if ($line =~ /Balloon release date and time/i)
   			{
   				chomp ($line);
	   		    my (@releaseInfo) =  (split(' ',trim($line)));
   				my ($date, $time) = (split('T',trim($releaseInfo[5])));
				my ($year, $month, $day) = split ('-',$date);
   				my ($hour, $min, $sec) = split(':',$time);

	   			if ($debug2) {print "HOURS: $hour   MIN: $min\n";}
   				my $releaseTime = sprintf("%02d:%02d:%02d", $hour, $min, $sec);
        	    my $releaseDate = sprintf("%04d, %02d, %02d", $year, $month, $day);
   		    	if ($debug2) {print "DATE: $releaseDate   TIME: $releaseTime\n";}
	            $header->setActualRelease($releaseDate,"YYYY, MM, DD",$releaseTime,"HH:MM:SS",0);
    			$header->setNominalRelease($releaseDate,"YYYY, MM, DD",$releaseTime,"HH:MM:SS",0);
	    	}
			$index++;
		} # end if $line

	} # end foreach line of @headerlines

	my $lat = 38.940;
   	my $lon = -99.565;
	my $alt = 646;

	$header->setLatitude($lat,$self->buildLatLonFormat($lat));
	$header->setLongitude($lon,$self->buildLatLonFormat($lon));
    $header->setAltitude($alt,"m");

    # ----------------------------------------------------------
    # Extract the date and time information from the file name
    # Expects filename similar to: 
	# upperair.Millersville_FP3_radiosonde.201507160600.one_second.txt
    # ----------------------------------------------------------
	# THIS CODE IS NOT NEEDED SINCE THERE ARE NO EMPTY FILES
	# It was originally included in the GTS converter so that
	# empty raw data files would create an output file with
	# a header only.
	# ----------------------------------------------------------
    if ($debug2) {print "file name = $file\n";}

	# if ($file =~ /^millersville_pecan3_(\d{4})(\d{2})(\d{2})_(\d{2})(\d{2})/)
	if ($file =~ /(\d{4})(\d{2})(\d{2})(\d{2})(\d{2})/)
	{
		my ($year, $month, $day, $hour, $min) = ($1,$2,$3,$4,$5);
        my $date = join ", ", $year, $month, $day;
		my $time = join ":", $hour,$min,'00';
        print "FROM FILE: DATE:  $date   TIME:  $time\n";

	}

    $header->setLine(7,"Ground Station Equipment:    ", "Digicora MW41 2.2.1");

    return $header;
}
                           
##------------------------------------------------------------------------------
# @signature void parseRawFile(String file)
# <p>Read the data from the specified file and convert it to the ESC format.</p>
#
# @input $file The name of the file to be parsed.
##------------------------------------------------------------------------------
sub parseRawFile {
    my ($self,$file) = @_;

    printf("\nProcessing file: %s\n",$file);
    
    open(my $FILE,$self->{"RAW_DIR"}."/".$file) or die("Can't open file: ".$file);
    my @lines = <$FILE>;

    my $number_lines_in_file = $#lines+1;

    #------------------------------------------------------------
    # ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,
    #  $atime,$mtime,$ctime,$blksize,$blocks) = stat($filename);
    #------------------------------------------------------------
    # Stat array results::
    #  0 dev device number of filesystem
    #  1 ino inode number
    #  2 mode file mode (type and permissions)
    #  3 nlink number of (hard) links to the file
    #  4 uid numeric user ID of file's owner
    #  5 gid numeric group ID of file's owner
    #  6 rdev the device identifier (special files only)
    #  7 size total size of file, in bytes
    #  8 atime last access time in seconds since the epoch
    #  9 mtime last modify time in seconds since the epoch
    # 10 ctime inode change time in seconds since the epoch (*)
    # 11 blksize preferred block size for file system I/O
    # 12 blocks actual number of blocks allocated
    #------------------------------------------------------------
    my @file_info = stat $FILE;

    if ($debug2) 
    {
		print "file_info:: @file_info\n"; 
       	print "number_lines_in_file:: $number_lines_in_file \n";
       	print "File size = $file_info[7]\n";
    }


    close($FILE);
    

    #------------------------------
    # Generate the sounding header.
    #------------------------------
	my @headerlines = @lines[0..12];
    my $header = $self->parseHeader($file,@headerlines);

    #-----------------------------------------------------------
    # Only continue processing the file if a header was created.
    #-----------------------------------------------------------
    if (defined($header)) 
	{

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

	    # ---------------------------------------------
    	# Needed for code to derive geopotential height
	    # ---------------------------------------------
		my $previous_record;
		my $geopotential_height;
		my $surfaceRecord = 1;

	    # ----------------------------------------------------
    	# Parse the data portion of the input file
	    # ----------------------------------------------------
		my $index = 0;

		foreach my $line (@lines) 
		{
        	$TotalRecProc++;

			# --------------------------------------------
			# Ignore the header lines.
			# HARD-CODED to skip the header lines
			# --------------------------------------------
	    	if ($index < 11) { $index++; next; }

        	# If it is a blank line, skip it. Helpful at end of file.
        	if ($line =~ /^\s*$/) { next; }

            $dataRecProc++;
       		if ($debug2) {print "TotalRecProc: $TotalRecProc, dataRecProc: $dataRecProc\n";}
	   		
			my @data = split(' ',$line);
		    my $record = ClassRecord->new($WARN,$file);

				# ---------------------------------------------------------
				# time $data[1]		# dewpt [5]           # ascent rate [10]
				# alt [2]	        # RH [6]	          # lat [11]
				# pressure [3]		# wind spd [8]        # lon [12]
				# temp [4]		    # wind dir [9]
				# ---------------------------------------------------------

       		if ($debug2) { print "Parse Data Record:: Temp, TD, RH, Winds, etc.\n";}

	        #----------------------------------------------------
    	    # missing values are ///// - HARD-CODED
      		#----------------------------------------------------
    		$record->setTime($data[1]);
		    	
			$record->setPressure($data[3],"mb") unless ($data[3] =~ /\/+/);
			$record->setTemperature($data[4],"C") unless ($data[4] =~ /\/+/);
			$record->setDewPoint($data[5],"C") unless ($data[5] =~ /\/+/);
			$record->setRelativeHumidity($data[6]) unless ($data[6] =~ /\/+/);

		    $record->setWindSpeed($data[8],"m/s") unless ($data[8] =~ /\/+/);
		    $record->setWindDirection($data[9]) unless ($data[9] =~ /\/+/);

		    # $record->setAscensionRate($data[10],"m/s") unless ($data[10] =~ /\/+/);
    	
			if ($surfaceRecord)
			{
				$record->setLatitude($header->getLatitude(),$self->buildLatLonFormat($header->getLatitude()));
				$record->setLongitude($header->getLongitude(),$self->buildLatLonFormat($header->getLongitude()));

				$record->setAltitude($header->getAltitude(),"m");
				$surfaceRecord = 0;
			}
			else
			{
				# calculate geopotential height and set altitude
				# leave lat/lon values as missing
				# ---------------------------------------------------------------
				# Calculate geopotential height
	    	    #-----------------------------------------------------------------
    	    	# BEWARE:  For PECAN 2015 SLoehrer says there are issues 
	           	# with the raw data altitudes, so compute the geopotential 
				# height/altitude and insert for all other than surface record.
        		# call calculateAltitude(last_press,last_temp,last_dewpt,last_alt,
				#                        this_press,this_temp,this_dewpt,this_alt,1)
				# Note that the last three parms in calculateAltitude
    	    	# are the pressure, temp, and dewpt for the current
	    	    # record. To check the altitude calculations, see
    	    	# the web interface tool at 
	        	#
		        # http://dmg.eol.ucar.edu/cgi-bin/conversions/calculations/altitude
				# NOTE: This code taken from VORTEX2 SUNY converter.
        		#-------------------------------------------------------------------
				if ($debug_geo_height) 
		        { 
    		        my $prev_press = $previous_record->getPressure(); 
        		    my $prev_temp = $previous_record->getTemperature(); 
            		my $prev_alt = $previous_record->getAltitude();
					my $prev_dewpt = $previous_record->getDewPoint();
		            print "\nCalc Geopotential Height from previous press = $prev_press, temp = $prev_temp, alt = $prev_alt,\n";
					print "and current press = $data[3] and temp = $data[4]\n"; 
        		}

		        if ($previous_record->getPressure() < 9990.0)
    		    {
        		    if ($debug){ print "prev_press < 9990.0 - NOT missing so calculate geopotential height.\n"; }
            	    # ---------------------------------------------------------
					# print "\tTHIS PRESSURE: $data[2]\n";
	                # for 621_001.LOG, pressure is 0.000 and illegal divison
					# error occurs when calculateAltitude is called
        	        # ---------------------------------------------------------
					if ($data[3] != 0.000)
					{
						# print "Calculating geopotential height\n";
						$geopotential_height = calculateAltitude($previous_record->getPressure(),
        	        	                                 $previous_record->getTemperature(), 
														 $previous_record->getDewPoint(), 
														 $previous_record->getAltitude(), 
														 $data[3], $data[4], $data[5], 1);
					}
					else
					{
						print "Bad Pressure $data[3] -- Geopotential height not calculated\n";
						$geopotential_height = 99999.0;
					}
		            if (defined($geopotential_height))
					{
	    		        $record->setAltitude($geopotential_height,"m");
					}
					else
					{
						print "WARNING: Was not able to calculate geopotential height\n";
						$geopotential_height = 99999.0;
					}
	    	    } # end calculate geopotential height
    	
			} # end if !$surfaceRecord
    	
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

	        #---------------------------------------------
    	    # Only save current record as previous record
        	# if values not missing. This affects the
	        # calculations of the geopotential height,
    	    # as the previous height must be non-missing. 
        	#---------------------------------------------
	        if ($debug_geo_height) 
			{
				my $press = $record->getPressure(); 
				my $temp = $record->getTemperature();
    	        my $alt = $record->getAltitude();
				my $dewpt = $record->getDewPoint();
            	print "\tCurrent Rec: press = $press, temp = $temp, ";
				print "dewpt = $dewpt, alt = $alt\n";
    	    }
        	if ( ($record->getPressure() < 9999.0)  && ($record->getTemperature() < 999.0)
	             && ($record->getAltitude() < 99999.0) )
			{
				# if ($debug_geo_height) { print "\tAssign current record to previous_record \n\n"; }
            	$previous_record = $record;
	        } 
			else 
			{
				if ($debug_geo_height) 
				{
					print "\t\tDo NOT assign current record to previous_record! ";
					print "Current record has missing values.\n\n";
			    }
        	}
	    
			printf($OUT $record->toString());

	    } #foreach

	} #if $header
	else
	{
		printf("Unable to make a header\n");

	} # Could not make header

} # parseRawFile()


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
# ESC formatted file. 
##------------------------------------------------------------------------------
sub readDataFiles {
    my ($self) = @_;
    
    my $cmd = "\n";
    
	opendir(my $RAW,$self->{"RAW_DIR"}) or die("Can't read raw directory ".$self->{"RAW_DIR"});

	# -------------------------------------------------------------------------
	# Input file names must be of the form: 
	# upperair.Millersville_FP3_radiosonde.YYYYMMDDHHmm.one_second.txt,
	# e.g., upperair.Millersville_FP3_radiosonde.201507160600.one_second.txt , 
	# where YYYY = year, MM = month, DD = day, HH = hour and mm = minute, and
	# "txt" is the suffix. All files with names of this form will be processed.
	# -------------------------------------------------------------------------
		
    my @files = grep(/^upperair.*\.txt$/,sort(readdir($RAW))); 

	closedir($RAW);

	open($WARN,">".$self->{"WARN_LOG"}) or die("Can't create ".$self->{"WARN_LOG"});

	# printf("Ready to read the files\n");
    foreach my $file (@files) 
    {
        $self->parseRawFile($file);   # Process each data file!
        if ($debug2) {print "After parseRawFile() - TotalRecProc: $TotalRecProc, dataRecProc: $dataRecProc\n";}

   	    $TotalRecProc = 0;
       	$dataRecProc = 0;

    } # process each file 
	close($WARN);

} # readDataFiles

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
