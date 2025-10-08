#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The FP2_NPS_ESC_sounding_converter.pl script is used for converting high
# resolution radiosonde data from ASCII to the EOL Sounding Composite (ESC) 
# format. PISA stands for the PECAN (2015) field project Sounding Array or
# "PECAN Integrated Sounding Array (PISA)". Note there there are 3 fixed PISA 
# sites in the PECAN (Hays, KS) field project:  FP2 - GREEN;Greensburg, KS; 
# FP3 - ELLIS;Ellis, KS; and FP6 - HESS;Hesston, KS.  This is the converter for
# the Greensburg, KS data.   Note if you see UMBC associated with this dataset, 
# it stands for the University of Maryland, Baltimore County.  FP2 stands for
# Fixed PISA site #2. Also, sometimes HU or Howard University is associated with 
# this site. SGP stands for Southern Great Plains.
#
# NOTE: These data were also converted to the CLASS format in real-time to be 
# placed on the GTS. See the IVEN notes for more information on that process.  
# 
# This converter was created to process the data into the EOL Sounding Composite
# (ESC) format. The incoming format is Vaisala FLEDT format variant (Vaisala RS41).
#
#</p> 
#
# @author Linda Echo-Hawk 30 Oct 2015
# @version PECAN 2015 ESC Processing of FP2 soundings into the
#          EOL Sounding Composite (ESC) format.
#        - This converter is based on the software developed by LEC
#          for the GTS processing of the FP2 soundings
#        - Checks required for GTS processing have been removed
#
#
# @author Linda Cully 2015-05-11
# @version PECAN_2015 version created from the DYNAMO_2011 Male converter.
#    - Changed all references in code from Male (Maldives) to PISA FP2 GREEN
#      for the PECAN 2015 project.
#
#---------------------------------
# Note that this converter is STRONGLY based on the DYNAMO Male, Maldives data
# conversion software so comments from that conversion may also be applicable.
#
# Note that the Male, Maldives raw data were in Vaisala "Digicora 3" format. 
# The file contains header info on lines 1-39. Actual data starts on line 40. 
# The raw data file names were in the form "43555_yyyymmddhhmm.tsv" where 
# yyyy = year, mm = month, dd = day, hh=hour, mm = minute. 43555 is the call 
# sign for Male Maldives. (Note that the PISA data has one extra header line 
# with the addition of the El variable.)
#---------------------------------
# This code makes the following assumptions:
#
#  - That the raw data file names shall be in the form
#         "UMBC_RS41_yyyymmdd_hhmmUT.dat" where yyyy = year, mm = month, 
#         dd = day, hh=hour, mm = minute.
#  - That the raw data are in the Vaisala FLEDT format variant. The file 
#         contains header info on lines 1-40. Actual data starts on line 41. 
#  - That the following directories exist: ../output, ../archive.
#
#-------------------------------------------------------------- 
#
# Note the following:
#  - This code computes the ascension rate, even though this is not required. 
#  - Blank lines at end of the raw data file are ignored.
#
# BEWARE:  Search for "HARDCODED" to find project-specific items that may
#          need to updated.
#
##Module------------------------------------------------------------------------
package FP2_NPS_ESC_sounding_converter;
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

my $TotalRecProc = 0;
my $dataRecProc = 0;

#----------------------------------------------------------------
# Set either debug value to 1 for varying amounts of debug.
#-----------------------------------------------------------------
my $debug = 0;   # higher level debug output
my $debug2 = 0;  # detailed debug output

printf "\nFP2_NPS_ESC_sounding_converter.pl began on ";print scalar localtime;printf "\n";
&main();
printf "\nFP2_NPS_ESC_sounding_converter.pl ended on ";print scalar localtime;printf "\n";

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the FP2 NPS ESC radiosonde data by converting it from 
# the native ASCII format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = FP2_NPS_ESC_sounding_converter->new();
    $converter->convert();
} # main

##------------------------------------------------------------------------------
# @signature FP2_NPS_ESC_sounding_converter.pl new()
# <p>Create a new instance of a FP2_NPS_ESC_sounding_converter.</p>
#
# @output $self A new FP2_NPS_ESC_sounding_converter object.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);
    
    $self->{"stations"} = ElevatedStationMap->new();
    
	# HARDCODED
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
    # platform, 591	Radiosonde, iMet-1
    $station->setPlatformIdNumber(591);
    # $station->setMobilityFlag("m"); 
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
} # convert()

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

    my $firstDataLine = 0;

    # printf("parsing header for %s\n",$file);


    # Set the type of sounding "Data Type:" header line
    $header->setType("NPS");
    $header->setReleaseDirection("Ascending");

    $header->setProject($self->{"PROJECT"});
    
    # HARDCODED
    # The Id will be the prefix of the output file
    $header->setId("GREEN_NPS");

    # Site info received from SL
    # "Release Site Type/Site ID:" header line
    $header->setSite("FP2 GREEN/Greensburg, KS");

    # ------------------------------------------------
    # Read through the file for additional header info
    # ------------------------------------------------
    my $index = 0;
    foreach my $line (@headerlines)
	{

        if ($debug2) {print "parseHeader:: (index = $index); line: xxx $line xxx \n";}

        # -----------------------------------------------------------
        # Add the non-predefined header lines to the header.
        # -----------------------------------------------------------
        if (($index > 0) && ($index < 11))
        {
			if ($line =~ /RS-Number/i)
            {
				chop ($line); chop ($line); # Trim control M/EOL char
                my ($label,@contents) = split(/:/,$line);
				print "SONDE INFO: @contents\n"; # 34530 (PECAN_FP2)
				my ($sonde_id, $loc_info) = split(' ', $contents[0]);
                my $sonde_type = "iMet1";
                $header->setLine(5, "Radiosonde Type".":",trim($sonde_type));
                $header->setLine(6, "Radiosonde Serial Number".":",trim($sonde_id));
           	} # RS-Number
       	} # index/line 1-10

        #--------------------------------------
        # Ignore the rest of the header lines.     HARDCODED
        #--------------------------------------
        if ($index < 40) 
        {
		    if ($debug) {print "If index < 40...processed header line. index = $index\n"} 

            $index++; 
            next; 
        } # Header lines 1-40

        #--------------------------------------------------------------
        # Process the DATA lines starting at line 40 in the raw data.
        #--------------------------------------------------------------
        # Find the lat/lon for the release location in the actual data.
        # Pull out of first data line. HERE
        #---------------------------------------------------------------
        else # Data lines - Found FIRST data line
        {
            if ($debug2) {print "\n\n***********FOUND Data Line!************\n (firstDataLine = $firstDataLine\n";}

			# ------------------------------------------------------------------
			# Only extract info from first data line; Skip all other data lines
			# ------------------------------------------------------------------
            if ($firstDataLine == 0)
            {
			    $firstDataLine = 1; 

                my @data = split(' ',$line);

                if ($debug2) 
				{
					print "data: @data\n"; 
					print "Lon = Data(15): xxx $data[15] xxx\n"; 
					print "Lat = Data(16): xxx $data[16] xxx\n";
				}
             	if ($debug2) {print "Alt (Height) = Data(6): xxx $data[6] xxx\n";}


    	        #-----------------------------------------------
        	    # Process the Lat/Lon data from the first record
            	# and put in output header.
	            #-----------------------------------------------
    	        if (($data[15] > -32768) && ($data[16] > -32768)) 
        		{

                    #--------------------------------------------------------------
               		# Format length must be the same as the value length or
	                # convertLatLong will complain (see example below)
    	            #
	                # For Male, Maldives:
    	            # base lat   = 36.6100006103516    base lon = -97.4899978637695
        	        # Lat format = DDDDDDDDDDDDDDDD  Lon format = -DDDDDDDDDDDDDDDD  
            	    #
                	# For Greensburg, KS:
	                # No lat/lon in header. First lat/lon in data is of the form:
    	            # Lat = 39.06   Lon = -76.88
        	        #       DDDDD         DDDDDD
            	    #-------------------------------------------------------------- 
	                #----------
    	            # Longitude
        	        #----------
            	    my $lon_fmt = $data[15] < 0 ? "-" : "";
                	while (length($lon_fmt) < length($data[15])) 
	           		{	 
                   		$lon_fmt .= "D"; 
                   	}

                	if ($data[15] != -32768) 
					{
						$header->setLongitude($data[15],$lon_fmt);
					}

                	if ($debug2) {print "Lon = Data(15): xxx $data[15] xxx\n";}
                	if ($debug2) {print "lon_fmt: xxx $lon_fmt xxx\n";}

                	#----------
	                # Latitude
    	            #----------
            	    my $lat_fmt = $data[16] < 0 ? "-" : "";
        	        while (length($lat_fmt) < length($data[16])) 
                   	{
						$lat_fmt .= "D"; 
                   	}

	                if ($data[16] != -32768) {$header->setLatitude($data[16],$lat_fmt);}
 
    	            if ($debug2) {print "Lat = Data(16): xxx $data[16] xxx\n";}
        	        if ($debug2) {print "lat_fmt: xxx $lon_fmt xxx\n";}

            	    #----------
                	# Altitude
	                #----------
    	            if ($data[6] != -32768) {$header->setAltitude($data[6],"m");} 

        	        last;
            	} #data[14/15] > -32768 - Not MISSING

	        } # firstDataLine - only process first data line for the header info; Skip rest

    	} # else Data lines
    } # foreach line

    # -----------------------------------------------------------------------
    # Extract the ACTUAL RELEASE date and time information from the file name
    # NOW:: Expect file name structure: UMBC_RS41_yyyymmdd_hhmmUT.dat .      HARDCODED
    #
    # WAS:: Expect file name structure: HUBV_RS92SGP_yyyymmdd_hhmmUT.dat .  
    # WAS:::    if ($filename =~ /(\d{4})(\d{2})(\d{2})(\d{2})(\d{2})/)
    # -----------------------------------------------------------------------
    my $date;
    my $time;

    # if ($filename =~ /(\d{4})(\d{2})(\d{2})_(\d{2})(\d{2})/)
    if ($file =~ /(\d{4})(\d{2})(\d{2})_(\d{2})(\d{2})/)
    {
        my ($yearInfo, $monthInfo, $dayInfo, $hourInfo, $minInfo) = ($1,$2,$3,$4,$5);

        $date = join ", ", $yearInfo, $monthInfo, $dayInfo;
        $time = join "", $hourInfo, ' ', $minInfo, ' 00';

        if ($debug) {print "date is $date\n";print "time is $time\n";}

    } # Pull date and time from file name

    $header->setActualRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
    $header->setNominalRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);

    return $header;
} # parseHeader()
                           
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
	# open(my $FILE,$self->{"RAW_WORK_DIR"}."/".$file) or die("Can't open file: ".$file);   # Make sure working on copy
    my @lines = <$FILE>;
    my $number_lines_in_file = $#lines+1;

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
    my @headerlines = @lines[0..43];
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
    	# Parse the data portion of the input file
    	# ----------------------------------------------------
	    my $index = 0;

	    foreach my $line (@lines)  
    	{
    		$TotalRecProc++;

	        if ($TotalRecProc >= 40)       # HARDCODED
    	    {
				$dataRecProc++;
	        } # Keep track of data lines

    	    if ($debug2) {print "TotalRecProc: $TotalRecProc, dataRecProc: $dataRecProc\n";}

	        #--------------------------------------------
    	    # Ignore the header lines and blank lines.
        	# Note that there tends to be a blank line
	        # at the end of the data. Skip that line, too.
    	    #---------------------------------------------
        	if ($debug2) {print "line: $line\n";}
	        if ($debug) {if ($line =~ /^\s*$/) {print "Line is empty. line = xxx $line xxx\n";} }

    	    if ($index < 40 || $line =~ /^\s*$/)   # HARDCODED to skip first 40 lines Plus any blank lines
        	{
				if ($debug2) {print "Skip header line in array.\n";}
    	        $index++; next; 
        	}
       
	        #------------------------------------------------------------------------------
    	    # Check for blank line particularly at end of file in Greensburg, KS PISA data.
        	# if missing or null or \n or line length not as expected....
	        # This was something checked in the Male, Maldives data.
    	    #------------------------------------------------------------------------------
        	my @data = split(' ',$line);
	        my $record = ClassRecord->new($WARN,$file);  

    	    if ($debug2) { print "Parse Data Record:: Temp, TD, RH, Winds, etc.\n";}
        	
            # --------------------------------------
			# Time     $data[0]      WindDir  $data[10]
			# Temp     $data[2]      WindSpd  $data[11]
			# RH       $data[3]      Elev.    $data[12]
			# Vwnd     $data[4]      Azimuth  $data[13]
			# Uwnd     $data[5]      Lon      $data[15]
			# Alt      $data[6]      Lat      $data[16]
			# Pres     $data[7]
			# Dewpt    $data[8]
        	
	        #---------------------------------------------------
   		    # missing values are -32768 - Assumption! HARDCODED
	        #---------------------------------------------------
    	    $record->setTime($data[0]);

        	$record->setPressure($data[7],"mb") if ($data[7] != -32768);


	        #---------------------------------------------------------
    	    # $record->setTemperature($data[2],"C") if ($data[2] != -32768);    
        	# Temp and Dewpt are in Kelvin.  C = K - 273.15
	        #---------------------------------------------------------
	        $record->setTemperature(($data[2]-273.15),"C") if ($data[2] != -32768);    
    	
        	$record->setDewPoint(($data[8]-273.15),"C") if ($data[8] != -32768);

	        $record->setRelativeHumidity($data[3]) if ($data[3] != -32768);

    	    $record->setUWindComponent($data[5],"m/s") if ($data[5] != -32768);

        	$record->setVWindComponent($data[4],"m/s") if ($data[4] != -32768);

	        $record->setWindSpeed($data[11],"m/s") if ($data[11] != -32768);

    	    $record->setWindDirection($data[10]) if ($data[10] != -32768);

        	#--------------------------------------------------
	        # get the lat/lon data. MISSING value = -32768     
    	    #--------------------------------------------------
	        if ($data[15] != -32768) 
    	    {
				$record->setLongitude($data[15],$self->buildLatlonFormat($data[15]));

        	    if ($debug2) {print "Lon = Data(15): xxx $data[15] xxx\n";}

	        } # if lon not missing

    	    if ($data[16] != -32768) 
        	{
				$record->setLatitude($data[16],$self->buildLatlonFormat($data[16]));

    	        if ($debug2) {print "Lat = Data(16): xxx $data[16] xxx\n";}

	        } # if lat not missing

    	    if ($debug2) {print "Latitude:: $data[16] , Longitude:: $data[15], TotalRecProc: $TotalRecProc, dataRecProc:: $dataRecProc \n";}

        	#----------------------------------------------------------
	        # Insert Ele (Elevation Angle) and Azi (Azimuth Angle) data
    	    # For setVariableValue(index, value):  
        	# index (1) is Ele column, index (2) is Azi column.
	        #----------------------------------------------------------
    	    $record->setVariableValue(1, $data[13]) if ($data[13] != -32768);   # New El variable
	       	$record->setVariableValue(2, $data[12]) if ($data[12] != -32768);   # AZ variable
    	
        	$record->setAltitude($data[6],"m") if ($data[6] != -32768);     # AKA Height in raw data.

	        #-------------------------------------------------------
    	    # Calculate the ascension rate which is the difference
        	# in altitudes divided by the change in time. Ascension
	        # rates can be positive, zero, or negative. But the time
    	    # must always be increasing (the norm) and not missing.
        	#
	        # Only save off the next non-missing values.
    	    # Ascension rates over spans of missing values are OK.
        	#-------------------------------------------------------
	        if ($debug) 
    	    { 
        	    my $time = $record->getTime(); 
            	my $alt = $record->getAltitude(); 
	            print "\nNEXT Line: prev_time, rec Time, prev_alt, rec Alt:: $prev_time, $time, $prev_alt, $alt\n";
    	    }

        	if ($prev_time != 9999  && $record->getTime()     != 9999  &&
            	  $prev_alt  != 99999 && $record->getAltitude() != 99999 &&
	              $prev_time != $record->getTime() ) 
    	    {
        	    $record->setAscensionRate( ($record->getAltitude() - $prev_alt) /
                	                        ($record->getTime() - $prev_time),"m/s");
            	
	            if ($debug) { print "Calc Ascension Rate.\n"; }
    	    } # If input non-missing, calc asc. rate.

        	#-----------------------------------------------------
	        # Only save off the next non-missing values. 
    	    # Ascension rates over spans of missing values are OK.
        	#-----------------------------------------------------
	        if ($debug) 
    	    { 
        	    my $rectime = $record->getTime(); my $recalt = $record->getAltitude();
            	if ($debug) {print "Try SAVE Line: rec Time, rec Alt:: $rectime, $recalt\n";  }
	        }

    	    if ($record->getTime() != 9999 && $record->getAltitude() != 99999)
        	{
            	$prev_time = $record->getTime();
            	$prev_alt = $record->getAltitude();

	            if ($debug) { print "Current rec has valid Time and Alt. Save as previous.\n"; }
    	    } # save next non-missing vals

        	#-----------------------------------
	        # Completed the ascension rate data
    	    #-----------------------------------
        	

			push(@record_list, $record);
	        # printf($OUT $record->toString());
        }

	    # --------------------------------------------------
		# Remove the last records in the file that are 
	    # descending (ascent rate is negative)
		# --------------------------------------------------
		foreach my $last_record (reverse(@record_list))
		{
		    if (($last_record->getAscensionRate() < 0.0) ||
			    ($last_record->getAscensionRate() == 999.0))
		    {
			    undef($last_record);
	    	} 
		    else 
		    {
			    last;
		    }
		}
	    #-------------------------------------------------------------
    	# Print the records to the file.
		#-------------------------------------------------------------
		foreach my $rec(@record_list) 
		{
	    	print ($OUT $rec->toString()) if (defined($rec));
		}	
	} # if defined ($header)

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


##-----------------------------------------------------
# @signature void readDataFiles()
# <p>Read in the files from the raw data directory and 
# convert each into an # ESC formatted file. 
##-----------------------------------------------------
sub readDataFiles {
    my ($self) = @_;

    opendir(my $RAW,$self->{"RAW_DIR"}) or die("Can't read raw directory ".$self->{"RAW_DIR"});

    #------------------------------------------------------------------------
    # Input file names must be of the form: UMBC_RS41_2015MMDD_hhmmUT.dat 
    #   where UMBC_RS41 is the call sign for the Greensburg, KS, yyyy = year,
    # mm = month, dd = day, hh = hour, mm = minute, and "dat" is the
    # suffix. This is the exact form. All files with names of this
    # form will be processed.
    #------------------------------------------------------------------------

    #HARD-CODED
    my @files = grep(/^UMBC_RS41/,sort(readdir($RAW)));

    closedir($RAW); 
    
	open($WARN,">".$self->{"WARN_LOG"}) or die("Can't create ".$self->{"WARN_LOG"});

	# printf("Ready to read the files\n");
    foreach my $file (@files) 
    {
        $self->parseRawFile($file);   # Process each data file!
        if ($debug2) {print "After parseRawFile() - TotalRecProc: $TotalRecProc, dataRecProc: $dataRecProc\n";}

        #----------------------------------------------------
		$TotalRecProc = 0;
       	$dataRecProc = 0;

    } # process each file 

    close($WARN);

} # readDataFiles()

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
} # trim()
