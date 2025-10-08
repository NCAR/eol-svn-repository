#! /usr/bin/perl -w

##Module---------------------------------------------------------------------
# <p>The GTS_FSL_Converter.pl script is used for converting GTS data in
# (ESRL) FSL format to the EOL Sounding Composite (ESC) format.</p>
#
#
# @author Linda Echo-Hawk 21 December 2018
# @version SAVANT 2018 
#      - to convert GTS messages from KDVN to ESC format for SAVANT
#      - Search for "HARD-CODED" to change project related constants.
#      - The platform should be changed to "447 Rawinsonde" from
#        "202 Rawinsonde, GTS" which was used in previous versions.
#      - No other modifications were required.
#
# @author Alley Robinson 19 October 2018
# @version HIWC RADAR 2018 
# 	   - Updated the code to be compatible with the 2018 HIWC RADAR Data files
# 	   - For This particular project, there were two data files that were processed between two different
# 	   time periods. I ran the script twice, based upon the file, and had all output files in one directory.
# 	   - The station list is in the DTS
# 	   - This is converted to ESC format
# 	   - Please read the instructions below as well.
#
# @author Alley Robinson 1 October 2018
# @version HAIC-HIWC Florida 2015
# 	   - Updated the code to be compatible with the HIWC Florida 2015 Data
#	   - This data will be converted to ESC Format
#	   - Please read the history of this code.
#
# @author Alley Robinson 08 January 2018
# @version TCI 2016
#	   - Updated the code to be compatible with the TCI 2016 Data
#	   - After this code runs, the data will be in ESC format. Ask the scientist
#	   if this needs to be converted into EOL format
#	   - If you need EOL format, go edit the script in the EOL_processing folder.
#		That should work for most ESC formatted data, but be sure to update with your project.
#	   - Review the instructions below as well.
#
# @author Linda Echo-Hawk 19 Jan 2016
# @version TCI 2015
#          - Search for "HARD-CODED" to change project related constants.
#          - Reviewed the FSL_format.txt document to confirm that sonde
#            info was up-to-date. NOTE: Scot later asked me to remove the
#            sonde info from the converter because he thought it was not
#            accurate. I commented out that code in case we want to update
#            it as a later date.
#
#
# @author Linda Echo-Hawk 2011_08-15
# @version PREDICT 2010 
#          - Updated function calls
#          - General code cleanup including removing "functions that contain
#            constants" such as getNetworkName().  These are now part of the
#            new() function that creates a converter instance. 
#          - Added new functions:  getMonth(), buildLatlonFormat(), and
#            clean_for_file_name()
#          - Search for "HARD-CODED" to change project related constants.
#
# @author Joel (assumed)
# @version Originally created: Conversion software for the National Weather
#            Service (NWS) GTS radiosonde messages into the ESC sounding format. 
#            Requested from FSL. For RAINEX 2005 and CuPIDO.
#
#
##Module---------------------------------------------------------------------
package GTS_FSL;
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
use ClassHeader;
use ClassRecord;
use ElevatedStationMap;
use Station;

my ($WARN);



printf "\nGTS_FSL_Converter.pl began on ";print scalar localtime;printf "\n";
&main();
printf "\nGTS_FSL_Converter.pl ended on ";print scalar localtime;printf "\n";


##---------------------------------------------------------------------------
# @signature void main()
# <p>Execute the script to convert the data.</p>
##---------------------------------------------------------------------------
sub main {
    my $converter = GTS_FSL->new();
    $converter->convert();
}

##---------------------------------------------------------------------------
# @signature void convert()
# <p>Convert the raw data into the CLASS format.</p>
##---------------------------------------------------------------------------
sub convert {
    my $self = shift;

    mkdir($self->{"OUTPUT_DIR"}) unless (-e $self->{"OUTPUT_DIR"});
    mkdir("../final") unless (-e "../final");

    open($WARN,">".$self->{"WARN_LOG"}) or die("Cannot open warning file.\n");

    $self->readRawDataFiles();
    $self->printStationFiles();

    close($WARN);
}

##---------------------------------------------------------------------------
# @signature EOL_Dropsonde_Converter new()
# <p>Create a new converter object.</p>
#
# @output $self The new converter.
##---------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = $invocant || ref($invocant);
    bless($self,$class);


    # ----------------------------------
    # HARD-CODED project specific values
    # ----------------------------------
    $self->{"PROJECT"} = "SAVANT"; 
    # $self->{"NETWORK"} = "2018_FL_CA_HI_MX_GTS";
    $self->{"NETWORK"} = "KDVN_GTS";
    
    $self->{"FINAL_DIR"} = "../final";
    $self->{"OUTPUT_DIR"} = "../output";
    $self->{"RAW_DIR"} = "../raw_data";
	
    $self->{"STATION_FILE"} = sprintf("%s/%s_%s_sounding_stationCD.out",$self->{"FINAL_DIR"},
                                      $self->clean_for_file_name($self->{"NETWORK"}),
                                      $self->clean_for_file_name($self->{"PROJECT"}));

    $self->{"SUMMARY"} = $self->{"OUTPUT_DIR"}."/station_summary.log";
    $self->{"WARN_LOG"} = $self->{"OUTPUT_DIR"}."/warning.log";


    $self->{"stations"} = ElevatedStationMap->new();

    return $self;
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


##---------------------------------------------------------------------------
# @signature void printSounding()
# <p>Generate the output file for the sounding.</p>
##---------------------------------------------------------------------------
sub printSounding 
{
    my ($self,$filename,$header,$records) = @_;

    my $station = $self->{"stations"}->getStation($header->getId(),$self->{"NETWORK"},$header->getLatitude(),$header->getLongitude(),$header->getAltitude());

    if (!defined($station)) 
	{
	    $station = Station->new($header->getId(),$self->{"NETWORK"});
	    $station->setStationName(sprintf("%s (GTS)", $header->getSite()));
	    $station->setLatitude($header->getLatitude(),$self->buildLatlonFormat($header->getLatitude()));
	    $station->setLongitude($header->getLongitude(),$self->buildLatlonFormat($header->getLongitude()));
	    $station->setElevation($header->getAltitude(),"m");

	    $station->setNetworkIdNumber(99);
        # 202	Rawinsonde, GTS
	    $station->setPlatformIdNumber(202);
	    $station->setReportingFrequency("12 hourly");
	    $station->setLatLongAccuracy(2);

	    $self->{"stations"}->addStation($station);
    }
    
    $station->insertDate($header->getNominalDate(),"YYYY, MM, DD");

    open(my $OUT,sprintf(">%s/%s",$self->{"OUTPUT_DIR"},$filename)) or die("Can't write to $filename\n");

    print($OUT $header->toString());
    
    foreach my $record (@{$records}) 
	{
		print($OUT $record->toString());
    }

    close($OUT);
}

##---------------------------------------------------------------------------
# @signature void printStationFiles()
# <p>Generate the stationCD.out file and the station summary log for the stations
# in the conversion.</p>
##---------------------------------------------------------------------------
sub printStationFiles {
    my $self = shift;
    my ($STN, $SUMMARY);

    open($STN, ">".$self->{"STATION_FILE"}) || 
	die("Cannot create the ".$self->{"STATION_FILE"}." file\n");
    foreach my $station ($self->{"stations"}->getAllStations()) {
        print($STN $station->toString()) if ($station->getBeginDate !~ /^9+$/);
    }
    close($STN);

    open($SUMMARY, ">".$self->{"SUMMARY"}) || 	die("Cannot create the ".$self->{"SUMMARY"}." file.\n");
    print($SUMMARY $self->{"stations"}->getStationSummary());
    close($SUMMARY);
}

##---------------------------------------------------------------------------
# @signature void readRawDataFiles()
# <p>Read all of the raw data files and convert them.</p>
##---------------------------------------------------------------------------
sub readRawDataFiles {
    my $self = shift;

    opendir(my $RAW,$self->{"RAW_DIR"}) or die("Can't open raw data directory\n");
    my @files = grep(/\.txt$/,readdir($RAW));
    closedir($RAW);

    
    foreach my $file (@files) {
	open(my $FILE,sprintf("%s/%s",$self->{"RAW_DIR"},$file)) or die("Can't open file: $file\n");
	
	printf("Processing: %s ...\n",$file);
	
	$self->readRawFile($FILE);
	
	close($FILE);
    }
}

##---------------------------------------------------------------------------
# @signature void readRawFile(FileHandle FILE)
# <p>Read the data in the file handle and print it to an output file.</p>
#
# @input $FILE The file handle holding the raw data.
##---------------------------------------------------------------------------
sub readRawFile {
    my $self = shift;
    my ($FILE) = @_;

    my ($header,$records,$windUnits,$filename);
    my $file_prefix = "";

    foreach my $line (<$FILE>) {
	my @data = split(' ',$line);
    
	if ($data[0] == 254) {
	    $self->printSounding($filename,$header,$records) if defined($header);
		if ($filename)
		{
            print "\tProcessing FILE: $filename\n";
		}

	    # Redefine the data holders for the new sounding.
	    $header = ClassHeader->new($WARN);
	    $records = undef();
	    $windUnits = undef();
	    $filename = undef();


        # ----------------------------------
        # HARD-CODED project specific values
        # ----------------------------------
	    $header->setType("GTS Sounding");
		$header->setReleaseDirection("Ascending");
	    $header->setProject($self->{"PROJECT"});
	    $header->setNominalRelease(sprintf("%04d%02d%02d",$data[4],$self->getMonth($data[3]),
					       $data[2]),"YYYYMMDD",sprintf("%02d",$data[1]),"HH",0);
	} elsif ($data[0] == 1) {
	  if ($data[3] =~ /([\d\.]+[NS])([\d\.]+[EW])/i) {
	    my $lat = $1;
	    my $lon = $2;
	    $line =~ s/$lat$lon/$lat $lon/;
	    @data = split(' ', $line);
	  }

	    my $lat = $data[3] =~ /N$/ ? substr($data[3],0,length($data[3])-1) : -1 * substr($data[3],0,length($data[3])-1);
	    my $lon = $data[4] =~ /E$/ ? substr($data[4],0,length($data[4])-1) : -1 * substr($data[4],0,length($data[4])-1);

        # Site is 5-digit code used in raw data file name, e.g., 61415
	    $header->setSite($data[2]);
		$file_prefix = $data[2];

        $header->setLatitude($lat,$self->buildLatlonFormat($lat));
        $header->setLongitude($lon,$self->buildLatlonFormat($lon));
		$header->setAltitude($data[5],"m");

	    if ($header->getNominalTime() =~ /^00:00/ && $data[6] > 2000 && $data[6] != 99999) {
		my ($date) = adjustDateTime($header->getNominalDate(),"YYYY, MM, DD","00:00","HH:MM",-1,0,0,0);
		$header->setActualRelease($date,"YYYY, MM, DD",sprintf("%04d",$data[6]),"HHMM",0);
	    }

	} elsif ($data[0] == 2) {
	} elsif ($data[0] == 3) {
		# Id is 3 or 4 char abbreviation, e.g., GQPP or CUN.
        # Four stations have "XXX" or unknown.
	    $header->setId($data[1]);
	    if (defined($header->getSite())) {
	      $header->setSite(sprintf("%s %s", $header->getSite(), $header->getId()));
	    }
		
		# ----------------------------------------------------
		# These values are from older projects and are
		# probably not correct for this project
		# ----------------------------------------------------
	    # if ($data[2] == 10) { $header->setLine(5,"Sonde Type:","VIZ \"A\""); }
	    # elsif ($data[2] == 11) { $header->setLine(5,"Sonde Type:","VIZ \"B\""); }
	    # elsif ($data[2] == 12) { $header->setLine(5,"Sonde Type:","Space Data Corp."); }
	    # elsif ($data[2] == 51) { $header->setLine(5,"Sonde Type:","VIZ-B2 (USA)"); }
	    # elsif ($data[2] == 52) { $header->setLine(5,"Sonde Type:","Vaisala RS80-57H"); }
	    # elsif ($data[2] == 99999) { }
	    # else { die("Unknown sonde type: ".$data[2]."\n".$line."\n"); }

	    if ($data[3] eq "ms") { $windUnits = "ms"; }
	    elsif ($data[3] eq "kt") { $windUnits = "knot"; }
	    else { die("Unknown wind units: ".$data[3]."\n"); }
	} elsif (4 <= $data[0] && $data[0] <= 9) {
	    if (!defined($filename)) {
		$filename = sprintf("%s_%04d%02d%02d%02d%02d.cls",$file_prefix,
				    split(", ",$header->getActualDate()),
				    split(":",$header->getActualTime()));
	    }

	    my $record = ClassRecord->new($WARN,$filename);

	    if ($data[0] == 9) {
		$record->setTime(0);

	    $record->setLatitude($header->getLatitude(),$self->buildLatlonFormat($header->getLatitude()));
	    $record->setLongitude($header->getLongitude(),$self->buildLatlonFormat($header->getLongitude()));
	    }

	    $record->setPressure(trim($data[1])/10,"mbar") unless (trim($data[1]) == 99999);
	    $record->setAltitude(trim($data[2]),"m") unless (trim($data[2]) == 99999);
	    $record->setTemperature(trim($data[3])/10,"C") unless (trim($data[3]) == 99999);
	    $record->setDewPoint(trim($data[4])/10,"C") unless (trim($data[4]) == 99999);
	    $record->setWindDirection(trim($data[5])) unless (trim($data[5]) == 99999);
	    if ($windUnits eq "ms") {
		$record->setWindSpeed(trim($data[6])/10,"m/s") unless (trim($data[6]) == 99999);
	    } elsif ($windUnits eq "knot") {
		$record->setWindSpeed(trim($data[6]),"knot") unless (trim($data[6]) == 99999);
	    }

	    push(@{$records},$record);
	} else {
	    die("Unknown value ".$data[0]." in first column of data line!\n");
	}
    }
    print "\tProcessing FILE: $filename\n";
    $self->printSounding($filename,$header,$records);
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

##---------------------------------------------------------------------------
# @signature String trim(String line)
# <p>Remove surrounding white space of a String.</p>
# 
# @input $line The String to trim.
# @output $line The trimmed line.
##---------------------------------------------------------------------------
sub trim {
    my $line = shift;
    $line =~ s/^\s+//g;
    $line =~ s/\s+$//g;
    return $line;
}
