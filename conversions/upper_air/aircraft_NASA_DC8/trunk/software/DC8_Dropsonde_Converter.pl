#! /usr/bin/perl -w

##Module---------------------------------------------------------------------
# <p>The DC8_Dropsonde_Converter converts EOL-format dropsonde data 
#    from the NASA DC8 Aircraft into the EOL Sounding Composite
#    (ESC) format.</p>
# 
# @author Linda Echo-Hawk 2012/01/09
# @version PREDICT
#          The DC8_Dropsonde_Converter.pl script was adapted from the 
#          EOL_Dropsonde_Converter.pl script for ITOP.
#          - software assumes raw data have *.eol extension
#          - NOTE: Properties file was adjust to have 
#                   MAXIMUM_QUESTIONABLE_ASCENT_RATE=20.0
#                   changed to 25.0 per Scot L.
#
# @author Linda Echo-Hawk 2011/05/18
# @version ITOP 
#          The EOL_Dropsonde_Converter.pl script was completely updated
#          and revised to use the ClassHeader and ClassRecord modules.  
# BEWARE:  This s/w assumes the raw input data has a *.Wwind extension and
#          may need to be modified if that is not the case.
#          - Search for HARD-CODED to change project specific values.
#
##Module---------------------------------------------------------------------
package DC8_Dropsonde_Converter;
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
use SimpleStationMap;
use Station;

my ($WARN);
my ($SUMMARY);

printf "\nDC8_Dropsonde_Converter began on ";print scalar localtime;printf "\n";
&main();
printf "\nDC8_Dropsonde_Converter ended on ";print scalar localtime;printf "\n";    



##---------------------------------------------------------------------------
# @signature void main()
# <p>Execute the script to convert the data.</p>
##---------------------------------------------------------------------------
sub main {                        
    my $converter = DC8_Dropsonde_Converter->new();
    $converter->convert();
}

##---------------------------------------------------------------------------
# @signature void convert()
# <p>Convert the raw data into the CLASS format.</p>
##---------------------------------------------------------------------------
sub convert {
    my $self = shift;

    mkdir($self->{"OUTPUT_DIR"}) unless (-e $self->{"OUTPUT_DIR"});
    mkdir($self->{"FINAL_DIR"}) unless (-e $self->{"FINAL_DIR"});

    open($WARN,">".$self->{"WARN_LOG"}) or die("Can't open warning file.\n");

    $self->readRawDataFiles();
    $self->printStationFiles();

    close($WARN);
}

##---------------------------------------------------------------------------
# @signature DC8_Dropsonde_Converter new()
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
    $self->{"PROJECT"} = "PREDICT"; 
    $self->{"NETWORK"} = "NASA_DC8";
    
    $self->{"FINAL_DIR"} = "../final";
    $self->{"OUTPUT_DIR"} = "../output";
    $self->{"RAW_DIR"} = "../raw_data";
	
	
    $self->{"STATION_FILE"} = sprintf("%s/%s_%s_sounding_stationCD.out",$self->{"FINAL_DIR"},
                                      $self->clean_for_file_name($self->{"NETWORK"}),
                                      $self->clean_for_file_name($self->{"PROJECT"}));

    $self->{"SUMMARY"} = $self->{"OUTPUT_DIR"}."/station_summary.log";
    $self->{"WARN_LOG"} = $self->{"OUTPUT_DIR"}."/warning.log";

    $self->{"stations"} = SimpleStationMap->new();

    return $self;
}

##---------------------------------------------------------------------------
# @signature void printStationFiles()
# <p>Generate the stationCD.out file and the station summary log for the stations
# in the conversion.</p>
##---------------------------------------------------------------------------
sub printStationFiles {
    my $self = shift;

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

##---------------------------------------------------------------------------
# @signature void readRawDataFiles()
# <p>Read all of the raw data files and convert them.</p>
##---------------------------------------------------------------------------
sub readRawDataFiles {
    my $self = shift;

    opendir(my $RAW,$self->{"RAW_DIR"}) or die("Cannot open raw directory.\n");
    my @files = grep(/\.eol$/,readdir($RAW));
    closedir($RAW);

    
	foreach my $file (@files) {
    open(my $FILE,sprintf("%s/%s",$self->{"RAW_DIR"},$file)) or 
	    die("Can't open file: $file\n");
	
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
    my $station;

    my $header = ClassHeader->new($WARN);
    
	# --------------------------------------------------------------
	# Pull out the raw header information to generate the new one.
	# --------------------------------------------------------------
    # "Data Type:" Header
    my ($type, $dir) = (split('/',(trim((split(/:/,<$FILE>))[1]))));
	# print "TYPE: $type DIR: $dir\n";
	$header->setType($type);
	$header->setReleaseDirection($dir);
	$header->setProject($self->{"PROJECT"});

    # This filehandle call just increments to read the next line
    # of the file (File Format/Version), which we want to skip
    <$FILE>;
    
	# --------------------------------------------------------------
	# "Release Site Type/Site ID:" Header (called Project Name/Platform in raw data)

	my ($stationInfo, $stn_id) = (split(',',(split(/:/,<$FILE>))[1]))[1..2];
	my ($flight,$aircraft) = (split('\/', $stationInfo));
	$flight = trim($flight);
    $stn_id = trim($stn_id);
    $stn_id =~ s/-//g;
	$stn_id = trim($stn_id);

	my $rel_site = sprintf("%s %s/%s\n",trim($aircraft),$stn_id,$flight);
	# print "SITE: $rel_site\n";
	chomp($rel_site);
	$header->setSite($rel_site);

    
	# --------------------------------------------------------------

    # Skip the "Launch Site:" line in the file
    <$FILE>;
    
	# --------------------------------------------------------------
   	
    # "Release Location:" Header
    my ($lon_deg,$lon_min,$lon,$lat_deg,$lat_min,$lat,$elev) =
	split(' ',(split(/:/,<$FILE>))[1]);
    # strip the commas off of these values	
	$lon_min =~ s/,//g;
    $lon =~ s/,//g;
    $lat =~ s/,//g;       
	$header->setLatitude($lat, $self->buildLatlonFormat($lat)) unless ($lat == 999.0);
	$header->setLongitude($lon, $self->buildLatlonFormat($lon)) unless ($lon == 999.0);
	$header->setAltitude($elev, "m") unless ($elev == 999.0);

    # --------------------------------------------------------------
    # UTC Launch Time (y,m,d,h,m,s):     2010, 08, 22, 19:20:56 
    # "UTC Release Time:" Header
    my @act_release = split(/,/,join(',',(split(/:/,<$FILE>))[1..3]));
    foreach my $val (@act_release) { $val = trim($val); }

	my $relDate = sprintf("%04d, %02d, %02d", $act_release[0], $act_release[1], $act_release[2]);
	my $relTime = sprintf("%02d:%02d:%02d", $act_release[3], $act_release[4], $act_release[5]);

    # set nominal time = actual time
    $header->setNominalRelease($relDate, "YYYY, MM, DD", $relTime, "HH:MM:SS", 0); 
    $header->setActualRelease($relDate, "YYYY, MM, DD", $relTime, "HH:MM:SS", 0);  
 

    # -----------------------------------
    
	# RAW: Sonde Id/Sonde Type:     102035023/
	# ESC: Sonde Id:     101715011
    my ($sondeInfo) = trim((split(/:/,<$FILE>))[1]);
	# put the info into an array 
	my @sonde = split(//,$sondeInfo);
	# strip off the '/' at the end
	splice (@sonde, -1);
    # then join the characters back together
	my $sonde_id = join "", @sonde;
    
	if ($sonde_id)
	{
		$header->setLine(6,"Sonde Id:", $sonde_id);
	}

    # -----------------------------------
    
	#  RAW: Reference Launch Data Source/Time:  AFRC WC-130J (ARWO)/19:20:57 
    #  ESC: Reference Launch Data Source/Time: AFRC WC-130J (ARWO)/00:09:22
    # my @launchData = (split(' ',<$FILE>))[4..6];
    my @launchData = (split(' ',<$FILE>));
	my $launch_data;
	if ($launchData[4] =~ /Manual/)
	{
		$launch_data = join " ", $launchData[4], $launchData[5];
	}
	else 
	{
		$launch_data = join " ", $launchData[4], $launchData[5], $launchData[6];
	}
	if ($launch_data)
	{
		$header->setLine(5,"Reference Launch Data Source/Time:", $launch_data);
	}

    # -----------------------------------
	# "System Operator/Comments:" Header
    my $comments = "";                        
    # "Post Processing Comments:" Header
	my $PPcomments = "";
    
	for (my $i = 0; $i < 6; $i++) {
	my $headerline = <$FILE>;
 	if ($headerline =~ /system operator/i) { 
		$comments = trim((split(/:/,$headerline))[1]); 
	}
	$header->setLine(7,"System Operator/Comments:", $comments);

	if ($headerline =~ /Post Processing Comments:/) { 
        my ($com1, $com2) = ((split(/:/,$headerline))[1..2]);
		# print "COM1: $com1  COM2: $com2\n";
		$com1 = trim($com1);
		$com2 = trim($com2);
		$PPcomments = join ":", $com1, $com2;
	}
    $header->setLine(8,"Post Processing Comments:", $PPcomments);

    }

    # Read the rest of the file into the @lines array.
    # This way the data can be "flipped" (order of output 
    # reversed) if necessary.
    my @lines = <$FILE>;
    

    #-----------------------------------------------------------------
    # Set the station information
    #-----------------------------------------------------------------
    # Define a new station if needed or update the existing one.
    if ($self->{"stations"}->hasStation($stn_id,$self->{"NETWORK"})) {
	$station = $self->{"stations"}->getStation($stn_id,$self->{"NETWORK"});
    } else {
	$station = Station->new($stn_id,$self->{"NETWORK"});
	$station->setStationName(sprintf("%s, %s",trim($aircraft),trim($stn_id)));
	$station->setLatLongAccuracy(2);
	$station->setStateCode("XX");
	$station->setReportingFrequency("no set schedule");
	$station->setNetworkIdNumber(99);
    # QCODIAC platform number 361 = "Aircraft, NASA DC-8"
	$station->setPlatformIdNumber(361);
	$station->setMobilityFlag("m");

	$self->{"stations"}->addStation($station);
    }
    # $station->insertDate(sprintf("%04d/%02d/%02d",@act_release),"YYYY/MM/DD");
    $station->insertDate($header->getNominalDate(),"YYYY, MM, DD");
    #-----------------------------------------------------------------

    #-------------------------------------------------
    # Open the output file in the ../output directory.
    #-------------------------------------------------
    my $filename = sprintf("%s_%s_%04d%02d%02d%02d%02d%02d.cls",
	           $self->{"NETWORK"}, $stn_id, 
			   split(/, /,$header->getActualDate()),
			   split(/:/,$header->getActualTime()));
	
	open(my $OUT,sprintf(">%s/%s",$self->{"OUTPUT_DIR"},$filename)) or 
		die("Cannot open output file\n");
	
    # print header to output file
    print($OUT $header->toString());  


    # Print out the data.
    #------------------------------------------------
    # Reverse the lines of the file (@lines) so that
    # that dropsonde data will be in the same order
    # (lowest to highest altitude) as other sounding
    # data (such as upsonde data) for composite sets.
    #
    # IMPORTANT:  If raw data has already been
    # reversed, this step won't be needed.
    #------------------------------------------------ 
    foreach my $line (reverse(@lines)) 
	{
		chomp($line);
	    my $rec = ClassRecord->new($WARN, $filename);
		my @data = split(' ',$line);

		$rec->setTime($data[0]) unless ($data[0]  == -999.00);
        $rec->setPressure($data[4], "hPa")  unless ($data[4] == -999.00); 
        $rec->setTemperature($data[5], "C") unless ($data[5] == -999.00);
		$rec->setDewPoint($data[6], "C") unless ($data[6] == -999.00); 
        $rec->setRelativeHumidity($data[7]) unless ($data[7] == -999.00); 
        $rec->setUWindComponent($data[8], "m/s") unless ($data[8]  == -999.00);
		$rec->setVWindComponent($data[9], "m/s") unless ($data[9]  == -999.00);
        $rec->setWindSpeed($data[10], "m/s") unless ($data[10]  == -999.00);
		$rec->setWindDirection($data[11]) unless ($data[11]  == -999.00);
        $rec->setAscensionRate($data[12], "m/s") unless ($data[12]  == -999.00);
        $rec->setLatitude($data[15],$self->buildLatlonFormat($data[15])) unless ($data[15] == -999.000000);
		$rec->setLongitude($data[14],$self->buildLatlonFormat($data[14])) unless ($data[14] == -999.000000);
		$rec->setAltitude($data[13], "m") unless ($data[13] == -999.00); 
        
	    print $OUT $rec->toString();
    }
    
    close($OUT);
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
