#! /usr/bin/perl -w

##Module---------------------------------------------------------------------
# <p>The MIST_Driftsonde_Converter converts MIST Driftsonde data from the 
# EOL format into the EOL Sounding Composite (ESC) format.</p>
#
# @author Linda Echo-Hawk 2009/09/24
# @version T-PARC This was originally created for T-PARC, and is based on
#                 the EOL_Dropsonde_Converter.pl.
# BEWARE:  This s/w assumes the raw input data has a *QC.eol.cls extension,
#          e.g., D20080920_115507.TF12d23_04E37246QC.eol.cls,
#          where "TF12d23" is the flight/drift information and
#          "04E37246" is the MIST Sonde Id.
##Module---------------------------------------------------------------------
package MIST_Driftsonde_Converter;
use strict;
use lib "/net/work/software/RICO/library/conversion_modules/Version3";

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
printf "\nMIST_Driftsonde_Converter began on ";print scalar localtime;printf "\n";
&main();
printf "\nMIST_Driftsonde_Converter ended on ";print scalar localtime;printf "\n";    

my $filename;

sub getNetworkName { return "MIST_Driftsonde"; }
sub getOutputDirectory { return "../output"; }
sub getProjectName { return "T-PARC"; }
sub getRawDataDirectory { return "../raw_data"; }
sub getStationFile { return sprintf("../final/%s_%s_stationCD.out",
				    getNetworkName(),getProjectName()); }
sub getSummaryFile { return "../output/station_summary.log"; }
sub getWarningFile { return "../output/warning.log"; }

##---------------------------------------------------------------------------
# @signature void main()
# <p>Execute the script to convert the data.</p>
##---------------------------------------------------------------------------
sub main {                        
    my $converter = MIST_Driftsonde_Converter->new();
    $converter->convert();
}

##---------------------------------------------------------------------------
# @signature void convert()
# <p>Convert the raw data into the CLASS format.</p>
##---------------------------------------------------------------------------
sub convert {
    my $self = shift;

    mkdir(getOutputDirectory()) unless (-e getOutputDirectory());
    mkdir("../final") unless (-e "../final");

    open($WARN,">".getWarningFile()) or die("Cannot open warning file.\n");

    $self->readRawDataFiles();
    $self->printStationFiles();

    close($WARN);
}

##---------------------------------------------------------------------------
# @signature MIST_Driftsonde_Converter new()
# <p>Create a new converter object.</p>
#
# @output $self The new converter.
##---------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = $invocant || ref($invocant);
    bless($self,$class);

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
    my ($STN, $SUMMARY);

    open($STN, ">".$self->getStationFile()) || 
	die("Cannot create the ".$self->getStationFile()." file\n");
    foreach my $station ($self->{"stations"}->getAllStations()) {
        print($STN $station->toString()) if ($station->getBeginDate !~ /^9+$/);
    }
    close($STN);

    open($SUMMARY, ">".$self->getSummaryFile()) || 
	die("Cannot create the ".$self->getSummaryFile()." file.\n");
    print($SUMMARY $self->{"stations"}->getStationSummary());
    close($SUMMARY);
}

##---------------------------------------------------------------------------
# @signature void readRawDataFiles()
# <p>Read all of the raw data files and convert them.</p>
##---------------------------------------------------------------------------
sub readRawDataFiles {
    my $self = shift;

    opendir(my $RAW,getRawDataDirectory()) or die("Cannot open raw data directory\n");
    my @files = grep(/\.cls$/,readdir($RAW));
    closedir($RAW);

    foreach my $file (@files) {
	open(my $FILE,sprintf("%s/%s",getRawDataDirectory(),$file)) or 
	    die("Can't open file: $file\n");
	
	printf("Processing: %s ...\n",$file);
    $filename = $file; 
	$self->readRawFile($FILE, $filename);
	
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


    # ---------------------------------------------
    # Extract the driftsonde flight information
    # and Mistsonde ID from the file name
    # BEWARE: Expects filename to be similar to:
    # D20080920_115507.TF12d23_04E37246QC.eol.cls
    # ---------------------------------------------
    # print "file name = $filename\n"; 
	my $driftInfo = (split(/\./,$filename))[1];
	$driftInfo =~ s/QC//g;
    my $driftInfoCopy = $driftInfo;
    my $MISTsondeId = (split (/_/,$driftInfoCopy))[1]; 

	# print "driftInfo = $driftInfo\n";
	# print "MISTsonde ID  = $MISTsondeId\n\n";
    # ----- Example Output ------------------------------------
	# file name = D20080920_115507.TF12d23_04E37246QC.eol.cls
	# driftInfo = TF12d23_04E37246
	# MISTsonde ID  = 04E37246

	# file name = D20080927_144800.TF16d27_04E36ECEQC.eol.cls
	# driftInfo = TF16d27_04E36ECE
	# MISTsonde ID  = 04E36ECE
 
    # ------------------------------------------------------------
    # Pull out the raw header information to generate the new one.
    # ------------------------------------------------------------
    # "Data Type:" Header  (called "Data Type/Direction" in raw data) 
    my ($type) = trim((split(/:/,<$FILE>))[1]);

    # This filehandle call just increments to read the next line
    # of the file (File Format/Version), which we want to skip
    <$FILE>;
    
	# "Release Site Type/Site ID:" Header (called Project Name/Platform in raw data)
 	my ($stationInfo, $flight) = (split(',',(split(/:/,<$FILE>))[1]))[1..2];
	chomp($flight);

	# print "Station:  $stationInfo\n";

	my ($platform,$stn_id) = (split('\/', $stationInfo));
	

    # my $stn_id = (split(' ', $stationInfo))[1];
    #$stn_id = trim($stn_id);
	$platform = trim ($platform);
    # $stn_id =~ s/,//g;       


    # Skip the "Launch Site:" line in the file
    <$FILE>;   	

    # "Release Location:" Header (called "Launch Location" in raw data) 
    my ($lon_deg,$lon_min,$lon,$lat_deg,$lat_min,$lat,$elev) =
	split(' ',(split(/:/,<$FILE>))[1]);
    # strip the commas off of these values	
	$lon_min =~ s/,//g;
    $lon =~ s/,//g;
    $lat =~ s/,//g;       


    # "UTC Release Time:" Header (called "UTC Launch Time" in raw data) 
    my @act_release = split(/,/,join(',',(split(/:/,<$FILE>))[1..3]));
    foreach my $val (@act_release) { $val = trim($val); }
 
    # Sonde Id/Sonde Type:
    my ($sonde_id) = trim((split(/:/,<$FILE>))[1]);

    # "Reference Launch Data Source/Time:" Header
 
    my ($launch_data) = trim((split(/:/,<$FILE>))[1]); 
    # my @launchData = (split(' ',<$FILE>))[4..6];
	# my $launch_data = join " ", @launchData;    

    # "System Operator/Comments:" Header
    my $comments = "";                        
    # "Post Processing Comments:" Header
	my $PPcomments = "";

    for (my $i = 0; $i < 6; $i++) {
	my $line = <$FILE>;
 	if ($line =~ /system operator/i) { 
		$comments = trim((split(/:/,$line))[1]); 
	}
	if ($line =~ /Post Processing Comments:/) { 
		$PPcomments = trim((split(/:/,$line))[1]);
	}
    }

    # ------------------------------------------------------------
    # Read the rest of the file into the @lines array.
    # This way the data can be "flipped" (order of output 
    # reversed) if necessary, as was the case for T-PARC.
    # ------------------------------------------------------------
    my @lines = <$FILE>;
    

    # my ($var_name1,$var_name2) = (split(' ',<$FILE>))[12..13];
    # my ($var_unit1,$var_unit2) = (split(' ',<$FILE>))[12..13];
    # <$FILE>;
 
#-------------------------------------------
# This section commented out by J. Clawson.
#-------------------------------------------
#    foreach my $val (@act_release) { $val = trim($val); }
#    printf("%s\n",join(" ",@act_release));

#    printf("%s: %s %s\n",$stn_id,$aircraft,$flight);
#    printf("%s %s %s\n",$lat,$lon,$elev);
#    printf("%04d/%02d/%02d %02d:%02d:%02d\n",@act_release);
#    printf("Sonde: %s\n",$sonde_id);
#    printf("Variable Columns: (%s %s) (%s %s)\n",$var_name1,$var_unit1,$var_name2,$var_unit2);

#    die();


    # ------------------------------------------------------------
    # Define a new station if needed or update the existing one.
    # ------------------------------------------------------------
    if ($self->{"stations"}->hasStation($stn_id,getNetworkName())) {
	$station = $self->{"stations"}->getStation($stn_id,getNetworkName());
    } else {
	$station = Station->new($stn_id,getNetworkName());
	# $station->setStationName(sprintf("%s, %s",trim($aircraft),trim($stn_id)));
	$station->setStationName(sprintf("%s, %s",trim($platform),trim($stn_id)));
	$station->setLatLongAccuracy(2);

	# $station->setCountry("XX");
	$station->setCountry("99");
	$station->setStateCode("XX");
	$station->setReportingFrequency("no set schedule");
	$station->setNetworkIdNumber(99);
    # QCODIAC platform number 387 = "MIST_Driftsonde"
	$station->setPlatformIdNumber(387);
	$station->setMobilityFlag("m");

	$self->{"stations"}->addStation($station);
    }
    $station->insertDate(sprintf("%04d/%02d/%02d",@act_release),"YYYY/MM/DD");


    # ------------------------------------------------------------
    # Open the output file and write out the data.
    # ------------------------------------------------------------
    open(my $OUT,sprintf(">%s/%s_%s_%04d%02d%02d%02d%02d.cls",getOutputDirectory(),
	         getNetworkName(), $platform, @act_release[0..4])) 
	         or die("Can't open output file.\n");

    # Print out the header lines.
    printf($OUT "Data Type:                         %s\n",$type);
    printf($OUT "Project ID:                        %s\n",getProjectName());
    printf($OUT "Release Site Type/Site ID:         %s, %s %s\n",trim($platform),$stn_id,$flight);
    printf($OUT "Release Location (lon,lat,alt):    %s %s, %s %s, %.5f, %.5f, %.1f\n",
	   $lon_deg,$lon_min,$lat_deg,$lat_min,$lon,$lat,$elev);
    printf($OUT "UTC Release Time (y,m,d,h,m,s):    %04d, %02d, %02d, %02d:%02d:%02d\n",
	   @act_release);	
    printf($OUT "Post Processing Comments:          %s\n",$PPcomments);	
    printf($OUT "Reference Launch Data Source/Time: %s\n",$launch_data);
    printf($OUT "Sonde Id/Sonde Type:               %s ID %s\n",$sonde_id,$MISTsondeId);
    printf($OUT "System Operator/Comments:          %s\n",$comments);
    printf($OUT "/\n/\n");
    printf($OUT "Nominal Release Time (y,m,d,h,m,s):%04d, %02d, %02d, %02d:%02d:%02d\n",
	   @act_release);
    printf($OUT " Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat    Ele   Azi   Alt    Qp   Qt   Qrh  Qu   Qv   QdZ\n");
    printf($OUT "  sec    mb     C     C     %s     m/s    m/s   m/s   deg   m/s      deg     deg    deg   deg    m    code code code code code code\n","%");
    printf($OUT "------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----\n");

    # Print out the data.
    my @last_data = ();
    #------------------------------------------------
    # Reverse the lines of the file (@lines) so that
    # that dropsonde data will be in the same order
    # (lowest to highest altitude) as other sounding
    # data (such as upsonde data) for composite sets.
    #
    # IMPORTANT:  If raw data has already been
    # reversed, this step won't be needed.
    #------------------------------------------------ 
    foreach my $line (reverse(@lines)) {    
	chomp($line);
	my @data = split(' ',$line);


	# this removes the 3 UTC Time colums at offset 1 
	splice(@data, 1, 3);
	# remove GeoPoAlt column & save it for later
	my @alt = splice(@data, 10, 1);
	# add columns for Ele and Azi
	splice(@data, 12, 0, '999.00');
	splice(@data, 13, 0, '999.00');
	# remove GPSAlt & Wwind (last three columns)
	splice(@data, 14, 3);
	# put GeoPoAlt column here
	push (@data, @alt);
	# add six columns for QC data, initially set to "missing"
	my @qual_data = qw(99.0 99.0 99.0 99.0 99.0 99.0);
	push (@data, @qual_data);

	# Convert missing values.
	$data[0]  =  9999.0 if ($data[0]  == -999.00);  # $data[0] = Time (sec)
	$data[1]  =  9999.0 if ($data[1]  == -999.00);  # $data[1] = Press (mb)
	$data[2]  =   999.0 if ($data[2]  == -999.00);  # $data[2] = Temp (C)
	#$data[3]  =   999.0 if ($data[3]  == -999.00);  # $data[3] = Dewpt (C)
	$data[3]  =   999.0 if ($data[3]  == -999.00 || length($data[3]) > 6);  # $data[3] = Dewpt (C)
    $data[4]  =   999.0 if ($data[4]  == -999.00);  # $data[4] = RH (%)
	$data[5]  =  9999.0 if ($data[5]  == -999.00);  # $data[5] = Ucmp (U Wind Component) (m/s)
	$data[6]  =  9999.0 if ($data[6]  == -999.00);  # $data[6] = Vcmp (V Wind Component) (m/s)
	$data[7]  =   999.0 if ($data[7]  == -999.00);  # $data[7] = spd (Wind Speed) (m/s)
	$data[8]  =   999.0 if ($data[8]  == -999.00);  # $data[8] = dir (Wind Direction) (deg)
	#$data[9]  =   999.0 if ($data[9]  == -999.00 || length($data[9]) > 5);  # $data[9] = Wcmp (Ascent Rate) (m/s)
    # need to account for -23.61 data = 6 chars
	$data[9]  =   999.0 if ($data[9]  == -999.00 || length($data[9]) > 6);  # $data[9] = Wcmp (Ascent Rate) (m/s)
	$data[10] =  9999.0 if ($data[10] == -999.000000);  # $data[10] = Lon (Longitude) (deg) (field width = 8)
	$data[11] =   999.0 if ($data[11] == -999.000000);  # $data[10] = Lat (Latitude) (deg) (field width = 7)
	$data[14] = 99999.0 if ($data[14] == -999.00);  # $data[14] = Altitude (m)


    # -------------------------------------------------
    # Remove the last digit to avoid rounding values 
    # like -99.99 to -100.00
	# (which is larger than the 5 spaces allowed 
    # even when printed with 1 decimal limit:  -100.0)
    # -------------------------------------------------
	if (length($data[3]) > 5)
	{
		# my $len = length($data[3]);
		# print "Dewpoint length = $len\n";
		# print "Dewpt before = $data[3] ";
		my @dewpt = split(//,$data[3]);
		splice (@dewpt, -1);
        $data[3] =join "", @dewpt;
		# print "Dewpt after = $data[3]\n\n";
	}

	# Convert flags to missing for missing values (Add Quality Control info)
	$data[15] = 9.0 if ($data[1] == 9999.0);  # $data[1] = Press
	$data[16] = 9.0 if ($data[2] ==  999.0);  # $data[2] = Temp
	$data[17] = 9.0 if ($data[4] ==  999.0);  # $data[4] = Rel. Humidity
	$data[18] = 9.0 if ($data[5] == 9999.0);  # $data[5] = U Wind Component
	$data[19] = 9.0 if ($data[6] == 9999.0);  # $data[6] = V Wind Component
	$data[20] = 9.0 if ($data[9] ==  999.0);  # $data[9] = Ascent Rate

	# Read the new line (@data) before printing the previous line (@last_data).
	# If the new line does not have the same "Time" as the previous line, 
	# print the previous line.
	if (@last_data && $last_data[0] != $data[0]) {
	    printf($OUT "%6.1f %6.1f %5.1f %5.1f %5.1f %6.1f %6.1f %5.1f %5.1f %5.1f %8.3f %7.3f %5.1f %5.1f %7.1f %4.1f %4.1f %4.1f %4.1f %4.1f %4.1f\n",@last_data);
	    @last_data = @data;
	}
 	# If two lines of data have the same time, the first line is printed, but
	# only after comparing each element of the two lines and replacing any
	# missing data in the first line (@last_data) with valid data from the 
	# second line (@data).  The replacement values are flagged as unchecked
	# if they are QC values.      	
	elsif (@last_data != 0) {
  
        # inform user of the duplicate lines
	    printf("Two data lines have duplicate start times: \n");
        foreach my $datum(@last_data){
            printf ("%6.1f  ",$datum);
        }
        printf("\n");
        foreach my $datum2(@data){
            printf ("%6.1f  ",$datum2);
        }
        printf("\n");           

	    if ($last_data[1] == 9999.0 && $data[1] != 9999.0) {
		$last_data[1] = $data[1];
		$last_data[15] = 99.0;
	    }
	    if ($last_data[2] == 999.0 && $data[2] != 999.0) {
		$last_data[2] = $data[2];
		$last_data[16] = 99.0;
	    }
	    if ($last_data[3] == 999.0 && $data[3] != 999.0) {
		$last_data[3] = $data[3];
	    }
	    if ($last_data[4] == 999.0 && $data[4] != 999.0) {
		$last_data[4] = $data[4];
		$last_data[17] = 99.0;
	    }
	    if ($last_data[5] == 9999.0 && $data[5] != 9999.0) {
		$last_data[5] = $data[5];
		$last_data[18] = 99.0;
	    }
	    if ($last_data[6] == 9999.0 && $data[6] != 9999.0) {
		$last_data[6] = $data[6];
		$last_data[19] = 99.0;
	    }
	    if ($last_data[7] == 999.0 && $data[7] != 999.0) {
		$last_data[7] = $data[7];
	    }
	    if ($last_data[8] == 999.0 && $data[8] != 999.0) {
		$last_data[8] = $data[8];
	    }
	    if ($last_data[9] == 999.0 && $data[9] != 999.0) {
		$last_data[9] = $data[9];
		$last_data[20] = 99.0;
	    }
	    if ($last_data[10] == 9999.000 && $data[10] != 9999.000) {
		$last_data[10] = $data[10];
	    }
	    if ($last_data[11] == 999.000 && $data[11] != 999.000) {
		$last_data[11] = $data[11];
	    }
	    if ($last_data[12] == 999.0 && $data[12] != 999.0) {
		$last_data[12] = $data[12];
	    }
	    if ($last_data[13] == 999.0 && $data[13] != 999.0) {
		$last_data[13] = $data[13];
	    }
	    if ($last_data[14] == 99999.0 && $data[14] != 99999.0) {
		$last_data[14] = $data[14];
	    }

        # inform user of final output for the duplicate lines
	    printf("Resulting output for the duplicate lines: \n");
        foreach my $lastData(@last_data){
            printf ("%6.1f  ",$lastData);
        }    
         printf("\n\n");   

	} else {
	    @last_data = @data;
	}
    }
    printf($OUT "%6.1f %6.1f %5.1f %5.1f %5.1f %6.1f %6.1f %5.1f %5.1f %5.1f %8.3f %7.3f %5.1f %5.1f %7.1f %4.1f %4.1f %4.1f %4.1f %4.1f %4.1f\n",@last_data);
    
    close($OUT);
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
