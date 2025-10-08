#! /usr/bin/perl -w

##Module---------------------------------------------------------------------
# <p>The GLASS_SND_Converter converts data from the GLASS site into the
# CLASS format.</p>
#
# @author Joel Clawson
# @version NAME This was originally created for NAME.
##Module---------------------------------------------------------------------
package GLASS_SND_Converter;
use strict;
use lib "/work/software/NAME/library/conversion_modules/Version2";
use ClassSounding;
use ElevatedStationMap;
use Station;

my ($WARN);
&main();

sub getNetworkName { return "GLASS"; }
sub getOutputDirectory { return "../output"; }
sub getProjectName { return "NAME"; }
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
    my $converter = GLASS_SND_Converter->new();
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
# @signature GLASS_SND_Converter new()
# <p>Create a new converter object.</p>
#
# @output $self The new converter.
##---------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = $invocant || ref($invocant);
    bless($self,$class);

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
    my ($STN, $SUMMARY);

    open($STN, ">".$self->getStationFile()) || 
	die("Cannot create the ".$self->getStationFile()." file\n");
    foreach my $station ($self->{"stations"}->getAllStations()) {
        print($STN $station->toQCF_String()) if ($station->getBeginDate !~ /^9+$/);
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

    # Pull out the raw header information to generate the new one.
    <$FILE>;
    my (undef(),$stn_id,undef(),$stn_name) = split(' ',(split(/:/,<$FILE>))[1]);
    my ($stn_src,$stn_type,$sonde_type,$sonde_fmt) = split(' ',(split(/:/,<$FILE>))[1]);
    my ($lon_deg,$lon_min,$lat_deg,$lat_min,$lon,$lat,$elev) =
	split(' ',(split(/:/,<$FILE>))[1]);
    my @act_release = split(/,/,join(',',(split(/:/,<$FILE>))[1..3]));
    my ($sonde_id) = trim((split(/:/,<$FILE>))[1]);
    my $comments = "";
    for (my $i = 0; $i < 6; $i++) {
	my $line = <$FILE>;
	if ($line =~ /system operator/i) { $comments = trim((split(/:/,$line))[1]); }
    }
    my ($var_name1,$var_name2) = (split(' ',<$FILE>))[12..13];
    my ($var_unit1,$var_unit2) = (split(' ',<$FILE>))[12..13];
    <$FILE>;
    my $nom_time = sprintf("%02d:00",($act_release[3] - ($act_release[3] % 3)));
    my $nom_date = sprintf("%04d/%02d/%02d",@act_release[0..2]);
    ($nom_date,$nom_time) = Conversions::adjustDateTime($nom_date,$nom_time,0,3,0);


#    printf("%s: %s %s %s\n",$stn_id,$stn_name,$stn_src,$stn_type);
#    printf("%s %s %s\n",$lat,$lon,$elev);
#    printf("%04d/%02d/%02d %02d:%02d:%02d\n",@act_release);
#    printf("Sonde: %s %s %s\n",$sonde_type,$sonde_fmt,$sonde_id);
#    printf("Variable Columns: (%s %s) (%s %s)\n",$var_name1,$var_unit1,$var_name2,$var_unit2);


    # Define a new station if needed or update the existing one.
    if ($self->{"stations"}->hasStation($stn_id,getNetworkName(),$lat,$lon,$elev)) {
	$station = $self->{"stations"}->getStation($stn_id,getNetworkName(),$lat,$lon,$elev);
    } else {
	$station = Station->new($stn_id,getNetworkName());
	$station->setStationName(sprintf("%s MX",$stn_name));

	my $lat_fmt = $lat < 0 ? "-" : "";
	while (length($lat_fmt) < length($lat)) { $lat_fmt .= "D"; }
	my $lon_fmt = $lon < 0 ? "-" : "";
	while (length($lon_fmt) < length($lon)) { $lon_fmt .= "D"; }

	$station->setLatitude($lat,$lat_fmt);
	$station->setLongitude($lon,$lon_fmt);
	$station->setElevation($elev,"m");
	$station->setLatLongAccuracy(2);

	$station->setCountry("MX");
	$station->setStateCode("XX");
	$station->setReportingFrequency("6 hourly");
	$station->setNetworkIdNumber(15);
	$station->setPlatformIdNumber(309);

	$self->{"stations"}->addStation($station);
    }
    $station->insertDate($nom_date);

    open(my $OUT,sprintf(">%s/%s_%04d%02d%02d%02d%02d.cls",getOutputDirectory(),$stn_id,
			 @act_release[0..4])) or die("Can't open output file.\n");

    # Print out the header lines.
    printf($OUT "Data Type:                         %s %s\n",$stn_src,$stn_type);
    printf($OUT "Project ID:                        %s\n",getProjectName());
    printf($OUT "Release Site Type/Site ID:         %s %s MX\n",$stn_id,$stn_name);
    printf($OUT "Release Location (lon,lat,alt):    %s %s %s %s %.5f, %.5f, %.1f\n",
	   $lon_deg,$lon_min,$lat_deg,$lat_min,$lon,$lat,$elev);
    printf($OUT "UTC Release Time (y,m,d,h,m,s):    %04d, %02d, %02d, %02d:%02d:%02d\n",
	   @act_release);
    printf($OUT "Radiosonde Manufacturer:           %s %s\n",$sonde_fmt,$sonde_type);
    printf($OUT "Radiosonde Serial Number:          %s\n",$sonde_id);
    printf($OUT "System Operator/Comments:          %s\n",$comments);
    printf($OUT "/\n/\n/\n");
    printf($OUT "Nominal Release Time (y,m,d,h,m,s):%04d, %02d, %02d, %s:00\n",
	   split(/\//,$nom_date),$nom_time);
    printf($OUT " Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat   Rng    Az    Alt    Qp   Qt   Qrh  Qu   Qv   QdZ\n");
    printf($OUT "  sec    mb     C     C     %s     m/s    m/s   m/s   deg   m/s      deg     deg    km   deg     m    code code code code code code\n","%");
    printf($OUT "------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----\n");

    # Print out the data.
    foreach my $line (<$FILE>) {
	chomp($line);
	my @data = split(' ',$line);

	# Convert missing values.
	$data[5]  = 9999.0 if ($data[5]  == 999.0);
	$data[6]  = 9999.0 if ($data[6]  == 999.0);
	$data[9]  =  999.0 if ($data[9]  ==  99.0 || length($data[9]) > 5);
	$data[10] = 9999.0 if ($data[10] == 999.0);

	# Convert flags to missing for missing values.
	$data[15] = 9.0 if ($data[1] == 9999.0);
	$data[16] = 9.0 if ($data[2] ==  999.0);
	$data[17] = 9.0 if ($data[4] ==  999.0);
	$data[18] = 9.0 if ($data[5] == 9999.0);
	$data[19] = 9.0 if ($data[6] == 9999.0);
	$data[20] = 9.0 if ($data[9] ==  999.0);

	printf($OUT "%6.1f %6.1f %5.1f %5.1f %5.1f %6.1f %6.1f %5.1f %5.1f %5.1f %8.3f %7.3f %5.1f %5.1f %7.1f %4.1f %4.1f %4.1f %4.1f %4.1f %4.1f\n",@data);
    }

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
