#! /usr/bin/perl -w
# Program asc2cls.pl
# program creates class file foreach wban and its associated UPA data
# contained in converted office note message
# constants

##Module--------------------------------------------------------------------------
# <p>The pacs-gts_converter.pl script converts monthly ASCII data files of soundings
# for PACS into the CLASS format.</p>
#
# @author Joel Clawson
# @version Jan2004 This is a refactored version of the asc2cls.pl script used by Darren
# Gallant.  The changes include the renaming of variables, separating functionality into
# separate functions, removing code that never gets run, and adding documentation.
##Module--------------------------------------------------------------------------
use strict;

use Time::Local;
my $pi = 3.14159265;
my $radian = 180.0/$pi;
my $degree = 1/$radian;
my $ESO = 6.1121;
my $K = 273.15;
my $R = 287.04;
my $Rv = 461.5;
my $EPS = .622;
my $G = 9.80665;
my $TRUE = 1;
my $FALSE = 0;

my $data_type = "GTS sounding";
my $project_id = "PACS";
my $KNOTS = $TRUE;
my @header;
$header[0] = "Data Type:                         ";
$header[1] = "Project ID:                        ";
$header[2] = "Release Site Type/Site ID:         ";
$header[3] = "Release Location (lon,lat,alt):    ";
$header[4] = "UTC Release Time (y,m,d,h,m,s):    ";
$header[5] = "Ascension No:                      ";
$header[6] = "Nominal Release Time (y,m,d,h,m,s):";
$header[7] = "System Operator/Comments:          ";
$header[8] = "Additional comments:               ";
$header[9] = "Ascii Input File:                  ";

my @line;
$line[0] = " Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat   Ele   Azi    Alt    Qp   Qt   Qrh  Qu   Qv   QdZ";
$line[1] =  "  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     m    code code code code code code";
$line[2] ="------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----";

my %MONTHS = (
	   january => {
	       FILE => "1",
	       MM   => "01",
	       DAYS => "31",
	       SPANISH => "encro",
	       ABREV   => "jan",
               NEXT    => "february",
	   },
	   february => {
	       FILE => "2",
	       MM   => "02",
	       DAYS => "28",
	       SPANISH => "feb",
	       ABREV   => "feb",
	       NEXT    => "march",
	   },
	   march => {
	       FILE => "3",
	       MM   => "03",
	       DAYS => "31",
	       SPANISH => "marzo",
	       ABREV   => "mar",
	       NEXT    => "april",
	   },
	   april => {
	       FILE => "4",
	       MM   => "04",
	       DAYS => "30",
	       SPANISH => "abril",
	       ABREV   => "apr",
	       NEXT    => "may",
	   },
	   may => {
	       FILE => "5",
	       MM   => "05",
	       DAYS => "31",
	       SPANISH => "mayo",
	       ABREV   => "may",
	       NEXT    => "june",
	   },
	   june => {
	       FILE => "6",
	       MM   => "06",
	       DAYS => "30",
	       SPANISH => "junio",
	       ABREV   => "jun",
	       NEXT    => "july",
	   },
	   july => {
	       FILE => "7",
	       MM   => "07",
	       DAYS => "31",
	       SPANISH => "julio",
	       ABREV   => "jul",
	       NEXT    => "august",
	   },
	   august => {
	       FILE => "8",
	       MM   => "08",
	       DAYS => "31",
	       SPANISH => "agosto",
	       ABREV   => "aug",
	       NEXT    => "september",
	   },
	   september => {
	       FILE => "9",
	       MM   => "09",
	       DAYS => "30",
	       SPANISH => "septiembre",
	       ABREV   => "sep",
	       NEXT    => "october",
	       MISSPELL => "setiembre",
	   },
	   october => {
	       FILE => "a",
	       MM   => "10",
	       DAYS => "31",
	       SPANISH => "octubre",
	       ABREV   => "oct",
	       NEXT    => "november",
	   },
	   november => {
	       FILE => "b",
	       MM   => "11",
	       DAYS => "30",
	       SPANISH => "noviembre",
	       ABREV   => "nov",
	       NEXT    => "december",
	   },
	   december => {
	       FILE => "c",
	       MM   => "12",
	       DAYS => "31",
	       SPANISH => "diciembre",
	       ABREV   => "dec",
	       NEXT    => "january",
	   },
	   );
my @months = keys %MONTHS;
my $main_dir = "../output";
my $stnfile = "UPPERAIR.TXT";

my %DATA;
my %HEADER;
my %WIND;
my %TIME;
my %FILE;
my %stn_info;
&main();

##------------------------------------------------------------------------
# @signature void loadStations()
# <p>Read in the data from the station list file and put into a hash.</p>
##------------------------------------------------------------------------
sub loadStations {
    open(my $STN,"../docs/$stnfile") || die "Can't open $stnfile!\n";
    while(<$STN>){
	if($_ =~ /(\d{5})0[ ]+(.+)[ ]{1,37}(\w{2})[ ]+(.\d+\.\d+)[ ]+(.\d+\.\d+)[ ]+(\d+\.\d+)[ ]+\d{6}[ ]+999912/){
	    $stn_info{$1}{"NAME"} = $2,
	    $stn_info{$1}{"COUNTRY"} = $3;
	    $stn_info{$1}{"LAT"} = $4;
	    $stn_info{$1}{"LON"} = $5;
	    $stn_info{$1}{"ALT"} = $6;
	    @{$FILE{$1}} = ();
	}
    }
    close($STN);
}

##------------------------------------------------------------------------
# @signature void parseMandSigLevel(float press, float alt, float temp, float dewdep, float wdir, float wspd, String outfile)
# <p>Parse the data from a mandatory/significant level raw data line.</p>
#
# @input $press The pressure value.
# @input $alt The altitude value.
# @input $temp The temperature value.
# @input $dewdep The dew point value.
# @input $wdir The wind direction value.
# @input $wspd The wind speed value.
# @input $outfile The output file where the data will eventually be written.
##------------------------------------------------------------------------
sub parseMandSigLevel {
    my ($press,$alt,$temp,$dewdep,$wdir,$wspd,$outfile) = @_;
    my @data_row = ();
    @data_row = &init_line(@data_row);

    if($press != 9999.0 && $press > 0){
	$data_row[1] = $press;
	if($alt != 99999.0){$data_row[14] = $alt;}
	if($temp != 999.0){
	    $data_row[2] = $temp;
	    if($dewdep != 99.9){
		my $dewpt = $temp - $dewdep;
		$data_row[3] = $dewpt unless ($dewpt > 999.9);
		($data_row[3],$data_row[4],$data_row[17]) = &calc_RH($data_row[2],$data_row[3]);
	    }
	}
	if($wspd != 999.0 && $wdir != 999.0){
	    unless($wspd <= 0 && $wdir <= 0){
		
		$data_row[7] = $wspd;$data_row[8] = $wdir;
		($data_row[5],$data_row[6],$data_row[7],$data_row[18],$data_row[19]) = 
		    &calc_UV($data_row[7],$data_row[8]);
	    }
	}

	@data_row = &check_QCflags(@data_row);

	$DATA{$outfile}{$data_row[1]} = &line_printer(@data_row);
    }
}

##------------------------------------------------------------------------
# @signature (int,String) parseSoundingSiteInfo(int wmo, int year, int mth, int day, int hour, int min, float lat, float lon, float elev, int file_open)
# <p>Parse the raw data line that contains the station information for the
# sounding.</p>
#
# @input $wmo The wmo number of the station.
# @input $year The year the sounding was taken.
# @input $mth The month of the year.
# @input $day The day of the month.
# @input $hour The hour of the day.
# @input $min The minute of the hour.
# @input $lat The latitude of the station.
# @input $lon The longitude of the station.
# @input $elev The elevation of the station.
# @input $file_open A flag telling if the sounding file is open.
# @output $file_open The flag stating if the sounding file is open.
# @output $outfile The name of the sounding output file.
##------------------------------------------------------------------------
sub parseSoundingSiteInfo {
    my ($wmo,$year,$mth,$day,$hour,$min,$lat,$lon,$elev,$file_open) = @_;

    # Convert to a 4 digit year.
    $year = sprintf("%02d%02d",$year > 10 ? 19 : 20,$year);

    # Convert the longitude to the correct value.
    $lon = $lon < 180 ? -$lon : 360 - $lon;

    # Check for leap year.
    if($year%4==0 && ($year%100 || $year%400==0)){
	$MONTHS{february}{DAYS} = 29;
    }

    my $outfile;
    if($mth <= 12 && $mth >= 1){
	my $month = &find_mth($mth);#print "$month $DAY $HOUR\n";
	my $mm = $MONTHS{$month}{"MM"};
	if($day > 0 && $day <= $MONTHS{$month}{"DAYS"} && $hour>=0 && $hour<=24){
	    ($file_open,$outfile) = 
		&filename($wmo,$year,$mm,$day,$hour,$min,$lat,$lon,$elev,
			  $file_open,$month) unless $wmo eq "NULL";
	}
    }

    return ($file_open,$outfile);
}

##------------------------------------------------------------------------
# @signture (String,int) parseStationLine(String line)
# <p>Read in the station id from the station line.</p>
#
# @input $line The line containing the station information.
# @output $id The station id.
# @output $file_open A flag stating if the sounding file is open.
##------------------------------------------------------------------------
sub parseStationLine {
    my $line = shift;

    $line =~ /STATION ([\S]+)/;
    my $id = $1;
    if ($id =~ /PSGAL/) { $id = "84008"; }

    return ($id,$FALSE);
}

##------------------------------------------------------------------------
# @signature void parseTropopauseLevel(float press, float temp, float dewdep, float wdir, float wspd, String outfile)
# <p>Parse the data in a raw tropopause data line.</p>
#
# @input $press The pressure value.
# @input $temp The temperature value.
# @input $dewdep The dew point value.
# @input $wdir The wind direction value.
# @input $wspd The wind speed value.
# @input $outfile The name of the output file where the data will be written.
##------------------------------------------------------------------------
sub parseTropopauseLevel {
    my ($press,$temp,$dewdep,$wdir,$wspd,$outfile) = @_;
	    
    my @data_row = ();@data_row = &init_line(@data_row);
    if($press != 9999.0 && $press > 0){
	$data_row[1] = $press;
	if($temp != 999.0){
	    $data_row[2] = $temp;
	    if($dewdep != 99.9 && $dewdep > 0){
		my $dewpt = $temp - $dewdep;
		$data_row[3] = $dewpt unless ($dewpt > 999.9);
		($data_row[3],$data_row[4],$data_row[17]) = 
		    &calc_RH($data_row[2],$data_row[3]);
	    }
	}
	if($wspd != 999.0 && $wdir != 999.0){
	    unless($wspd <= 0 && $wdir <= 0){
		$data_row[7] = $wspd;$data_row[8] = $wdir;
		($data_row[5],$data_row[6],$data_row[7],$data_row[18],$data_row[19]) = 
		    &calc_UV($data_row[7],$data_row[8]);
	    }
	}

	@data_row = &check_QCflags(@data_row);
	$DATA{$outfile}{$data_row[1]} = &line_printer(@data_row);
    }
}

##------------------------------------------------------------------------
# @signature void parseWindByHeight(float alt, float wdir, float wspd, String outfile)
# <p>Parse a raw data line that contains the wind by height data.</p>
#
# @input $alt The altitude value.
# @input $wdir The wind direction value.
# @input $wspd The wind speed value.
# @input $outfile The name of the output file where the data will be written.
##------------------------------------------------------------------------
sub parseWindByHeight {
    my ($alt,$wdir,$wspd,$outfile) = @_;
    
    my @data_row = ();
    @data_row = &init_line(@data_row);
    
    if($alt != 99999.0 && $wspd != 999.0 && $wdir != 999.0){
	unless($wspd <= 0 && $wdir <= 0){
	    $data_row[14] = $alt;
	    $data_row[7] = $wspd;$data_row[8] = $wdir;
	    #&calc_UV($data_row[7],$data_row[8]);

	    ($data_row[5],$data_row[6],$data_row[7],$data_row[18],$data_row[19]) = 
		&calc_UV($data_row[7],$data_row[8]);

	    @data_row = &check_QCflags(@data_row);
	    $WIND{$outfile}{$alt} = &line_printer(@data_row);
	}
    }
}

##------------------------------------------------------------------------
# @signature void main(String[] files)
# <p>Convert the raw PACS-GTS sounding data to the CLASS format.</p>
#
# @input $files A list of the names of the files to be converted.
##------------------------------------------------------------------------
sub main {
    if(@ARGV < 1){
	print "Usage is asc2cls.pl file(s)\n";
	exit;
    }

    mkdir($main_dir) unless (-e $main_dir);

    loadStations();


    my @files = grep(/(|.+)upa(|.+)(|\.gz)$/i,@ARGV);
    foreach my $file (@files) {

	printf("Processing file: %s ...\n",$file);

	$file =~ /(\d{4})(\d{2})/;
	my $dir;
	if ($2 < 7) { $dir = sprintf("%s/%04d01-%04d06",$main_dir,$1,$1); }
	else        { $dir = sprintf("%s/%04d07-%04d12",$main_dir,$1,$1); }

	mkdir($dir) unless (-e $dir);

	my $logfile = sprintf("%s/%04d%02d.log",$dir,$1,$2);

	open(LOG,">>$main_dir/$logfile")||die "Can't open $logfile\n";
	&timestamp;


	my $ASC;
	if($file =~ /\.gz/){
	    open($ASC,"gzcat $file|") || die "Can't open $file\n";
	}else{
	    open($ASC,$file) || die "Can't open $file\n";
	}

	my $line;
	my $outfile;
	my $FILE_OPEN;
	my $wmo;
	while(defined($line = <$ASC>)){
	    if ($line =~ /STATION/) {
		($wmo,$FILE_OPEN) = parseStationLine($line);

	    }elsif($line =~ /^\s*(\d{1,2})[ ]+(\d{1,2})[ ]+(\d{1,2})[ ]+\d+[ ]+(\d+)\.(\d+)[ ]+(.\d+\.\d+)[ ]+(\d+\.\d+)[ ]+(\d+\.)/){

		($FILE_OPEN,$outfile) = parseSoundingSiteInfo($wmo,$1,$2,$3,$4,$5,$6,$7,$8,$FILE_OPEN);
	    }elsif($FILE_OPEN){
		if($line =~ /[ ]+(\d+)[ ]+(\d+\.\d)[ ]+(.\d+\.\d)[ ]+(.\d+\.\d)[ ]+(\d+\.\d)[ ]+(\d+\.\d)[ ]+(\d+\.\d)/){ #Mandatory and Significant level
		    parseMandSigLevel($2,$3,$4,$5,$6,$7,$outfile);
		}elsif($line=~/^[ ]+\d+[ ]+(\d+\.\d)[ ]+(\d+\.\d)[ ]+(\d+\.\d)[ ]+$/){ # Wind by height
		    parseWindByHeight($1,$2,$3,$outfile);
		}elsif($line =~ /[ ]+(\d+)[ ]+(\d+\.\d)[ ]+(.\d+\.\d)[ ]+(\d+\.\d)[ ]+(\d+\.\d)[ ]+(\d+\.\d)/){ #Tropopause level
		    parseTropopauseLevel($2,$3,$4,$5,$6,$outfile);
		} 
	    }
	}
	&write_data;
	close($ASC);
    }
    &timestamp;
    #print "FINI\n";
}

##------------------------------------------------------------------------
# @signature void timestamp()
# <p>Print out the current time to the log file.</p>
##------------------------------------------------------------------------
sub timestamp{
    my($sec,$min,$hour,$mday,$mon,$year,$wday,$julian,$isdst) = gmtime(time);
    my $TIME = sprintf("%02d%s%02d%s%02d",$hour,":",$min,":",$sec);
    my $DATE = sprintf("%02d%s%02d%s%02d",$mon+1,"/",$mday,"/",$year);
    print LOG "GMT time and day $TIME $DATE\n";  
}# end sub timestamp

##------------------------------------------------------------------------
# @signature String find_mth(int index)
# <p>Find the two digit month based on the index of the month.</p>
#
# @input $index The index of the month.
# @output $month The two digit month string.
##------------------------------------------------------------------------
sub find_mth{
    my $i = 0;
    while(exists($MONTHS{$months[$i]})){
	if($_[0] == $MONTHS{$months[$i]}{"MM"}){
	    return $months[$i];
        }
	$i++;
    }
    return $FALSE;
}

##------------------------------------------------------------------------
# @signature void writeheader(String wmo, String file)
# <p>Write the header information to the output file.</p>
# 
# @input $wmo The wmo id for the file.
# @input $file The name of the file where the header will be written.
##------------------------------------------------------------------------
sub writeheader{
    my ($wmo,$file) = @_;

    printf OUT ("%s%s\n",$header[0],$data_type);
    printf OUT ("%s%s\n",$header[1],$project_id);

    my $site = $wmo;
    if(exists($stn_info{$wmo}{"NAME"})){
	my $station = $stn_info{$wmo}{"NAME"};
	$station =~ tr/[ ]{2,}//s;
	$site = sprintf("%s %s, %s",$site,$station,$stn_info{$wmo}{"COUNTRY"});
    }

    printf OUT ("%s%s\n",$header[2],$site);

    my $stn_loc = sprintf("%3d %5.2f%s,%3d %5.2f%s, %7.2f, %6.2f, %7.1f",
			  &calc_pos($HEADER{"LON"}{$file},$HEADER{"LAT"}{$file}),
			  $HEADER{"ELEV"}{$file});

    printf OUT ("%s%s\n",$header[3],$stn_loc);
    printf OUT ("%s%s\n",$header[4],$TIME{"GMT"}{"$file"});

    for my $i (0..5) { printf OUT ("%s\n","/"); }
    printf OUT ("%s%s\n",$header[6],$TIME{"NOMINAL"}{"$file"});
    for my $i (0..2) { printf OUT ("%s\n",$line[$i]); }
}

##------------------------------------------------------------------------
# @signature float[] init_line(float[] data)
# <p>Initialize the data array to the appropriate missing values.</p>
#
# @input $data The array to initialize.
# @output $date The initialized array.
##------------------------------------------------------------------------
sub init_line {
    for my $i(0,1,5,6,10){$_[$i] = 9999.0;}
    for my $i(2,3,4,7,8,9,11,12,13){$_[$i] = 999.0;}
    for my $i(15..20){$_[$i] = 9.0;}
    $_[14] = 99999.0;
    return @_;
}
    
##------------------------------------------------------------------------
# @signature void printer(float[] data)
# <p>Print the values in the array to the output file.</p>
#
# @input $data The array of data to be printed.
##------------------------------------------------------------------------
sub printer {
    printf OUT ("%6.1f %6.1f %5.1f %5.1f %5.1f %6.1f %6.1f %5.1f %5.1f %5.1f %8.3f %7.3f %5.1f %5.1f %7.1f %4.1f %4.1f %4.1f %4.1f %4.1f %4.1f\n",@_[0..20]) if ($_[0] > 0);
}

##------------------------------------------------------------------------
# @signature String line_printer(float[] data)
# <p>Convert the array of data to a formatted String.</p>
#
# @input $data The array of data to be formatted.
# @output $line The formatted data line.
##------------------------------------------------------------------------
sub line_printer {
    return sprintf ("%6.1f %6.1f %5.1f %5.1f %5.1f %6.1f %6.1f %5.1f %5.1f %5.1f %8.3f %7.3f %5.1f %5.1f %7.1f %4.1f %4.1f %4.1f %4.1f %4.1f %4.1f\n",@_[0..20]);
}

##------------------------------------------------------------------------
# @signature (float,float,float) calc_RH(float temp, float dewpt)
# <p>Calculate the relative humidity value.</p>
#
# @input $temp The temperature value.
# @input $dewpt The dew point value.
# @output $dewpt The updated dew point value.
# @output $rh The relative humidity value.
# @output $rh_flag The relative humidity flag.
##------------------------------------------------------------------------
sub calc_RH {
    my ($temp,$dewpt) = @_;
    my $emb;
    my ($rh,$rh_flag) = (999.0,9.0);
    if($temp != 999.0 && $dewpt != 999.0 && $temp >= $dewpt){
        if($dewpt < -99.9){
	    $dewpt = -99.9;
	    $rh_flag = 4.0;
	}
	
	$emb = exp(($dewpt*19.4817+440.8)/($dewpt+243.5));

        $rh = (100.*$emb)/((exp(17.67*$temp/($temp+243.5))*$ESO));
        $rh_flag = 99.0 unless $rh_flag eq "4.0";
    }
    return ($dewpt,$rh,$rh_flag);
}

##------------------------------------------------------------------------
# @signature (float,float,float,float,float) calc_UV(float spd, float dir)
# <p>Calculate the u and v wind components.</p>
#
# @input $spd The wind speed value.
# @input $dir The wind direction value.
# @output $uwind The u wind component.
# @output $vwind The v wind component.
# @output $spd The updated wind speed value.
# @output $uflag The u component flag.
# @output $vflag The v component flag.
##------------------------------------------------------------------------
sub calc_UV {
    my ($spd,$dir) = @_;
    my ($uwind,$vwind,$uflag,$vflag) = (9999.0,9999.0,9.0,9.0);
    if($spd < 999.0 && $dir <= 360.0 && $spd >= 0 && $dir >= 0.0){

	if($KNOTS){$spd = &knots2meters($spd); }

	$uwind = sin(($dir+180.0)*$degree)*$spd;
	$vwind = cos(($dir+180.0)*$degree)*$spd;
	$uflag = 99.0;
	$vflag = 99.0;
    }

    return ($uwind,$vwind,$spd,$uflag,$vflag);
}

sub numerically{ $a<=>$b};

##------------------------------------------------------------------------
# @signature (int,float,String,int,float,String,float,float) calc_pos(float lat, float lon)
# <p>Calculate the location of the station for the header.</p>
#
# @input $lat The latitude of the sounding.
# @input $lon The longitude of the sounding.
# @output $lon_deg The longitude degrees.
# @output $lon_min The longitude minutes.
# @output $lon_dir The longitude direction.
# @output $lat_deg The latitude degrees.
# @output $lat_min The latitude minutes.
# @output $lat_dir The latitude direction.
# @output $lon The longitude in degrees.
# @output $lat The latitude in degrees.
##------------------------------------------------------------------------
sub calc_pos{
    my ($lon,$lat) = @_;
    my $lon_min = (abs($lon) - int(abs($lon)))*60;
    my $lat_min = (abs($lat) - int(abs($lat)))*60;
    my $lon_dir = "'W";if($lon > 0){$lon_dir = "'E";}
    my $lat_dir = "'N";if($lat < 0){$lat_dir = "'S";}
    return (abs(int($lon)),$lon_min,$lon_dir,abs(int($lat)),$lat_min,$lat_dir,$lon,$lat);
}

##------------------------------------------------------------------------
# @signature float f2c(float temp)
# <p>Convert a temperature from farenheit to celcius.</p>
#
# @input $temp The temp in &deg;F.
# @output $temp The temp in &deg;C.
##------------------------------------------------------------------------
sub f2c { return (($_[0] - 32) * (5/9)); }

##------------------------------------------------------------------------
# @signature float knots2meters(float spd)
# <p>Convert a speed from knots to meters per second.
#
# @input $spd The speed in knots.
# @output $spd The speed in m/s.
##------------------------------------------------------------------------
sub knots2meters { return ($_[0]*0.514444444); }

##------------------------------------------------------------------------
# @signature (int,String) filename(String wmo, int year, int mth, int day, int hour, int min, float lat, float lon, float elev, int file_open, float month)
# <p>Determine the file name and set up header information for the sounding.</p>
#
# @input $wmo The wmo id for the station.
# @input $year The year of the sounding.
# @input $mth The month of the year.
# @input $day The day of the month.
# @input $hour The hour of the day.
# @input $min The minute of the hour.
# @input $lat The latitude of the station.
# @input $lon The longitude of the station.
# @input $elev The elevation of the station.
# @input $file_open A flag if the file for the sounding is open.
# @input $month The index of the month in the year.
# @output $file_open The flag if the file for the sounding is open.
# @output $outfile The name of the output file where the sounding will be written.
##------------------------------------------------------------------------
sub filename {
    my ($wmo,$year,$mth,$day,$hour,$min,$lat,$lon,$elev,$file_open,$month) = @_;
    
    my $outfile = sprintf("%s_%04d%02d%02d%02d%02d.cls",$wmo,$year,$mth,$day,$hour,$min);
#    print "Outfile $outfile\n";
    
    $TIME{"GMT"}{"$outfile"} = sprintf("%04d, %02d, %02d, %02d:%02d:00",$year,$mth,$day,
				       $hour,$min);;
    $TIME{"YEAR"}{"$outfile"} = $year;  

    my $nominal;
    if($hour == 0){
	$nominal = "00:00:00";
    }elsif($hour > 0 && $hour <= 3) {
	$nominal = "03:00:00";
    }elsif($hour > 3 && $hour <= 6){
	$nominal = "06:00:00";
    }elsif($hour > 6 && $hour <= 9){
	$nominal = "09:00:00";
    }elsif($hour > 9 && $hour <= 12){
	$nominal = "12:00:00";
    }elsif($hour > 12 && $hour <= 15){
	$nominal = "15:00:00";
    }elsif($hour > 15 && $hour <= 18){
	$nominal = "18:00:00";
    }elsif($hour > 18 && $hour <= 21){
	$nominal = "21:00:00";
    }else{
	$nominal = "00:00:00";
	$day++;
        if($day > $MONTHS{$month}{"DAYS"}){
	    $day = 1;$month = $MONTHS{$month}{"NEXT"};
            if($MONTHS{$month}{"MM"} eq "01"){$year++;}#end if
	}
    }

    $TIME{"NOMINAL"}{$outfile} = sprintf("%04d, %02d, %02d, %s",$year,$MONTHS{$month}{"MM"},
					 $day,$nominal);

    $HEADER{"LAT"}{"$outfile"} = $lat;
    $HEADER{"LON"}{"$outfile"} = $lon;
    $HEADER{"ELEV"}{"$outfile"} = $elev; 

    if(grep(/$outfile/,@{$FILE{$wmo}})){
	$file_open = $TRUE;
    } elsif (!$file_open) {
	push (@{$FILE{$wmo}},$outfile);
	$file_open = $TRUE;
    }

    return ($file_open,$outfile);
}

##------------------------------------------------------------------------
# @signature float[] check_QCflags(float[] class_array)
# <p>Check the flags with the data in the array.</p>
#
# @input $class_array The array of data.
# @output $class_array The checked array of data.
##------------------------------------------------------------------------
sub check_QCflags {
    my @class_array = @_;

    $class_array[15] = $class_array[1] == 9999.0 ? 9.0 : 99.0;
    $class_array[16] = $class_array[2] ==  999.0 ? 9.0 : 99.0;
    $class_array[17] = $class_array[4] ==  999.0 ? 9.0 : 99.0;
    $class_array[18] = $class_array[5] == 9999.0 ? 9.0 : 99.0;
    $class_array[19] = $class_array[6] == 9999.0 ? 9.0 : 99.0;

    return @class_array;
}

##------------------------------------------------------------------------
# @signature float calc_alt(float current_press, float current_temp, float current_dewpt, float* last_press, float* last_temp, float* last_dewpt, float* last_alt)
# <p>Calculate the altitude</p>
#
# @input $current_press The current pressure value.
# @input $current_temp The current temperature value.
# @input $current_dewpt The current dew point value.
# @input $last_press A pointer to the previous pressure value.
# @input $last_temp A pointer to the previous temperature value.
# @input $last_dewpt A pointer to the previous dew point value.
# @input $last_alt A pointer to the previous altitude value.
# @output $alt The calcualate altitude.
##------------------------------------------------------------------------
sub calc_alt {
    my($current_press,$current_temp,$current_dewpt,
       $last_press,$last_temp,$last_dewpt,$last_alt) = @_;
    
    my $previous_temp = $$last_temp + $K;
    my $previous_dewpt = $$last_dewpt + $K;

    my $kelvin_temp = $K + $current_temp;
    my $kelvin_dewpt = $K + $current_dewpt;

    #case 1 valid temp,last_temp,dewpt,last_dewpt
    if($current_dewpt != 999.0 && $$last_dewpt != 999.0){
	my $Q1 = &calc_q($$last_press,$previous_temp,$previous_dewpt);
	my $Q2 = &calc_q($current_press,$kelvin_temp,$kelvin_dewpt);
	my $TV1 = &calc_virT($previous_temp,$Q1);
	my $TV2 = &calc_virT($kelvin_temp,$Q2);
	my $dens1 = &calc_density($$last_press,$TV1);
	my $dens2 = &calc_density($current_press,$TV2);
	my $Tbar = &calc_Tbar($dens1,$dens2,$TV1,$TV2);
	$$last_alt=$$last_alt+((($R*$Tbar)/$G) * (log($$last_press/$current_press)));

    #case 2 valid temp,last_temp,dewpt
    }elsif($current_dewpt != 999.0 && $$last_dewpt == 999.0){
        my $Q2 = &calc_q($current_press,$kelvin_temp,$kelvin_dewpt);
        my $TV2 = &calc_virT($kelvin_temp,$Q2);
        my $dens1 = &calc_density($$last_press,$TV2);
        my $dens2 = &calc_density($current_press,$kelvin_temp);
        my $Tbar = &calc_Tbar($dens1,$dens2,$previous_temp,$kelvin_temp);
	$$last_alt= $$last_alt+((($R*$Tbar)/$G)*(log($$last_press/$current_press)));

    # case 3 vaild temp,last_temp,last_dewpt
    }elsif($current_dewpt == 999.0 && $$last_dewpt != 999.0){
        my $Q1 = &calc_q($$last_press,$previous_temp,$previous_dewpt);
        my $TV1 = &calc_virT($previous_temp,$Q1);
        my $dens1 = &calc_density($$last_press,$TV1);
        my $dens2 = &calc_density($current_press,$kelvin_temp);
        my $Tbar = &calc_Tbar($dens1,$dens2,$TV1,$kelvin_temp);
        $$last_alt = $$last_alt+((($R*$Tbar)/$G)*(log($$last_press/$current_press)));

    # case 4 vaild temp,last_temp
    }else{
	my $dens1 = &calc_density($$last_press,$previous_temp);
	my $dens2 = &calc_density($current_press,$kelvin_temp);
	my $Tbar = &calc_Tbar($dens1,$dens2,$previous_temp,$kelvin_temp);
	$$last_alt=$$last_alt+((($R*$Tbar)/$G)*(log($$last_press/$current_press)));
    }

    $$last_press = $current_press;
    $$last_temp = $current_temp;
    $$last_dewpt = $current_dewpt;
    return $$last_alt;
}

##------------------------------------------------------------------------
# @signature float calc_q(float press, float temp, float dewpt)
# <p>Calculate the 'q' value for the altitude calculation.</p>
#
# @input $press The pressure value.
# @input $temp The temperature value.
# @input $dewpt The dew point value.
# @output $q The 'q' value.
##------------------------------------------------------------------------
sub calc_q {
    my ($press,$temp,$dewpt) = @_;
    my $L = 2500000 - 2369 * ($temp - $K);
    my $X = (1/$K) - (1/$temp);
    my $expo = ($L/$Rv) * $X;
    my $es = ($ESO*100) * exp($expo);
    return (($es*$EPS)/($press*100)*(1-($es/($press*100))));
}
 
##------------------------------------------------------------------------
# @signature float calc_Tbar(float d1, float d2, float t1, float t2)
##------------------------------------------------------------------------
sub calc_Tbar{
    my ($d1,$d2,$t1,$t2) = @_;
    return ((($d1*$t1)+($d2*$t2))/($d1+$d2));
}
 
##------------------------------------------------------------------------
# @signature float calc_density(float press, float temp)
##------------------------------------------------------------------------
sub calc_density{
    my ($press,$temp) = @_;
    return (($press*100)/($R*$temp));
}
 
##------------------------------------------------------------------------
# @signature float calc_virT(float temp, float q)
##------------------------------------------------------------------------
sub calc_virT{
    my ($temp,$q) = @_;
    return ($temp*(1 + (.61 * $q)));
}

##------------------------------------------------------------------------
# @signature void write_data()
# <p>Write the data to the output file.</p>
##------------------------------------------------------------------------
sub write_data {

    foreach my $key (sort keys %FILE){
	foreach my $file (sort @{$FILE{$key}}){

	    $file =~ /_(\d{4})(\d{2})/;
	    
	    my $dir;
	    if ($2 < 7) {
		$dir = sprintf("%s/%04d01-%04d06",$main_dir,$1,$1);
	    } else {
		$dir = sprintf("%s/%04d07-%04d12",$main_dir,$1,$1);
	    }

	    mkdir($dir) unless (-e $dir);

	    open(OUT,">$dir/$file")||die "Can't open $file\n";
            &writeheader($key,$file);

            my @file_data = reverse(sort numerically keys %{$DATA{$file}});
            my $measurement_count = scalar(@file_data)-1;
            my $alt = 99999.0;
            
	    if($measurement_count>5){ &calc_press_alt($file,@file_data); }
	    my @wind_data = sort numerically keys %{$WIND{$file}};
            @wind_data = grep(/\d+\.\d/,@wind_data);

	    $measurement_count = scalar(@wind_data);
	    my $i = 0; 
	    my $line_cnt = 0;

	    foreach my $row (@file_data){
		my @data_row = split(' ',$DATA{$file}{$row});
                if($measurement_count == 0 || $data_row[14] == 99999.0 || $i >= $measurement_count){
		    &printer(@data_row);
		    $line_cnt++;
                }else{
		    &check_altitude(\$i,$measurement_count,\$line_cnt,
				    \@data_row,\@wind_data,$file);
		}
	    }


	    foreach my $alt(@wind_data[$i..$measurement_count-1]){
		&printer(split(' ',$WIND{$file}{$alt}));
                $line_cnt++;
            }

	    printf LOG ("%16s %4d %s %s\n",$file,$line_cnt,"lines of data",$main_dir);

	    undef(%{$DATA{$file}});
	    undef(%{$WIND{$file}});
	}
    }
}


##------------------------------------------------------------------------
# @signature void check_altitude(int* i, int measurement_count, int* line_count, float[]* data_row, float[]* wind_data, String file)
##------------------------------------------------------------------------
sub check_altitude {
    my $i = shift;
    my $measurement_count = shift;
    my $line_cnt = shift;
    my $data_row = shift;
    my $wind_data = shift;
    my $file = shift;


    while($$i < $measurement_count){
	my $alt = $$wind_data[$$i];
	if($alt == $$data_row[14]){
	    my @wind = (split(' ',$WIND{$file}{$alt}));
	    $$i++;
	    foreach my $elem(5,6,7,8,18,19){
		$$data_row[$elem] = $wind[$elem];
	    }
	    &printer(@$data_row);
	    return;
	}elsif($alt < $$data_row[14]){
	    &printer(split(' ',$WIND{$file}{$alt}));
	    $$i++;
	    $$line_cnt++;
	}else{
	    &printer(@$data_row);
	    return;
	}
    }
}

##------------------------------------------------------------------------
# @signature void calc_press_alt(String file, float[] met_data)
# <p>Calculate the pressure altitude?</p>
#
# @input $file The file where the data will be written.
# @input $met_data The array of data used for the calculation.
##------------------------------------------------------------------------
sub calc_press_alt{
    my $file = shift;
    my @met_data = @_;
    my $SURF = $FALSE;
    my $i = 0;
  
    my @data_row = split(' ',$DATA{$file}{$met_data[$i]});

    my $press = $data_row[1];
    while(!$SURF && $i < 4){
	
	@data_row = split(' ',$DATA{$file}{$met_data[$i]});
	if($data_row[2] != 999.0 && $data_row[3] != 999.0){
	    $data_row[14] = $HEADER{"ELEV"}{$file};
	    $DATA{$file}{$met_data[$i]} = &line_printer(@data_row);
	    $SURF = $TRUE;
	}
	$i++;
    }

    my ($last_press,$last_temp,$last_dewpt,$last_alt);

    my $FIRST = $TRUE;
    foreach my $row (@met_data){
	my @data_row = split(' ',$DATA{$file}{$row});
        if($data_row[14] != 99999.0){ 
	    if($data_row[1] != 9999.0 && $data_row[2] != 999.0){
		$last_alt = $data_row[14];
		$last_press = $data_row[1];
		$last_temp = $data_row[2];
		$last_dewpt = $data_row[3];
                if($FIRST){$FIRST = $FALSE;}
	    }
	}else{
	    if($data_row[1] != 9999.0 && $data_row[2] != 999.0 ){
		$data_row[14] = &calc_alt(@data_row[1..3],\$last_press,\$last_temp,\$last_dewpt,\$last_alt) unless ($FIRST || $data_row[1] <= 0 || $data_row[14] != 99999.0);
	    }
	}
	
	$data_row[14] = 99999.0 unless $data_row[14] >= 0;
	$DATA{$file}{$row} = &line_printer(@data_row);
    }
}

