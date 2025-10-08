#! /usr/bin/perl -w
##Module------------------------------------------------------------------------
# <p>The NWS_ESC_to_EOL_Converter.pl script is used for converting NWS soundings
# from the EOL Sounding Composite (ESC) format to the EOL format. There are
# differences between the two formats in both the headers and the data, the
# number and order of the data columns, and the "missing" values. Other 
# differences include the spacing in the two header columns, differences
# in the lat/lon placement in the header Release (Launch) Location columns,
# the starting times ("0" versus "-1"). The EOL format contains both GPS and 
# Geopotential height, but the altitude used in the ESC format is the 
# Geopotential height; therefore, the GPS Altitude is set to "missing"
# in the EOL formatted output file. Examples appear below. (Please extend
# screen width to see the complete data line.)
#
# ESC Format (data to be converted):
#
#Data Type:                         CSU Mobile Radiosonde/Ascending
#Project ID:                        PECAN
#Release Site Type/Site ID:         Mobile/CSU_Mobile
#Release Location (lon,lat,alt):    101 12.18'W, 39 30.70'N, -101.203, 39.512, 1005.0
#UTC Release Time (y,m,d,h,m,s):    2015, 06, 02, 03:03:00
#Radiosonde Type:                   RS92-SGP
#Radiosonde Serial Number:          K2333016
#Ground Station Software:           Digicora MW41 2.1.0
#Ground Check Pressure Corr:        0.80
#Ground Check Temperature Corr:     -0.05
#Ground Check Humidity Corr:        U1: -0.1/U2: -0.1
#Nominal Release Time (y,m,d,h,m,s):2015, 06, 02, 03:03:00
# Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat   Ele   Azi    Alt    Qp   Qt   Qrh  Qu   Qv   QdZ
#  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     m    code code code code code code
#------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----
#   0.0  901.0  20.8  17.2  80.0   -2.6    3.1   4.0 140.0 999.0 -101.203  39.512 999.0 999.0  1005.0  1.0  1.0  1.0  1.0  1.0  9.0
#
#EOL Format (converted data file):
#
#Data Type/Direction:                       CSU Mobile Radiosonde/Ascending
#File Format/Version:                       EOL Sounding Format/1.1
#Project Name/Platform:                     PECAN
#Launch Site:                               Mobile/CSU_Mobile
#Launch Location (lon,lat,alt):             101 12.18'W -101.203000, 39 30.70'N  39.512000,  1005.00
#UTC Launch Time (y,m,d,h,m,s):             2015, 06, 02, 03:03:00
#Sonde Id/Sonde Type:                       K2333016/RS92-SGP
#Reference Launch Data Source/Time:         
#System Operator/Comments:                  
#Post Processing/Comments:                  
#/
#  Time   -- UTC  --   Press    Temp   Dewpt    RH     Uwind   Vwind   Wspd     Dir     dZ    GeoPoAlt     Lon         Lat      GPSAlt 
#   sec   hh mm   ss     mb      C       C       %      m/s     m/s     m/s     deg     m/s       m        deg         deg         m   
#-------- -- -- ----- ------- ------- ------- ------- ------- ------- ------- ------- ------- -------- ----------- ----------- --------
#   -1.00  3  3  0.00  901.00   20.80   17.20   80.00   -2.60    3.10    4.00  140.00 -999.00  1005.00 -101.203000   39.512000  -999.00
#
# </p> 
#
#
# @author Linda Echo-Hawk
# @version TCI for the GTS soundings
#          - For TCI the station ID was added to the 
#            beginning of the output file name or else
#            some of the files would have been overwritten.
#
# @author Linda Echo-Hawk
# @version PECAN
#          - Note that the converter expects the file name
#            to be of the form xxxx_YYYYMMDDHHmmss.cls.qc
#            (e.g., KBRO_20151018230359.cls.qc) where 
#            xxxx is the NWS station, YYYY is the year,
#            MM is the month, DD is the day, HH is the
#            hour, mm are the minutes, and ss are the
#            seconds. The converter expects that the 
#            file name will contain a value for seconds.
#            If seconds are not included in the file 
#            name, you must hard-code in a "00" value.
#          - Note that the user must place the ESC files to be
#            converted into an /ESC_files directory. The files
#            to be converted should have a ".cls.qc" extension.
#          - The converted files will be placed in the 
#            /EOL_output directory. The converter will create
#            this directory if it does not exist. The output
#            file name will be of the form DYYYYMMdd_HHmmss_P.1.eol
#            (e.g., D20150602_030300_P.1.eol) where YYYY 
#            is the year, MM is the month, DD is the day,
#            HH is the hour, and mm are the minutes. A 
#            value of "00" is hard-coded in for seconds.
#          - The converter does not read the "time" value
#            from the data record. Because the ESC format
#            begins with a 0 second value and the EOL format
#            begins with a -1 second value, the time values are 
#            hard-coded to start at "-1" and are incremented
#            by one second with each record. If the data is
#            not 1-second resolution data, this will need 
#            to be changed.
#          - Because of variations in the ESC header, this 
#            converter may need to be changed. Be sure to
#            carefully check the header against the values
#            the converter is checking for in case changes
#            are needed.
#          - Depending on the ESC header, this converter may
#            need to be changed. For instance, this converter 
#            expects "Radiosonde Type" and "Radiosonde Serial 
#            Number" to appear on two separate lines in the 
#            ESC header, and we often put these on one line.
#
##Module------------------------------------------------------------------------
package ESC_to_EOL_Converter;
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
 
use ClassConstants qw(:DEFAULT);
use ClassHeader;
use ClassRecord;
use DpgCalculations;


printf "\nESC_to_EOL_Converter.pl began on ";print scalar localtime;printf "\n";
&main();
printf "\nESC_to_EOL_Converter.pl ended on ";print scalar localtime;printf "\n";

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process radiosonde data by converting it from the
# EOL Sounding Composite (ESC) format into the EOL format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = ESC_to_EOL_Converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature ESC_to_EOL_Converter new()
# <p>Create a new instance of an ESC_to_EOL_Converter.</p>
#
# @output $self A new ESC_to_EOL_Converter object.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);
    
    
	$self->{"OUTPUT_DIR"} = "../EOL_output";
    $self->{"RAW_DIR"} = "../ESC_data";
    
    return $self;
}


##------------------------------------------------------------------------------
# @signature void convert()
# <p>Convert the ESC-formatted data to the EOL format.</p>
##------------------------------------------------------------------------------
sub convert {
    my ($self) = @_;
    
    mkdir($self->{"OUTPUT_DIR"}) unless (-e $self->{"OUTPUT_DIR"});
    
    $self->readDataFiles();
}

                           
##------------------------------------------------------------------------------
# @signature void parseRawFiles(String file)
# <p>Read the data from the specified file and convert it to the EOL format.</p>
#
# @input $file The name of the file to be parsed.
##------------------------------------------------------------------------------
sub parseRawFile {
    my ($self,$file) = @_;
    
    printf("\nProcessing file: %s\n",$file);
    my $filename = $file;

    open(my $FILE,$self->{"RAW_DIR"}."/".$file) or die("Can't open file: ".$file);
    my @lines = <$FILE>;
    close($FILE);
    

    # ----------------------------------------------------
    # Create the output file name and open the output file
	# Input name convention: 72402_201509242300.cls.qc
	# Output name convention: D20140723_105827_P.1.eol
    # ----------------------------------------------------
	my $outfile;


    my ($base_filename, $ext, $qc) = split(/\./, $filename);
	# print "\tBASE: $base_filename  EXT: $ext   QC: $qc\n";
	
	my $date = "";
	my $file_time = "";
	if ($filename =~ /(\w{5})_(\d{4})(\d{2})(\d{2})(\d{2})(\d{2})/)
	{
		my ($stn_id,$year,$month,$day,$hour,$min) = ($1,$2,$3,$4,$5,$6);
		$date = join "", $year, $month, $day;
		$file_time = join "", $hour,$min,"00";
		$outfile = sprintf "%s_D%s_%s_P.1.eol", $stn_id, $date, $file_time;
	}

    printf("\tOutput file name:  %s\n", $outfile);


	open(my $OUT,">".$self->{"OUTPUT_DIR"}."/".$outfile)
	    or die("Can't open output file for $file\n");

    # Gather up the header info
	my $datatype;
	my $project;
	my $launchSite;
	my $launchLocation;
	my $launchDate;
	my $launchTime;
	my $sondeInfo;                                                            
	my $refLaunchInfo;
	my ($lon_deg,$lon_min,$releaselon,$lat_deg,$lat_min,$releaselat,$elev);
	my $sonde_id = "";
	my $launch_data = "";
	my $comments = "";
	my $sondeType;
	my $sondeId;

                  
	my @headerlines = @lines[0..15];
	foreach my $hdr (@headerlines)
	{
		chomp $hdr;

		if ($hdr =~ /Data Type:/)
		{
			$datatype = trim((split(/:/,$hdr))[1]);
		}
		if ($hdr =~ /Project ID:/)
		{
			$project = trim((split(/:/,$hdr))[1]);
		}
		if ($hdr =~ /Release Site Type/)
		{
			$launchSite = trim((split(/:/,$hdr))[1]);
		}
		if ($hdr =~ /Release Location/)
		{		
			($lon_deg,$lon_min,$lat_deg,$lat_min,$releaselon,$releaselat,$elev) = 
				split(' ',(split(/:/,$hdr))[1]);
			print "\tLON: $releaselon   LAT: $releaselat\n";
				
		    # strip the commas off of these two values
			$releaselon =~ s/,//g;
			$releaselat =~ s/,//g;
			$lon_min =~ s/,//g;
			$lat_min =~ s/,//g;
	
		}

		if ($hdr =~ /UTC Release Time/)
		{
			my $hdr_time = $hdr;
			my @timeline = split(' ', $hdr_time);
            $launchTime = trim($timeline[7]);
			print "\tLAUNCH TIME = $launchTime";

			$launchDate = trim((split(/:/,$hdr))[1]);
			my @launch = split(' ',$launchDate);
            pop @launch;
			$launchDate = join(" ", @launch);
			print "  LAUNCH DATE = $launchDate\n";

		}
		if ($hdr =~ /Radiosonde Type/)
		{
			$sondeType = trim((split(/:/,$hdr))[1]);
		}
		if ($hdr =~ /Radiosonde Serial Number/)
		{
			$sondeId = trim((split(/:/,$hdr))[1]);
		}
	}
    
	# -----------------------
    # print out the header
    # -----------------------
    printf($OUT "Data Type/Direction:                       %s\n", $datatype);
	printf($OUT "File Format/Version:                       EOL Sounding Format/1.1\n");
	printf($OUT "Project Name/Platform:                     %s\n", $project);
	printf($OUT "Launch Site:                               %s\n", $launchSite);
	printf($OUT "Launch Location (lon,lat,alt):             %s %s %10.6f, %s %s %10.6f, %8.2f\n",
	                                                            $lon_deg,$lon_min,$releaselon,$lat_deg,$lat_min,$releaselat,$elev);
	printf($OUT "UTC Launch Time (y,m,d,h,m,s):             %s %s\n", $launchDate,$launchTime);
	printf($OUT "Sonde Id/Sonde Type:                       %s\n", $comments);
	# printf($OUT "Sonde Id/Sonde Type:                       %s/%s\n", $sondeId, $sondeType);
	printf($OUT "Reference Launch Data Source/Time:         %s\n", $comments);
	printf($OUT "System Operator/Comments:                  %s\n", $comments);
	printf($OUT "Post Processing/Comments:                  %s\n", $comments);

	printf($OUT "/\n");
    printf($OUT "  Time   -- UTC  --   Press    Temp   Dewpt    RH     Uwind   Vwind   Wspd     Dir     dZ    GeoPoAlt     Lon         Lat      GPSAlt \n");
	printf($OUT "   sec   hh mm   ss     mb      C       C       %s      m/s     m/s     m/s     deg     m/s       m        deg         deg         m   \n","%");
	printf($OUT "-------- -- -- ----- ------- ------- ------- ------- ------- ------- ------- ------- ------- -------- ----------- ----------- --------\n");



	# ----------------------------------------------------
    # Parse the data portion of the input file
    # ----------------------------------------------------
	my $time = -1;
    my $pres;
	my $temp;
	my $dewpt;
	my $RH;
	my $Ucmp;
	my $Vcmp;
	my $spd;
	my $dir;
	my $Wcmp;
	my $GeoPoAlt;
	my $lon;
	my $lat;
	my $GPSAlt = -999.00;
	my $newline;

	my ($hh,$mm,$ss) = split(/:/, $launchTime);
	# print "\tHH: $hh, MM $mm, SS $ss\n";

	my $index = 0;
	foreach my $line (@lines) {
	    # Ignore the header lines and check for blank last line
	    if (($index < 15) || ($line =~ /^\s*$/)) { $index++; next; }
	    
		chomp ($line);
	    my @data = split (' ', $line);
		
		if ($data[1] !~ /9999.0/)
		{
			$pres = $data[1];
		}
		else
		{
			$pres = -999.00;
		}
		
		# $temp
		if ($data[2] !~ /999.0/)
		{
			$temp = $data[2];
		}
		else
		{
			$temp = -999.00;
		}

		# $dewpt
		if ($data[3] !~ /999.0/)
		{
			$dewpt = $data[3];
		}
		else
		{
			$dewpt = -999.00;
		}
		
		# $RH
		if ($data[4] !~ /999.0/)
		{
			$RH = $data[4];
		}
		else
		{
			$RH = -999.00;
		}
		
		# Ucmp (wind)
		if ($data[5] !~ /9999.0/)
		{
			$Ucmp = $data[5];
		}
		else
		{
			$Ucmp = -999.00;
		}
		
		# Vcmp (wind)
		if ($data[6] !~ /9999.0/)
		{
			$Vcmp = $data[6];
		}
		else
		{
			$Vcmp = -999.00;
		}
		
		
		# Wind spd
		if ($data[7] !~ /999.0/)
		{
			$spd = $data[7];
		}
		else
		{
			$spd = -999.00;
		}
		
		# Wind Dir
		if ($data[8] !~ /999.0/)
		{
			$dir = $data[8];
		}
		else
		{
			$dir = -999.00;
		}
		
		# Wcmp
		if ($data[9] !~ /999.0/)
		{
			$Wcmp = $data[9];
		}
		else
		{
			$Wcmp = -999.00;
		}
		
		# $lon
		if ($data[10] !~ /9999.0/)
		{
			$lon = $data[10];
		}
		else
		{
			$lon = -999.000000;
		}
		
		# $lat
		if ($data[11] !~ /999.0/)
		{
			$lat = $data[11];
		}
		else
		{
			$lat = -999.000000;
		}
		
		# $GeoPoAlt
		if ($data[14] !~ /99999.0/)
		{
			$GeoPoAlt = $data[14];
		}
		else
		{
			$GeoPoAlt = -999.00;
		}

        
		$newline = sprintf " %7.2f %2d %2d %5.2f %7.2f %7.2f %7.2f %7.2f %7.2f %7.2f %7.2f %7.2f %7.2f %8.2f %11.6f %11.6f %8.2f\n", $time,$hh,$mm,$ss, $pres, $temp, $dewpt, $RH, $Ucmp, $Vcmp, $spd, $dir, $Wcmp, $GeoPoAlt, $lon, $lat, $GPSAlt;
		
		printf ($OUT $newline);

        # ------------------------------------------------
		# Increment the time and the UTC hh mm ss columns
        # ------------------------------------------------
    	$time++;
        if ($ss < 59)
		{
			$ss++;
		}
		else
		{
			$ss = 0;
			if ($mm < 59)
			{
				$mm++;
			}
			else
			{
				$mm = 0;
				$hh++;
			}
		}

	}

	$index++;
}



##------------------------------------------------------------------------------
# @signature void readDataFiles()
# <p>Read in the files from the raw data directory and convert each into an
# ESC formatted file.</p>
##------------------------------------------------------------------------------
sub readDataFiles {
    my ($self) = @_;
    
    opendir(my $RAW,$self->{"RAW_DIR"}) or die("Can't read raw directory ".$self->{"RAW_DIR"});
	# HARD-CODED FILE NAME
    my @files = grep(/\.cls.qc$/,sort(readdir($RAW)));
    closedir($RAW);
    
    foreach my $file (@files) {
	$self->parseRawFile($file);
    }
    
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
