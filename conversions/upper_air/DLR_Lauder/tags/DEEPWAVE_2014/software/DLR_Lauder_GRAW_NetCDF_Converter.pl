#! /usr/bin/perl 
#
##Module-------------------------------------------------------------------------
# <p>The DLR_Lauder_GRAW_NetCDF_Converter.pl script is used for converting the 
# DEEPWAVE DLR Lauder NetCDF sounding data files for the GRAW  
# radiosonde data to the EOL Sounding Composite (ESC) Format.
#
# @usage ARMnetCDF_to_ESC.pl [--limit] [--num_soundings=8] [--max=10]
#        --limit  Limit the number of records processed to --max and put
#                 output files in output directory
#        --max    If not specified, the default limit is records with
#                 times less than 10000.0 seconds (max=5000 for this
#                 2-second data)
#        --num_soundings   Set to either 4 or 8 soundings per day; if not
#                 set, default is 4 per day (6 hourly).  This choice 
#                 determines which nominal time code will be used.
#        NOTE:    If the limit option is not used, all records will be processed
#                 and placed in the verbose_output directory
#
#
# BEWARE:  The servers tsunami and merlot were decommissioned in early 2012. 
#          The Perl NetCDF.pm module was installed on tikal on 17 April 2012.
#          Run this converter script on tikal.
#
#
# @author Linda Echo-Hawk 28 Oct 2014
# @version DEEPWAVE to process the DLR Lauder GRAW sounding data
#          - This converter is based on the ARMnetCDF_to_ESC.pl script. The
#            data is quite different from ARM data. It does not use a 
#            base_time variable and release time must be gotten from 
#            the file name. File names should be in this format: 
#            data_lauder_[graw | vaisala]YYYYMMDDHHmm.nc
#            (e.g., data_lauder_graw201407160530.nc)
#          - Pressure is given in Pascals - use convertPressure from 
#            the perl library
#          - The U and V wind components must be calculated if wind speed
#            and wind direction are good values
#          - Code was added to set the surface data value (line 1 data) for 
#            ascent rate to "missing" instead of zero
#          - The GRAW data did not contain any files with times >9999.0
#          - Code was added to change the flag values if the data 
#            is "missing."
#          - NOTE: The NetCDF files do not require use of the
#            basetime variable found in the ARM NetCDF converters.
#
#
# @author Linda Echo-Hawk 2014-06-05
# @version CONTRAST 
# @use ARMnetCDF_to_ESC.pl --limit --num_soundings=8 >&! 8-hourly_results.txt
#            See the notes from MPEX below regarding base_time.
#          - BEWARE:  The file's base_time variable no longer indicates
#            the release time.  If you need code for netCDF files that 
#            have base time = release time, then use DC3 version 
#            of the ARM converter.
#
#
# @author Linda Echo-Hawk 2013-07-11
# @version MPEX for processing of SGP C1 Lamont, OK ARM soundings
# @use ARMnetCDF_to_ESC.pl --limit --num_soundings=8 >&! results.txt
#          - BEWARE:  The file's base_time variable no longer indicates
#            the release time.  If you need code for netCDF files that 
#            have base time = release time, then use DC3 version 
#            of the ARM converter.
#          - Changes had to be made in the ARM converter because in 
#            previous datasets, the netCDF file's base_time variable 
#            represented the release time in seconds. For these data, 
#            base_time represents midnight of the release day and the 
#            value in time_offset must be added to it to determine the 
#            release date/time. This required several small code changes. 
#          - Also, in previous datasets, the values in the netCDF file's 
#            time_offset section were used as the time in the *.cls file. 
#            The values were (e.g.) 0, 2, 4, ... but now the values in 
#            that section are the same as the values in the time section, 
#            and both represent the offset from the base time (midnight of 
#            the release date). I used the base_time plus the offset to 
#            determine release date/time and for the time of the sounding 
#            data, I used "time" minus the initial "time_offset" value.
#          - Search for "HARD-CODED" to find other values that may need 
#            to be changed for other projects.
#          - NOTE: Default is set for 4 soundings per day (6 hourly).
#
#
# @author Linda Echo-Hawk 2012-12-03
# @version DC3 for processing of SGP C1 Lamont, OK ARM 2012 soundings
# @use ARMnetCDF_to_ESC.pl --limit --num_soundings=8 >&! results.txt
#          Search for "HARD-CODED" to find other values that may need 
#          to be changed for other projects.
#          - NOTE: Set for 4 soundings per day.
#
#
# @author Linda Echo-Hawk 2012-04-23
# @version DYNAMO for processing of Nauru ARM 2011-12 soundings
# @use ARMnetCDF_to_ESC.pl --limit --num_soundings=4 >&! results.txt
#          Search for "HARD-CODED" to find other values that may need 
#          to be changed for other projects.
#          - NOTE:  Used the command line switch for number of 
#            soundings processed set to 8, but there were only 2
#            per day.  The nominal time code worked fine as is.
#
# @author Linda Echo-Hawk 2012-04-19
# @version DYNAMO for processing of Manus ARM 2012 soundings
# @use ARMnetCDF_to_ESC.pl --limit --num_soundings=8 >&! results.txt
#
# @author Linda Echo-Hawk 2012-04-16
# @version DYNAMO for processing of Gan ARM 2012 soundings
# @use ARMnetCDF_to_ESC.pl --limit --num_soundings=8 >&! results.txt
#
# @author Linda Echo-Hawk 2012-02-03
# @version DYNAMO 2011 for processing of Gan Manus soundings
#          Search for "HARD-CODED" to find other values that may need 
#          to be changed for other projects.
#          - Added command line switch for number of soundings processed.  
#            This will determine which code to use for determining 
#            nominal time, and can either be "4" (6-hourly) or 
#            "8" (3-hourly) soundings per day.  If soundings are 
#            collected at different frequency than this, new
#            code may need to be added.
#
#
# @author Linda Echo-Hawk 2012-01-13
# @version DYNAMO 2011 for processing of Gan ARM soundings
#          Search for "HARD-CODED" to find other values that may need 
#          to be changed for other projects.
#          - Code was added to determine nominal time based on
#            3-hourly soundings (8 per day).  The original code
#            to determine nominal time for 6-hourly soundings 
#            was commented out but not removed.
#
# @author Linda Echo-Hawk 2010-12-12
# @version VORTEX2_2010 Minor modifications to the 2009 converter
#          - Added check for dewpoint < -99.999 which get rounded up
#            to -100.0 and are greater than the allowed 5 characters.
#            These values are changed to "missing."
#          - One file had time > 10000.0 seconds, so the limit 
#            command line option was used.
#
# @author Linda Echo-Hawk 2009-11-11
# @version VORTEX2  This converter was written for ARM-CART soundings,
#          using the cosmic2raob.pl NetCDF converter as a basis.
#          Actual and nominal time are calculated from the file
#          variables "base_time" and "time" (offset since midnight).
#          Command line options were added to allow the user to limit
#          the number of data records processed. For VORTEX2, the 
#          limit for this 2-second data is 5000.  This may need to be 
#          changed for other projects.
#          Search for "HARD-CODED" to find other values that may need 
#          to be changed for other projects.
# BEWARE:  The SCUDS skew-t generator cannot handles files with >9999
#          records (per Scot Loehrer to Linda Cully, 2008-11-12).
#
#
##Module-------------------------------------------------------------------------
# package ARM_NetCDF_Converter;
package DLR_Lauder_GRAW_NetCDF_Converter;
# use strict 'vars';

#-------------------------------------------------------------------------------
# Note that a backslash before a variable indicates a reference to that
# variable.
#-------------------------------------------------------------------------------
# Include NetCDF module in this code. Note that the NetCDF module is NOT
# installed on every EOL machine. 'use' is a compile time directive. It is
# shorthand for the following:
#BEGIN {
#       require YourModule;
#       YourModule->import(LIST);
#      } 
# By wrapping the require and import in an eval, we can let the user know
# where to run the code and exit gracefully.
#
#use NetCDF;	# does NOT die gracefully when run on a machine where
		# NetCDF.pm is not installed. So...
BEGIN {
  eval {
    require NetCDF;
    NetCDF->import();
  };

  if ($@) {
    print "\nERROR: This code requires the NetCDF.pm module which is\n".
   	  "apparently not installed on the machine on which you are \n".
	  "attempting to run the code. Please run this code on tikal.\n\n"; 
    exit(1);
  }
}

if (-e "/net/work") {
    use lib "/net/work/lib/perl/Utilities";
    use lib "/net/work/lib/perl/UpperAir";
    use lib "/net/work/lib/perl/Station";
} else {
    use lib "/work/lib/perl/Utilities";
    use lib "/work/lib/perl/UpperAir";
    use lib "/work/lib/perl/Station";
}
use DpgConversions;
use DpgCalculations;  
use DpgDate qw(:DEFAULT);
use NCutils;                               
use Data::Dumper;    
use ClassConstants qw(:DEFAULT);  
use ClassHeader;
use ClassRecord;
use ElevatedStationMap;
use Station;
# import module to set up command line options
use Getopt::Long;

# PERL standard module which allows the code to refer to each component of the 
# time by name, i.e. ->year
use Time::gmtime;

# Routines ParseDate and UnixDate are contained within this CPAN module
use Date::Manip;  

my ($WARN);

# -------------------------------------
# too many global variables
# -------------------------------------
my $year;
my $month;
my $day;
my $hour;
my $minute;
my $second;

my $baselat;
my $baselon;
my $basealt;
my $lat_fmt;
my $lon_fmt;
my %global_atts;


printf "\nDLR_Lauder_GRAW_NetCDF_Converter.pl began on ";print scalar localtime;printf "\n";

# read command line arguments 
my $result;   
# $num_soundings can be 4 or 8, and the nominal time calculation will 
# change depending on which is selected - default is 4 (every 6 hours)
my $num_soundings;
# $three_hourly means 8 soundings per day, every three hours
my $three_hourly = 0;
# limit number of data records processed; default is process all records
my $limit;
# if ($limit), specify number of records to process; default is 5000
# HARD-CODED value
my $maxRecords = 5000;
# "limit:i" i is optional, "limit=i" i is required
$result = GetOptions("limit" => \$limit, "num_soundings:i" => \$num_soundings, "max:i" => \$maxRecords);
# $result = GetOptions("limit" => \$limit, "max:i" => \$maxRecords);

if ($num_soundings == 8)
{
	$three_hourly = 1;
	print ("\n\nSetting three-hourly variable\n\n");
}
if ($limit)
{
 	printf("Processing with limit option set.\n\n");
}
else
{
	printf("Process all records - no limit set.\n\n");
}                   


&main();

if ($limit)
{
 	printf("\nProcessing limited to records with times less than 10000.0 seconds.  Remaining records were cut off.  Converted output in output directory.\n\n");
}
else
{
	printf("\nProcessed all records - no limit set.  Converted output in verbose_output directory.\n\n");
}      
printf "\nDLR_Lauder_GRAW_NetCDF_Converter.pl ended on ";print scalar localtime;printf "\n";


#*********************************************************************

# There are a ton of print statements in this code for debugging and 
# informational purposes.  Turn them on or off and see what you get (-:
sub DEBUG       {return 0; }
sub DEBUGoutput {return 0;}		# debug info for parse_data_rec subroutine
sub DEBUGgetV {return 0;}               # debug info for netCDF subroutine
sub DEBUGFileStats {return 1;}

# Output file extension
sub getOutfileExtension { return ".cls"; } 

# I like to define variables FALSE and TRUE and use them rather than one
# and zero in by comparison statements.  I think it is clearer.
sub FALSE       {return 0; }
sub TRUE        {return 1; }

#*********************************************************************

##------------------------------------------------------------------------------
# @signature void main()
# <p>Execute the conversion of the data.</p>
##------------------------------------------------------------------------------
sub main {
    # my $converter = ARM_NetCDF_Converter->new();
    my $converter = DLR_Lauder_GRAW_NetCDF_Converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature void convert()

# <p>Convert all of the raw data and create the output files for the conversion.</p>
##------------------------------------------------------------------------------
sub convert {
	my ($self) = @_;

    mkdir($self->{"VERBOSE_OUTPUT_DIR"}) unless (-e $self->{"VERBOSE_OUTPUT_DIR"});
	if ($limit)
	{
		mkdir($self->{"OUTPUT_DIR"}) unless (-e $self->{"OUTPUT_DIR"});
	}
    mkdir($self->{"FINAL_DIR"}) unless (-e $self->{"FINAL_DIR"});

    $self->readRawDataFiles();
    $self->printStationFiles();
}


##------------------------------------------------------------------------------
# @signature DLR_Lauder_NetCDF_Converter new()
# <p>Create a new DLR_Lauder_NetCDF_Converter instance.</p>
#
# @output $converter The new converter.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);

    $self->{"stations"} = ElevatedStationMap->new();

    # HARD-CODED
    $self->{"PROJECT"} = "DEEPWAVE";
    # HARD-CODED
    $self->{"NETWORK"} = "DLR_Lauder";
    
    $self->{"FINAL_DIR"} = "../final";
    $self->{"OUTPUT_DIR"} = "../output";
    $self->{"VERBOSE_OUTPUT_DIR"} = "../verbose_output";

    $self->{"RAW_DIR"} = "../graw_raw_data";
    
    $self->{"STATION_FILE"} = sprintf("%s/%s_%s_sounding_stationCD.out",$self->{"FINAL_DIR"},
				      $self->cleanForFileName($self->{"NETWORK"}),
				      $self->cleanForFileName($self->{"PROJECT"}));
    $self->{"WARN_LOG"} = $self->{"OUTPUT_DIR"}."/warning.log";
    $self->{"SUMMARY"} = $self->{"OUTPUT_DIR"}."/station_summary.log";

    return $self;
}

##---------------------------------------------------------------------------
# @signature void printStationFiles()
# <p>Generate the stationCD.out file and the station summary log for the stations
# in the conversion.</p>
##---------------------------------------------------------------------------
sub printStationFiles {
    my ($self) = @_;

    my ($STN, $SUMMARY);

    open(my $STN, ">".$self->{"STATION_FILE"}) || 
	    die("Cannot create the ".$self->{"STATION_FILE"}." file\n");
    foreach my $station ($self->{"stations"}->getAllStations()) {
        print($STN $station->toString()) if ($station->getBeginDate !~ /^9+$/);
    }

    close($STN);

    open($SUMMARY, ">".$self->{"SUMMARY"}) || die("Cannot create the ".$self->{"SUMMARY"}." file.\n");
    print($SUMMARY $self->{"stations"}->getStationSummary());
    close($SUMMARY);
}

##------------------------------------------------------------------------------
# @signature void readRawDataFiles()
# <p>Determine all of the raw NetCDF data files that need to be processed
# and then process them.</p>
##------------------------------------------------------------------------------
sub readRawDataFiles {
    my ($self) = @_;

    opendir(my $RAW,$self->{"RAW_DIR"}) or die("Cannot open raw directory.\n");
    my @files = readdir($RAW);
    closedir($RAW);

    open($WARN,">".$self->{"WARN_LOG"}) or die("Can't create ".$self->{"WARN_LOG"});

    foreach my $file (sort(@files)) {
	$self->readRawFile($file) if ($file =~ /\.nc$/);
    }

    close($WARN);
}

##---------------------------------------------------------------------
# @signature void processHeaderInfo()
# <p>Get missing header data and print the header.</p>
##---------------------------------------------------------------------   
sub processHeaderInfo
{
	my $file_name = shift;
	
    # HARD-CODED value
	my $sondeType = "Graw DFM-09";
	my $sondeSoftware = "Grawmet version 5.9.2.18";

    # ----------------------------------------------------------
    # Extract the date and time information from the file name
    # Expects filename similar to: data_lauder_graw201406190331.nc
	# or data_lauder_vaisala201408011723.nc
    # ----------------------------------------------------------
    my $date = "";
	my $time = "";
    # print "This File: $file_name\n";
	if ($file_name =~ /(\d{4})(\d{2})(\d{2})(\d{2})(\d{2})/)
	{
		my ($year, $month, $day, $hour, $min) = ($1,$2,$3,$4,$5);
        $date = join ", ", $year, $month, $day;
		$time = join ":", $hour,$min,"00";
        print "(Process Header Info) DATE:  $date   TIME:  $time\n";
	}
	
	# HARD-CODED from separate file (see /docs dir)
	my %sondeIds = (
	
	"data_lauder_graw201406190331.nc" => ["Not Available"],
	"data_lauder_graw201406190825.nc" => ["322039"],
	"data_lauder_graw201406191131.nc" => ["322044"],
	"data_lauder_graw201406191431.nc" => ["322026"],
	"data_lauder_graw201406240536.nc" => ["327328"],
	"data_lauder_graw201406240935.nc" => ["322014"],
	"data_lauder_graw201406241133.nc" => ["327328"],
	"data_lauder_graw201406241422.nc" => ["327397"],
	"data_lauder_graw201406241737.nc" => ["340726"],
	"data_lauder_graw201406242326.nc" => ["340754"],
	"data_lauder_graw201406250233.nc" => ["401311"],
	"data_lauder_graw201406250529.nc" => ["340729"],
	"data_lauder_graw201406250828.nc" => ["401320"],
	"data_lauder_graw201406251128.nc" => ["401310"],
	"data_lauder_graw201406251432.nc" => ["340665"],
	"data_lauder_graw201406251732.nc" => ["401351"],
	"data_lauder_graw201406300544.nc" => ["340686"],
	"data_lauder_graw201406300836.nc" => ["340655"],
	"data_lauder_graw201406301127.nc" => ["340678"],
	"data_lauder_graw201407040357.nc" => ["341172"],
	"data_lauder_graw201407040701.nc" => ["340562"],
	"data_lauder_graw201407041002.nc" => ["341147"],
	"data_lauder_graw201407041305.nc" => ["341187"],
	"data_lauder_graw201407121305.nc" => ["341185"],
	"data_lauder_graw201407121605.nc" => ["340675"],
	"data_lauder_graw201407121907.nc" => ["341169"],
	"data_lauder_graw201407122202.nc" => ["341160"],
	"data_lauder_graw201407160000.nc" => ["Not Available"],
	"data_lauder_graw201407160240.nc" => ["Not Available"],
	"data_lauder_graw201407160530.nc" => ["Not Available"]
	);

	my $sondeId = $sondeIds{$file_name}[0];
	print "SONDE ID: $sondeId\n";


	# -------------------------------------------------------------
    # Use the record 0 lat/lon info for header lat/lon
	# and determine degrees and minutes
	# -------------------------------------------------------------
	# format length must be the same as the value length or
    # convertLatLong will complain (see example below)
    # base lat = 36.6100006103516 base lon = -97.4899978637695
    # Lat format = DDDDDDDDDDDDDDDD  Lon format = -DDDDDDDDDDDDDDDD  
	$lat_fmt = $baselat < 0 ? "-" : "";  
	while (length($lat_fmt) < length($baselat)) { $lat_fmt .= "D"; } 
	$lon_fmt = $baselon < 0 ? "-" : "";  
    while (length($lon_fmt) < length($baselon)) { $lon_fmt .= "D"; } 
	# print "Lat format = $lat_fmt  Lon format = $lon_fmt\n";
    # get the degrees and minutes values and directions
	my ($lat_deg,$lat_min,undef()) = convertLatLong($baselat,$lat_fmt,"DM");
	my ($lon_deg,$lon_min,undef()) = convertLatLong($baselon,$lon_fmt,"DM"); 
	my $lat_dir = $lat_deg < 0 ? "S" : "N";
	my $lon_dir = $lon_deg < 0 ? "W" : "E";
    

	# -------------------------------------------------------------
    # print out the header lines.
	# -------------------------------------------------------------
    printf(OUTFILE "Data Type:                         DLR Lauder Radiosonde/Ascending\n");
    printf(OUTFILE "Project ID:                        DEEPWAVE\n");
    printf(OUTFILE "Release Site Type/Site ID:         Lauder, New Zealand\n");
    printf(OUTFILE "Release Location (lon,lat,alt):    %03d %05.2f'%s, %02d %05.2f'%s, %.3f, %.3f, %.1f\n",
	     			abs($lon_deg),$lon_min,$lon_dir,abs($lat_deg),$lat_min,$lat_dir,$baselon,$baselat,$basealt);  
    printf(OUTFILE "UTC Release Time (y,m,d,h,m,s):    %04d, %02d, %02d, %02d:%02d:00\n", 
					$year,$month,$day,$hour,$min);         	
    printf(OUTFILE "Radiosonde Type:                   %s\n", $sondeType);
    printf(OUTFILE "Radiosonde Serial Number:          %s\n", $sondeId);
    printf(OUTFILE "Ground Station Software:           %s\n", $sondeSoftware);
    printf(OUTFILE "/\n/\n/\n");
    printf(OUTFILE "Nominal Release Time (y,m,d,h,m,s):%04d, %02d, %02d, %02d:%02d:00\n", 
	    			$year,$month,$day,$hour,$min);

    printf(OUTFILE " Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat    Ele   Azi   Alt    Qp   Qt   Qrh  Qu   Qv   QdZ\n");
    printf(OUTFILE "  sec    mb     C     C     %s     m/s    m/s   m/s   deg   m/s      deg     deg    deg   deg    m    code code code code code code\n","%");
    printf(OUTFILE "------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----\n"); 


}


#---------------------------------------------------------------------
# @signature void parse_data_rec()
#
#---------------------------------------------------------------------
sub parse_data_rec
{
	my $file_name = shift;
    my $recdimLen = shift;
    my $var = shift;

    my %output = ();

    foreach (my $recnum=0; $recnum < $recdimLen; $recnum++) 
	{
		if (&DEBUGoutput) {print "\n\n";}
    	if (&DEBUGoutput) {print "Processing record $recnum\n\n";}

    	# ----------------------------------------------------
    	# Get the lat/lon/alt information from recnum 0 for 
    	# the header and print the header to the file
    	# ----------------------------------------------------
		if ($recnum == 0)
		{
			$baselat = -45.038;
			$baselon = 169.684;
			$basealt = getVar("height", $var, $recnum);
			processHeaderInfo($file_name);
		}
          
		my $windSpd;
		my $windDir;

  	    # $recnum is the index of all the data for a single time.  
    	# Process this record and the next one.
    	for (my $i = $recnum; $i < $recdimLen && $i <= $recnum+1; $i++) 
		{
			# -------------------------------
			# Need these wind variables to 
			# calculate u and v compoents
			# -------------------------------
			$windSpd = 0;
			$windDir = 0;
			# -------------------------------
			
			$output{Time}[$i] = getVar("time",$var,$i); 
	  		if (($output{Time}[$i] =~ /nan/i) || ($output{Time}[$i] =~ /-2147483/))
	  			{ $output{Time}[$i] = 9999.0; } 

	  		

			$output{Press}[$i] = getVar("pressure",$var,$i); #Pressure (hPa)
	  		if (($output{Press}[$i] =~ /nan/i) || ($output{Press}[$i] =~ /-2147483/))
			{ 
				$output{Press}[$i] = 9999.0; 
                # set the qc flag
	            $output{Qp}[$i] = 9.0;
			} 
			else
			{
				$output{Press}[$i] = convertPressure($output{Press}[$i],"Pa","mb");
            	# set the qc flag
			    $output{Qp}[$i] = 99.0;
			}

	  		
			$output{Temp}[$i] = getVar("temperature",$var,$i);  #Dry Bulb Temp (C)	 
	  		if (($output{Temp}[$i] =~ /nan/i) || ($output{Temp}[$i] =~ /-2147483/))
	  		{
				$output{Temp}[$i] = 999.0;
				# set the qc flag
				$output{Qt}[$i] = 9.0;
			}
			else
			{
                # set the qc flag
				$output{Qt}[$i] = 99.0;
			}

	  		
			$output{Dewpt}[$i] = getVar("dew_point",$var,$i);   #Dewpoint Temp (C)
	  		if (($output{Dewpt}[$i] =~ /nan/i) || ($output{Dewpt}[$i] =~ /-2147483/))
	  			{$output{Dewpt}[$i] = 999.0; } 
            # Handle dewpoints with >5 chars, e.g., -100.0
            # Using < -99.9 also catches -99.9, so added extra decimal places
	  		# if ($output{Dewpt}[$i] < -99.9999) 
	  		# NOTE: The value -99.98 rounds up to -100.0 and causes >130 char
			# line length, so change this to missing
	  		if ($output{Dewpt}[$i] <= -99.98) 
			{
				# print "Dewpt value greater than 5 chars $output{Dewpt}[$i]\n";
				$output{Dewpt}[$i] = 999.0; 
			} 


	 		$output{RH}[$i] = getVar("rel_humidity",$var,$i);  #Relative Humidity (%)
	  		if (($output{RH}[$i] =~ /nan/i) || ($output{RH}[$i] =~ /-2147483/))
			{
				$output{RH}[$i] = 999.0; 
				# set the qc flag
				$output{Qrh}[$i] = 9.0;
			} 
			else
			{
				# set the qc flag
				$output{Qrh}[$i] = 99.0;
			}

			# ------------------------------------------------------------------------
			# if windspeed and wind_dir are not missing, calculate u and v components
			# ------------------------------------------------------------------------
			my $good_value_windSpd = 0;
			my $good_value_windDir = 0;

			$output{spd}[$i] = getVar("wind_speed",$var,$i);  #Wind Speed (m/s)	  
	  		if (($output{spd}[$i] =~ /nan/i) || ($output{spd}[$i] =~ /-2147483/))
	  		{
				$output{spd}[$i] = 999.0; 
			}
		    else
			{
				$windSpd = $output{spd}[$i];
				$good_value_windSpd = 1;
			}

	  		
			$output{dir}[$i] = getVar("wind_direction",$var,$i);  #Wind Direction (deg)	  
	  		if (($output{dir}[$i] =~ /nan/i) || ($output{dir}[$i] =~ /-2147483/))
	  		{
				$output{dir}[$i] = 999.0; 
			}
			else
			{
				$windDir = $output{dir}[$i];
				$good_value_windDir = 1;
			}

		
			# the GRAW data has no ucmp wind data, must calculate
            # the GRAW data has no vcmp wind data, must calculate
			my $u_component;
			my $v_component;
			if (($good_value_windSpd) && ($good_value_windDir))
			{
				my @uv_values = calculateUVfromWind($windSpd, $windDir);
				
				$u_component = $uv_values[0];
				if (defined($u_component))
				{

                	$output{Ucmp}[$i] = $u_component;
                	# set the qc flag
					$output{Qu}[$i] = 99.0;
                }
				else
				{
					print "No U component found\n";
				}


				$v_component = $uv_values[1];
				if (defined($v_component))
				{
					$output{Vcmp}[$i] = $v_component;
					# set the qc flag
					$output{Qv}[$i] = 99.0;
				}
				else
				{
					print "No V component found\n";
				}

			}
			else
			{
				# set the values and flags to missing
   				$output{Ucmp}[$i] = 9999.0;
   				# set the qc flag
   				$output{Qu}[$i] = 9.0;
   				
				
				$output{Vcmp}[$i] = 9999.0;
            	# set the qc flag
				$output{Qv}[$i] = 9.0;
			}

            my $current_time = $output{Time}[$i];
	  		$output{Wcmp}[$i] = getVar("balloon ascent rate",$var,$i);  #Ascent Rate (m/s)
	  		if (($output{Wcmp}[$i] =~ /nan/i) || ($output{Wcmp}[$i] =~ /-2147483/) ||
				($current_time == 0))
	  		{
				$output{Wcmp}[$i] = 999.0; 
				# set the qc flag
				$output{Qdz}[$i] = 9.0;
			}
			else
			{
			    # set the qc flag
				$output{Qdz}[$i] = 99.0;
			}

	  		
			$output{Lon}[$i] = getVar("longitude",$var,$i);  #East Longitude (deg)	  
	  		if (($output{Lon}[$i] =~ /nan/i) || ($output{Lon}[$i] =~ /-2147483/))
	  			{$output{Lon}[$i] = 9999.0; } 
	  
	  		
			$output{Lat}[$i] = getVar("latitude",$var,$i);  #North Latitude (deg)	  
	  		if (($output{Lat}[$i] =~ /nan/i) || ($output{Lat}[$i] =~ /-2147483/))
	  			{$output{Lat}[$i] = 999.0; } 
	  

	  		$output{Alt}[$i] = getVar("height",$var,$i);  #altitude (m)	  
	  		if (($output{Alt}[$i] =~ /nan/i) || ($output{Alt}[$i] =~ /-2147483/))
	  			{$output{Alt}[$i] = 99999.0; } 


            # ---- vaiasala only, not in the graw data
			# $output{Azi}[$i] = 999.0; hard-coded in $outputRecord below
            # $output{Ele}[$i] = 999.0;


	  	} # End for $i loop

	    # print output here
     	my $outputRecord = sprintf "%6.1f %6.1f %5.1f %5.1f %5.1f %6.1f %6.1f %5.1f %5.1f %5.1f %8.3f %7.3f 999.0 999.0 %7.1f %4.1f %4.1f %4.1f %4.1f %4.1f %4.1f\n",
	 		$output{Time}[$recnum], $output{Press}[$recnum], $output{Temp}[$recnum], 
	 		$output{Dewpt}[$recnum], $output{RH}[$recnum], $output{Ucmp}[$recnum],$output{Vcmp}[$recnum],
	 		$output{spd}[$recnum], $output{dir}[$recnum], $output{Wcmp}[$recnum],$output{Lon}[$recnum],
	 		$output{Lat}[$recnum], $output{Alt}[$recnum], $output{Qp}[$recnum], $output{Qt}[$recnum],
	 		$output{Qrh}[$recnum], $output{Qu}[$recnum], $output{Qv}[$recnum], $output{Qdz}[$recnum];
    
		# We need to limit the data records to times < 10000 seconds.
    	# For the ARM 2-second data soundings, 5000 records is the max.
    	if ($limit)
		{
            # HARD-CODED value
			if ($recnum < 5000)
			{
				print OUTFILE $outputRecord;
			}
			elsif ($recnum == 5000)
			{
    			# print "Record number = $recnum\n";
        		print "\tLimited processing to records with times less than 10000.0 seconds.  Remaining records were cut off.\n";
			}
		}
		else
		{
	   		print OUTFILE $outputRecord;
		}
	} # End for $recnum loop
}

#---------------------------------------------------------------------
# @signature void getData()
# <p>Read in all the data for each variable that occurs in out QCF output
# i.e. time, station info, temperature, dewpoint, etc.
# Ignore other variables
#
# @output  %var{$variable}{values} Adds the array values to the hash for
#       each variable
#---------------------------------------------------------------------
sub getData 
{
	my $file_name = shift;
    my $ncid = shift;
    my $recDimName = shift;
    my $var = shift;
    my $recdimLen = shift;	# number of records in the record dimension
    
	# my $basetime;
	my $variable;
    foreach $variable (&getFields) 
	{
        #--------------------------------------------------------------------- 
        # Make sure that the variable the user has requested in getFields 
        # actually exists in the data.
        #--------------------------------------------------------------------- 
	    if (!defined($var->{$variable})) 
		{
            print "WARNING: Unknown variable $variable requested by user";
            print " in code at getFields declaration.\n";
            exit(1);
        }
    }
                                    
    # Loop over each record (time) in the netCDF file, read in the data for
    # that record
    foreach (my $record=0; $record < $recdimLen; $record++) 
	{
        if (&DEBUG) {print "Reading in data for record $record\n";}

        # Loop through each parameter we want to extract from the raw data
        # as given in the descriptor file
        # Assign parameters to a raw record structure.
        foreach $variable (&getFields) 
		{
            if (&DEBUG) {print "Reading in data for variable $variable\n";}

            my @values = ();
            my $dimLen;  # not used (echohawk)

            #-------------------------------------------------------------------
            # If the variable has only one dimension, 
            # then read it in. Note that varget saves the data to
            # the first index of @values.  It appears to save the data point as
            # a float, so information on significant digits is lost, and we get
            # numbers like 38.4943313598633
            #-------------------------------------------------------------------
            if ($var->{$variable}{ndims} == 1 ) 
			{
                my @coords = ($record);
                my @counts = (1);
                if (NetCDF::varget($ncid,$var->{$variable}{varid},\@coords,
                      \@counts, \@values) == -1) 
				{
					die "Can't get data for variable $variable:$!\n";
                }
                $var->{$variable}{values}[$record] = $values[0];
            }
			           
            else 
			{
                print "ERROR: Don't know how to read in variable: $variable(";
                foreach my $dim ( 0 .. $var->{$variable}{ndims}-1) {
                    my $dimname = $var->{$variable}{dimname}[$dim];
                  print "$var->{$variable}{dimname}[$dim]=$var->{$variable}{$dimname} ";
                }
                print ")\n";
            }
        } # End foreach $variable
    } # End foreach $record

    # --------------------------------------------------
    # Now we have all the variables for all the records, 
	# so loop through again and print the record
    # --------------------------------------------------
    # create some shorter test output
	# NOTE that $basetime is from the ARM converters
	#
	# &parse_data_rec(10,$var,$basetime);
    # &parse_data_rec($recdimLen,$var,$basetime);
    &parse_data_rec($file_name, $recdimLen, $var);
}


#---------------------------------------------------------------------
# Get date and location from global atts in input netCDF file.
#---------------------------------------------------------------------
sub get_global_atts 
{
    my $ncid = shift;
    my $atttype;
    my %global_atts= (); 
	# HARD-CODED global attribute values from the NetCDF file
	foreach my $attname ("instrument") 
	{
		# to inquire about the global attribute $attname
		if (NetCDF::attinq($ncid,-1,$attname,$atttype,my $attlen) == -1) 
		{
            die "Can't inquire of attribute type of $ARGV:$!\n";
		}
		my @value; 
	    # to get the value of a vector global attribute
        if (NetCDF::attget($ncid,-1,$attname,\@value) == -1) 
		{
            die "Can't inquire of value of attribute of $ARGV:$!\n";
		}

        ###############################################
        # my global_atts are strings (chars)   
    	###############################################
		if ($attname)
		{
		    # remove the null string terminator from each string
		    splice(@value, -1);
	        # the foreach loop shows the chars filling the array
			# foreach my $val(@value)
			# {
			#    print "my string is $val\n";
	   	    # } 
	 		my $str = pack("C*", @value);
	   		$global_atts{$attname} = $str;
	   		if (&DEBUGgetV)
	   		{print "\t$attname = \"$str\"\n";}
    	}
		# FROM COSMIC2RAOB.PL: Assume all atts have numeric values (int,short,float,double). 
    	# Char and  byte won't work this way. Need to pack chars and store byte as ptr to
		# array.
    	else 
		{  
		    $global_atts{$attname} = $value[0]; 
		}
	}   

	# print Dumper(%global_atts);
    # Dumper Output
    # $VAR1 = 'serial_number';
    # $VAR2 = 'E1152121';
    # $VAR3 = 'facility_id';
    # $VAR4 = 'C1: Lamont, Oklahoma';
    # $VAR5 = 'site_id';
    # $VAR6 = 'sgp';

    return(%global_atts);

}


#---------------------------------------------------------------------
# Data fields of interest.
#---------------------------------------------------------------------
sub getFields {
	
	# HARD-CODED variable names from the NetCDF data file
	my @names = ("time", "balloon ascent rate", "longitude", "latitude", "height", 
   	             "pressure", "temperature", "rel_humidity", "dew_point", "wind_speed",
   				 "wind_direction");

    return(@names);
}

sub getVar {
    my $varname = shift;
    my $var = shift;
    my $index = shift;

    my $value = $$var{$varname}{values}[$index];
    if (&DEBUGoutput) {print "$varname $index $value\n";}
    return $value;
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

    printf("\nProcessing file: %s\n",$file_name);
   	my $ncid;
   	if (($ncid = NetCDF::open($file,0)) == -1) 
	{
           die "Cannot open file: $file\n";
   	}   

    #-----------------------------------------------------------------
    # Get date/time  information from the file name
	#
	# data_lauder_graw201406190331.nc OR
	# data_lauder_vaisala201408011723.nc
    #-----------------------------------------------------------------    	
	
    my $date = "";
	my $time = "";

	if ($file =~ /(\d{4})(\d{2})(\d{2})(\d{2})(\d{2})/)
	{
		($year, $month, $day, $hour, $min) = ($1,$2,$3,$4,$5);
        $date = join ", ", $year, $month, $day;
		$time = join ":", $hour,$min,"00";
        # print "(ReadRawFile) DATE:  $date   TIME:  $time\n";
	}

		
    #-------------------------------------------------
    # Open the output file in the ../output directory.
    #-------------------------------------------------
   	my $ext = &getOutfileExtension;
    # HARD-CODED value
	# get the global attributes for this file
   	%global_atts = get_global_atts($ncid);
	my $sondeType = $global_atts{"instrument"};
	print "SONDE TYPE: $sondeType\n";
	my $facilityId = "DLR Lauder";
     
	my $outfile; 
    if (!$limit)
	{

        $outfile = sprintf "%s/%s_%04d%02d%02d%02d%02d%s", $self->{"VERBOSE_OUTPUT_DIR"},
	                       $self->{"NETWORK"}, $year, 
		   				   $month, $day, $hour, $min, $ext; 
        open (OUTFILE,">$outfile")
           or die "Can't open output file $outfile:$!\n";       
	}
	else
	{

        $outfile = sprintf "%s/%s_%04d%02d%02d%02d%02d%s", $self->{"OUTPUT_DIR"},
	                       $self->{"NETWORK"}, $year, 
		   				   $month, $day, $hour, $min, $ext; 
        open (OUTFILE,">$outfile") 
           or die "Can't open verbose output file $outfile:$!\n";        
	}
    print "\tOutput file name: $outfile\n";
    #-------------------------------------------------
    # Read all the data from the entire input netCDF file, outputting
    # ascii as we go. (output is included within this input routine)
    #-------------------------------------------------
    (my $recDimName, my $var, my $recdimsize) = readNetCDFheader($file);
    # getData($ncid,$recDimName, $var, $recdimsize, $file_name);
    getData($file_name, $ncid, $recDimName, $var, $recdimsize);


    #-----------------------------------------------------------------
    # All soundings come from Lauder
    #-----------------------------------------------------------------
    my $station = $self->{"stations"}->getStation($self->{"NETWORK"},$self->{"NETWORK"}, 											$baselat, $baselon, $basealt);
    if (!defined($station)) {
        $station = $self->build_default_station($self->{"NETWORK"},$self->{"NETWORK"});
		$station->setLatitude($baselat, $lat_fmt);
		$station->setLongitude($baselon, $lon_fmt);
		$station->setElevation($basealt,"m");
        $self->{"stations"}->addStation($station);
       }
    my $nomDate = sprintf("%04d, %02d, %02d",$year,$nomMonth,$nomDay);  
	# print "Nominal Date for Station:  $nomDate\n";
	$station->insertDate($date, "YYYY, MM, DD");         
	
    #-----------------------------------------------------------------
    # Close the input file
    #-----------------------------------------------------------------

  	if (NetCDF::close($ncid) == -1) {
        die "Can't close $file\n";
    }

    close(OUTFILE);
}

##------------------------------------------------------------------------------
# @signature Station build_default_station(String station_id, String network)
# <p>Create a default station for the Ron Brown Ship using the specified
# station_id and network.</p>
#
# @input $station_id The identifier of the station to be created.
# @input $network The network the station belongs to.
# @return The new station object with the default values for the network.
##------------------------------------------------------------------------------
sub build_default_station {
    my ($self,$station_id,$network) = @_;
    my $station = Station->new($station_id,$network);

    # HARD-CODED values
    $station->setStationName($network);
	# $station->setCountry("XX");
    $station->setStateCode("99");
    $station->setReportingFrequency("no set schedule");
    $station->setNetworkIdNumber(99);
	# Platform 416, Radiosonde, GRAW DFM-09
    $station->setPlatformIdNumber(416);
    # $station->setMobilityFlag("m");
    return $station;
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
# @signature String buildLatlonFormat(String value)
# <p>Generate the decimal format for the specified value.</p>
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

