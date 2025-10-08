#! /usr/bin/perl 
#
##Module-------------------------------------------------------------------------
# <p>The CLAMPS_NetCDF_Converter.pl script is used for converting the OU/NSSL 
# CLAMPS NetCDF sounding data files to the EOL Sounding Composite (ESC) Format.
#
# @usage CLAMPS_NetCDF_Converter.pl [--limit] [--num_soundings=8] [--max=10]
#        --limit  Limit the number of records processed to --max and put
#                 output files in output directory
#        --max    If not specified, the default limit is records with
#                 times less than 10000.0 seconds (max=9999 for this
#                 1-second data)
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
#          - BEWARE: There are (at least) two kinds of NetCDF files. 
#          - In one type, the file's base_time variable indicates the 
#            release time, so base time = release time in seconds. The 
#            values in the netCDF file's time_offset section were used 
#            as the time in the *.cls file. The values are (e.g.) 0, 
#            2, 4, etc. Use the DC3 version of the ARM converter for
#            this type of data or this converter for PECAN MP1 CLAMPS.
#
#          - In the second type, base_time represents midnight of the 
#            release day and the value in time_offset must be added 
#            to it to determine the release date/time. The values in
#            the time offset section are the same as the values in the 
#            time section, and both represent the offset from the base 
#            time (midnight of the release date). I used the base_time 
#            plus the offset to determine release date/time and for the 
#            time of the sounding data, I used "time" minus the initial 
#            "time_offset" value. Use the PECAN ARM Facility version
#            for this type of data.
#
# @author Linda Echo-Hawk 17 May 2016
# @version PECAN Mobile PISA 1 (MP1) CLAMPS OU/NSSL developed for PECAN
#          based on the ARMnetCDF_to_ESC.pl script
#          - Search for "HARD-CODED" to find other values that may need 
#            to be changed for other projects.
#          - Added code to calculate ascension rate. Since this is an
#            old-style converter and doesn't use toString from the
#            Perl library, we need to tell it how to do this.
#          - Code was added to calculate geopotential height since
#            most of the raw data files have altitude starting at
#            ground level.
#          - Scot L. provided a text file (sfc_elev.txt) with the
#            release altitude values for each file. The converter
#            reads in this file and assigns the correct surface 
#            elevation to the header and surface record.
#          - Added code to convert wind speed from knots to m/s. 
#            This was later removed when the new version of the
#            data provided wind speed in m/s units.
#          - Added code to set wind speed to "missing" if the value
#            was too large to fit the format (i.e., greater than
#            5 characters including decimal point). If wind speed
#            is set to missing because of this, then all wind 
#            parameters are set to missing (per Scot L.).
#          - Two files required hard-coding surface lat/lon
#            values provided by Scot.
#
#
#
#
#
#
# NOTES BELOW ARE FROM THE ARMnetCDF_to_ESC.pl
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
package CLAMPS_NetCDF_Converter;
use strict 'vars';

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
# Dump to see hash contents
use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Sortkeys = 1;


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

my $nomYear;
my $nomMonth;
my $nomDay;

my $outfile; # output file name
my $baselat;
my $baselon;
my $basealt;
my $lat_fmt;
my $lon_fmt;
my %global_atts;


printf "\nCLAMPS_NetCDF_Converter.pl began on ";print scalar localtime;printf "\n";

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
my $maxRecords = 9999;
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
printf "\nCLAMPS_NetCDF_Converter.pl ended on ";print scalar localtime;printf "\n";


#*********************************************************************

# There are a ton of print statements in this code for debugging and 
# informational purposes.  Turn them on or off and see what you get (-:
sub DEBUG       {return 0; }
sub DEBUGoutput {return 0;}		# debug info for parse_data_rec subroutine
sub DEBUGgetV {return 0;}               # debug info for netCDF subroutine
sub DEBUGFileStats {return 0;}
sub DEBUGascentRate {return 0;}

sub DEBUGgeoHeight {return 0};

# Output file extension
sub getOutfileExtension { return ".cls"; } 

# Define variables FALSE and TRUE and use them rather than one
# and zero in by comparison statements.  I think it is clearer.
sub FALSE       {return 0; }
sub TRUE        {return 1; }

#*********************************************************************

##------------------------------------------------------------------------------
# @signature void main()
# <p>Execute the conversion of the data.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = CLAMPS_NetCDF_Converter->new();
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
# @signature CLAMPS_NetCDF_Converter new()
# <p>Create a new CLAMPS_NetCDF_Converter instance.</p>
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
    $self->{"PROJECT"} = "PECAN";
    # HARD-CODED
    $self->{"NETWORK"} = "OU_NSSL_CLAMPS";
    
    $self->{"FINAL_DIR"} = "../final";
    $self->{"OUTPUT_DIR"} = "../output";
    $self->{"VERBOSE_OUTPUT_DIR"} = "../verbose_output";
    $self->{"RAW_DIR"} = "../raw_data";
    
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

    open($WARN,">".$self->{"WARN_LOG"}) or die("Can't open warning file.\n");

    foreach my $file (sort(@files)) {
	$self->readRawFile($file) if ($file =~ /\.cdf$/);
    }

    close($WARN);
}

##---------------------------------------------------------------------
# @signature void processHeaderInfo()
# <p>Get missing header data and print the header.</p>
##---------------------------------------------------------------------   
sub processHeaderInfo
{
	my $basetime = shift;
	my $nomOffset = shift;
	
    # HARD-CODED value
	my $sondeType = "IMET1";

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
    # Determine the actual release time and date from the file's
    # "base_time" variable 
    # NOTE: For Gan $base_time = 1316671260 ;
	# -------------------------------------------------------------
    # print "base_time = $basetime\n";
	# base_time data value: base_time = 1244935620 (VORTEX2)
	my ($d, $t) = getDateFromSeconds($basetime);
	print "\tFROM getDateFromSeconds: actual date = $d  time = $t\n";
	my ($btYear, $btMonth, $btDay) = split(/\//, $d);
	my ($btHour, $btMinute, $btSecond) = split(/:/, $t);

	# -------------------------------------------------------------
    # Determine the nominal release time and date
	# -------------------------------------------------------------
    # If soundings are taken 4 times per day, nominal times
    # will be 00, 06, 12, or 18 hours.  
    # If soundings are taken 8 times per day, nominal times 
	# will be 00, 03, 06, 09, 12, 15, 18, or 21 hours.
	# -------------------------------------------------------------
    my $nomZone;
    # no multiplier for hour =~ /00/
	if ($three_hourly)
	{
	    # -------------------------------------------------------------
        # Soundings are taken 8 times per day, and nominal times
        # will be 00, 03, 06, 09, 12, 15, 18 or 21 hours.  Determine the
        # closest nominal to the actual time and get the multiplier.
	    # -------------------------------------------------------------
        if (($hour =~ /01/) || ($hour =~ /02/) || ($hour =~ /03/)) { $nomZone = 1; }
        elsif (($hour =~ /04/) || ($hour =~ /05/) || ($hour =~ /06/)) { $nomZone = 2; }
        elsif (($hour =~ /07/) || ($hour =~ /08/) || ($hour =~ /09/)) { $nomZone = 3; }
        elsif (($hour =~ /10/) || ($hour =~ /11/) || ($hour =~ /12/)) { $nomZone = 4; }
        elsif (($hour =~ /13/) || ($hour =~ /14/) || ($hour =~ /15/)) { $nomZone = 5; }
        elsif (($hour =~ /16/) || ($hour =~ /17/) || ($hour =~ /18/)) { $nomZone = 6; }
        elsif (($hour =~ /19/) || ($hour =~ /20/) || ($hour =~ /21/)) { $nomZone = 7; }
        elsif (($hour =~ /22/) || ($hour =~ /23/)) { $nomZone = 8; }
	}

    else
	{
	    # -------------------------------------------------------------
        # Soundings are taken four times per day, and nominal times
        # will be 00, 06, 12, or 18 hours.  Determine the closest
        if (($hour =~ /03/) || ($hour =~ /04/) || ($hour =~ /05/) || 
	    	($hour =~ /07/) || ($hour =~ /08/)) 
	    {	$nomZone = 1;  }
	    elsif (($hour =~ /09/) || ($hour =~ /10/) || ($hour =~ /11/) || 
	           ($hour =~ /13/) || ($hour =~ /14/))
	    {	$nomZone = 2;  }
 	    elsif (($hour =~ /15/) || ($hour =~ /16/) || ($hour =~ /17/) || 
	           ($hour =~ /19/) || ($hour =~ /20/))
	    {	$nomZone = 3;  }
  	    # elsif (($hour =~ /21/) || ($hour =~ /22/) || ($hour =~ /23/) || 
	    #       ($hour =~ /01/) || ($hour =~ /02/))
        else
	    {
		    $nomZone = 4;
	    } 
	}
	# -------------------------------------------------------------

    # From base_time subtract time since midnight ($nomOffset)
	# found in variable "time" initial data record 0.
	# Add back time to correct nominal time ($nomZone * $nomFactor)
	# $nomFactor will be multiplied by the $nomZone found above
	# -------------------------------------------------------------
    # $nomFactor determined by:
	# (86400 s/day) / (4 drops/day) = 21600 s/drop 
    # (86500 s/day) / (8 drops/day) = 10800 s/drop
	#
	# -------------------------------------------------------------
	my $nomFactor;
	if ($three_hourly)
	{
		$nomFactor = 10800; # 8 drops/day 8*10800 = 86400 s/day
	}
	else
	{
		$nomFactor = 21600; # 4 drops/day 4*21600 = 86400 s/day
    }

    my $nom_time = $basetime - $nomOffset + ($nomZone * $nomFactor);
	my ($nd, $nt) = getDateFromSeconds($nom_time);
	# print "\tFROM getDateFromSeconds: nominal date = $nd  time = $nt\n";
 	# FROM getDateFromSeconds: date = 2009/06/30  time = 17:28:00 
 	# ------------------------------------------------------
	# Don't make these local, they are needed to pass the
    # date info back for the station object so are global
	# my ($nomYear, $nomMonth, $nomDay) = split(/\//, $nd);
	# ------------------------------------------------------
 	($nomYear, $nomMonth, $nomDay) = split(/\//, $nd);
	my ($nomHour, $nomMinute, $nomSecond) = split(/:/, $nt);
    
   
	# -------------------------------------------------------------
    # Compare file's base_time value with the file name date/time
	# -------------------------------------------------------------               
	if (($year != $btYear) || ($month != $btMonth) || ($day != $btDay))
	{
		print "WARNING: File base_time date does not match file name date\n";
		print "FILE DATE: $year $month $day\n";
	}
	elsif (($hour != $btHour) || ($minute != $btMinute) || ($second != $btSecond))
	{
		print "WARNING: File base_time time does not match file name time\n";
	}


	# -------------------------------------------------------------
    # print out the header lines.
	# -------------------------------------------------------------
    printf(OUTFILE "Data Type:                         OU/NSSL CLAMPS Mobile/Ascending\n");
    printf(OUTFILE "Project ID:                        PECAN\n");
    printf(OUTFILE "Release Site Type/Site ID:         OU/NSSL CLAMPS Mobile\n");
    printf(OUTFILE "Release Location (lon,lat,alt):    %03d %05.2f'%s, %02d %05.2f'%s, %.3f, %.3f, %.1f\n",
	     			abs($lon_deg),$lon_min,$lon_dir,abs($lat_deg),$lat_min,$lat_dir,$baselon,$baselat,$basealt);  
    printf(OUTFILE "UTC Release Time (y,m,d,h,m,s):    %04d, %02d, %02d, %02d:%02d:%02d\n", 
					$year,$month,$day,$hour,$minute,$second);         	
    printf(OUTFILE "Sonde Id/Sonde Type:               %s\n", $sondeType);
    printf(OUTFILE "Ground Station Equipment:          iMET-3050\n");
    printf(OUTFILE "/\n/\n/\n/\n");
    # printf(OUTFILE "Nominal Release Time (y,m,d,h,m,s):%04d, %02d, %02d, %02d:%02d:%02d\n", 
	#     			$nomYear,$nomMonth,$nomDay,$nomHour,$nomMinute,$nomSecond);
    printf(OUTFILE "Nominal Release Time (y,m,d,h,m,s):%04d, %02d, %02d, %02d:%02d:%02d\n", 
	    			$year,$month,$day,$hour,$minute,$second);

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
    my $recdimLen = shift;
    my $var = shift;
	my $basetime = shift;

	my $nomOffset;

    # create a hash to store the data for each record
	my %output = ();

    # ------------------------------------------------------
	# Read in the file of surface elevations provided by Scot
	# to find the surface value and the amount to adjust the
	# remaining altitude data records
	# ------------------------------------------------------
	my @surfaceElevation = &readSurfaceElevFile();
	# print Dumper(@surfaceElevation);

    my $filename;
	my $sfc_elev;
	foreach my $line(@surfaceElevation)
	{
		chomp($line);
		($filename, $sfc_elev) = split(' ', $line);
		# print "FILE: $filename  SURFACE ELEV: $sfc_elev\n";
		# print "OUTPUT FILE $outfile\n";
		my @file_match_name = split(/\//, $outfile);
		if ($filename =~ /$file_match_name[2]/)
		{
			print "FOUND MATCHING FILE: $filename MATCHES $file_match_name[2]\n";
			
			$basealt = $sfc_elev;
			print "ALT ADJ = $basealt\n";
			last;
		}
	}


    foreach (my $recnum=0; $recnum < $recdimLen; $recnum++) 
	{
		if (&DEBUGoutput) {print "\n\n";}
    	if (&DEBUGoutput) {print "Processing record $recnum\n\n";}

    	# ----------------------------------------------------
    	# Get the lat/lon information from recnum 0 for 
    	# the header and print the header to the file
    	# ----------------------------------------------------
		if ($recnum == 0)
		{
			# the "time" variable gives the offset since midnight
		    $nomOffset = getVar("time", $var, $recnum); 

            # Scot let me know that we need to change the
			# surface lat/lon values for these two files.
			if ($filename =~ /201506080300/)
			{
				    $baselat = 38.004;
				   	$baselon = -98.747;
			}
			elsif ($filename =~ /201506250300/)
			{
				    $baselat = 39.572;
				   	$baselon = -97.640;
			}
			else
			{
				$baselat = getVar("lat", $var, $recnum);
				$baselon = getVar("lon", $var, $recnum);
			}
			
			processHeaderInfo($basetime,$nomOffset);
		}
          
  	    # $recnum is the index of all the data for a single time.  
    	# Process this record and the next one.
    	for (my $i = $recnum; $i < $recdimLen && $i <= $recnum+1; $i++) 
    	# for (my $i = $recnum; $i < $recdimLen; $i++) # never exits loop 
		{
			$output{Time}[$i] = getVar("time_offset",$var,$i); #Time offset from base time
	  		if ($output{Time}[$i] == -9999) {$output{Time}[$i] = 9999.0 } 

	  		$output{Press}[$i] = getVar("pres",$var,$i); #Pressure (hPa)
	  		if ($output{Press}[$i] == -9999) {$output{Press}[$i] = 9999.0 } 

	  		$output{Temp}[$i] = getVar("tdry",$var,$i);  #Dry Bulb Temp (C)	 
	  		if ($output{Temp}[$i] == -9999) {$output{Temp}[$i] = 999.0 } 

	  		$output{Dewpt}[$i] = getVar("dp",$var,$i);   #Dewpoint Temp (C)
	  		if ($output{Dewpt}[$i] == -9999) {$output{Dewpt}[$i] = 999.0 } 
            # Handle dewpoints with >5 chars, e.g., -100.0
            # Using < -99.9 also catches -99.9, so added extra decimal places
	  		# if ($output{Dewpt}[$i] < -99.9999) 
	  		# NOTE: The value -99.98 rounds up to -100.0 and causes >130 char
			# line length, so change this to missing
	  		if ($output{Dewpt}[$i] <= -99.98) 
			{
				# print "Dewpt value greater than 5 chars $output{Dewpt}[$i]\n";
				$output{Dewpt}[$i] = 999.0 
			} 

	 		$output{RH}[$i] = getVar("rh",$var,$i);  #Relative Humidity (%)	  
	  		if ($output{RH}[$i] == -9999) {$output{RH}[$i] = 999.0 } 

			# -----------------------------------------------------------
			# If wind speed is greater than 999 (won't fit format)
			# set all four wind values to "missing"
			# -----------------------------------------------------------
	  		$output{spd}[$i] = getVar("wspd",$var,$i);

	  		if ($output{spd}[$i] == -9999) {$output{spd}[$i] = 999.0 } 

			if (($output{spd}[$i] > 999.0) || ($output{Time}[$i] == 0.0))
			{
				print "Set all wind values to missing for Time = $output{Time}[$i]\n";
				$output{spd}[$i] = 999.0;
				$output{dir}[$i] = 999.0;
				$output{Ucmp}[$i] = 9999.0;
				$output{Vcmp}[$i] = 9999.0;
			}
            else
			{
		  		$output{dir}[$i] = getVar("deg",$var,$i);  #Wind Direction (deg)	  
				# if ($output{dir}[$i] =~ /360/) {$output{dir}[$i] = 0 }
				if ($output{dir}[$i] >= 360.0) {$output{dir}[$i] = 0 }
	  			if ($output{dir}[$i] == -9999) {$output{dir}[$i] = 999.0 }

				my @uvwind = calculateUVfromWind($output{spd}[$i], $output{dir}[$i]);
				# print "UVWind values: @uvwind\n";

		  		$output{Ucmp}[$i] = $uvwind[0];   #Eastward Wind Component (m/s) 
		  		if ($output{Ucmp}[$i] == -9999) {$output{Ucmp}[$i] = 9999.0 } 

	  			$output{Vcmp}[$i] = $uvwind[1];  #Northward Wind Component (m/s)
	  			if ($output{Vcmp}[$i] == -9999) {$output{Vcmp}[$i] = 9999.0 } 
	        }

            # Two files have incorrect surface lat/lon values
	  		$output{Lon}[$i] = getVar("lon",$var,$i);  #East Longitude (deg)	  
	  		if ($output{Lon}[$i] == -9999) {$output{Lon}[$i] = 9999.0 } 
			if ($output{Time}[$i] == 0.0)
			{
				if ($filename =~ /201506080300/)
				{
					$output{Lon}[$i] = -98.747;
				}
				if ($filename =~ /201506250300/)
				{
					$output{Lon}[$i] = -97.640;
				}
			}
	  
            # Two files have incorrect surface lat/lon values
	  		$output{Lat}[$i] = getVar("lat",$var,$i);  #North Latitude (deg)	  
	  		if ($output{Lat}[$i] == -9999) {$output{Lat}[$i] = 999.0 } 
			if ($output{Time}[$i] == 0.0)
			{
				if ($filename =~ /201506080300/)
				{
					$output{Lat}[$i] = 38.004;
				}
				if ($filename =~ /201506250300/)
				{
					$output{Lat}[$i] = 39.572;
				}
			}
	  
	  		
			$output{Alt}[$i] = getVar("alt",$var,$i);  #altitude (m)	  
	  		if ($output{Alt}[$i] == -9999)
			{
				$output{Alt}[$i] = 99999.0;
				print "ALT WAS MISSING AT $output{Time}[$i] \n";
			}
   			elsif ($output{Time}[$i] == 0.0)
   			{
   				$output{Alt}[$i] = $basealt;
				# print "SETTING output to basealt at T=0\n";
   			}
			elsif ($output{Time}[$i] > 0.0)
			{

				# ---------------------------------------------------------------
				# Calculate geopotential height
	    	    #-----------------------------------------------------------------
    	    	# BEWARE:  For MP1, surface altitude set to zero, so we 
				# must calculate geopotential height. S. Loehrer has provided
				# initial surface altitude  
        		# call calculateAltitude(last_press,last_temp,last_dewpt,last_alt,
				#                        this_press,this_temp,this_dewpt,1)
				# Note that the last three parms in calculateAltitude
    	    	# are the pressure, temp, and dewpt for the current
	    	    # record. To check the altitude calculations, see
    	    	# the web interface tool at 
	        	#
		        # http://dmg.eol.ucar.edu/cgi-bin/conversions/calculations/altitude
				# NOTE: This code taken from VORTEX2 SUNY converter.
        		#-------------------------------------------------------------------
    	        # print "\nRECORD No. $i (i) - Calc Geopotential Height: prev press: $output{Press}[$i-1], prev temp: $output{Temp}[$i-1], prev alt $output{Alt}[$i-1],\n";
				# print "and current press: $output{Press}[$i] and current temp: $output{Temp}[$i]\n"; 
		        if ($output{Press}[$i-1] < 9990.0)
   			    {
            	    # ---------------------------------------------------------
					# print "\tTHIS PRESSURE: $data[2]\n";
	                # for 621_001.LOG, pressure is 0.000 and illegal divison
					# error occurs when calculateAltitude is called
        	        # ---------------------------------------------------------
					if ($output{Press}[$i] != 0.000)
					{
						$output{Alt}[$i] = calculateAltitude($output{Press}[$i-1], 
														 $output{Temp}[$i-1], 
								    					 $output{Dewpt}[$i-1], 
														 $output{Alt}[$i-1], 
														 $output{Press}[$i], 
														 $output{Temp}[$i],
														 $output{Dewpt}[$i], 1);

						# print "Calculated geopotential height: $output{Alt}[$i]\n";
					}
					else
					{
						print "Bad Pressure $output{Press}[$i] -- Geopotential height not calculated\n";
						$output{Alt}[$i] = 99999.0;
					}
		            if (!defined($output{Alt}[$i]))
					{
						print "WARNING: Was not able to calculate geopotential height\n";
						$output{Alt}[$i] = 99999.0;
					}
    	    	} # end calculate geopotential height
			}
            
            # ------------------------------------------------------------------
			# Need to calculate ascension rate
			#
            # $record->setAscensionRate( ($record->getAltitude() - $prev_alt) /
            #                          ($record->getTime() - $prev_time),"m/s");
            # ------------------------------------------------------------------
			if (($output{Time}[$i-1] !~ /9999/) && ($output{Time}[$i] !~ /9999/) &&
				($output{Alt}[$i-1] !~ /99999/) && ($output{Alt}[$i] !~ /99999/) &&
				($output{Time}[$i-1] != $output{Time}[$i]))
			{
				$output{Wcmp}[$i] = (($output{Alt}[$i] - $output{Alt}[$i-1]) /
									  ($output{Time}[$i] - $output{Time}[$i-1]));	

	  			if ($output{Wcmp}[$i] == -9999) 
				{
					$output{Wcmp}[$i] = 999.0;
				}
				
				if (&DEBUGascentRate) 
				{                    
					print "Previous time $output{Time}[$i-1] Previous alt $output{Alt}[$i-1] ";
					print "Current time $output{Time}[$i] Current Alt $output{Alt}[$i]\n";
					print "Ascension rate = $output{Wcmp}[$i]\n"; 
				}
					
			}             
			elsif (($output{Time}[$i-1] == 9999.0) && ($output{Alt}[$i-1] == 99999.0))
			{
				$output{Wcmp}[$i] = 999.0;
			}
			elsif ($output{Time}[$i] == 0.0)
			{
				# First record should have ascent rate set to missing
				$output{Wcmp}[$i] = 999.0;
			}

            # -------------------------------------------------------------
	  		# change qc values to either missing or unchecked (per Scot)
            # -------------------------------------------------------------
	  		$output{Qp}[$i] = getVar("qc_pres",$var,$i);    #QC Pressure
	  		if ($output{Press}[$i] == 9999.0) 
	  		{
		  		$output{Qp}[$i] = 9.0;
	 		}
	  		else
	  		{
		  		$output{Qp}[$i] = 99.0;
	  		}
	  		$output{Qt}[$i] = getVar("qc_tdry",$var,$i);    #QC Temp
	  		if ($output{Temp}[$i] == 999.0)
	  		{
		  		$output{Qt}[$i] = 9.0;
	  		}
	  		else
	  		{
		  		$output{Qt}[$i] = 99.0;
	  		}	  
	  		$output{Qrh}[$i] = getVar("qc_rh",$var,$i);     #QC RH
	  		if ($output{RH}[$i] == 999.0) 
	  		{
		  		$output{Qrh}[$i] = 9.0;
	  		}
	  		else
	  		{
		  		$output{Qrh}[$i] = 99.0;
	  		}
	  		$output{Qu}[$i] = getVar("qc_u_wind",$var,$i);  #QC Ucmp 
 	  		if ($output{Ucmp}[$i] == 9999.0) 
	  		{
		  		$output{Qu}[$i] = 9.0;
	  		}
	  		else
	  		{
		  		$output{Qu}[$i] = 99.0;
	  		}                          
	  		$output{Qv}[$i] = getVar("qc_v_wind",$var,$i);  #QC Vcmp
 	  		if ($output{Vcmp}[$i] == 9999.0) 
	  		{
		  		$output{Qv}[$i] = 9.0;
	  		}
	  		else
	  		{
		  		$output{Qv}[$i] = 99.0;
	  		}	                          
	  		$output{Qdz}[$i] = getVar("qc_asc",$var,$i);    #QC Wcmp
 	  		if ($output{Wcmp}[$i] == 999.0) 
	  		{
		  		$output{Qdz}[$i] = 9.0;
	  		}
	  		else
	  		{
		  		$output{Qdz}[$i] = 99.0;
	  		}            
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
			if ($recnum < 10000)
			{
				print OUTFILE $outputRecord;
			}
			elsif ($recnum == 10000)
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
    my $ncid = shift;
    my $recDimName = shift;
    my $var = shift;
    my $recdimLen = shift;	# number of records in the record dimension
	# my $file_name = shift;
    
	my $basetime;
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
            # If the variable has only one dimension (aka "dim"), 
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
			# the variable base_time has zero dimensions
			# we don't want to read it in for every record, just once is enough
			elsif ($var->{$variable}{ndims} == 0)
			{
				# there is only one data value in base_time
				if ($record == 0)
				{
					# print "Record # $record\n";
					my @coords = ($record);
                	my @counts = (1);
					my @value;
                	if (NetCDF::varget($ncid,$var->{$variable}{varid},\@coords,
                    	  \@counts, \@values) == -1) 
					{
						die "Can't get data for variable $variable:$!\n";     
					}
					foreach my $val (@values)
					{
				    	# print "$val\n";
						$basetime = $val;
					}
					# print "base_time = $basetime\n";
				}
			}
			           
            # Otherwise, warn the user that we don't know how to read in the
            # variable.
            #-------------------------------------------------------------------
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
	# &parse_data_rec(10,$var,$basetime);
    &parse_data_rec($recdimLen,$var,$basetime);
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
    foreach my $attname ("site_id","facility_id","serial_number") 
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
    # my @names = (base_time,time_offset,'time',pres,tdry,dp,rh,wspd,deg,
	# 			lon,lat,alt);
    my @names = (base_time,time_offset,pres,tdry,dp,rh,wspd,deg,
				lon,lat,alt);
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
# @signature void readSurfaceElevFile(file_name)
# <p>Read the contents of the file into an array.</p>
#
# @input $file_name The name of the raw data file to be read.
# @output array of surface elevations
##------------------------------------------------------------------------------
sub readSurfaceElevFile {
    my $self = shift;
    # my $file_name = shift;
    # my $file = sprintf("%s/%s",$self->{"RAW_DIR"},$file_name);

    open(my $FILE, sprintf("sfc_elev.txt")) or die("Can't read file into array\n");
    my @elev_data = <$FILE>;
    close ($FILE);

	return @elev_data;

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
    # Get information from the file name
	# V1 data: clampssonde.C1.b1.20150601.030000.cdf (example file name)
	# V2 data: clampssondeC1.b1.20150601.030000.cdf
   	# facility_id: C1: Lamont, Oklahoma; site_id: clampsonde; proc level: = b1
   	# Parse the GNSS ID from the filename -- used in cosmic2raob.pl
    #-----------------------------------------------------------------    	
   	my @parts = split '\.',$file_name;
   	my $gnssid = $parts[0];
   	my $procLevel = $parts[1];
		
    #-----------------------------------------------------------------
    # Get the date and time from the file name
    #-----------------------------------------------------------------    	
	my $dateInfo = $parts[2];
   	if ($dateInfo =~ /(\d{4})(\d{2})(\d{2})/)
   	{
   		($year, $month, $day) = ($1,$2,$3);
		print "FILE NAME DATE: $year $month $day\n";
   	}
		
   	my $timeInfo = $parts[3];
   	if ($timeInfo =~ /(\d{2})(\d{2})(\d{2})/)
   	{
   		($hour, $minute, $second) = ($1,$2,$3);
   	}


    #-------------------------------------------------
    # Open the output file in the ../output directory.
    #-------------------------------------------------
   	my $ext = &getOutfileExtension;
    # HARD-CODED value
	# my $sndType = "CLAMPS"; # not used
	# get the global attributes for this file
	# commented out for PECAN MP1 since these values
	# are not found in the raw data global attributes
   	# %global_atts = get_global_atts($ncid);
	# my $facilityId = $global_atts{"facility_id"};
	# print "FACILITY ID: $facilityId\n";
     
	# my $outfile; 
    $outfile; 
    if (!$limit)
	{

        $outfile = sprintf "%s/%s_%04d%02d%02d%02d%02d%s", $self->{"VERBOSE_OUTPUT_DIR"},
	                       $self->{"NETWORK"}, $year, 
		   				   $month, $day, $hour, $minute, $ext; 
        open (OUTFILE,">$outfile")
           or die "Can't open verbose output file $outfile:$!\n";       
	}
	else
	{

        $outfile = sprintf "%s/%s_%04d%02d%02d%02d%02d%s", $self->{"OUTPUT_DIR"},
	                       $self->{"NETWORK"}, $year, 
		   				   $month, $day, $hour, $minute, $ext; 
        open (OUTFILE,">$outfile") 
           or die "Can't open output file $outfile:$!\n";        
	}
    print "\tOutput file name: $outfile\n";
    #-------------------------------------------------
    # Read all the data from the entire input netCDF file, outputting
    # ascii as we go. (output is included within this input routine)
    #-------------------------------------------------
    (my $recDimName, my $var, my $recdimsize) = readNetCDFheader($file);
    getData($ncid,$recDimName, $var, $recdimsize);
    # getData($ncid,$recDimName, $var, $recdimsize, $file_name);


    #-----------------------------------------------------------------
    # Create the station
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
	$station->insertDate($nomDate, "YYYY, MM, DD");         
	
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
    # $station->setStateCode(99);
    $station->setReportingFrequency("6-hourly");
    $station->setNetworkIdNumber(99);
    # 591	Radiosonde, iMet-1
    $station->setPlatformIdNumber(591);
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

