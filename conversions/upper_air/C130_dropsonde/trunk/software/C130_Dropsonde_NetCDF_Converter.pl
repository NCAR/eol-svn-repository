#! /usr/bin/perl 
#
##Module-------------------------------------------------------------------------
# <p>The previous (original was ISS not C130) ISS_Dropsonde_NetCDF_Converter.pl script is used for converting 
# ISS AVAPS NetCDF sounding data files to the EOL Sounding Composite (ESC) Format.
#
# @usage ISS_Dropsonde_NettCDF_to_ESC.pl [--limit] [--num_soundings=8] [--max=10]
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
# @author Linda Echo-Hawk 7 April 2023
# @veersion SWEX ISS AVAPS data conversion
#          - This converter is based on the CACTI ARM converter for RELAMPAGO.
#            with code to determine correct facility ID based on the facility_id
#            global attribute, the sonde ID is determined from the serial_number 
#            global attribute, and the release time from the file name. Nominal 
#            time is the same as the release time, and in 2010 a check was added
#            for dewpoint < -99.999 which gets rounded up to -100.0 and are 
#            greater than the allowed 5 characters. These values are changed 
#            to "missing."
#          - Set the command line switch (limit) for soundings collection frequency.  
#            This will determine which code to use for determining nominal time,
#            and can either be "4" (6-hourly) or "8" (3-hourly) soundings per day.  
#            If soundings are collected at different frequency than this, new
#            code may need to be added. NOTE: Default limit is set for 
#            4 soundings per day (6 hourly).
#          - BEWARE:  The file's base_time variable no longer indicates
#            the release time.  If you need code for netCDF files that 
#            have base time = release time, then use the DC3 version 
#            of the ARM converter.

# BEWARE:  The SCUDS skew-t generator cannot handles files with >9999
#          records (per Scot Loehrer to Linda Cully, 2008-11-12).
#
##Module-------------------------------------------------------------------------
package ARM_NetCDF_Converter;
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

# PERL standard module which allows the code to refer to each component of the 
# time by name, i.e. ->year
use Time::gmtime;

# Routines ParseDate and UnixDate are contained within this CPAN module
use Date::Manip;  

my ($WARN);

# -------------------------------------
# too many global variables
# -------------------------------------
my $file_name;
my $year;
my $month;
my $day;
my $hour;
my $minute;
my $second;

my $nomYear;
my $nomMonth;
my $nomDay;

my $basetime;
my $baselat;
my $baselon;
my $basealt;
my $lat_fmt;
my $lon_fmt;
my %global_atts;


printf "\nC130_Dropsonde_NetCDF_Converter.pl began on ";print scalar localtime;printf "\n";

# read command line arguments 
my $result;   
# $num_soundings can be 4 or 8, and the nominal time calculation will 
# change depending on which is selected - default is 4 (every 6 hours)         QUESTION
my $num_soundings;
# $three_hourly means 8 soundings per day, every three hours
my $three_hourly = 0;
# limit number of data records processed; default is process all records
my $limit;
# if ($limit), specify number of records to process; default is 5000
# HARD-CODED value
# my $maxRecords = 5000;
my $maxRecords = 10000;
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
printf "\nARMnetCDF_to_ESC.pl ended on ";print scalar localtime;printf "\n";


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
    my $converter = ARM_NetCDF_Converter->new();
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
    # $self->printStationFiles();  fails due to dup station locs
}


##------------------------------------------------------------------------------
# @signature ARM_NetCDF_Converter new()
# <p>Create a new ARM_NetCDF_Converter instance.</p>
#
# @output $converter The new converter.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;               #QUESTION
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);

    # $self->{"stations"} = ElevatedStationMap->new();

    # HARD-CODED
    $self->{"PROJECT"} = "CAESAR";
    # HARD-CODED
    $self->{"NETWORK"} = "ACS_NRD41";
    
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
	$basetime = shift;
    my $nomOffset = shift;
	$baselat = shift;
	$baselon = shift;
	$basealt = shift;

    # HARD-CODED value
	my $sondeType = "NCAR Research Dropsonde NRD41";

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


    #-----------------------------------------------------------------
    # Get the date and time from the file name
    #-----------------------------------------------------------------    	
    my @file_parts = split '\.',$file_name;
	my $dateInfo = $file_parts[2];
   	if ($dateInfo =~ /(\d{4})(\d{2})(\d{2})/)
   	{
   		($year, $month, $day) = ($1,$2,$3);
   	}
		
   	my $timeInfo = $file_parts[3];
   	if ($timeInfo =~ /(\d{2})(\d{2})(\d{2})/)  #QUESTION
   	{
   		($hour, $minute, $second) = ($1,$2,$3);
   	}

	# -------------------------------------------------------------
    # print out the header lines.
	# -------------------------------------------------------------
    printf(OUTFILE "Data Type:                         AVAPS SOUNDING DATA/Descending\n");
    printf(OUTFILE "Project ID:                        CAESAR\n");
    # printf(OUTFILE "Release Site Type/Site ID:         %s %s\n", $global_atts{"PlatformType"}, $global_atts{"PlatformId"});
    printf(OUTFILE "Release Site Type/Site ID:         NSF/NCAR C-130 N130AR\n");
    # printf(OUTFILE "Release Site Type/Site ID:         RSV Aurora Australis/VNAA\n");
    printf(OUTFILE "Release Location (lon,lat,alt):    %03d %05.2f'%s, %02d %05.2f'%s, %.3f, %.3f, %.1f\n",
	     			abs($lon_deg),$lon_min,$lon_dir,abs($lat_deg),$lat_min,$lat_dir,$baselon,$baselat,$basealt);  
    printf(OUTFILE "UTC Release Time (y,m,d,h,m,s):    %04d, %02d, %02d, %02d:%02d:%02d\n", 
					$year,$month,$day,$hour,$minute,$second);         	
    printf(OUTFILE "Radiosonde Type:                   %s\n", $sondeType);
	printf(OUTFILE "Radiosonde System Software:        Aspen V4.0.2\n");
    printf(OUTFILE "Radiosonde Serial Number:          %s\n", $global_atts{"SondeId"});
	if ($global_atts{"Comment"})
	{
		printf(OUTFILE "Comment:                           %s\n", $global_atts{"Comment"});
	}
	else
	{
        printf(OUTFILE "Comment:                           None\n");
	}
    printf(OUTFILE "/\n/\n");
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
	$basetime = shift;

	print "\n\tNum records = $recdimLen\n";
	
	my $nomOffset;
	my @output_array;
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
		    $nomOffset = getVar("time", $var, $recnum); 
			print "NomOffset = $nomOffset\n";
		}

    	# ----------------------------------------------------
  	    # $recnum is the index of all the data for a single time.  
    	# Process this record and the next one.
    	# ----------------------------------------------------
    	for (my $i = $recnum; $i < $recdimLen && $i < $recnum+1; $i++) 
		{
			#$output{Time}[$i] = (getVar("time_offset",$var,$i) - $nomOffset); 
			##Time offset from base time in older NetCDF versions
			$output{Time}[$i] = (getVar("time",$var,$i) );
	  		if ($output{Time}[$i] =~ /-9999/) {$output{Time}[$i] = 9999.0 } 
			
	  		$output{Press}[$i] = getVar("pres",$var,$i); #Pressure (hPa)
	  		if ($output{Press}[$i] =~ /-999/) {$output{Press}[$i] = 9999.0 } 

	  		$output{Temp}[$i] = getVar("tdry",$var,$i);  #Dry Bulb Temp (C)	 
	  		if ($output{Temp}[$i] =~ /-999/) {$output{Temp}[$i] = 999.0 } 

	  		$output{Dewpt}[$i] = getVar("dp",$var,$i);   #Dewpoint Temp (C)
	  		if ($output{Dewpt}[$i] =~ /-999/) {$output{Dewpt}[$i] = 999.0 } 
            # Handle dewpoints with >5 chars, e.g., -100.0
            # Using < -99.9 also catches -99.9, so added extra decimal places
	  		# if ($output{Dewpt}[$i] < -99.9999) 
	  		# NOTE: The value -99.98 rounds up to -100.0 and causes >130 char
			# line length, so change this to missing
	  		# if ($output{Dewpt}[$i] <= -99.98) 
	  		if ($output{Dewpt}[$i] <= -99.95) 
			{
				# print "Dewpt value greater than 5 chars $output{Dewpt}[$i]\n";
				$output{Dewpt}[$i] = 999.0 
			} 

	 		$output{RH}[$i] = getVar("rh",$var,$i);  #Relative Humidity (%)	  
	  		if ($output{RH}[$i] =~ /-999/) {$output{RH}[$i] = 999.0 } 

	  		$output{Ucmp}[$i] = getVar("u_wind",$var,$i);  #Eastward Wind Component (m/s)	  
	  		if ($output{Ucmp}[$i] =~ /-999/) {$output{Ucmp}[$i] = 9999.0 } 

	  		$output{Vcmp}[$i] = getVar("v_wind",$var,$i);  #Northward Wind Component (m/s)
	  		if ($output{Vcmp}[$i] =~ /-999/) {$output{Vcmp}[$i] = 9999.0 } 

	  		$output{spd}[$i] = getVar("wspd",$var,$i);  #Wind Speed (m/s)	  
	  		if ($output{spd}[$i] =~ /-999/) {$output{spd}[$i] = 999.0 } 

	  		$output{dir}[$i] = getVar("wdir",$var,$i);  #Wind Direction (deg)	  
	  		if ($output{dir}[$i] =~ /-999/) {$output{dir}[$i] = 999.0 } 

	  		$output{Wcmp}[$i] = getVar("dz",$var,$i);  #Ascent Rate (m/s)
	  		if ($output{Wcmp}[$i] =~ /-999/) {$output{Wcmp}[$i] = 999.0 } 

            # ----------------------------------------------------------------
			# Files have several lines of missing lat/lon values at the 
			# end of the file. To determine that last valid lat/lon,
			# we set $baselon and $baselat with the current value if it's
			# not missing. $basealt will always be missing for the 
			# SWEX NSSL dropsondes.
            # ----------------------------------------------------------------

	  		$output{Lon}[$i] = getVar("lon",$var,$i);  #East Longitude (deg)	  
	  		if ($output{Lon}[$i] =~ /-999/) 
			{
				$output{Lon}[$i] = 9999.0; 
			}
			else
			{
				$baselon = $output{Lon}[$i];
			}
	  
	  		$output{Lat}[$i] = getVar("lat",$var,$i);  #North Latitude (deg)	  
	  		if ($output{Lat}[$i] =~ /-999/) 
			{
				$output{Lat}[$i] = 999.0; 
			}
			else
			{
				$baselat = $output{Lat}[$i];
			}

	  		$output{Alt}[$i] = getVar("alt",$var,$i);  #altitude (m)	  
	  		if ($output{Alt}[$i] =~ /-999/) 
			{
				$output{Alt}[$i] = 99999.0;
				$basealt = $output{Alt}[$i];
			}
            # ----------------------------------------------------------------
	  		

			
			# -------------------
			# change qc values to either missing (9) or unchecked (99) (per Scot)
			# -------------------
	  		$output{Qp}[$i] = getVar("qc_pres",$var,$i);    #QC Pressure
	  		if ($output{Press}[$i] == 9999.0) 
	  		{
		  		$output{Qp}[$i] = 9.0; # missing
	 		}
	  		else
	  		{
		  		$output{Qp}[$i] = 99.0; # unchecked
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
	  		
			$output{Qdz}[$i] = getVar("qc_dz",$var,$i);    #QC Wcmp
 	  		
			if ($output{Wcmp}[$i] == 999.0) 
	  		{
		  		$output{Qdz}[$i] = 9.0;
	  		}
	  		else
	  		{
		  		$output{Qdz}[$i] = 99.0;
	  		}            

	  	} # End for $i == $recnum loop, Process this record a


        # ---------------------
	    # print output here
        # ---------------------
		#
		
     	my $outputRecord = sprintf "%6.1f %6.1f %5.1f %5.1f %5.1f %6.1f %6.1f %5.1f %5.1f %5.1f %8.3f %7.3f 999.0 999.0 %7.1f %4.1f %4.1f %4.1f %4.1f %4.1f %4.1f\n",
	 		$output{Time}[$recnum], $output{Press}[$recnum], $output{Temp}[$recnum], 
	 		$output{Dewpt}[$recnum], $output{RH}[$recnum], $output{Ucmp}[$recnum],$output{Vcmp}[$recnum],
	 		$output{spd}[$recnum], $output{dir}[$recnum], $output{Wcmp}[$recnum],$output{Lon}[$recnum],
	 		$output{Lat}[$recnum], $output{Alt}[$recnum], $output{Qp}[$recnum], $output{Qt}[$recnum],
	 		$output{Qrh}[$recnum], $output{Qu}[$recnum], $output{Qv}[$recnum], $output{Qdz}[$recnum];


		# We need to limit the data records to times < 10000 seconds.
    	# For the ARM 2-second data soundings, 5000 records is the max.
		# NOTE: For the SWEX AVAPS dropsondes, no limit was needed.
    	if ($limit)
		{
            # HARD-CODED value
			# if ($recnum < 5000)
			if ($recnum < 10000)
			{
				print OUTFILE $outputRecord;
			}
			# elsif ($recnum == 5000)
			elsif ($recnum == 10000)
			{
    			# print "Record number = $recnum\n";
        		print "\tLimited processing to records with times less than 10000.0 seconds.  Remaining records were cut off.\n";
			}
		}
		else
		{
	   		# print OUTFILE $outputRecord;  # push into array
			push (@output_array, $outputRecord);
		}
	} # End for $recnum loop
    
	
    processHeaderInfo($basetime,$nomOffset,$baselat,$baselon,$basealt);
	#foreach my $line (reverse(@output_array))
	#ISS AVAPS dropsonde files are already in the correct order
	foreach my $line (@output_array)
	{
		print OUTFILE $line;
	}
      
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
    
	$basetime;
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
			# the variable base_time has zero dims
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
				    	#print "$val\n";
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
    # foreach my $attname ("site_id","facility_id","serial_number") 
    foreach my $attname ("Comment","SondeId","Flight","Project","PlatformType","PlatformId") 
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

		    # remove the null string terminator from each string - 
			# not needed for SWEX AVAPS
			
		    
		    # removes last character of value (Twin Otte instead of Twin Otter) 
		    # splice(@value, -1);
	        
			
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

	 print Dumper(%global_atts);
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
    #my @names = (base_time,time_offset,'time',pres,tdry,dp,rh,u_wind,v_wind,wspd,deg,
	#			asc,lon,lat,alt,qc_time,qc_pres,qc_tdry,qc_dp,qc_wspd,qc_deg,qc_rh,qc_u_wind,qc_v_wind,qc_asc);
    #my @names = (launch_time,'time',pres,tdry,dp,rh,u_wind,v_wind,w_wind,wspd,wdir,
	#                dz,lat,lon,alt,reference_lat,reference_lon,reference_alt);
    my @names = (launch_time,'time',pres,tdry,dp,rh,u_wind,v_wind,w_wind,wspd,wdir,
	                dz,lat,lon,alt);
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
    # my $file_name = shift;
    $file_name = shift;
    my $file = sprintf("%s/%s",$self->{"RAW_DIR"},$file_name);

    printf("\nProcessing file: %s\n",$file_name);
   	my $ncid;
   	if (($ncid = NetCDF::open($file,0)) == -1) 
	{
           die "Cannot open file: $file\n";
   	}   
	else 
	{
		print "NCID  $ncid\n";

	}

    #-----------------------------------------------------------------
    # Get information from the file name
	# corsondewnpnM1.b1.20181015.000000.cdf (example file name)
	# There are two ARM Mobile Facilities M1 and S1 for RELAMPAGO-CACTI
	# COR M1: Cordoba, Argentina; Mobile Facility AMF1(CACTI), AMF1 (main site for CACTI)
	# COR S1: Cordoba, Argentina;  Mobile Facility (CACTI), BBSS sonde (at airport)
	# 
	# sgpsondewnpnC1.b1.20090630.172800.cdf (example file name)
   	# facility_id: C1: Lamont, Oklahoma; site_id: sgp; proc level: = b1
   	# Parse the GNSS ID from the filename -- used in cosmic2raob.pl
    #-----------------------------------------------------------------    	
   	# my @parts = split '\.',$file_name;
   	my @parts = split '_',$file_name;
   	my $facility = $parts[1];
	if ($facility =~ /AVAPS$/)
	{
		$facility = "AVAPS";
	}
	else
	{
		$facility = "AVAPS";
	}
   	# my $procLevel = $parts[1];
		
    #-----------------------------------------------------------------
    # Get the date and time from the file name
	# this code is repeated.... TODO
    #-----------------------------------------------------------------    	
	my $dateInfo = $parts[5];


	if ($dateInfo =~ /SDR/)
	{
		$dateInfo = $parts[6];
	}
        
	print "This is the date $dateInfo \n";
	

        if ($dateInfo =~ /(\d{4})(\d{2})(\d{2})/)
        {
                ($year, $month, $day) = ($1,$2,$3);
        }

       #Then split the $date_Info on the "T"
      
        my @date_parts = split 'T',$dateInfo;


        my $timeInfo = $date_parts[1];
       
        print "This is the time $timeInfo \n";

	if ($timeInfo =~ /(\d{2})(\d{2})(\d{2})/)
        {
                ($hour, $minute, $second) = ($1,$2,$3);
        }


	#if ($dateInfo =~ /(\d{4})(\d{2})(\d{2})/)
	#{
	#	($year, $month, $day) = ($1,$2,$3);
	#}
		
	#my $timeInfo = $parts[5];
	#if ($timeInfo =~ /(\d{2})(\d{2})(\d{2})/)
	#{
	#	($hour, $minute, $second) = ($1,$2,$3);
	#}


    #-------------------------------------------------
    # Open the output file in the ../output directory.
    #-------------------------------------------------
   	my $ext = &getOutfileExtension;
    # HARD-CODED value
	my $sndType = "NRD41";
	# get the global attributes for this file
   	%global_atts = get_global_atts($ncid);
	my $facilityId = $global_atts{"facility_id"};
	print "FACILITY ID: $facilityId\n";
	my $facilityId = "AVAPS";
     
	my $outfile; 
    if (!$limit)
	{

        $outfile = sprintf "%s/%s_%04d%02d%02d%02d%02d%02d%s", $self->{"VERBOSE_OUTPUT_DIR"},
	                       $self->{"NETWORK"}, $year, 
		   				   $month, $day, $hour, $minute, $second, $ext; 
        open (OUTFILE,">$outfile")
           or die "Can't open output file $outfile:$!\n";       
	}
	else
	{

        $outfile = sprintf "%s/%s_%s_%04d%02d%02d%02d%02d%02d%s", $self->{"OUTPUT_DIR"},
	                       $self->{"NETWORK"}, $year, 
		   				   $month, $day, $hour, $minute, $second, $ext; 
        open (OUTFILE,">$outfile") 
           or die "Can't open verbose output file $outfile:$!\n";        
	}
    print "\tOutput file name: $outfile\n";
    #-------------------------------------------------
    # Read all the data from the entire input netCDF file, outputting
    # ascii as we go. (output is included within this input routine)
    #-------------------------------------------------
    (my $recDimName, my $var, my $recdimsize) = readNetCDFheader($file);
    getData($ncid,$recDimName, $var, $recdimsize);


    #-----------------------------------------------------------------
    # All soundings come from two stations: M1 and S1
    #-----------------------------------------------------------------
#    my $station = $self->{"stations"}->getStation($self->{"NETWORK"},$self->{"NETWORK"}, 											$baselat, $baselon, $basealt);
#    if (!defined($station)) {
#        $station = $self->build_default_station($self->{"NETWORK"},$self->{"NETWORK"});
#		$station->setLatitude($baselat, $lat_fmt);
#		$station->setLongitude($baselon, $lon_fmt);
#		$station->setElevation($basealt,"m");
#        $self->{"stations"}->addStation($station);
#       }
    my $nomDate = sprintf("%04d, %02d, %02d",$year,$nomMonth,$nomDay);  
	# print "Nominal Date for Station:  $nomDate\n";
#	$station->insertDate($nomDate, "YYYY, MM, DD");         
	
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
    $station->setStateCode("40");
    $station->setReportingFrequency("6-hourly");
    $station->setNetworkIdNumber(99);
	# Platform 1179	Vaisala RS41-SGP
    $station->setPlatformIdNumber(1179);
    #$station->setMobilityFlag("m");
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

