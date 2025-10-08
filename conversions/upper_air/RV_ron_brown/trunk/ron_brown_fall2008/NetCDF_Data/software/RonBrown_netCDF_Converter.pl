#! /usr/bin/perl 
#
##Module-------------------------------------------------------------------------
# <p>The RonBrown_netCDFtoESC.pl script is used for converting the 
# Ron Brown NetCDF data to the EOL Sounding Composite (ESC) format.
#
#
# @author Linda Echo-Hawk 2010_03_08
# @version VOCALS_2008  This converter was designed for the Ron Brown
#          data, based on the ARMnetCDF converter written for VORTEX2. 
#          - All sounding data was in one netCDF file.  
#          - The data is:
#               "vertically interpolated to standard 10-m height
#               increments.  For a nominal ascent rate of 5 m/s 
#               the 10 m height interval corresponds roughly to 
#               the 2 s time interval" (from the readme doc).  
#          - The header variables are one-dimensional variables,
#            and the data variables are two-dimensional, with the 
#            exception of 1-d height.  
#          - Each of the 216 soundings contains 2501 data entries.  
#          - The converter uses a series of loops to read the data 
#            associated with each sounding. 
#          - Code removes last lines with no valid data (missing 
#            pressure values), because not all soundings had 2501 
#            valid data points.
#          - Search for "HARD-CODED" to find values that may need 
#            to be changed for other projects.
#
##Module-------------------------------------------------------------------------
package RonBrown_netCDF_Converter;
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
	  "attempting to run the code. Please run this code on merlot\n".
	  "or tsunami.\n\n"; 
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
use SimpleStationMap;
use Station;

# PERL standard module which allows the code to refer to each component of the 
# time by name, i.e. ->year
use Time::gmtime;

# Routines ParseDate and UnixDate are contained within this CPAN module
use Date::Manip;  

my ($WARN);

# global vars
my $relYear;
my $relMonth;
my $relDay;


printf "\nRonBrown_netCDF_Converter.pl began on ";print scalar localtime;printf "\n";
&main();
printf "\nRonBrown_netCDF_Converter.pl ended on ";print scalar localtime;printf "\n";

#*********************************************************************

# There are a ton of print statements in this code for debugging and 
# informational purposes.  Turn them on or off and see what you get (-:
sub DEBUG       {return 0; }
sub DEBUGoutput {return 0;}		# debug info for parse_data_rec subroutine
sub DEBUGgetV {return 0;}               # debug info for netCDF subroutine
sub DEBUGFileStats {return 1;}

# I like to define variables FALSE and TRUE and use them rather than one
# and zero in by comparison statements.  I think it is clearer.
sub FALSE       {return 0; }
sub TRUE        {return 1; }

#*********************************************************************
#-------------------------------------------------
# A collection of functions that contain constants
# HARD-CODED values
#-------------------------------------------------
sub getNetworkName { return "RonBrown";  }                        
sub getOutputDirectory { return "../output"; }
sub getProjectName { return "VOCALS_2008"; }
sub getRawDirectory { return "../raw_data"; }
sub getStationFile { return sprintf("../final/%s_%s_stationCD.out",
				    getNetworkName(),getProjectName()); }
sub getSummaryFile { return "../output/station_summary.log"; }
sub getWarningFile { return "../output/warning.log"; }

##------------------------------------------------------------------------------
# @signature void main()
# <p>Execute the conversion of the data.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = RonBrown_netCDF_Converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature void convert()

# <p>Convert all of the raw data and create the output files for the conversion.</p>
##------------------------------------------------------------------------------
sub convert {
	my ($self) = @_;

    mkdir(getOutputDirectory()) unless (-e getOutputDirectory());

    mkdir("../final") unless (-e "../final");

    $self->readRawDataFiles();
    $self->printStationFiles();
}


##------------------------------------------------------------------------------
# @signature RonBrown_netCDF_Converter new()
# <p>Create a new RonBrown_netCDF_Converter instance.</p>
#
# @output $converter The new converter.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);

    $self->{"stations"} = SimpleStationMap->new();

    $self->{"FINAL_DIR"} = "../final";
    $self->{"NETWORK"} = getNetworkName();
    $self->{"PROJECT"} = getProjectName(); 

    $self->{"STATION_FILE"} = sprintf("%s/%s_%s_sounding_stationCD.out",$self->{"FINAL_DIR"},
                                      $self->clean_for_file_name($self->{"NETWORK"}),
                                      $self->clean_for_file_name($self->{"PROJECT"}));

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

    open($STN, ">".$self->getStationFile()) || die("Cannot create the ".$self->getStationFile()." file\n");
    foreach my $station ($self->{"stations"}->getAllStations()) {
        print($STN $station->toString()) if ($station->getBeginDate !~ /^9+$/);
    }

    close($STN);

    open($SUMMARY, ">".$self->getSummaryFile()) || die("Cannot create the ".$self->getSummaryFile()." file.\n");
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

    opendir(my $RAW,$self->getRawDirectory()) or die("Cannot open raw directory.\n");
    my @files = readdir($RAW);
    closedir($RAW);

    open($WARN,">".$self->getWarningFile()) or die("Can't open warning file.\n");

    # there is only one data file, so this is not really necessary
    foreach my $file (sort(@files)) {
	$self->readRawFile($file) if ($file =~ /\.nc$/);
    }

    close($WARN);
}

#---------------------------------------------------------------------
# @signature void parse_data_rec()
#
#---------------------------------------------------------------------
sub parse_data_rec
{
    my $soundingLen = shift; # sounding length is 2501
    my $var = shift;
    my $sounding = shift; # 216 total soundings

    my $baselat;
    my $baselon;
    my $basealt;
    my $relHour;
    my $relMinute;
    my $lat_fmt;
    my $lon_fmt;
    my $qualFlag;
	my $qcRemark = "";

    my %output = ();
    my $surfaceRecord = 1;

    foreach (my $recnum=0; $recnum < $soundingLen; $recnum++) 
	{
		if (&DEBUGoutput) {print "\n\n";}
    	if (&DEBUGoutput) {print "Processing record $recnum, sounding $sounding\n\n";}

    	# ----------------------------------------------------
    	# Get the lat/lon info from recnum=$sounding 
    	# for the header and print the header to the file
    	# ----------------------------------------------------
		if ($recnum == $sounding)
		{
			$baselat = getVar("lat", $var, $recnum);
			$baselon = getVar("lon", $var, $recnum);
			$basealt = 0;
			$relYear = getVar("year", $var, $recnum);
			$relMonth = getVar("month", $var, $recnum);
			$relDay = getVar("day", $var, $recnum);
			$relHour = getVar("hour", $var, $recnum);
			$relMinute = getVar("minute", $var, $recnum);
			$qualFlag = getVar("flag", $var, $recnum);

			# print "QUAL FLAG = $qualFlag\n";

			if ($qualFlag == 0) 
			{ 
				$qcRemark = "Sonde OK"; 
				# print "SOUNDING: $sounding   REMARK: $qcRemark\n";
			}
			elsif ($qualFlag == 84) # T flag
			{
				$qcRemark = "Near surface temp data removed (> 1C diff from mast temp)";
				# print "SOUNDING: $sounding   REMARK: $qcRemark\n";
			}
			elsif ($qualFlag == 99) # c flag
			{ 
				$qcRemark = "RH has small amplitude cycles; data kept"; 
				# print "SOUNDING: $sounding   REMARK: $qcRemark\n";
			}
			elsif ($qualFlag == 67) # C flag
			{ 
				$qcRemark = "RH has large amplitude cycles; data removed"; 

				# print "SOUNDING: $sounding   REMARK: $qcRemark\n";
			}

			elsif ($qualFlag == 72) # H flag
			{ 
				$qcRemark = "RH out of range; RH data removed"; 
				
				# print "SOUNDING: $sounding   REMARK: $qcRemark\n";
			}
			else
			{
				print "WARNING: UNKNOWN QC FLAG ENCOUNTERED:  $qualFlag\n";
			}
			
	        # -------------------------------------------------------------
            # Use the lat/lon info for header and surface lat/lon
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


            #-------------------------------------------------
            # Open the output file in the ../output directory.
            #-------------------------------------------------
	        my $outfile; 
	        my $site = "WTEC";
			my $relSecond = "00";
            $outfile = sprintf "%s/%s_%04d%02d%02d%02d%02d.cls", getOutputDirectory(), 
								$site,$relYear,$relMonth,$relDay,$relHour,$relMinute;
            open (OUTFILE,">$outfile") 
                 or die "Can't open output file $outfile:$!\n";       
 
            # -------------------------------------------------------------

			# print "Printing header info for recnum $recnum, sounding $sounding\n\n";
            printf(OUTFILE "Data Type:                         Ron Brown Soundings/Ascending\n");
            printf(OUTFILE "Project ID:                        VOCALS_2008\n");
            printf(OUTFILE "Release Site Type/Site ID:         R/V Ron Brown/WTEC\n");
            printf(OUTFILE "Release Location (lon,lat,alt):    %03d %05.2f'%s, %02d %05.2f'%s, %.3f, %.3f, %.1f\n",
	    			abs($lon_deg),$lon_min,$lon_dir,abs($lat_deg),$lat_min,$lat_dir,$baselon,$baselat,$basealt);  
            printf(OUTFILE "UTC Release Time (y,m,d,h,m,s):    %04d, %02d, %02d, %02d:%02d:%02d\n", 
    				$relYear,$relMonth,$relDay,$relHour,$relMinute,$relSecond);         	
            # printf(OUTFILE "Sonde Id/Sonde Type:               %s/%s\n", $global_atts{"serial_number"}, $sondeType);
			printf(OUTFILE "Data Quality Notes:                %s\n", $qcRemark);                
            printf(OUTFILE "/\n/\n/\n/\n/\n");
            printf(OUTFILE "Nominal Release Time (y,m,d,h,m,s):%04d, %02d, %02d, %02d:%02d:%02d\n", 
          			$relYear,$relMonth,$relDay,$relHour,$relMinute,$relSecond);

            printf(OUTFILE " Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat    Ele   Azi   Alt    Qp   Qt   Qrh  Qu   Qv   QdZ\n");
            printf(OUTFILE "  sec    mb     C     C     %s     m/s    m/s   m/s   deg   m/s      deg     deg    deg   deg    m    code code code code code code\n","%");
             printf(OUTFILE "------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----\n"); 

		}
	}

    foreach (my $recnum=0; $recnum < $soundingLen; $recnum++) 
	{
		my $missingVerticalCoordinate = 0;
        my $time = 0;  
  	    # $recnum is the index of all the data for a single height.  
    	for (my $i = $recnum; $i < $soundingLen && $i <= $recnum+1; $i++)
		{
			$time = $recnum * 2;
 
	  		$output{Press}[$i] = getVar("pres",$var,$i); #Pressure (hPa)
	  		if ($output{Press}[$i] =~ /nan/i) {$output{Press}[$i] = 9999.0 } 

	  		$output{Temp}[$i] = getVar("T",$var,$i);  #Temp (C)	 
	  		if ($output{Temp}[$i] =~ /nan/i) {$output{Temp}[$i] = 999.0 } 

			if (($recnum > 5) && (($output{Temp}[$i] == 999.0) 
				&& ($output{Press}[$i] == 9999.0)))
			{
				$missingVerticalCoordinate = 1;
			}

	  		$output{Dewpt}[$i] = getVar("Td",$var,$i);   #Dewpoint Temp (C)
	  		if ($output{Dewpt}[$i] =~ /nan/i) {$output{Dewpt}[$i] = 999.0 } 

	 		$output{RH}[$i] = getVar("RH",$var,$i);  #Relative Humidity (%)	  
	  		if ($output{RH}[$i] =~ /nan/i) {$output{RH}[$i] = 999.0 } 

	  		$output{Ucmp}[$i] = getVar("u",$var,$i);  #Eastward Wind Component (m/s)	  
	  		if ($output{Ucmp}[$i] =~ /nan/i) {$output{Ucmp}[$i] = 9999.0 } 

	  		$output{Vcmp}[$i] = getVar("v",$var,$i);  #Northward Wind Component (m/s)
	  		if ($output{Vcmp}[$i] =~ /nan/i) {$output{Vcmp}[$i] = 9999.0 } 

	  		$output{spd}[$i] = getVar("wndspd",$var,$i);  #Wind Speed (m/s)	  
	  		if ($output{spd}[$i] =~ /nan/i) {$output{spd}[$i] = 999.0 } 

	  		$output{dir}[$i] = getVar("wnddir",$var,$i);  #Wind Direction (deg)	  
	  		if ($output{dir}[$i] =~ /nan/i) {$output{dir}[$i] = 999.0 } 
            
            # -------------------------------------------------
			# Ascent rate will always be 10m/2s, 
			# except for surface record "missing" value
            # --------------------------------------------------
            $output{Wcmp}[$i] = (10/2);
			# --------------------------------------------------
            # Lat/Lon values will be only once per sounding.
            # For surface record, use base values from above.
			# All other records should be "missing" values.
            # --------------------------------------------------
	        $output{Lon}[$i] = 9999.0;

	        $output{Lat}[$i] = 999.0;

	  		$output{Alt}[$i] = getVar("height",$var,$i);  #altitude (m)	  
	  		if ($output{Alt}[$i] =~ /nan/i) {$output{Alt}[$i] = 99999.0 } 
            
	  		# ----------------------------------------
            # set QC flags
	  		# ----------------------------------------
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
	  		#$output{Qdz}[$i] = getVar("qc_asc",$var,$i);    #QC Wcmp
 	  		if ($output{Wcmp}[$i] == 999.0) 
	  		{
		  		$output{Qdz}[$i] = 9.0;
	  		}
	  		else
	  		{
		  		$output{Qdz}[$i] = 99.0;
			}            

		} # End for $i=$recnum loop

        # ----------------------------------------                   
		my $surfaceAscent = 999.0;
		my $surfaceAscentFlag = 9.0;
        # ----------------------------------------                   

        my $outputRecord;
	    # print output here
		# Wcmp for all after surface should be 10/2 = 5
		if (!$surfaceRecord)
		{
        	$outputRecord = sprintf "%6.1f %6.1f %5.1f %5.1f %5.1f %6.1f %6.1f %5.1f %5.1f %5.1f %8.3f %7.3f 999.0 999.0 %7.1f %4.1f %4.1f %4.1f %4.1f %4.1f %4.1f\n",
	 		    $time, $output{Press}[$recnum], $output{Temp}[$recnum], 
	 		    $output{Dewpt}[$recnum], $output{RH}[$recnum], $output{Ucmp}[$recnum],
				$output{Vcmp}[$recnum],  $output{spd}[$recnum], $output{dir}[$recnum], 
			    $output{Wcmp}[$recnum], $output{Lon}[$recnum], $output{Lat}[$recnum], 
				$output{Alt}[$recnum], $output{Qp}[$recnum], $output{Qt}[$recnum],
	 		    $output{Qrh}[$recnum], $output{Qu}[$recnum], $output{Qv}[$recnum], 
				$output{Qdz}[$recnum];    
	    }
		else
		{
			# print out surface record version
            # Wcmp for surface should be 999.0 (missing)
			$outputRecord = sprintf "%6.1f %6.1f %5.1f %5.1f %5.1f %6.1f %6.1f %5.1f %5.1f %5.1f %8.3f %7.3f 999.0 999.0 %7.1f %4.1f %4.1f %4.1f %4.1f %4.1f %4.1f\n",
	 		    $time, $output{Press}[$recnum], $output{Temp}[$recnum], 
	 		    $output{Dewpt}[$recnum], $output{RH}[$recnum], $output{Ucmp}[$recnum],
				$output{Vcmp}[$recnum], $output{spd}[$recnum], $output{dir}[$recnum], 
				$surfaceAscent, $baselon, $baselat, $basealt, 
				$output{Qp}[$recnum], $output{Qt}[$recnum], $output{Qrh}[$recnum], 
				$output{Qu}[$recnum], $output{Qv}[$recnum], $surfaceAscentFlag;   
		    $surfaceRecord = 0;        
		}

        # put if !verticalcoordinate here
		if (!$missingVerticalCoordinate)
		{
		    # print the previous data record
	   	    print OUTFILE $outputRecord;
		}
		else
		{
			# print the last (previous) data record and end
			print OUTFILE $outputRecord;
			last;
		}

	} # End foreach (my $recnum) loop
	
    print "Done printing outfile for sounding $sounding\n";

    close(OUTFILE);
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
    my $var = shift;
    my $recdimLen = shift;	# number of records in the record dimension (216)
	my $sounding = shift;   # the current sounding (0 to 215)
    my $soundingLen = 2501;
	
	my $variable;
    foreach $variable (&getHeaderFields) 
	{
        #--------------------------------------------------------------------- 
        # Make sure that the variable the user has requested in getFields 
        # actually exists in the data.
        #--------------------------------------------------------------------- 
	    if (!defined($var->{$variable})) 
		{
            print "WARNING: Unknown variable $variable requested by user";
            print " in code at getHeaderFields declaration.\n";
            exit(1);
        }
    }
     
    foreach $variable (&getDataFields) 
	{
        #--------------------------------------------------------------------- 
        # Make sure that the variable the user has requested in getFields 
        # actually exists in the data.
        #--------------------------------------------------------------------- 
	    if (!defined($var->{$variable})) 
		{
            print "WARNING: Unknown variable $variable requested by user";
            print " in code at getDataFields declaration.\n";
            exit(1);
        }
    }            
	# We only want the value for 1-d header vars for that sounding
    # foreach (my $record=0; $record < 1; $record++) 
    foreach (my $record=$sounding; $record < $sounding+1; $record++) 
	{
        if (&DEBUG) {print "Reading in data for record $record\n";}
        # Loop through each parameter we want to extract from the raw data
        foreach $variable (&getHeaderFields) 
		{
            if (&DEBUG) {print "Reading in data for header variable $variable\n";}
            my @headerValues = ();     
            #-------------------------------------------------------------------
            # Note that varget saves the data to the first index of @values.
            # It appears to save the data point as a float, so information on 
			# significant digits is lost, and we get numbers like 38.4943313598633
            #-------------------------------------------------------------------
			#
			# For Ron Brown, header variables are 1-dimensional with respect to 
            # launch (year, yday, month, day, hour, minute, lat, lon, height & flag.
		    # We don't want the height data (varid=8) for the header though.
            #-------------------------------------------------------------------
            if (($var->{$variable}{ndims} == 1 ) && ($var->{$variable}{varid} != 8)) 
			{
                # my @start = ($record);
				# this is the starting point for data collection
                my @start = ($sounding);
                my @counts = (1);
                if (NetCDF::varget($ncid,$var->{$variable}{varid},\@start,
                      \@counts, \@headerValues) == -1) 
				{
					die "Can't get data for header variable $variable:$!\n";
                }
                $var->{$variable}{values}[$record] = $headerValues[0];
            }
		}
	}
    # Get the 216 quality flag values (one per sounding)
	# would be better to only get this once per file, instead of per sounding
    foreach (my $record=0; $record < $recdimLen; $record++) 
	{
        if (&DEBUG) {print "Reading in data for record $record\n";}

        # Loop through each parameter we want to extract from the raw data
        foreach $variable (&getQualityFields) 
		{
            if (&DEBUG) {print "Reading in data for data variable $variable\n";}

			my @qcValues = ();

            if (($var->{$variable}{ndims} == 1 ) && ($var->{$variable}{varid} == 19)) 
			{
                my @start = ($record);
                my @counts = (1);
				# print "calling varget for RECORD $record\n";
                if (NetCDF::varget($ncid,$var->{$variable}{varid},\@start,
                      \@counts,\@qcValues) == -1) 
				{
					die "Can't get data for height variable $variable:$!\n";
                }                                                          
                # -----------------------------------------------------------
                # for testing purposes
				# -----------------------------------------------------------
				# my $snd = $record + 1;
				# if ($qcValues[0] > 31)
				# {
			       # $qcValues[0] will hold an ASCII value (e.g., 84 = T)
			       # convert ASCII to char value
			    #    my $rep = chr($qcValues[0]);
				#    print "SOUNDING $snd  \tFLAG: $qcValues[0] is $rep\n";
				# }
				# else
			    # {
				#     print "SOUNDING $snd  \tFLAG: $qcValues[0]\n";
				# }
                
                $var->{$variable}{values}[$record] = $qcValues[0]; 
            }         

        } # End foreach $variable
    } # End foreach $record


    # Loop over each record in the netCDF file and read in the data
    foreach (my $record=0; $record < $soundingLen; $record++) 
	{
        if (&DEBUG) {print "Reading in data for record $record\n";}

        # Loop through each parameter we want to extract from the raw data
        foreach $variable (&getDataFields) 
		{
            if (&DEBUG) {print "Reading in data for data variable $variable\n";}

			# Dimension Length: Each sounding contains 2501 data points 
			# height of each sounding is 0-25000m (in increments of 10m)
			my @dataValues = ();
            #-------------------------------------------------------------------
			# For Ron Brown, data vars are 2-dimensional (launch, altitude):
			# pres, T, RH, Td, wnddir, wndspd, except for 1-d height.
			# @start: starting point for data transfer
			# @count: length of data points along each dimension for data transfer
            #-------------------------------------------------------------------
            if ($var->{$variable}{ndims} == 2 )
			{
                # @start will need to be incremented with each new sounding
				my @start = ($sounding,0);
				# my @count should not change - all soundings have 2501 values
				my @count = (1,$soundingLen);
				# my @count = (1,2501);
                if (NetCDF::varget($ncid,$var->{$variable}{varid},\@start,
                      \@count, \@dataValues) == -1) 
				{
					die "Can't get data for data variable $variable:$!\n";
                }
				foreach my $data(@dataValues)
				{
                    $var->{$variable}{values}[$record] = $dataValues[$record];
				}
            }
			# We want the height data (varid = 8)
            elsif (($var->{$variable}{ndims} == 1 ) && ($var->{$variable}{varid} == 8)) 
			{
                my @start = ($record);
                my @counts = (1);
                if (NetCDF::varget($ncid,$var->{$variable}{varid},\@start,
                      \@counts, \@dataValues) == -1) 
				{
					die "Can't get data for height variable $variable:$!\n";
                }
                $var->{$variable}{values}[$record] = $dataValues[0]; 
            }
            # Otherwise, warn user we don't know how to read in the variable
            else 
			{
				# Creates a huge file - don't do it!
                #print "CRITICAL ERROR: Don't know how to read in variable: $variable (";
				#for (my $dim = 0;$dim<$var->{$variable}{ndims}; $dim++) {
				#	my $dimname = $var->{$variable}{dimname}[$dim];
				#	print "$var->{$variable}{dimname}[$dim]=$var->{$variable}{$dimname} ";
				#print ") \n";           			
            }
        } # End foreach $variable
    } # End foreach $record
	
    # print Dumper($var);

    # --------------------------------------------------
    # Now we have all the variables for all the records, 
	# so loop through again and print the record
    # --------------------------------------------------
    &parse_data_rec($soundingLen,$var,$sounding);
	# create some shorter test output files
    # &parse_data_rec(10,$var,$sounding);
}


#---------------------------------------------------------------------
# Header data fields of interest.
#---------------------------------------------------------------------
sub getHeaderFields {
	# HARD-CODED variable names from the NetCDF data file

    my @headerNames = qw(year month day hour minute lat lon);

    return(@headerNames);
}
  
#---------------------------------------------------------------------
# Data fields of interest.
#---------------------------------------------------------------------
sub getDataFields {
	# HARD-CODED variable names from the NetCDF data file

    my @dataNames = qw(height pres T RH Td wnddir wndspd u v flag);

    return(@dataNames);
}


#---------------------------------------------------------------------
# Quality flag fields of interest.
#---------------------------------------------------------------------
sub getQualityFields {
	# HARD-CODED variable names from the NetCDF data file

    my @qualityNames = qw(flag);

    return(@qualityNames);
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
    my $file = sprintf("%s/%s",$self->getRawDirectory(),$file_name);

    printf("Processing file: %s\n",$file_name);
   	my $ncid;
   	if (($ncid = NetCDF::open($file,0)) == -1) 
	{
           die "Cannot open file: $file\n";
   	}   

    #-------------------------------------------------
    # Get information about the input netCDF file
    #-------------------------------------------------
	print "Calling readNetCDFheader\n";
    (my $recDimName, my $var, my $recdimsize) = readNetCDFheader($file);

    print "recDimName = $recDimName\n";
	# print "var = $var\n";
	print "recdimsize = $recdimsize\n";

	# print Dumper($var);

    # Processing file: VOCALS2008_soundings_z1_0.nc
    # recDimName = launch
    # var = HASH(0x99f7550)
    # recdimsize = 216 or NUMBER OF SOUNDINGS


    # process all 216 (recdimsize) soundings
    for (my $sounding = 0; $sounding < $recdimsize; $sounding++)
	{
		getData($ncid, $var, $recdimsize,$sounding);
   
    #-----------------------------------------------------------------
    # All soundings come from the Research Vessel Ron Brown. Since the
    # Ron Brown is a mobile station, the lat/lons will be missing
    # in the stationCD.out file and the type will be 'm'.
    #-----------------------------------------------------------------
    my $station = $self->{"stations"}->getStation(getNetworkName(),$self->{"NETWORK"});
    if (!defined($station)) {
        $station = $self->build_default_station(getNetworkName(),$self->{"NETWORK"});
        $self->{"stations"}->addStation($station);
    }
    my $relDate = sprintf("%04d, %02d, %02d",$relYear,$relMonth,$relDay);  
	# print "Release Date:  $relDate\n";
	$station->insertDate($relDate, "YYYY, MM, DD");         

    }


    #-----------------------------------------------------------------
    # Close the input file
    #-----------------------------------------------------------------

  	if (NetCDF::close($ncid) == -1) {
        die "Can't close $file\n";
    }
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
    $station->setStateCode("99");
    $station->setReportingFrequency("no set schedule");
    $station->setNetworkIdNumber(99);
    $station->setPlatformIdNumber(999);
    $station->setMobilityFlag("m");
    return $station;
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






