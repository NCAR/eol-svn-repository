#!/opt/bin/perl -w

#--------------------------------------------------------------------------------------
# CEOP_sfc_refsite_SGP.pl
#
# This s/w is used in converting SGP surface netCDF files into CEOP output.
# It handles IRT, SIRS, and SMOS platforms for multiple (C1-E24) sites.
#
# This Perl script is used in the processing of CEOP GAPP SGP data.  Input files are
# previously prepared using nesob_dump (nesob_dump is a variation of nc_dump specifically
# rewritten for extracting variables from a netCDF file) and consist of lines of data
# matching the parameters needed.
#
# A script named "get_vars.sh" is run in each directory where the raw data is. It
# prepares the input files (naming them with a "*.dat" extension) and has a line in it
# with the variable names to extract. These variables match the "@xxxx_fields" in this s/w.
# If you change which variables to extract from the netCDF file, you will need to change
# the "fields" array, as well.
#
# 21 may 99, ds
# rev 26jul99, ds
#    added bowen, home_15, and home_30 variables to grab from netCDF file
# rev 30jul99, ds
#    added section to create 3 station *.out files
# rev 03aug99, ds
#    added checks for AOI and TOI
# rev 09aug99, ds
#    made year 4 digits long in output file
# rev 19aug99, ds
#    all values are multiplied by -1.0
#    so output matches tilden output
# rev 17may00, ds
#    added stn_name hash to put out long name
# rev 12 Dec 02, ds
#    fixed up for use with CEOP data
# rev 11 Sep 03, ds
#    fixed up the section "divide into arrays by date" so it will work with more than one year
# rev 26 Nov 03, ds
#    now checking QC fields in netCDF files to set our own flags:
#
#		0 - failed no checks - G
#		1 - missing	- M
#		2 - minimum	- D
#		3 - missing/min	- B
#		4 - maximum - D
#		5 - missing/max - B
#		7 - missing/min/max - B
#		8 - delta - D
#		9 - missing/delta - B
#		10 - min/delta - B
#		11 - missing/min/delta - B
#		12 - max/delta - B
#		14 - min/max/delta - B
#		15 - missing/min/max/delta - B
#
# rev 01 Dec 03, ds
#	separated surface and flux processing
# rev 9 May 04, ds
#	added flagging per DQR reports, per Scot
#	more flagging added per Scot, email 28 May 04
# rev 11 Jun 04, ds
#   set "_" data values to Missing
#   fixed bug in net rad calc, where "0" values were set to Missing 
# rev 08 Aug 05, ds
#   added check on site IDs so that those without any data for a day are
#     still printed out with all missing values, as long as they have data
#     for at least one day within the Time of Interest
#--------------------------------------------------------------------------------------
#    IMPORTANT:  If values for home_15 or home_30 are corrupted, the following
#    fields should be considered suspect; inspection of the 5 and 15 minute home
#    signal data is required to determine validity:  e, h.
#--------------------------------------------------------------------------------------

$DEBUG = 0;
$DEBUG1 = 0;				# for even more messages

#-----------------------------
# get our subroutines in
# NEW ones pass flags in
#-----------------------------
unshift (@INC, ".");
require ("./bin/calc_dewpoint_NEW.pl");
require ("./bin/calc_specific_humidity_NEW.pl");
require ("./bin/calc_UV_winds_NEW.pl");

#--------------------------------------------------------------------------------------
# These parameters are in each input file.
#       base_time:      the time offset is figured from this
#       lat:            latitude
#       lon:            longitude
#       alt:            elevation
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
# the parameters we want from each set of files, by platform
#--------------------------------------------------------------------------------------
@irt_fields   = ("time_offset", "sfc_ir_temp");
@sirs_fields  = ("time_offset", "up_long_hemisp", "down_long_hemisp_shaded", "up_short_hemisp", "down_short_hemisp");
@smos_fields  = ("time_offset", "wspd", "qc_wspd", "wdir", "qc_wdir", "temp", "qc_temp", "rh", "qc_rh", "bar_pres", "qc_bar_pres", "precip", "qc_precip");

%params = (
    "irt"   => \@irt_fields,
    "sirs"  => \@sirs_fields,
    "smos"  => \@smos_fields
);

#--------------------------------------------------------------------------------------
# a list of the parameters we want, in order as printed to the output files
#--------------------------------------------------------------------------------------

# (param num)       =        1         2           3        4            5          6            7         8         9         10          11                12                  13                      14                    15          16         17           18         19
@sfc_parameter_list = ("bar_pres",  "temp",    "dew_pt",   "rh",    "spec_hum",  "wspd",      "wdir",   "u_wind", "v_wind", "precip", "snow_depth", "down_short_hemisp", "up_short_hemisp", "down_long_hemisp_shaded", "up_long_hemisp",  "net", "sfc_ir_temp", "par_in", "par_out");
@sfc_obs            = ("stn_pres", "temp_air", "dew_pt", "rel_hum", "spec_hum", "wind_spd", "wind_dir", "U_wind", "V_wind", "precip",    "snow",         "short_in",        "short_out",             "long_in",           "long_out",  "net_rad", "skintemp",   "par_in", "par_out");
$sfc_param_count    = 19;

#--------------------------------------------------------------------------------------
# where the data files are for input (previously created from the netCDF files)
#--------------------------------------------------------------------------------------
%dirs = (
    "irt"   => "IRT/",
    "sirs"  => "SIRS/",
    "smos"  => "SMOS/"
);

%filename_pattern = (
    "irt"   => 'sgpirt10m([CE]\d{1,3}).{4}(\d{4})(\d{2})(\d{2})\.(\d{2})(\d{2})',
    "sirs"  => 'sgpsirs([CE]\d{1,3}).{4}(\d{4})(\d{2})(\d{2})\.(\d{2})(\d{2})',
    "smos"  => 'sgp1smos([CE]\d{1,3}).{4}(\d{4})(\d{2})(\d{2})\.(\d{2})(\d{2})'
);

#--------------------------------------------------------------------------------------
# the stations and their full names
#--------------------------------------------------------------------------------------
%stn_name = (
    "E1"=> "E1_Larned",
    "E2"=> "E2_Hillsboro",
    "E3"=> "E3_Le_Roy",
    "E4"=> "E4_Plevna",
    "E5"=> "E5_Halstead",
    "E6"=> "E6_Towanda",
    "E7"=> "E7_Elk_Falls",
    "E8"=> "E8_Coldwater",
    "E9"=> "E9_Ashton",
    "E10"=>"E10_Tyro",
    "E11"=>"E11_Byron",
    "E12"=>"E12_Pawhuska",
    "E13"=>"E13_Lamont",
    "E14"=>"E14_Lamont",
    "C1"=> "C1_Lamont",
    "C2"=> "C2_Lamont",
    "E15"=>"E15_Ringwood",
    "E16"=>"E16_Vici",
    "E18"=>"E18_Morris",
    "E19"=>"E19_El_Reno",
    "E20"=>"E20_Meeker",
    "E21"=>"E21_Okmulgee",
    "E22"=>"E22_Cordell",
    "E23"=>"E23_Ft_Cobb",
    "E24"=>"E24_Cyril",
    "E25"=>"E25_Seminole",
    "E26"=>"E26_Cement",
    "E27"=>"E27_Earlsboro"
);


#-----------------------------
# project specific variables
#-----------------------------
                           
$project_name = "CEOP";
$CSE_id       = "GAPP";
$site_id      = "SGP";
$platform_id  = "XXXX";             # e.g. "SMOS"           
$stn_id       = "Exx";              # e.g. "E18"

$network            = "ARM_$platform_id";
$project_begin_date = 20031001;     # EOP4
$project_end_date   = 20041231;     

$precip_accum = 0;                  # for accumulation of precip by the half hour
$missing_obs  = -9999;				# missing in data
$MISSING      = -999.99;			# our missing value
                               
#-----------------------------
# files in, put in arrays
#-----------------------------
 
foreach $network (keys (%dirs)) {
    print "Reading filenames in $dirs{$network} directory... \n";
    opendir (FILEDIR, $dirs{$network}) || die "Can't open $dirs{$network}\n";
    @this_dir = sort(readdir(FILEDIR));
    #-----------------------------
    # get rid of "." and ".." 
    #-----------------------------
    shift(@this_dir);
    shift(@this_dir);
    #-----------------------------
    # add path to filenames
    #-----------------------------
    @this_dir = map($dirs{$network}.$_, @this_dir);
    push (@sfc_infiles,  @this_dir);
    closedir(FILEDIR);
}

#-----------------------------
# sort infile lists by date
#-----------------------------
@sfc_infiles = sort file_date_sort (@sfc_infiles);

#-----------------------------
# set default values in our
# output arrays
#-----------------------------
&clear_array("sfc");

#-----------------------------------------------------------------------------
# Divide the infile names into arrays by date, and then put
# references to the arrays into a hash indexed on the date.
#-----------------------------------------------------------------------------

my @months=(0,31,28,31,30,31,30,31,31,30,31,30,31);
my @lmonths=(0,31,29,31,30,31,30,31,31,30,31,30,31);
$start_year  = substr($project_begin_date, 0, 4);
$start_month = substr($project_begin_date, 4, 2);
$end_year    = substr($project_end_date, 0, 4);

print "start month and year = $start_month/$start_year, end year = $end_year\n";
for ($i=$start_year; $i <= $end_year; $i++) {               # these are the years we want 
  my @m=(isLeap($i) ? @lmonths : @months);
  for ($j= ($i==$start_year) ? $start_month : 1; $j <= 12; $j++) {          
    for ($k=1; $k <= $m[$j]; $k++) {                        # these are the days of the months
        $the_date = sprintf("%4.4d%2.2d%2.2d", $i, $j, $k);  # put our date together with the month and day
		last if($the_date > $project_end_date);
        #-----------------------------
        # divide into arrays by date
        #-----------------------------
        my @sfc_date_array = grep(/$the_date/, @sfc_infiles);   # get all files for sfc and this date into an array
        #-----------------------------
        # put refs to arrays into hash
        #-----------------------------
        $sfc{$the_date} = \@sfc_date_array;             # get reference to the date_array and store in sfc hash indexed by date
        print ("the files for sfc $the_date = @{$sfc{$the_date}}\n") if ($DEBUG1);
    }
  }
}
 
#-----------------------------
# files out
#-----------------------------
 
$outfile1 = $CSE_id . "_" . $site_id . "_" . $site_id . "_" . $project_begin_date . "_" . $project_end_date . ".sfc";
# $station_out = $CSE_id . "_".$site_id."_station.out";
# $CD_station_out = $CSE_id . "_".$site_id."_stationCD.out";
# $stn_id_out = $CSE_id . "_".$site_id."_stn_id.out";

#-----------------------------------------------------------------------------
# Open files used in conversion
#-----------------------------------------------------------------------------

open (OUTFILE_SFC, ">./out/final/$outfile1") || die "Can't open $outfile1";
# open (STNSOUT1, ">./out/$station_out") || die "Can't open $station_out";
# open (STNSOUT2, ">./out/$CD_station_out") || die "Can't open $CD_station_out";
# open (STNSOUT3, ">./out/$stn_id_out") || die "Can't open $stn_id_out";

writeHeader("sfc") if ($DEBUG);

#-----------------------------
# set default values in our
# output arrays
#-----------------------------

foreach $obs (@sfc_obs) {
	${$obs} = $MISSING;
} 

#--------------------------------------------------------------------------------------
# print the names of the stations with any data in this TOI
#--------------------------------------------------------------------------------------

@siteID = sort keys(%stn_in_data);
print "Our sites for this TOI are:\n";
foreach $site_ID (sort keys %stn_in_data) {
	print " $site_ID,";
}
print "\n\n";

#-----------------------------------------------------------------------------
# start reading in the surface data, a day at a time
#-----------------------------------------------------------------------------

foreach $date (sort keys (%sfc)) { 									# get each date in sorted order
	print ("date = $date\n") if ($DEBUG);
	$num_files = @{$sfc{$date}};
	print "have $num_files files for $date\n" if ($DEBUG);
	if($num_files == 0) {
        $date_str = $date;
        substr($date_str, 4, 0) = "/";
        substr($date_str, 7, 0) = "/";
        print ("Will write all missing data for station $stn_id on $date_str\n") if ($DEBUG);
	}
	foreach $infile (@{$sfc{$date}}) {                            	# now read in the filenames one at a time
    	next if ($infile !~ /dat$/);                                # only take files ending in ".dat"
 	   	print ("\n************************\nOpening $infile for input\n") if ($DEBUG);
    	open (INFILE, "$infile") || die ("Can't open $infile\n");
    	$infile =~ /(^[A-Z]{3,5})/;                                 # get the platform name from the path
    	$platform = lc($1);                                         #   and convert to lowercase
    	print ("this platform: $platform\n") if ($DEBUG);

    	while ($this_line = <INFILE>) {
        	@line_value = split(" ", $this_line);
	        print "This line is: $this_line" if ($DEBUG1);

        	#--------------------------------------------------------------------------------------
        	# Test to see what variable we have on this line, and save the constants for this file.
        	# Files are read in order, so each time we see "netCDF" it is a new file being read.
        	#--------------------------------------------------------------------------------------
				    
        	if ($line_value[0] eq "netcdf") {
            	$filename = $line_value[1];
	            print ("filename: $filename\n") if($DEBUG);
	    
	            #--------------------------------------------------------------------------------------
	            # Get our station name and date from the filename 
	            # Check the date against project dates and if outside the TOI get another line.
	            #--------------------------------------------------------------------------------------
	            print ("matching $filename_pattern{$platform}\n") if($DEBUG1);
	            $filename =~ /$filename_pattern{$platform}/;
	            ($stn_id, $yr, $mon, $day, $hour, $min) = ($1, $2, $3, $4, $5, $6); 
	            print ("stn = $stn_id, yr = $yr, mon = $mon, day = $day, time = $hour:$min\n") if($DEBUG);
	            $this_date = $yr.$mon.$day;
	            if(($this_date < $project_begin_date) || ($this_date > $project_end_date)) {
	                print "this $filename data for $this_date is out of the TOI\n\n";
	                last;
	            }
				$stnlist{$stn_id} = 1;

	        #--------------------------------------------------------------------------------------
 	        # Get the constants (lat, lon, alt, base time) from this file
	        #--------------------------------------------------------------------------------------
	        } elsif ($line_value[0] eq "lat:") {
	            $lat = $line_value[1];
				${$stn_id}{lat} = $lat;
	        } elsif ($line_value[0] eq "lon:") {
	            $lon = $line_value[1];
				${$stn_id}{lon} = $lon;
	        } elsif ($line_value[0] eq "alt:") {
	            $elev = $line_value[1];
				${$stn_id}{elev} = $elev;
	        } elsif ($line_value[0] eq "base_time:") {
	            $baseTime = $line_value[1];
	        } else {
    
            #--------------------------------------------------------------------------------------
            # All others should be the values for each time.  Get the variable name which will match
            # one of our "fields" values (with a ":" after it), take off the trailing comma in each 
            # slot of the "line_value" array, and copy the array to an array named after the variable. 
            #--------------------------------------------------------------------------------------

	            $index = 0; 
	            foreach (@{$params{$platform}}) {
	                $obsName = ${$params{$platform}}[$index++];
	                if ($line_value[0] eq $obsName.":") {
	 	                print ("this obs = $obsName for the $platform platform, and it is param number $index, while line value 0 = $line_value[0] and obs name = $obsName\n") if ($DEBUG1);
	
	                    #--------------------------------------------------------------------------------------
	                    # Check that each line of values has the same number of observations as the time line.
	                    #    (Subtract 1 for the first value on the line, which is the variable name.)
	                    #--------------------------------------------------------------------------------------
	    
	                    $numObs = ($#line_value - 2) if ($line_value[0] eq "time_offset:");    
	                    $thisNumObs = $#line_value - 2;                 
	                    die "a: Different number of observations in the $line_value[0] line; expecting $numObs, but have $thisNumObs\n" if ($numObs != $thisNumObs);
	    
	                    #------------------------------------------------------------
	                    # Get rid of the commas after the values, change
	                    # any -0.0 values to 0.0, and "NAN" to -999.99,
						# also "_" to -999.99 (added 11 Jun 04, ds),
						# and set to Missing if less than -899.
	                    #------------------------------------------------------------
	    
	                    for ($i=1; $i <= $numObs +1; $i++) {
	                        $line_value[$i] =~ s/,//g;
	                        $line_value[$i] = $MISSING if ($line_value[$i] eq "nan");
                           	$line_value[$i] = $MISSING if ($line_value[$i] eq "_");
	                        $line_value[$i] = $MISSING if ($line_value[$i] =~ /e+/);				# catch very large numbers in data
	                        $line_value[$i] = $MISSING if (($line_value[$i] < -899) || ($line_value[$i] == 99999) || ($line_value[$i] == 6999));
	                		$line_value[$i] = 0.00 if (sprintf("%8.2f", $line_value[$i]) == -0.00); 
	                    }
	   
	                    #------------------------------------------------------------
						# do conversions for pressure and skin temp, and
						# switch the sign of latent and sensible heat fluxes
	                    #------------------------------------------------------------

						if ($obsName eq "bar_pres") {
	                    	for ($i=1; $i <= $numObs; $i++) {
	                        	$line_value[$i] *= 10 if ($line_value[$i] != $MISSING);			# kPa * 10 = hPa
							}
						} elsif ($obsName eq "sfc_ir_temp") {
	                    	for ($i=1; $i <= $numObs; $i++) {
	                        	$line_value[$i] -= 273.15 if ($line_value[$i] != $MISSING);		# Kelvin - 273.15 = Celsius
							}
						} elsif ($obsName eq "e" || $obsName eq "h") { 							# sensible and latent heat fluxes, Scot decided that we should switch the sign 
	                    	for ($i=1; $i <= $numObs; $i++) {
								$line_value[$i] *= -1.0 unless ($line_value[$i] == 0 || $line_value[$i] == $MISSING || $line_value[$i] > 9999);
								print "switched sign of $line_value[$i] for the $obsName obs\n" if($DEBUG1);
							}
						}
							
	                    print "and the line values are: @line_value\n" if ($DEBUG1);
	
	                    #------------------------------------------------------------
	                    # Put into an array named after the the variable
	                    #------------------------------------------------------------
	                    @{$obsName} = @line_value;
	                    print "this array of $obsName obs is @{$obsName}\n" if ($DEBUG1);
	                } 													# <---- end if match obsName and param
	            } 														# <---- end foreach params
        	} 															# <---- end all other params
    	} 																# <---- end while infile line
	
	  	#--------------------------------------------------------------------------------------
		# Check that the date/time in the filename matches the date/time in the data.
		# Add the 1st time_offset to the base_time and convert to GMT to compare.
		# The 0'th value is the variable name, so index of obs starts at 1.
		# Add 1 to the month because gmtime() returns months indexed from 0 to 11.
		# Add 1900 to the year because gmtime() returns years starting at 0 for 1900.
		#--------------------------------------------------------------------------------------
		
		@begin_time = gmtime($baseTime + $time_offset[1]);
		print "base time = $baseTime, time offset = $time_offset[1]\n" if ($DEBUG);
		$begin_month = $begin_time[4] + 1;
		$begin_year = $begin_time[5] + 1900;
	  
		if (($yr != $begin_year) || ($mon != $begin_month) || ($day != $begin_time[3]) || ($hour != $begin_time[2]) || ($min != $begin_time[1])) {
		    print "We got a time problem with $filename\n";
		    print "julian day: $begin_time[7], year: $yr,$begin_year, month: $mon,$begin_month, day: $day,$begin_time[3], hour: $hour,$begin_time[2], min: $min,$begin_time[1]\n";   
		    die "That's all!";
		}
	
		$date_str = "$yr/$mon/$day";
		print ("Will write data for station $stn_id on $date_str\n\n") if ($DEBUG1);
	
		#-----------------------------------------------------------------------
        # Put the values of the obs into a hash of arrays prepared by the
        # clear_array() subroutine, and indexed by the station and time.
		#-----------------------------------------------------------------------

	    $j = 0;               				# Note: params are indexed in the output line array by number
		foreach $param (@sfc_parameter_list) {
	        if (defined(@{$param})) {
				$qc_flags = "qc_".$param;
			    for ($obsNum = 1; $obsNum <= $numObs; $obsNum++) {      		# get every value, one at a time
	        		@this_gmtime = gmtime($baseTime + $time_offset[$obsNum]);
	        		$min = $this_gmtime[1];
	        		$hour = $this_gmtime[2];
					print "this qc_flags = $qc_flags, and the line of values = @{$qc_flags}\n" if($obsNum == 2 && $DEBUG1); 
		        	$sfc_out{$stn_id}[$hour][$min][$j] = @{$param}[$obsNum];
		        	$sfc_flag{$stn_id}[$hour][$min][$j] = @{$qc_flags}[$obsNum] if(defined(@{$qc_flags}));
		        	print "***THis sfc flag for $param = $sfc_flag{$stn_id}[$hour][$min][$j]\n" if(!defined(@{$qc_flags}) && $DEBUG1);
	    		} # <----- end for obsNum   
			}
		    $j++;
		} # <---- end foreach param

	    #------------------------------------------------------------
		# we have the values, so clear out the params arrays used
	    #------------------------------------------------------------
		foreach $param ("time_offset", @sfc_parameter_list) {
			undef(@{$param});
			$qc_flags = "qc_".$param;
			undef(@{$qc_flags});
		}
	    if ($obsNum-1 != $numObs) {
	        printf ("*** Had %d number of observations in $filename, but was expecting %d!\n", $obsNum - 1, $numObs);
			die "Wrong number, let's stop!";
	    }
	}   # <----- end foreach infile

	&writeDate("sfc", $date_str);
	undef %sfc_out;
	undef %sfc_flag;
	&clear_array("sfc");
	foreach $id (keys %stnlist) {
		undef($stnlist{$id});
	}
}   # <----- end foreach date, sfc

close (OUTFILE_SFC);

#----------------------------------------------------------------------------------------
# Set up the array (@sfc_out) which will feed the values for all the
# parameters for one day into the output lines. This equates to all the obs in 
# all the files for one day. The array is indexed on each station ID, each hour and 
# minute in the day, and on the parameter's position in the output line (1-19, or 1-4).
#----------------------------------------------------------------------------------------

sub clear_array {
    my($array_name) = @_;
    $flags_name = $array_name . "_flag";
    $array_name = $array_name . "_out";
    if ($array_name eq "sfc_out") {
    	$end_num = $sfc_param_count + 1;
	} else {
        die "don't know this array to clear: $array_name\n";
	}
    foreach $stn (keys (%stn_name)) {
    	for ($hour_num=0; $hour_num <= 24; $hour_num++) {
			for ($min_num=0; $min_num <= 60; $min_num++) {
	      		for ($param_num=0; $param_num <= $end_num; $param_num++) {
                	${$array_name}{$stn}[$hour_num][$min_num][$param_num] = $MISSING;
                    ${$flags_name}{$stn}[$hour_num][$min_num][$param_num] = -99;
                }
            }
        }
	}
}

#--------------------------------------------------------------------------------------
# Use this sort algorithm as the place to track stations with any data in this TOI.
#--------------------------------------------------------------------------------------

sub file_date_sort {

    $a =~ /([CE]\d{1,3}).{4}(\d{8})\.(\d{6})\.dat$/;
    $a_id = $1;
    $a_datetime = $2.$3;
    $b =~ /([CE]\d{1,3}).{4}(\d{8})\.(\d{6})\.dat$/;
    $b_id = $1;
    $b_datetime = $2.$3;
	$stn_in_data{$a_id} = 1;
	$stn_in_data{$b_id} = 1;

    $retval = ($a_id cmp $b_id);
    if ($retval != 0) {
        return $retval;
    } else {
        $retval = $a_datetime <=> $b_datetime;
        return $retval;
    }
}


sub isLeap {
    my($year) = @_;
    return 1 if $year % 400 == 0;
    return 0 if $year % 100 == 0;
    return 1 if $year % 4 == 0;
    return 0;
}


#------------------------------------------------------------
# print out the header lines 
#------------------------------------------------------------       

sub writeHeader {
    my($out_type) = @_;
          
    if ($out_type eq "sfc") {
		print OUTFILE_SFC "   date    time     date    time    CSE ID      site ID        station ID        lat        lon      elev  ";
		foreach $param ("stn pres", " f", "temp_air", " f", " dew pt ", " f", " rel hum", " f", "spec hum", " f", " wnd spd", " f", " wnd dir", " f", " U wind ", " f", " V wind ", " f", " precip ", " f", "  snow  ", " f", " short in", " f", " shortout", " f", " long in ", " f", " long out", " f", "  net rad", " f", " skintemp", " f", " par_in  ", " f", " par_out ", " f") {
			print OUTFILE_SFC "$param"; 
		}
		print OUTFILE_SFC "\n";
		print OUTFILE_SFC "---------- ----- ---------- ----- ---------- --------------- --------------- ---------- ----------- -------"; 
		for ($i=0; $i<11; $i++) {
			print OUTFILE_SFC " ------- -"; 
		}
		for ($i=0; $i<8; $i++) {
			print OUTFILE_SFC " -------- -"; 
		}
		print OUTFILE_SFC "\n";
    } else {
        die "don't know this output type: $out_type!";
    }
}


#--------------------------------------------------------------------------------   
##  writeDate - write all the data values for 
#               one day, for all stations.
#
#   input:		type of data (sfc, flx, stm, twr, etc)
#				date (yyyy/mm/dd)
#--------------------------------------------------------------------------------   
sub writeDate
{
	local ($met_type, $the_date) = @_;
	my $have_precip = 0;
	print "in writeDate, writing $met_type data for $the_date\n\n" if($DEBUG);

 	#------------------------------------------------------------       
    # Loop through the stns, and the hours and minutes,
	# get the precip accumulated until then, calculate it,
    # and print the values at each half hour for each station.
    # Note: for the new reference version, we are putting out
    #       30-min records. Therefore, if real UTC time < 15 OR
    #       real UTC time > 45, then nominal UTC time = 00, else
    #       nominal UTC time = 30. This has not been implemented,
	#		since we are dealing with 1 minute data.
	# Also: now printing all times for all stations in TOI.
    #------------------------------------------------------------       
    foreach $stn (@siteID) { 
		next if($stn !~ /[A-Z]\d+/);
#------------------------------------------------------------       
# Commented out lines limited output to only those stations
# with data in a day. Changed 8/8/05 per Scot's request to
# print out data for all stations, even when missing. 
#------------------------------------------------------------       
#   foreach $stn (keys (%stn_name)) { 
#     if (defined ($stnlist{$stn}) && $stnlist{$stn} != 0) {    
#------------------------------------------------------------       
        for ($hour=0; $hour < 24; $hour++) {
            $real_hour = $hour;
        	for ($min=0; $min < 60; $min++) {
				if($met_type eq "sfc")  {
					$this_precip = $sfc_out{$stn}[$hour][$min][9];       	# precip is number 9 in array
	           		if ($this_precip > 0) {    							
	           			$precip_accum += $this_precip;    					# accum ALL precip values before the half hour 
						$have_precip = 1;
					}
 	           		print ("this precip for $stn at $hour:$min = $this_precip, and precip accum = $precip_accum\n") if ($DEBUG1);     
				}

				if ($min == 0 || $min == 30) {
				  if ($met_type eq "sfc") {
	           		$precip_accum = 0.0 if ($precip_accum < 0.005);    		# to avoid negative values 
 	           		print ("this precip for $stn at $hour:$min = $this_precip, and precip accum (adjusted for 30 min period) = $precip_accum\n") if ($DEBUG1);     
					$sfc_out{$stn}[$hour][$min][9] = $precip_accum unless($have_precip == 0);
					$real_min = $min;
           			&writeSFCline($stn, $the_date, $hour, $min); 
		    		$precip_accum = 0;
					$have_precip  = 0;
				  } else {
           			&writeFLXline($stn, $the_date, $hour, $min); 
				  }
				} # <------- end min on half hour
			}	# <------- end minutes of hour 
		}
#	  }   	    # <------- if defined stnlist{stn}
	  $precip_accum = 0;													# just in case any slips past our half hour
	  $have_precip = 0;
	}
}


#--------------------------------------------------------------------------------   
##  writeSFCline - write the surface data values to the SFC output file. 
# 
#  input:   
#       $stn        the station with the readings
#		$date		yyyy/mm/dd
#       $hour 
#       $min 
#
#       global values:
#					$real_min, and all the data obs
#
#  output:  a single line of surface data for that time
#--------------------------------------------------------------------------------   

sub writeSFCline
{ 
    local ($id, $date, $hour, $min) = @_;
    $long_name = $stn_name{$id};
	my $lat  = ${$id}{lat};
	my $lon  = ${$id}{lon};
	my $elev = ${$id}{elev};

    #------------------------------------------------------------
	# Put the hash values into scalar values named after the obs
    #------------------------------------------------------------
	$print = 0;
    $j = 0;
    foreach $obs (@sfc_obs) {
		${$obs} = $sfc_out{$id}[$hour][$min][$j];
		$the_flag = $obs."_flag";
		${$the_flag} = $sfc_flag{$id}[$hour][$min][$j];
        print ("for $stn_name{$id}, at $hour:$min $obs: $sfc_out{$id}[$hour][$min][$j], which is same as ${$obs}, and $the_flag is ${$the_flag}, which is the same as $sfc_flag{$id}[$hour][$min][$j]\n") if ($DEBUG);
		$print = 1 if (${$obs} != $MISSING);
		$j++;
	} 

    #------------------------------------------------------------
	# decision made to include empty records, 12 May 04, per Scot
    #------------------------------------------------------------
	# return ("empty line") if ($print == 0);							# don't print lines with all obs missing
    #------------------------------------------------------------

    #------------------------------------------------------------
    # Print out the first part of our line for the output to SFC
    #------------------------------------------------------------       
   
    printf OUTFILE_SFC ("%10s %02d:%02d %10s %02d:%02d %-10s %-15s %-15s %10.5f %11.5f %7.2f", $date, $hour, $min, $date, $real_hour, $real_min, $CSE_id, $site_id, $long_name, $lat, $lon, $elev);
    #   format -   yyyy/mm/dd hh:mm yyyy/mm/dd hh:mm CSE_id site_id station_id dec_lat dec_lon elevation 
    
    #------------------------------------------------------------
    # Print out the stn_pressure (and the flag)
    #------------------------------------------------------------ 
                  
 	$stn_pres_flag = &get_flag(\$stn_pres, $stn_pres_flag, "bar_pres", $id, $date, $hour, $min);
    printf OUTFILE_SFC (" %7.2f", $stn_pres);
	printf OUTFILE_SFC (" %s", $stn_pres_flag);
        
    #------------------------------------------------------------
    # Print out the temperature
    #------------------------------------------------------------ 
        
 	$temp_air_flag = &get_flag(\$temp_air, $temp_air_flag, "temp", $id, $date, $hour, $min);
    printf OUTFILE_SFC (" %7.2f", $temp_air);
	printf OUTFILE_SFC (" %s", $temp_air_flag);
        
    #------------------------------------------------------------
    # Calculate and print out the dew point temperature 
	#  (using rel hum flag)
    #------------------------------------------------------------

 	$rel_hum_flag = &get_flag(\$rel_hum, $rel_hum_flag, "rh", $id, $date, $hour, $min);
    &calc_dewpoint($rel_hum, $rel_hum_flag, $temp_air, $temp_air_flag);  
    printf OUTFILE_SFC (" %7.2f", $dew_point);
	printf OUTFILE_SFC (" %s", $dew_point_flag);

    #------------------------------------------------------------
    # Print out the relative humidity  value
    #------------------------------------------------------------

    printf OUTFILE_SFC (" %7.2f", $rel_hum);
	printf OUTFILE_SFC (" %s", $rel_hum_flag);

    #------------------------------------------------------------
    # Calculate the specific humidity, convert to 
	# g/kg from kg/kg, and print out the value
    #------------------------------------------------------------

    &calc_specific_humidity($dew_point, $dew_point_flag, $stn_pres, $stn_pres_flag); 
	$specific_humidity *= 1000 unless ($specific_humidity == $MISSING);
    printf OUTFILE_SFC (" %7.2f", $specific_humidity);
	printf OUTFILE_SFC (" %s", $specific_humidity_flag);

    #------------------------------------------------------------
    # Print the wind speed value
    #------------------------------------------------------------

 	$wind_spd_flag = &get_flag(\$wind_spd, $wind_spd_flag, "wspd", $id, $date, $hour, $min);
    printf OUTFILE_SFC (" %7.2f", $wind_spd);
	printf OUTFILE_SFC (" %s", $wind_spd_flag);
        
    #------------------------------------------------------------
    # Print the wind direction value
    #------------------------------------------------------------

 	$wind_dir_flag = &get_flag(\$wind_dir, $wind_dir_flag, "wdir", $id, $date, $hour, $min);
    printf OUTFILE_SFC (" %7.2f", $wind_dir);
	printf OUTFILE_SFC (" %s", $wind_dir_flag);
    
    #------------------------------------------------------------
    # Calculate and print out the U wind component
    #------------------------------------------------------------
                
    &calc_UV_winds($wind_spd, $wind_spd_flag, $wind_dir, $wind_dir_flag);
    printf OUTFILE_SFC (" %7.2f", $U_wind);
	printf OUTFILE_SFC (" %s", $U_wind_flag);

    #------------------------------------------------------------
    # Print out the V wind component
    #------------------------------------------------------------
                
    printf OUTFILE_SFC (" %7.2f", $V_wind);
	printf OUTFILE_SFC (" %s", $V_wind_flag);

    #------------------------------------------------------------
    # Print out the precipitation value
    #------------------------------------------------------------

 	$precip_flag = &get_flag(\$precip, $precip_flag, "precip", $id, $date, $hour, $min);
    printf OUTFILE_SFC (" %7.2f", $precip);
	printf OUTFILE_SFC (" %s", $precip_flag);

    #------------------------------------------------------------
    # Print out the Snow depth
    #------------------------------------------------------------

 	$snow_flag = &get_flag(\$snow, $snow_flag, "snow_depth", $id, $date, $hour, $min);
    printf OUTFILE_SFC (" %7.2f", $snow);
	printf OUTFILE_SFC (" %s", $snow_flag);

    #------------------------------------------------------------
    # Print out the incoming shortwave radiation
    #------------------------------------------------------------ 
          
 	$short_in_flag = &get_flag(\$short_in, $short_in_flag, "down_short_hemisp", $id, $date, $hour, $min);
    printf OUTFILE_SFC (" %8.2f", $short_in);
	printf OUTFILE_SFC (" %s", $short_in_flag);

    #------------------------------------------------------------
    # Print out the outgoing shortwave radiation
    #------------------------------------------------------------ 
          
 	$short_out_flag = &get_flag(\$short_out, $short_out_flag, "up_short_hemisp", $id, $date, $hour, $min);
    printf OUTFILE_SFC (" %8.2f", $short_out);
	printf OUTFILE_SFC (" %s", $short_out_flag);

    #------------------------------------------------------------
    # Print out the incoming longwave radiation
    #------------------------------------------------------------ 
          
 	$long_in_flag = &get_flag(\$long_in, $long_in_flag, "down_long_hemisp_shaded", $id, $date, $hour, $min);
    printf OUTFILE_SFC (" %8.2f", $long_in);
	printf OUTFILE_SFC (" %s", $long_in_flag);

    #------------------------------------------------------------
    # Print out the outgoing longwave radiation
    #------------------------------------------------------------ 
          
 	$long_out_flag = &get_flag(\$long_out, $long_out_flag, "up_long_hemisp", $id, $date, $hour, $min);
    printf OUTFILE_SFC (" %8.2f", $long_out);
	printf OUTFILE_SFC (" %s", $long_out_flag);

    #------------------------------------------------------------
    # Calculate and print out the net radiation
    #------------------------------------------------------------ 

	if ($short_in != $MISSING && $long_in != $MISSING && $short_out != $MISSING  && $long_out != $MISSING) {
    	$net_rad = $short_in + $long_in - $short_out - $long_out;   		# down_short + down_long - up_short - up_long
    	$net_rad = 0 if( sprintf("%8.2f", $net_rad) == -0.00);
	} else {
		$net_rad = $MISSING;
	}
		#---------------------------------------
		# carry through the flags
		#---------------------------------------
	$flag_precedence = {"M"=>11, "N"=>10, "C"=>9, "I"=>8, "X"=>7, "B"=>6, "E"=>5, "D"=>4, "U"=>3, "G"=>2, "T"=>1};

	$flag_override = ${$flag_precedence}{$short_in_flag} > ${$flag_precedence}{$long_in_flag} ? $short_in_flag : $long_in_flag;
	$flag_override = ${$flag_precedence}{$short_out_flag} > ${$flag_precedence}{$flag_override} ? $short_out_flag : $flag_override;
	$flag_override = ${$flag_precedence}{$long_out_flag} > ${$flag_precedence}{$flag_override} ? $long_out_flag : $flag_override;

    $net_rad_flag = $flag_override;

    printf OUTFILE_SFC (" %8.2f", $net_rad);
	printf OUTFILE_SFC (" %s", $net_rad_flag);

    #------------------------------------------------------------
    # Print out skin temperature
    #------------------------------------------------------------ 
          
 	$skintemp_flag = &get_flag(\$skintemp, $skintemp_flag, "sfc_ir_temp", $id, $date, $hour, $min);
    printf OUTFILE_SFC (" %8.2f", $skintemp);
	printf OUTFILE_SFC (" %s", $skintemp_flag);

    #------------------------------------------------------------
    # Print out the incoming PAR
    #------------------------------------------------------------ 
          
 	$par_in_flag = &get_flag(\$par_in, $par_in_flag, "par_in", $id, $date, $hour, $min);
    printf OUTFILE_SFC (" %8.2f", $par_in);
	printf OUTFILE_SFC (" %s", $par_in_flag);

    #------------------------------------------------------------
    # Print out the outgoing PAR
    #------------------------------------------------------------ 
          
 	$par_out_flag = &get_flag(\$par_out, $par_out_flag, "par_out", $id, $date, $hour, $min);
    printf OUTFILE_SFC (" %8.2f", $par_out);
	printf OUTFILE_SFC (" %s", $par_out_flag);

    #------------------------------------------------------------
	# finish line with a line feed
    #------------------------------------------------------------
    print OUTFILE_SFC ("\n");
}


#------------------------------------------------------------
# mapping of ARM flags to the JOSS QCF flags:
#
#		0 - failed no checks - G
#		1 - missing	- M
#		2 - minimum	- D
#		3 - missing/min	- B
#		4 - maximum - D
#		5 - missing/max - B
#		7 - missing/min/max - B
#		8 - delta - D
#		9 - missing/delta - B
#		10 - min/delta - B
#		11 - missing/min/delta - B
#		12 - max/delta - B
#		14 - min/max/delta - B
#		15 - missing/min/max/delta - B
#------------------------------------------------------------
#
# Many networks have a Data Quality Report (DQR) for a given
# time period. Scot has taken the ones for networks in our TOI
# and grouped them into files which are read by 2 utility 
# programs--"convert_datetime.pl", and "make_flag_code.pl".
# The final product for the DQR for each network--named 
# "dqr_<network>_code.frag"--is in the proper form to be 
# added to this section of the conversion code in place of 
# the lines under "following section fixes flags according 
# to DQRs".
#
#------------------------------------------------------------

sub get_flag
{
    my $obs_ref = shift;
	my $flag_val = shift;
	my $var = shift;
	my $id = shift;
	my $date = shift;			# like 2003/04/16
	my $hour = shift;
	my $min = shift;

    local $obs_value = $$obs_ref;
	my $new_flag = "U";

	my $our_year	= substr($date, 0, 4);
	my $our_month 	= substr($date, 5, 2);
	my $our_day 	= substr($date, 8, 2);
	my $our_time 	= sprintf("%02d%02d", $hour, $min) * 0.0001;
	my $datetime = ($our_year.$our_month.$our_day) + $our_time;			# so 1/1/03 1230 = 20030101.123

	print "   in get_flag, obs_ref = $obs_ref, flag_val = $flag_val, obs_value = $obs_value, id = $id, date = $date, this time = $hour:$min, our time = $our_time, date time = $datetime\n" if($DEBUG1);
    if(($obs_value == -999.99) || ($obs_value == $missing_obs)) {
        $$obs_ref = $MISSING;
        return ("M");  
    } elsif (($obs_value < -999.99) || ($obs_value > 9999.99)) {
        $$obs_ref = $MISSING;
        return ("C"); 
	} elsif ($flag_val == 0) {
		$new_flag = "G";
	} elsif ($flag_val == 1) {
        $$obs_ref = $MISSING;
		return ("M");
	} elsif (($flag_val == 2) || ($flag_val == 4) || ($flag_val == 8)) {
		$new_flag = "D";
	} elsif ($flag_val == 3 || ($flag_val == 5) || ($flag_val == 7)) {
		$new_flag = "B";
    } elsif (($flag_val >= 9) && ($flag_val <= 15)) {
		$new_flag = "B";
	} elsif ($flag_val == -99) {
        $new_flag = "U";
    } elsif (($flag_val < 0) || ($flag_val == 6) || ($flag_val == 13) || ($flag_val > 15)) {
		print "Very BAD flag value of $flag_val for $date_str, $hour:$min and $site_id ID!!!\n"; 
        $new_flag = "U";
    } else {
		die "get flag problem! should not ever get here!\n";
	}

    #---------------------------------------------------------------------------
    # following section fixes flags according to DQRs
    #---------------------------------------------------------------------------
    # C1
    #---------------------------------------------------------------------------
    if ($id eq "C1") {
        if ($var eq "sfc_ir_temp") {
           if (($datetime >= 20040630.1330 && $datetime <= 20040630.1430) ||
			   ($datetime >= 20040723.2130 && $datetime <= 20040726.1400) ||
			   ($datetime == 20040728.1500) ||
			   ($datetime == 20041102.2130)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "down_short_hemisp") {
           if ($datetime >= 20031030.1630 && $datetime <= 20031030.2300) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E1
    #---------------------------------------------------------------------------
    } elsif ($id eq "E1") {
        if ( $var eq "up_long_hemisp" || $var eq "down_long_hemisp_shaded" || $var eq "up_short_hemisp" || $var eq "down_short_hemisp") {
           if ($datetime >= 20031014.2023 && $datetime <= 20031021.1930) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "down_long_hemisp_shaded") {
           if (($datetime >= 20031031.0200 && $datetime <= 20031031.0300) ||
               ($datetime >= 20031111.0438 && $datetime <= 20031118.0220)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "down_short_hemisp") {
           if (($datetime >= 20040202.1200 && $datetime <= 20040202.2000) ||
               ($datetime >= 20040210.2050 && $datetime <= 20040420.2015)) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "up_short_hemisp") {
           if ($datetime >= 20040210.2050 && $datetime <= 20040420.2015) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ( $var eq "wspd" || $var eq "wdir" || $var eq "temp" || $var eq "rh" || $var eq "bar_pres" ) {
           if (($datetime >= 20031021.1745 && $datetime <= 20031021.1945) ||
               ($datetime >= 20040420.1955 && $datetime <= 20040420.2020) ||
               ($datetime >= 20041019.1920 && $datetime <= 20041019.1950)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E2
    #---------------------------------------------------------------------------
    } elsif ($id eq "E2") {
        if ($var eq "down_long_hemisp_shaded") {
           if ($datetime == 20031113.2130) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "down_short_hemisp") {
           if ($datetime >= 20031113.1830 && $datetime <= 20031113.2000) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
           if (($datetime >= 20040202.1200 && $datetime <= 20040202.2000) ||
               ($datetime >= 20040212.1612 && $datetime <= 20040325.1600)) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "up_short_hemisp") {
           if ($datetime >= 20040212.1612 && $datetime <= 20040325.1600) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E3
    #---------------------------------------------------------------------------
    } elsif ($id eq "E3") {
        if ($var eq "down_short_hemisp") {
           if ($datetime >= 20040202.1200 && $datetime <= 20040202.2000) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ( $var eq "wspd" || $var eq "wdir" || $var eq "temp" || $var eq "rh" || $var eq "bar_pres" ) {
           if (($datetime >= 20031105.1733 && $datetime <= 20031105.1815) ||
               ($datetime >= 20040407.1645 && $datetime <= 20040407.1730) ||
               ($datetime >= 20041006.1534 && $datetime <= 20041006.1620)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E4
    #---------------------------------------------------------------------------
    } elsif ($id eq "E4") {
        if ( $var eq "wspd" || $var eq "wdir" || $var eq "temp" || $var eq "rh" || $var eq "bar_pres" ) {
           if (($datetime >= 20031022.1455 && $datetime <= 20031022.1655) ||
               ($datetime >= 20040421.1529 && $datetime <= 20040421.1550) ||
               ($datetime >= 20041020.1515 && $datetime <= 20041020.1535)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "rh") {
           if (($datetime >= 20030922.0447 && $datetime <= 20031027.1655) ||
               ($datetime >= 20031002.1136 && $datetime <= 20031022.1648)) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "rh") {
           if (($datetime >= 20040117.1545 && $datetime <= 20040119.1730) ||
               ($datetime >= 20040422.1630 && $datetime <= 20040506.1531)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "dew_pt" || $var eq "spec_hum") {
           if ($datetime == 20040422.1630) { 
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "up_long_hemisp") {
           if (($datetime == 20031022.1630) ||
               ($datetime == 20041020.1530)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E5
    #---------------------------------------------------------------------------
    } elsif ($id eq "E5") {
        if ($var eq "down_short_hemisp") {
           if (($datetime >= 20040202.1200 && $datetime <= 20040202.2000) ||
               ($datetime >= 20040411.1200 && $datetime <= 20040505.1645)) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ( $var eq "wspd" || $var eq "wdir" || $var eq "temp" || $var eq "rh" || $var eq "bar_pres" ) {
           if (($datetime >= 20031021.1915 && $datetime <= 20031021.1942) ||
               ($datetime >= 20040421.1830 && $datetime <= 20040421.1900) ||
               ($datetime >= 20041020.1906 && $datetime <= 20041020.1918)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E6
    #---------------------------------------------------------------------------
    } elsif ($id eq "E6") {
        if ( $var eq "wspd" || $var eq "wdir" || $var eq "temp" || $var eq "rh" || $var eq "bar_pres" ) {
           if (($datetime >= 20031106.1525 && $datetime <= 20031106.1600) ||
               ($datetime >= 20040408.1511 && $datetime <= 20040408.1600) ||
               ($datetime >= 20041007.1407 && $datetime <= 20041007.1500)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "bar_pres" || $var eq "spec_hum") {
           if ($datetime >= 20040826.1630 && $datetime <= 20040909.1500) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E7
    #---------------------------------------------------------------------------
    } elsif ($id eq "E7") {
        if ($var eq "down_short_hemisp") {
           if (($datetime >= 20031121.0317 && $datetime <= 20031203.0000) ||
               ($datetime >= 20040330.1430 && $datetime <= 20040406.0210) ||
               ($datetime >= 20040210.2050 && $datetime <= 20040420.2015)) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "down_long_hemisp_shaded") {
           if ($datetime >= 20040420.1825 && $datetime <= 20040629.1915) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "up_short_hemisp") {
           if ($datetime >= 20040210.2050 && $datetime <= 20040420.2015) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ( $var eq "wspd" || $var eq "wdir" || $var eq "temp" || $var eq "rh" || $var eq "bar_pres" ) {
           if (($datetime >= 20031104.2144 && $datetime <= 20031104.2230) ||
               ($datetime >= 20040406.1958 && $datetime <= 20040406.2040) ||
               ($datetime >= 20041005.1939 && $datetime <= 20041005.2020)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E8
    #---------------------------------------------------------------------------
    } elsif ($id eq "E8") {
        if ($var eq "down_short_hemisp") {
           if (($datetime >= 20040202.1200 && $datetime <= 20040202.2000) ||
               ($datetime >= 20040315.0150 && $datetime <= 20040331.1200) ||
               ($datetime >= 20040210.1825 && $datetime <= 20040420.1825)) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "up_short_hemisp") {
           if ($datetime >= 20040210.1825 && $datetime <= 20040420.1825) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ( $var eq "wspd" || $var eq "wdir" || $var eq "temp" || $var eq "rh" || $var eq "bar_pres" ) {
           if (($datetime >= 20031021.1700 && $datetime <= 20031021.1735) ||
               ($datetime >= 20040420.1740 && $datetime <= 20040420.1820) ||
               ($datetime >= 20041019.1730 && $datetime <= 20041019.1800)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ( $var eq "rh" || $var eq "dew_pt" || $var eq "spec_hum" ) {
           if (($datetime == 20041214.0730) ||
               ($datetime >= 20041214.0930 && $datetime <= 20041214.1330) ||
               ($datetime >= 20041223.0000 && $datetime <= 20041223.1530) ||
               ($datetime >= 20041223.2130 && $datetime <= 20041224.1600) ||
               ($datetime >= 20041225.0700 && $datetime <= 20041225.1100)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E9
    #---------------------------------------------------------------------------
    } elsif ($id eq "E9") {
        if ($var eq "down_short_hemisp") {
           if (($datetime >= 20031014.0000 && $datetime <= 20031021.1600) ||
               ($datetime >= 20040220.1600 && $datetime <= 20040223.0500) ||
               ($datetime >= 20041224.0000 && $datetime <= 20041231.2330)) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ( $var eq "wspd" || $var eq "wdir" || $var eq "temp" || $var eq "rh" || $var eq "bar_pres" ) {
           if (($datetime >= 20031104.1654 && $datetime <= 20031104.1735) ||
               ($datetime >= 20040406.1553 && $datetime <= 20040406.1640) ||
               ($datetime >= 20041005.1616 && $datetime <= 20041005.1650)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E10
    #---------------------------------------------------------------------------
    } elsif ($id eq "E10") {
        if ($var eq "down_short_hemisp") {
           if ($datetime >= 20040210.1630 && $datetime <= 20040421.1425) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "up_short_hemisp") {
           if ($datetime >= 20040210.1630 && $datetime <= 20040421.1425) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
           if ($datetime == 20040713.2030) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E11
    #---------------------------------------------------------------------------
    } elsif ($id eq "E11") {
        if ($var eq "down_short_hemisp") {
           if (($datetime >= 20040220.1500 && $datetime <= 20040301.0130) ||
               ($datetime >= 20040217.1741 && $datetime <= 20040505.0000)) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "up_short_hemisp") {
           if ($datetime >= 20040217.1741 && $datetime <= 20040505.0000) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "up_long_hemisp") {
           if ($datetime >= 20040504.0000 && $datetime <= 20040622.1855) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ( $var eq "wspd" || $var eq "wdir" || $var eq "temp" || $var eq "rh" || $var eq "bar_pres" ) {
           if (($datetime >= 20031014.1930 && $datetime <= 20031014.1941) ||
               ($datetime >= 20040413.2011 && $datetime <= 20040413.2035) ||
               ($datetime >= 20041026.1840 && $datetime <= 20041026.1900)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "rh") {
           if ($datetime >= 20040414.0055 && $datetime <= 20040713.1525) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E12
    #---------------------------------------------------------------------------
    } elsif ($id eq "E12") {
        if ( $var eq "up_long_hemisp" || $var eq "down_long_hemisp_shaded" || $var eq "up_short_hemisp" || $var eq "down_short_hemisp") {
           if ($datetime >= 20040219.2212 && $datetime <= 20040310.2247) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E13
    #---------------------------------------------------------------------------
    } elsif ($id eq "E13") {
        if ( $var eq "wspd" || $var eq "wdir" || $var eq "temp" || $var eq "rh" || $var eq "bar_pres" ) {
           if (($datetime >= 20031016.1827 && $datetime <= 20031016.1855) ||
               ($datetime >= 20040415.1958 && $datetime <= 20040415.2020) ||
               ($datetime >= 20041028.1835 && $datetime <= 20041028.1855)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ( $var eq "temp" || $var eq "dew_pt" || $var eq "spec_hum" ) {
           if ($datetime == 20040727.0000) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E15
    #---------------------------------------------------------------------------
    } elsif ($id eq "E15") {
        if ($var eq "down_short_hemisp") {
           if ($datetime >= 20040220.1900 && $datetime <= 20040229.1700) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ( $var eq "wspd" || $var eq "wdir" || $var eq "temp" || $var eq "rh" || $var eq "bar_pres" ) {
           if (($datetime >= 20031014.1700 && $datetime <= 20031014.1730) ||
               ($datetime >= 20040413.1615 && $datetime <= 20040413.1700) ||
               ($datetime >= 20041026.1640 && $datetime <= 20041026.1750)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "rh") {
           if (($datetime >= 20031007.1425 && $datetime <= 20031007.1428) ||
               ($datetime >= 20031009.0105 && $datetime <= 20031009.0159)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E16
    #---------------------------------------------------------------------------
    } elsif ($id eq "E16") {
        if ($var eq "down_short_hemisp") {
           if ($datetime >= 20040206.2200 && $datetime <= 20040218.1650) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E20
    #---------------------------------------------------------------------------
    } elsif ($id eq "E20") {
        if ( $var eq "wspd" || $var eq "wdir" || $var eq "temp" || $var eq "rh" || $var eq "bar_pres" ) {
           if (($datetime >= 20031029.2149 && $datetime <= 20031029.2230) ||
               ($datetime >= 20040428.2027 && $datetime <= 20040428.2105) ||
               ($datetime >= 20040929.1607 && $datetime <= 20040929.1655)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "up_short_hemisp") {
           if ($datetime == 20031029.2200) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "wspd") {
           if ($datetime >= 20040930.1900 && $datetime <= 20041013.1737) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E21
    #---------------------------------------------------------------------------
    } elsif ($id eq "E21") {
        if ($var eq "down_short_hemisp") {
           if (($datetime >= 20031018.1600 && $datetime <= 20031029.1620) ||
               ($datetime >= 20031019.0000 && $datetime <= 20040226.2359) ||
               ($datetime >= 20040528.1500 && $datetime <= 20040609.1342)) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ( $var eq "wspd" || $var eq "wdir" || $var eq "temp" || $var eq "rh" || $var eq "bar_pres" ) {
           if (($datetime >= 20031029.1519 && $datetime <= 20031029.1615) ||
               ($datetime >= 20040428.1430 && $datetime <= 20040428.1530) ||
               ($datetime >= 20041108.2200 && $datetime <= 20041108.2220)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "bar_pres") {
           if ($datetime >= 20040512.1532 && $datetime <= 20040628.1520) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E24
    #---------------------------------------------------------------------------
    } elsif ($id eq "E24") {
        if ( $var eq "wspd" || $var eq "wdir" || $var eq "temp" || $var eq "rh" || $var eq "bar_pres" ) {
           if (($datetime >= 20031030.1641 && $datetime <= 20031030.1730) ||
               ($datetime >= 20040429.1631 && $datetime <= 20040429.1715) ||
               ($datetime >= 20040930.1634 && $datetime <= 20040930.1715)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "precip") {
           if ($datetime >= 20040229.0000 && $datetime <= 20040318.2359) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "down_long_hemisp_shaded") {
           if ($datetime == 20040921.1900) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E27
    #---------------------------------------------------------------------------
    } elsif ($id eq "E27") {
        if ( $var eq "wspd" || $var eq "wdir" || $var eq "temp" || $var eq "rh" || $var eq "bar_pres" ) {
           if (($datetime >= 20031029.1808 && $datetime <= 20031029.1855) ||
               ($datetime >= 20040428.1712 && $datetime <= 20040428.1755) ||
               ($datetime >= 20040929.2012 && $datetime <= 20040929.2045)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "up_short_hemisp") {
           if ($datetime == 20040929.2030) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    }

    return $new_flag;
}

