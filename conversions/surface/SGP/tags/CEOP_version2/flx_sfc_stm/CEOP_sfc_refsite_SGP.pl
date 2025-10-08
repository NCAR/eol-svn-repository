#!/usr/bin/perl -w

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
# rev 2005, ds
#   Scot said switch signs for sensible and latent heat fluxes (see line 400) 
# rev 30 Apr 10, ds
#   added MET network for 2009 and beyond
#   since SMOS and MET are both used in 'version2' time period, the MET 
#   param names are changed to the SMOS ones (see lines 416-432)
# rev 1 May 10, ds
#   Specific  corrections were applied to SIRS short_in and short_out
#   see lines starting at #818
#      (Scot's email of Jan. 11, 2010)
#   strDate_numDate subroutine added to change dates from string to number
# rev 6 Jun 10, ds
#   more flagging changes from Scot's email of June 3rd
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
@met_fields   = ("time_offset", "atmos_pressure", "qc_atmos_pressure", "temp_mean", "qc_temp_mean", "rh_mean", "qc_rh_mean",  "wspd_vec_mean", "qc_wspd_vec_mean", "wdir_vec_mean", "qc_wdir_vec_mean", "tbrg_precip_total_corr", "qc_tbrg_precip_total_corr");

%params = (
    "irt"   => \@irt_fields,
    "sirs"  => \@sirs_fields,
    "smos"  => \@smos_fields,
	"met"   => \@met_fields
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
    "smos"  => "SMOS/",
	"met"   => "MET/"
);

%filename_pattern = (
    "irt"   => 'sgpirt10m([CE]\d{1,3}).{4}(\d{4})(\d{2})(\d{2})\.(\d{2})(\d{2})',
    "sirs"  => 'sgpsirs([CE]\d{1,3}).{4}(\d{4})(\d{2})(\d{2})\.(\d{2})(\d{2})',
    "smos"  => 'sgp1smos([CE]\d{1,3}).{4}(\d{4})(\d{2})(\d{2})\.(\d{2})(\d{2})',
    "met"   => 'sgpmet([CE]\d{1,3}).{4}(\d{4})(\d{2})(\d{2})\.(\d{2})(\d{2})'
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
$CSE_id       = "CPPA";
$site_id      = "SGP";
$platform_id  = "XXXX";             # e.g. "SMOS"           
$stn_id       = "Exx";              # e.g. "E18"

$network            = "ARM_$platform_id";
$project_begin_date = 20050101;     # version2 time period
$project_end_date   = 20091231;     

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

open (OUTFILE_SFC, ">./out/$outfile1") || die "Can't open $outfile1";
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
						# rename met params to match smos ones
	                    #------------------------------------------------------------
						$obsName = "bar_pres" if $obsName eq "atmos_pressure";
						$obsName = "qc_bar_pres" if $obsName eq "qc_atmos_pressure";
						$obsName = "temp" if $obsName eq "temp_mean";
						$obsName = "qc_temp" if $obsName eq "qc_temp_mean";
						$obsName = "rh" if $obsName eq "rh_mean";
						$obsName = "qc_rh" if $obsName eq "qc_rh_mean";
						$obsName = "wspd" if $obsName eq "wspd_vec_mean";
						$obsName = "qc_wspd" if $obsName eq "qc_wspd_vec_mean";
						$obsName = "wdir" if $obsName eq "wdir_vec_mean";
						$obsName = "qc_wdir" if $obsName eq "qc_wdir_vec_mean";
						$obsName = "precip" if $obsName eq "tbrg_precip_total_corr";
						$obsName = "qc_precip" if $obsName eq "qc_tbrg_precip_total_corr";
						$obsName = "" if $obsName eq "";
						$obsName = "qc_" if $obsName eq "qc_";

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
	if($stn_pres != $MISSING) {
		$stn_pres_flag = "B" if($stn_pres < 600 || $stn_pres > 1200);		# per Scot's email of June 3, 2010
	}
    printf OUTFILE_SFC (" %7.2f", $stn_pres);
	printf OUTFILE_SFC (" %s", $stn_pres_flag);
        
    #------------------------------------------------------------
    # Print out the temperature
    #------------------------------------------------------------ 
        
 	$temp_air_flag = &get_flag(\$temp_air, $temp_air_flag, "temp", $id, $date, $hour, $min);
	$temp_air_flag = "B" if($temp_air > 50);							# per Scot's email of June 3, 2010
    printf OUTFILE_SFC (" %7.2f", $temp_air);
	printf OUTFILE_SFC (" %s", $temp_air_flag);
        
    #------------------------------------------------------------
    # Calculate and print out the dew point temperature 
	#  (using rel hum flag)
    #------------------------------------------------------------

 	$rel_hum_flag = &get_flag(\$rel_hum, $rel_hum_flag, "rh", $id, $date, $hour, $min);
	if($rel_hum != $MISSING) {
		$rel_hum_flag = "B" if($rel_hum < 0 || $rel_hum > 110);				# per Scot's email of June 3, 2010
	}
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
	#
	# The following corrections were applied to SIRS:
	# E27 from 1649 on 05/20/2009 to 2005 on 06/03/2009
	# down_short_hemisp(new) = down_short_hemisp(old) * 128.92 / 104.15
	#   from Scot's email of Jan. 11, 2010
    #------------------------------------------------------------ 
          
	$short_in_flag = &get_flag(\$short_in, $short_in_flag, "down_short_hemisp", $id, $date, $hour, $min);
	if($id eq "E27") {
    	$in_datetime = &strDate_numDate($date, $hour, $min);
		if (($in_datetime >= 20090520.1649 && $in_datetime <= 20090603.2005) && ($short_in != $MISSING)) {
			$old_short_in = $short_in;
			$short_in = $short_in * (128.92/104.15);
			print "down_short_hemisp fixed at $date on $hour:$min from $old_short_in to $short_in\n";
		}
	}
	if($short_in != $MISSING) {
		$short_in_flag = "B" if($short_in < -100 || $short_in > 2000);			 # per Scot's email of June 3, 2010
	}
    printf OUTFILE_SFC (" %8.2f", $short_in);
	printf OUTFILE_SFC (" %s", $short_in_flag);

    #------------------------------------------------------------
    # Print out the outgoing shortwave radiation
	#
	# The following corrections were applied to SIRS:
	# E27 from 1649 on 05/20/2009 to 2005 on 06/03/2009
	# up_short_hemisp(new) = up_short_hemisp(old) * 116.66/111.85
	#   from Scot's email of Jan. 11, 2010
    #------------------------------------------------------------ 
          
 	$short_out_flag = &get_flag(\$short_out, $short_out_flag, "up_short_hemisp", $id, $date, $hour, $min);
	if($id eq "E27") {
    	$out_datetime = &strDate_numDate($date, $hour, $min);
		if (($out_datetime >= 20090520.1649 && $out_datetime <= 20090603.2005) && ($short_out != $MISSING)) {
			$old_short_out = $short_out;
			$short_out = $short_out * (116.66/111.85);
			print "up_short_hemisp fixed at $date on $hour:$min from $old_short_out to $short_out\n";
		}
	}
	if($short_out != $MISSING) {
		$short_out_flag = "B" if($short_out < -100 || $short_out > 2000);		# per Scot's email of June 3, 2010
	}
	printf OUTFILE_SFC (" %8.2f", $short_out);
	printf OUTFILE_SFC (" %s", $short_out_flag);

    #------------------------------------------------------------
    # Print out the incoming longwave radiation
    #------------------------------------------------------------ 
          
 	$long_in_flag = &get_flag(\$long_in, $long_in_flag, "down_long_hemisp_shaded", $id, $date, $hour, $min);
	if($long_in != $MISSING) {
		$long_in_flag = "B" if($long_in < 0 || $long_in > 800);					# per Scot's email of June 3, 2010
	}
    printf OUTFILE_SFC (" %8.2f", $long_in);
	printf OUTFILE_SFC (" %s", $long_in_flag);

    #------------------------------------------------------------
    # Print out the outgoing longwave radiation
    #------------------------------------------------------------ 
          
 	$long_out_flag = &get_flag(\$long_out, $long_out_flag, "up_long_hemisp", $id, $date, $hour, $min);
	if($long_out != $MISSING) {
		$long_out_flag = "B" if($long_out < 0 || $long_out > 800);				# per Scot's email of June 3, 2010
	}
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
	if($net_rad != $MISSING) {
		$net_rad_flag = "B" if($net_rad < -500 || $net_rad  > 1500);			# per Scot's email of June 3, 2010
	}
    printf OUTFILE_SFC (" %8.2f", $net_rad);
	printf OUTFILE_SFC (" %s", $net_rad_flag);

    #------------------------------------------------------------
    # Print out skin temperature
    #------------------------------------------------------------ 
          
 	$skintemp_flag = &get_flag(\$skintemp, $skintemp_flag, "sfc_ir_temp", $id, $date, $hour, $min);
	if($skintemp != $MISSING) {
		$skintemp_flag = "B" if ($skintemp < -40);		# per Scot's email of June 3, 2010
	}
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
# and grouped them into files which are read by a utility 
# program--"prepare_DQR.pl".
#
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

    my $datetime = &strDate_numDate($date, $hour, $min);

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
        if ($var eq "up_short_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20060703.1509 && $datetime <= 20070306.1610) {
               $new_flag = "D";
           }
           if ($datetime >= 20061113.0900 && $datetime <= 20070123.1930) {
               $new_flag = "D";
           }
           if ($datetime >= 20041201.0000 && $datetime <= 20050101.0000) {
               $new_flag = "D";
           }
        }
        if ($var eq "down_long_hemisp_shaded") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20041201.0000 && $datetime <= 20050101.0000) {
               $new_flag = "D";
           }
           if ($datetime >= 20040927.1429 && $datetime <= 20050107.1623) {
               $new_flag = "D";
           }
        }
        if ($var eq "up_long_hemisp") {
           if ($datetime >= 20080117.0000 && $datetime <= 20080422.2052) {
               $new_flag = "D";
           }
           if ($datetime >= 20071003.1400 && $datetime <= 20080116.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20071003.1400 && $datetime <= 20080116.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20041201.0000 && $datetime <= 20050101.0000) {
               $new_flag = "D";
           }
           if ($datetime >= 20040927.1429 && $datetime <= 20050107.1623) {
               $new_flag = "D";
           }
           if ($datetime >= 20070117.0000 && $datetime <= 20080422.2052) {
               $new_flag = "B";
           }
        }
        if ($var eq "down_short_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20041201.0000 && $datetime <= 20050101.0000) {
               $new_flag = "D";
           }
        }
    #---------------------------------------------------------------------------
    # E1
    #---------------------------------------------------------------------------
    } elsif ($id eq "E1") {
        if ($var eq "wspd") {
           if ($datetime >= 20050202.0000 && $datetime <= 20050204.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20071211.0157 && $datetime <= 20071211.2059) {
               $new_flag = "B";
           }
           if ($datetime >= 20081112.2102 && $datetime <= 20081112.2142) {
               $new_flag = "B";
           }
           if ($datetime >= 20080401.1940 && $datetime <= 20080401.2005) {
               $new_flag = "B";
           }
           if ($datetime >= 20071017.1905 && $datetime <= 20071017.1934) {
               $new_flag = "B";
           }
           if ($datetime >= 20070403.1912 && $datetime <= 20070403.1930) {
               $new_flag = "B";
           }
           if ($datetime >= 20061003.2002 && $datetime <= 20061003.2039) {
               $new_flag = "B";
           }
           if ($datetime >= 20060418.2047 && $datetime <= 20060418.2110) {
               $new_flag = "B";
           }
           if ($datetime >= 20051116.2111 && $datetime <= 20051116.2200) {
               $new_flag = "B";
           }
           if ($datetime >= 20050405.1914 && $datetime <= 20050405.1935) {
               $new_flag = "B";
           }
        }
        if ($var eq "wdir") {
           if ($datetime >= 20050202.0000 && $datetime <= 20050204.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20071211.0157 && $datetime <= 20071211.2059) {
               $new_flag = "B";
           }
           if ($datetime >= 20051129.2100 && $datetime <= 20051213.2230) {
               $new_flag = "B";
           }
           if ($datetime >= 20081112.2102 && $datetime <= 20081112.2142) {
               $new_flag = "B";
           }
           if ($datetime >= 20080401.1940 && $datetime <= 20080401.2005) {
               $new_flag = "B";
           }
           if ($datetime >= 20071017.1905 && $datetime <= 20071017.1934) {
               $new_flag = "B";
           }
           if ($datetime >= 20070403.1912 && $datetime <= 20070403.1930) {
               $new_flag = "B";
           }
           if ($datetime >= 20061003.2002 && $datetime <= 20061003.2039) {
               $new_flag = "B";
           }
           if ($datetime >= 20060418.2047 && $datetime <= 20060418.2110) {
               $new_flag = "B";
           }
           if ($datetime >= 20051116.2111 && $datetime <= 20051116.2200) {
               $new_flag = "B";
           }
           if ($datetime >= 20050405.1914 && $datetime <= 20050405.1935) {
               $new_flag = "B";
           }
        }
        if ($var eq "down_long_hemisp_shaded") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "down_short_hemisp") {
           if ($datetime >= 20080628.2300 && $datetime <= 20080708.2150) {
               $new_flag = "D";
           }
           if ($datetime >= 20080510.0000 && $datetime <= 20080514.1827) {
               $new_flag = "D";
           }
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "precip") {
           if ($datetime >= 20080131.0000 && $datetime <= 20080202.2000) {
               $new_flag = "D";
           }
        }
        if ($var eq "bar_pres") {
           if ($datetime >= 20081112.2102 && $datetime <= 20081112.2142) {
               $new_flag = "D";
           }
           if ($datetime >= 20080401.1940 && $datetime <= 20080401.2005) {
               $new_flag = "D";
           }
           if ($datetime >= 20071017.1905 && $datetime <= 20071017.1934) {
               $new_flag = "D";
           }
           if ($datetime >= 20070403.1912 && $datetime <= 20070403.1930) {
               $new_flag = "D";
           }
           if ($datetime >= 20061003.2002 && $datetime <= 20061003.2039) {
               $new_flag = "D";
           }
           if ($datetime >= 20060418.2047 && $datetime <= 20060418.2110) {
               $new_flag = "D";
           }
           if ($datetime >= 20051116.2111 && $datetime <= 20051116.2200) {
               $new_flag = "D";
           }
           if ($datetime >= 20050405.1914 && $datetime <= 20050405.1935) {
               $new_flag = "D";
           }
        }
        if ($var eq "up_short_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20060221.2008 && $datetime <= 20060307.1950) {
               $new_flag = "D";
           }
        }
        if ($var eq "up_long_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "temp") {
           if ($datetime >= 20070403.0000 && $datetime <= 20070626.2025) {
               $new_flag = "D";
           }
           if ($datetime >= 20081112.2102 && $datetime <= 20081112.2142) {
               $new_flag = "D";
           }
           if ($datetime >= 20080401.1940 && $datetime <= 20080401.2005) {
               $new_flag = "D";
           }
           if ($datetime >= 20071017.1905 && $datetime <= 20071017.1934) {
               $new_flag = "D";
           }
           if ($datetime >= 20070403.1912 && $datetime <= 20070403.1930) {
               $new_flag = "D";
           }
           if ($datetime >= 20061003.2002 && $datetime <= 20061003.2039) {
               $new_flag = "D";
           }
           if ($datetime >= 20060418.2047 && $datetime <= 20060418.2110) {
               $new_flag = "D";
           }
           if ($datetime >= 20051116.2111 && $datetime <= 20051116.2200) {
               $new_flag = "D";
           }
           if ($datetime >= 20050405.1914 && $datetime <= 20050405.1935) {
               $new_flag = "D";
           }
           if ($datetime >= 20071217.2103 && $datetime <= 20071226.2126) {
               $new_flag = "B";
           }
        }
        if ($var eq "fc") {
           if ($datetime >= 20051018.2000 && $datetime <= 20051021.1800) {
               $new_flag = "B";
           }
        }
        if ($var eq "rh") {
           if ($datetime >= 20081112.2102 && $datetime <= 20081112.2142) {
               $new_flag = "D";
           }
           if ($datetime >= 20080401.1940 && $datetime <= 20080401.2005) {
               $new_flag = "D";
           }
           if ($datetime >= 20071017.1905 && $datetime <= 20071017.1934) {
               $new_flag = "D";
           }
           if ($datetime >= 20070403.1912 && $datetime <= 20070403.1930) {
               $new_flag = "D";
           }
           if ($datetime >= 20061003.2002 && $datetime <= 20061003.2039) {
               $new_flag = "D";
           }
           if ($datetime >= 20060418.2047 && $datetime <= 20060418.2110) {
               $new_flag = "D";
           }
           if ($datetime >= 20051116.2111 && $datetime <= 20051116.2200) {
               $new_flag = "D";
           }
           if ($datetime >= 20050405.1914 && $datetime <= 20050405.1935) {
               $new_flag = "D";
           }
           if ($datetime >= 20071217.2103 && $datetime <= 20071222.1559) {
               $new_flag = "B";
           }
           if ($datetime >= 20071217.2103 && $datetime <= 20071226.2126) {
               $new_flag = "B";
           }
        }
        if ($var eq "lv_e") {
           if ($datetime >= 20051018.2000 && $datetime <= 20051021.1800) {
               $new_flag = "B";
           }
        }
    #---------------------------------------------------------------------------
    # E10
    #---------------------------------------------------------------------------
    } elsif ($id eq "E10") {
        if ($var eq "down_long_hemisp_shaded") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "up_short_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "up_long_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "fc") {
           if ($datetime >= 20080510.1130 && $datetime <= 20080625.1500) {
               $new_flag = "D";
           }
           if ($datetime >= 20041214.2230 && $datetime <= 20050111.2200) {
               $new_flag = "D";
           }
           if ($datetime >= 20070502.1530 && $datetime <= 20070507.0200) {
               $new_flag = "B";
           }
        }
        if ($var eq "down_short_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "lv_e") {
           if ($datetime >= 20041214.2230 && $datetime <= 20050111.2200) {
               $new_flag = "D";
           }
           if ($datetime >= 20070502.1530 && $datetime <= 20070507.0200) {
               $new_flag = "B";
           }
        }
    #---------------------------------------------------------------------------
    # E11
    #---------------------------------------------------------------------------
    } elsif ($id eq "E11") {
        if ($var eq "wspd") {
           if ($datetime >= 20050104.0000 && $datetime <= 20050203.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20071211.2327 && $datetime <= 20071212.1907) {
               $new_flag = "B";
           }
           if ($datetime >= 20081021.1605 && $datetime <= 20081021.1620) {
               $new_flag = "B";
           }
           if ($datetime >= 20080408.1940 && $datetime <= 20080408.2025) {
               $new_flag = "B";
           }
           if ($datetime >= 20071010.1826 && $datetime <= 20071010.1838) {
               $new_flag = "B";
           }
           if ($datetime >= 20070420.1758 && $datetime <= 20070420.1824) {
               $new_flag = "B";
           }
           if ($datetime >= 20061010.1826 && $datetime <= 20061010.1847) {
               $new_flag = "B";
           }
           if ($datetime >= 20060425.1827 && $datetime <= 20060425.1900) {
               $new_flag = "B";
           }
           if ($datetime >= 20051121.2057 && $datetime <= 20051121.2110) {
               $new_flag = "B";
           }
           if ($datetime >= 20050412.1821 && $datetime <= 20050412.1840) {
               $new_flag = "B";
           }
        }
        if ($var eq "wdir") {
           if ($datetime >= 20050104.0000 && $datetime <= 20050203.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20071211.2327 && $datetime <= 20071212.1907) {
               $new_flag = "B";
           }
           if ($datetime >= 20081021.1605 && $datetime <= 20081021.1620) {
               $new_flag = "B";
           }
           if ($datetime >= 20080408.1940 && $datetime <= 20080408.2025) {
               $new_flag = "B";
           }
           if ($datetime >= 20071010.1826 && $datetime <= 20071010.1838) {
               $new_flag = "B";
           }
           if ($datetime >= 20070420.1758 && $datetime <= 20070420.1824) {
               $new_flag = "B";
           }
           if ($datetime >= 20061010.1826 && $datetime <= 20061010.1847) {
               $new_flag = "B";
           }
           if ($datetime >= 20060425.1827 && $datetime <= 20060425.1900) {
               $new_flag = "B";
           }
           if ($datetime >= 20051121.2057 && $datetime <= 20051121.2110) {
               $new_flag = "B";
           }
           if ($datetime >= 20050412.1821 && $datetime <= 20050412.1840) {
               $new_flag = "B";
           }
        }
        if ($var eq "down_long_hemisp_shaded") {
           if ($datetime >= 20081202.1700 && $datetime <= 20081202.1800) {
               $new_flag = "D";
           }
           if ($datetime >= 20081110.0300 && $datetime <= 20081110.0600) {
               $new_flag = "D";
           }
           if ($datetime >= 20081108.1900 && $datetime <= 20081108.2200) {
               $new_flag = "D";
           }
           if ($datetime >= 20081107.2000 && $datetime <= 20081108.1200) {
               $new_flag = "D";
           }
           if ($datetime >= 20081106.1800 && $datetime <= 20081106.2100) {
               $new_flag = "D";
           }
           if ($datetime >= 20081105.1300 && $datetime <= 20081106.0200) {
               $new_flag = "D";
           }
           if ($datetime >= 20081104.2300 && $datetime <= 20081105.0300) {
               $new_flag = "D";
           }
           if ($datetime >= 20081030.0700 && $datetime <= 20081030.1030) {
               $new_flag = "D";
           }
           if ($datetime >= 20081026.0100 && $datetime <= 20081029.1730) {
               $new_flag = "D";
           }
           if ($datetime >= 20081024.0300 && $datetime <= 20081024.1200) {
               $new_flag = "D";
           }
           if ($datetime >= 20081022.0000 && $datetime <= 20081022.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20081021.1300 && $datetime <= 20081021.1800) {
               $new_flag = "D";
           }
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "up_short_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "up_long_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "temp") {
           if ($datetime >= 20081021.1605 && $datetime <= 20081021.1620) {
               $new_flag = "D";
           }
           if ($datetime >= 20080408.1940 && $datetime <= 20080408.2025) {
               $new_flag = "D";
           }
           if ($datetime >= 20071010.1826 && $datetime <= 20071010.1838) {
               $new_flag = "D";
           }
           if ($datetime >= 20070420.1758 && $datetime <= 20070420.1824) {
               $new_flag = "D";
           }
           if ($datetime >= 20061010.1826 && $datetime <= 20061010.1847) {
               $new_flag = "D";
           }
           if ($datetime >= 20060425.1827 && $datetime <= 20060425.1900) {
               $new_flag = "D";
           }
           if ($datetime >= 20051121.2057 && $datetime <= 20051121.2110) {
               $new_flag = "D";
           }
           if ($datetime >= 20050412.1821 && $datetime <= 20050412.1840) {
               $new_flag = "D";
           }
        }
        if ($var eq "down_short_hemisp") {
           if ($datetime >= 20081202.1700 && $datetime <= 20081202.1800) {
               $new_flag = "D";
           }
           if ($datetime >= 20081110.0300 && $datetime <= 20081110.0600) {
               $new_flag = "D";
           }
           if ($datetime >= 20081108.1900 && $datetime <= 20081108.2200) {
               $new_flag = "D";
           }
           if ($datetime >= 20081107.2000 && $datetime <= 20081108.1200) {
               $new_flag = "D";
           }
           if ($datetime >= 20081106.1800 && $datetime <= 20081106.2100) {
               $new_flag = "D";
           }
           if ($datetime >= 20081105.1300 && $datetime <= 20081106.0200) {
               $new_flag = "D";
           }
           if ($datetime >= 20081104.2300 && $datetime <= 20081105.0300) {
               $new_flag = "D";
           }
           if ($datetime >= 20081030.0700 && $datetime <= 20081030.1030) {
               $new_flag = "D";
           }
           if ($datetime >= 20081026.0100 && $datetime <= 20081029.1730) {
               $new_flag = "D";
           }
           if ($datetime >= 20081024.0300 && $datetime <= 20081024.1200) {
               $new_flag = "D";
           }
           if ($datetime >= 20081022.0000 && $datetime <= 20081022.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20081021.1300 && $datetime <= 20081021.1800) {
               $new_flag = "D";
           }
           if ($datetime >= 20070614.0336 && $datetime <= 20070814.2005) {
               $new_flag = "D";
           }
           if ($datetime >= 20051001.0000 && $datetime <= 20060404.1545) {
               $new_flag = "D";
           }
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "rh") {
           if ($datetime >= 20081021.1605 && $datetime <= 20081021.1620) {
               $new_flag = "D";
           }
           if ($datetime >= 20080408.1940 && $datetime <= 20080408.2025) {
               $new_flag = "D";
           }
           if ($datetime >= 20071010.1826 && $datetime <= 20071010.1838) {
               $new_flag = "D";
           }
           if ($datetime >= 20070420.1758 && $datetime <= 20070420.1824) {
               $new_flag = "D";
           }
           if ($datetime >= 20061010.1826 && $datetime <= 20061010.1847) {
               $new_flag = "D";
           }
           if ($datetime >= 20060425.1827 && $datetime <= 20060425.1900) {
               $new_flag = "D";
           }
           if ($datetime >= 20051121.2057 && $datetime <= 20051121.2110) {
               $new_flag = "D";
           }
           if ($datetime >= 20050412.1821 && $datetime <= 20050412.1840) {
               $new_flag = "D";
           }
        }
        if ($var eq "precip") {
           if ($datetime >= 20080131.0000 && $datetime <= 20080202.2000) {
               $new_flag = "D";
           }
        }
        if ($var eq "bar_pres") {
           if ($datetime >= 20081021.1605 && $datetime <= 20081021.1620) {
               $new_flag = "D";
           }
           if ($datetime >= 20080408.1940 && $datetime <= 20080408.2025) {
               $new_flag = "D";
           }
           if ($datetime >= 20071010.1826 && $datetime <= 20071010.1838) {
               $new_flag = "D";
           }
           if ($datetime >= 20070420.1758 && $datetime <= 20070420.1824) {
               $new_flag = "D";
           }
           if ($datetime >= 20061010.1826 && $datetime <= 20061010.1847) {
               $new_flag = "D";
           }
           if ($datetime >= 20060425.1827 && $datetime <= 20060425.1900) {
               $new_flag = "D";
           }
           if ($datetime >= 20051121.2057 && $datetime <= 20051121.2110) {
               $new_flag = "D";
           }
           if ($datetime >= 20050412.1821 && $datetime <= 20050412.1840) {
               $new_flag = "D";
           }
        }
    #---------------------------------------------------------------------------
    # E12
    #---------------------------------------------------------------------------
    } elsif ($id eq "E12") {
        if ($var eq "e") {
           if ($datetime >= 20060821.1200 && $datetime <= 20060829.1630) {
               $new_flag = "D";
           }
           if ($datetime >= 20050301.1800 && $datetime <= 20050329.1830) {
               $new_flag = "D";
           }
           if ($datetime >= 20060415.2300 && $datetime <= 20060425.1700) {
               $new_flag = "B";
           }
           if ($datetime >= 20051208.1100 && $datetime <= 20051208.1300) {
               $new_flag = "B";
           }
        }
        if ($var eq "down_long_hemisp_shaded") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "up_short_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20050426.1726 && $datetime <= 20050503.1840) {
               $new_flag = "B";
           }
        }
        if ($var eq "up_long_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "h") {
           if ($datetime >= 20060821.1200 && $datetime <= 20060829.1630) {
               $new_flag = "D";
           }
           if ($datetime >= 20050301.1800 && $datetime <= 20050329.1830) {
               $new_flag = "D";
           }
           if ($datetime >= 20060415.2300 && $datetime <= 20060425.1700) {
               $new_flag = "B";
           }
           if ($datetime >= 20051208.1100 && $datetime <= 20051208.1300) {
               $new_flag = "B";
           }
        }
        if ($var eq "down_short_hemisp") {
           if ($datetime >= 20070619.0000 && $datetime <= 20070814.1700) {
               $new_flag = "D";
           }
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20050426.1726 && $datetime <= 20050503.1840) {
               $new_flag = "B";
           }
        }
    #---------------------------------------------------------------------------
    # E13
    #---------------------------------------------------------------------------
    } elsif ($id eq "E13") {
        if ($var eq "wspd") {
           if ($datetime >= 20050105.0000 && $datetime <= 20050203.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20071210.2212 && $datetime <= 20071211.1945) {
               $new_flag = "B";
           }
           if ($datetime >= 20060501.1545 && $datetime <= 20060511.1825) {
               $new_flag = "B";
           }
           if ($datetime >= 20081030.1600 && $datetime <= 20081030.1625) {
               $new_flag = "B";
           }
           if ($datetime >= 20080411.1800 && $datetime <= 20080411.1840) {
               $new_flag = "B";
           }
           if ($datetime >= 20071009.1955 && $datetime <= 20071009.2025) {
               $new_flag = "B";
           }
           if ($datetime >= 20070416.1414 && $datetime <= 20070416.1441) {
               $new_flag = "B";
           }
           if ($datetime >= 20061013.1605 && $datetime <= 20061013.1635) {
               $new_flag = "B";
           }
           if ($datetime >= 20060501.1438 && $datetime <= 20060501.1545) {
               $new_flag = "B";
           }
           if ($datetime >= 20051123.1940 && $datetime <= 20051123.1953) {
               $new_flag = "B";
           }
           if ($datetime >= 20050415.1435 && $datetime <= 20050415.1455) {
               $new_flag = "B";
           }
        }
        if ($var eq "e") {
           if ($datetime >= 20060902.0400 && $datetime <= 20060914.1900) {
               $new_flag = "D";
           }
           if ($datetime >= 20060828.0030 && $datetime <= 20060828.0730) {
               $new_flag = "D";
           }
           if ($datetime >= 20060614.2030 && $datetime <= 20060615.1430) {
               $new_flag = "D";
           }
           if ($datetime >= 20060605.2330 && $datetime <= 20060606.1600) {
               $new_flag = "D";
           }
           if ($datetime >= 20060430.2100 && $datetime <= 20060501.1530) {
               $new_flag = "D";
           }
           if ($datetime >= 20060603.1900 && $datetime <= 20060604.2330) {
               $new_flag = "D";
           }
           if ($datetime >= 20060423.2300 && $datetime <= 20060423.2300) {
               $new_flag = "D";
           }
           if ($datetime >= 20050303.2130 && $datetime <= 20050331.2100) {
               $new_flag = "D";
           }
           if ($datetime >= 20060122.2330 && $datetime <= 20060126.2040) {
               $new_flag = "B";
           }
           if ($datetime >= 20050608.1900 && $datetime <= 20050706.1630) {
               $new_flag = "B";
           }
        }
        if ($var eq "wdir") {
           if ($datetime >= 20050105.0000 && $datetime <= 20050203.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20071210.2212 && $datetime <= 20071211.1945) {
               $new_flag = "B";
           }
           if ($datetime >= 20081030.1600 && $datetime <= 20081030.1625) {
               $new_flag = "B";
           }
           if ($datetime >= 20080411.1800 && $datetime <= 20080411.1840) {
               $new_flag = "B";
           }
           if ($datetime >= 20071009.1955 && $datetime <= 20071009.2025) {
               $new_flag = "B";
           }
           if ($datetime >= 20070416.1414 && $datetime <= 20070416.1441) {
               $new_flag = "B";
           }
           if ($datetime >= 20061013.1605 && $datetime <= 20061013.1635) {
               $new_flag = "B";
           }
           if ($datetime >= 20060501.1438 && $datetime <= 20060501.1545) {
               $new_flag = "B";
           }
           if ($datetime >= 20051123.1940 && $datetime <= 20051123.1953) {
               $new_flag = "B";
           }
           if ($datetime >= 20050415.1435 && $datetime <= 20050415.1455) {
               $new_flag = "B";
           }
        }
        if ($var eq "down_long_hemisp_shaded") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20050525.1409 && $datetime <= 20050602.2205) {
               $new_flag = "B";
           }
        }
        if ($var eq "down_short_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "precip") {
           if ($datetime >= 20080131.0000 && $datetime <= 20080202.2000) {
               $new_flag = "D";
           }
           if ($datetime >= 20081013.0000 && $datetime <= 20081017.1755) {
               $new_flag = "B";
           }
        }
        if ($var eq "bar_pres") {
           if ($datetime >= 20081030.1600 && $datetime <= 20081030.1625) {
               $new_flag = "D";
           }
           if ($datetime >= 20080411.1800 && $datetime <= 20080411.1840) {
               $new_flag = "D";
           }
           if ($datetime >= 20071009.1955 && $datetime <= 20071009.2025) {
               $new_flag = "D";
           }
           if ($datetime >= 20070416.1414 && $datetime <= 20070416.1441) {
               $new_flag = "D";
           }
           if ($datetime >= 20061013.1605 && $datetime <= 20061013.1635) {
               $new_flag = "D";
           }
           if ($datetime >= 20060501.1438 && $datetime <= 20060501.1545) {
               $new_flag = "D";
           }
           if ($datetime >= 20051123.1940 && $datetime <= 20051123.1953) {
               $new_flag = "D";
           }
           if ($datetime >= 20050415.1435 && $datetime <= 20050415.1455) {
               $new_flag = "D";
           }
        }
        if ($var eq "up_long_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20050525.1409 && $datetime <= 20050602.2205) {
               $new_flag = "B";
           }
        }
        if ($var eq "up_short_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "temp") {
           if ($datetime >= 20081208.1547 && $datetime <= 20081210.2059) {
               $new_flag = "D";
           }
           if ($datetime >= 20081030.1600 && $datetime <= 20081030.1625) {
               $new_flag = "D";
           }
           if ($datetime >= 20080411.1800 && $datetime <= 20080411.1840) {
               $new_flag = "D";
           }
           if ($datetime >= 20071009.1955 && $datetime <= 20071009.2025) {
               $new_flag = "D";
           }
           if ($datetime >= 20070416.1414 && $datetime <= 20070416.1441) {
               $new_flag = "D";
           }
           if ($datetime >= 20061013.1605 && $datetime <= 20061013.1635) {
               $new_flag = "D";
           }
           if ($datetime >= 20060501.1438 && $datetime <= 20060501.1545) {
               $new_flag = "D";
           }
           if ($datetime >= 20051123.1940 && $datetime <= 20051123.1953) {
               $new_flag = "D";
           }
           if ($datetime >= 20050415.1435 && $datetime <= 20050415.1455) {
               $new_flag = "D";
           }
        }
        if ($var eq "h") {
           if ($datetime >= 20060902.0400 && $datetime <= 20060914.1900) {
               $new_flag = "D";
           }
           if ($datetime >= 20060828.0030 && $datetime <= 20060828.0730) {
               $new_flag = "D";
           }
           if ($datetime >= 20060614.2030 && $datetime <= 20060615.1430) {
               $new_flag = "D";
           }
           if ($datetime >= 20060605.2330 && $datetime <= 20060606.1600) {
               $new_flag = "D";
           }
           if ($datetime >= 20060430.2100 && $datetime <= 20060501.1530) {
               $new_flag = "D";
           }
           if ($datetime >= 20060603.1900 && $datetime <= 20060604.2330) {
               $new_flag = "D";
           }
           if ($datetime >= 20060423.2300 && $datetime <= 20060423.2300) {
               $new_flag = "D";
           }
           if ($datetime >= 20050303.2130 && $datetime <= 20050331.2100) {
               $new_flag = "D";
           }
           if ($datetime >= 20060122.2330 && $datetime <= 20060126.2040) {
               $new_flag = "B";
           }
           if ($datetime >= 20050608.1900 && $datetime <= 20050706.1630) {
               $new_flag = "B";
           }
        }
        if ($var eq "c_shf1") {
           if ($datetime >= 20060828.0030 && $datetime <= 20060828.0730) {
               $new_flag = "D";
           }
        }
        if ($var eq "rh") {
           if ($datetime >= 20081208.1547 && $datetime <= 20081210.2059) {
               $new_flag = "D";
           }
           if ($datetime >= 20070416.0000 && $datetime <= 20070618.2000) {
               $new_flag = "D";
           }
           if ($datetime >= 20081030.1600 && $datetime <= 20081030.1625) {
               $new_flag = "D";
           }
           if ($datetime >= 20080411.1800 && $datetime <= 20080411.1840) {
               $new_flag = "D";
           }
           if ($datetime >= 20071009.1955 && $datetime <= 20071009.2025) {
               $new_flag = "D";
           }
           if ($datetime >= 20070416.1414 && $datetime <= 20070416.1441) {
               $new_flag = "D";
           }
           if ($datetime >= 20061013.1605 && $datetime <= 20061013.1635) {
               $new_flag = "D";
           }
           if ($datetime >= 20060501.1438 && $datetime <= 20060501.1545) {
               $new_flag = "D";
           }
           if ($datetime >= 20051123.1940 && $datetime <= 20051123.1953) {
               $new_flag = "D";
           }
           if ($datetime >= 20050415.1435 && $datetime <= 20050415.1455) {
               $new_flag = "D";
           }
        }
        if ($var eq "g1") {
           if ($datetime >= 20060828.0030 && $datetime <= 20060828.0730) {
               $new_flag = "D";
           }
        }
    #---------------------------------------------------------------------------
    # E14
    #---------------------------------------------------------------------------
    } elsif ($id eq "E14") {
        if ($var eq "h") {
           if ($datetime >= 20070114.0500 && $datetime <= 20070202.1600) {
               $new_flag = "D";
           }
           if ($datetime >= 20060707.1930 && $datetime <= 20060714.1500) {
               $new_flag = "B";
           }
           if ($datetime >= 20060505.2100 && $datetime <= 20060613.1400) {
               $new_flag = "B";
           }
           if ($datetime >= 20060124.1500 && $datetime <= 20060213.1930) {
               $new_flag = "B";
           }
           if ($datetime >= 20051201.1930 && $datetime <= 20051213.1630) {
               $new_flag = "B";
           }
           if ($datetime >= 20051104.1330 && $datetime <= 20051117.1630) {
               $new_flag = "B";
           }
        }
        if ($var eq "fc") {
           if ($datetime >= 20070114.0500 && $datetime <= 20070202.1600) {
               $new_flag = "D";
           }
           if ($datetime >= 20060707.1930 && $datetime <= 20060714.1500) {
               $new_flag = "B";
           }
           if ($datetime >= 20060505.2100 && $datetime <= 20060613.1400) {
               $new_flag = "B";
           }
           if ($datetime >= 20060124.1500 && $datetime <= 20060213.1930) {
               $new_flag = "B";
           }
           if ($datetime >= 20051201.1930 && $datetime <= 20051213.1630) {
               $new_flag = "B";
           }
           if ($datetime >= 20051104.1330 && $datetime <= 20051117.1630) {
               $new_flag = "B";
           }
           if ($datetime >= 20050825.0000 && $datetime <= 20050829.0230) {
               $new_flag = "B";
           }
        }
        if ($var eq "lv_e") {
           if ($datetime >= 20070114.0500 && $datetime <= 20070202.1600) {
               $new_flag = "D";
           }
           if ($datetime >= 20060707.1930 && $datetime <= 20060714.1500) {
               $new_flag = "B";
           }
           if ($datetime >= 20060505.2100 && $datetime <= 20060613.1400) {
               $new_flag = "B";
           }
           if ($datetime >= 20060124.1500 && $datetime <= 20060213.1930) {
               $new_flag = "B";
           }
           if ($datetime >= 20051201.1930 && $datetime <= 20051213.1630) {
               $new_flag = "B";
           }
           if ($datetime >= 20051104.1330 && $datetime <= 20051117.1630) {
               $new_flag = "B";
           }
           if ($datetime >= 20050825.0000 && $datetime <= 20050829.0230) {
               $new_flag = "B";
           }
        }
    #---------------------------------------------------------------------------
    # E15
    #---------------------------------------------------------------------------
    } elsif ($id eq "E15") {
        if ($var eq "wspd") {
           if ($datetime >= 20050104.0000 && $datetime <= 20050203.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20071211.0727 && $datetime <= 20071212.0001) {
               $new_flag = "B";
           }
           if ($datetime >= 20050108.0000 && $datetime <= 20050108.1645) {
               $new_flag = "B";
           }
           if ($datetime >= 20081021.1800 && $datetime <= 20081021.1840) {
               $new_flag = "B";
           }
           if ($datetime >= 20080408.1700 && $datetime <= 20080408.1828) {
               $new_flag = "B";
           }
           if ($datetime >= 20071010.1557 && $datetime <= 20071010.1619) {
               $new_flag = "B";
           }
           if ($datetime >= 20070420.1450 && $datetime <= 20070420.1525) {
               $new_flag = "B";
           }
           if ($datetime >= 20061010.1612 && $datetime <= 20061010.1645) {
               $new_flag = "B";
           }
           if ($datetime >= 20060425.1555 && $datetime <= 20060425.1652) {
               $new_flag = "B";
           }
           if ($datetime >= 20051121.1753 && $datetime <= 20051121.1850) {
               $new_flag = "B";
           }
           if ($datetime >= 20050412.1600 && $datetime <= 20050412.1630) {
               $new_flag = "B";
           }
        }
        if ($var eq "e") {
           if ($datetime >= 20080611.1500 && $datetime <= 20080620.1500) {
               $new_flag = "D";
           }
           if ($datetime >= 20051030.0000 && $datetime <= 20051108.1815) {
               $new_flag = "D";
           }
           if ($datetime >= 20050301.1800 && $datetime <= 20050329.1900) {
               $new_flag = "D";
           }
           if ($datetime >= 20050513.0800 && $datetime <= 20050524.1700) {
               $new_flag = "D";
           }
           if ($datetime >= 20041108.1830 && $datetime <= 20050301.1630) {
               $new_flag = "D";
           }
           if ($datetime >= 20060110.1330 && $datetime <= 20060117.1730) {
               $new_flag = "B";
           }
        }
        if ($var eq "wdir") {
           if ($datetime >= 20060307.0517 && $datetime <= 20060328.1904) {
               $new_flag = "D";
           }
           if ($datetime >= 20050104.0000 && $datetime <= 20050203.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20071211.0727 && $datetime <= 20071212.0001) {
               $new_flag = "B";
           }
           if ($datetime >= 20081021.1800 && $datetime <= 20081021.1840) {
               $new_flag = "B";
           }
           if ($datetime >= 20080408.1700 && $datetime <= 20080408.1828) {
               $new_flag = "B";
           }
           if ($datetime >= 20071010.1557 && $datetime <= 20071010.1619) {
               $new_flag = "B";
           }
           if ($datetime >= 20070420.1450 && $datetime <= 20070420.1525) {
               $new_flag = "B";
           }
           if ($datetime >= 20061010.1612 && $datetime <= 20061010.1645) {
               $new_flag = "B";
           }
           if ($datetime >= 20060425.1555 && $datetime <= 20060425.1652) {
               $new_flag = "B";
           }
           if ($datetime >= 20051121.1753 && $datetime <= 20051121.1850) {
               $new_flag = "B";
           }
           if ($datetime >= 20050412.1600 && $datetime <= 20050412.1630) {
               $new_flag = "B";
           }
        }
        if ($var eq "down_long_hemisp_shaded") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "corr_soil_heat_flow_1") {
           if ($datetime >= 20090127.1930 && $datetime <= 20090225.1830) {
               $new_flag = "D";
           }
        }
        if ($var eq "down_short_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "precip") {
           if ($datetime >= 20080131.0000 && $datetime <= 20080202.2000) {
               $new_flag = "D";
           }
        }
        if ($var eq "bar_pres") {
           if ($datetime >= 20081021.1800 && $datetime <= 20081021.1840) {
               $new_flag = "D";
           }
           if ($datetime >= 20080408.1700 && $datetime <= 20080408.1828) {
               $new_flag = "D";
           }
           if ($datetime >= 20071010.1557 && $datetime <= 20071010.1619) {
               $new_flag = "D";
           }
           if ($datetime >= 20070420.1450 && $datetime <= 20070420.1525) {
               $new_flag = "D";
           }
           if ($datetime >= 20061010.1612 && $datetime <= 20061010.1645) {
               $new_flag = "D";
           }
           if ($datetime >= 20060425.1555 && $datetime <= 20060425.1652) {
               $new_flag = "D";
           }
           if ($datetime >= 20051121.1753 && $datetime <= 20051121.1850) {
               $new_flag = "D";
           }
           if ($datetime >= 20050412.1600 && $datetime <= 20050412.1630) {
               $new_flag = "D";
           }
           if ($datetime >= 20060103.1705 && $datetime <= 20060131.1804) {
               $new_flag = "B";
           }
        }
        if ($var eq "up_long_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "up_short_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "temp") {
           if ($datetime >= 20081021.1800 && $datetime <= 20081021.1840) {
               $new_flag = "D";
           }
           if ($datetime >= 20080408.1700 && $datetime <= 20080408.1828) {
               $new_flag = "D";
           }
           if ($datetime >= 20071010.1557 && $datetime <= 20071010.1619) {
               $new_flag = "D";
           }
           if ($datetime >= 20070420.1450 && $datetime <= 20070420.1525) {
               $new_flag = "D";
           }
           if ($datetime >= 20061010.1612 && $datetime <= 20061010.1645) {
               $new_flag = "D";
           }
           if ($datetime >= 20060425.1555 && $datetime <= 20060425.1652) {
               $new_flag = "D";
           }
           if ($datetime >= 20051121.1753 && $datetime <= 20051121.1850) {
               $new_flag = "D";
           }
           if ($datetime >= 20050412.1600 && $datetime <= 20050412.1630) {
               $new_flag = "D";
           }
        }
        if ($var eq "h") {
           if ($datetime >= 20080611.1500 && $datetime <= 20080620.1500) {
               $new_flag = "D";
           }
           if ($datetime >= 20051030.0000 && $datetime <= 20051108.1815) {
               $new_flag = "D";
           }
           if ($datetime >= 20050301.1800 && $datetime <= 20050329.1900) {
               $new_flag = "D";
           }
           if ($datetime >= 20050513.0800 && $datetime <= 20050524.1700) {
               $new_flag = "D";
           }
           if ($datetime >= 20041108.1830 && $datetime <= 20050301.1630) {
               $new_flag = "D";
           }
           if ($datetime >= 20060110.1330 && $datetime <= 20060117.1730) {
               $new_flag = "B";
           }
        }
        if ($var eq "c_shf1") {
           if ($datetime >= 20041108.1830 && $datetime <= 20050301.1630) {
               $new_flag = "D";
           }
        }
        if ($var eq "rh") {
           if ($datetime >= 20081021.1800 && $datetime <= 20081021.1840) {
               $new_flag = "D";
           }
           if ($datetime >= 20080408.1700 && $datetime <= 20080408.1828) {
               $new_flag = "D";
           }
           if ($datetime >= 20071010.1557 && $datetime <= 20071010.1619) {
               $new_flag = "D";
           }
           if ($datetime >= 20070420.1450 && $datetime <= 20070420.1525) {
               $new_flag = "D";
           }
           if ($datetime >= 20061010.1612 && $datetime <= 20061010.1645) {
               $new_flag = "D";
           }
           if ($datetime >= 20060425.1555 && $datetime <= 20060425.1652) {
               $new_flag = "D";
           }
           if ($datetime >= 20051121.1753 && $datetime <= 20051121.1850) {
               $new_flag = "D";
           }
           if ($datetime >= 20050412.1600 && $datetime <= 20050412.1630) {
               $new_flag = "D";
           }
        }
        if ($var eq "g1") {
           if ($datetime >= 20041108.1830 && $datetime <= 20050301.1630) {
               $new_flag = "D";
           }
        }
    #---------------------------------------------------------------------------
    # E16
    #---------------------------------------------------------------------------
    } elsif ($id eq "E16") {
        if ($var eq "down_long_hemisp_shaded") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "up_short_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "up_long_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "h") {
           if ($datetime >= 20051109.1630 && $datetime <= 20060118.1230) {
               $new_flag = "B";
           }
        }
        if ($var eq "fc") {
           if ($datetime >= 20060621.1500 && $datetime <= 20060623.1600) {
               $new_flag = "B";
           }
           if ($datetime >= 20051109.1630 && $datetime <= 20060118.1230) {
               $new_flag = "B";
           }
        }
        if ($var eq "down_short_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "lv_e") {
           if ($datetime >= 20060621.1500 && $datetime <= 20060623.1600) {
               $new_flag = "B";
           }
           if ($datetime >= 20051109.1630 && $datetime <= 20060118.1230) {
               $new_flag = "B";
           }
        }
    #---------------------------------------------------------------------------
    # E18
    #---------------------------------------------------------------------------
    } elsif ($id eq "E18") {
        if ($var eq "e") {
           if ($datetime >= 20080506.1215 && $datetime <= 20080617.2120) {
               $new_flag = "D";
           }
           if ($datetime >= 20080415.0030 && $datetime <= 20080506.2110) {
               $new_flag = "D";
           }
           if ($datetime >= 20060531.0430 && $datetime <= 20060602.2130) {
               $new_flag = "D";
           }
           if ($datetime >= 20050301.2330 && $datetime <= 20050330.0000) {
               $new_flag = "D";
           }
           if ($datetime >= 20050727.0730 && $datetime <= 20050803.2100) {
               $new_flag = "D";
           }
           if ($datetime >= 20061030.2200 && $datetime <= 20061107.2130) {
               $new_flag = "B";
           }
        }
        if ($var eq "down_long_hemisp_shaded") {
           if ($datetime >= 20070512.1030 && $datetime <= 20080102.2240) {
               $new_flag = "D";
           }
           if ($datetime >= 20070512.1030 && $datetime <= 20080102.2240) {
               $new_flag = "D";
           }
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "up_short_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "up_long_hemisp") {
           if ($datetime >= 20070118.2230 && $datetime <= 20070724.1400) {
               $new_flag = "D";
           }
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "h") {
           if ($datetime >= 20080506.1215 && $datetime <= 20080617.2120) {
               $new_flag = "D";
           }
           if ($datetime >= 20080415.0030 && $datetime <= 20080506.2110) {
               $new_flag = "D";
           }
           if ($datetime >= 20060531.0430 && $datetime <= 20060602.2130) {
               $new_flag = "D";
           }
           if ($datetime >= 20050301.2330 && $datetime <= 20050330.0000) {
               $new_flag = "D";
           }
           if ($datetime >= 20050727.0730 && $datetime <= 20050803.2100) {
               $new_flag = "D";
           }
           if ($datetime >= 20061030.2200 && $datetime <= 20061107.2130) {
               $new_flag = "B";
           }
        }
        if ($var eq "down_short_hemisp") {
           if ($datetime >= 20080618.0000 && $datetime <= 20080630.2110) {
               $new_flag = "D";
           }
           if ($datetime >= 20050203.1200 && $datetime <= 20050215.2250) {
               $new_flag = "D";
           }
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
    #---------------------------------------------------------------------------
    # E19
    #---------------------------------------------------------------------------
    } elsif ($id eq "E19") {
        if ($var eq "e") {
           if ($datetime >= 20050303.1730 && $datetime <= 20050331.1600) {
               $new_flag = "D";
           }
           if ($datetime >= 20050507.2300 && $datetime <= 20050512.1445) {
               $new_flag = "D";
           }
           if ($datetime >= 20080509.1330 && $datetime <= 20080509.1330) {
               $new_flag = "B";
           }
           if ($datetime >= 20080507.1530 && $datetime <= 20080507.2130) {
               $new_flag = "B";
           }
           if ($datetime >= 20080507.0600 && $datetime <= 20080507.0900) {
               $new_flag = "B";
           }
           if ($datetime >= 20051113.0000 && $datetime <= 20060119.1630) {
               $new_flag = "B";
           }
           if ($datetime >= 20051024.0600 && $datetime <= 20051025.0100) {
               $new_flag = "B";
           }
        }
        if ($var eq "down_long_hemisp_shaded") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "up_short_hemisp") {
           if ($datetime >= 20071013.0000 && $datetime <= 20080104.1645) {
               $new_flag = "D";
           }
           if ($datetime >= 20071013.0000 && $datetime <= 20080104.1645) {
               $new_flag = "D";
           }
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "up_long_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "h") {
           if ($datetime >= 20050303.1730 && $datetime <= 20050331.1600) {
               $new_flag = "D";
           }
           if ($datetime >= 20050507.2300 && $datetime <= 20050512.1445) {
               $new_flag = "D";
           }
           if ($datetime >= 20080509.1330 && $datetime <= 20080509.1330) {
               $new_flag = "B";
           }
           if ($datetime >= 20080507.1530 && $datetime <= 20080507.2130) {
               $new_flag = "B";
           }
           if ($datetime >= 20080507.0600 && $datetime <= 20080507.0900) {
               $new_flag = "B";
           }
           if ($datetime >= 20051113.0000 && $datetime <= 20060119.1630) {
               $new_flag = "B";
           }
           if ($datetime >= 20051024.0600 && $datetime <= 20051025.0100) {
               $new_flag = "B";
           }
        }
        if ($var eq "down_short_hemisp") {
           if ($datetime >= 20071013.0000 && $datetime <= 20080104.1645) {
               $new_flag = "D";
           }
           if ($datetime >= 20071013.0000 && $datetime <= 20080104.1645) {
               $new_flag = "D";
           }
           if ($datetime >= 20060111.0000 && $datetime <= 20060413.1522) {
               $new_flag = "D";
           }
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "g1") {
           if ($datetime >= 20051113.0000 && $datetime <= 20060119.1630) {
               $new_flag = "B";
           }
        }
    #---------------------------------------------------------------------------
    # E2
    #---------------------------------------------------------------------------
    } elsif ($id eq "E2") {
        if ($var eq "e") {
           if ($datetime >= 20080607.0030 && $datetime <= 20080612.1730) {
               $new_flag = "D";
           }
           if ($datetime >= 20080502.1630 && $datetime <= 20080515.2000) {
               $new_flag = "D";
           }
           if ($datetime >= 20050310.1600 && $datetime <= 20050324.1700) {
               $new_flag = "D";
           }
           if ($datetime >= 20050804.2200 && $datetime <= 20050811.1530) {
               $new_flag = "D";
           }
           if ($datetime >= 20051217.2130 && $datetime <= 20051221.1930) {
               $new_flag = "B";
           }
           if ($datetime >= 20051209.0700 && $datetime <= 20051209.0700) {
               $new_flag = "B";
           }
           if ($datetime >= 20051208.1130 && $datetime <= 20051208.1430) {
               $new_flag = "B";
           }
           if ($datetime >= 20051208.0700 && $datetime <= 20051208.0700) {
               $new_flag = "B";
           }
        }
        if ($var eq "down_long_hemisp_shaded") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "up_short_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "up_long_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "h") {
           if ($datetime >= 20080607.0030 && $datetime <= 20080612.1730) {
               $new_flag = "D";
           }
           if ($datetime >= 20080502.1630 && $datetime <= 20080515.2000) {
               $new_flag = "D";
           }
           if ($datetime >= 20050310.1600 && $datetime <= 20050324.1700) {
               $new_flag = "D";
           }
           if ($datetime >= 20050804.2200 && $datetime <= 20050811.1530) {
               $new_flag = "D";
           }
           if ($datetime >= 20051217.2130 && $datetime <= 20051221.1930) {
               $new_flag = "B";
           }
           if ($datetime >= 20051209.0700 && $datetime <= 20051209.0700) {
               $new_flag = "B";
           }
           if ($datetime >= 20051208.1130 && $datetime <= 20051208.1430) {
               $new_flag = "B";
           }
           if ($datetime >= 20051208.0700 && $datetime <= 20051208.0700) {
               $new_flag = "B";
           }
        }
        if ($var eq "down_short_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
    #---------------------------------------------------------------------------
    # E20
    #---------------------------------------------------------------------------
    } elsif ($id eq "E20") {
        if ($var eq "wspd") {
           if ($datetime >= 20070120.1927 && $datetime <= 20070121.0405) {
               $new_flag = "D";
           }
           if ($datetime >= 20070112.1613 && $datetime <= 20070115.2117) {
               $new_flag = "D";
           }
           if ($datetime >= 20071210.0150 && $datetime <= 20071210.1704) {
               $new_flag = "B";
           }
           if ($datetime >= 20081007.2130 && $datetime <= 20081007.2155) {
               $new_flag = "B";
           }
           if ($datetime >= 20080423.1815 && $datetime <= 20080423.1920) {
               $new_flag = "B";
           }
           if ($datetime >= 20071024.1819 && $datetime <= 20071024.1906) {
               $new_flag = "B";
           }
           if ($datetime >= 20070425.1921 && $datetime <= 20070425.1948) {
               $new_flag = "B";
           }
           if ($datetime >= 20061025.1745 && $datetime <= 20061025.1825) {
               $new_flag = "B";
           }
           if ($datetime >= 20060510.1925 && $datetime <= 20060510.2040) {
               $new_flag = "B";
           }
           if ($datetime >= 20051109.2116 && $datetime <= 20051109.2145) {
               $new_flag = "B";
           }
           if ($datetime >= 20050427.1824 && $datetime <= 20050427.1915) {
               $new_flag = "B";
           }
        }
        if ($var eq "e") {
           if ($datetime >= 20060617.0900 && $datetime <= 20060620.1915) {
               $new_flag = "D";
           }
           if ($datetime >= 20050302.1900 && $datetime <= 20050330.1900) {
               $new_flag = "D";
           }
           if ($datetime >= 20051221.0930 && $datetime <= 20051221.1730) {
               $new_flag = "B";
           }
        }
        if ($var eq "wdir") {
           if ($datetime >= 20071210.0150 && $datetime <= 20071210.1704) {
               $new_flag = "B";
           }
           if ($datetime >= 20050812.1651 && $datetime <= 20050817.1921) {
               $new_flag = "B";
           }
           if ($datetime >= 20081007.2130 && $datetime <= 20081007.2155) {
               $new_flag = "B";
           }
           if ($datetime >= 20080423.1815 && $datetime <= 20080423.1920) {
               $new_flag = "B";
           }
           if ($datetime >= 20071024.1819 && $datetime <= 20071024.1906) {
               $new_flag = "B";
           }
           if ($datetime >= 20070425.1921 && $datetime <= 20070425.1948) {
               $new_flag = "B";
           }
           if ($datetime >= 20061025.1745 && $datetime <= 20061025.1825) {
               $new_flag = "B";
           }
           if ($datetime >= 20060510.1925 && $datetime <= 20060510.2040) {
               $new_flag = "B";
           }
           if ($datetime >= 20051109.2116 && $datetime <= 20051109.2145) {
               $new_flag = "B";
           }
           if ($datetime >= 20050427.1824 && $datetime <= 20050427.1915) {
               $new_flag = "B";
           }
        }
        if ($var eq "down_long_hemisp_shaded") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "down_short_hemisp") {
           if ($datetime >= 20070115.0000 && $datetime <= 20070606.1745) {
               $new_flag = "D";
           }
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20050804.2207 && $datetime <= 20050817.1840) {
               $new_flag = "B";
           }
        }
        if ($var eq "precip") {
           if ($datetime >= 20080131.0000 && $datetime <= 20080202.2000) {
               $new_flag = "D";
           }
        }
        if ($var eq "bar_pres") {
           if ($datetime >= 20081007.2130 && $datetime <= 20081007.2155) {
               $new_flag = "D";
           }
           if ($datetime >= 20080423.1815 && $datetime <= 20080423.1920) {
               $new_flag = "D";
           }
           if ($datetime >= 20071024.1819 && $datetime <= 20071024.1906) {
               $new_flag = "D";
           }
           if ($datetime >= 20070425.1921 && $datetime <= 20070425.1948) {
               $new_flag = "D";
           }
           if ($datetime >= 20061025.1745 && $datetime <= 20061025.1825) {
               $new_flag = "D";
           }
           if ($datetime >= 20060510.1925 && $datetime <= 20060510.2040) {
               $new_flag = "D";
           }
           if ($datetime >= 20051109.2116 && $datetime <= 20051109.2145) {
               $new_flag = "D";
           }
           if ($datetime >= 20050427.1824 && $datetime <= 20050427.1915) {
               $new_flag = "D";
           }
        }
        if ($var eq "up_long_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "up_short_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "temp") {
           if ($datetime >= 20081007.2130 && $datetime <= 20081007.2155) {
               $new_flag = "D";
           }
           if ($datetime >= 20080423.1815 && $datetime <= 20080423.1920) {
               $new_flag = "D";
           }
           if ($datetime >= 20071024.1819 && $datetime <= 20071024.1906) {
               $new_flag = "D";
           }
           if ($datetime >= 20070425.1921 && $datetime <= 20070425.1948) {
               $new_flag = "D";
           }
           if ($datetime >= 20061025.1745 && $datetime <= 20061025.1825) {
               $new_flag = "D";
           }
           if ($datetime >= 20060510.1925 && $datetime <= 20060510.2040) {
               $new_flag = "D";
           }
           if ($datetime >= 20051109.2116 && $datetime <= 20051109.2145) {
               $new_flag = "D";
           }
           if ($datetime >= 20050427.1824 && $datetime <= 20050427.1915) {
               $new_flag = "D";
           }
        }
        if ($var eq "h") {
           if ($datetime >= 20060617.0900 && $datetime <= 20060620.1915) {
               $new_flag = "D";
           }
           if ($datetime >= 20050302.1900 && $datetime <= 20050330.1900) {
               $new_flag = "D";
           }
           if ($datetime >= 20051221.0930 && $datetime <= 20051221.1730) {
               $new_flag = "B";
           }
        }
        if ($var eq "c_shf1") {
           if ($datetime >= 20051221.0930 && $datetime <= 20051221.1730) {
               $new_flag = "B";
           }
        }
        if ($var eq "rh") {
           if ($datetime >= 20081007.2130 && $datetime <= 20081007.2155) {
               $new_flag = "D";
           }
           if ($datetime >= 20080423.1815 && $datetime <= 20080423.1920) {
               $new_flag = "D";
           }
           if ($datetime >= 20071024.1819 && $datetime <= 20071024.1906) {
               $new_flag = "D";
           }
           if ($datetime >= 20070425.1921 && $datetime <= 20070425.1948) {
               $new_flag = "D";
           }
           if ($datetime >= 20061025.1745 && $datetime <= 20061025.1825) {
               $new_flag = "D";
           }
           if ($datetime >= 20060510.1925 && $datetime <= 20060510.2040) {
               $new_flag = "D";
           }
           if ($datetime >= 20051109.2116 && $datetime <= 20051109.2145) {
               $new_flag = "D";
           }
           if ($datetime >= 20050427.1824 && $datetime <= 20050427.1915) {
               $new_flag = "D";
           }
        }
        if ($var eq "g1") {
           if ($datetime >= 20051221.0930 && $datetime <= 20051221.1730) {
               $new_flag = "B";
           }
        }
    #---------------------------------------------------------------------------
    # E21
    #---------------------------------------------------------------------------
    } elsif ($id eq "E21") {
        if ($var eq "wspd") {
           if ($datetime >= 20060524.1400 && $datetime <= 20060802.1320) {
               $new_flag = "D";
           }
           if ($datetime >= 20050203.0000 && $datetime <= 20050207.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20071210.0925 && $datetime <= 20071210.1304) {
               $new_flag = "B";
           }
           if ($datetime >= 20070113.2149 && $datetime <= 20070118.1932) {
               $new_flag = "B";
           }
           if ($datetime >= 20081007.1540 && $datetime <= 20081007.1605) {
               $new_flag = "B";
           }
           if ($datetime >= 20080423.1442 && $datetime <= 20080423.1520) {
               $new_flag = "B";
           }
           if ($datetime >= 20071024.1405 && $datetime <= 20071024.1500) {
               $new_flag = "B";
           }
           if ($datetime >= 20070425.1550 && $datetime <= 20070425.1645) {
               $new_flag = "B";
           }
           if ($datetime >= 20061024.2035 && $datetime <= 20061024.2105) {
               $new_flag = "B";
           }
           if ($datetime >= 20060510.1439 && $datetime <= 20060510.1515) {
               $new_flag = "B";
           }
           if ($datetime >= 20051109.1526 && $datetime <= 20051109.1545) {
               $new_flag = "B";
           }
           if ($datetime >= 20050426.2055 && $datetime <= 20050426.2145) {
               $new_flag = "B";
           }
        }
        if ($var eq "wdir") {
           if ($datetime >= 20060524.1400 && $datetime <= 20060802.1320) {
               $new_flag = "D";
           }
           if ($datetime >= 20050203.0000 && $datetime <= 20050207.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20071210.0925 && $datetime <= 20071210.1304) {
               $new_flag = "B";
           }
           if ($datetime >= 20070113.2149 && $datetime <= 20070118.1932) {
               $new_flag = "B";
           }
           if ($datetime >= 20081007.1540 && $datetime <= 20081007.1605) {
               $new_flag = "B";
           }
           if ($datetime >= 20080423.1442 && $datetime <= 20080423.1520) {
               $new_flag = "B";
           }
           if ($datetime >= 20071024.1405 && $datetime <= 20071024.1500) {
               $new_flag = "B";
           }
           if ($datetime >= 20070425.1550 && $datetime <= 20070425.1645) {
               $new_flag = "B";
           }
           if ($datetime >= 20061024.2035 && $datetime <= 20061024.2105) {
               $new_flag = "B";
           }
           if ($datetime >= 20060510.1439 && $datetime <= 20060510.1515) {
               $new_flag = "B";
           }
           if ($datetime >= 20051109.1526 && $datetime <= 20051109.1545) {
               $new_flag = "B";
           }
           if ($datetime >= 20050426.2055 && $datetime <= 20050426.2145) {
               $new_flag = "B";
           }
        }
        if ($var eq "down_long_hemisp_shaded") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "down_short_hemisp") {
           if ($datetime >= 20070801.0000 && $datetime <= 20080701.1500) {
               $new_flag = "D";
           }
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "precip") {
           if ($datetime >= 20080131.0000 && $datetime <= 20080202.2000) {
               $new_flag = "D";
           }
        }
        if ($var eq "bar_pres") {
           if ($datetime >= 20081007.1540 && $datetime <= 20081007.1605) {
               $new_flag = "D";
           }
           if ($datetime >= 20081007.1540 && $datetime <= 20081007.1605) {
               $new_flag = "D";
           }
           if ($datetime >= 20071024.1405 && $datetime <= 20071024.1500) {
               $new_flag = "D";
           }
           if ($datetime >= 20071024.1405 && $datetime <= 20071024.1500) {
               $new_flag = "D";
           }
           if ($datetime >= 20061024.2035 && $datetime <= 20061024.2105) {
               $new_flag = "D";
           }
           if ($datetime >= 20060510.1439 && $datetime <= 20060510.1515) {
               $new_flag = "D";
           }
           if ($datetime >= 20051109.1526 && $datetime <= 20051109.1545) {
               $new_flag = "D";
           }
           if ($datetime >= 20050426.2055 && $datetime <= 20050426.2145) {
               $new_flag = "D";
           }
           if ($datetime >= 20051001.2103 && $datetime <= 20051018.1435) {
               $new_flag = "B";
           }
        }
        if ($var eq "up_long_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "up_short_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "temp") {
           if ($datetime >= 20081007.1540 && $datetime <= 20081007.1605) {
               $new_flag = "D";
           }
           if ($datetime >= 20080423.1442 && $datetime <= 20080423.1520) {
               $new_flag = "D";
           }
           if ($datetime >= 20071024.1405 && $datetime <= 20071024.1500) {
               $new_flag = "D";
           }
           if ($datetime >= 20070425.1550 && $datetime <= 20070425.1645) {
               $new_flag = "D";
           }
           if ($datetime >= 20061024.2035 && $datetime <= 20061024.2105) {
               $new_flag = "D";
           }
           if ($datetime >= 20060510.1439 && $datetime <= 20060510.1515) {
               $new_flag = "D";
           }
           if ($datetime >= 20051109.1526 && $datetime <= 20051109.1545) {
               $new_flag = "D";
           }
           if ($datetime >= 20050426.2055 && $datetime <= 20050426.2145) {
               $new_flag = "D";
           }
           if ($datetime >= 20070322.1348 && $datetime <= 20070328.1541) {
               $new_flag = "B";
           }
        }
        if ($var eq "fc") {
           if ($datetime >= 20060413.1230 && $datetime <= 20060425.1500) {
               $new_flag = "B";
           }
        }
        if ($var eq "rh") {
           if ($datetime >= 20050821.1601 && $datetime <= 20050822.0742) {
               $new_flag = "D";
           }
           if ($datetime >= 20050806.1221 && $datetime <= 20050817.0309) {
               $new_flag = "D";
           }
           if ($datetime >= 20081007.1540 && $datetime <= 20081007.1605) {
               $new_flag = "D";
           }
           if ($datetime >= 20080423.1442 && $datetime <= 20080423.1520) {
               $new_flag = "D";
           }
           if ($datetime >= 20071024.1405 && $datetime <= 20071024.1500) {
               $new_flag = "D";
           }
           if ($datetime >= 20070425.1550 && $datetime <= 20070425.1645) {
               $new_flag = "D";
           }
           if ($datetime >= 20061024.2035 && $datetime <= 20061024.2105) {
               $new_flag = "D";
           }
           if ($datetime >= 20060510.1439 && $datetime <= 20060510.1515) {
               $new_flag = "D";
           }
           if ($datetime >= 20051109.1526 && $datetime <= 20051109.1545) {
               $new_flag = "D";
           }
           if ($datetime >= 20050426.2055 && $datetime <= 20050426.2145) {
               $new_flag = "D";
           }
           if ($datetime >= 20070322.1348 && $datetime <= 20070328.1541) {
               $new_flag = "B";
           }
           if ($datetime >= 20060104.1535 && $datetime <= 20060215.1600) {
               $new_flag = "B";
           }
           if ($datetime >= 20050817.0310 && $datetime <= 20050821.1600) {
               $new_flag = "B";
           }
        }
        if ($var eq "lv_e") {
           if ($datetime >= 20060413.1230 && $datetime <= 20060425.1500) {
               $new_flag = "B";
           }
        }
    #---------------------------------------------------------------------------
    # E22
    #---------------------------------------------------------------------------
    } elsif ($id eq "E22") {
        if ($var eq "e") {
           if ($datetime >= 20060821.0630 && $datetime <= 20060830.1800) {
               $new_flag = "D";
           }
           if ($datetime >= 20050316.1830 && $datetime <= 20050330.1930) {
               $new_flag = "D";
           }
           if ($datetime >= 20050827.2000 && $datetime <= 20050831.1730) {
               $new_flag = "D";
           }
           if ($datetime >= 20050306.1400 && $datetime <= 20050316.1900) {
               $new_flag = "D";
           }
           if ($datetime >= 20060110.0730 && $datetime <= 20060118.2000) {
               $new_flag = "B";
           }
        }
        if ($var eq "down_long_hemisp_shaded") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "up_short_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "up_long_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "h") {
           if ($datetime >= 20060821.0630 && $datetime <= 20060830.1800) {
               $new_flag = "D";
           }
           if ($datetime >= 20050316.1830 && $datetime <= 20050330.1930) {
               $new_flag = "D";
           }
           if ($datetime >= 20050827.2000 && $datetime <= 20050831.1730) {
               $new_flag = "D";
           }
           if ($datetime >= 20060110.0730 && $datetime <= 20060118.2000) {
               $new_flag = "B";
           }
        }
        if ($var eq "down_short_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
    #---------------------------------------------------------------------------
    # E24
    #---------------------------------------------------------------------------
    } elsif ($id eq "E24") {
        if ($var eq "wspd") {
           if ($datetime >= 20050106.0000 && $datetime <= 20050204.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20071209.2311 && $datetime <= 20071210.1804) {
               $new_flag = "B";
           }
           if ($datetime >= 20070117.0019 && $datetime <= 20070117.0821) {
               $new_flag = "B";
           }
           if ($datetime >= 20070116.0530 && $datetime <= 20070116.1209) {
               $new_flag = "B";
           }
           if ($datetime >= 20070112.1445 && $datetime <= 20070115.2137) {
               $new_flag = "B";
           }
           if ($datetime >= 20081008.1650 && $datetime <= 20081008.1726) {
               $new_flag = "B";
           }
           if ($datetime >= 20080424.1543 && $datetime <= 20080424.1655) {
               $new_flag = "B";
           }
           if ($datetime >= 20071025.1441 && $datetime <= 20071025.1515) {
               $new_flag = "B";
           }
           if ($datetime >= 20070426.1530 && $datetime <= 20070426.1605) {
               $new_flag = "B";
           }
           if ($datetime >= 20061026.1605 && $datetime <= 20061026.1640) {
               $new_flag = "B";
           }
           if ($datetime >= 20060511.1640 && $datetime <= 20060511.1720) {
               $new_flag = "B";
           }
           if ($datetime >= 20051110.1700 && $datetime <= 20051110.1800) {
               $new_flag = "B";
           }
           if ($datetime >= 20050428.1633 && $datetime <= 20050428.1715) {
               $new_flag = "B";
           }
        }
        if ($var eq "wdir") {
           if ($datetime >= 20050106.0000 && $datetime <= 20050204.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20071209.2311 && $datetime <= 20071210.1804) {
               $new_flag = "B";
           }
           if ($datetime >= 20070117.0019 && $datetime <= 20070117.0821) {
               $new_flag = "B";
           }
           if ($datetime >= 20070116.0530 && $datetime <= 20070116.1209) {
               $new_flag = "B";
           }
           if ($datetime >= 20070112.1445 && $datetime <= 20070115.2137) {
               $new_flag = "B";
           }
           if ($datetime >= 20081008.1650 && $datetime <= 20081008.1726) {
               $new_flag = "B";
           }
           if ($datetime >= 20080424.1543 && $datetime <= 20080424.1655) {
               $new_flag = "B";
           }
           if ($datetime >= 20071025.1441 && $datetime <= 20071025.1515) {
               $new_flag = "B";
           }
           if ($datetime >= 20070426.1530 && $datetime <= 20070426.1605) {
               $new_flag = "B";
           }
           if ($datetime >= 20061026.1605 && $datetime <= 20061026.1640) {
               $new_flag = "B";
           }
           if ($datetime >= 20060511.1640 && $datetime <= 20060511.1720) {
               $new_flag = "B";
           }
           if ($datetime >= 20051110.1700 && $datetime <= 20051110.1800) {
               $new_flag = "B";
           }
           if ($datetime >= 20050428.1633 && $datetime <= 20050428.1715) {
               $new_flag = "B";
           }
        }
        if ($var eq "down_long_hemisp_shaded") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "down_short_hemisp") {
           if ($datetime >= 20050407.0000 && $datetime <= 20050414.1610) {
               $new_flag = "D";
           }
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "precip") {
           if ($datetime >= 20080131.0000 && $datetime <= 20080202.2000) {
               $new_flag = "D";
           }
        }
        if ($var eq "bar_pres") {
           if ($datetime >= 20081008.1650 && $datetime <= 20081008.1726) {
               $new_flag = "D";
           }
           if ($datetime >= 20080424.1543 && $datetime <= 20080424.1655) {
               $new_flag = "D";
           }
           if ($datetime >= 20071025.1441 && $datetime <= 20071025.1515) {
               $new_flag = "D";
           }
           if ($datetime >= 20070426.1530 && $datetime <= 20070426.1605) {
               $new_flag = "D";
           }
           if ($datetime >= 20061026.1605 && $datetime <= 20061026.1640) {
               $new_flag = "D";
           }
           if ($datetime >= 20060511.1640 && $datetime <= 20060511.1720) {
               $new_flag = "D";
           }
           if ($datetime >= 20051110.1700 && $datetime <= 20051110.1800) {
               $new_flag = "D";
           }
           if ($datetime >= 20050428.1633 && $datetime <= 20050428.1715) {
               $new_flag = "D";
           }
           if ($datetime >= 20070511.2012 && $datetime <= 20070524.1604) {
               $new_flag = "B";
           }
        }
        if ($var eq "up_long_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "up_short_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "temp") {
           if ($datetime >= 20081008.1650 && $datetime <= 20081008.1726) {
               $new_flag = "D";
           }
           if ($datetime >= 20080424.1543 && $datetime <= 20080424.1655) {
               $new_flag = "D";
           }
           if ($datetime >= 20071025.1441 && $datetime <= 20071025.1515) {
               $new_flag = "D";
           }
           if ($datetime >= 20070426.1530 && $datetime <= 20070426.1605) {
               $new_flag = "D";
           }
           if ($datetime >= 20061026.1605 && $datetime <= 20061026.1640) {
               $new_flag = "D";
           }
           if ($datetime >= 20060511.1640 && $datetime <= 20060511.1720) {
               $new_flag = "D";
           }
           if ($datetime >= 20051110.1700 && $datetime <= 20051110.1800) {
               $new_flag = "D";
           }
           if ($datetime >= 20050428.1633 && $datetime <= 20050428.1715) {
               $new_flag = "D";
           }
        }
        if ($var eq "fc") {
           if ($datetime >= 20050824.2330 && $datetime <= 20050829.0300) {
               $new_flag = "B";
           }
           if ($datetime >= 20050622.0900 && $datetime <= 20050624.1500) {
               $new_flag = "B";
           }
        }
        if ($var eq "rh") {
           if ($datetime >= 20081008.1650 && $datetime <= 20081008.1726) {
               $new_flag = "D";
           }
           if ($datetime >= 20080424.1543 && $datetime <= 20080424.1655) {
               $new_flag = "D";
           }
           if ($datetime >= 20071025.1441 && $datetime <= 20071025.1515) {
               $new_flag = "D";
           }
           if ($datetime >= 20070426.1530 && $datetime <= 20070426.1605) {
               $new_flag = "D";
           }
           if ($datetime >= 20061026.1605 && $datetime <= 20061026.1640) {
               $new_flag = "D";
           }
           if ($datetime >= 20060511.1640 && $datetime <= 20060511.1720) {
               $new_flag = "D";
           }
           if ($datetime >= 20051110.1700 && $datetime <= 20051110.1800) {
               $new_flag = "D";
           }
           if ($datetime >= 20050428.1633 && $datetime <= 20050428.1715) {
               $new_flag = "D";
           }
        }
        if ($var eq "lv_e") {
           if ($datetime >= 20080301.0000 && $datetime <= 20080412.0230) {
               $new_flag = "D";
           }
           if ($datetime >= 20050824.2330 && $datetime <= 20050829.0300) {
               $new_flag = "B";
           }
           if ($datetime >= 20050622.0900 && $datetime <= 20050624.1500) {
               $new_flag = "B";
           }
        }
    #---------------------------------------------------------------------------
    # E25
    #---------------------------------------------------------------------------
    } elsif ($id eq "E25") {
        if ($var eq "down_long_hemisp_shaded") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "up_short_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "up_long_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "down_short_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
    #---------------------------------------------------------------------------
    # E26
    #---------------------------------------------------------------------------
    } elsif ($id eq "E26") {
        if ($var eq "e") {
           if ($datetime >= 20060221.0400 && $datetime <= 20060225.1700) {
               $new_flag = "D";
           }
           if ($datetime >= 20051220.0800 && $datetime <= 20051222.1500) {
               $new_flag = "D";
           }
           if ($datetime >= 20050303.1800 && $datetime <= 20050331.1700) {
               $new_flag = "D";
           }
           if ($datetime >= 20030110.1700 && $datetime <= 20050303.1600) {
               $new_flag = "D";
           }
        }
        if ($var eq "h") {
           if ($datetime >= 20060221.0400 && $datetime <= 20060225.1700) {
               $new_flag = "D";
           }
           if ($datetime >= 20051220.0800 && $datetime <= 20051222.1500) {
               $new_flag = "D";
           }
           if ($datetime >= 20050303.1800 && $datetime <= 20050331.1700) {
               $new_flag = "D";
           }
           if ($datetime >= 20030110.1700 && $datetime <= 20050303.1600) {
               $new_flag = "D";
           }
        }
        if ($var eq "c_shf1") {
           if ($datetime >= 20060221.0400 && $datetime <= 20060225.1700) {
               $new_flag = "D";
           }
           if ($datetime >= 20051220.0800 && $datetime <= 20051222.1500) {
               $new_flag = "D";
           }
           if ($datetime >= 20030110.1700 && $datetime <= 20050303.1600) {
               $new_flag = "D";
           }
        }
        if ($var eq "g1") {
           if ($datetime >= 20060221.0400 && $datetime <= 20060225.1700) {
               $new_flag = "D";
           }
           if ($datetime >= 20051220.0800 && $datetime <= 20051222.1500) {
               $new_flag = "D";
           }
           if ($datetime >= 20030110.1700 && $datetime <= 20050303.1600) {
               $new_flag = "D";
           }
        }
    #---------------------------------------------------------------------------
    # E27
    #---------------------------------------------------------------------------
    } elsif ($id eq "E27") {
        if ($var eq "wspd") {
           if ($datetime >= 20090127.1029 && $datetime <= 20090129.2359) {
               $new_flag = "B";
           }
           if ($datetime >= 20071210.0644 && $datetime <= 20071211.0937) {
               $new_flag = "B";
           }
           if ($datetime >= 20070112.1534 && $datetime <= 20070113.0604) {
               $new_flag = "B";
           }
           if ($datetime >= 20081007.1925 && $datetime <= 20081007.1955) {
               $new_flag = "B";
           }
           if ($datetime >= 20080423.1705 && $datetime <= 20080423.1735) {
               $new_flag = "B";
           }
           if ($datetime >= 20071024.1647 && $datetime <= 20071024.1715) {
               $new_flag = "B";
           }
           if ($datetime >= 20070427.1603 && $datetime <= 20070427.1645) {
               $new_flag = "B";
           }
           if ($datetime >= 20061025.1542 && $datetime <= 20061025.1625) {
               $new_flag = "B";
           }
           if ($datetime >= 20060510.1650 && $datetime <= 20060510.1725) {
               $new_flag = "B";
           }
           if ($datetime >= 20051109.1827 && $datetime <= 20051109.1930) {
               $new_flag = "B";
           }
           if ($datetime >= 20050427.1516 && $datetime <= 20050427.1605) {
               $new_flag = "B";
           }
        }
        if ($var eq "e") {
           if ($datetime >= 20070807.2130 && $datetime <= 20070813.1600) {
               $new_flag = "D";
           }
           if ($datetime >= 20060811.1300 && $datetime <= 20060821.2330) {
               $new_flag = "D";
           }
           if ($datetime >= 20060426.1830 && $datetime <= 20060817.1730) {
               $new_flag = "D";
           }
           if ($datetime >= 20060614.0500 && $datetime <= 20060620.1730) {
               $new_flag = "D";
           }
           if ($datetime >= 20060531.0600 && $datetime <= 20060607.1730) {
               $new_flag = "D";
           }
           if ($datetime >= 20050302.1600 && $datetime <= 20050330.1730) {
               $new_flag = "D";
           }
           if ($datetime >= 20050817.1630 && $datetime <= 20050827.1930) {
               $new_flag = "D";
           }
           if ($datetime >= 20050805.2200 && $datetime <= 20050817.1600) {
               $new_flag = "D";
           }
           if ($datetime >= 20060711.0130 && $datetime <= 20060721.1730) {
               $new_flag = "B";
           }
           if ($datetime >= 20060218.0330 && $datetime <= 20060301.1615) {
               $new_flag = "B";
           }
        }
        if ($var eq "wdir") {
           if ($datetime >= 20090226.1854 && $datetime <= 20090520.1550) {
               $new_flag = "D";
           }
           if ($datetime >= 20090211.0000 && $datetime <= 20090224.1525) {
               $new_flag = "D";
           }
           if ($datetime >= 20090127.1029 && $datetime <= 20090210.2359) {
               $new_flag = "B";
           }
           if ($datetime >= 20071210.0644 && $datetime <= 20071211.0937) {
               $new_flag = "B";
           }
           if ($datetime >= 20071010.1624 && $datetime <= 20071011.1425) {
               $new_flag = "B";
           }
           if ($datetime >= 20070112.1534 && $datetime <= 20070113.0604) {
               $new_flag = "B";
           }
           if ($datetime >= 20051209.1904 && $datetime <= 20051213.1456) {
               $new_flag = "B";
           }
           if ($datetime >= 20081007.1925 && $datetime <= 20081007.1955) {
               $new_flag = "B";
           }
           if ($datetime >= 20080423.1705 && $datetime <= 20080423.1735) {
               $new_flag = "B";
           }
           if ($datetime >= 20071024.1647 && $datetime <= 20071024.1715) {
               $new_flag = "B";
           }
           if ($datetime >= 20070427.1603 && $datetime <= 20070427.1645) {
               $new_flag = "B";
           }
           if ($datetime >= 20061025.1542 && $datetime <= 20061025.1625) {
               $new_flag = "B";
           }
           if ($datetime >= 20060510.1650 && $datetime <= 20060510.1725) {
               $new_flag = "B";
           }
           if ($datetime >= 20051109.1827 && $datetime <= 20051109.1930) {
               $new_flag = "B";
           }
           if ($datetime >= 20050427.1516 && $datetime <= 20050427.1605) {
               $new_flag = "B";
           }
        }
        if ($var eq "down_long_hemisp_shaded") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "down_short_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "precip") {
           if ($datetime >= 20080131.0000 && $datetime <= 20080202.2000) {
               $new_flag = "D";
           }
           if ($datetime >= 20060426.1636 && $datetime <= 20060509.1537) {
               $new_flag = "D";
           }
           if ($datetime >= 20060509.1537 && $datetime <= 20060509.1538) {
               $new_flag = "B";
           }
        }
        if ($var eq "bar_pres") {
           if ($datetime >= 20081007.1925 && $datetime <= 20081007.1955) {
               $new_flag = "D";
           }
           if ($datetime >= 20080423.1705 && $datetime <= 20080423.1735) {
               $new_flag = "D";
           }
           if ($datetime >= 20071024.1647 && $datetime <= 20071024.1715) {
               $new_flag = "D";
           }
           if ($datetime >= 20070427.1603 && $datetime <= 20070427.1645) {
               $new_flag = "D";
           }
           if ($datetime >= 20061025.1542 && $datetime <= 20061025.1625) {
               $new_flag = "D";
           }
           if ($datetime >= 20060510.1650 && $datetime <= 20060510.1725) {
               $new_flag = "D";
           }
           if ($datetime >= 20051109.1827 && $datetime <= 20051109.1930) {
               $new_flag = "D";
           }
           if ($datetime >= 20050427.1516 && $datetime <= 20050427.1605) {
               $new_flag = "D";
           }
        }
        if ($var eq "up_long_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "up_short_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "temp") {
           if ($datetime >= 20081007.1925 && $datetime <= 20081007.1955) {
               $new_flag = "D";
           }
           if ($datetime >= 20080423.1705 && $datetime <= 20080423.1735) {
               $new_flag = "D";
           }
           if ($datetime >= 20071024.1647 && $datetime <= 20071024.1715) {
               $new_flag = "D";
           }
           if ($datetime >= 20070427.1603 && $datetime <= 20070427.1645) {
               $new_flag = "D";
           }
           if ($datetime >= 20061025.1542 && $datetime <= 20061025.1625) {
               $new_flag = "D";
           }
           if ($datetime >= 20060510.1650 && $datetime <= 20060510.1725) {
               $new_flag = "D";
           }
           if ($datetime >= 20051109.1827 && $datetime <= 20051109.1930) {
               $new_flag = "D";
           }
           if ($datetime >= 20050427.1516 && $datetime <= 20050427.1605) {
               $new_flag = "D";
           }
        }
        if ($var eq "h") {
           if ($datetime >= 20070807.2130 && $datetime <= 20070813.1600) {
               $new_flag = "D";
           }
           if ($datetime >= 20060811.1300 && $datetime <= 20060821.2330) {
               $new_flag = "D";
           }
           if ($datetime >= 20060426.1830 && $datetime <= 20060817.1730) {
               $new_flag = "D";
           }
           if ($datetime >= 20060614.0500 && $datetime <= 20060620.1730) {
               $new_flag = "D";
           }
           if ($datetime >= 20060531.0600 && $datetime <= 20060607.1730) {
               $new_flag = "D";
           }
           if ($datetime >= 20050302.1600 && $datetime <= 20050330.1730) {
               $new_flag = "D";
           }
           if ($datetime >= 20050817.1630 && $datetime <= 20050827.1930) {
               $new_flag = "D";
           }
           if ($datetime >= 20050805.2200 && $datetime <= 20050817.1600) {
               $new_flag = "D";
           }
           if ($datetime >= 20060711.0130 && $datetime <= 20060721.1730) {
               $new_flag = "B";
           }
           if ($datetime >= 20060218.0330 && $datetime <= 20060301.1615) {
               $new_flag = "B";
           }
        }
        if ($var eq "c_shf1") {
           if ($datetime >= 20070807.2130 && $datetime <= 20070813.1600) {
               $new_flag = "D";
           }
           if ($datetime >= 20060811.1300 && $datetime <= 20060821.2330) {
               $new_flag = "D";
           }
           if ($datetime >= 20060426.1830 && $datetime <= 20060817.1730) {
               $new_flag = "D";
           }
           if ($datetime >= 20060614.0500 && $datetime <= 20060620.1730) {
               $new_flag = "D";
           }
        }
        if ($var eq "rh") {
           if ($datetime >= 20081007.1925 && $datetime <= 20081007.1955) {
               $new_flag = "D";
           }
           if ($datetime >= 20080423.1705 && $datetime <= 20080423.1735) {
               $new_flag = "D";
           }
           if ($datetime >= 20071024.1647 && $datetime <= 20071024.1715) {
               $new_flag = "D";
           }
           if ($datetime >= 20070427.1603 && $datetime <= 20070427.1645) {
               $new_flag = "D";
           }
           if ($datetime >= 20061025.1542 && $datetime <= 20061025.1625) {
               $new_flag = "D";
           }
           if ($datetime >= 20060510.1650 && $datetime <= 20060510.1725) {
               $new_flag = "D";
           }
           if ($datetime >= 20051109.1827 && $datetime <= 20051109.1930) {
               $new_flag = "D";
           }
           if ($datetime >= 20050427.1516 && $datetime <= 20050427.1605) {
               $new_flag = "D";
           }
        }
        if ($var eq "g1") {
           if ($datetime >= 20070807.2130 && $datetime <= 20070813.1600) {
               $new_flag = "D";
           }
           if ($datetime >= 20060811.1300 && $datetime <= 20060821.2330) {
               $new_flag = "D";
           }
           if ($datetime >= 20060426.1830 && $datetime <= 20060817.1730) {
               $new_flag = "D";
           }
           if ($datetime >= 20060614.0500 && $datetime <= 20060620.1730) {
               $new_flag = "D";
           }
        }
    #---------------------------------------------------------------------------
    # E3
    #---------------------------------------------------------------------------
    } elsif ($id eq "E3") {
        if ($var eq "wspd") {
           if ($datetime >= 20050103.0000 && $datetime <= 20050131.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20071211.0101 && $datetime <= 20071211.1959) {
               $new_flag = "B";
           }
           if ($datetime >= 20050107.0845 && $datetime <= 20050109.0915) {
               $new_flag = "B";
           }
           if ($datetime >= 20050106.1515 && $datetime <= 20050106.2115) {
               $new_flag = "B";
           }
           if ($datetime >= 20081029.1540 && $datetime <= 20081029.1620) {
               $new_flag = "B";
           }
           if ($datetime >= 20080416.1700 && $datetime <= 20080416.1745) {
               $new_flag = "B";
           }
           if ($datetime >= 20071031.1430 && $datetime <= 20071031.1458) {
               $new_flag = "B";
           }
           if ($datetime >= 20070418.1711 && $datetime <= 20070418.1745) {
               $new_flag = "B";
           }
           if ($datetime >= 20061018.1735 && $datetime <= 20061018.1815) {
               $new_flag = "B";
           }
           if ($datetime >= 20060503.1545 && $datetime <= 20060503.1630) {
               $new_flag = "B";
           }
           if ($datetime >= 20051102.1641 && $datetime <= 20051102.1835) {
               $new_flag = "B";
           }
           if ($datetime >= 20050420.1714 && $datetime <= 20050420.1800) {
               $new_flag = "B";
           }
        }
        if ($var eq "wdir") {
           if ($datetime >= 20050103.0000 && $datetime <= 20050131.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20071211.0101 && $datetime <= 20071211.1959) {
               $new_flag = "B";
           }
           if ($datetime >= 20050107.0845 && $datetime <= 20050109.0915) {
               $new_flag = "B";
           }
           if ($datetime >= 20050106.1515 && $datetime <= 20050106.2115) {
               $new_flag = "B";
           }
           if ($datetime >= 20081029.1540 && $datetime <= 20081029.1620) {
               $new_flag = "B";
           }
           if ($datetime >= 20080416.1700 && $datetime <= 20080416.1745) {
               $new_flag = "B";
           }
           if ($datetime >= 20071031.1430 && $datetime <= 20071031.1458) {
               $new_flag = "B";
           }
           if ($datetime >= 20070418.1711 && $datetime <= 20070418.1745) {
               $new_flag = "B";
           }
           if ($datetime >= 20061018.1735 && $datetime <= 20061018.1815) {
               $new_flag = "B";
           }
           if ($datetime >= 20060503.1545 && $datetime <= 20060503.1630) {
               $new_flag = "B";
           }
           if ($datetime >= 20051102.1641 && $datetime <= 20051102.1835) {
               $new_flag = "B";
           }
           if ($datetime >= 20050420.1714 && $datetime <= 20050420.1800) {
               $new_flag = "B";
           }
        }
        if ($var eq "down_long_hemisp_shaded") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20060614.1612 && $datetime <= 20060712.1830) {
               $new_flag = "B";
           }
        }
        if ($var eq "down_short_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20061023.0000 && $datetime <= 20070418.1820) {
               $new_flag = "D";
           }
           if ($datetime >= 20060614.1612 && $datetime <= 20060712.1830) {
               $new_flag = "B";
           }
        }
        if ($var eq "precip") {
           if ($datetime >= 20080131.0000 && $datetime <= 20080202.2000) {
               $new_flag = "D";
           }
           if ($datetime >= 20060628.1710 && $datetime <= 20060712.1820) {
               $new_flag = "D";
           }
        }
        if ($var eq "bar_pres") {
           if ($datetime >= 20081029.1540 && $datetime <= 20081029.1620) {
               $new_flag = "D";
           }
           if ($datetime >= 20080416.1700 && $datetime <= 20080416.1745) {
               $new_flag = "D";
           }
           if ($datetime >= 20071031.1430 && $datetime <= 20071031.1458) {
               $new_flag = "D";
           }
           if ($datetime >= 20070418.1711 && $datetime <= 20070418.1745) {
               $new_flag = "D";
           }
           if ($datetime >= 20061018.1735 && $datetime <= 20061018.1815) {
               $new_flag = "D";
           }
           if ($datetime >= 20060503.1545 && $datetime <= 20060503.1630) {
               $new_flag = "D";
           }
           if ($datetime >= 20051102.1641 && $datetime <= 20051102.1835) {
               $new_flag = "D";
           }
           if ($datetime >= 20050420.1714 && $datetime <= 20050420.1800) {
               $new_flag = "D";
           }
        }
        if ($var eq "up_long_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20060614.1612 && $datetime <= 20060712.1830) {
               $new_flag = "B";
           }
        }
        if ($var eq "up_short_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20060614.1612 && $datetime <= 20060712.1830) {
               $new_flag = "B";
           }
        }
        if ($var eq "temp") {
           if ($datetime >= 20081029.1540 && $datetime <= 20081029.1620) {
               $new_flag = "D";
           }
           if ($datetime >= 20080416.1700 && $datetime <= 20080416.1745) {
               $new_flag = "D";
           }
           if ($datetime >= 20071031.1430 && $datetime <= 20071031.1458) {
               $new_flag = "D";
           }
           if ($datetime >= 20070418.1711 && $datetime <= 20070418.1745) {
               $new_flag = "D";
           }
           if ($datetime >= 20061018.1735 && $datetime <= 20061018.1815) {
               $new_flag = "D";
           }
           if ($datetime >= 20060503.1545 && $datetime <= 20060503.1630) {
               $new_flag = "D";
           }
           if ($datetime >= 20051102.1641 && $datetime <= 20051102.1835) {
               $new_flag = "D";
           }
           if ($datetime >= 20050420.1714 && $datetime <= 20050420.1800) {
               $new_flag = "D";
           }
        }
        if ($var eq "fc") {
           if ($datetime >= 20050929.1100 && $datetime <= 20060208.1700) {
               $new_flag = "B";
           }
        }
        if ($var eq "rh") {
           if ($datetime >= 20081029.1540 && $datetime <= 20081029.1620) {
               $new_flag = "D";
           }
           if ($datetime >= 20080416.1700 && $datetime <= 20080416.1745) {
               $new_flag = "D";
           }
           if ($datetime >= 20071031.1430 && $datetime <= 20071031.1458) {
               $new_flag = "D";
           }
           if ($datetime >= 20070418.1711 && $datetime <= 20070418.1745) {
               $new_flag = "D";
           }
           if ($datetime >= 20061018.1735 && $datetime <= 20061018.1815) {
               $new_flag = "D";
           }
           if ($datetime >= 20060503.1545 && $datetime <= 20060503.1630) {
               $new_flag = "D";
           }
           if ($datetime >= 20051102.1641 && $datetime <= 20051102.1835) {
               $new_flag = "D";
           }
           if ($datetime >= 20050420.1714 && $datetime <= 20050420.1800) {
               $new_flag = "D";
           }
        }
        if ($var eq "lv_e") {
           if ($datetime >= 20050929.1100 && $datetime <= 20060208.1700) {
               $new_flag = "B";
           }
        }
    #---------------------------------------------------------------------------
    # E4
    #---------------------------------------------------------------------------
    } elsif ($id eq "E4") {
        if ($var eq "wspd") {
           if ($datetime >= 20050103.0000 && $datetime <= 20050206.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20071210.2324 && $datetime <= 20071211.0559) {
               $new_flag = "B";
           }
           if ($datetime >= 20050103.1030 && $datetime <= 20050105.0202) {
               $new_flag = "B";
           }
           if ($datetime >= 20081015.1635 && $datetime <= 20081015.1715) {
               $new_flag = "B";
           }
           if ($datetime >= 20080403.1804 && $datetime <= 20080403.1841) {
               $new_flag = "B";
           }
           if ($datetime >= 20071017.1652 && $datetime <= 20071017.1727) {
               $new_flag = "B";
           }
           if ($datetime >= 20070404.1737 && $datetime <= 20070404.1809) {
               $new_flag = "B";
           }
           if ($datetime >= 20061004.1655 && $datetime <= 20061004.1745) {
               $new_flag = "B";
           }
           if ($datetime >= 20060419.1553 && $datetime <= 20060419.1625) {
               $new_flag = "B";
           }
           if ($datetime >= 20051117.1700 && $datetime <= 20051117.1729) {
               $new_flag = "B";
           }
           if ($datetime >= 20050406.1520 && $datetime <= 20050406.1600) {
               $new_flag = "B";
           }
        }
        if ($var eq "e") {
           if ($datetime >= 20051020.1315 && $datetime <= 20051102.0330) {
               $new_flag = "D";
           }
           if ($datetime >= 20050309.1730 && $datetime <= 20050323.1800) {
               $new_flag = "D";
           }
           if ($datetime >= 20080402.2000 && $datetime <= 20080515.1700) {
               $new_flag = "B";
           }
           if ($datetime >= 20051218.0400 && $datetime <= 20051228.1630) {
               $new_flag = "B";
           }
           if ($datetime >= 20050513.1130 && $datetime <= 20050513.1930) {
               $new_flag = "B";
           }
           if ($datetime >= 20050424.0830 && $datetime <= 20050601.1600) {
               $new_flag = "B";
           }
        }
        if ($var eq "wdir") {
           if ($datetime >= 20050103.0000 && $datetime <= 20050206.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20071210.2324 && $datetime <= 20071211.0559) {
               $new_flag = "B";
           }
           if ($datetime >= 20050103.1030 && $datetime <= 20050105.0202) {
               $new_flag = "B";
           }
           if ($datetime >= 20081015.1635 && $datetime <= 20081015.1715) {
               $new_flag = "B";
           }
           if ($datetime >= 20080403.1804 && $datetime <= 20080403.1841) {
               $new_flag = "B";
           }
           if ($datetime >= 20071017.1652 && $datetime <= 20071017.1727) {
               $new_flag = "B";
           }
           if ($datetime >= 20070404.1737 && $datetime <= 20070404.1809) {
               $new_flag = "B";
           }
           if ($datetime >= 20061004.1655 && $datetime <= 20061004.1745) {
               $new_flag = "B";
           }
           if ($datetime >= 20060419.1553 && $datetime <= 20060419.1625) {
               $new_flag = "B";
           }
           if ($datetime >= 20051117.1700 && $datetime <= 20051117.1729) {
               $new_flag = "B";
           }
           if ($datetime >= 20050406.1520 && $datetime <= 20050406.1600) {
               $new_flag = "B";
           }
        }
        if ($var eq "down_long_hemisp_shaded") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "down_short_hemisp") {
           if ($datetime >= 20080113.1400 && $datetime <= 20080123.1817) {
               $new_flag = "D";
           }
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20050716.1412 && $datetime <= 20050727.1600) {
               $new_flag = "D";
           }
        }
        if ($var eq "precip") {
           if ($datetime >= 20080131.0000 && $datetime <= 20080202.2000) {
               $new_flag = "D";
           }
        }
        if ($var eq "bar_pres") {
           if ($datetime >= 20081015.1635 && $datetime <= 20081015.1715) {
               $new_flag = "D";
           }
           if ($datetime >= 20080403.1804 && $datetime <= 20080403.1841) {
               $new_flag = "D";
           }
           if ($datetime >= 20071017.1652 && $datetime <= 20071017.1727) {
               $new_flag = "D";
           }
           if ($datetime >= 20070404.1737 && $datetime <= 20070404.1809) {
               $new_flag = "D";
           }
           if ($datetime >= 20061004.1655 && $datetime <= 20061004.1745) {
               $new_flag = "D";
           }
           if ($datetime >= 20060419.1553 && $datetime <= 20060419.1625) {
               $new_flag = "D";
           }
           if ($datetime >= 20051117.1700 && $datetime <= 20051117.1729) {
               $new_flag = "D";
           }
           if ($datetime >= 20050406.1520 && $datetime <= 20050406.1600) {
               $new_flag = "D";
           }
        }
        if ($var eq "up_long_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "up_short_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "temp") {
           if ($datetime >= 20081015.1635 && $datetime <= 20081015.1715) {
               $new_flag = "D";
           }
           if ($datetime >= 20080403.1804 && $datetime <= 20080403.1841) {
               $new_flag = "D";
           }
           if ($datetime >= 20071017.1652 && $datetime <= 20071017.1727) {
               $new_flag = "D";
           }
           if ($datetime >= 20070404.1737 && $datetime <= 20070404.1809) {
               $new_flag = "D";
           }
           if ($datetime >= 20061004.1655 && $datetime <= 20061004.1745) {
               $new_flag = "D";
           }
           if ($datetime >= 20060419.1553 && $datetime <= 20060419.1625) {
               $new_flag = "D";
           }
           if ($datetime >= 20051117.1700 && $datetime <= 20051117.1729) {
               $new_flag = "D";
           }
           if ($datetime >= 20050406.1520 && $datetime <= 20050406.1600) {
               $new_flag = "D";
           }
        }
        if ($var eq "h") {
           if ($datetime >= 20051020.1315 && $datetime <= 20051102.0330) {
               $new_flag = "D";
           }
           if ($datetime >= 20050309.1730 && $datetime <= 20050323.1800) {
               $new_flag = "D";
           }
           if ($datetime >= 20080402.2000 && $datetime <= 20080515.1700) {
               $new_flag = "B";
           }
           if ($datetime >= 20051218.0400 && $datetime <= 20051228.1630) {
               $new_flag = "B";
           }
           if ($datetime >= 20050513.1130 && $datetime <= 20050513.1930) {
               $new_flag = "B";
           }
           if ($datetime >= 20050424.0830 && $datetime <= 20050601.1600) {
               $new_flag = "B";
           }
        }
        if ($var eq "c_shf1") {
           if ($datetime >= 20050513.1130 && $datetime <= 20050513.1930) {
               $new_flag = "B";
           }
        }
        if ($var eq "rh") {
           if ($datetime >= 20081015.1635 && $datetime <= 20081015.1715) {
               $new_flag = "D";
           }
           if ($datetime >= 20080403.1804 && $datetime <= 20080403.1841) {
               $new_flag = "D";
           }
           if ($datetime >= 20071017.1652 && $datetime <= 20071017.1727) {
               $new_flag = "D";
           }
           if ($datetime >= 20070404.1737 && $datetime <= 20070404.1809) {
               $new_flag = "D";
           }
           if ($datetime >= 20061004.1655 && $datetime <= 20061004.1745) {
               $new_flag = "D";
           }
           if ($datetime >= 20060419.1553 && $datetime <= 20060419.1625) {
               $new_flag = "D";
           }
           if ($datetime >= 20051117.1700 && $datetime <= 20051117.1729) {
               $new_flag = "D";
           }
           if ($datetime >= 20050406.1520 && $datetime <= 20050406.1600) {
               $new_flag = "D";
           }
        }
        if ($var eq "g1") {
           if ($datetime >= 20050513.1130 && $datetime <= 20050513.1930) {
               $new_flag = "B";
           }
        }
    #---------------------------------------------------------------------------
    # E5
    #---------------------------------------------------------------------------
    } elsif ($id eq "E5") {
        if ($var eq "wspd") {
           if ($datetime >= 20050106.0000 && $datetime <= 20050201.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20071210.0245 && $datetime <= 20071213.2027) {
               $new_flag = "B";
           }
           if ($datetime >= 20050104.1900 && $datetime <= 20050104.2159) {
               $new_flag = "B";
           }
           if ($datetime >= 20081015.1505 && $datetime <= 20081015.1545) {
               $new_flag = "B";
           }
           if ($datetime >= 20080402.1636 && $datetime <= 20080402.1707) {
               $new_flag = "B";
           }
           if ($datetime >= 20071016.2002 && $datetime <= 20071016.2030) {
               $new_flag = "B";
           }
           if ($datetime >= 20070405.1633 && $datetime <= 20070405.1646) {
               $new_flag = "B";
           }
           if ($datetime >= 20061004.1845 && $datetime <= 20061004.2010) {
               $new_flag = "B";
           }
           if ($datetime >= 20060419.1845 && $datetime <= 20060419.1920) {
               $new_flag = "B";
           }
           if ($datetime >= 20051117.2045 && $datetime <= 20051117.2115) {
               $new_flag = "B";
           }
           if ($datetime >= 20050406.1905 && $datetime <= 20050406.1925) {
               $new_flag = "B";
           }
        }
        if ($var eq "wdir") {
           if ($datetime >= 20050106.0000 && $datetime <= 20050201.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20071210.0245 && $datetime <= 20071213.2027) {
               $new_flag = "B";
           }
           if ($datetime >= 20050104.1900 && $datetime <= 20050104.2159) {
               $new_flag = "B";
           }
           if ($datetime >= 20081015.1505 && $datetime <= 20081015.1545) {
               $new_flag = "B";
           }
           if ($datetime >= 20080402.1636 && $datetime <= 20080402.1707) {
               $new_flag = "B";
           }
           if ($datetime >= 20071016.2002 && $datetime <= 20071016.2030) {
               $new_flag = "B";
           }
           if ($datetime >= 20070405.1633 && $datetime <= 20070405.1646) {
               $new_flag = "B";
           }
           if ($datetime >= 20061004.1845 && $datetime <= 20061004.2010) {
               $new_flag = "B";
           }
           if ($datetime >= 20060419.1845 && $datetime <= 20060419.1920) {
               $new_flag = "B";
           }
           if ($datetime >= 20051117.2045 && $datetime <= 20051117.2115) {
               $new_flag = "B";
           }
           if ($datetime >= 20050406.1905 && $datetime <= 20050406.1925) {
               $new_flag = "B";
           }
        }
        if ($var eq "down_long_hemisp_shaded") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "down_short_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20070529.2006 && $datetime <= 20070531.1752) {
               $new_flag = "D";
           }
           if ($datetime >= 20070123.1500 && $datetime <= 20070130.0000) {
               $new_flag = "D";
           }
        }
        if ($var eq "precip") {
           if ($datetime >= 20080131.0000 && $datetime <= 20080202.2000) {
               $new_flag = "D";
           }
        }
        if ($var eq "bar_pres") {
           if ($datetime >= 20081015.1505 && $datetime <= 20081015.1545) {
               $new_flag = "D";
           }
           if ($datetime >= 20080402.1636 && $datetime <= 20080402.1707) {
               $new_flag = "D";
           }
           if ($datetime >= 20071016.2002 && $datetime <= 20071016.2030) {
               $new_flag = "D";
           }
           if ($datetime >= 20070405.1633 && $datetime <= 20070405.1646) {
               $new_flag = "D";
           }
           if ($datetime >= 20061004.1845 && $datetime <= 20061004.2010) {
               $new_flag = "D";
           }
           if ($datetime >= 20060419.1845 && $datetime <= 20060419.1920) {
               $new_flag = "D";
           }
           if ($datetime >= 20051117.2045 && $datetime <= 20051117.2115) {
               $new_flag = "D";
           }
           if ($datetime >= 20050406.1905 && $datetime <= 20050406.1925) {
               $new_flag = "D";
           }
        }
        if ($var eq "up_short_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20070529.2006 && $datetime <= 20070531.1752) {
               $new_flag = "D";
           }
        }
        if ($var eq "up_long_hemisp") {
           if ($datetime >= 20080625.1736 && $datetime <= 20080710.1530) {
               $new_flag = "D";
           }
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "temp") {
           if ($datetime >= 20081015.1505 && $datetime <= 20081015.1545) {
               $new_flag = "D";
           }
           if ($datetime >= 20080402.1636 && $datetime <= 20080402.1707) {
               $new_flag = "D";
           }
           if ($datetime >= 20071016.2002 && $datetime <= 20071016.2030) {
               $new_flag = "D";
           }
           if ($datetime >= 20070405.1633 && $datetime <= 20070405.1646) {
               $new_flag = "D";
           }
           if ($datetime >= 20061004.1845 && $datetime <= 20061004.2010) {
               $new_flag = "D";
           }
           if ($datetime >= 20060419.1845 && $datetime <= 20060419.1920) {
               $new_flag = "D";
           }
           if ($datetime >= 20051117.2045 && $datetime <= 20051117.2115) {
               $new_flag = "D";
           }
           if ($datetime >= 20050406.1905 && $datetime <= 20050406.1925) {
               $new_flag = "D";
           }
        }
        if ($var eq "h") {
           if ($datetime >= 20090424.1400 && $datetime <= 20090429.2230) {
               $new_flag = "B";
           }
           if ($datetime >= 20090406.0400 && $datetime <= 20090406.1730) {
               $new_flag = "B";
           }
           if ($datetime >= 20090216.0400 && $datetime <= 20090401.1730) {
               $new_flag = "B";
           }
           if ($datetime >= 20070418.1930 && $datetime <= 20070517.1815) {
               $new_flag = "B";
           }
           if ($datetime >= 20051120.1930 && $datetime <= 20051214.1930) {
               $new_flag = "B";
           }
        }
        if ($var eq "fc") {
           if ($datetime >= 20070616.0300 && $datetime <= 20071212.2030) {
               $new_flag = "D";
           }
           if ($datetime >= 20070418.1930 && $datetime <= 20070517.1815) {
               $new_flag = "B";
           }
           if ($datetime >= 20051120.1930 && $datetime <= 20051214.1930) {
               $new_flag = "B";
           }
           if ($datetime >= 20060614.2130 && $datetime <= 20060617.1030) {
               $new_flag = "B";
           }
        }
        if ($var eq "rh") {
           if ($datetime >= 20081015.1505 && $datetime <= 20081015.1545) {
               $new_flag = "D";
           }
           if ($datetime >= 20080402.1636 && $datetime <= 20080402.1707) {
               $new_flag = "D";
           }
           if ($datetime >= 20071016.2002 && $datetime <= 20071016.2030) {
               $new_flag = "D";
           }
           if ($datetime >= 20070405.1633 && $datetime <= 20070405.1646) {
               $new_flag = "D";
           }
           if ($datetime >= 20061004.1845 && $datetime <= 20061004.2010) {
               $new_flag = "D";
           }
           if ($datetime >= 20060419.1845 && $datetime <= 20060419.1920) {
               $new_flag = "D";
           }
           if ($datetime >= 20051117.2045 && $datetime <= 20051117.2115) {
               $new_flag = "D";
           }
           if ($datetime >= 20050406.1905 && $datetime <= 20050406.1925) {
               $new_flag = "D";
           }
        }
        if ($var eq "lv_e") {
           if ($datetime >= 20070616.0300 && $datetime <= 20071212.2030) {
               $new_flag = "D";
           }
           if ($datetime >= 20090424.1400 && $datetime <= 20090429.2230) {
               $new_flag = "B";
           }
           if ($datetime >= 20090406.0400 && $datetime <= 20090406.1730) {
               $new_flag = "B";
           }
           if ($datetime >= 20090216.0400 && $datetime <= 20090401.1730) {
               $new_flag = "B";
           }
           if ($datetime >= 20070418.1930 && $datetime <= 20070517.1815) {
               $new_flag = "B";
           }
           if ($datetime >= 20051120.1930 && $datetime <= 20051214.1930) {
               $new_flag = "B";
           }
           if ($datetime >= 20060614.2130 && $datetime <= 20060617.1030) {
               $new_flag = "B";
           }
        }
    #---------------------------------------------------------------------------
    # E6
    #---------------------------------------------------------------------------
    } elsif ($id eq "E6") {
        if ($var eq "wspd") {
           if ($datetime >= 20050104.0000 && $datetime <= 20050203.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20071212.1020 && $datetime <= 20071213.1855) {
               $new_flag = "B";
           }
           if ($datetime >= 20050104.1800 && $datetime <= 20050105.0200) {
               $new_flag = "B";
           }
           if ($datetime >= 20081029.1815 && $datetime <= 20081029.1845) {
               $new_flag = "B";
           }
           if ($datetime >= 20080417.1600 && $datetime <= 20080417.1650) {
               $new_flag = "B";
           }
           if ($datetime >= 20071101.1602 && $datetime <= 20071101.1625) {
               $new_flag = "B";
           }
           if ($datetime >= 20070419.1626 && $datetime <= 20070419.1710) {
               $new_flag = "B";
           }
           if ($datetime >= 20061019.1440 && $datetime <= 20061019.1520) {
               $new_flag = "B";
           }
           if ($datetime >= 20060504.1540 && $datetime <= 20060504.1605) {
               $new_flag = "B";
           }
           if ($datetime >= 20051103.1552 && $datetime <= 20051103.1720) {
               $new_flag = "B";
           }
           if ($datetime >= 20050421.1440 && $datetime <= 20050421.1525) {
               $new_flag = "B";
           }
        }
        if ($var eq "wdir") {
           if ($datetime >= 20050101.0000 && $datetime <= 20051103.1615) {
               $new_flag = "D";
           }
           if ($datetime >= 20050104.0000 && $datetime <= 20050203.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20071212.1020 && $datetime <= 20071213.1855) {
               $new_flag = "B";
           }
           if ($datetime >= 20050104.1800 && $datetime <= 20050105.0200) {
               $new_flag = "B";
           }
           if ($datetime >= 20081029.1815 && $datetime <= 20081029.1845) {
               $new_flag = "B";
           }
           if ($datetime >= 20080417.1600 && $datetime <= 20080417.1650) {
               $new_flag = "B";
           }
           if ($datetime >= 20071101.1602 && $datetime <= 20071101.1625) {
               $new_flag = "B";
           }
           if ($datetime >= 20070419.1626 && $datetime <= 20070419.1710) {
               $new_flag = "B";
           }
           if ($datetime >= 20061019.1440 && $datetime <= 20061019.1520) {
               $new_flag = "B";
           }
           if ($datetime >= 20060504.1540 && $datetime <= 20060504.1605) {
               $new_flag = "B";
           }
           if ($datetime >= 20051103.1552 && $datetime <= 20051103.1720) {
               $new_flag = "B";
           }
           if ($datetime >= 20050421.1440 && $datetime <= 20050421.1525) {
               $new_flag = "B";
           }
        }
        if ($var eq "down_long_hemisp_shaded") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20050113.1200 && $datetime <= 20050210.1700) {
               $new_flag = "D";
           }
        }
        if ($var eq "down_short_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20050113.1200 && $datetime <= 20050210.1700) {
               $new_flag = "D";
           }
        }
        if ($var eq "precip") {
           if ($datetime >= 20080131.0000 && $datetime <= 20080202.2000) {
               $new_flag = "D";
           }
           if ($datetime >= 20080912.0000 && $datetime <= 20081016.1545) {
               $new_flag = "B";
           }
        }
        if ($var eq "bar_pres") {
           if ($datetime >= 20081029.1815 && $datetime <= 20081029.1845) {
               $new_flag = "D";
           }
           if ($datetime >= 20080417.1600 && $datetime <= 20080417.1650) {
               $new_flag = "D";
           }
           if ($datetime >= 20071101.1602 && $datetime <= 20071101.1625) {
               $new_flag = "D";
           }
           if ($datetime >= 20070419.1626 && $datetime <= 20070419.1710) {
               $new_flag = "D";
           }
           if ($datetime >= 20061019.1440 && $datetime <= 20061019.1520) {
               $new_flag = "D";
           }
           if ($datetime >= 20060504.1540 && $datetime <= 20060504.1605) {
               $new_flag = "D";
           }
           if ($datetime >= 20051103.1552 && $datetime <= 20051103.1720) {
               $new_flag = "D";
           }
           if ($datetime >= 20050421.1440 && $datetime <= 20050421.1525) {
               $new_flag = "D";
           }
        }
        if ($var eq "up_long_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "up_short_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "temp") {
           if ($datetime >= 20081029.1815 && $datetime <= 20081029.1845) {
               $new_flag = "D";
           }
           if ($datetime >= 20080417.1600 && $datetime <= 20080417.1650) {
               $new_flag = "D";
           }
           if ($datetime >= 20071101.1602 && $datetime <= 20071101.1625) {
               $new_flag = "D";
           }
           if ($datetime >= 20070419.1626 && $datetime <= 20070419.1710) {
               $new_flag = "D";
           }
           if ($datetime >= 20061019.1440 && $datetime <= 20061019.1520) {
               $new_flag = "D";
           }
           if ($datetime >= 20060504.1540 && $datetime <= 20060504.1605) {
               $new_flag = "D";
           }
           if ($datetime >= 20051103.1552 && $datetime <= 20051103.1720) {
               $new_flag = "D";
           }
           if ($datetime >= 20050421.1440 && $datetime <= 20050421.1525) {
               $new_flag = "D";
           }
        }
        if ($var eq "fc") {
           if ($datetime >= 30050830.1930 && $datetime <= 20050913.0700) {
               $new_flag = "B";
           }
        }
        if ($var eq "rh") {
           if ($datetime >= 20050604.0200 && $datetime <= 20050630.1545) {
               $new_flag = "D";
           }
           if ($datetime >= 20081029.1815 && $datetime <= 20081029.1845) {
               $new_flag = "D";
           }
           if ($datetime >= 20080417.1600 && $datetime <= 20080417.1650) {
               $new_flag = "D";
           }
           if ($datetime >= 20071101.1602 && $datetime <= 20071101.1625) {
               $new_flag = "D";
           }
           if ($datetime >= 20070419.1626 && $datetime <= 20070419.1710) {
               $new_flag = "D";
           }
           if ($datetime >= 20061019.1440 && $datetime <= 20061019.1520) {
               $new_flag = "D";
           }
           if ($datetime >= 20060504.1540 && $datetime <= 20060504.1605) {
               $new_flag = "D";
           }
           if ($datetime >= 20051103.1552 && $datetime <= 20051103.1720) {
               $new_flag = "D";
           }
           if ($datetime >= 20050421.1440 && $datetime <= 20050421.1525) {
               $new_flag = "D";
           }
        }
        if ($var eq "lv_e") {
           if ($datetime >= 30050830.1930 && $datetime <= 20050913.0700) {
               $new_flag = "B";
           }
        }
    #---------------------------------------------------------------------------
    # E7
    #---------------------------------------------------------------------------
    } elsif ($id eq "E7") {
        if ($var eq "wspd") {
           if ($datetime >= 20050111.0000 && $datetime <= 20050205.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20071207.0105 && $datetime <= 20071209.1610) {
               $new_flag = "B";
           }
           if ($datetime >= 20081028.1815 && $datetime <= 20081028.1840) {
               $new_flag = "B";
           }
           if ($datetime >= 20080415.2050 && $datetime <= 20080415.2250) {
               $new_flag = "B";
           }
           if ($datetime >= 20071030.1944 && $datetime <= 20071030.2006) {
               $new_flag = "B";
           }
           if ($datetime >= 20070417.1913 && $datetime <= 20070417.1941) {
               $new_flag = "B";
           }
           if ($datetime >= 20061017.1945 && $datetime <= 20061017.2030) {
               $new_flag = "B";
           }
           if ($datetime >= 20060502.1945 && $datetime <= 20060502.2025) {
               $new_flag = "B";
           }
           if ($datetime >= 20051101.2127 && $datetime <= 20051101.2210) {
               $new_flag = "B";
           }
           if ($datetime >= 20050419.2042 && $datetime <= 20050419.2135) {
               $new_flag = "B";
           }
        }
        if ($var eq "e") {
           if ($datetime >= 20050914.0000 && $datetime <= 20050920.2000) {
               $new_flag = "D";
           }
           if ($datetime >= 20050308.2000 && $datetime <= 20050322.1900) {
               $new_flag = "D";
           }
           if ($datetime >= 20060321.1000 && $datetime <= 20060321.2030) {
               $new_flag = "B";
           }
        }
        if ($var eq "wdir") {
           if ($datetime >= 20050111.0000 && $datetime <= 20050205.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20071207.0105 && $datetime <= 20071209.1610) {
               $new_flag = "B";
           }
           if ($datetime >= 20081028.1815 && $datetime <= 20081028.1840) {
               $new_flag = "B";
           }
           if ($datetime >= 20080415.2050 && $datetime <= 20080415.2250) {
               $new_flag = "B";
           }
           if ($datetime >= 20071030.1944 && $datetime <= 20071030.2006) {
               $new_flag = "B";
           }
           if ($datetime >= 20070417.1913 && $datetime <= 20070417.1941) {
               $new_flag = "B";
           }
           if ($datetime >= 20061017.1945 && $datetime <= 20061017.2030) {
               $new_flag = "B";
           }
           if ($datetime >= 20060502.1945 && $datetime <= 20060502.2025) {
               $new_flag = "B";
           }
           if ($datetime >= 20051101.2127 && $datetime <= 20051101.2210) {
               $new_flag = "B";
           }
           if ($datetime >= 20050419.2042 && $datetime <= 20050419.2135) {
               $new_flag = "B";
           }
        }
        if ($var eq "down_long_hemisp_shaded") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20050405.1950 && $datetime <= 20050419.2100) {
               $new_flag = "B";
           }
        }
        if ($var eq "down_short_hemisp") {
           if ($datetime >= 20080204.1900 && $datetime <= 20080515.1820) {
               $new_flag = "D";
           }
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20050303.2200 && $datetime <= 20050405.1950) {
               $new_flag = "D";
           }
           if ($datetime >= 20070812.1906 && $datetime <= 20070904.1940) {
               $new_flag = "B";
           }
        }
        if ($var eq "precip") {
           if ($datetime >= 20080131.0000 && $datetime <= 20080202.2000) {
               $new_flag = "D";
           }
           if ($datetime >= 20061114.2040 && $datetime <= 20061128.1930) {
               $new_flag = "D";
           }
        }
        if ($var eq "bar_pres") {
           if ($datetime >= 20081028.1815 && $datetime <= 20081028.1840) {
               $new_flag = "D";
           }
           if ($datetime >= 20080415.2050 && $datetime <= 20080415.2250) {
               $new_flag = "D";
           }
           if ($datetime >= 20071030.1944 && $datetime <= 20071030.2006) {
               $new_flag = "D";
           }
           if ($datetime >= 20070417.1913 && $datetime <= 20070417.1941) {
               $new_flag = "D";
           }
           if ($datetime >= 20061017.1945 && $datetime <= 20061017.2030) {
               $new_flag = "D";
           }
           if ($datetime >= 20060502.1945 && $datetime <= 20060502.2025) {
               $new_flag = "D";
           }
           if ($datetime >= 20051101.2127 && $datetime <= 20051101.2210) {
               $new_flag = "D";
           }
           if ($datetime >= 20050419.2042 && $datetime <= 20050419.2135) {
               $new_flag = "D";
           }
        }
        if ($var eq "up_long_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "up_short_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "latent_heat_flux") {
           if ($datetime >= 20090420.1830 && $datetime <= 20090622.1500) {
               $new_flag = "D";
           }
           if ($datetime >= 20090208.1430 && $datetime <= 20090415.1800) {
               $new_flag = "D";
           }
        }
        if ($var eq "temp") {
           if ($datetime >= 20081028.1815 && $datetime <= 20081028.1840) {
               $new_flag = "D";
           }
           if ($datetime >= 20080415.2050 && $datetime <= 20080415.2250) {
               $new_flag = "D";
           }
           if ($datetime >= 20071030.1944 && $datetime <= 20071030.2006) {
               $new_flag = "D";
           }
           if ($datetime >= 20070417.1913 && $datetime <= 20070417.1941) {
               $new_flag = "D";
           }
           if ($datetime >= 20061017.1945 && $datetime <= 20061017.2030) {
               $new_flag = "D";
           }
           if ($datetime >= 20060502.1945 && $datetime <= 20060502.2025) {
               $new_flag = "D";
           }
           if ($datetime >= 20051101.2127 && $datetime <= 20051101.2210) {
               $new_flag = "D";
           }
           if ($datetime >= 20050419.2042 && $datetime <= 20050419.2135) {
               $new_flag = "D";
           }
        }
        if ($var eq "h") {
           if ($datetime >= 20050914.0000 && $datetime <= 20050920.2000) {
               $new_flag = "D";
           }
           if ($datetime >= 20050308.2000 && $datetime <= 20050322.1900) {
               $new_flag = "D";
           }
           if ($datetime >= 20060321.1000 && $datetime <= 20060321.2030) {
               $new_flag = "B";
           }
        }
        if ($var eq "sensible_heat_flux") {
           if ($datetime >= 20090420.1830 && $datetime <= 20090622.1500) {
               $new_flag = "D";
           }
           if ($datetime >= 20090208.1430 && $datetime <= 20090415.1800) {
               $new_flag = "D";
           }
        }
        if ($var eq "rh") {
           if ($datetime >= 20081028.1815 && $datetime <= 20081028.1840) {
               $new_flag = "D";
           }
           if ($datetime >= 20080415.2050 && $datetime <= 20080415.2250) {
               $new_flag = "D";
           }
           if ($datetime >= 20071030.1944 && $datetime <= 20071030.2006) {
               $new_flag = "D";
           }
           if ($datetime >= 20070417.1913 && $datetime <= 20070417.1941) {
               $new_flag = "D";
           }
           if ($datetime >= 20061017.1945 && $datetime <= 20061017.2030) {
               $new_flag = "D";
           }
           if ($datetime >= 20060502.1945 && $datetime <= 20060502.2025) {
               $new_flag = "D";
           }
           if ($datetime >= 20051101.2127 && $datetime <= 20051101.2210) {
               $new_flag = "D";
           }
           if ($datetime >= 20050419.2042 && $datetime <= 20050419.2135) {
               $new_flag = "D";
           }
        }
    #---------------------------------------------------------------------------
    # E8
    #---------------------------------------------------------------------------
    } elsif ($id eq "E8") {
        if ($var eq "wspd") {
           if ($datetime >= 20050104.0000 && $datetime <= 20050207.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20071210.0506 && $datetime <= 20071210.0746) {
               $new_flag = "B";
           }
           if ($datetime >= 20050104.1645 && $datetime <= 20050104.2102) {
               $new_flag = "B";
           }
           if ($datetime >= 20081112.1628 && $datetime <= 20081112.1915) {
               $new_flag = "B";
           }
           if ($datetime >= 20080401.1653 && $datetime <= 20080401.1750) {
               $new_flag = "B";
           }
           if ($datetime >= 20071017.1628 && $datetime <= 20071017.1712) {
               $new_flag = "B";
           }
           if ($datetime >= 20070403.1653 && $datetime <= 20070403.1740) {
               $new_flag = "B";
           }
           if ($datetime >= 20061003.1730 && $datetime <= 20061003.1758) {
               $new_flag = "B";
           }
           if ($datetime >= 20060418.1735 && $datetime <= 20060418.1814) {
               $new_flag = "B";
           }
           if ($datetime >= 20051116.1845 && $datetime <= 20051116.1945) {
               $new_flag = "B";
           }
           if ($datetime >= 20050405.1720 && $datetime <= 20050405.1745) {
               $new_flag = "B";
           }
        }
        if ($var eq "e") {
           if ($datetime >= 20060623.0130 && $datetime <= 20060627.1730) {
               $new_flag = "D";
           }
           if ($datetime >= 20050804.2000 && $datetime <= 20050809.2030) {
               $new_flag = "D";
           }
           if ($datetime >= 20050308.1800 && $datetime <= 20050322.1730) {
               $new_flag = "D";
           }
           if ($datetime >= 20050421.0230 && $datetime <= 20050426.2215) {
               $new_flag = "D";
           }
           if ($datetime >= 20070222.1630 && $datetime <= 20070306.2100) {
               $new_flag = "B";
           }
        }
        if ($var eq "wdir") {
           if ($datetime >= 20060522.1247 && $datetime <= 20060530.1945) {
               $new_flag = "D";
           }
           if ($datetime >= 20050104.0000 && $datetime <= 20050207.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20071210.0506 && $datetime <= 20071210.0746) {
               $new_flag = "B";
           }
           if ($datetime >= 20050104.1645 && $datetime <= 20050104.2102) {
               $new_flag = "B";
           }
           if ($datetime >= 20081112.1628 && $datetime <= 20081112.1915) {
               $new_flag = "B";
           }
           if ($datetime >= 20080401.1653 && $datetime <= 20080401.1750) {
               $new_flag = "B";
           }
           if ($datetime >= 20071017.1628 && $datetime <= 20071017.1712) {
               $new_flag = "B";
           }
           if ($datetime >= 20070403.1653 && $datetime <= 20070403.1740) {
               $new_flag = "B";
           }
           if ($datetime >= 20061003.1730 && $datetime <= 20061003.1758) {
               $new_flag = "B";
           }
           if ($datetime >= 20060418.1735 && $datetime <= 20060418.1814) {
               $new_flag = "B";
           }
           if ($datetime >= 20051116.1845 && $datetime <= 20051116.1945) {
               $new_flag = "B";
           }
           if ($datetime >= 20050405.1720 && $datetime <= 20050405.1745) {
               $new_flag = "B";
           }
        }
        if ($var eq "down_long_hemisp_shaded") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "down_short_hemisp") {
           if ($datetime >= 20080506.0330 && $datetime <= 20080708.1750) {
               $new_flag = "D";
           }
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20050727.1930 && $datetime <= 20050809.1940) {
               $new_flag = "D";
           }
        }
        if ($var eq "precip") {
           if ($datetime >= 20080131.0000 && $datetime <= 20080202.2000) {
               $new_flag = "D";
           }
        }
        if ($var eq "bar_pres") {
           if ($datetime >= 20081112.1628 && $datetime <= 20081112.1915) {
               $new_flag = "D";
           }
           if ($datetime >= 20080401.1653 && $datetime <= 20080401.1750) {
               $new_flag = "D";
           }
           if ($datetime >= 20071017.1628 && $datetime <= 20071017.1712) {
               $new_flag = "D";
           }
           if ($datetime >= 20070403.1653 && $datetime <= 20070403.1740) {
               $new_flag = "D";
           }
           if ($datetime >= 20061003.1730 && $datetime <= 20061003.1758) {
               $new_flag = "D";
           }
           if ($datetime >= 20060418.1735 && $datetime <= 20060418.1814) {
               $new_flag = "D";
           }
           if ($datetime >= 20051116.1845 && $datetime <= 20051116.1945) {
               $new_flag = "D";
           }
           if ($datetime >= 20050405.1720 && $datetime <= 20050405.1745) {
               $new_flag = "D";
           }
        }
        if ($var eq "up_short_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
           if ($datetime >= 20060522.1300 && $datetime <= 20060530.2020) {
               $new_flag = "D";
           }
        }
        if ($var eq "up_long_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "temp") {
           if ($datetime >= 20060522.1247 && $datetime <= 20060530.1945) {
               $new_flag = "D";
           }
           if ($datetime >= 20081112.1628 && $datetime <= 20081112.1915) {
               $new_flag = "D";
           }
           if ($datetime >= 20080401.1653 && $datetime <= 20080401.1750) {
               $new_flag = "D";
           }
           if ($datetime >= 20071017.1628 && $datetime <= 20071017.1712) {
               $new_flag = "D";
           }
           if ($datetime >= 20070403.1653 && $datetime <= 20070403.1740) {
               $new_flag = "D";
           }
           if ($datetime >= 20061003.1730 && $datetime <= 20061003.1758) {
               $new_flag = "D";
           }
           if ($datetime >= 20060418.1735 && $datetime <= 20060418.1814) {
               $new_flag = "D";
           }
           if ($datetime >= 20051116.1845 && $datetime <= 20051116.1945) {
               $new_flag = "D";
           }
           if ($datetime >= 20050405.1720 && $datetime <= 20050405.1745) {
               $new_flag = "D";
           }
        }
        if ($var eq "h") {
           if ($datetime >= 20060623.0130 && $datetime <= 20060627.1730) {
               $new_flag = "D";
           }
           if ($datetime >= 20050804.2000 && $datetime <= 20050809.2030) {
               $new_flag = "D";
           }
           if ($datetime >= 20050308.1800 && $datetime <= 20050322.1730) {
               $new_flag = "D";
           }
           if ($datetime >= 20050421.0230 && $datetime <= 20050426.2215) {
               $new_flag = "D";
           }
           if ($datetime >= 20070222.1630 && $datetime <= 20070306.2100) {
               $new_flag = "B";
           }
        }
        if ($var eq "c_shf1") {
           if ($datetime >= 20050421.0230 && $datetime <= 20050426.2215) {
               $new_flag = "D";
           }
        }
        if ($var eq "rh") {
           if ($datetime >= 20060522.1247 && $datetime <= 20060530.1945) {
               $new_flag = "D";
           }
           if ($datetime >= 20081112.1628 && $datetime <= 20081112.1915) {
               $new_flag = "D";
           }
           if ($datetime >= 20080401.1653 && $datetime <= 20080401.1750) {
               $new_flag = "D";
           }
           if ($datetime >= 20071017.1628 && $datetime <= 20071017.1712) {
               $new_flag = "D";
           }
           if ($datetime >= 20070403.1653 && $datetime <= 20070403.1740) {
               $new_flag = "D";
           }
           if ($datetime >= 20061003.1730 && $datetime <= 20061003.1758) {
               $new_flag = "D";
           }
           if ($datetime >= 20060418.1735 && $datetime <= 20060418.1814) {
               $new_flag = "D";
           }
           if ($datetime >= 20051116.1845 && $datetime <= 20051116.1945) {
               $new_flag = "D";
           }
           if ($datetime >= 20050405.1720 && $datetime <= 20050405.1745) {
               $new_flag = "D";
           }
        }
        if ($var eq "g1") {
           if ($datetime >= 20050421.0230 && $datetime <= 20050426.2215) {
               $new_flag = "D";
           }
        }
    #---------------------------------------------------------------------------
    # E9
    #---------------------------------------------------------------------------
    } elsif ($id eq "E9") {
        if ($var eq "wspd") {
           if ($datetime >= 20071211.0052 && $datetime <= 20071211.0102) {
               $new_flag = "B";
           }
           if ($datetime >= 20050104.1830 && $datetime <= 20050105.0702) {
               $new_flag = "B";
           }
           if ($datetime >= 20081028.1515 && $datetime <= 20081028.1600) {
               $new_flag = "B";
           }
           if ($datetime >= 20080415.1643 && $datetime <= 20080415.1725) {
               $new_flag = "B";
           }
           if ($datetime >= 20071031.1620 && $datetime <= 20071031.1650) {
               $new_flag = "B";
           }
           if ($datetime >= 20070417.1517 && $datetime <= 20070417.1615) {
               $new_flag = "B";
           }
           if ($datetime >= 20061017.1610 && $datetime <= 20061017.1640) {
               $new_flag = "B";
           }
           if ($datetime >= 20060502.1545 && $datetime <= 20060502.1650) {
               $new_flag = "B";
           }
           if ($datetime >= 20051101.1636 && $datetime <= 20051101.1720) {
               $new_flag = "B";
           }
           if ($datetime >= 20050409.1655 && $datetime <= 20050409.1735) {
               $new_flag = "B";
           }
        }
        if ($var eq "e") {
           if ($datetime >= 20071222.1500 && $datetime <= 20080108.1800) {
               $new_flag = "D";
           }
           if ($datetime >= 20051030.0700 && $datetime <= 20051101.1615) {
               $new_flag = "D";
           }
           if ($datetime >= 20050308.1630 && $datetime <= 20050322.1600) {
               $new_flag = "D";
           }
           if ($datetime >= 20051031.0900 && $datetime <= 20051101.1630) {
               $new_flag = "D";
           }
        }
        if ($var eq "wdir") {
           if ($datetime >= 20071211.0052 && $datetime <= 20071211.0102) {
               $new_flag = "B";
           }
           if ($datetime >= 20050104.1830 && $datetime <= 20050105.0702) {
               $new_flag = "B";
           }
           if ($datetime >= 20081028.1515 && $datetime <= 20081028.1600) {
               $new_flag = "B";
           }
           if ($datetime >= 20080415.1643 && $datetime <= 20080415.1725) {
               $new_flag = "B";
           }
           if ($datetime >= 20071031.1620 && $datetime <= 20071031.1650) {
               $new_flag = "B";
           }
           if ($datetime >= 20070417.1517 && $datetime <= 20070417.1615) {
               $new_flag = "B";
           }
           if ($datetime >= 20061017.1610 && $datetime <= 20061017.1640) {
               $new_flag = "B";
           }
           if ($datetime >= 20060502.1545 && $datetime <= 20060502.1650) {
               $new_flag = "B";
           }
           if ($datetime >= 20051101.1636 && $datetime <= 20051101.1720) {
               $new_flag = "B";
           }
           if ($datetime >= 20050409.1655 && $datetime <= 20050409.1735) {
               $new_flag = "B";
           }
        }
        if ($var eq "down_long_hemisp_shaded") {
           if ($datetime >= 20090422.1600 && $datetime <= 20090424.1606) {
               $new_flag = "D";
           }
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "down_short_hemisp") {
           if ($datetime >= 20090422.1600 && $datetime <= 20090424.1606) {
               $new_flag = "D";
           }
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "precip") {
           if ($datetime >= 20080131.0000 && $datetime <= 20080202.2000) {
               $new_flag = "D";
           }
        }
        if ($var eq "bar_pres") {
           if ($datetime >= 20081028.1515 && $datetime <= 20081028.1600) {
               $new_flag = "D";
           }
           if ($datetime >= 20080415.1643 && $datetime <= 20080415.1725) {
               $new_flag = "D";
           }
           if ($datetime >= 20071031.1620 && $datetime <= 20071031.1650) {
               $new_flag = "D";
           }
           if ($datetime >= 20070417.1517 && $datetime <= 20070417.1615) {
               $new_flag = "D";
           }
           if ($datetime >= 20061017.1610 && $datetime <= 20061017.1640) {
               $new_flag = "D";
           }
           if ($datetime >= 20060502.1545 && $datetime <= 20060502.1650) {
               $new_flag = "D";
           }
           if ($datetime >= 20051101.1636 && $datetime <= 20051101.1720) {
               $new_flag = "D";
           }
           if ($datetime >= 20050409.1655 && $datetime <= 20050409.1735) {
               $new_flag = "D";
           }
        }
        if ($var eq "up_long_hemisp") {
           if ($datetime >= 20090422.1600 && $datetime <= 20090424.1606) {
               $new_flag = "D";
           }
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "up_short_hemisp") {
           if ($datetime >= 20090422.1600 && $datetime <= 20090424.1606) {
               $new_flag = "D";
           }
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "D";
           }
        }
        if ($var eq "temp") {
           if ($datetime >= 20081028.1515 && $datetime <= 20081028.1600) {
               $new_flag = "D";
           }
           if ($datetime >= 20080415.1643 && $datetime <= 20080415.1725) {
               $new_flag = "D";
           }
           if ($datetime >= 20071031.1620 && $datetime <= 20071031.1650) {
               $new_flag = "D";
           }
           if ($datetime >= 20070417.1517 && $datetime <= 20070417.1615) {
               $new_flag = "D";
           }
           if ($datetime >= 20061017.1610 && $datetime <= 20061017.1640) {
               $new_flag = "D";
           }
           if ($datetime >= 20060502.1545 && $datetime <= 20060502.1650) {
               $new_flag = "D";
           }
           if ($datetime >= 20051101.1636 && $datetime <= 20051101.1720) {
               $new_flag = "D";
           }
           if ($datetime >= 20050409.1655 && $datetime <= 20050409.1735) {
               $new_flag = "D";
           }
        }
        if ($var eq "h") {
           if ($datetime >= 20071222.1500 && $datetime <= 20080108.1800) {
               $new_flag = "D";
           }
           if ($datetime >= 20051030.0700 && $datetime <= 20051101.1615) {
               $new_flag = "D";
           }
           if ($datetime >= 20050308.1630 && $datetime <= 20050322.1600) {
               $new_flag = "D";
           }
           if ($datetime >= 20051031.0900 && $datetime <= 20051101.1630) {
               $new_flag = "D";
           }
        }
        if ($var eq "rh") {
           if ($datetime >= 20081028.1515 && $datetime <= 20081028.1600) {
               $new_flag = "D";
           }
           if ($datetime >= 20080415.1643 && $datetime <= 20080415.1725) {
               $new_flag = "D";
           }
           if ($datetime >= 20071031.1620 && $datetime <= 20071031.1650) {
               $new_flag = "D";
           }
           if ($datetime >= 20070417.1517 && $datetime <= 20070417.1615) {
               $new_flag = "D";
           }
           if ($datetime >= 20061017.1610 && $datetime <= 20061017.1640) {
               $new_flag = "D";
           }
           if ($datetime >= 20060502.1545 && $datetime <= 20060502.1650) {
               $new_flag = "D";
           }
           if ($datetime >= 20051101.1636 && $datetime <= 20051101.1720) {
               $new_flag = "D";
           }
           if ($datetime >= 20050409.1655 && $datetime <= 20050409.1735) {
               $new_flag = "D";
           }
        }
    }

    #---------------------------------------------------------------------------
    # following section fixes flags according to Scot's directions
    #---------------------------------------------------------------------------
    # C1
    #---------------------------------------------------------------------------
    if ($id eq "C1") {
        if ($var eq "up_long_hemisp") {
           if ($datetime >= 20070107.1000 && $datetime <= 20070110.1300) {
               $new_flag = "D";
           }
        }
        if ($var eq "down_long_hemisp_shaded") {
           if ($datetime >= 20071007.0430 && $datetime <= 20071007.0430) {
               $new_flag = "B";
           }
           if ($datetime >= 20071013.2000 && $datetime <= 20071013.2000) {
               $new_flag = "B";
           }
           if ($datetime >= 20070910.0730 && $datetime <= 20070910.0730) {
               $new_flag = "B";
           }
           if ($datetime >= 20070909.1800 && $datetime <= 20070909.1800) {
               $new_flag = "B";
           }
           if ($datetime >= 20070909.0730 && $datetime <= 20070909.0800) {
               $new_flag = "B";
           }
        }
        if ($var eq "down_short_hemisp") {
           if ($datetime >= 20050821.1900 && $datetime <= 20050821.1900) {
               $new_flag = "D";
           }
           if ($datetime >= 20080609.1930 && $datetime <= 20080609.1930) {
               $new_flag = "D";
           }
        }
    #---------------------------------------------------------------------------
    # E1
    #---------------------------------------------------------------------------
    } elsif ($id eq "E1") {
        if ($var eq "temp") {
           if ($datetime >= 20070226.1200 && $datetime <= 20070301.1230) {
               $new_flag = "D";
           }
        }
    #---------------------------------------------------------------------------
    # E11
    #---------------------------------------------------------------------------
    } elsif ($id eq "E11") {
        if ($var eq "down_long_hemisp_shaded") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "B";
           }
        }
        if ($var eq "up_long_hemisp") {
           if ($datetime >= 20071210.0000 && $datetime <= 20071225.2359) {
               $new_flag = "B";
           }
        }
    #---------------------------------------------------------------------------
    # E13
    #---------------------------------------------------------------------------
    } elsif ($id eq "E13") {
        if ($var eq "up_long_hemisp") {
           if ($datetime >= 20080107.0030 && $datetime <= 20080107.0030) {
               $new_flag = "B";
           }
           if ($datetime >= 20071231.0600 && $datetime <= 20071231.0600) {
               $new_flag = "B";
           }
           if ($datetime >= 20071228.1700 && $datetime <= 20071228.1700) {
               $new_flag = "B";
           }
           if ($datetime >= 20071228.0730 && $datetime <= 20071228.0730) {
               $new_flag = "B";
           }
        }
        if ($var eq "down_long_hemisp_shaded") {
           if ($datetime >= 20080104.0330 && $datetime <= 20080104.0400) {
               $new_flag = "D";
           }
           if ($datetime >= 20071227.2200 && $datetime <= 20071228.1700) {
               $new_flag = "B";
           }
           if ($datetime >= 20071229.0100 && $datetime <= 20071229.0100) {
               $new_flag = "B";
           }
           if ($datetime >= 20071231.0500 && $datetime <= 20071231.0500) {
               $new_flag = "B";
           }
           if ($datetime >= 20071231.0400 && $datetime <= 20071231.0400) {
               $new_flag = "B";
           }
           if ($datetime >= 20080107.1400 && $datetime <= 20080107.1400) {
               $new_flag = "B";
           }
           if ($datetime >= 20080107.0600 && $datetime <= 20080107.0600) {
               $new_flag = "B";
           }
           if ($datetime >= 20080106.2230 && $datetime <= 20080106.2300) {
               $new_flag = "B";
           }
           if ($datetime >= 20080104.0030 && $datetime <= 20080104.0100) {
               $new_flag = "B";
           }
           if ($datetime >= 20080103.1230 && $datetime <= 20080103.1500) {
               $new_flag = "B";
           }
        }
    #---------------------------------------------------------------------------
    # E16
    #---------------------------------------------------------------------------
    } elsif ($id eq "E16") {
        if ($var eq "up_long_hemisp") {
           if ($datetime >= 20080116.1530 && $datetime <= 20080116.1730) {
               $new_flag = "B";
           }
           if ($datetime >= 20080115.1600 && $datetime <= 20080116.1200) {
               $new_flag = "B";
           }
           if ($datetime >= 20080114.1630 && $datetime <= 20080115.0130) {
               $new_flag = "B";
           }
           if ($datetime >= 20080113.1700 && $datetime <= 20080114.0200) {
               $new_flag = "B";
           }
           if ($datetime >= 20080112.1630 && $datetime <= 20080113.0100) {
               $new_flag = "B";
           }
           if ($datetime >= 20080111.1700 && $datetime <= 20080112.0100) {
               $new_flag = "B";
           }
           if ($datetime >= 20080110.0830 && $datetime <= 20080111.0030) {
               $new_flag = "B";
           }
           if ($datetime >= 20080105.1600 && $datetime <= 20080109.0030) {
               $new_flag = "B";
           }
           if ($datetime >= 20080104.1800 && $datetime <= 20080105.0830) {
               $new_flag = "B";
           }
           if ($datetime >= 20080103.1930 && $datetime <= 20080104.0100) {
               $new_flag = "B";
           }
        }
        if ($var eq "down_long_hemisp_shaded") {
           if ($datetime >= 20080116.1530 && $datetime <= 20080116.1730) {
               $new_flag = "B";
           }
           if ($datetime >= 20080116.1100 && $datetime <= 20080116.1200) {
               $new_flag = "B";
           }
           if ($datetime >= 20080114.1630 && $datetime <= 20080115.0130) {
               $new_flag = "B";
           }
           if ($datetime >= 20080113.1730 && $datetime <= 20080114.0200) {
               $new_flag = "B";
           }
           if ($datetime >= 20080112.1700 && $datetime <= 20080113.0100) {
               $new_flag = "B";
           }
           if ($datetime >= 20080110.0830 && $datetime <= 20080111.0030) {
               $new_flag = "B";
           }
           if ($datetime >= 20080108.2030 && $datetime <= 20080109.0030) {
               $new_flag = "B";
           }
           if ($datetime >= 20080105.1600 && $datetime <= 20080108.0900) {
               $new_flag = "B";
           }
        }
        if ($var eq "down_short_hemisp") {
           if ($datetime >= 20080111.1700 && $datetime <= 20080111.1730) {
               $new_flag = "B";
           }
           if ($datetime >= 20080108.0300 && $datetime <= 20080108.0300) {
               $new_flag = "B";
           }
           if ($datetime >= 20080107.1630 && $datetime <= 20080107.1700) {
               $new_flag = "B";
           }
           if ($datetime >= 20080107.1130 && $datetime <= 20080107.1400) {
               $new_flag = "B";
           }
        }
    #---------------------------------------------------------------------------
    # E18
    #---------------------------------------------------------------------------
    } elsif ($id eq "E18") {
        if ($var eq "up_short_hemisp") {
           if ($datetime >= 20061108.0330 && $datetime <= 20061115.0500) {
               $new_flag = "D";
           }
        }
        if ($var eq "up_long_hemisp") {
           if ($datetime >= 20091011.0500 && $datetime <= 20091011.0500) {
               $new_flag = "B";
           }
           if ($datetime >= 20091010.1730 && $datetime <= 20091010.1800) {
               $new_flag = "B";
           }
           if ($datetime >= 20091010.1500 && $datetime <= 20091010.1530) {
               $new_flag = "B";
           }
        }
    #---------------------------------------------------------------------------
    # E2
    #---------------------------------------------------------------------------
    } elsif ($id eq "E2") {
        if ($var eq "down_long_hemisp_shaded") {
           if ($datetime >= 20090616.0630 && $datetime <= 20090616.0630) {
               $new_flag = "D";
           }
           if ($datetime >= 20050224.1400 && $datetime <= 20050224.1600) {
               $new_flag = "D";
           }
           if ($datetime >= 20050223.2030 && $datetime <= 20050223.2030) {
               $new_flag = "D";
           }
           if ($datetime >= 20050101.1300 && $datetime <= 20050102.0400) {
               $new_flag = "D";
           }
        }
    #---------------------------------------------------------------------------
    # E20
    #---------------------------------------------------------------------------
    } elsif ($id eq "E20") {
        if ($var eq "up_long_hemisp") {
           if ($datetime >= 20070306.1800 && $datetime <= 20070306.2130) {
               $new_flag = "D";
           }
           if ($datetime >= 20070228.1500 && $datetime <= 20070218.1730) {
               $new_flag = "D";
           }
           if ($datetime >= 20070314.0700 && $datetime <= 20070314.0730) {
               $new_flag = "B";
           }
           if ($datetime >= 20070313.0600 && $datetime <= 20070313.1500) {
               $new_flag = "B";
           }
           if ($datetime >= 20070310.2000 && $datetime <= 20070310.2230) {
               $new_flag = "B";
           }
           if ($datetime >= 20070309.1930 && $datetime <= 20070309.1930) {
               $new_flag = "B";
           }
           if ($datetime >= 20070307.1700 && $datetime <= 20070307.1700) {
               $new_flag = "B";
           }
           if ($datetime >= 20070304.2230 && $datetime <= 20070304.2300) {
               $new_flag = "B";
           }
           if ($datetime >= 20070302.1630 && $datetime <= 20070302.1700) {
               $new_flag = "B";
           }
           if ($datetime >= 20070301.1430 && $datetime <= 20070301.1430) {
               $new_flag = "B";
           }
           if ($datetime >= 20070301.0230 && $datetime <= 20070301.0730) {
               $new_flag = "B";
           }
           if ($datetime >= 20070227.1500 && $datetime <= 20070227.1630) {
               $new_flag = "B";
           }
           if ($datetime >= 20070225.0400 && $datetime <= 20070225.1130) {
               $new_flag = "B";
           }
           if ($datetime >= 20070222.1730 && $datetime <= 20070222.1730) {
               $new_flag = "B";
           }
           if ($datetime >= 20070221.1430 && $datetime <= 20070221.1430) {
               $new_flag = "B";
           }
           if ($datetime >= 20070221.0400 && $datetime <= 20070221.0500) {
               $new_flag = "B";
           }
           if ($datetime >= 20070220.1500 && $datetime <= 20070220.1530) {
               $new_flag = "B";
           }
           if ($datetime >= 20070217.1800 && $datetime <= 20070217.1800) {
               $new_flag = "B";
           }
           if ($datetime >= 20070216.1500 && $datetime <= 20070216.1500) {
               $new_flag = "B";
           }
           if ($datetime >= 20070215.1530 && $datetime <= 20070215.1900) {
               $new_flag = "B";
           }
           if ($datetime >= 20070211.1700 && $datetime <= 20070211.1700) {
               $new_flag = "B";
           }
           if ($datetime >= 20070210.1530 && $datetime <= 20070210.2330) {
               $new_flag = "B";
           }
           if ($datetime >= 20070202.0500 && $datetime <= 20070202.1700) {
               $new_flag = "B";
           }
           if ($datetime >= 20070131.2030 && $datetime <= 20070201.1630) {
               $new_flag = "B";
           }
           if ($datetime >= 20070130.1630 && $datetime <= 20070130.1800) {
               $new_flag = "B";
           }
           if ($datetime >= 20070128.1600 && $datetime <= 20070128.1600) {
               $new_flag = "B";
           }
           if ($datetime >= 20070127.1630 && $datetime <= 20070127.1900) {
               $new_flag = "B";
           }
           if ($datetime >= 20070125.1600 && $datetime <= 20070125.1600) {
               $new_flag = "B";
           }
           if ($datetime >= 20070123.2330 && $datetime <= 20070123.2330) {
               $new_flag = "B";
           }
           if ($datetime >= 20070123.1700 && $datetime <= 20070123.1730) {
               $new_flag = "B";
           }
           if ($datetime >= 20070123.0030 && $datetime <= 20070123.0030) {
               $new_flag = "B";
           }
           if ($datetime >= 20070122.1700 && $datetime <= 20070122.2030) {
               $new_flag = "B";
           }
           if ($datetime >= 20070121.1500 && $datetime <= 20070121.1600) {
               $new_flag = "B";
           }
           if ($datetime >= 20070110.1530 && $datetime <= 20070110.1530) {
               $new_flag = "B";
           }
           if ($datetime >= 20070108.1430 && $datetime <= 20070108.1630) {
               $new_flag = "B";
           }
           if ($datetime >= 20070108.0830 && $datetime <= 20070108.0830) {
               $new_flag = "B";
           }
           if ($datetime >= 20070106.1700 && $datetime <= 20070106.2200) {
               $new_flag = "B";
           }
           if ($datetime >= 20070103.1530 && $datetime <= 20070103.1530) {
               $new_flag = "B";
           }
           if ($datetime >= 20070103.0400 && $datetime <= 20070103.0600) {
               $new_flag = "B";
           }
           if ($datetime >= 20061224.1600 && $datetime <= 20061224.1730) {
               $new_flag = "B";
           }
           if ($datetime >= 20061223.1530 && $datetime <= 20061223.1530) {
               $new_flag = "B";
           }
           if ($datetime >= 20061223.0730 && $datetime <= 20061223.0900) {
               $new_flag = "B";
           }
           if ($datetime >= 20061219.1800 && $datetime <= 20061220.0530) {
               $new_flag = "B";
           }
           if ($datetime >= 20061217.1730 && $datetime <= 20061217.1730) {
               $new_flag = "B";
           }
           if ($datetime >= 20061217.0430 && $datetime <= 20061217.0630) {
               $new_flag = "B";
           }
           if ($datetime >= 20061209.1800 && $datetime <= 20061209.1800) {
               $new_flag = "B";
           }
           if ($datetime >= 20061208.1800 && $datetime <= 20061208.1800) {
               $new_flag = "B";
           }
           if ($datetime >= 20061205.1500 && $datetime <= 20061205.1600) {
               $new_flag = "B";
           }
        }
    #---------------------------------------------------------------------------
    # E21
    #---------------------------------------------------------------------------
    } elsif ($id eq "E21") {
        if ($var eq "rh") {
           if ($datetime >= 20050104.1030 && $datetime <= 20050104.1800) {
               $new_flag = "D";
           }
           if ($datetime >= 20050914.1600 && $datetime <= 20050920.1300) {
               $new_flag = "B";
           }
           if ($datetime >= 20050110.1900 && $datetime <= 20050110.2000) {
               $new_flag = "B";
           }
           if ($datetime >= 20050107.2230 && $datetime <= 20050108.1600) {
               $new_flag = "B";
           }
           if ($datetime >= 20050104.1800 && $datetime <= 20050105.2100) {
               $new_flag = "B";
           }
           if ($datetime >= 20050103.1000 && $datetime <= 20050104.1000) {
               $new_flag = "B";
           }
        }
    #---------------------------------------------------------------------------
    # E22
    #---------------------------------------------------------------------------
    } elsif ($id eq "E22") {
        if ($var eq "up_long_hemisp") {
           if ($datetime >= 20050531.2300 && $datetime <= 20050531.2300) {
               $new_flag = "B";
           }
           if ($datetime >= 20050518.0330 && $datetime <= 20050518.0330) {
               $new_flag = "B";
           }
           if ($datetime >= 20050506.2230 && $datetime <= 20050506.2230) {
               $new_flag = "B";
           }
        }
    #---------------------------------------------------------------------------
    # E24
    #---------------------------------------------------------------------------
    } elsif ($id eq "E24") {
        if ($var eq "up_long_hemisp") {
           if ($datetime >= 20070523.2030 && $datetime <= 20070523.2100) {
               $new_flag = "B";
           }
           if ($datetime >= 20070522.1900 && $datetime <= 20070522.2300) {
               $new_flag = "B";
           }
           if ($datetime >= 20070520.1900 && $datetime <= 20070520.2130) {
               $new_flag = "B";
           }
           if ($datetime >= 20070518.1600 && $datetime <= 20070518.2100) {
               $new_flag = "B";
           }
           if ($datetime >= 20070517.1600 && $datetime <= 20070518.0000) {
               $new_flag = "B";
           }
           if ($datetime >= 20070516.1800 && $datetime <= 20070516.2330) {
               $new_flag = "B";
           }
           if ($datetime >= 20070514.1400 && $datetime <= 20070515.0130) {
               $new_flag = "B";
           }
           if ($datetime >= 20070514.0100 && $datetime <= 20070514.0130) {
               $new_flag = "B";
           }
           if ($datetime >= 20070513.1330 && $datetime <= 20070513.1700) {
               $new_flag = "B";
           }
           if ($datetime >= 20070512.1400 && $datetime <= 20070513.0130) {
               $new_flag = "B";
           }
           if ($datetime >= 20070511.1430 && $datetime <= 20070512.0100) {
               $new_flag = "B";
           }
           if ($datetime >= 20070510.1600 && $datetime <= 20070510.2300) {
               $new_flag = "B";
           }
           if ($datetime >= 20070509.1900 && $datetime <= 20070509.2300) {
               $new_flag = "B";
           }
           if ($datetime >= 20070509.1730 && $datetime <= 20070509.1730) {
               $new_flag = "B";
           }
           if ($datetime >= 20061129.1700 && $datetime <= 20061205.1530) {
               $new_flag = "B";
           }
           if ($datetime >= 20061129.0730 && $datetime <= 20061129.1000) {
               $new_flag = "B";
           }
           if ($datetime >= 20061002.1700 && $datetime <= 20061002.2330) {
               $new_flag = "B";
           }
           if ($datetime >= 20060831.1100 && $datetime <= 20060831.1130) {
               $new_flag = "B";
           }
           if ($datetime >= 20060831.0830 && $datetime <= 20060831.0900) {
               $new_flag = "B";
           }
           if ($datetime >= 20050312.1830 && $datetime <= 20050313.0030) {
               $new_flag = "B";
           }
           if ($datetime >= 20050304.2200 && $datetime <= 20050304.2230) {
               $new_flag = "B";
           }
           if ($datetime >= 20050220.2000 && $datetime <= 20050220.2030) {
               $new_flag = "B";
           }
           if ($datetime >= 20050215.2100 && $datetime <= 20050215.2100) {
               $new_flag = "B";
           }
           if ($datetime >= 20050214.1900 && $datetime <= 20050214.1930) {
               $new_flag = "B";
           }
           if ($datetime >= 20050213.2200 && $datetime <= 20050213.2230) {
               $new_flag = "B";
           }
        }
        if ($var eq "down_long_hemisp_shaded") {
           if ($datetime >= 20070523.2030 && $datetime <= 20070523.2100) {
               $new_flag = "B";
           }
           if ($datetime >= 20070522.1900 && $datetime <= 20070522.2300) {
               $new_flag = "B";
           }
           if ($datetime >= 20070520.1900 && $datetime <= 20070520.2130) {
               $new_flag = "B";
           }
           if ($datetime >= 20070518.1600 && $datetime <= 20070518.2100) {
               $new_flag = "B";
           }
           if ($datetime >= 20070517.1600 && $datetime <= 20070518.0000) {
               $new_flag = "B";
           }
           if ($datetime >= 20070516.1800 && $datetime <= 20070516.2330) {
               $new_flag = "B";
           }
           if ($datetime >= 20070514.1400 && $datetime <= 20070515.0130) {
               $new_flag = "B";
           }
           if ($datetime >= 20070514.0100 && $datetime <= 20070514.0130) {
               $new_flag = "B";
           }
           if ($datetime >= 20070513.1330 && $datetime <= 20070513.1700) {
               $new_flag = "B";
           }
           if ($datetime >= 20070512.1400 && $datetime <= 20070513.0130) {
               $new_flag = "B";
           }
           if ($datetime >= 20070511.1430 && $datetime <= 20070512.0100) {
               $new_flag = "B";
           }
           if ($datetime >= 20070510.1600 && $datetime <= 20070510.2300) {
               $new_flag = "B";
           }
           if ($datetime >= 20070509.1900 && $datetime <= 20070509.2300) {
               $new_flag = "B";
           }
           if ($datetime >= 20070509.1730 && $datetime <= 20070509.1730) {
               $new_flag = "B";
           }
           if ($datetime >= 20061129.1700 && $datetime <= 20061205.1530) {
               $new_flag = "B";
           }
           if ($datetime >= 20061129.0730 && $datetime <= 20061129.1000) {
               $new_flag = "B";
           }
           if ($datetime >= 20061002.1700 && $datetime <= 20061002.2330) {
               $new_flag = "B";
           }
           if ($datetime >= 20060831.1100 && $datetime <= 20060831.1130) {
               $new_flag = "B";
           }
           if ($datetime >= 20060831.0830 && $datetime <= 20060831.0900) {
               $new_flag = "B";
           }
           if ($datetime >= 20050312.1830 && $datetime <= 20050313.0030) {
               $new_flag = "B";
           }
           if ($datetime >= 20050304.2200 && $datetime <= 20050304.2230) {
               $new_flag = "B";
           }
           if ($datetime >= 20050220.2000 && $datetime <= 20050220.2030) {
               $new_flag = "B";
           }
           if ($datetime >= 20050215.2100 && $datetime <= 20050215.2100) {
               $new_flag = "B";
           }
           if ($datetime >= 20050214.1900 && $datetime <= 20050214.1930) {
               $new_flag = "B";
           }
           if ($datetime >= 20050213.2200 && $datetime <= 20050213.2230) {
               $new_flag = "B";
           }
        }
        if ($var eq "down_short_hemisp") {
           if ($datetime >= 20090628.1530 && $datetime <= 20090628.1630) {
               $new_flag = "D";
           }
           if ($datetime >= 20090627.1300 && $datetime <= 20090627.1430) {
               $new_flag = "D";
           }
           if ($datetime >= 20090625.0930 && $datetime <= 20090625.1330) {
               $new_flag = "D";
           }
           if ($datetime >= 20070518.1600 && $datetime <= 20070518.2330) {
               $new_flag = "D";
           }
           if ($datetime >= 20070517.1530 && $datetime <= 20070518.0030) {
               $new_flag = "D";
           }
           if ($datetime >= 20070516.1730 && $datetime <= 20070516.2330) {
               $new_flag = "D";
           }
           if ($datetime >= 20090701.0300 && $datetime <= 20090701.1530) {
               $new_flag = "B";
           }
           if ($datetime >= 20090630.0600 && $datetime <= 20090630.0600) {
               $new_flag = "B";
           }
           if ($datetime >= 20061203.0230 && $datetime <= 20061203.1400) {
               $new_flag = "B";
           }
           if ($datetime >= 20061202.0400 && $datetime <= 20061202.1500) {
               $new_flag = "B";
           }
           if ($datetime >= 20061130.1130 && $datetime <= 20061130.1130) {
               $new_flag = "B";
           }
           if ($datetime >= 20061002.2000 && $datetime <= 20061002.2330) {
               $new_flag = "B";
           }
           if ($datetime >= 20050818.1430 && $datetime <= 20050818.1430) {
               $new_flag = "B";
           }
           if ($datetime >= 20050331.1730 && $datetime <= 20050331.1730) {
               $new_flag = "B";
           }
        }
    #---------------------------------------------------------------------------
    # E27
    #---------------------------------------------------------------------------
    } elsif ($id eq "E27") {
        if ($var eq "precip") {
           if ($datetime >= 20091005.2130 && $datetime <= 20091006.2000) {
               $new_flag = "D";
           }
        }
    #---------------------------------------------------------------------------
    # E3
    #---------------------------------------------------------------------------
    } elsif ($id eq "E3") {
        if ($var eq "up_short_hemisp") {
           if ($datetime >= 20051011.1200 && $datetime <= 20051019.1300) {
               $new_flag = "D";
           }
        }
        if ($var eq "up_long_hemisp") {
           if ($datetime >= 20090910.1400 && $datetime <= 20091028.1300) {
               $new_flag = "B";
           }
        }
        if ($var eq "down_long_hemisp_shaded") {
           if ($datetime >= 20090910.1400 && $datetime <= 20091028.1300) {
               $new_flag = "B";
           }
           if ($datetime >= 20060808.0200 && $datetime <= 20060808.0200) {
               $new_flag = "B";
           }
        }
        if ($var eq "down_short_hemisp") {
           if ($datetime >= 20090912.1500 && $datetime <= 20090912.1500) {
               $new_flag = "B";
           }
        }
    #---------------------------------------------------------------------------
    # E5
    #---------------------------------------------------------------------------
    } elsif ($id eq "E5") {
        if ($var eq "up_long_hemisp") {
           if ($datetime >= 20071228.1430 && $datetime <= 20071228.1600) {
               $new_flag = "B";
           }
        }
    #---------------------------------------------------------------------------
    # E6
    #---------------------------------------------------------------------------
    } elsif ($id eq "E6") {
        if ($var eq "up_long_hemisp") {
           if ($datetime >= 20090819.2100 && $datetime <= 20091231.2330) {
               $new_flag = "B";
           }
        }
    #---------------------------------------------------------------------------
    # E7
    #---------------------------------------------------------------------------
    } elsif ($id eq "E7") {
        if ($var eq "up_long_hemisp") {
           if ($datetime >= 20090526.1830 && $datetime <= 20091231.2330) {
               $new_flag = "B";
           }
        }
    #---------------------------------------------------------------------------
    # E9
    #---------------------------------------------------------------------------
    } elsif ($id eq "E9") {
        if ($var eq "up_long_hemisp") {
           if ($datetime >= 20070523.1830 && $datetime <= 20070529.1600) {
               $new_flag = "B";
           }
        }
        if ($var eq "down_long_hemisp_shaded") {
           if ($datetime >= 20070523.1830 && $datetime <= 20070529.1600) {
               $new_flag = "B";
           }
        }
    }

    #---------------------------------------------------------------------------
    # following section fixes more flags according to Scot's directions
    #---------------------------------------------------------------------------
    # C1
    #---------------------------------------------------------------------------
    if ($id eq "C1") {
        if ($var eq "up_long_hemisp") {
           if ($datetime >= 20070114.0230 && $datetime <= 20070116.2330) {
               $new_flag = "D";
           }
           if ($datetime >= 20070106.0400 && $datetime <= 20070107.0930) {
               $new_flag = "D";
           }
        }
    #---------------------------------------------------------------------------
    # E11
    #---------------------------------------------------------------------------
    } elsif ($id eq "E11") {
        if ($var eq "up_long_hemisp") {
           if ($datetime >= 20081027.2000 && $datetime <= 20081029.1630) {
               $new_flag = "D";
           }
           if ($datetime >= 20081027.1300 && $datetime <= 20081027.1300) {
               $new_flag = "D";
           }
           if ($datetime >= 20081026.0530 && $datetime <= 20081026.1700) {
               $new_flag = "D";
           }
           if ($datetime >= 20081022.1500 && $datetime <= 20081022.1900) {
               $new_flag = "D";
           }
           if ($datetime >= 20081022.0800 && $datetime <= 20081022.0800) {
               $new_flag = "D";
           }
        }
    #---------------------------------------------------------------------------
    # E13
    #---------------------------------------------------------------------------
    } elsif ($id eq "E13") {
        if ($var eq "up_long_hemisp") {
           if ($datetime >= 20081006.2230 && $datetime <= 20081006.2300) {
               $new_flag = "D";
           }
        }
    #---------------------------------------------------------------------------
    # E16
    #---------------------------------------------------------------------------
    } elsif ($id eq "E16") {
        if ($var eq "down_short_hemisp") {
           if ($datetime >= 20080113.1730 && $datetime <= 20080113.1730) {
               $new_flag = "B";
           }
        }
    #---------------------------------------------------------------------------
    # E21
    #---------------------------------------------------------------------------
    } elsif ($id eq "E21") {
        if ($var eq "rh") {
           if ($datetime >= 20060218.1430 && $datetime <= 20060218.1730) {
               $new_flag = "D";
           }
           if ($datetime >= 20061208.1300 && $datetime <= 20061208.1330) {
               $new_flag = "B";
           }
           if ($datetime >= 20070216.0830 && $datetime <= 20070216.1400) {
               $new_flag = "B";
           }
           if ($datetime >= 20060218.1330 && $datetime <= 20060218.1400) {
               $new_flag = "B";
           }
        }
    #---------------------------------------------------------------------------
    # E24
    #---------------------------------------------------------------------------
    } elsif ($id eq "E24") {
        if ($var eq "up_long_hemisp") {
           if ($datetime >= 20060831.1200 && $datetime <= 20060831.1230) {
               $new_flag = "B";
           }
           if ($datetime >= 20060831.0930 && $datetime <= 20060831.0930) {
               $new_flag = "B";
           }
        }
    #---------------------------------------------------------------------------
    # E3
    #---------------------------------------------------------------------------
    } elsif ($id eq "E3") {
        if ($var eq "temp") {
           if ($datetime >= 20090818.1730 && $datetime <= 20090818.2100) {
               $new_flag = "D";
           }
           if ($datetime >= 20090822.0200 && $datetime <= 20090822.0430) {
               $new_flag = "B";
           }
        }
        if ($var eq "rh") {
           if ($datetime >= 20090818.1730 && $datetime <= 20090818.2100) {
               $new_flag = "D";
           }
           if ($datetime >= 20061028.1430 && $datetime <= 20061028.1600) {
               $new_flag = "D";
           }
           if ($datetime >= 20061026.2300 && $datetime <= 20061027.0230) {
               $new_flag = "B";
           }
           if ($datetime >= 20061220.1600 && $datetime <= 20061220.1800) {
               $new_flag = "B";
           }
           if ($datetime >= 20061221.2000 && $datetime <= 20061221.2300) {
               $new_flag = "B";
           }
           if ($datetime >= 20050116.1800 && $datetime <= 20050116.1800) {
               $new_flag = "B";
           }
           if ($datetime >= 20050112.1830 && $datetime <= 20050113.2000) {
               $new_flag = "B";
           }
        }
    #---------------------------------------------------------------------------
    # E4
    #---------------------------------------------------------------------------
    } elsif ($id eq "E4") {
        if ($var eq "up_long_hemisp") {
           if ($datetime >= 20061005.1730 && $datetime <= 20061005.1730) {
               $new_flag = "B";
           }
        }
    #---------------------------------------------------------------------------
    # E6
    #---------------------------------------------------------------------------
    } elsif ($id eq "E6") {
        if ($var eq "up_long_hemisp") {
           if ($datetime >= 20090423.1530 && $datetime <= 20090423.1530) {
               $new_flag = "B";
           }
        }
    #---------------------------------------------------------------------------
    # E7
    #---------------------------------------------------------------------------
    } elsif ($id eq "E7") {
        if ($var eq "down_long_hemisp_shaded") {
           if ($datetime >= 20080515.1700 && $datetime <= 20080515.1700) {
               $new_flag = "B";
           }
           if ($datetime >= 20070630.1130 && $datetime <= 20070630.1130) {
               $new_flag = "B";
           }
        }
    #---------------------------------------------------------------------------
    # E8
    #---------------------------------------------------------------------------
    } elsif ($id eq "E8") {
        if ($var eq "rh") {
           if ($datetime >= 20061016.0830 && $datetime <= 20061017.1900) {
               $new_flag = "B";
           }
           if ($datetime >= 20050123.0230 && $datetime <= 20050123.1400) {
               $new_flag = "B";
           }
           if ($datetime >= 20050114.0330 && $datetime <= 20050117.1400) {
               $new_flag = "B";
           }
        }
    #---------------------------------------------------------------------------
    # E9
    #---------------------------------------------------------------------------
    } elsif ($id eq "E9") {
        if ($var eq "down_long_hemisp_shaded") {
           if ($datetime >= 20080131.2000 && $datetime <= 20080131.2130) {
               $new_flag = "B";
           }
        }
        if ($var eq "up_long_hemisp") {
           if ($datetime >= 20080131.2000 && $datetime <= 20080131.2130) {
               $new_flag = "B";
           }
        }
    }

    return $new_flag;
}

sub strDate_numDate {

	my $strDate = shift;			# like 2003/04/16
	my $our_hour = shift;
	my $our_min = shift;

	my $our_year	= substr($date, 0, 4);
	my $our_month 	= substr($date, 5, 2);
	my $our_day 	= substr($date, 8, 2);
	my $our_time 	= sprintf("%02d%02d", $our_hour, $our_min) * 0.0001;
	my $numDate     = ($our_year.$our_month.$our_day) + $our_time;			# so 1/1/03 1230 = 20030101.123
	print ("in strDate_numDate with $strDate $our_hour:$our_min changed to $numDate\n") if ($DEBUG1);
	return $numDate;				# like 20030416
}
