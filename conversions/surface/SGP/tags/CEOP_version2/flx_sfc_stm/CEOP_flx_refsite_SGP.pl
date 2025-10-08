#!/usr/bin/perl -w

#--------------------------------------------------------------------------------------
# CEOP_flx_refsite_SGP.pl
#
# This s/w is used in converting SGP flux netCDF files into CEOP output.
# It handles the EBBR and ECOR platforms for multiple (C1-E24) sites.
#
# This Perl script is used in the processing of CEOP GAPP SGP data.  Input files are
# previously prepared using nesob_dump (nesob_dump is a variation of nc_dump specifially
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
# rev 30 Nov 03, ds
#	separated flux from surface processing
# rev 30 Mar 04, ds
#	fixed bug in array indexing which skipped over all data at 23:30
# rev 1 April 04, ds
#	no fooling! really added extensive flagging per DQR reports, per Scot
# rev 23 July 05, ds
#   added processing for ECOR data
# rev 08 Aug 05, ds
#   added check on site IDs so that those without any data for a day are
#     still printed out with all missing values, as long as they have data
#     for at least one day within the Time of Interest
# rev 20 Apr 10, ds
#   renamed ECOR params to match EBBR names (see lines 399- 403)
#   this version of s/w is ONLY for data from 2008 and before
# rev 18 Jun 10, ds
#   flagging changes added from Scot's email of June 4th
#--------------------------------------------------------------------------------------
#    IMPORTANT:  If values for home_15 or home_30 are corrupted, the following
#    fields should be considered suspect; inspection of the 5 and 15 minute home
#    signal data is required to determine validity:  e, h.
#--------------------------------------------------------------------------------------

$DEBUG = 1;
$DEBUG1 = 0;				# for even more messages

#-----------------------------
# get our subroutines in
#-----------------------------
#   none required
#-----------------------------

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
@ebbr_fields  = ("time_offset", "c_shf1", "g1", "e", "qc_e", "h", "qc_h");		  # corrected soil heat flow 1, soil heat flow at the surface 1, latent heat flux, sensible heat flux
@ecor_fields  = ("time_offset", "h", "qc_h", "lv_e", "qc_lv_e", "fc", "qc_fc");   # sensible heat flux, latent heat flux, co2 flux

%params = (
    "ebbr"  => \@ebbr_fields,
	"ecor"  => \@ecor_fields
);

#--------------------------------------------------------------------------------------
# a list of the parameters we want, in order as printed to the output files
#--------------------------------------------------------------------------------------

# (param num)       =   1    2     3      4 
@flx_parameter_list = ("h", "e", "fc", "c_shf1");
@flx_obs			= ("sens_flux", "lat_flux", "CO2_flux", "soil_flux");
$flx_param_count	= 4;
$sfc_param_count    = 0;

#--------------------------------------------------------------------------------------
# where the data files are for input (previously created from the netCDF files)
#--------------------------------------------------------------------------------------
%dirs = (
    "ebbr"  => "EBBR/",
	"ecor"  => "ECOR/"
);

%filename_pattern = (
    "ebbr"  => 'sgp30ebbr([CE]\d{1,3}).{4}(\d{4})(\d{2})(\d{2})\.(\d{2})(\d{2})',
    "ecor"  => 'sgp30ecor([CE]\d{1,3}).{4}(\d{4})(\d{2})(\d{2})\.(\d{2})(\d{2})',
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
$CSE_id       = "CPPA";				# version2
$site_id      = "SGP";
$platform_id  = "XXXX";             # e.g. "SMOS"           
$stn_id       = "Exx";              # e.g. "E18"

$network            = "ARM_$platform_id";
$project_begin_date = 20050101;     # version2 NOTE: this s/w only for 2008 and before! (see header note for 4/20/10)
$project_end_date   = 20081231;     

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
    #-----------------------------
    # divide up by dataset type 
    #-----------------------------
    push (@flx_infiles,  @this_dir);
    closedir(FILEDIR);
}

#-----------------------------
# sort infile lists by  date
#-----------------------------
@flx_infiles = sort file_date_sort (@flx_infiles);

#-----------------------------
# set default values in our
# output arrays
#-----------------------------
&clear_array("flx");

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
        my @flx_date_array = grep(/$the_date/, @flx_infiles);   # get all files for flx and this date into an array
        #-----------------------------
        # put refs to arrays into hash
        #-----------------------------
        $flx{$the_date} = \@flx_date_array;             # get reference to the date_array and store in flx hash
        print ("the files for flx $the_date = @{$flx{$the_date}}\n") if ($DEBUG1);
    }
  }
}
 
#-----------------------------
# files out
#-----------------------------
 
$outfile2 = $CSE_id . "_" . $site_id . "_" . $site_id . "_" . $project_begin_date . "_" . $project_end_date . ".flx";
# $station_out = $CSE_id . "_".$site_id."_station.out";
# $CD_station_out = $CSE_id . "_".$site_id."_stationCD.out";
# $stn_id_out = $CSE_id . "_".$site_id."_stn_id.out";

#-----------------------------------------------------------------------------
# Open files used in conversion
#-----------------------------------------------------------------------------

open (OUTFILE_FLX, ">./out/$outfile2") || die "Can't open $outfile2";
# open (STNSOUT1, ">./out/$station_out") || die "Can't open $station_out";
# open (STNSOUT2, ">./out/$CD_station_out") || die "Can't open $CD_station_out";
# open (STNSOUT3, ">./out/$stn_id_out") || die "Can't open $stn_id_out";

writeHeader("flx") if ($DEBUG);

#-----------------------------
# set default values in our
# output arrays
#-----------------------------

foreach $obs (@flux_obs) {
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
# start reading in the flux data, a day at a time
#-----------------------------------------------------------------------------

foreach $date (sort keys (%flx)) {                              	# get each date in sorted order
	print ("date = $date\n") if ($DEBUG);
	$num_files = @{$flx{$date}};
   	print "have $num_files files for $date\n" if ($DEBUG);
	if($num_files == 0) {
        $date_str = $date;
        substr($date_str, 4, 0) = "/";
        substr($date_str, 7, 0) = "/";
        print ("Will write all missing data for station $stn_id on $date_str\n") if ($DEBUG);
	}
	foreach $infile (@{$flx{$date}}) {                            	# now read in the filenames one at a time
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
	                if ($line_value[0] eq $obsName.":") {										# the $#line_value gives us index of last obs (which is 49)
	                    $numObs = (@line_value - 2) if ($line_value[0] eq "time_offset:");      # the line includes obs name at beginning of line and semi-colon at end  
			#			print "the number of obs for $obsName in $infile for $platform = $numObs, last uncounted obs on line = $line_value[$#line_value]\n";

						if ($platform eq "ebbr" || $platform eq "ecor") {
	                    	$thisNumObs = @line_value - 2;       # includes obs name at beginning of line and semi-colon at end  
	 	                	print ("this obs = $obsName and the index = $index with $numObs number of obs, while line value 0 = $line_value[0] and obs name = $obsName\n") if ($DEBUG1);
	                    	die "b: Different number of observations in the $line_value[0] line; expecting $numObs, but have $thisNumObs\n" if ($numObs != $thisNumObs);

		                    #------------------------------------------------------------
		                    # Get rid of the commas after the values, change
	    	                # any -0.0 values to 0.0, and "NAN" to -999.99,
							# and set to Missing if less than -899.
	            	        #------------------------------------------------------------
	    
	                	    for ($i=1; $i <= $numObs; $i++) { 	# real obs run from 1 to numObs
	                    	    $line_value[$i] =~ s/,//g;
	                        	$line_value[$i] = $MISSING if ($line_value[$i] eq "nan");
	                        	$line_value[$i] = $MISSING if ($line_value[$i] =~ /e+/);				# catch very large numbers in data
	                       		$line_value[$i] = $MISSING if (($line_value[$i] < -899) || ($line_value[$i] == 99999) || ($line_value[$i] == 6999));
	                			$line_value[$i] = 0.00 if (sprintf("%8.2f", $line_value[$i]) == -0.00); 
	                    	}

		                    #------------------------------------------------------------
							# switch the sign of soil heat flux and flow, 
							# and latent and sensible heat fluxes, for EBBR ONLY,
							# and CO2 flux for ECOR
		                    #------------------------------------------------------------

							if ($obsName eq "c_shf1" || $obsName eq "g1" || $obsName eq "fc") {			# soil heat flux and flow, and CO2 flux, Scot decided that we should switch the sign for EBBR 
	        	             	for ($i=1; $i <= $numObs; $i++) {
									$line_value[$i] *= -1.0 unless ($line_value[$i] == 0 || $line_value[$i] == -999.99 || $line_value[$i] > 9999);
								}
							} elsif (($obsName eq "e" || $obsName eq "h") && ($platform eq "ebbr")) { 	# sensible and latent heat fluxes, Scot decided that we should switch the sign for EBBR 
	                    		for ($i=1; $i <= $numObs; $i++) {
									$line_value[$i] *= -1.0 unless ($line_value[$i] == 0 || $line_value[$i] == $MISSING || $line_value[$i] > 9999);
								}
							}

		                    #------------------------------------------------------------
							# rename ecor params to match ebbr names
		                    #------------------------------------------------------------
							$obsName = "e" if $obsName eq "lv_e";
							$obsName = "qc_e" if $obsName eq "qc_lv_e";


	                    	print "and the line values are: @line_value\n" if ($DEBUG1);
	
		                    #------------------------------------------------------------
		                    # Put into an array named after the the variable
		                    #------------------------------------------------------------
	    	                @{$obsName} = @line_value;
	        	            print "this array of $obsName is @{$obsName}\n" if ($DEBUG1);

						}												# <---- end if ebbr or ecor
                    }                                                   # <---- end if match obsName and param
                }                                                       # <---- end foreach params
            }                                                           # <---- end all other params
        }                                                               # <---- end while infile line

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
        print ("Will write data for station $stn_id on $date_str\n") if ($DEBUG);

        #-----------------------------------------------------------------------
        # Put the values of the obs into a hash of arrays prepared by the
        # clear_array() subroutine, and indexed by the station and time.
        #-----------------------------------------------------------------------

	    $j = 0;               				# Note: params are indexed in the output line array by number
		foreach $param (@flx_parameter_list) {
	        if (defined(@{$param})) {
				$qc_flags = "qc_".$param;
			    for ($obsNum = 1; $obsNum <= $numObs; $obsNum++) {      		# get every value, one at a time
	        		@this_gmtime = gmtime($baseTime + $time_offset[$obsNum]);
	        		$min = $this_gmtime[1];
	        		$hour = $this_gmtime[2];
					print "this qc_flags = $qc_flags, and the line of values = @{$qc_flags}\n" if($obsNum == 2 && $DEBUG1); 
		        	$flx_out{$stn_id}[$hour][$min][$j] = @{$param}[$obsNum];
		        	$flx_flag{$stn_id}[$hour][$min][$j] = @{$qc_flags}[$obsNum] if(defined(@{$qc_flags}));
					print "at $hour:$min on $date_str for $stn_id, \tj = $j, obsnum = $obsNum, obs=$flx_out{$stn_id}[$hour][$min][$j], \tflag=$flx_flag{$stn_id}[$hour][$min][$j]\n" if($DEBUG1); 
	    		} # <----- end for obsNum   
			}
		    $j++;
		} # <---- end foreach param

	    #------------------------------------------------------------
		# we have the values, so clear out the params arrays used
	    #------------------------------------------------------------
		foreach $param ("time_offset", @flx_parameter_list) {
			undef(@{$param});
			$qc_flags = "qc_".$param;
			undef(@{$qc_flags});
		}
	    if ($obsNum-1 != $numObs) {
	        printf ("*** Had %d number of observations in $filename, but was expecting %d!\n", $obsNum - 1, $numObs);
			die "Wrong number, let's stop!";
	    }
    }   # <----- end foreach infile

	&writeDate("flx", $date_str, $platform);
	undef %flx_out;
	undef %flx_flag;
    &clear_array("flx");
    foreach $id (keys %stnlist) {
        undef($stnlist{$id});
    }
}   # <----- end foreach date, flx

close (OUTFILE_FLX);

#----------------------------------------------------------------------------------------
# Set up the array (@flx_out) which will feed the values for all the
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
	} elsif ($array_name eq "flx_out") {
    	$end_num = $flx_param_count + 1;
	} else {
        die "don't know this array to clear: $array_name\n";
	}
    foreach $stn (keys (%stn_name)) {
    	for $hour_num (0..24) {
			for $min_num (0..60) {
	      		for $param_num (0..$end_num) {
                	${$array_name}{$stn}[$hour_num][$min_num][$param_num] = $MISSING;
                    ${$flags_name}{$stn}[$hour_num][$min_num][$param_num] = -99;
#					print "We are in clear array, and the $flags_name for $stn at $hour_num:$min_num, param num. $param_num was set to -99!!\n" if($DEBUG1);
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
    } elsif ($out_type eq "flx") {
		print OUTFILE_FLX "   date    time     date    time    CSE ID      site ID        station ID        lat        lon      elev    height sensible f  latent  f     CO2  f   soil   f";
		print OUTFILE_FLX "\n";
		print OUTFILE_FLX "---------- ----- ---------- ----- ---------- --------------- --------------- ---------- ----------- ------- -------"; 
		for ($i=0; $i<4; $i++) {
			print OUTFILE_FLX (" -------- -"); 
		}
		print OUTFILE_FLX "\n";
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
	local ($met_type, $the_date, $this_platform) = @_;
	my $have_precip = 0;
	print "in writeDate, writing $met_type data for $this_platform on $the_date\n\n" if($DEBUG);

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
	}
}


# --------------------------------------------------------------------------------   
##  writeFLXline - write the flux data values to the FLX output file. 
# 
#  input:   
#       $stn        the station with the readings
#		$date
#       $hour 
#       $min 
#
#       global values:
#					$real_min, and all the data obs
#
#  output:  a single line of surface data for that time
#--------------------------------------------------------------------------------   

sub writeFLXline
{ 
    local ($id, $date, $hour, $min) = @_;
    $long_name = $stn_name{$id};
	my $lat  = ${$id}{lat};
	my $lon  = ${$id}{lon};
	my $elev = ${$id}{elev};
	my $the_platform = "ebbr";

	if ($id eq E1 ||  $id eq E3 || $id eq E5 || $id eq E6 || $id eq E10 || $id eq E14 || $id eq E16 || $id eq E21 || $id eq E24 ) {
		$the_platform = "ecor";
	} 

	$c_shf1_depth = -0.05;
	$g1_depth	  = $MISSING;
	$ecor_height  = 3;
	$E21_height   = 15;

	if ($the_platform eq "ebbr") {
 		$sensor_height = $g1_depth; 
	} elsif ($the_platform eq "ecor") {
		$sensor_height = $ecor_height;
		if ($id eq "E21") {
			$sensor_height = $E21_height;
		}
	} else {
		die "need a platform in writeFLXline! it is now: $the_platform\n";
	}

	print "in writeFLXline the platform passed in is: $the_platform.\n" if($DEBUG);

    #------------------------------------------------------------
	# Put the hash values into scalar values named after the obs
    #------------------------------------------------------------
	$print = 0;
    $j = 0;
    foreach $obs (@flx_obs) {
		${$obs} = $flx_out{$id}[$hour][$min][$j];
		$the_flag = $obs."_flag";
		${$the_flag} = $flx_flag{$id}[$hour][$min][$j];
        print ("for $stn_name{$id}, at $hour:$min $obs: $flx_out{$id}[$hour][$min][$j], which is same as ${$obs}, and flag is ${$the_flag}, which is the same as $flx_flag{$id}[$hour][$min][$j]\n") if ($DEBUG1);
		$print = 1 if (${$obs} != $MISSING);
		$j++;
	} 
    
    #------------------------------------------------------------
	# decision made to include empty records, 12 May 04, per Scot
    #------------------------------------------------------------
	# return ("empty line") if ($print == 0);							# don't print lines with all obs missing
    #------------------------------------------------------------

    #------------------------------------------------------------
    # Print out all but the soil heat flux values to the FLX file 
	#   (heat flux has separate depth, so separate line).
    #------------------------------------------------------------       
   
  	$sens_flux_flag = &get_flag(\$sens_flux, $sens_flux_flag, "sensible", $id, $date, $hour, $min);
  	$lat_flux_flag = &get_flag(\$lat_flux, $lat_flux_flag, "latent", $id, $date, $hour, $min);
  	$CO2_flux_flag = &get_flag(\$CO2_flux, $CO2_flux_flag, "co2", $id, $date, $hour, $min);
    $soil_flux_flag = &get_flag(\$soil_flux, $soil_flux_flag, "soil", $id, $date, $hour, $min);
 
    printf OUTFILE_FLX ("%10s %02d:%02d %10s %02d:%02d %-10s %-15s %-15s %10.5f %11.5f %7.2f %7.2f", $date, $hour, $min, $date, $hour, $min, $CSE_id, $site_id, $long_name, $lat, $lon, $elev, $sensor_height);
    #   format -   yyyy/mm/dd hh:mm yyyy/mm/dd hh:mm CSE_id site_id station_id dec_lat dec_lon elevation sensor_height
  
    #------------------------------------------------------------
    # Print out the sensible heat flux
    #------------------------------------------------------------ 
          
    printf OUTFILE_FLX (" %8.2f", $sens_flux);
  	printf OUTFILE_FLX (" %s", $sens_flux_flag);

    #------------------------------------------------------------
    # Print out the latent heat flux
    #------------------------------------------------------------ 
          
    printf OUTFILE_FLX (" %8.2f", $lat_flux);
	printf OUTFILE_FLX (" %s", $lat_flux_flag);
  
    #------------------------------------------------------------
    # Print out the CO2 flux
    #------------------------------------------------------------ 
            
    printf OUTFILE_FLX (" %8.2f", $CO2_flux);
    printf OUTFILE_FLX (" %s", $CO2_flux_flag);
  
    #------------------------------------------------------------
    # Print out the soil heat flux. 
	# (separate line used for EBBR, because has separate height)
    #------------------------------------------------------------ 

	if ($the_platform eq "ecor") {
      printf OUTFILE_FLX (" %8.2f", $soil_flux);
      printf OUTFILE_FLX (" %s", $soil_flux_flag);
      print OUTFILE_FLX ("\n");
	} elsif ($the_platform eq "ebbr") {
      printf OUTFILE_FLX (" %8.2f", $MISSING);
   	  printf OUTFILE_FLX (" %s", "M");
      print OUTFILE_FLX ("\n");

      #------------------------------------------------------------
      # Now print out only the soil heat flux values to the FLX file 
      #------------------------------------------------------------       

   	  $sensor_height = $c_shf1_depth; 
      printf OUTFILE_FLX ("%10s %02d:%02d %10s %02d:%02d %-10s %-15s %-15s %10.5f %11.5f %7.2f %7.2f", $date, $hour, $min, $date, $hour, $min, $CSE_id, $site_id, $long_name, $lat, $lon, $elev, $sensor_height);
      #   format -   yyyy/mm/dd hh:mm yyyy/mm/dd hh:mm CSE_id site_id station_id dec_lat dec_lon elevation sensor_height
  
	  for ($i=0; $i < 3; $i++) { 
      	printf OUTFILE_FLX (" %8.2f", $MISSING);
   	  	printf OUTFILE_FLX (" %s", "M");
	  }          
  
      #------------------------------------------------------------
      # Print out the soil heat flux
      #------------------------------------------------------------ 
            
      printf OUTFILE_FLX (" %8.2f", $soil_flux);
      printf OUTFILE_FLX (" %s", $soil_flux_flag);
 
      print OUTFILE_FLX ("\n");
	}
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
        if ($var eq "co2") {
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
        if ($var eq "co2") {
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
        if ($var eq "co2") {
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
        if ($var eq "soil") {
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
        if ($var eq "co2") {
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
        if ($var eq "co2") {
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
        if ($var eq "co2") {
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
        if ($var eq "co2") {
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
        if ($var eq "co2") {
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
        if ($var eq "co2") {
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
        if ($var eq "latent") {
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
        if ($var eq "sensible") {
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
    # E12
    #---------------------------------------------------------------------------
    if ($id eq "E12") {
        if ($var eq "soil") {
           if ($datetime >= 20051208.1130 && $datetime <= 20051208.1300) {
               $new_flag = "B";
           }
        }
    #---------------------------------------------------------------------------
    # E13
    #---------------------------------------------------------------------------
    } elsif ($id eq "E13") {
        if ($var eq "sensible") {
           if ($datetime >= 20080303.0000 && $datetime <= 20080303.0000) {
               $new_flag = "D";
           }
           if ($datetime >= 20050107.1830 && $datetime <= 20050107.1900) {
               $new_flag = "D";
           }
        }
        if ($var eq "soil") {
           if ($datetime >= 20061017.1800 && $datetime <= 20061022.1000) {
               $new_flag = "B";
           }
           if ($datetime >= 20060828.0030 && $datetime <= 20060828.0730) {
               $new_flag = "B";
           }
        }
    #---------------------------------------------------------------------------
    # E16
    #---------------------------------------------------------------------------
    } elsif ($id eq "E16") {
        if ($var eq "sensible") {
           if ($datetime >= 20080302.2200 && $datetime <= 20080302.2200) {
               $new_flag = "D";
           }
           if ($datetime >= 20060617.0030 && $datetime <= 20060617.0030) {
               $new_flag = "D";
           }
           if ($datetime >= 20050820.2230 && $datetime <= 20050820.2230) {
               $new_flag = "D";
           }
           if ($datetime >= 20050131.1530 && $datetime <= 20050131.1530) {
               $new_flag = "D";
           }
        }
    #---------------------------------------------------------------------------
    # E18
    #---------------------------------------------------------------------------
    } elsif ($id eq "E18") {
        if ($var eq "soil") {
           if ($datetime >= 20070802.0430 && $datetime <= 20070802.0930) {
               $new_flag = "D";
           }
           if ($datetime >= 20070428.0300 && $datetime <= 20070508.0530) {
               $new_flag = "D";
           }
        }
    #---------------------------------------------------------------------------
    # E19
    #---------------------------------------------------------------------------
    } elsif ($id eq "E19") {
        if ($var eq "soil") {
           if ($datetime >= 20070512.0500 && $datetime <= 20070512.0530) {
               $new_flag = "D";
           }
           if ($datetime >= 20070510.1930 && $datetime <= 20070510.2030) {
               $new_flag = "D";
           }
           if ($datetime >= 20071111.2100 && $datetime <= 20071112.0800) {
               $new_flag = "D";
           }
           if ($datetime >= 20061125.2100 && $datetime <= 20061125.2200) {
               $new_flag = "D";
           }
           if ($datetime >= 20061124.1630 && $datetime <= 20061124.2130) {
               $new_flag = "D";
           }
           if ($datetime >= 20061123.2200 && $datetime <= 20061123.2200) {
               $new_flag = "D";
           }
        }
    #---------------------------------------------------------------------------
    # E20
    #---------------------------------------------------------------------------
    } elsif ($id eq "E20") {
        if ($var eq "soil") {
           if ($datetime >= 20091123.1630 && $datetime <= 20091125.1330) {
               $new_flag = "D";
           }
           if ($datetime >= 20060222.0300 && $datetime <= 20060225.1630) {
               $new_flag = "D";
           }
           if ($datetime >= 20051221.1000 && $datetime <= 20051222.1430) {
               $new_flag = "D";
           }
        }
    #---------------------------------------------------------------------------
    # E22
    #---------------------------------------------------------------------------
    } elsif ($id eq "E22") {
        if ($var eq "soil") {
           if ($datetime >= 20081031.2130 && $datetime <= 20081102.2230) {
               $new_flag = "D";
           }
           if ($datetime >= 20071021.2000 && $datetime <= 20071120.1830) {
               $new_flag = "D";
           }
           if ($datetime >= 20070723.1700 && $datetime <= 20070801.1900) {
               $new_flag = "D";
           }
           if ($datetime >= 20070720.0630 && $datetime <= 20070720.1630) {
               $new_flag = "D";
           }
        }
    #---------------------------------------------------------------------------
    # E26
    #---------------------------------------------------------------------------
    } elsif ($id eq "E26") {
        if ($var eq "soil") {
           if ($datetime >= 20091014.0030 && $datetime <= 20091017.1530) {
               $new_flag = "D";
           }
           if ($datetime >= 20090313.0030 && $datetime <= 20090326.1530) {
               $new_flag = "D";
           }
           if ($datetime >= 20081219.0730 && $datetime <= 20081231.1500) {
               $new_flag = "D";
           }
           if ($datetime >= 20071209.0700 && $datetime <= 20071209.1100) {
               $new_flag = "D";
           }
           if ($datetime >= 20060221.0430 && $datetime <= 20060225.1630) {
               $new_flag = "D";
           }
           if ($datetime >= 20051220.0900 && $datetime <= 20051222.0830) {
               $new_flag = "D";
           }
        }
    #---------------------------------------------------------------------------
    # E27
    #---------------------------------------------------------------------------
    } elsif ($id eq "E27") {
        if ($var eq "soil") {
           if ($datetime >= 20071215.0200 && $datetime <= 20071216.1330) {
               $new_flag = "D";
           }
           if ($datetime >= 20080926.1600 && $datetime <= 20081003.1930) {
               $new_flag = "D";
           }
           if ($datetime >= 20080924.1400 && $datetime <= 20080924.1530) {
               $new_flag = "D";
           }
           if ($datetime >= 20080325.1000 && $datetime <= 20080325.1030) {
               $new_flag = "D";
           }
           if ($datetime >= 20070716.1230 && $datetime <= 20070716.1230) {
               $new_flag = "D";
           }
           if ($datetime >= 20070517.1800 && $datetime <= 20070517.1830) {
               $new_flag = "D";
           }
           if ($datetime >= 20060811.1730 && $datetime <= 20060821.1700) {
               $new_flag = "D";
           }
           if ($datetime >= 20060714.0430 && $datetime <= 20060719.0600) {
               $new_flag = "D";
           }
           if ($datetime >= 20060524.2330 && $datetime <= 20060617.0100) {
               $new_flag = "D";
           }
           if ($datetime >= 20060506.1430 && $datetime <= 20060518.1930) {
               $new_flag = "D";
           }
           if ($datetime >= 20060315.1330 && $datetime <= 20060321.1230) {
               $new_flag = "D";
           }
           if ($datetime >= 20060311.1330 && $datetime <= 20060311.1630) {
               $new_flag = "D";
           }
           if ($datetime >= 20060310.1730 && $datetime <= 20060310.1930) {
               $new_flag = "D";
           }
        }
    #---------------------------------------------------------------------------
    # E7
    #---------------------------------------------------------------------------
    } elsif ($id eq "E7") {
        if ($var eq "soil") {
           if ($datetime >= 20070822.1600 && $datetime <= 20070822.1630) {
               $new_flag = "D";
           }
           if ($datetime >= 20061226.1500 && $datetime <= 20061226.1900) {
               $new_flag = "D";
           }
           if ($datetime >= 20061221.1600 && $datetime <= 20061222.2230) {
               $new_flag = "D";
           }
           if ($datetime >= 20061213.1500 && $datetime <= 20061216.2200) {
               $new_flag = "D";
           }
           if ($datetime >= 20061127.2100 && $datetime <= 20061128.1500) {
               $new_flag = "D";
           }
           if ($datetime >= 20061116.0630 && $datetime <= 20061116.1400) {
               $new_flag = "D";
           }
        }
    #---------------------------------------------------------------------------
    # E8
    #---------------------------------------------------------------------------
    } elsif ($id eq "E8") {
        if ($var eq "soil") {
           if ($datetime >= 20090813.0830 && $datetime <= 20090819.1500) {
               $new_flag = "D";
           }
           if ($datetime >= 20050421.0300 && $datetime <= 20050425.0700) {
               $new_flag = "D";
           }
        }
    }

	return $new_flag;
}

