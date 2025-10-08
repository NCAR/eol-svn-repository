#!/opt/bin/perl -w

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
#--------------------------------------------------------------------------------------
#    IMPORTANT:  If values for home_15 or home_30 are corrupted, the following
#    fields should be considered suspect; inspection of the 5 and 15 minute home
#    signal data is required to determine validity:  e, h.
#--------------------------------------------------------------------------------------

$DEBUG = 0;
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
$CSE_id       = "GAPP";
$site_id      = "SGP";
$platform_id  = "XXXX";             # e.g. "SMOS"           
$stn_id       = "Exx";              # e.g. "E18"

$network            = "ARM_$platform_id";
$project_begin_date = 20031001;     # EOP4
$project_end_date   = 20041231;     

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

open (OUTFILE_FLX, ">./out/final/$outfile2") || die "Can't open $outfile2";
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
	# following section fixes flags according to DQRs, and Scot
	#---------------------------------------------------------------------------
	# ECOR
    #---------------------------------------------------------------------------
    # E1
    #---------------------------------------------------------------------------
    if ($id eq "E1") {
        if ( $var eq "sensible" || $var eq "latent" || $var eq "co2") {
           if ($datetime >= 20041019.2030 && $datetime <= 20041025.1735) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E3
    #---------------------------------------------------------------------------
    } elsif ($id eq "E3") {
        if ( $var eq "sensible" || $var eq "latent" || $var eq "co2") {
           if ($datetime >= 20041219.1353 && $datetime <= 20041220.0931) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "latent" || $var eq "co2") {
           if ($datetime >= 20040616.1730 && $datetime <= 20040630.1730) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E6
    #---------------------------------------------------------------------------
    } elsif ($id eq "E6") {
        if ( $var eq "latent" || $var eq "co2") {
           if ($datetime >= 20040705.2200 && $datetime <= 20040823.1200) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E10
    #---------------------------------------------------------------------------
    } elsif ($id eq "E10") {
        if ( $var eq "sensible" || $var eq "latent" || $var eq "co2") {
           if ($datetime >= 20040714.0100 && $datetime <= 20040813.1800) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ( $var eq "sensible" || $var eq "latent" || $var eq "co2") {
           if ($datetime >= 20031203.2200 && $datetime <= 20031205.2030) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ( $var eq "latent" ) {
           if ($datetime >= 20041214.2230) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E16
    #---------------------------------------------------------------------------
    } elsif ($id eq "E16") {
        if ( $var eq "sensible" || $var eq "latent" || $var eq "co2") {
           if ($datetime >= 20040112.0000 && $datetime <= 20040428.1500) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E24
    #---------------------------------------------------------------------------
    } elsif ($id eq "E24") {
        if ( $var eq "sensible" || $var eq "latent" || $var eq "co2") {
           if ($datetime >= 20040604.1000 && $datetime <= 20040916.1730) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
	# EBBR
    #---------------------------------------------------------------------------
    # E2
    #---------------------------------------------------------------------------
    } elsif ($id eq "E2") {
        if ($var eq "sensible" || $var eq "latent") {
           if ($datetime >= 20041002.0630 && $datetime <= 20041021.1500) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "c_shf1" || $var eq "g1") {
           if ($datetime >= 20040411.1830 && $datetime <= 20040601.1800) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ( $var eq "c_shf1" || $var eq "g1" || $var eq "latent" || $var eq "sensible") {
           if ($datetime >= 20031009.1800 && $datetime <= 20031009.1800) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "sensible" || $var eq "latent") {
           if (($datetime >= 20031009.1700 && $datetime <= 20031009.1730) ||
               ($datetime >= 20031214.1500 && $datetime <= 20031214.2200) ||
               ($datetime >= 20040831.1300 && $datetime <= 20040909.1500) ||
               ($datetime >= 20041021.1500 && $datetime <= 20041021.1530)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "latent") {
           if ($datetime == 20031028.2300 || $datetime == 20041111.0600) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }

    #---------------------------------------------------------------------------
    # E4
    #---------------------------------------------------------------------------
    } elsif ($id eq "E4") {
        if ($var eq "sensible" || $var eq "latent") {
           if ($datetime >= 20040725.1200 && $datetime <= 20040728.1500) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "sensible" || $var eq "latent") {
           if (($datetime >= 20031008.1530 && $datetime <= 20031008.1645) ||
               ($datetime >= 20041020.1600 && $datetime <= 20041020.1630)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E7
    #---------------------------------------------------------------------------
    } elsif ($id eq "E7") {
        if ($var eq "sensible" || $var eq "latent") {
           if (($datetime >= 20030727.1330 && $datetime <= 20031007.2000) ||
               ($datetime >= 20031013.0000 && $datetime <= 20031118.2000)) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "sensible" || $var eq "latent") {
           if (($datetime >= 20031019.1730 && $datetime <= 20031019.1930) ||
               ($datetime >= 20031020.1330 && $datetime <= 20031020.1630) ||
               ($datetime >= 20031021.1630 && $datetime <= 20031021.1830) ||
               ($datetime >= 20031022.0000 && $datetime <= 20031022.1830) ||
               ($datetime >= 20031023.0000 && $datetime <= 20031023.1630) ||
               ($datetime >= 20031104.2200 && $datetime <= 20031104.2230) ||
               ($datetime >= 20040810.1925 && $datetime <= 20040824.1820) ||
               ($datetime >= 20041005.2000 && $datetime <= 20041005.2030)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E8
    #---------------------------------------------------------------------------
    } elsif ($id eq "E8") {
        if ($var eq "sensible" || $var eq "latent") {
           if ($datetime >= 20040405.0000 && $datetime <= 20040406.1755) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "sensible" || $var eq "latent") {
           if (($datetime >= 20031007.1700 && $datetime <= 20031007.1735) ||
               ($datetime >= 20031213.1400 && $datetime <= 20031214.2200) ||
               ($datetime >= 20040830.1200 && $datetime <= 20040907.1730) ||
               ($datetime >= 20041019.1730 && $datetime <= 20041019.1800)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E9
    #---------------------------------------------------------------------------
    } elsif ($id eq "E9") {
        if ($var eq "sensible" || $var eq "latent") {
           if (($datetime >= 20031104.1730 && $datetime <= 20031104.1800) ||
               ($datetime >= 20031214.1000 && $datetime <= 20031214.1930) ||
               ($datetime >= 20040628.1200 && $datetime <= 20040629.1530) ||
               ($datetime >= 20041005.1630 && $datetime <= 20041005.1700)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E12
    #---------------------------------------------------------------------------
    } elsif ($id eq "E12") {
        if ( $var eq "c_shf1" || $var eq "g1" || $var eq "latent" || $var eq "sensible") {
           if ($datetime >= 20040219.2212 && $datetime <= 20040310.2247) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "sensible" || $var eq "latent") {
           if (($datetime >= 20031014.1300 && $datetime <= 20031014.1800) ||
               ($datetime >= 20031028.1750 && $datetime <= 20031028.1845) ||
               ($datetime >= 20041012.1630 && $datetime <= 20041012.1700)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "latent") {
           if ($datetime >= 20041012.1700 && $datetime <= 20041020.2100) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E13
    #---------------------------------------------------------------------------
    } elsif ($id eq "E13") {
        if ($var eq "sensible" || $var eq "latent") {
           if (($datetime >= 20030929.1800 && $datetime <= 20031002.1845) ||
               ($datetime >= 20031013.1830 && $datetime <= 20031013.1900) ||
               ($datetime >= 20031214.1730 && $datetime <= 20031214.1830) ||
               ($datetime >= 20040811.0230 && $datetime <= 20040811.1824) ||
               ($datetime >= 20040908.1200 && $datetime <= 20040915.1930) ||
               ($datetime >= 20041028.1830 && $datetime <= 20041028.1900)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E15
    #---------------------------------------------------------------------------
    } elsif ($id eq "E15") {
        if ( $var eq "c_shf1" || $var eq "g1" || $var eq "latent" || $var eq "sensible") {
           if ($datetime >= 20011011.1500 && $datetime <= 20040928.1509) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "sensible" || $var eq "latent") {
           if (($datetime >= 20031011.2230 && $datetime <= 20031012.2359) ||
               ($datetime >= 20031013.0000 && $datetime <= 20031014.1830)) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "sensible" || $var eq "latent") {
           if (($datetime >= 20031011.2230 && $datetime <= 20031012.2330) ||
               ($datetime >= 20031105.1400 && $datetime <= 20031111.1800) ||
               ($datetime >= 20031014.1700 && $datetime <= 20031014.1730) ||
               ($datetime >= 20031213.1500 && $datetime <= 20031215.0000) ||
               ($datetime >= 20041026.1630 && $datetime <= 20041026.1700)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E18
    #---------------------------------------------------------------------------
    } elsif ($id eq "E18") {
        if ($var eq "sensible" || $var eq "latent") {
           if (($datetime >= 20040126.1915 && $datetime <= 20030413.2045) ||
               ($datetime >= 20040419.0500 && $datetime <= 20040613.1700) ||
               ($datetime >= 20040706.1830 && $datetime <= 20040706.2100)) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "g1") {
           if ($datetime >= 20040628.1300 && $datetime <= 20040728.1500) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "sensible" || $var eq "latent") {
           if (($datetime >= 20031028.2225 && $datetime <= 20031028.2315) ||
               ($datetime >= 20040928.2230 && $datetime <= 20040928.2300)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ( $var eq "latent" ) {
           if ($datetime >= 20040811.1730 && $datetime <= 20040818.0130) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E19
    #---------------------------------------------------------------------------
    } elsif ($id eq "E19") {
        if ( $var eq "c_shf1" || $var eq "g1" || $var eq "latent" || $var eq "sensible") {
           if ($datetime >= 20040108.1600 && $datetime <= 20040122.1530) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "sensible" || $var eq "latent") {
           if ($datetime >= 20040205.1700 && $datetime <= 20040415.1530) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ( $var eq "c_shf1" || $var eq "g1" || $var eq "latent" || $var eq "sensible") {
           if (($datetime >= 20031103.2000 && $datetime <= 20031113.1615) ||
               ($datetime >= 20031010.1800 && $datetime <= 20031010.1800) ||
               ($datetime >= 20040529.2215 && $datetime <= 20040610.1510)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "sensible" || $var eq "latent") {
           if (($datetime >= 20031114.2115 && $datetime <= 20031126.1600) ||
               ($datetime >= 20031010.1530 && $datetime <= 20031010.1600) ||
               ($datetime >= 20031016.1500 && $datetime <= 20031016.1530) ||
               ($datetime >= 20041028.1430 && $datetime <= 20041028.1500)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E20
    #---------------------------------------------------------------------------
    } elsif ($id eq "E20") {
        if ($var eq "sensible" || $var eq "latent" || $var eq "g1") {
           if ($datetime >= 20030927.1800 && $datetime <= 20031210.2230) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "sensible" || $var eq "latent") {
           if (($datetime >= 20040301.0000 && $datetime <= 20040331.2355) ||
               ($datetime >= 20040803.0300 && $datetime <= 20040915.0200) ||
               ($datetime >= 20040924.0700 && $datetime <= 20040929.1800) ||
               ($datetime >= 20040908.2130 && $datetime <= 20040915.1930) ||
               ($datetime >= 20041126.2100 && $datetime <= 20041208.2100)) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ( $var eq "c_shf1" || $var eq "g1" || $var eq "latent" || $var eq "sensible") {
           if ($datetime >= 20040915.1930 && $datetime <= 20040915.2030) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "sensible" || $var eq "latent") {
           if (($datetime >= 20031015.0210 && $datetime <= 20031029.0230) ||
               ($datetime >= 20030930.0000 && $datetime <= 20031001.2100) ||
               ($datetime >= 20031029.2200 && $datetime <= 20031029.2245) ||
               ($datetime >= 20040929.1730 && $datetime <= 20040929.1800)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E22
    #---------------------------------------------------------------------------
    } elsif ($id eq "E22") {
        if ($var eq "sensible" || $var eq "latent") {
           if (($datetime >= 20030929.1800 && $datetime <= 20031001.1630) ||
               ($datetime >= 20031015.1900 && $datetime <= 20031015.1930) ||
               ($datetime >= 20040622.0400 && $datetime <= 20040623.1650) ||
               ($datetime >= 20040701.1330 && $datetime <= 20040707.1900) ||
               ($datetime >= 20041027.1700 && $datetime <= 20041027.1730)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "sensible" || $var eq "latent") {
           if ($datetime >= 20041216.1730) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E26
    #---------------------------------------------------------------------------
    } elsif ($id eq "E26") {
        if ($var eq "sensible" || $var eq "latent") {
           if ($datetime >= 20030321.1530 && $datetime <= 20031012.2355) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ( $var eq "c_shf1" || $var eq "g1" || $var eq "latent" || $var eq "sensible") {
           if (($datetime >= 20041027.1930 && $datetime <= 20041105.1000) ||
               ($datetime >= 20041110.0600 && $datetime <= 20041124.1500) ||
               ($datetime >= 20041029.1100 && $datetime <= 20041029.1600)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "sensible" || $var eq "latent") {
           if (($datetime >= 20031030.1545 && $datetime <= 20031113.1545) ||
               ($datetime >= 20031030.1535 && $datetime <= 20031030.1615) ||
               ($datetime >= 20040930.1530 && $datetime <= 20040930.1600)) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "sensible" || $var eq "latent") {
           if ($datetime >= 20031201.0000 && $datetime <= 20040228.2330) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E27
    #---------------------------------------------------------------------------
    } elsif ($id eq "E27") {
        if ($var eq "sensible" || $var eq "latent") {
           if (($datetime >= 20031015.1030 && $datetime <= 20031117.0630) ||
               ($datetime >= 20030906.1200 && $datetime <= 20031011.0730) ||
               ($datetime >= 20031029.1830 && $datetime <= 20031029.1945) ||
               ($datetime >= 20030912.1700 && $datetime <= 20031012.2359) ||
               ($datetime >= 20031206.0900 && $datetime <= 20031206.0900) ||
               ($datetime >= 20040301.0000 && $datetime <= 20040331.2355) ||
               ($datetime >= 20040610.0600 && $datetime <= 20040721.1725)) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "sensible" || $var eq "latent") {
           if ($datetime >= 20040929.2000 && $datetime <= 20040929.2030) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ( $var eq "c_shf1" || $var eq "g1" || $var eq "latent" || $var eq "sensible") {
           if ($datetime >= 20040721.1700 && $datetime <= 20040721.1900) {
                $new_flag = "B";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    }

 	#---------------------------------------------------------------------------
	# following section fixes flags according to Scot, email of 9Aug05
	#---------------------------------------------------------------------------

	if ($var eq "latent") {
		$new_flag = "D" if($obs_value > 700 || $obs_value < -200);
	}

	if ($var eq "sensible") {
		$new_flag = "D" if($obs_value > 600 || $obs_value < -200);
	}

	if ($var eq "soil") {
		$new_flag = "D" if($obs_value > 150 || $obs_value < -100);
		$new_flag = "B" if($obs_value > 300 || $obs_value < -200);
	}

	return $new_flag;
}

