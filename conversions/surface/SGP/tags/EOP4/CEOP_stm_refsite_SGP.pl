#!/opt/bin/perl -w

#--------------------------------------------------------------------------------------
# CEOP_soil_refsite_SGP.pl
#
# This s/w is used in converting SGP soil netCDF files into CEOP output.
# It handles multiple platforms for multiple (C1-E24) sites.
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
# rev 11 Jan 04, ds
#	revised for soil processing
# rev 14 May 04, ds
#	flags added per Scot's email of 14May04
#	all missing records are now included in output
# rev 08 Aug 05, ds
#   added check on site IDs so that those without any data for a day are
#     still printed out with all missing values, as long as they have data
#     for at least one day within the Time of Interest
# rev 19 Sep 05, ds
#	missing values are only put out for hourly times, since the raw data is hourly
# rev 28 Sep 05, ds
#   levels that are missing for entire period are not put out, now
#   see email of 26 Sep 05 from Scot on which ones to exclude
#--------------------------------------------------------------------------------------
#    IMPORTANT:  If values for home_15 or home_30 are corrupted, the following
#    fields should be considered suspect; inspection of the 5 and 15 minute home
#    signal data is required to determine validity:  e, h.
#--------------------------------------------------------------------------------------

$DEBUG = 0;
$DEBUG1 = 0;				# for even more messages

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
@swats_fields = qw(time_offset depth tsoil_W qc_tsoil_W watcont_W qc_watcont_W);

%params = (
	"swats" => \@swats_fields
);

#--------------------------------------------------------------------------------------
# a list of the parameters we want, in order as printed to the output files
# depths = 5, 15, 25, 35, 60, 85, 125, 175cm
#--------------------------------------------------------------------------------------

# (index into array) =      0      1        2
@stm_parameter_list = qw( depth tsoil_W watcont_W);
$stm_param_count	= 3;
$num_depths = 8;

#--------------------------------------------------------------------------------------
# where the data files are for input (previously created from the netCDF files)
#--------------------------------------------------------------------------------------
%dirs = (
	"swats" => "SWATS/"
);

%filename_pattern = (
    "swats" => 'sgpswats([CE]\d{1,3}).{4}(\d{4})(\d{2})(\d{2})\.(\d{2})(\d{2})'
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
$zero_min = 0;
$thirty_min = 30;
$have_data = 0;
                               
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
    push (@stm_infiles,  @this_dir);
    closedir(FILEDIR);
}

#-----------------------------
# sort infile lists by date
#-----------------------------
@stm_infiles = sort file_date_sort (@stm_infiles);

#-----------------------------
# set default values in our
# output arrays
#-----------------------------
&clear_array($num_depths);

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
        my @stm_date_array = grep(/$the_date/, @stm_infiles);   # get all files for stm and this date into an array
        #-----------------------------
        # put refs to arrays into hash
        #-----------------------------
        $stm{$the_date} = \@stm_date_array;             # get reference to the date_array and store in stm hash
        print ("the files for stm $the_date = @{$stm{$the_date}}\n") if ($DEBUG1);
    }
  }
}
 
#-----------------------------
# files out
#-----------------------------
 
$outfile4 = $CSE_id . "_" . $site_id . "_" . $site_id . "_" . $project_begin_date . "_" . $project_end_date . ".stm";
# $station_out = $CSE_id . "_".$site_id."_station.out";
# $CD_station_out = $CSE_id . "_".$site_id."_stationCD.out";
# $stn_id_out = $CSE_id . "_".$site_id."_stn_id.out";

#-----------------------------------------------------------------------------
# Open files used in conversion
#-----------------------------------------------------------------------------

open (OUTFILE_STM, ">./out/final/$outfile4") || die "Can't open $outfile4";
# open (STNSOUT1, ">./out/$station_out") || die "Can't open $station_out";
# open (STNSOUT2, ">./out/$CD_station_out") || die "Can't open $CD_station_out";
# open (STNSOUT3, ">./out/$stn_id_out") || die "Can't open $stn_id_out";

writeHeader("stm") if ($DEBUG);

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
# start reading in the soil data, a day at a time
#-----------------------------------------------------------------------------

foreach $date (sort keys (%stm)) {                              	# get each date in sorted order
	print ("date = $date\n") if ($DEBUG);
	$num_files = @{$stm{$date}};
   	print "have $num_files files for $date\n" if ($DEBUG);
	if($num_files == 0) {
        $date_str = $date;
        substr($date_str, 4, 0) = "/";
        substr($date_str, 7, 0) = "/";
        print ("Will write all missing data for station $stn_id on $date_str\n") if ($DEBUG);
	}
	foreach $infile (@{$stm{$date}}) {                            	# now read in the filenames one at a time
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
	                if ($line_value[0] eq $obsName.":") {
	                    $thisNumObs = $#line_value - 2;       # includes obs name at beginning of line and semi-colon at end  
						print "obs are indexed from 0 - $thisNumObs for $obsName in $filename\n" if ($DEBUG1);

						if ($platform eq "swats") { 
							if ($obsName eq "time_offset") {
								shift(@line_value);		# get rid of obs name at beginning of line
								pop(@line_value);     	# get rid of semi-colon at end of line
		                    	$numObs = $#line_value;                 
		                    	die "Different number of observations in the $line_value[0] line; expecting $numObs, but have $thisNumObs\n" if ($numObs != $thisNumObs);
	
			                    #------------------------------------------------------------
			                    # Get rid of the commas after the values, change
		    	                # any -0.0 values to 0.0, and "NAN" to -999.99,
								# and set to Missing if less than -899.
		            	        #------------------------------------------------------------
		    
		                	    for ($i=0; $i <= $numObs; $i++) {
                       	        	$line_value[$i] =~ s/,//g;
                                	$line_value[$i] = $MISSING if ($line_value[$i] eq "nan");
                                	$line_value[$i] = $MISSING if ($line_value[$i] =~ /e+/);                # catch very large numbers in data
	                       			$line_value[$i] = $MISSING if (($line_value[$i] < -899) || ($line_value[$i] == 99999) || ($line_value[$i] == 6999));
                                	$line_value[$i] = 0.00 if (sprintf("%8.2f", $line_value[$i]) == -0.00);
	                            }

                            	print "and the line values are: @line_value\n" if ($DEBUG1);

                            	#------------------------------------------------------------
                            	# Put into an array named after the the variable
                            	#------------------------------------------------------------
                            	@{$obsName} = @line_value;

                            	print "this array of $obsName is @{$obsName}\n" if ($DEBUG1);
							} elsif ($obsName eq "depth") {
								shift(@line_value);		# get rid of obs name at beginning of line
								pop(@line_value);     	# get rid of semi-colon at end of line
	      						$numDepths = $#line_value;
		                	   	for ($i=0; $i <= $numDepths; $i++) {
		                    	   	$line_value[$i] =~ s/,//g;
								}
		    	                @{$obsName} = @line_value;
			        	        print "this array of $obsName is indexed from 0 to $numDepths; the values are: @{$obsName}\n\n" if ($DEBUG1);
							} else {
								$obsNum = 0;				
								shift(@line_value);		# get rid of obs name at beginning of first depth line
								do {
			                    	#------------------------------------------------------------
			                    	# Get rid of the commas after the values, change
		    	                	# any -0.0 values to 0.0, and "NAN" to -999.99,
									# also "_" to -999.99 (added 16 May 04, ds),
									# and set to Missing if less than -899.
		            	        	#------------------------------------------------------------
		    
		                	    	for ($i=0; $i <= $numDepths; $i++) {
                       	        		$line_value[$i] =~ s/,//g;
                                		$line_value[$i] = $MISSING if ($line_value[$i] eq "nan");
                                		$line_value[$i] = $MISSING if ($line_value[$i] eq "_");
                                		$line_value[$i] = $MISSING if ($line_value[$i] =~ /e+/);                # catch very large numbers in data
	                       				$line_value[$i] = $MISSING if (($line_value[$i] < -899) || ($line_value[$i] == 99999) || ($line_value[$i] == 6999));
                                		$line_value[$i] = 0.00 if (sprintf("%8.2f", $line_value[$i]) == -0.00);
		                    		}

	                				#--------------------------------------------------------------------------------------
	                				# Check that each line of values has the same number of observations as depths.
	                				#--------------------------------------------------------------------------------------
	
	   	                			$thisNumDepths = $#line_value;          	# w/o semi-colon at end
	       	            			die "x: Different number of depths in the $obsName line; expecting $numDepths, but have $thisNumDepths\n" if ($numDepths != $thisNumDepths);

									$depth_num = 0;
									foreach $obs (@line_value) {
										${$obsName}[$obsNum][$depth_num] = $obs;
			        	            	print "  $obsName: stn id $stn_id, date $this_date, obs num $obsNum, depth num $depth_num is: $obs\n" if ($DEBUG);
										$depth_num++;
									}
		
	    							$this_line = <INFILE>; 
	        						@line_value = split(" ", $this_line);
									$obsNum++;
	         						print "\nThis line within SWATS at obsNum = $obsNum is: $this_line" if ($DEBUG1);
	 							} while ($this_line !~ /;$/);					# we have multiple lines of depths, so get until end of this param

                                #------------------------------------------------------------
                                # Get rid of the commas after the values, change
                                # any -0.0 values to 0.0, and "NAN" to -999.99,
                                # and set to Missing if less than -899.
                                #------------------------------------------------------------
        
								pop(@line_value);     							# get rid of semi-colon at end of line
                                for ($i=0; $i <= $numDepths; $i++) {
                       	       		$line_value[$i] =~ s/,//g;
                               		$line_value[$i] = $MISSING if ($line_value[$i] eq "nan");
                               		$line_value[$i] = $MISSING if ($line_value[$i] =~ /e+/);                # catch very large numbers in data
	                       			$line_value[$i] = $MISSING if (($line_value[$i] < -899) || ($line_value[$i] == 99999) || ($line_value[$i] == 6999));
                               		$line_value[$i] = 0.00 if (sprintf("%8.2f", $line_value[$i]) == -0.00);
                                }

                                #--------------------------------------------------------------------------------------
                                # Check that each line of values has the same number of observations as times.
                                #  and each time has the same number of observations as depths. 
                                #--------------------------------------------------------------------------------------

		                    	die "d: Different number of observations in the $obsName line; expecting $numObs, but have $obsNum\n" if ($numObs != $obsNum);
                                $thisNumDepths = $#line_value;     
                                die "y: Different number of depths in the $obsName line; expecting $numDepths, but have $thisNumDepths\n" if ($numDepths != $thisNumDepths);

								$depth_num = 0;
								foreach $obs (@line_value) {
									${$obsName}[$obsNum][$depth_num] = $obs;
		         	            	print "  $obsName: stn id $stn_id, date $this_date, obs num $obsNum, depth num $depth_num is: $obs\n" if ($DEBUG);
									$depth_num++;
								}
		
								$soilObs = $obsNum;
				
								if ($DEBUG) {
                               		print "All the obs on $this_date for $obsName: \n";
                               		for ($this_ob=0; $this_ob <= $soilObs; $this_ob++) {
                                    	for ($this_depth=0; $this_depth <= $numDepths; $this_depth++) {
											@check_time =  gmtime($baseTime + $time_offset[$this_ob]);
   											$check_min = $check_time[1]; 
											$check_hr = $check_time[2];
                                        	print ("Look - stn id $stn_id in $platform for $obsName, on $this_date at $check_hr:$check_min, obs num $this_ob, depth num $this_depth, $depth[$this_depth] cm, is: ${$obsName}[$this_ob][$this_depth]\n") if ($DEBUG);
                                    	}
                                	}
								}										# <---- end if DEBUG
                            }											# <---- end else depths
                        }												# <---- end if platform swats
                    }                                                   # <---- end if match obsName and param
                }                                                       # <---- end foreach params
            }                                                           # <---- end all other params
        }                                                               # <---- end while infile line

        #--------------------------------------------------------------------------------------
        # Check that the date/time in the filename matches the date/time in the data.
		# Add the 1st time_offset to the base_time and convert to GMT to compare.
        # Add 1 to the month because gmtime() returns months indexed from 0 to 11.
        # Add 1900 to the year because gmtime() returns years starting at 0 for 1900.
        #--------------------------------------------------------------------------------------
        
        @begin_time = gmtime($baseTime + $time_offset[0]);
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
        for ($obsNum = 0; $obsNum <= $numObs; $obsNum++) {              # get every value, one at a time; obs name has been popped off, so start at index of 0
            @this_gmtime = gmtime($baseTime + $time_offset[$obsNum]);
            $min = $this_gmtime[1];
            $hour = $this_gmtime[2];

			#------------------------------------------------------------
	        # only print values on the half hour (SWATS data is hourly)
			# Swats soil temp and soil moisture data have depths 
			# multiply soil moisture values by 100 to get percent
			#------------------------------------------------------------
			foreach $param ("tsoil_W") {
				$qc_flags = "qc_".$param;
            	for ($i=0; $i <= $numDepths; $i++) {
	                $SoilTemp_out{$stn_id}[$hour][$min][$i] = ${$param}[$obsNum][$i];
	        		$SoilTemp_flag{$stn_id}[$hour][$min][$i] = ${$qc_flags}[$obsNum][$i] if(defined(@{$qc_flags}));
					if ($DEBUG) {
	           			print ("soilTemp: for $stn_id at obsnum $obsNum, time $hour:$min, param $param, this depth num $i, $depth[$i] cm: ${$param}[$obsNum][$i], flag = $SoilTemp_flag{$stn_id}[$hour][$min][$i]\n") if (defined(${$param}[$obsNum][$i]));
					}
	            }					# <---- end for this_depth
	        }           			# <---- end foreach param
			foreach $param ("watcont_W") {
				$qc_flags = "qc_".$param;
	            for ($i=0; $i <= $numDepths; $i++) {
	                $SoilMoist_out{$stn_id}[$hour][$min][$i] = ${$param}[$obsNum][$i] * 100 unless (${$param}[$obsNum][$i] == -999.99);		# convert to percent by multiplying by 100
	        		$SoilMoist_flag{$stn_id}[$hour][$min][$i] = ${$qc_flags}[$obsNum][$i] if(defined(@{$qc_flags}));
					if ($DEBUG) {
	              		print ("soilmois: for $stn_id at obsnum $obsNum, time $hour:$min, param $param this depth num $i, $depth[$i] cm: ${$param}[$obsNum][$i],  flag = $SoilMoist_flag{$stn_id}[$hour][$min][$i]\n") if (defined(${$param}[$obsNum][$i]));
					}
	            }						# <---- end for this_depth
	        }           				# <---- end foreach param
        }                  				# <---- end for obsNum   

	    #------------------------------------------------------------
		# clear out the params arrays used for the last file
	    #------------------------------------------------------------
		foreach $param ("time_offset", "depth", "tsoil_W", "qc_tsoil_W", "watcont_W", "qc_watcont_W") {
			undef(@{$param});
		}
	}									# <---- end for each infile

	&writeDate("stm", $date_str);

	undef %SoilMoist_out;
	undef $SoilMoist_flag;
	undef %SoilTemp_out;
	undef $SoilTemp_flag;
    &clear_array($num_depths);

    foreach $id (keys %stnlist) {
        undef($stnlist{$id});
    }
}   # <----- end foreach date, stm

close (OUTFILE_STM);


#----------------------------------------------------------------------------------------
## clear_array()
#
# Set up the hashes (SoilTemp and SoilMoist * _out and *_flag) which will feed the 
# values for all the parameters for one day into the output lines. This equates to 
# all the obs in all the files for one day. The hashes are indexed on each station ID, 
# each hour and minute in the day, and on the index for the depth.
#----------------------------------------------------------------------------------------

sub clear_array {
    my($depth_num) = @_;
    foreach $stn (keys (%stn_name)) {
    	for $hour_num (0..24) {
			for $min_num (0..60) {
	      		for $i (0..$depth_num) {
                	$SoilTemp_out{$stn}[$hour_num][$min_num][$i] = $MISSING;
                    $SoilTemp_flag{$stn}[$hour_num][$min_num][$i] = -99;
                	$SoilMoist_out{$stn}[$hour_num][$min_num][$i] = $MISSING;
                    $SoilMoist_flag{$stn}[$hour_num][$min_num][$i] = -99;
                }
            }
        }
	}
}


#----------------------------------------------------------------------------------------
## file_date_sort()
#
# Use this sort algorithm as the place to track stations with any data in this TOI.
#----------------------------------------------------------------------------------------

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
## writeHeader()
#
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
    } elsif ($out_type eq "stm") {
		print OUTFILE_STM "   date    time     date    time    CSE ID      site ID        station ID        lat        lon      elev    height soiltemp f soilmst  f";
		print OUTFILE_STM "\n";
		print OUTFILE_STM "---------- ----- ---------- ----- ---------- --------------- --------------- ---------- ----------- ------- -------"; 
		for ($i=0; $i<2; $i++) {
			print OUTFILE_STM (" -------- -"); 
		}
		print OUTFILE_STM "\n";
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

 	#------------------------------------------------------------       
    # Loop through the stns, and the hours and minutes,
    # and print the values at each half hour for each station.
	# Skip over completely missing records (flags = -99).
    # Note: for the new reference version, we are putting out
    #       30-min records. Therefore, if real UTC time < 15 OR
    #       real UTC time > 45, then nominal UTC time = 00, else
    #       nominal UTC time = 30.
	# Also: now printing all times for all stations in TOI.
    #------------------------------------------------------------       
    foreach $stn (@siteID) { 
		next if($stn !~ /[A-Z]\d+/);
        for ($hour=0; $hour < 24; $hour++) {
        	for ($real_min=0; $real_min < 60; $real_min++) {
	      	  for ($k=0; $k < $num_depths; $k++) {					
                next if (($SoilTemp_flag{$stn}[$hour][$real_min][$k] == -99) && ($SoilMoist_flag{$stn}[$hour][$real_min][$k] == -99)); 	# put out all missing records, now
				if ($real_min <= 15) {
					$min = 0;
					$real_hour = $hour;
					$real_date = $the_date;
					$zero_min = $real_min;
					print "zero min = $real_min, real hour = $real_hour, stn=$stn, date=$the_date, depth=$k\n" if($DEBUG);
					$have_data = 1;
					$k = $num_depths;
				} elsif ($real_min > 15 && $real_min <= 45) {
					$min = 30;
					$real_hour = $hour;
					$real_date = $the_date;
				#	$thirty_min = $real_min;
					$zero_min = $real_min;
					print "zero min = $real_min, real hour = $real_hour, stn=$stn, date=$the_date, depth=$k\n" if($DEBUG);
					$have_data = 1;
					$k = $num_depths;
				} elsif ($real_min > 45) {
					$min = 0;
					$real_hour = $hour;
					$real_date = $the_date;
					$yyyy = substr($the_date, 0, 4);
					$mm = substr($the_date, 5, 2);
					$dd = substr($the_date, 8, 2);
			      	for ($k = $num_depths-1; $k >= 0; $k--) {												# fill in the missing hour before the shift
       					&writeSTMline($stn, $the_date, $hour, 0, $real_date, $real_hour, 0, $k); 		# this works okay as long as date doesn't change
					}			# <---- end for depth_num
    				#------------------------------------------------------------       
	      			# for ($k=0; $k < $num_depths; $k++) {					
       				# 	&writeSTMline($stn, $the_date, $hour, 30, $real_date, $real_hour, 30, $k); 
					# }			# <---- end for depth_num
    				#------------------------------------------------------------       
					print ("will  increment hour, and possibly the day for $stn at $the_date (yyyy = $yyyy, mm = $mm, dd = $dd), $hour:$real_min! st = $SoilTemp_out{$stn}[$hour][$real_min][$k] st_flag = $SoilTemp_flag{$stn}[$hour][$real_min][$k], sm = $SoilMoist_out{$stn}[$hour][$real_min][$k] sm_flag = $SoilMoist_flag{$stn}[$hour][$real_min][$k]\n");
            		&local2UTC($yyyy, $mm, $dd, $hour, $min, 1);
					$the_date = $date_str;
					$zero_min = $real_min;
					print "zero min = $real_min, real hour = $real_hour, with hour change, stn=$stn, date=$the_date, depth=$k\n";
					$have_data = 1;
					$k = $num_depths;
				}
	  		  }         # <---- end foreach depth
			}			# <---- end minutes of hour 
			$real_date = $the_date unless ($real_min > 45 && $have_data == 1);
			$real_hour = $hour unless ($real_min > 45 && $have_data == 1);
			for ($k = $num_depths-1; $k >= 0; $k--) {												# fill in the missing hour before the shift
       			&writeSTMline($stn, $the_date, $hour, 0, $real_date, $real_hour, $zero_min, $k); 
			}			# <---- end for depth_num
			$real_hour = $hour;
    		#------------------------------------------------------------       
	      	# for ($k=0; $k < $num_depths; $k++) {					
       		# 	&writeSTMline($stn, $the_date, $hour, 30, $real_date, $real_hour, $thirty_min, $k); 
			# }			# <---- end for depth_num
    		#------------------------------------------------------------       
			$have_data = 0;
			$zero_min = 0;
			$thirty_min = 30;
		}				# <---- end hour
	  }					# <---- end foreach stn
}


# --------------------------------------------------------------------------------   
##  writeSTMline - write the soil data values to the STM output file. 
# 
#  input:   
#		stn, date, hour, min, actual date, actual hour, actual_min, index into depths
#
#       global values:
#			all the data obs
#
#  output:  all the soil output for each depth for all stations for one day
#--------------------------------------------------------------------------------   

sub writeSTMline
{ 
    my ($id, $date, $hour, $min, $actual_date, $actual_hour, $actual_min, $i) = @_;
    my $long_name = $stn_name{$id};
	my $lat  = ${$id}{lat};
	my $lon  = ${$id}{lon};
	my $elev = ${$id}{elev};

	#-------------------
	# the depths 
	#-------------------
    my @depths = ( -0.05, -0.15, -0.25, -0.35, -0.60, -0.85, -1.25, -1.75);
	$sensor_height = $depths[$i];  

    #------------------------------------------------------------
	# Don't print out levels that are missing for all of the TOI
	# Station IDs and heights taken from Scot's email of 26Sep05
    #------------------------------------------------------------
	
	if ($id eq "E5" && $sensor_height == -1.25) {
		return;
	} elsif (($id eq "E10") && ($sensor_height == -0.85 || $sensor_height == -1.25 || $sensor_height == -1.75)) {
		return;
	} elsif (($id eq "E12") && ($sensor_height == -0.85 || $sensor_height == -1.25 || $sensor_height == -1.75)) {
		return;
	} elsif (($id eq "E13") && ($sensor_height == -1.25 || $sensor_height == -1.75)) {
		return;
	} elsif ($id eq "E20" && $sensor_height == -1.75) {
		return;
	} elsif ($id eq "E24" && $sensor_height == -1.75) {
		return;
	}

	my $st = $SoilTemp_out{$id}[$actual_hour][$actual_min][$i];
    my $sm = $SoilMoist_out{$id}[$actual_hour][$actual_min][$i];
	my $st_flag = $SoilTemp_flag{$id}[$actual_hour][$actual_min][$i];
	my $sm_flag = $SoilMoist_flag{$id}[$actual_hour][$actual_min][$i];

    #------------------------------------------------------------
    # Print out all the values to the stm file 
    #------------------------------------------------------------       

    $st_flag = &get_flag(\$st, $st_flag, "tsoil_W", $id, $date, $hour, $min, $sensor_height);
    $sm_flag = &get_flag(\$sm, $sm_flag, "watcont_W", $id, $date, $hour, $min, $sensor_height);

    #------------------------------------------------------------
	# decision made to include empty records, 12 May 04, per Scot
    #------------------------------------------------------------
	# return ("empty line") if ($print == 0);							# don't print lines with all obs missing
    #------------------------------------------------------------

	print ("the sensor height = $sensor_height where the index into depths = $i\n") if($DEBUG);
    printf OUTFILE_STM ("%10s %02d:%02d %10s %02d:%02d %-10s %-15s %-15s %10.5f %11.5f %7.2f %7.2f", $date, $hour, $min, $actual_date, $actual_hour, $actual_min, $CSE_id, $site_id, $long_name, $lat, $lon, $elev, $sensor_height);

    #   format -   yyyy/mm/dd hh:mm yyyy/mm/dd hh:mm CSE_id site_id station_id dec_lat dec_lon elevation sensor_height
  
    #------------------------------------------------------------
    # Print out the soil temperature
    #------------------------------------------------------------ 
            
      printf OUTFILE_STM (" %8.2f", $st);
  	  printf OUTFILE_STM (" %s", $st_flag);
  
      #------------------------------------------------------------
      # Print out the soil moisture
      #------------------------------------------------------------ 
            
      printf OUTFILE_STM (" %8.2f", $sm);
  	  printf OUTFILE_STM (" %s", $sm_flag);
  
      print OUTFILE_STM ("\n");
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
	my $ht = shift;

    local $obs_value = $$obs_ref;
	my $new_flag = "U";

	my $our_year	= substr($date, 0, 4);
	my $our_month 	= substr($date, 5, 2);
	my $our_day 	= substr($date, 8, 2);
	my $our_time 	= sprintf("%02d%02d", $hour, $min) * 0.0001;
	my $datetime = ($our_year.$our_month.$our_day) + $our_time;			# so 1/1/03 1230 = 20030101.123

	print "   in get_flag, var = $var, obs_ref = $obs_ref, flag_val = $flag_val, obs_value = $obs_value, id = $id, date = $date, this time = $hour:$min, our time = $our_time, date time = $datetime, height = $ht.\n" if($DEBUG1);
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

    #---------------------------------------------------------------------------
    # E1
    #---------------------------------------------------------------------------
    if ($id eq "E1") {
        if (($var eq "watcont_W") || ($var eq "tsoil_W")) {
        	if ($ht == -0.25 || $ht == -0.35 || $ht == -0.60 || $ht == -0.85 || $ht == -1.25 || $ht == -1.75) {
	           if ($datetime == 20040210.2100) {
                 $new_flag = "B";
                 print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            	}
			}
		}
        if ($var eq "watcont_W") {
        	if ($ht == -0.85) {
	           if ($datetime >= 20040617.0500 && $datetime <= 20040617.0700) {
                 $new_flag = "D";
                 print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            	}
			}
        }
        if ($var eq "tsoil_W") {
        	if ($ht == -0.85) {
	           if ($datetime == 20040620.1000 || $datetime == 20040622.1400 || $datetime >= 20040623.1000) {
                 $new_flag = "B";
                 print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            	}
			}
        }
    #---------------------------------------------------------------------------
    # E2
    #---------------------------------------------------------------------------
    } elsif ($id eq "E2") {
        if ($var eq "watcont_W") {
        	if ($ht == -0.15) {
	           if ($datetime == 20031023.1500) {
                 $new_flag = "B";
                 print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            	}
			}
        }
    #---------------------------------------------------------------------------
    # E3
    #---------------------------------------------------------------------------
    } elsif ($id eq "E3") {
        if ($var eq "watcont_W") {
           if (($datetime >= 20040114.0500 && $datetime <= 20040114.0800) ||
               ($datetime >= 20040114.1800 && $datetime <= 20040114.2100) ||
               ($datetime >= 20040307.1000 && $datetime <= 20040310.1715) ||
               ($datetime >= 20040602.1700 && $datetime <= 20040606.1300) ||
               ($datetime >= 20040610.2000 && $datetime <= 20040610.2100)) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "watcont_W") {
        	if ($ht == -0.05 || $ht == -0.15 || $ht == -0.25 || $ht == -0.35 || $ht == -0.60 || $ht == -0.85 || $ht == -1.25 || $ht == -1.75) {
               $new_flag = "D";
               print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
			}
		}
    #---------------------------------------------------------------------------
    # E4
    #---------------------------------------------------------------------------
    } elsif ($id eq "E4") {
        if ($var eq "watcont_W") {
           if (($datetime >= 20030910.1600 && $datetime <= 20031008.1600) ||
               ($datetime >= 20040212.1500 && $datetime <= 20040212.1900)) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "tsoil_W") {
        	if ($ht == -0.05 || $ht == -0.15 || $ht == -0.25 || $ht == -0.35 || $ht == -0.60 || $ht == -0.85 || $ht == -1.25 || $ht == -1.75) {
	           if ($datetime == 20031008.1600) {
                 $new_flag = "D";
                 print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
			   }
			}
        	if ($ht == -0.15 || $ht == -0.25) {
	           if ($datetime == 20031022.1700) {
                 $new_flag = "B";
                 print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
			   }
			}
		}
    #---------------------------------------------------------------------------
    # E7
    #---------------------------------------------------------------------------
    } elsif ($id eq "E7") {
        if ($var eq "watcont_W") {
           if ($datetime >= 20040608.1900 && $datetime <= 20040608.2000) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
        if ($var eq "tsoil_W") {
     		if ($ht == -1.25 || $ht == -1.75) {
	           if ($datetime == 20040526.0000 || $datetime == 20040608.1900) {
                 $new_flag = "B";
                 print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
			   }
			}
		}
    #---------------------------------------------------------------------------
    # E8
    #---------------------------------------------------------------------------
    } elsif ($id eq "E8") {
        if ($var eq "watcont_W") {
        	if ($ht == -0.15) {
            	$new_flag = "D";
            	print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
        	}
		}
    #---------------------------------------------------------------------------
    # E10
    #---------------------------------------------------------------------------
    } elsif ($id eq "E10") {
        if ($var eq "watcont_W") {
        	if ($ht == -0.05 || $ht == -0.15 || $ht == -0.25 || $ht == -0.35 || $ht == -0.60) {
           		if ($datetime >= 20040130.0900 && $datetime <= 20040214.2100) {
                   $new_flag = "D";
                   print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
				}
            }
        }
    #---------------------------------------------------------------------------
    # E12
    #---------------------------------------------------------------------------
    } elsif ($id eq "E12") {
        if ($var eq "watcont_W") {
           if ($datetime >= 20040219.2212 && $datetime <= 20040310.2247) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
           	if ($ht == -0.05 || $ht == -0.15 || $ht == -0.25 || $ht == -0.35 || $ht == -0.60) {
           		if (($datetime == 20031124.1800 || $datetime == 20040217.1800) ||
					($datetime >= 20040414.0300 && $datetime <= 20040414.0600)) {
                	$new_flag = "B";
                	print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
				}
			}
        }
        if ($var eq "tsoil_W") {
           if ($datetime >= 20040219.2212 && $datetime <= 20040310.2247) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E13
    #---------------------------------------------------------------------------
    } elsif ($id eq "E13") {
        if ($var eq "watcont_W") {
        	if ($ht == -0.85) {
            	$new_flag = "D";
            	print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
			}
        }
    #---------------------------------------------------------------------------
    # E15
    #---------------------------------------------------------------------------
    } elsif ($id eq "E15") {
        if ($var eq "watcont_W") {
        	if ($ht == -0.25 || $ht == -0.35 || $ht == -0.60 || $ht == -0.85 || $ht == -1.25 || $ht == -1.75) {
            	$new_flag = "D";
            	print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
			}
        }
    #---------------------------------------------------------------------------
    # E16
    #---------------------------------------------------------------------------
    } elsif ($id eq "E16") {
        if ($var eq "watcont_W") {
        	if ($ht == -1.75) {
            	$new_flag = "D";
            	print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
			}
        }
    #---------------------------------------------------------------------------
    # E19
    #---------------------------------------------------------------------------
    } elsif ($id eq "E19") {
        if ($var eq "watcont_W") {
        	if ($ht == -0.05) {
           		if ($datetime >= 20041203.1900 && $datetime <= 20041203.2300) {
                	$new_flag = "B";
                	print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            	}
			}
        	if ($ht == -0.85 || $ht == -1.25) {
           		if ($datetime >= 20040826.0300 && $datetime <= 20040826.0500) {
                	$new_flag = "B";
                	print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            	}
			}
		}
        if ($var eq "tsoil_W") {
        	if ($ht == -0.05) {
           		if ($datetime >= 20041203.1900 && $datetime <= 20041203.2300) {
                	$new_flag = "B";
                	print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            	}
			}
        	if ($ht == -1.75) {
           		if ($datetime >= 20040826.0300 && $datetime <= 20040826.0500) {
                	$new_flag = "B";
                	print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            	}
			}
        }
    #---------------------------------------------------------------------------
    # E20
    #---------------------------------------------------------------------------
    } elsif ($id eq "E20") {
        if ($var eq "watcont_W") {
           if ($datetime >= 20040709.1800 && $datetime <= 20040721.1900) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        }
    #---------------------------------------------------------------------------
    # E22
    #---------------------------------------------------------------------------
    } elsif ($id eq "E22") {
        if ($var eq "watcont_W") {
        	if ($ht == -0.05 || $ht == -0.15 || $ht == -0.25 || $ht == -0.35 || $ht == -0.60 || $ht == -0.85 || $ht == -1.25 || $ht == -1.75) {
           		if ($datetime >= 20040828.0400 && $datetime <= 20040828.1700) {
            		$new_flag = "B";
            		print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
				}
			}
        }	
    #---------------------------------------------------------------------------
    # E24
    #---------------------------------------------------------------------------
    } elsif ($id eq "E24") {
        if ($var eq "watcont_W") {
           if (($datetime >= 20040205.1700 && $datetime <= 20040219.1700) ||
               ($datetime >= 20040430.2200 && $datetime <= 20040513.1900) ||
               ($datetime >= 20040513.1800 && $datetime <= 20040513.1900)) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        	if ($ht == -0.35 || $ht == -0.85 || $ht == -1.25) {
           		if (($datetime >= 20031113.1800 && $datetime <= 20031126.1600) ||
					($datetime >= 20041007.1300)) {
            		$new_flag = "D";
            		print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
				}
			}
	       	if ($ht == -0.05) {
    	        $new_flag = "B";
        	    print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
			}
        	if ($ht == -0.60) {
           		$new_flag = "D";
           		print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
			}
        }
        if ($var eq "tsoil_W") {
	       	if ($ht == -0.25) {
           		if ($datetime == 20040513.1800) {
    	        	$new_flag = "B";
        	    	print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
				}
			}
		}
    #---------------------------------------------------------------------------
    # E27
    #---------------------------------------------------------------------------
    } elsif ($id eq "E27") {
        if ($var eq "watcont_W") {
           if ($datetime >= 20030820.1700 && $datetime <= 20040218.1700) {
                $new_flag = "D";
                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
            }
        	if ($ht == -0.05 || $ht == -0.15 || $ht == -0.25 || $ht == -0.35 || $ht == -0.60 || $ht == -0.85 || $ht == -1.25 || $ht == -1.75) {
           		if ($datetime == 20040218.1700 || $datetime >= 20041001.1000) {
            		$new_flag = "D";
            		print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
				}
			}
        }
        if ($var eq "tsoil_W") {
	       	if ($ht == -1.75) {
           		if ($datetime >= 20031001.0000 && $datetime <= 20040405.2200) {
            		$new_flag = "D";
            		print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);
				}
			}
		}
    }

    return $new_flag;
}


#----------------------------------------------------------------
#  local2UTC - convert time from local to UTC 
# 
#  input:   
#       $year       4 digits, e.g. 1995
#       $month      1 or 2 digits
#       $day        1 or 2 digits
#       $hour       hours run 0-23
#       $offset     number of hours to add to local time 
#                   to get UTC time (can be negative)
# 
#  output:  $date_str, $new_hour
#  side effects: $year, $month, $day, $hour are set to correct values
#                for the time going in 
# 
#  January 23, 1997 Janine Goldstein - added alt date string in format
#  necessary for the station information files. Added calculations for
#  negative offsets.
# 
#  27 Oct 96, ds: converted from C to Perl
#  09 Aug 99, ds: uses full 4 digit year, now
#----------------------------------------------------------------*/

sub local2UTC {
    ($year, $month, $day, $hour, $min_value, $offset) = @_;

    $leap_year = 0;
    if ( (($year % 4 == 0) && ($year % 100 != 0)) || ($year % 400 == 0) ) {
        $leap_year = 1;
    }

    die "Time offset in hours must be less than 24" if ($offset >= 24 || $offset <= -24); 

    $hour = $hour + $offset;
    
    if ($hour >= 24) {
        $hour = $hour - 24;
        $day = $day + 1;

        if ($month == 2 && $day > 28 && $leap_year == 0) {
            $month = $month + 1;
            $day = 1;
        } elsif ($month == 2 && $day > 29 && $leap_year == 1) {
            $month = $month + 1;
            $day = 1;
        } elsif (($month == 4 || $month == 6 || $month == 9 || $month == 11) && ($day > 30) ) {
            $month = $month + 1;
            $day = 1;
        } elsif ($day > 31) {
            if ($month != 12) {
                $month = $month + 1;
                $day = 1;
            } else {
                $year = $year + 1;
                $month = $day = 1;
            }
        } 
    } elsif ($hour < 0) {
        $hour = $hour + 24;
        $day = $day - 1;

        if ($day < 1) {
          if ($month == 3 && $leap_year == 0) {
            $month = $month - 1;
            $day = 28;
          } elsif ($month == 3 && $leap_year == 1) {
            $month = $month - 1;
            $day = 29;
          } elsif (($month == 5 || $month == 7 || $month == 10 || $month == 12) ) {
            $month = $month - 1;
            $day = 30;
          } else {
            if ($month != 1) {
                $month = $month - 1;
                $day = 31;
            } else {
                $year = $year - 1;
                $month = 12;
                $day = 31;
            }
          } 
       }
    }
 
    $date_str = sprintf("%4.4d/%2.2d/%2.2d", $year, $month, $day);
    $new_hour = sprintf("%02d:%02d", $hour, $min_value);
    $alt_date_string = sprintf("%4.4d%2.2d%2.2d", $year, $month, $day);
}
