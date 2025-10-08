#!/usr/bin/perl -w

#--------------------------------------------------------------------------------------
# process_skintemp.pl
#
# This s/w is used in converting EBBR surface, 30 minute, Netcdf files into QCF format.
#
# This Perl script is used in the processing of NESOB96 Skin Temp data.  Input comes
# from STDIN, and consists of lines of data extracted from a netCDF file using nesob_dump.
# (nesob_dump is a variation of nc_dump specifially rewritten for this processing.)
# A script named "do_process_skintemp.sh" is used to run this s/w, and has a line in it  
# with the variable names to extract.  These variables match the "@fields" in this s/w. 
# If you change which variables to extract from the netCDF file, you will need to change 
# the "fields" array, as well.
# 
# 21 may 99, ds
# rev 30jul99, ds
#    added section to create 3 station *.out files
# rev 03aug99, ds
#    added checks for AOI and TOI
# rev 09aug99, ds
#    made year 4 digits long in output file
# rev 02nov99, ds
#    now processes skin temp
# rev 06dec99, ds
#    convert Kelvin temps to Celsius
#--------------------------------------------------------------------------------------

$LEAPYEAR = 0;
@monthDay = (0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);

if ($LEAPYEAR) {
    $monthDay[2] = 29;
}

@fields = ("time_offset", "sfc_temp");

#-----------------------------
# project specific variables
#-----------------------------
                           
$network    = "ARM_SGP";
$project_begin_date = 19970401;         
$project_end_date   = 19980331;     
$project_begin_lat = 34.00;         # south to north, lesser to greater
$project_end_lat   = 39.00;
$project_begin_lon = -100.50;       # west to east, lesser to greater
$project_end_lon   = -94.50;

$century = 1900;
$continue = $have_obs = 0;			# zero these values out, to start
                               
foreach $obsName (@fields) {		# ditto for these
    $sumObs{$obsName} = 0;
    $cntObs{$obsName} = 0;
}

#-----------------------------
# files in
#-----------------------------
# files come from STDIN

# open (LOGFILE, ">a.log")  || die "Can't open a.log for output";

#--------------------------------------------------------------------------------------
# These are the variables which we are interested in, and which have been extracted
# from the netCDF file by nesob_dump.  
#       time_offset:    the times of the observations
#       sfc_temp:       effective Surface (Skin) Temperature
#--------------------------------------------------------------------------------------

while (<STDIN>) {
    @line_value = split(" ");

    #--------------------------------------------------------------------------------------
    # Test to see what variable we have on this line, and save the constants for this file.
    # Files are read in order, so each time we see "netCDF" it is a new file being read.
    #--------------------------------------------------------------------------------------

    if ($line_value[0] eq "netcdf") {
        $filename = $line_value[1];
        print "$filename\n";

        #--------------------------------------------------------------------------------------
        # Get our station name and date from the filename 
        # Check the date against project dates and if outside the TOI get another line.
        #--------------------------------------------------------------------------------------

        ($stn_id, $yr, $mon, $day, $hour, $min, $sec) = /sgpmfr10m(C\d{1,3}).{4}(\d{2})(\d{2})(\d{2})\.(\d{2})(\d{2})(\d{2})/;
		print "id=$stn_id, year=$yr, month=$mon, day=$day, hour=$hour, minute=$min, second=$sec\n";
        $full_year = $century + $yr;
        $this_date = $full_year.$mon.$day;
        if($this_date < $project_begin_date || $this_date > $project_end_date) {
            print "this data is out of the TOI\n\n";
            last;
        }
        
        #--------------------------------------------------------------------------------------
        # By going to next when we are out of the AOI, we miss this code which opens
        # a file preparatory to writing it.  So, the data is never written out, since
        # there is no filehandle.
        #--------------------------------------------------------------------------------------
        
        $outFilename = $stn_id."_10m_".$yr.$mon.$day.".skn";

        #--------------------------------------------------------------------------------------
        # If there is an output file with this name already, then we have more times to add
        # to it, so open with an append. Otherwise, open a new file for output.
        #--------------------------------------------------------------------------------------

        if (-e "./10m/$outFilename") {
            print "have $outFilename filename already, will add to it\n";
            open (OUTFILE, ">>10m/$outFilename") || die "Can't open $outFilename for output";
        } else {
	        print "will open $outFilename\n";
            open (OUTFILE, ">10m/$outFilename")  || die "Can't open $outFilename for output";
        } 
    } elsif ($line_value[0] eq "data:") {
		# do nothing

    #--------------------------------------------------------------------------------------
    # Check the lat/lon against project AOI and if outside, get another line.
    #--------------------------------------------------------------------------------------

    } elsif ($line_value[0] eq "lat:") {
        $lat = $line_value[1];
        if($lat < $project_begin_lat || $lat > $project_end_lat) {
            print "this data is out of the AOI\n\n";
            last;
        }
    } elsif ($line_value[0] eq "lon:") {
        $lon = $line_value[1];
        if($lon < $project_begin_lon || $lon > $project_end_lon) {
            print "this data is out of the AOI\n\n";
            last;
        }
    } elsif ($line_value[0] eq "alt:") {
        $elev = $line_value[1];
    } elsif ($line_value[0] eq "base_time:") {
        $baseTime = $line_value[1];
    } else {
        #--------------------------------------------------------------------------------------
        # All others should be the values for each time.  Get the variable name which will match
        # one of our "fields" values (with a ":" after it), take off the trailing comma in each 
        # slot of the "line_value" array, and copy the array to an array named after the variable. 
        #--------------------------------------------------------------------------------------
    
        foreach $obsName (@fields) {
            if ($line_value[0] eq "$obsName:") {
    
                #--------------------------------------------------------------------------------------
                # Check that each line of values has the same number of observations as the time line,
                # and that this number is 4320.  (The first value on the line is the variable name.)
                #--------------------------------------------------------------------------------------

                $numObs = ($#line_value - 1) if ($obsName eq "time_offset");    
                warn "*** the number of observations in $filename is: $numObs\n" if (($numObs != 4320) && ($obsName eq "time_offset"));
                $thisNumObs = $#line_value - 1;                 
                die "Different number of observations in the $line_value[0] line; expecting $numObs, but have $thisNumObs\n" if ($numObs != $thisNumObs);
    
                #------------------------------------------
                # Get rid of the commas after the values
                #------------------------------------------
    
                for ($i=0; $i<=$thisNumObs; $i++) {
                    $line_value[$i] =~ s/,//g;
                }
    
                #------------------------------------------
                # Put into an array named after the variable
                #------------------------------------------
    
                @{$obsName} = @line_value;
                print "wrote an array for $obsName\n";
				last;
            }
        }
    }
    $continue = 1;						# yes, we have data
}


#--------------------------------------------------------------------------------------
# Check that the date/time in the filename matches the date/time in the data.
# Add the 1st time_offset to the base_time and convert to GMT to compare.
# Add 1 to the month because gmtime() returns months indexed from 0 to 11.
# But don't even bother going on if we have no data (continue isn't 1).
#--------------------------------------------------------------------------------------
if ($continue == 1) {
    @begin_time = gmtime($baseTime + $time_offset[1]);
    $begin_month = $begin_time[4] + 1;
    
    if (($yr != $begin_time[5]) || ($mon != $begin_month) || ($day != $begin_time[3]) || ($hour != $begin_time[2]) || ($min != $begin_time[1]) || ($sec != $begin_time[0])) {
        print "We got a time problem with $filename\n";
        print "julian day: $begin_time[7], year: $yr-$begin_time[5], month: $mon-$begin_month, day: $day-$begin_time[3], hour: $hour-$begin_time[2], min: $min-$begin_time[1], sec: $sec-$begin_time[0]\n";   
        die "That's all!";
    }

    #--------------------------------------------------------------------------------------
    # Put the values together by time and write to the output file.
    # The 0'th value is the variable name, so index of obs starts at 1.
    #--------------------------------------------------------------------------------------
    
    $date = "$full_year/$mon/$day"; 
	print "Will write data for $date\n";
    $prev_time = $this_time = $time_offset[1]; 
    $i = 1;
	$next_time_to_write = 30;
    
    do {
        if ($sec == 0) {										# write the times at 00 and 30 min, except for very first
			if (($min == 0 && $hour != 0) || $min == 30) {		# 00 hour for this day was written from last input file
            	writeToFile($hour, $min, %sumObs);
        	}
		}

	    if ($min > $next_time_to_write) {						# catch times which have jumped ahead
           	writeToFile($hour, $next_time_to_write, %sumObs);
		}

        #------------------------------------------------------------
        # Check time, should be 20 seconds ahead of last reading. If
        # not, adjust count of hour, min, sec in loop to agree.
        #------------------------------------------------------------
        $this_time = $time_offset[$i];
        $time_difference = ($this_time - $prev_time);
        if ($time_difference != 0) {
#            print LOGFILE "*** difference is $time_difference seconds between obs this_time = $this_time and prev_time = $prev_time, index = $i \n" if ($time_difference != 20); 
			$min_diff = int ($time_difference / 60);			# seconds to minutes
			$sec_diff = $time_difference % 60;					# remainder to seconds
		
			$temp_sec = $sec + $sec_diff;						

			if ($temp_sec >= 60) {								# check if more than 60 seconds
				$min_diff += int ($temp_sec / 60);				# seconds to minutes
				$sec = $temp_sec % 60;							# remainder to seconds
			} else {
				$sec = $temp_sec;
			}

			$temp_min = $min + $min_diff;

			if ($temp_min >= 60) {								# check if more than 60 minutes
				$hour_diff = int ($temp_min / 60);				# minutes to hours
				$hour += $hour_diff;
				$min = $temp_min % 60;							# remainder to minutes
			} else {
				$min = $temp_min;
			}
			
			die "hour = $hour!\n" if ($hour > 24);
        }
			
        #------------------------------------------------------------
        # Take 20-second obs and average them for the 30-minute value. 
        #------------------------------------------------------------
        foreach $obsName (@fields) {
			next if ($obsName eq "time_offset");
	        $thisObs = ${$obsName}[$i];
        	if (!defined($thisObs) || $thisObs eq "_" || $thisObs eq "NaN") {
	            $thisObs = 0;
#	            print LOGFILE "** $hour:$min:$sec, time offset=$this_time, obsNum=$i, cnt=$cntObs{$obsName}, $obsName, obs=$thisObs, sum=$sumObs{$obsName}\n";
	        } elsif ($thisObs == -9999 || $thisObs == 9999) {
	            $thisObs = 0;
#	            print LOGFILE "** $hour:$min:$sec, time offset=$this_time, obsNum=$i, cnt=$cntObs{$obsName}, $obsName, obs=$thisObs, sum=$sumObs{$obsName}\n";
	        } else {
	            $sumObs{$obsName} += $thisObs;
	    		$cntObs{$obsName} += 1;
				$have_obs++;
#	            print LOGFILE "at $hour:$min:$sec, time offset=$this_time, obsNum=$i, cnt=$cntObs{$obsName}, $obsName, obs=$thisObs, sum=$sumObs{$obsName}\n";
	        }
		} 
        $prev_time = $this_time;
        $i++;
    } while ($i <= $numObs);
   
	if ($hour == 23 && $min > 30) {
		$hour = 0;
		$min = 0;

    	#--------------------------------------------------------------------------------------
    	# For data from 23:30 to 24:00, increment the day, 
    	# and set the time to 00:00.
    	# Change day number, month, and/or year, if at end of a month.
    	#--------------------------------------------------------------------------------------
    	if (++$day > $monthDay[$mon]) {
    	    if (++$mon > 12) {
    	        $mon = 1;
    	        $yr = ($yr+1) % 100;
    	    } 
    	    $day = 1;
    	}
		$yr = sprintf("%2.2d", $yr);
		$mon = sprintf("%2.2d", $mon);
		$day = sprintf("%2.2d", $day);
	    $full_year = $century + $yr;
#	    $date = sprintf("%2.2d/%2.2d/%2.2d", $full_year, $mon, $day);
	    $date = "$full_year/$mon/$day";
	    print "Will write data for $date\n";

    	close (OUTFILE);

    	#--------------------------------------------------------------------------------------
   	    # If there is an output file with this name already, then we have more times to add
  	    # to it, so open with an append. Otherwise, open a new file for output.
 	    #--------------------------------------------------------------------------------------
	    
        $outFilename = $stn_id."_10m_".$yr.$mon.$day.".skn";
	    if (-e "./10m/$outFilename") {
	        print "have $outFilename filename already, will add to it\n";
	        open (OUTFILE, ">>10m/$outFilename") || die "Can't open $outFilename for output";
	    } else {
	        print "will open $outFilename\n";
	        open (OUTFILE, ">10m/$outFilename")  || die "Can't open $outFilename for output";
	    } 
	    writeToFile($hour, $min, %sumObs); 
	} elsif ($have_obs > 0) {
		$hour++ if ($next_time_to_write == 60);
       	writeToFile($hour, $next_time_to_write, %sumObs);
	}

    if ($i-1 != $numObs) {
        printf ("*** Had %d number of observations in $filename, but was expecting %d!\n", $i - 1, $numObs);
    }
}

close (OUTFILE);

# --------------------------------------------------------------------------------
#  writeToFile - write the data values for each half hour;
#				 figure standard deviation; and convert Kelvin to Celsius
#
#  input:
#       $hr         the hour of this averaged value
#       $mn         the minute of this averaged value
#       %sum        the total of all observations in the averaging period, 
#					in a hash indexed on the parameter name
#
#       global values:
#                   $date, $network, $stn_id, $lat, $lon, $elev, $have_obs, 
#					$next_time_to_write
#       			%cntObs - the total number of observations which were summed,
#							  in a hash by parameter name	
#
#  output:  		a single line of data for that time
#  side effects: 	$cntObs{$obsName} and $sumObs{$obsName} get set to 0
#					$have_obs gets set to 0
#					$next_time_to_write gets set to either 30 or 60
#--------------------------------------------------------------------------------

sub writeToFile
{
    local ($hr, $mn, %sum) = @_;
	local ($k, $avgValue);

	$mn = 0 if ($mn == 60);

    #-----------------------------------------------------------------------
    # Change station id to be more descriptive.
    #-----------------------------------------------------------------------
    if ($stn_id eq "C1") {
    	$stn_name = "E13: Lamont_CF1";
    }
    else {
    	print "ERROR: unknown station id $stn_id\n";
    	exit(1);
    }

    #------------------------------------------------------------
    # Print out the first part of our line for the output
    #------------------------------------------------------------       
    printf OUTFILE ("%10s %-2.2d:%-2.2d %-10s %-15s %10.5f %11.5f %3d %7.2f", $date, $hr, $mn, $network, $stn_name, $lat, $lon, 0, $elev);
    # format -   yy/mm/dd, time, network, station id, dec lat, dec lon, occ = 0, elevation 
 
    #------------------------------------------------------------
    # Print out the parameters
    #------------------------------------------------------------ 

    foreach $obsName (@fields) {
		next if ($obsName eq "time_offset");
	    if ($cntObs{$obsName} > 0) {
	        $avgValue = $sum{$obsName} / $cntObs{$obsName};
	        $flag = "U";
    		#---------------------------
    		# Figure Standard Deviation
    		#---------------------------
	    	if ($cntObs{$obsName} > 14) {
				$sqDev = 0;
            	for ($k=1; $k <= $cntObs{$obsName}; $k++)  {
           	    	$dev = ${$obsName}[$k] - $avgValue;
          	       	$sqDev += ($dev * $dev);
        	    }
				$k--;												# back to actual count of values
            	$stdDev = sqrt($sqDev / ($k - 1));
			}
    		#---------------------------
			# convert Kelvin to Celsius
    		#---------------------------
			$avgValue -= 273.15;			
	    } else {
	        $avgValue = -999.99;
	        $flag = "M";
			$stdDev = -999.99;
#			print LOGFILE "  have 0 counts for $obsName\n";
	    }
	
#       print LOGFILE "Standard Deviation: $sqDev $k $stdDev\n";
#	    print LOGFILE ("    writing $avgValue to file for $obsName\n");
#	    printf LOGFILE ("writing %8.2f %s to file at %2.2d:%2.2d:%2.2d (at real time of %2.2d:%2.2d) for %s with %d counts\n", $avgValue, $flag, $hr, $mn, $sec, $hour, $min, $obsName, $cntObs{$obsName});    

	    printf OUTFILE (" %8.2f %s %7.2f", $avgValue, $flag, $stdDev);
	    $sumObs{$obsName} = 0;
	    $cntObs{$obsName} = 0;
	}

    if ($mn == 0) {                     			# keep track of having written 
        $next_time_to_write = 30;
    } else {
        $next_time_to_write = 60;
    }
	print OUTFILE ("\n");
#    print LOGFILE "\n";
	$have_obs = 0;
}    
