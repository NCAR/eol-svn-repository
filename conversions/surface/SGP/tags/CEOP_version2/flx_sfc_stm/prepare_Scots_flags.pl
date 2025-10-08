#!/usr/bin/perl -w

#-----------------------------------------------------------
# prepare_Scots_flags.pl
#
# Run this to change the lines in the file Scot prepares
# to code to insert into the perl program.
#
# Reads the text files which Scot put together
# and changes the date style from
# "mm/dd/yyyy  hhmm" to yyyymmdd.hhmm and puts into lines 
# ready to be pasted into the conversion program code.
#
# Note: Tabs must be changed to spaces in the input file
#       before conversion.
#
# 1 April 04, ds
#   original convert_datetime.pl
# 12 Dept 08, ds
#   adapted from convert_datetime.pl and make_flag_code.pl
# 21 Feb 10, ds
#   updated to work with latest DQRs
# 15 Apr 10, ds
#   formatted output correctly for insertion into code
# 8 Jun 10, ds
#   fixed up for another change in Scot's text file
#-----------------------------------------------------------

#-----------------------------------------------------------
# Note:
#  When implementing the flagging from the DQR reports,
#  remember that any parameters derived using these flagged
#  values should have that same flag applied to them, as well.
#  (The conversion code sets flags and prints out values to
#  the outfile in the proper order, as shown.)
# 
#  dewpoint relies on:
#      rel_hum
#      temp_air
#
#  specific_humidity relies on:
#      dewpoint
#          rel_hum
#          temp_air
#      stn_pres
#
#  UV winds relies on:
#      wind_spd
#      wind_dir
#
#  net_rad relies on:
#      short_in
#      long_in
#      short_out
#      long_out
#-----------------------------------------------------------

$DEBUG = 1;
%dqr = ();
$have_first_line = 0;

#--------------------------------------------------------------------------------------
# the input file  
#--------------------------------------------------------------------------------------

# $file_in = "code_frag/SGP_SFC_flagging_2005_2009.txt";    
$file_in = "code_frag/SGP_SFC_more_flagging_2005_2009.txt";

#--------------------------------------------------------------------------------------
# the output file
#--------------------------------------------------------------------------------------
# $file_frag = "code_frag/add_Scots_flags_to_conversion_software.txt";
$file_frag = "code_frag/add_more_Scots_sfc_flags_to_conversion_software.txt";
open(OUTFILE, ">$file_frag") || die "Couldn't open $file_frag\n";

print OUTFILE "    #---------------------------------------------------------------------------\n";
print OUTFILE "    # following section fixes flags according to Scot's directions\n";

print "\nRunning $0\n";
    open(INFILE, $file_in) || die "Couldn't open $file_in\n";
    print "reading in the data from $file_in \n";
 
	#-------------------------------------------------------------------------------------------------------------
	#  sample lines of input
	#-------------------------------------------------------------------------------------------------------------
	#           1                2               3      4    5   6     7       8     9   10    11
	#-------------------------------------------------------------------------------------------------------------
	#          ID             variable         flag   month/day/year hourmin  month/day/year hourmin
	#-------------------------------------------------------------------------------------------------------------
	# NOT used, but here for comparison. See next group for this code.
	# DQR line: C2           sfc_ir_temp         B      03/27/2005   2330        04/03/2005   0240     D050504.2
	#		    C3           sfc_ir_temp         D      01/03/2008   1000        01/07/2008   0515     D080121.2
	#-------------------------------------------------------------------------------------------------------------
	#      Station ID         Parameter        Flag     BeginDate  BeginTime      EndDate    EndTime
	#			C1           down_long           B      09/09/2007   0730        09/09/2007   0800   
	#			C1           down_long           B      09/09/2007   1800        09/09/2007   1800
	#-------------------------------------------------------------------------------------------------------------

	while ($this_line = <INFILE>) {
		if ($this_line =~ /(^[CE]\d{1,3}) +(\w+) +(\w) +(\d{2})\/(\d{2})\/(\d{4}) +(\d{4}) +(\d{2})\/(\d{2})\/(\d{4}) +(\d{4})/) { 
			print $this_line if ($DEBUG);
			$site_id = $1;
			$variable = $2;
			$flag = $3;
			$start_dt = "$6$4$5.$7";
			$end_dt   = "$10$8$9.$11";
			$date_range = "(\$datetime >= $start_dt && \$datetime <= $end_dt)";

			if ($DEBUG) {
            	print "\n matched site: $1.\n";	
				print "start date = $start_dt\n";
				print "end date = $end_dt\n";
				print "site ID = $site_id\n";
				print "parameter = $variable\n";
				print "flag = $flag\n";
				print  "date range = $date_range\n";
				print  "\n";
			}

			#-----------------------------------------------------------
			# put daterange into an array off of flag at end of hash
			#-----------------------------------------------------------
			push (@{$dqr{$site_id}{$variable}{$flag}}, $date_range);

		}   # <---end if this line matches

  	}  	# <--end while this  line

#-----------------------------------------------------------
# Write out lines to be added to conversion code,
#  grouping sites, then the variables for them.
#-----------------------------------------------------------

#-----------------------------------------------------------
# get sites
#-----------------------------------------------------------
foreach $site_val (sort(keys %dqr)) {
	print OUTFILE ("    #---------------------------------------------------------------------------\n");
	print OUTFILE ("    # ".$site_val."\n"); 
	print OUTFILE ("    #---------------------------------------------------------------------------\n");
	if ($have_first_line == 0) {
		print OUTFILE ('    if ($id eq "'.$site_val."\") {\n");
		$have_first_line = 1;
	} else {
		print OUTFILE ('    } elsif ($id eq "'.$site_val."\") {\n");
	}

	#-----------------------------------------------------------
	# get variables
	#-----------------------------------------------------------
    foreach $var_val (keys(%{$dqr{$site_val}})) {
		print OUTFILE ('        if ($var eq "' . $var_val);
		print OUTFILE ('") {'."\n");

		#-----------------------------------------------------------
		# get flags and date ranges
		#-----------------------------------------------------------
     	foreach $flag_val (keys(%{$dqr{$site_val}{$var_val}})) {
			my $num = @{$dqr{$site_val}{$var_val}{$flag_val}};
			print "size of array for $site_val and $var_val and $flag_val = $num\n" if($DEBUG);
			$num--;
			while ($num >= 0) {
				print OUTFILE ("           if $dqr{$site_val}{$var_val}{$flag_val}[$num--] {\n");
				print OUTFILE ('               $new_flag = "'.$flag_val."\";\n");
				print OUTFILE ("           }\n");
			}
	  	}
		print OUTFILE ("        }\n");   		# <--end of var_vals
	}
}

print OUTFILE ("    }\n");          			# close out sequence of else-if's
print OUTFILE ("\n");							# close out subroutine
print OUTFILE ('    return $new_flag;'."\n");

close INFILE;
close OUTFILE;
print "done. See $file_frag for lines to paste into conversion code.\n\n";
