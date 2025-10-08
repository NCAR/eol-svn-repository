#!/opt/bin/perl -w

#----------------------------------------------
#
# make_flag_code.pl
#
# Run this to change the DQR file to code to
# insert in the perl program.
#
# Note:
#  When implementing the DQR flagging from the DQR reports,                  
#  remember that any parameters derived using these flagged
#  values should have that same flag applied to them, as well.
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
#  
# 8 May 04, ds
#----------------------------------------------

# $infile = "DQR_SMOS_flag.file";
# $outfile = "dqr_smos_code.frag";

# $infile = "DQR_SIRS_flag.file";
# $outfile = "dqr_sirs_code.frag";

$infile = "DQR_TWR_flag.file";
$outfile = "dqr_twr_code.frag";

$i = 0;													# for counting date time lines

open(INFILE, $infile) || die "Couldn't open $infile\n";
open(OUTFILE, ">$outfile") || die "Couldn't open $outfile\n";
print "Reading in the data from $infile \n";

print OUTFILE "    #---------------------------------------------------------------------------\n";
print OUTFILE "    # following section fixes flags according to DQRs\n";

while ($input_line = <INFILE>) {
	if ($input_line =~ /(^[CE]\d{1,3}) /) {				# site ID, e.g."E9"
		&put_dates($flag) if($i > 0);					# finish last flag if we have dates for it
		$site_id = $1;
		print OUTFILE ("    #---------------------------------------------------------------------------\n");
		print OUTFILE ("    # ".$site_id."\n"); 
		print OUTFILE ("    #---------------------------------------------------------------------------\n");
		print OUTFILE ('    } elsif ($id eq "'.$site_id."\") {\n");
	} elsif ($input_line =~ /\([BD]\)/) {				# either "(D)" or "(B)" for the flag
		&put_dates($flag) if($i > 0);					# finish last flag if we have dates for it
		($this_var, $flag) = split(" ", $input_line);
		$flag = substr($flag, 1, 1);					# get rid of parentheses around flag
		print OUTFILE ('        if ($var eq "'.$this_var.'") {'."\n");
	} elsif ($input_line =~ /if /) {
        $date_line[$i] = $input_line;                  	# get all date lines for a single variable and flag into an array
		$i++;
	}
}
								
&put_dates($flag) if($i > 0);							# finish last flag if we have dates for it
print OUTFILE ("    }\n");
print OUTFILE ("\n");									# close out subroutine
print OUTFILE ('    return $new_flag;'."\n");
print OUTFILE ("}\n");

sub put_dates
{
	if ($i == 1) {
		substr($date_line[0], -3, 3, "{\n");			# fix up first date line
	} elsif ($i > 1) {
		substr($date_line[0], 11, 4, "if ((");
		$j = 1;
		while ($j < $i-1) {
			substr($date_line[$j], 11, 3, "    ");		# fix up middle date lines
			$j++;
		}
		substr($date_line[$j], 11, 3, "    ");			# fix up last date line
		substr($date_line[$j], -4, 4, ") {\n");
	}

	for ($j=0; $j < $i; $j++) { 
		print OUTFILE $date_line[$j];
	}
	print OUTFILE ('                $new_flag = "'.$flag.'";'."\n");
	print OUTFILE ('                print "on $datetime, for $id, overrode orig flag = $flag_val with DQR value = $new_flag for $var\n" if($DEBUG1);'."\n");
	print OUTFILE ("            }\n");
	print OUTFILE ("        }\n");

	$i = 0;
	undef @date_line;
}
