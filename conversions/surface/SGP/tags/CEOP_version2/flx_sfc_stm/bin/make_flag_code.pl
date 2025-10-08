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
#
# rev 18 July 05, ds
#   Now handles "all parameters except precip";
#   "All parameters" in *flag.file lines, and
#   any number of obs names joined by "and". 
#
#   Runs all networks one after another, now.
#----------------------------------------------

$DEBUG = 0;

#--------------------------------------------------------------------------------------
# the parameters we want from each set of files, by platform
#--------------------------------------------------------------------------------------
@sirs_fields  = ("up_long_hemisp", "down_long_hemisp_shaded", "up_short_hemisp", "down_short_hemisp");
@smos_fields  = ("wspd", "wdir", "temp", "rh", "bar_pres", "precip");
@swats_fields = ("tsoil_W", "watcont_W");
@twr_fields   = ("temp_60m", "temp_25m", "rh_60m", "rh_25m");
@ebbr_fields  = ("c_shf1", "g1", "e", "h");
@ecor_fields  = ("h", "lv_e", "fc");

%params = (
    "sirs"  => \@sirs_fields,
    "smos"  => \@smos_fields,
	"swats" => \@swats_fields,
    "twr"   => \@twr_fields,
    "ebbr"  => \@ebbr_fields,
    "ecor"  => \@ecor_fields
);

#--------------------------------------------------------------------------------------
# the input file
#--------------------------------------------------------------------------------------
%file_in = (
    "sirs"  => "../code_frag/DQR_SIRS_flag.file", 
    "smos"  => "../code_frag/DQR_SMOS_flag.file",
	"swats" => "../code_frag/DQR_SWATS_flag.file",
	"twr"   => "../code_frag/DQR_TWR_flag.file",
	"ebbr"  => "../code_frag/DQR_EBBR_flag.file",
	"ecor"  => "../code_frag/DQR_ECOR_flag.file"
);

#--------------------------------------------------------------------------------------
# the output file
#--------------------------------------------------------------------------------------
%file_out = (
    "sirs"  => "../code_frag/dqr_sirs_code.frag", 
    "smos"  => "../code_frag/dqr_smos_code.frag",
	"swats" => "../code_frag/dqr_swats_code.frag",
	"twr"   => "../code_frag/dqr_twr_code.frag",
	"ebbr"  => "../code_frag/dqr_ebbr_code.frag",
	"ecor"  => "../code_frag/dqr_ecor_code.frag"
);

$i = 0;													# for counting date time lines

foreach $network (keys (%file_in)) {
	open(INFILE, $file_in{$network}) || die "Couldn't open $file_in{$network}\n";
	open(OUTFILE, ">$file_out{$network}") || die "Couldn't open $file_out{$network}\n";
	print "Reading in the data from $file_in{$network} \n";

	print OUTFILE "    #---------------------------------------------------------------------------\n";
	print OUTFILE "    # following section fixes flags according to DQRs\n";

	while ($input_line = <INFILE>) {
		print $input_line if ($DEBUG);
		if ($input_line =~ /(^[CE]\d{1,3})/) {				# site ID, e.g."E9"
			print "matched site: $1.\n" if ($DEBUG);
			&put_dates($flag) if($i > 0);					# finish last flag if we have dates for it
			$site_id = $1;
			print OUTFILE ("    #---------------------------------------------------------------------------\n");
			print OUTFILE ("    # ".$site_id."\n"); 
			print OUTFILE ("    #---------------------------------------------------------------------------\n");
			print OUTFILE ('    } elsif ($id eq "'.$site_id."\") {\n");
		} elsif ($input_line =~ /\([BD]\)/) {				# either "(D)" or "(B)" for the flag
			print "matched B or D flag.\n" if ($DEBUG);
			&put_dates($flag) if($i > 0);					# finish last flag if we have dates for it
			chomp($input_line);
			$flag = substr($input_line, -2, 1);				# get new flag at end of string, without parentheses
			if ($input_line =~ /all parameters except precip/i) {	# case insensitive
				#------------------------------------------------------
				# example: "all parameters except precip (B)"
				# simple enough: every parameter but "precip"
				#------------------------------------------------------
				$num_params = $#{$params{$network}};
				print OUTFILE ('        if ( ');
				for($x=0; $x<$num_params; $x++) {
					next if(${$params{$network}}[$x] eq "precip");
					print OUTFILE ('$var eq "'.${$params{$network}}[$x].'" || '); 
				}
				if(${$params{$network}}[$x] ne "precip") {
					print OUTFILE ('$var eq "'.${$params{$network}}[$x].'") {'."\n");
				} else {
					seek(OUTFILE, -4, 2);
					print OUTFILE (' ) {'."\n");
				}
			} elsif ($input_line =~ /all parameters/i)  {					# <-------- end all params but precip
				#------------------------------------------------------
				# example: "all parameters (B)"
				# we check against each parameter name
				#------------------------------------------------------
				$num_params = $#{$params{$network}};
				print OUTFILE ('        if ( ');
				for($x=0; $x<$num_params; $x++) {
					print OUTFILE ('$var eq "'.${$params{$network}}[$x].'" || ');
				}
				print OUTFILE ('$var eq "'.${$params{$network}}[$x].'") {'."\n");
			} else {														# <-------- end all params
				#------------------------------------------------------
				# example: "h and e and g1 (D)"
				# more than 1, less than all; names joined with "and"
				#------------------------------------------------------
				(@flag_line) = split(" ", $input_line);
				$num_ands = 0;
				foreach $word(@flag_line) {
					$num_ands++ if($word eq "and");
				}
				#----------------------
				# first one, start line
				#----------------------
				print OUTFILE ('        if ($var eq "' . $flag_line[0]);
				#----------------------
				# if more, add with OR
				#----------------------
				for($x=1, $y=2; $x<=$num_ands; $x++) {
					print OUTFILE ('" || $var eq "'.$flag_line[$y]);
					$y += 2;
				}
				#----------------------
				# close line out
				#----------------------
				print OUTFILE ('") {'."\n");
			}																# <-------- end named parameters
		} elsif ($input_line =~ /if /) {
			print "added line to array named date_line.\n" if ($DEBUG);
	        $date_line[$i] = $input_line;                  	# get all date lines for a single variable and flag into an array
			$i++;
		}													# <--------- end if date line
	} 														# <--------- end while input line
								
	&put_dates($flag) if($i > 0);							# finish last flag if we have dates for it
	print OUTFILE ("    }\n");
	print OUTFILE ("\n");									# close out subroutine
	print OUTFILE ('    return $new_flag;'."\n");
	print OUTFILE ("}\n");
}															# <--------- end foreach network


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
