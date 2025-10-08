#!/opt/bin/perl -w

#---------------------------------------------------------
# convert_DQR_datetime.pl
#
# Utility to read the text files which Scot put together
# from the ARM DQR reports, and change the date style from
# "mm/dd/yyyy  hhmm" to yyyymmdd.hhmm and in a line ready
# to be read by the "make_flag_code.pl" script. Lines 
# without dates are output unchanged.
#
# Note: Tabs must be changed to spaces in the input file
#       before conversion.
#       
#       Also, uncomment each set of network input and 
#       output files at a time and run. 
#
# 1 April 04, ds
#---------------------------------------------------------

# $infile = "../SIRS/ARM_SGP_SIRS_flagging.txt";
# $outfile = "../code_frag/DQR_SIRS_flag.file";

# $infile = "../SMOS/ARM_SGP_SMOS_flagging.txt";
# $outfile = "../code_frag/DQR_SMOS_flag.file";

# $infile = "../SWATS/ARM_SGP_SWATS_flagging.txt";
# $outfile = "../code_frag/DQR_SWATS_flag.file";

# $infile = "../TWR10x/ARM_SGP_TWR_flagging.txt";
# $outfile = "../code_frag/DQR_TWR_flag.file";
 
# $infile = "../EBBR/ARM_SGP_EBBR_flagging.txt";
# $outfile = "../code_frag/DQR_EBBR_flag.file";

$infile = "../ECOR/ARM_SGP_ECOR_flagging.txt";
$outfile = "../code_frag/DQR_ECOR_flag.file";

open (INFILE, $infile) || die "Can't open $infile";
open (OUTFILE, ">$outfile") || die "Can't open $outfile";

while ($this_line = <INFILE>) {
	#--------------------------------------------------------------------------------------------
	#			           1       2        3        4          5        6        7        8
	#                     10   /  21   /   2003     1745       10  /    21  /   2003     1945  
	#--------------------------------------------------------------------------------------------
	if ($this_line =~ /(\d{2})\/(\d{2})\/(\d{4}) +(\d{4}) +(\d{2})\/(\d{2})\/(\d{4}) +(\d{4})/) {
		$start_dt = "$3$1$2.$4";
		$end_dt   = "$7$5$6.$8";
		print OUTFILE "           if (\$datetime >= $start_dt && \$datetime <= $end_dt) ||\n";
	#--------------------------------------------------------------------------------------------
	# Use next line for checking data time conversion:
	#--------------------------------------------------------------------------------------------
	#	print OUTFILE "       $start_dt    $end_dt  -  $1/$2/$3  $4    $5/$6/$7  $8\n";
	#--------------------------------------------------------------------------------------------
	} else {
		print OUTFILE $this_line;
	}

}

close INFILE;
close OUTFILE;
