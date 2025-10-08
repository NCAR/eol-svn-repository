#!/opt/bin/perl -w

#---------------------------------------------------------
# convert_DQR_datetime.pl
#
# Utility to read the text files which Scot put together
# from the ARM DQR reports, and change the date style from
# "mm/dd/yyyy  hhmm" to yyyymmdd.hhmm and in a line ready
# to be pasted into the conversion program code.
#
# Note: Tabs must be changed to spaces in the input file
#       before conversion.
#
# 1 April 04, ds
#---------------------------------------------------------

# $infile = "SMOS/GAPP_SGP_SMOS_flagging.txt";
# $outfile = "DQR_SMOS_flag.file";

# $infile = "SIRS/GAPP_SGP_SIRS_flagging.txt";
# $outfile = "DQR_SIRS_flag.file";

$infile = "GAPP_SGP_TWR_flagging.txt";
$outfile = "DQR_TWR_flag.file";
 
open (INFILE, $infile) || die "Can't open $infile";
open (OUTFILE, ">$outfile") || die "Can't open $outfile";

while ($this_line = <INFILE>) {
	#--------------------------------------------------------------------------------------------
	#			           1       2        3        4          5        6        7        8
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
