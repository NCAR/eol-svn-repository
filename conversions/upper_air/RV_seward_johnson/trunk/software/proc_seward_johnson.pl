#! /usr/bin/perl -w
# program proc_seward_johnson.pl
##Module------------------------------------------------------------------------
# <p>The proc_seward_johnson.pl script is used for converting sonde data
# from the Research Vessel (R/V) Seward Johnson. This is a control script
# that checks to see if both the PTU and Wind input data files are available
# before processing that time stamped date. The original script was written
# by Joel Clawon. Apparently, Joel wrote this script as a wrapper program 
# for another program written in 1999 by Darren Gallant. This code could
# probably be combined with the other code into a single program. Also,
# there appears to be two versions of the Darren Gallant program. One with
# an updated dewPt calculation. 
#
# BEWARE: There appears to be two versions of the Darren Gallant program.
#       One with an updated dewPt calculation.
#        WHICH ONE SHOULD BE USED? Currently calling seward_johnson.pl .</p>
#
# @author Joel Clawson
# @version 1.0 It was adapted from the Ron Brown conversion script.
#
# @author L. Cully
# @version Updated Sep 2008 by L. Cully. Added header info, comments, etc. 
#     Added some informational output statements.
# BEWARE: This s/w assumes the raw input data (*.cls) in /raw_data directories.
##Module------------------------------------------------------------------------

$program = "./seward_johnson.pl";

####$program = "./seward_johnson_dp_update.pl"; - Line added by LCully for testing only.

$TRUE = 1;
#$file_cnt = 0;

print "\nBegin proc_seward_johnson.pl\n";

opendir(DIR,"../raw_data");
@ptu_files = grep(/\d+\.PTU/i, readdir(DIR));
opendir(DIR,"../raw_data");
@wnd_files = grep(/\d+\.WND/i, readdir(DIR));
foreach $file (@ptu_files) {
	$timestamp = substr($file,0,8);
	push(@PTU_TIMES, $timestamp);
	$times{$timestamp}{"PTU"} = $TRUE;
}
foreach $file (@wnd_files) {
    $timestamp = substr($file,0,8);
	push(@WND_TIMES, $timestamp);
    $times{$timestamp}{"WND"} = $TRUE;
}
$count = 0;
foreach $time (@PTU_TIMES){
	if($times{$time}{"WND"}){
		$times{$time}{"BOTH"} = $TRUE;
		$cmd = "$program ../raw_data/$time.ptu ../raw_data/$time.wnd"; $count++; print "Executing:: $cmd\n";
	}else{
		$cmd = "$program ../raw_data/$time.ptu"; $count++; print "Executing:: $cmd\n";
	}
	system($cmd);
}
foreach $time (@WND_TIMES){
	if(!$times{$time}{"BOTH"}){
		$cmd = "$program ../raw_data/$time.wnd"; $count++; print "Executing:: $cmd\n";
		system($cmd);
	}
}

print"Number of Files processed: $count\n";
#print "$file_cnt files processed\n";
print "\nEnd proc_seward_johnson.pl\n";
