#!/bin/perl -w
# program proc_RON_BROWN.pl
$main_dir = "/raid3/ntdir/PACS/ship/ron_brown";
$program = "$main_dir/RON_BROWN.pl";
$FALSE = 0; $TRUE = 1;
$file_cnt = 0;
opendir(DIR,"$main_dir/ascii_files");
@ptu_files = grep(/\d+\.ptu/i, readdir(DIR));
opendir(DIR,"$main_dir/ascii_files");
@wnd_files = grep(/\d+\.wnd/i, readdir(DIR));
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
open(OUT, ">tmp_rb");
foreach $time (@PTU_TIMES){
	if($times{$time}{"WND"}){
		$times{$time}{"BOTH"} = $TRUE;
		$cmd = "$program $time.ptu $time.wnd"; $count++; #print "$cmd\n";
	}else{
		$cmd = "$program $time.ptu"; $count++; #print "$cmd\n";
	}
}
foreach $time (@WND_TIMES){
	if(!$times{$time}{"BOTH"}){
		$cmd = "$program $time.wnd"; $count++; #print "$cmd\n";
	}
}

print"$count\n";
#print "$file_cnt files processed\n";
print "fini\n";




















