#! /usr/bin/perl -w
# program proc_seward_johnson.pl
$program = "./seward_johnson.pl";
$TRUE = 1;
#$file_cnt = 0;
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
		$cmd = "$program ../raw_data/$time.ptu ../raw_data/$time.wnd"; $count++; print "$cmd\n";
	}else{
		$cmd = "$program ../raw_data/$time.ptu"; $count++; print "$cmd\n";
	}
	system($cmd);
}
foreach $time (@WND_TIMES){
	if(!$times{$time}{"BOTH"}){
		$cmd = "$program ../raw_data/$time.wnd"; $count++; print "$cmd\n";
		system($cmd);
	}
}

print"$count\n";
#print "$file_cnt files processed\n";
print "fini\n";
