#!/bin/perl -w
# program proc_RON_BROWN.pl
$main_dir = "/raid3/ntdir/PACS/ship/ron_brown";
$program = "$main_dir/RON_BROWN.pl";
$FALSE = 0; $TRUE = 1;
$file_cnt = 0;
opendir(DIR,"$main_dir/ascii_files");
@files = grep(/\d+\.(ptu|wnd)/i, readdir(DIR));
foreach $file (@files) {
	$timestamp = substr($file,0,8);
	push(@times, $timestamp);
}
@sorted_times = sort by_number @times;
$number = scalar(@sorted_times);
$x = 0;@new_list= ();
until ($x == $number) {
	if($sorted_times[$x] == $sorted_times[$x+1]){
		$part = $sorted_times[$x];
		push(@new_list, $part); $x += 2; 
		$times{$part}{"BOTH"} = $TRUE;
	}else{
		$part = $sorted_times[$x];
		push(@new_list, $part); $x++;
		$times{$part}{"BOTH"} = $FALSE; 
	}
}
foreach $time(@new_list){
	if($times{$time}{"BOTH"}){
		$cmd = "$program $time.ptu $time.wnd"; print "$cmd\n";$file_cnt++;
	}else{
		foreach $file(@files){
			if (grep(/$time/, $file)){
				$cmd = "$program $file"; print "$cmd\n";$file_cnt++;
			}
		}
	}
}
$new_number = scalar(@new_list);
#print "$new_number\n";	
print "$file_cnt files processed\n";
print "fini\n";

sub by_number {$a<=>$b};

