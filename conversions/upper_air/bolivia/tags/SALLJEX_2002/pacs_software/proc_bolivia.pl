#!/bin/perl -w
# program proc_galapagos.pl

$main_dir = "/raid3/ntdir/PACS/bolivia_raobs";
$program = "$main_dir/bolivia_create_class.pl";
$logfile = "$main_dir/bolivia_log.txt";

open(LOG, ">>$logfile") || die "Can't open $logfile\n";

@files = <raw_files/*dat raw_files/bad_data/*dat>;
#opendir(DIR,"$main_dir/raw_files/");
#@files = (grep(/.*dat$/,readdir(DIR)));
#@files = (grep(/^1998032415\.dat$/,readdir(DIR)));
#closedir(DIR);

&timestamp;

$file_cnt = 0;
foreach $file (sort @files){
    $cmd = "$program $file";print "$cmd\n";
	system($cmd); 
	$file_cnt++;
}# end foreach $arg

&timestamp;

print LOG "$file_cnt files processed\n";close(LOG);
print "$file_cnt files processed\n";
print "fini\n";

sub timestamp{
    my ($sec,$min,$hour,$mday,$mon,$year,$wday,$julian,$isdst) = gmtime(time);
    $mon+=1;
    if($mday < 10 && length($mday) < 2){$mday = "0$mday";} 
    if($mon < 10 && length($mon) < 2){$mon = "0$mon";}
    if($year < 10 && length($year) < 2){$year = "0$year";}
    if($hour < 10 && length($hour) < 2){$hour = "0$hour";}
    if($min < 10 && length($min) < 2){$min = "0$min";}
    if($sec < 10 && length($sec) < 2){$sec = "0$sec";}
    $TIME = sprintf("%s%s%s%s%s",$hour,":",$min,":",$sec);
    $DATE = sprintf("%s%s%s%s%s",$mon,"/",$mday,"/",$year);
    print LOG "GMT time and day $TIME $DATE\n";  
}# end sub timestamp
