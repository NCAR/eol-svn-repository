#!/bin/perl/ -w
# Program NSSL-CLASS_merge.pl Darren R. Gallant JOSS September 1st, 2000
# merges ebufr files
$TRUE = 1;$FALSE = 0;$LEAP = $FALSE;
$main_dir = "/work/1R/IHOP_SND/NSSL-CLASS";
$ebufr_dir =  "$main_dir/10sec/ebufr_files";
$day_dir = "$ebufr_dir/month_files";
$ebufsort = "/home/gallant/bin/ebufsort";
$merge = "/home/gallant/bin/merge_Solaris";
$comments = qq("IHOP_2002 NSSL Class Sndings");
%months = (1=>"01",2=>"02",3=>"03",4=>"04",5=>"05",6=>"06",7=>"07",8=>"08",
9=>"09","a"=>"10","b"=>"11","c"=>"12");
@MONTHS = ("5","6");
#@MONTHS = (6);
$year = "2002";$file_dir = "$ebufr_dir";
foreach $month(sort @MONTHS){
    $mth = $months{$month};print $month,"\n";
    chdir $ebufr_dir;
    $count = 0;
    $mergefile = "NSSL_".$mth.$year.".ebufr";
    $tarfile = "NSSL_".$mth.$year.".tar";
    print "$mergefile $tarfile\n";
    opendir(ETC,"$file_dir") || die "Can't open $file_dir directory!\n";
    @files = (grep(/^NSSL$month\d{6}\.ebufr/i,readdir(ETC)));
    closedir(ETC);
    if(scalar(@files)){
	chdir $file_dir;
        $merge_string = join(' ',@files);
	#$cmd = "$merge -OutFile $mergefile $merge_string";
	$cmd = "$merge -OutFile $mergefile -Comment $comments -SuppressComments $merge_string";
	print $cmd,"\n";system($cmd);
        $cmd = "gnutar cvf $tarfile $merge_string";
        print $cmd,"\n";system($cmd);
	#$cnd = "rm -f $merge_string";print $cmd,"\n";system($cmd);
	$new_mergefile = $mergefile.".2";
	$cmd = "$ebufsort -q -i $mergefile -o $new_mergefile -p time -s lat";
	print $cmd,"\n";system($cmd);
	print "renaming $new_mergefile $mergefile\n";
	rename($new_mergefile,$mergefile);
	$cmd = "mv $mergefile $day_dir";print $cmd,"\n";system($cmd);
    }# end if
}# end foreach $month
print "Fini\n";

