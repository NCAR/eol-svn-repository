#!/bin/perl/ -w
# Program PERU-GTS_merge.pl Darren R. Gallant JOSS September 1st, 2000
# merges ebufr files
$TRUE = 1;$FALSE = 0;$LEAP = $FALSE;
$main_dir = "/work/1R/PACS_SND/SALLJEX_SND/peru";
$ebufr_dir =  "$main_dir/ebufr_files";
$day_dir = "$main_dir/ebufr_files/month_files";
$ebufsort = "/home/gallant/bin/ebufsort";
$merge = "/home/gallant/bin/merge_Solaris";
%months = (1=>"01",2=>"02",3=>"03",4=>"04",5=>"05",6=>"06",7=>"07",8=>"08",
9=>"09","a"=>"10","b"=>"11","c"=>"12");
@MONTHS = ("a","b","c");
@YEARS = ("2002","2003");
#@YEARS = ("1995");
#@MONTHS = (6);
foreach $year(sort @YEARS){
    $file_dir = "$ebufr_dir/$year";
    chdir $ebufr_dir;
    $count = 0;
    $mergefile = "PERU-GTS_".$year.".ebufr";
    $tarfile = "PERU-GTS_".$year.".tar";
    print "$mergefile $tarfile\n";
    opendir(ETC,"$file_dir") || die "Can't open $file_dir directory!\n";
    @files = (grep(/^\w{3,4}(\d{5}|(a|b|c)\d{4})\.ebufr/i,readdir(ETC)));
    closedir(ETC);
    if(scalar(@files)){
	chdir $file_dir;
        $merge_string = join(' ',@files);
	#$cmd = "$merge -OutFile $mergefile $merge_string";
	$cmd = "$merge -OutFile $mergefile $merge_string";
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

