#!/bin/perl/ -w
# Program ARM_merge.pl Darren R. Gallant JOSS September 1st, 2000
# merges ebufr files
$TRUE = 1;$FALSE = 0;$LEAP = $FALSE;
$main_dir = "/work/1R/gallant/ARM";
$ebufr_dir =  "$main_dir/ebufr_files";
$day_dir = "$main_dir/ebufr_files/year_files";
$ebufsort = "/home/gallant/bin/ebufsort";
$merge = "/home/gallant/bin/merge_Solaris";
@YEARS = ("2001","2002","2003");
foreach $year(sort @YEARS){
    $file_dir = "$ebufr_dir/$year";
    chdir $ebufr_dir;
    $count = 0;
    $mergefile = "ARM_".$year.".ebufr";
    $tarfile = "ARM_".$year.".tar";
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

