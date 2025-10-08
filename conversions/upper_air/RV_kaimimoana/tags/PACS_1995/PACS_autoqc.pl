#!/bin/perl -w
# takes command line arguement directoryname or station name
# runs 2secavg_autoqc_TROP
# located in /raid3/ntdir/PACS/ship/class_files
$main_dir = "/raid3/ntdir/PACS/ship";
$program = "$main_dir/p2p_PACS_autoqc_TROPLAT";
$class_dir = "$main_dir/class_files";
if(@ARGV<1){
    print "Usage is PACS_autoqc.pl dir(s)\n";
    exit;
}
chdir $main_dir;
foreach $year(@ARGV){
    $file_dir = "$class_dir/$year";
    opendir(ETC,"$file_dir") || die "unable to open $file_dir!\n";
#read all files
    @files=(grep(/^(DIS|KAI)(\d{5}|\w\d{4})\.cls(|\.gz)$/i,readdir(ETC)));
    closedir(ETC);
    @files = sort(@files);
    chdir $file_dir;$file_count = 0;
    foreach $file (sort @files){
	if($file =~ /\.gz/){
	    $pos = index($file,".gz");
	    $infile = substr($file,0,$pos);
	    $cmd = "gunzip $file";system($cmd);
	    print $cmd,"\n";   
	}else{
	    $infile = $file;
	}# end if-else
	$outfile = $infile.".2";
	#print "$infile $outfile\n";
	$cmd = "$program $infile $outfile";
	print $cmd,"\n";system($cmd);
	rename($outfile,$infile);
	#$cmd = "gzip -f $infile";print $cmd,"\n";system($cmd);
	#$cmd = "gzip -f $outfile";print $cmd,"\n";system($cmd);
	$file_count++;
    }# end foreach loop
}# end foreach $year
print "$file_count files processed\n";
print "fini\n";



















