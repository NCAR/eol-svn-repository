#!/bin/perl -w
# takes command line arguement directoryname or station name
# runs 2secavg_autoqc_TROP
# located in /raid/gcip/PACS/class_format
$main_dir = "/raid/7/PACS/cocos_island";
$program = "/data3/toga/programs/p2p_PACS_autoqc_TROPLAT";
$class_dir = "$main_dir/class_format";
#$class_dir = "$main_dir/tape_extract";
chdir $main_dir;
opendir(ETC,"$class_dir") || die "unable to open $class_dir!\n";
#read all files
@files=(grep(/^COC(\d{7}|\w\d{6})\.cls(|\.gz)$/i,readdir(ETC)));
closedir(ETC);
@files = sort(@files);
chdir $class_dir;$file_count = 0;
foreach $file (sort @files){
   if($file =~ /\.gz/){
     $pos = index($file,".gz");
     $infile = substr($file,0,$pos);
     $cmd = "/usr/bin/nice gunzip $file";system($cmd);
     print $cmd,"\n";   
   }else{
     $infile = $file;
   }# end if-else

   $outfile = $infile.".2";
   #print "$infile $outfile\n";
   $cmd = "nice $program $infile $outfile";
   print $cmd,"\n";system($cmd);
   rename($outfile,$infile);
   $cmd = "/usr/bin/nice gzip -f $infile";print $cmd,"\n";system($cmd);
   #$cmd = "/usr/bin/nice gzip -f $outfile";print $cmd,"\n";system($cmd);
   $file_count++;
}# end foreach loop
print "$file_count files processed\n";
print "fini\n";



















