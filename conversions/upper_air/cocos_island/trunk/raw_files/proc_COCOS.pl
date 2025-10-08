#!/bin/perl -w
# program proc_COCOS.pl
# Darren R. Gallant
$main_dir = "/net/torrent/raid/7/PACS/cocos_island/raw_files";
chdir $main_dir;
@files = grep(/\d{8}(|\.gz)/,@ARGV);
$file_cnt = 0;
foreach $file(@files){
  $cmd = "/usr/bin/nice ../COCOS_create_class.pl $file";print $cmd,"\n";
  system($cmd);
  $cmd = "nice gzip -f COCOS*.cls";#print $cmd,"\n";
  #system($cmd);
  $file_cnt++;
}# end foreach
print "FINI $file_cnt files processed\n";
