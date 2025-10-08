#!/bin/perl -w
# program proc_RON_BROWN.pl
$main_dir = "/raid3/ntdir/PACS/ship";
$program = "$main_dir/PACS_create_class.pl";
if(@ARGV < 1){
    print "Usage is proc_SHIP.pl file(s)\n";
    exit;
}
$file_cnt = 0;
foreach $arg(@ARGV){
    @input = split('/',$arg);
    $cmd = "$program $input[1]";print $cmd,"\n";system($cmd); 
    $file_cnt++;
}# end foreach $arg
print "$file_cnt files processed\n";
print "fini\n";




















