#!/usr/bin/perl

opendir (DIR, ".") || die "Can't open current dir: $!\n";
@files = grep { /fb$/} readdir(DIR);
closedir DIR;

foreach $file (sort @files) {
  print "Processing $file\n";
  $line = '';
  open (FB, $file) || die "Can't open $file: $!\n";
  while (<FB>) {$line .= $_};
  close FB;

  $line =~ s/VOLD(.|\R){1224}VOLD/VOLD/;

  $newfile = "$file.new";

  open(FBO, ">$newfile") || die "Can't open $file.new: $!\n";
  print FBO $line;
  close FBO;

  system("mv $newfile $file");
}
