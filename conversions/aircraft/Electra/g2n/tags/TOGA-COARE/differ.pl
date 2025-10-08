#!/usr/bin/perl

use strict;
use FindBin qw($Bin);

#chdir "$Bin" or die "Unable to set Bin Directory";

my @files;
if ($#ARGV < 0) {
    opendir(DIR, ".");
    @files = grep(/\.nc$/,readdir(DIR));
    closedir(DIR);
}
else
{
    @files = @ARGV;
}

foreach my $file (@files) {
    print "Dumping $file\n";
    system("ncdump $file > DUMP.$file");
    print "Dumping old/$file\n";
    system("ncdump ./old/$file > DUMP.old.$file");
    print "Running DIFF\n\n";
    system("diff DUMP.$file DUMP.old.$file");
    print "Continue? ";
    my $cont = <>;
    chomp($cont);
    if ($cont ne "") { last; }
}

