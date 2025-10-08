#! /usr/bin/perl

# Deploys the files to the executable directory from this working directory.

use strict;
use File::Copy;

my @DEPLOY_LIST = ("NWS_ascii_archive.pl",
		   "NWS_binary_archive.pl",
                   "NWS_MSS_archive.pl",
		   "NWS_RRS_archive.pl");
my $DEPLOY_DIR = "../bin";


&main();

sub main {

    mkdir($DEPLOY_DIR) unless (-e $DEPLOY_DIR);
    
    foreach my $file (@DEPLOY_LIST) {
	chmod(0774,sprintf("%s/%s",$DEPLOY_DIR,$file));
	copy($file,sprintf("%s/%s",$DEPLOY_DIR,$file)) or die("Can't copy file $file to $DEPLOY_DIR/$file\n");
        system(sprintf("chgrp eol %s/%s", $DEPLOY_DIR, $file));
	chmod(0554,sprintf("%s/%s",$DEPLOY_DIR,$file));
    }
}
