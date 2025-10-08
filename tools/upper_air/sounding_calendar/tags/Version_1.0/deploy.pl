#! /usr/bin/perl

# Deploys the files to the executable directory from this working directory.

use strict;
use File::Copy;

my @DEPLOY_LIST = ("src/sounding_calendar_inventory.pl");
my @DOC_LIST = ("docs/howto_sounding_inventory.html");
my $DEPLOY_DIR = "/net/work/bin/scripts/inventory";
my $DEPLOY_DOC = "/net/web/dmg/html/software/tools/upper_air/sounding_calendar_inventory";

&main();

sub main {

    mkdir($DEPLOY_DIR) unless (-e $DEPLOY_DIR);
    mkdir($DEPLOY_DOC) unless (-e $DEPLOY_DOC);    

    foreach my $file (@DEPLOY_LIST) {
        $file =~ s/^src\///;
	chmod(0774,sprintf("%s/%s",$DEPLOY_DIR,$file));
	copy(sprintf("src/%s", $file), sprintf("%s/%s",$DEPLOY_DIR,$file)) or die("Can't copy file $file to $DEPLOY_DIR/$file\n");
	chmod(0554,sprintf("%s/%s",$DEPLOY_DIR,$file));
        system("chgrp cds $DEPLOY_DIR/$file");
    }

    foreach my $file (@DOC_LIST) {
       $file =~ s/^docs\///;
       chmod(0774, sprintf("%s/%s", $DEPLOY_DOC, $file));
       copy(sprintf("docs/%s", $file), sprintf("%s/%s", $DEPLOY_DOC, $file)) or die("Can't copy file $file to $DEPLOY_DOC/$file\n");
       system("chgrp cds $DEPLOY_DOC/$file");
       chmod(0444, sprintf("%s/%s", $DEPLOY_DOC, $file));
    }
}
