#! /usr/bin/perl

# Deploys the files to the executable directory from this working directory.

use strict;
use File::Copy;

my $BIN_SRC = "src";
my $BIN_DEST = "/net/work/bin/plotters";
my $DOC_SRC = "docs";
my $DOC_DEST = "/net/web/dmg/html/software/tools/upper_air/plot_skewt";

&main();

sub main {

    mkdir($BIN_DEST) unless (-e $BIN_DEST);
    mkdir($DOC_DEST) unless (-e $DOC_DEST);

    opendir(my $BIN, $BIN_SRC) or die("Can't read $BIN_SRC\n");
    foreach my $file (grep(/\.pl$/, readdir($BIN))) {
	chmod(0774,sprintf("%s/%s",$BIN_DEST,$file));
	copy(sprintf("%s/%s", $BIN_SRC, $file), sprintf("%s/%s",$BIN_DEST,$file)) or die("Can't copy file $file to $BIN_DEST/$file\n");
	chmod(0554,sprintf("%s/%s",$BIN_DEST,$file));
        system("chgrp cds $BIN_DEST/$file");
    }
    closedir($BIN);

    opendir(my $DOC, $DOC_SRC) or die("Can't read $DOC_SRC\n");
    foreach my $file (grep(/\.html$/, readdir($DOC))) {
       chmod(0774, sprintf("%s/%s", $DOC_DEST, $file));
       copy(sprintf("%s/%s", $DOC_SRC, $file), sprintf("%s/%s", $DOC_DEST, $file)) or die("Can't copy file $file to $DOC_DEST/$file\n");
       system("chgrp cds $DOC_DEST/$file");
       chmod(0444, sprintf("%s/%s", $DOC_DEST, $file));
    }
}
