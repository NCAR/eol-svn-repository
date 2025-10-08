#! /usr/bin/perl

# Deploys the files to the executable directory from this working directory.

use strict;
use File::Copy;

my $BIN_SRC = "src";
my $BIN_DEST = "/net/work/lib/perl/Precip";

&main();

sub main {

    mkdir($BIN_DEST) unless (-e $BIN_DEST);

    opendir(my $BIN, $BIN_SRC) or die("Can't read $BIN_SRC\n");
    foreach my $file (grep(/.+\..+$/, readdir($BIN))) {
        if ($file !~ /^Test/i) {
	    chmod(0774,sprintf("%s/%s",$BIN_DEST,$file));
	    copy(sprintf("%s/%s", $BIN_SRC, $file), sprintf("%s/%s",$BIN_DEST,$file)) or die("Can't copy file $file to $BIN_DEST/$file\n");
	    chmod(0554,sprintf("%s/%s",$BIN_DEST,$file));
            system("chgrp cds $BIN_DEST/$file");
        }
    }
    closedir($BIN);
}
