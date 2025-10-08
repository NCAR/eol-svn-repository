#! /usr/bin/perl -w

##Module-----------------------------------------------------------------------
# <p>The autoqc.pl script is a script that runs the autoqc binary file on all
# of the files in the INPUT_DIR.  It places the files in the OUTPUT_DIR and 
# moves all of the error log files that auto qc generates in the ERR_DIR.</p>
##Module-----------------------------------------------------------------------
use strict;
use File::Copy;

my $INPUT_DIR = "../output";
my $OUTPUT_DIR = "../final";
my $ERR_DIR = "../logs";

&main();

##----------------------------------------------------------------------------
# @signature void main()
# <p>Run the auto qc on all of the CLASS files in the INPUT_DIR.</p>
##----------------------------------------------------------------------------
sub main {
    mkdir($OUTPUT_DIR) unless (-e $OUTPUT_DIR);
    mkdir($ERR_DIR)    unless (-e $ERR_DIR);

    opendir(my $IN,$INPUT_DIR);

    # Read in the directories of files.
    foreach my $file (grep(/\.cls/,readdir($IN))) {

	if ($file =~ /^BRW/i) {
	    # Special Case for Barrow, Alaska
	    system(sprintf("../../autoqc/bin/autoqc_ALASKA %s/%s %s/%s",
			   $INPUT_DIR,$file,$OUTPUT_DIR,$file));
	} else {
	    # The rest of the sites are in the plains
	    system(sprintf("../../autoqc/bin/autoqc_PLAINS %s/%s %s/%s",
			   $INPUT_DIR,$file,$OUTPUT_DIR,$file));
	}
	
	# Move the ERR log to the ERR_DIR
	move(sprintf("%s/%s.err",$INPUT_DIR,$file),$ERR_DIR);
    }
    closedir($IN);
}


