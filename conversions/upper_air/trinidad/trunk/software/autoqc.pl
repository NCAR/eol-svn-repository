#! /usr/bin/perl -w

##Module-----------------------------------------------------------------------
# <p>The autoqc.pl script is a script that runs the autoqc binary file on all
# of the files in the INPUT_DIR.  It places the files in the OUTPUT_DIR and 
# moves all of the error log files that auto qc generates in the ERR_DIR.</p>
#
# THIS SCRIPT IS TO RUN THE OLD, ORIGINAL C VERSION OF THE AUTO qc. DON'T
# USE THIS!!!!  Use the new Java form of the autoQC.
#
##Module-----------------------------------------------------------------------
use strict;
use File::Copy;

my $AUTOQC = "/work/software/RAINEX/library/upper_air/autoQC/bin/autoqc_RAINEX_5point_avg";

my $INPUT_DIR = "../averaged";
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
    foreach my $file (grep(/\.cls$/,readdir($IN))) {
	
	# Run the AUTO QC on the file
	system(sprintf("%s %s/%s %s/%s",$AUTOQC,$INPUT_DIR,$file,$OUTPUT_DIR,$file));

	# Move the ERR log to the ERR_DIR
	move(sprintf("%s/%s.err",$INPUT_DIR,$file),$ERR_DIR);
    }
    closedir($IN);
}


