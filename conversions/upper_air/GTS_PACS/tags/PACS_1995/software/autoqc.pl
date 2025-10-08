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
my $ERR_DIR = "../err_logs";

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
    foreach my $dir (grep(/\-/,readdir($IN))) {
	
	# Define the files.
	my $out_dir = sprintf("%s/%s",$OUTPUT_DIR,$dir);
	my $err_dir = sprintf("%s/%s",$ERR_DIR,$dir);
	
	# Make directories as needed.
	mkdir($out_dir) unless (-e $out_dir);
	mkdir($err_dir) unless (-e $err_dir);

	opendir(my $INPUT,sprintf("%s/%s",$INPUT_DIR,$dir));

	# Loop through the class files in the directory.
	foreach my $file (grep(/\.cls$/,readdir($INPUT))) {
	    
	    # Run the AUTO QC on the file
	    system(sprintf("/work/software/PACS/library/upper_air/autoQC/bin/autoqc_PACS %s/%s/%s %s/%s",
			   $INPUT_DIR,$dir,$file,$out_dir,$file));

	    # Move the ERR log to the ERR_DIR
	    move(sprintf("%s/%s/%s.err",$INPUT_DIR,$dir,$file),$err_dir);
	}

	closedir($INPUT);
    }
    closedir($IN);
}
