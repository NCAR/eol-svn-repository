#! /usr/bin/perl -w

##Module----------------------------------------------------------------------------
# <p>The create_5mb_files.pl script is a script that cycles through all of the networks
# in the INPUTDIR and extracts the 5 mb level from all of the CLASS formatted files.
# It uses the FORTRAN <code>extract5mb</code> or <code>s2m</code> software to extract
# the 5 mb levels from the CLASS files.  It will extract the 5mb levels on all of the
# CLASS files that are found in the INPUTDIR.</p>
#
# @author Joel Clawson 02/03/2006
# @version 1.0 The original creation of the script.
##Module----------------------------------------------------------------------------
use strict;
use Cwd;
use File::Copy;

# Make all errors print to STDOUT
*STDERR = *STDOUT;

# The location of the 5mb extraction software
my $PROGRAM = "../../software/extract5mb";

# The constants for directories used by the script.
my $INPUTDIR = "../data";
my $FINALDIR = "../5mb";
my $LOGDIR = "../logs";

&main();

##----------------------------------------------------------------------------------
# @signature void main()
# <p>Create 5mb files from all of the CLASS files in the INPUTDIR.</p>
#
# @warning This removes all old data from previous runs in the directories it uses
# for storing the output files.
##----------------------------------------------------------------------------------
sub main {

    # Remove all old data and create new directories.
    system("rm -rf $FINALDIR") if (-e $FINALDIR);
    system("rm -rf $LOGDIR") if (-e $LOGDIR);
    mkdir($FINALDIR);
    mkdir($LOGDIR);

    opendir(my $DIR,$INPUTDIR) or die("Can't open directory: $INPUTDIR\n");
    my @networks = grep(/^[^\.]+$/,readdir($DIR));
    closedir($DIR);

    foreach my $network (sort(@networks)) {

	# Make individual network directories in output directories.
	mkdir(sprintf("%s/%s",$FINALDIR,$network)) unless (-e sprintf("%s/%s",$FINALDIR,$network));
	mkdir(sprintf("%s/%s",$LOGDIR,$network)) unless (-e sprintf("%s/%s",$LOGDIR,$network));

	# Get the list of CLASS files for the network
	opendir(my $DIR,sprintf("%s/%s",$INPUTDIR,$network)) or die("Can't open directory: $network\n");
	my @files = grep(/\.cls$/,readdir($DIR));
	closedir($DIR);

	# Move to the data directory
	chdir(sprintf("%s/%s",$INPUTDIR,$network)) or die("Can't change to $INPUTDIR/$network\n");
	printf("Working in: %s\n",cwd());

	# Loop through all of the CLASS files for the network
	foreach my $file (@files) {

	    printf("\t%s %s\n",$PROGRAM,$file);

	    # Extract the 5mb levels from the file.
	    system(sprintf("%s %s",$PROGRAM,$file));

	    # Rename the 5mb and move it to the final output directory
	    $file =~ s/cls$/05mb/;
	    move($file,sprintf("../%s/%s/%s",$FINALDIR,$network,$file)) or die("Can't copy to final directory\n");

	    # Rename the log file and move it to the log directory
	    $file =~ s/05mb$/log/;
	    move($file,sprintf("../%s/%s/%s",$LOGDIR,$network,$file)) or die("Can't copy to log direcotry\n");
	}

	# Undo the change directory and return to the cwd.
	chdir("../../software");
    }
}
