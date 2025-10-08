#! /usr/bin/perl -w

##Module---------------------------------------------------------------------------------
# <p>The copy_class_files.pl script is used for copying CLASS formated files from the
# final working directory of individual networks to a DATA_DIR in the composite directory
# to be used in the day file generation.  It will search for files that end in both .cls
# and .cls.gz.  It will only copy the files that are in the project's time of interest by
# reading the nominal release time in the data files.  All files that are copied and not
# gzipped, will be gzipped.</p>
# <p>This script requires the user to edit the project's start and end date along with
# telling it which networks should be included in the composite and where the final data
# is located to be copied.</p>
#
# @author Joel Clawson 02/02/2006
# @version 1.0 The original creation of the script.
##Module---------------------------------------------------------------------------------
use strict;
use lib "/work/software/NAME/library/conversion_modules/Version5";
use DpgDate;
use File::Copy;

# The Hash of networks to include in the composite
my $networks;
$networks->{"GLASS"} = "/work/NAME/upper_air/GLASS/final";
$networks->{"ISS"} = "/work/NAME/upper_air/ISS/final";
$networks->{"NWS"} = "/work/NAME/upper_air/NWS/final";
$networks->{"RV_altair"} = "/work/NAME/upper_air/RV_altair/final";
$networks->{"RV_el_puma"} = "/work/NAME/upper_air/RV_el_puma/tethersonde/final";
$networks->{"RV_ulloa"} = "/work/NAME/upper_air/RV_ulloa/final";
$networks->{"SMN"} = "/work/NAME/upper_air/SMN/final";
$networks->{"belize"} = "/work/NAME/upper_air/belize/final";
$networks->{"galapagos"} = "/work/PACS/upper_air/galapagos/soundings/archive/200506/final";
$networks->{"phoenix"} = "/work/NAME/upper_air/phoenix/final";
$networks->{"pibal"} = "/work/NAME/upper_air/pibal/final";
$networks->{"san_jose"} = "/work/NAME/upper_air/san_jose/final";
$networks->{"yuma"} = "/work/NAME/upper_air/yuma/final";

# The project time of interest dates
my $START_DATE = "20040601";
my $END_DATE = "20040930";

# The directory where the CLASS files are to be kept for turning into day files.
my $DATA_DIR = "../data";

&main();

##---------------------------------------------------------------------------------
# @signature void main()
# <p>Copy all of the sounding class files for the project's time of interest to
# the DATA_DIR and compress them if they have not already been compressed.</p>
#
# @warning Only the files that have a nominal release time in the project's time
# of interest will be copied to the DATA_DIR.
##---------------------------------------------------------------------------------
sub main {

    # Remove all of the old data (To prevent from accidental use of older data).
    system("rm -rf $DATA_DIR") if (-e $DATA_DIR);
    mkdir($DATA_DIR);

    # Loop through all of the networks that are to be included in the composite.
    foreach my $network (keys(%{ $networks})) {

	# Create a directory for the network.
	mkdir(sprintf("%s/%s",$DATA_DIR,$network)) or die("Can't create $network directory\n");
	
	opendir(my $DATA,$networks->{$network}) or die(sprintf("Can't read %s\n",$networks->{$network}));

	# Look for the class files in the directory (may be gzipped)
	foreach my $file (grep(/\.cls(\.gz)?$/,readdir($DATA))) {

	    # Get the nominal release time from the CLASS file
	    open(my $FILE,sprintf("zmore %s/%s |",$networks->{$network},$file)) or
		die("Can't read file: $file\n");
	    my $nom_line = (<$FILE>)[12];
	    close($FILE);

	    # Parse out the release time.
	    $nom_line =~ /:\s*(\d{4}, \d{2}, \d{2})/;

	    # Only copy files that are within the project time of interest
	    if (compareDates($1,"YYYY, MM, DD",$START_DATE,"YYYYMMDD") <= 0 &&
		compareDates($1,"YYYY, MM, DD",$END_DATE,"YYYYMMDD") >= 0) {

		# Copy the file.
		copy(sprintf("%s/%s",$networks->{$network},$file),
		     sprintf("%s/%s/%s",$DATA_DIR,$network,$file)) or die("Can't copy $file!\n");
		
		# Gzip the file if it is not already
		gzip(sprintf("%s/%s/%s",$DATA_DIR,$network,$file)) unless ($file =~ /\.gz$/);
	    }
	}
	closedir($DATA);
    }
}

##---------------------------------------------------------------------------------
# @signature void gzip(String file)
# <p>Compress the specified file using gzip.</p>
#
# @input $file The file to be compressed.
# @warning This function will die if the file cannot be gzipped.
##---------------------------------------------------------------------------------
sub gzip {
    my ($file) = @_;

    system("gzip $file") == 0 or die("Can't gzip $file\n");
}
