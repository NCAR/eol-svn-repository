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

if (-e "/net/work") {
    use lib "/net/work/software/TREX/library/conversion_modules/Version6";
} else {
    use lib "/work/software/TREX/library/conversion_modules/Version6";
}
use DpgDate;
use File::Copy;

my $WORK = -e "/net/work" ? "/net/work" : "/work";

# The Hash of networks to include in the composite
my $networks;
$networks->{"AFRL"} = "$WORK/TREX/upper_air/AFRL/final";
$networks->{"ISS"} = "$WORK/TREX/upper_air/ISS/final";
$networks->{"MGAUS"} = "$WORK/TREX/upper_air/MGAUS/final";
$networks->{"NWS"} = "$WORK/TREX/upper_air/NWS/final";
$networks->{"BAE146"} = "$WORK/TREX/upper_air/bae-146/final";
$networks->{"ChinaLake"} = "$WORK/TREX/upper_air/china_lake/final";
$networks->{"HIAPER"} = "$WORK/TREX/upper_air/hiaper/final";
$networks->{"Leeds"} = "$WORK/TREX/upper_air/leeds/final";
$networks->{"LeMoore"} = "$WORK/TREX/upper_air/lemoore/final";

# The project time of interest dates
my $START_DATE = "20060301";
my $END_DATE = "20060430";

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
        my @files = grep(/\.cls(\.short)?\.qc(\.gz)?$/,readdir($DATA));
        rewinddir($DATA);
        @files = grep(/\.cls(\.short)?(\.gz)?$/,readdir($DATA)) if (@files == 0);
        closedir($DATA);

	# Look for the class files in the directory (may be gzipped)
	foreach my $file (@files) {

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

		# Now that the file is gzipped, make sure the file name matches.
		$file .= ".gz";
	    }


	    if ($file =~ /(.+\.cls)(\.short)?\.qc\.gz$/) {
                move(sprintf("%s/%s/%s",$DATA_DIR,$network,$file),
		     sprintf("%s/%s/%s.gz",$DATA_DIR,$network,$1)) or die("Can't move $file\n");
            }
	}
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
