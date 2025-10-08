#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The NWS_MSS_archive.pl script is to be used for archiving the NWS data
# that is currently being stored on the local system to the mass store.  It
# archives the binary, ASCII, and RRS data for the specified year by creating
# a tar file for each type of data for each station (i.e. NWS_ASCII.2004.KMPX.tar.gz
# would be the 2004 ASCII data for station KMPX).  Once the data is in the
# tar file, the individual files are removed from the system.  The tar.gz files
# are finally copied to the final archive on the MSS and the local tar ball is
# removed.
#
# @author Joel Clawson
# @version 1.1 Update the script to archive the RRS ASCII files.  Corrected the
#     deletion of the local archive RRS files to actually delete the files once
#     they have been put into tar files.
#
# @author Joel Clawson
# @version 1.0 The original creation of the script.
##Module------------------------------------------------------------------------
use strict;
use Cwd;

# Directories for the data
my $ASCII_DIR = "../archive/ascii_files";
my $ASCII_MSS = "/JOSS/DATA/RAW/SOUNDING/NWS/ASCII";
my $BINARY_DIR = "../archive/binary_files";
my $BINARY_MSS = "/JOSS/DATA/RAW/SOUNDING/NWS/BINARY";
my $RRS_DIR = "../archive/rrs_files";
my $RRS_MSS = "/JOSS/DATA/RAW/SOUNDING/NWS/RRS";
my $RRS_ASCII_DIR = "../archive/rrs_text";
my $RRS_ASCII_MSS = "/JOSS/DATA/RAW/SOUNDING/NWS/RRS_ASCII";

# Local working directory for the script.
my $MSS_STAGE = "../mss_stage";

&main();

##------------------------------------------------------------------------
# @signature void main(int year)
# <p>Archive all of the NWS data for the specified year on the mass store
# system and remove it from the local disk.</p>
#
# @input $year The year to be archived.
##------------------------------------------------------------------------
sub main {
    if (scalar(@ARGV) != 1 || $ARGV[0] !~ /^\d{4}$/) {
	printf("Usage: NWS_MSS_archive.pl YYYY\n");
	exit(1);
    }
    my ($year) = @ARGV;
    
    # Make the working directory for the tar balls.
    create_directory($MSS_STAGE) unless(-e $MSS_STAGE);

    # Create all of the tar files to be archived on the MSS
    my @bin_files = tar_files("BINARY",$BINARY_DIR,$year);
    my @ascii_files = tar_files("ASCII",$ASCII_DIR,$year);
    my @rrs_files = tar_files("RRS",$RRS_DIR,$year);
    my @rrs_text_files = tar_files("RRS_ASCII",$RRS_ASCII_DIR,$year);

    # Archive all of the files to the MSS
    copy_to_mss($BINARY_MSS,$year,@bin_files);
    copy_to_mss($ASCII_MSS,$year,@ascii_files);
    copy_to_mss($RRS_MSS,$year,@rrs_files);
    copy_to_mss($RRS_ASCII_MSS,$year,@rrs_text_files);

    # Remove the working directory now that all the tar files are on MSS
    rmdir($MSS_STAGE) or printf("Cannot remove $MSS_STAGE directory\n");
}

##------------------------------------------------------------------------
# @signature void copy_to_mss(String dir, int year, String[] files)
# <p>Copy all of the files to the mass store.  After each successful
# copy to the mass store, the file will be removed from the local
# system.</p>
#
# @input $dir The mass store directory where the files are to be archived.
# @input $year The year of the files being archived.
# @input $files The list of files to be archived on the mass store.
##------------------------------------------------------------------------
sub copy_to_mss {
    my ($dir,$year,@files) = @_;

    foreach my $file (sort(@files)) {
	printf("Copying file %s to the mass store.\n",$file);
	if (system(sprintf("msrcp -wpwd jossdata -pe 32767 %s/%s mss:%s/%04d/%s",$MSS_STAGE,$file,$dir,$year,$file))) {
	    printf("Unable to copy %s to the mass store!  It is still in %s\n",$file,$MSS_STAGE);
	} else {
	    # Copy to MSS was successful, now remove file from local system
	    unlink(sprintf("%s/%s",$MSS_STAGE,$file)) or printf("Unable to remove %s/%s\n",$MSS_STAGE,$file);
	}
    }
}

##------------------------------------------------------------------------
# @signature void create_directory(String path)
# <p>Create the directory structure specified in the path.</p>
#
# @input $path The path to be created.
# @warning This function will die if it cannot make a directory.
##------------------------------------------------------------------------
sub create_directory {
    my $path = shift;
    my @dirs = split(/\//,$path);
    my $count = 1;
    my $accum_dir = $dirs[0];
    while ($count < scalar(@dirs)) {
        $accum_dir = sprintf("%s/%s",$accum_dir,$dirs[$count]);
        if (!(-e $accum_dir)) {
            mkdir($accum_dir) || die("Cannot create $accum_dir\n");
        }
        $count++;
    }
}

##------------------------------------------------------------------------
# @signature void tar(String outfile, String $infiles)
# <p>Create the specified tar file by adding all of the files that are
# listed in infiles and compress it using gzip.</p>
#
# @input $outfile The name of the tar file to be created.
# @input $infiles The list of files to be archived in the tar file.
# @warning The script will exit if either the tar command or the gzip
# command fail.
##------------------------------------------------------------------------
sub tar {
    my ($outfile,$infiles) = @_;

    # Create the tar file.
    if (system(sprintf("tar -cf %s %s",$outfile,$infiles))) {
	printf("The tar file $outfile could not be created!\n");
	exit(1);
    }

    # Gzip the tar file.
    if (system(sprintf("gzip %s",$outfile))) {
	printf("Unable to gzip $outfile\n");
	exit(1);
    }
}

##------------------------------------------------------------------------
# @signature String[] tar_files(String key, String basedir, int year)
# <p>Create tar files for all of the stations in the basedir for the
# specified year.</p>
#
# @input $key The key for the type of tar ball being created (ASCII, BINARY, RRS)
# @input $basedir The directory where the data to be tarred can be found.
# @input $year The year of the data to be tarred.
# @output $files The list of tar files that were created.
##------------------------------------------------------------------------
sub tar_files {
    my ($key,$basedir,$year) = @_;
    my $dir = sprintf("%s/%04d",$basedir,$year);

    # Only create tar balls if there is a directory to read.
    return unless (-e $dir);

    printf("Directory: %s\n",$dir);

    # Get the list of station directories in the basedir
    opendir(my $DIR,$dir) or die("Can't read $dir\n");
    my @dirs = grep(/^[^\.]+$/,readdir($DIR));
    closedir($DIR);

    my $cwd = cwd();

    # List for the station tar files.
    my @files = ();
    foreach my $station (sort(@dirs)) {

	# Go into the directory to only put in the data files with no directory structure
	chdir(sprintf("%s/%s",$dir,$station));
	printf("\tCreating tar ball for %s\n",cwd());

	# Get the list of files for the station
	opendir(my $STN,".") or die("Can't read ".cwd()."\n");
	my @inputfiles = grep(/^[^\.]+/,readdir($STN));
	closedir($STN);

	# Create the tar.gz file for the station and add it to the list.
	tar(sprintf("../../../%s/NWS_%s.%04d.%s.tar",$MSS_STAGE,$key,$year,$station),join(" ",@inputfiles));
	push(@files,sprintf("NWS_%s.%04d.%s.tar.gz",$key,$year,$station));

	# Remove the files that were put into the tar ball.
	foreach my $file (sort(@inputfiles)) {
	    # The RRS stations are further divided by month directories.  The entire month
	    # directory and containing files must be removed.
            if ($key =~ /RRS/i) {
                (system("rm -rf $file") == 0) or printf("Can't remove: $file\n");
            } 
	    # The MicroArt files can be removed one at a time.
	    else {
	        unlink($file) or printf("Cannot remove: $file\n");
            }
	}

	# Need to return to the start directory so all station can be processed the same
	chdir($cwd);

	# Remove the station directory now that it is empty.
	rmdir(sprintf("%s/%s",$dir,$station)) or printf("Can't remove: %s/%s\n",$dir,$station);
    }

    # Remove the year directory now that the tar files have been created and is empty.
    rmdir($dir) or printf("Can't remove %s\n",$dir);

    return @files;
}
