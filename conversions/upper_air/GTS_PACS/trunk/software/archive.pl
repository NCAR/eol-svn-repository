#! /usr/bin/perl -w



use strict;
use File::Copy;

my $ARCHIVE = "../archive";
my @TO_ARCHIVE = ("raw_data","output","err_logs","final","ebufr");

&main();

##--------------------------------------------------------------------
# @signature void main()
# <p>Archive the data by moving it to the archive directory and gzipping
# the files to save room.</p>
##--------------------------------------------------------------------
sub main {
    
    # Loop through the directories to be archived.
    foreach my $dir (@TO_ARCHIVE) {

	my $arch_dir = sprintf("%s/%s",$ARCHIVE,$dir);
	my $read_dir = sprintf("../%s",$dir);

	# Make the archive directory as necessary
	mkdir($arch_dir) unless (-e $arch_dir);

	opendir(my $READ,$read_dir);

	# Loop through the files of the directory
	foreach my $subdir (readdir($READ)) {
	    if (-d sprintf("%s/%s",$read_dir,$subdir)) {
		archive_subdir($subdir,$read_dir,$arch_dir) unless ($subdir =~ /^\.+$/);
	    } else {
		archive_file($subdir,$read_dir,$arch_dir) unless ($subdir =~ /^\.+$/);
	    }
	}

	# See how many files are left in the directory.
	my $count = scalar(readdir($READ));

	closedir($READ);

	# Remove the directory if it is empty and it is not the raw_data directory.
	rmdir($read_dir) if ((!defined($count) || $count == 0) && $read_dir !~ /raw_data/);
    }
}

##--------------------------------------------------------------------
# @signature void archive_file(String file, String input_dir, String output_dir)
# <p>Move the file from the input directory to the output directory and
# gzip it.</p>
#
# @input $file The file to be moved.
# @input $input_dir The directory where the file is currently located.
# @input $output_dir The directory where the file is to be moved.
##--------------------------------------------------------------------
sub archive_file {
    my ($file, $input_dir,$output_dir) = @_;

    # Move the file.
    move(sprintf("%s/%s",$input_dir,$file),
	 sprintf("%s/%s",$output_dir,$file));
    
    # Gzip the file.
    system(sprintf("gzip %s/%s",$output_dir,$file));
}

##--------------------------------------------------------------------
# @signature void archive_subdir(String dir, String input_dir, String output_dir)
# <p>Archive the files in the current subdirectory.</p>
#
# @input $dir The directory to be archived.
# @input $input_dir The directory where the subdirectory is located.
# @input $output_dir The directory where the subdirectory it to be moved.
# @warning This is a recursive function that could continuously go down the directory
# tree.
##--------------------------------------------------------------------
sub archive_subdir {
    my ($dir,$input_dir,$output_dir) = @_;

    my $in_subdir = sprintf("%s/%s",$input_dir,$dir);
    my $out_subdir = sprintf("%s/%s",$output_dir,$dir);

    mkdir($out_subdir) unless (-e $out_subdir);

    opendir(my $SUBDIR,$in_subdir);
    
    foreach my $file (readdir($SUBDIR)) {
	if (-d sprintf("%s/%s",$in_subdir,$file)) {
	    archive_subdir($file,$in_subdir,$out_subdir) unless ($file =~ /^\.+$/);
	} else {
	    archive_file($file,$in_subdir,$out_subdir) unless ($file =~ /^\.+$/);
	}
    }

    my $count = scalar(readdir($SUBDIR));

    closedir($SUBDIR);

    rmdir($in_subdir) if (!defined($count) || $count == 0);
}
