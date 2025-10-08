#! /usr/bin/perl -w

##Module------------------------------------------------------------------
# <p>The create_ebufr_files.pl script creates a day CLASS file for each day
# in the input directory that will be used to create the EBUFR files.  It
# concatanates all of the files for a specified date into a single file
# using the <i>cat</i> system command.  It will replace any existing file
# in the output directory and remove any files that have zero size.</p>
# <p>The script will also convert the CLASS files into the EBUFR files.  
# It will place them into the output directory by year and remove the 
# CLASS day file once it has been completed.</p>
##Module------------------------------------------------------------------
use strict;
use Cwd;
use File::Copy;

# Project specific variables
my $PROJECT = "NAME";
my $PROGRAM = "/work/software/NAME/library/upper_air/EBUFR/bin/class2ebufr_Solaris";

my $INPUT_DIR = "../final";
my $OUTPUT_DIR = "../ebufr";
my @EBUFR_FILES = ("class_file","code_08_021","code_33_254","control_scf.txt","desc_file");

&main();

##------------------------------------------------------------------------
# @signature void main()
# <p>Create a day file for each day in the final directory.</p>
##------------------------------------------------------------------------
sub main {

    mkdir($OUTPUT_DIR) unless (-e $OUTPUT_DIR);

    # Move the ebufr files to the output directory where they will be needed.
    foreach my $file (@EBUFR_FILES) { copy($file,sprintf("%s/%s",$OUTPUT_DIR,$file)); }
    
    opendir(my $INPUT,$INPUT_DIR) or die("Can't open input directory.\n");
    my @files = grep(/\.cls/,readdir($INPUT));
    closedir($INPUT);

    my %dates;
    foreach my $file (@files) {
	open(my $FILE,sprintf("%s/%s",$INPUT_DIR,$file));
	my @lines = <$FILE>;
	close($FILE);

	$lines[11] =~ /(\d+),\s+(\d+),\s+(\d+)/;

	push(@{ $dates{sprintf("%04d%02d%02d",$1,$2,$3)}}, sprintf("%s/%s",$INPUT_DIR,$file));
    }

    foreach my $date (sort(keys(%dates))) {
	# Define the output file.
	my $out_file = sprintf("%s/%s.cls",$OUTPUT_DIR,$date);

	# Remove the output file if it already exists.
	unlink($out_file) if (-e $out_file);


	# Create the day files by 'cat'ting together the files for the current date.
	system(sprintf("cat %s > %s",join(' ',@{$dates{$date}}),$out_file));

	
	# Remove file that are 0 sized.
	unlink($out_file) if (-z $out_file);

	    
	create_ebufr_file($out_file) if (-e $out_file);
    }

    closedir($INPUT);

    # Remove the ebufr files since they are no longer needed.
    foreach my $file (@EBUFR_FILES) { unlink(sprintf("%s/%s",$OUTPUT_DIR,$file)); }
}

##------------------------------------------------------------------------
# @signature void create_ebufr_file(String file)
# <p>Convert the specified CLASS file into EBUFR and put it into the final
# directory.</p>
#
# @input $file The name of the CLASS file to be converted.
##------------------------------------------------------------------------
sub create_ebufr_file {
    my $file = shift;
    my $cwd = cwd();

    # Need to change to the directory to run the class2ebufr binary
    chdir($OUTPUT_DIR);

    # Remove the directory path of the file name.
    $file =~ s/$OUTPUT_DIR\///;

    # Convert the CLASS file to EBUFR.
    system(sprintf("/usr/bin/nice -4 %s %s",$PROGRAM,$file));

    # Remove the CLASS day file.
    unlink($file);

    # Rename the CLASS file to the EBUFR file extension.
    $file =~ s/cls/ebufr/;

    # Move the EBUFR file into the final directory.
    move($file,sprintf("%s_Belize_HighRes_%s",$PROJECT,$file));

    # Return to the directory where the script was before the function began.
    chdir($cwd);
}
