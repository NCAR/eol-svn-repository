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
use lib "/work/software/conversion_modules/Version2";
use Conversions;
use Cwd;
use File::Copy;

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

    # Get the list of directories in the input directory
    foreach my $dir (sort(readdir($INPUT))) {

	# Ignore the . and .. directories.
	next if ($dir =~ /^\.+$/);

	printf("Processing directory: %s/%s ...\n",$INPUT_DIR,$dir);

	# Create the year directory if it does not exist.
	my $year_dir = sprintf("%s/%04d",$OUTPUT_DIR,substr($dir,0,4));
	mkdir($year_dir) unless(-e $year_dir);

	# Define the first date in the directory.
	my $current_date = sprintf("%04d/%02d/01",substr($dir,0,4),substr($dir,4,2));

	# Define the current working month.
	my $current_month = substr($current_date,0,7);
	$current_month =~ s/\///g;

	while ($current_month <= substr($dir,7)) {

	    # Create a date without the / to use for selecting the files.
	    my $work_date = $current_date;
	    $work_date =~ s/\///g;

	    # Define the output file.
	    my $out_file = sprintf("%s/%s.cls",$OUTPUT_DIR,$work_date);

	    # Remove the output file if it already exists.
	    unlink($out_file) if (-e $out_file);

	    # Create the day files by 'cat'ting together the files for the current date.
	    system(sprintf("cat %s/%s/*_%s*.cls > %s",$INPUT_DIR,$dir,$work_date,
			   $out_file));
	    
	    # Remove file that are 0 sized.
	    unlink($out_file) if (-z $out_file);

	    
	    create_ebufr_file($out_file,$year_dir) if (-e $out_file);
	    

	    # Advance the date and recalculate the month
	    ($current_date,undef()) = Conversions::adjustDateTime($current_date,"00:00",
								  1,0,0);
	    $current_month = substr($current_date,0,7);
	    $current_month =~ s/\///g;
	}
    }

    closedir($INPUT);

    # Remove the ebufr files since they are no longer needed.
    foreach my $file (@EBUFR_FILES) { unlink(sprintf("%s/%s",$OUTPUT_DIR,$file)); }
}

##------------------------------------------------------------------------
# @signature void create_ebufr_file(String file, String final_dir)
# <p>Convert the specified CLASS file into EBUFR and put it into the final
# directory.</p>
#
# @input $file The name of the CLASS file to be converted.
# @input $final_dir The directory where the EBUFR file should be put.
##------------------------------------------------------------------------
sub create_ebufr_file {
    my $file = shift;
    my $final_dir = shift;
    my $cwd = cwd();

    # Need to change to the directory to run the class2ebufr binary
    chdir($OUTPUT_DIR);

    # Remove the directory path of the file name.
    $file =~ s/$OUTPUT_DIR\///;

    # Convert the CLASS file to EBUFR.
    system(sprintf("/usr/bin/nice -4 ../software/class2ebufr %s",$file));

    # Remove the CLASS day file.
    unlink($file);

    # Rename the CLASS file to the EBUFR file extension.
    $file =~ s/cls/ebufr/;

    # Move the EBUFR file into the final directory.
    move($file,sprintf("%s/PACS-GTS_%s",$final_dir,$file));

    # Return to the directory where the script was before the function began.
    chdir($cwd);
}
