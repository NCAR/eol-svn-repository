#! /usr/bin/perl -w

##Module------------------------------------------------------------------
# <p>The create_highres_ebufr_files.pl script is a script to generate EBUFR'ed
# day files for a Highest Resolution Sounding Composite.  It assumes that
# all of the files in the INPUT_DIR are to be included in the composite and
# that they are either .cls or .cls.gz files.</p>
# <p>The script reads all of the files, reads the nominal release time from
# the data and assigns them to a day file.  A day file is created by using
# the <i>natural sort order</i> of the file names (which assumes the files are
# named ID_YYYYMMDDHHMM.cls).  This will put all of the soundings for a
# station together and in order of the time of their release.</p>
# <p>The script will also keep counts for each station and in total of how
# many soundings are included in the composite.  This is printed out after
# all of the EBUFR files have been created.</p>
#
# @author Joel Clawson
# @version 1.0 The original creation of this script.
##Module------------------------------------------------------------------
use strict;
use Cwd;
use File::Copy;

# Project specific variables
my $PROJECT = "NAME";
my $PROGRAM = "/work/software/NAME/library/upper_air/EBUFR/bin/class2ebufr_Solaris";

my $INPUT_DIR = "../data";
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
    
    my @files = ();
    my $sounding_count;

    opendir(my $INPUT,$INPUT_DIR) or die("Can't open input directory.\n");
 
    # Get all of the class files in each network directory
    foreach my $dir (grep(/^[^\.]+$/,readdir($INPUT))) {
	$sounding_count->{$dir} = 0;
	opendir(my $DIR,sprintf("%s/%s",$INPUT_DIR,$dir)) or die("Can't read $dir\n");
	foreach my $file (grep(/\.cls/,readdir($DIR))) {
	    push(@files,sprintf("%s/%s",$dir,$file));
	    $sounding_count->{$dir}++;
	}
	closedir($DIR);

    }
    closedir($INPUT);

    # Get the list of dates in the data files to know which day files to create.
    my %dates;
    foreach my $file (@files) {
	open(my $FILE,sprintf("zmore %s/%s |",$INPUT_DIR,$file));
	my @lines = <$FILE>;
	close($FILE);

	$lines[12] =~ /(\d+),\s+(\d+),\s+(\d+)/;

	push(@{ $dates{sprintf("%04d%02d%02d",$1,$2,$3)}}, sprintf("%s/%s",$INPUT_DIR,$file));
    }

    foreach my $date (sort(keys(%dates))) {
	# Define the output file.
	my $out_file = sprintf("%s/%s.cls",$OUTPUT_DIR,$date);

	# Remove the output file if it already exists.
	unlink($out_file) if (-e $out_file);

	# Create the day files by 'cat'ting together the files for the current date.
	system(sprintf("gzcat %s > %s",join(' ',@{$dates{$date}}),$out_file));
	
	# Remove file that are 0 sized.
	unlink($out_file) if (-z $out_file);


	# Make the EBUFR day file if there is data to make it
	create_ebufr_file($out_file) if (-e $out_file);
    }

    closedir($INPUT);

    # Remove the ebufr files since they are no longer needed.
    foreach my $file (@EBUFR_FILES) { unlink(sprintf("%s/%s",$OUTPUT_DIR,$file)); }

    # Print out the number of soundings for each network and the total
    # included in the final dataset.
    my $sum = 0;
    foreach my $network (sort(keys(%{ $sounding_count}))) {
	printf("%s: %d\n",$network,$sounding_count->{$network});
	$sum += $sounding_count->{$network};
    }
    printf("\nTotal: %d\n\n",$sum);
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
    move($file,sprintf("%s_HighRes_%s",$PROJECT,$file));

    # Return to the directory where the script was before the function began.
    chdir($cwd);
}
