#! /usr/bin/perl -w

##Module-------------------------------------------------------------
#
#
# @author Joel Clawson
##Module-------------------------------------------------------------
use strict;
use File::Copy;

# Constants
my $CLASS_DIR = ".";
my $PLOT_DIR = ".";

my @PRESS_LIMITS;
my @TEMP_LIMITS;

my $WIND_CLEAR_FREQ = 0;
my $KEEP_TMP_FILES = 0;
my $REMOVE_DESCENT = 0;
my $REMOVE_DESCENT_COUNT = 3;

&main();

##------------------------------------------------------------------------
# @signature void main()
# <p>Run the script.</p>
##------------------------------------------------------------------------
sub main {
    read_args(@ARGV);

    create_directory($PLOT_DIR) or die("Can't create $PLOT_DIR\n");
    create_plots();
}

##------------------------------------------------------------------------
# @signature void clear_wind_params(String new_file)
# <p>Remove some of the wind parameter values in the class file to make
# the wind part of the skewt easier to read.</p>
#
# @input $new_file The file to have its wind values removed.
##------------------------------------------------------------------------
sub clear_wind_params {
    my ($new_file) = @_;

    # Open the two files.
    open(my $INPUT,$new_file) or die("Cannot open: $new_file\n");
    my @lines = <$INPUT>;
    close($INPUT);

    open(my $OUTPUT,">$new_file") or die("Cannot open: $new_file\n");

    my $line_count = 1;
    foreach my $line (@lines) {
	
	# Print out the header as is.
	if ($line_count < 16) { 
	    if ($line =~ /^\/$/ && $line_count > 12) { next; }
	    print($OUTPUT $line); 
	}
	else {
	    # Get the time from the data line/
	    $line =~ /^\s*(\-?\d+\.\d)/;
	    my $time = $1;
	    next if ($time < 0);

	    # Keep the whole record if it matches the frequency.
	    if ($time % $WIND_CLEAR_FREQ == 0) {
		printf($OUTPUT $line);
	    } else {
		# Blank out the wind values.
		printf($OUTPUT "%s",substr($line,0,31));
		print($OUTPUT " 9999.0 9999.0 999.0 999.0");
		printf($OUTPUT "%s",substr($line,57));
	    }
	}

	$line_count++;
    }

    close($OUTPUT);
}

##------------------------------------------------------------------------
# @signature int createDirectory(String path)
# <p>Create the directory structure specified in the path.</p>
#
# @input $path The path to be created.
# @output $success A boolean value if the directory was able to be created.
##------------------------------------------------------------------------
sub create_directory {
    my $path = shift;
    my @dirs = split(/\//,$path);
    my $count = 1;
    my $accum_dir = $dirs[0];
    while ($count < scalar(@dirs)) {
        $accum_dir = sprintf("%s/%s",$accum_dir,$dirs[$count]);
        if (!(-e $accum_dir)) {
            mkdir($accum_dir) || return 0;
            chmod(0775,$accum_dir);
        }
        $count++;
    }
    return 1;
}


##------------------------------------------------------------------------
# @signature void create_plots()
# <p>Create the Skewt plots from the class files that are located in the
# working directory.</p>
##------------------------------------------------------------------------
sub create_plots {

    # Get the list of CLASS files to convert
    opendir(my $READ,$CLASS_DIR);
    my @files = grep(/\.cls$/,readdir($READ));
    closedir($READ);

    foreach my $file (@files) {
	printf("File: %s\n",$file);

	$file =~ /^(.+)_(\d{4})(\d{2})(\d{2})(\d{2})(\d{2})/;
	my ($id,$year,$month,$day,$hour,$min) = ($1,$2,$3,$4,$5,$6);

	# Define directories and file names that will be used.
	my $OUT_DIR = sprintf("%s/%s",$PLOT_DIR,$id);
	create_directory($OUT_DIR) or die("Can't create $OUT_DIR\n");
	my $new_file = sprintf("%s/%s",$OUT_DIR,$file);
	my $script_file = $new_file.".script";
	$file = sprintf("%s/%s",$CLASS_DIR,$file);
	my $temp_class = sprintf("%s.tmp",$new_file);

	# Set up the CLASS file that will be manipulated and prepare it for plotting.
	copy($file,$temp_class) or die("Can't copy $file to $temp_class\n");
	remove_descent($temp_class) if ($REMOVE_DESCENT);
	clear_wind_params($temp_class) if ($WIND_CLEAR_FREQ);

	# Create the post script file name
	$new_file =~ s/\.cls/\.Skewt\.ps/;

	# Generate the script that will create the post script file.
	open(my $FILE,">$script_file") or die("Cannot open skewt temp file.\n");
	printf($FILE "#! /bin/csh\n");
	printf($FILE "/usr/local/bin/suds << EOF\n");
	printf($FILE "file %s class NCAR/EOL\n", $temp_class);
	printf($FILE "color data2 rgb 0 0 1\n");
	printf($FILE "output ./%s psc\n", $new_file);
	printf($FILE "xyplot wdir alt\n");
	printf($FILE "quit\nEOF\n");
	close($FILE);

	# Create the post script file and remove the temporary script
	chmod(0774,$script_file);
	system($script_file);
	unlink($script_file) if (!$KEEP_TMP_FILES);
	
	# Remove the class file that had the wind values changed.
	unlink($temp_class) if (!$KEEP_TMP_FILES);

	# Get the Latitude and Longitude from the Class file for the image.
	open(my $CLASS,$file);
	<$CLASS>;<$CLASS>;<$CLASS>;
	my @data = split(/,/,<$CLASS>);
	close($CLASS);
	

	# Define the name of the GIF file.
	my $gif_file = sprintf("%s/%s_%04d%02d%02d%02d%02d.xywdir.gif",
			       $OUT_DIR,$id,$year,$month,$day,$hour,$min);

	# Create the GIF file from the post script file.
	system(sprintf("/usr/local/bin/convert -rotate 90 -crop 0x0 -pen black -draw \"text 45,30 '%s %s'\" %s %s",$data[4],$data[5],$new_file,$gif_file));

	# Remove the post script file.
	unlink($new_file) if (!$KEEP_TMP_FILES);
    }
}

sub print_usage {

    printf("Usage:\n\n  plot_skewt.pl -d <CLASS_DIR> -o <out_dir> -w <freq> -k -nodesc <count> -press <pressure range> -temp <temperature range>\n");
    printf("REQUIRED:\n\n");
    printf("  -d <CLASS_DIR> the location of the CLASS files to be plotted.\n");
    printf("\n");
    printf("OPTIONS:\n\n");
    printf("-help displays this help message.\n\n");
    printf("NOTE: the followign options can be given in any order!\n\n");
    printf("  -o      The directory where the plots (and kepts files) are to be stored.\n");
    printf("  -w      The frequency of the wind values to be kept (in seconds).  \n");
    printf("          i.e 60 will keep winds every minute.\n");
    printf("  -nodesc Flag for telling the script to remove the descent data in the file.\n");
    printf("  -k      Flag for keeping the intermediate files used for creating the plot.\n");
    printf("  -press  Lower and upper pressure limits for the plot.\n");
    printf("  -temp   Lower and upper temperature limits for the plot.\n");
    printf("\n");
    printf("EXAMPLES:\n");
    printf("  plot_skewt.pl -d ../final -o ../skewt -w 60 -nodesc 3 -press 1050 500 -temp 0 40\n");
}

##------------------------------------------------------------------------
# @signature void read_args(String[] params)
# <p>Read and parse the command line arguments passed to the script.</p>
#
# @input params The list of parameters sent to the script.
##------------------------------------------------------------------------
sub read_args {
    my @params = @_;

    if ($params[0] eq "-help") {
	print_usage();
	exit(0);
    }

    while (@params) {
	my $param = shift(@params);

	if ($param eq "-o") {
	    $PLOT_DIR = shift(@params);
	} elsif ($param eq "-d") {
	    $CLASS_DIR = shift(@params);
	} elsif ($param eq "-w") {
	    $WIND_CLEAR_FREQ = shift(@params);
	} elsif ($param eq "-k") {
	    $KEEP_TMP_FILES = 1;
	} elsif ($param eq "-press") {
	    @PRESS_LIMITS = (shift(@params),shift(@params));
	} elsif ($param eq "-temp") {
	    @TEMP_LIMITS = (shift(@params),shift(@params));
	} elsif ($param eq "-nodesc") {
	    $REMOVE_DESCENT = 1;
	    $REMOVE_DESCENT_COUNT = shift(@params);
	} else {
	    print_usuage();
	    die("Unknown paramater: $param\n");
	}
    }


    print_usage() if ($CLASS_DIR eq "");
}

##------------------------------------------------------------------------
# @signature voie remove_descent(String file)
# <p>Remove the data in the file that is descending.</p>
#
# @input $file The file to have the descent data removed.
##------------------------------------------------------------------------
sub remove_descent {
    my ($file) = @_;

    open(my $FILE,$file) or die("Can't open $file\n");
    my @lines = <$FILE>;
    close($FILE);

    open($FILE,">$file") or die("Can't write to $file\n");
    for (my $i = 0; $i < 15; $i++) {
	print($FILE shift(@lines));
    }
    
    my $count = 0;
    while (@lines && $count < $REMOVE_DESCENT_COUNT) {
	my $line = shift(@lines);
	print($FILE $line);

	if ((split(' ',$line))[9] < 0) {
	    $count++;
	} else {
	    $count = 0;
	}
    }

    close($FILE);
}
