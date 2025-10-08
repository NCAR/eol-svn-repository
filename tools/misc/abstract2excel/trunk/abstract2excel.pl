#! /usr/bin/perl -w

##Module-----------------------------------------------------------------
# <p>The <code>abstract2excel.pl</code> script is to convert the text
# version of the JOSS online abstracts into a file readable for Microsoft
# Excel which can then be merged into a Word template.</p>
# <p>This reads the text abstracts in a directory and outputs them into
# a single file that is tab separated into columns.</p>
#
# @use abstract2excel.pl <file_directory> <output_file>
#
# @author Joel Clawson
##Module-----------------------------------------------------------------
use strict;

my @key_order = ();
my %all_data = ();

&main();

##------------------------------------------------------------------------
# @signature void main(String raw_dir, String outfile)
# <p>Read in the files in the raw directory and put them into a single
# csv format output file for Microsoft Excel to be able to read in.</p>
#
# @input $raw_dir The raw directory containing the abstracts.
# @input $outfile The full path file name of the file to store the data.
# @warning This function will die if it gets a bad directory or if it
# does not get the correct number of parameters.
##------------------------------------------------------------------------
sub main {
    if (scalar(@ARGV) != 2) { 
	printf("Usage: abstract2excel.pl <file_directory> <output_file>\n");
	exit(1);
    }
    my ($raw_dir,$outfile) = @ARGV;

    # Read the files in the data directory
    opendir(RAW_DIR,$raw_dir) or die("Cannot open: $raw_dir\n");
    my @raw_files = readdir(RAW_DIR);
    closedir(RAW_DIR);

    # Cycle through the raw files.
    my $define_keys = 1;
    foreach my $file (@raw_files) {
	loadDataFromFile(sprintf("%s/%s",$raw_dir,$file),$define_keys)
	    if ($file !~ /^\.+$/);
	$define_keys = 0 if ($file !~ /^\.+$/);
    }

    printToFile($outfile);
}

##------------------------------------------------------------------------
# @signature void loadDataFromFile(String file, int define_keys)
# <p>Read in the data in the specified file into the hash that contains
# all of the data to be put into the output file.</p>
#
# @param $file The name of the file to be read.
# @param $define_keys A flag to see if the column parameters need to be
# defined.
##------------------------------------------------------------------------
sub loadDataFromFile {
    my $file = shift;
    my $define_keys = shift;
    my $temp = "a2e.tmp";

    # Convert the file to unix text.
    system("dos2unix -n $file $temp");
    
    open(INPUT, $temp) or die("Cannot open $temp\n");

    my $key = "";
    while(<INPUT>) {
	chomp($_); # Remove the new line character.
	my @data = split(',', $_);

	my $value;
	if (scalar(@data) == 2) {
	    $key = trim($data[0]);
	    $value = trim($data[1]);
	    push(@key_order,$key) if ($define_keys);
	} elsif (scalar(@data) == 1) {
	    $value = trim($data[0]);
	}

	# Set up the data hash for the lines value
	if (!defined($all_data{$file}{$key})) { $all_data{$file}{$key} = ""; }
	else { $all_data{$file}{$key} .= " "; }

	# Add the value to the hash.
	$all_data{$file}{$key} .= $value if (scalar(@data) > 0);
    }

    close(INPUT);

    # Remove the unix file.
    unlink($temp);
}

##------------------------------------------------------------------------
# @signature void printToFile(String file)
# <p>Print the data in the hash that has been read in from the raw directory
# into the output file.</p>
#
# @input $file The name of the output file.
##------------------------------------------------------------------------
sub printToFile {
    my $file = shift;

    open(OUTPUT, ">$file") or die("Cannot open $file\n");

    foreach my $key (@key_order) {
	printf(OUTPUT "\"%s\",",$key);
    }
    printf(OUTPUT "\n");

    foreach my $raw (keys(%all_data)) {
	foreach my $key (@key_order) {
	    if (defined($all_data{$raw}{$key})) {
		printf(OUTPUT "\"%s\",", $all_data{$raw}{$key});
	    } else {
		printf(OUTPUT "\"\",");
	    }
	}
	printf(OUTPUT "\n");
    }
    
    close(OUTPUT);
}

##------------------------------------------------------------------------
# @signature String trim(String line)
# <p>Remove the leading and trailing whitespace of a String.</p>
#
# @input $line The String to be trimmed.
# @output $line The trimmed String.
##------------------------------------------------------------------------
sub trim {
    my $line = shift;
    $line =~ s/^\s+//g;
    $line =~ s/\s+$//g;
    return $line;
}
