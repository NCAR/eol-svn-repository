#!/usr/bin/perl
#
# Code to touch/msretention MSS files received in purge notices.
# Created by Janine Goldstein 4/11/2008
#
# Updated JAG 7/9/2008
#       Implemented cos-blocked identification so that if we get a purge notice
#       for a cos-blocked file, the code will try to update *ALL* files in the
#       taplog as cos-blocked files.

use lib "/net/work/lib/perl/MassStore";
use MSSfile;
use TLfile;

# Usage: $0 purgelist.<date>
if (!defined @ARGV)
	{
	usage();
	}

print "\n*******************************************************************\n";
print "Running $0\n";
print "This script will touch ALL files for each project, not just the\n";
print "files listed in the SCD purge email.\n";
print "*******************************************************************\n\n";

# Initialize some global vars
$prod = 0;

while ($ARGV = shift)
    {
    # Check for "production mode"
    if ($ARGV =~ /-r/) 
        {
        $prod = 1;
	if ($prod) {print "Production run!\n";}
        next;
        }

    # Get the listing of files to touch from the purgelist created by the user
    # from the SCD email.
    open (FILELIST, "$ARGV") or die "Can't open $ARGV:$!\n";

    # Loop through all the files to touch and figure out which taplog files
    # contains the entry for that file. Save a list of all taplog files found
    # to an array so we can deal with dups, i.e. more than one file to touch
    # may be found in the same taplog, and this script will generate a "taplogs 
    # array" entry for each.
    @taplogs=();
    while ($listing = <FILELIST>)
        {
	# remove the end of line char
	chomp $listing;

	# Split the filename from the path 
	my $mss_file = new MSSfile();
	$mss_file->path($listing);
	$mss_file->parse_filename_from_path();
	if ($mss_file->filename() eq "none") {next};

	# Search for the MSS filename (w/o path) in all the taplog.# files 
	# in /scr/raf2/Prod_Data/archives/taplog and determine which taplog
	# file, if any, contains this data file.
	$mss_file->find_taplog();
	
	# If you don't find a taplog file containing this MSS file, then
	# just update this single file.
	if ($mss_file->taplog eq "none") 
	    {
	    $mss_file->touch($prod);
	    next
	    };

	# If the entry is found, make sure it is not a duplicate and then add
	# it to the array.
	$found = 0;
	foreach $line (@taplogs)
	    {
	    if ($line eq $mss_file->taplog) {$found = 1;}
	    }
	if ($found == 0) 
	    {
	    push @taplogs,$mss_file->taplog();
	    }
	}

    print "\n***************************************************************\n";
    print "Loop through taplog files and touch all files in each\n";
    print "***************************************************************\n";

    # Loop through taplogs array and process all files found (not just those
    # in FILELIST.
    if (@taplogs == ()) {print "No taplogs found.\n\n";}
    foreach $taplog (@taplogs) 
	{
	print "\n# Processing taplog file ".$taplog."\n";
	print "Began at ".localtime()."\n";

	# Now read all the other MSS files from the taplog file so we can touch
	# every file for that project at once.
	open (TAPLOG,$taplog) or die "Can't open $taplog:$!\n";
	my $found_path = 0;
	while ($line = <TAPLOG>)
	    {
	    # If the last line was the path, check if it extends onto this line.
	    if ($line =~ /RAF/) {
		print "WARNING: Found a second line of path: $line\n";
		print "This eventuallity was not implemented in this code!!!!!\n";
		print "If code runs as-is, files in this second path will not be touched.\n";
		print "You must touch them manually!!!!\n";
	    }

	    # Find the project from the header
	    if ($line =~ /Project/) 
		{
		$project = $line; 
		# Remove spaces from beginning of line.
		$project =~ s/^ *//; 
		# Split number and name into two lines for printing.
		$project =~ s/ *Project name/\nProject name/;
		print $project."\n";
		}

	    # There are multiple MSS path conventions possible. Determine which
	    # one we are working on by reading the "MSS path name:" from the
	    # header.
	    if ($line =~ / *MSS path name: *(\/.*) */) 
		{
		$path_template = $1;
		# Some files have two templates (the second for cos-blocked
		# files is the same as the first with a C after it). 
		# If the second template exists, and it is a cos-blocked template
		# set a flag to process it separately.
		if($path_template =~ / .*C$/) 
		    { 
		    print "Files will be found under paths of form:". +
			$path_template."\n";
		    $path_template =~ s/ .*$//; 
		    $cosblocked = 1;
		    }
		elsif ($path_template =~ / /)
		    {
		    print "ERROR: Unknown second template in path $path_template.";
		    print " Second template not processed.\n";
		    $path_template =~ s/ .*$//; 
		    print "Files will be found under path of form:". +
			$path_template."\n";
		    }
		else
		    { 
		    print "Files will be found under path of form:". +
			$path_template."\n";
		    }
		# Sometimes the next line is also a path:
		$found_path = 1;
		}

	    # Check for the TL # in the header in case it is there and not in 
	    # the flight line, but don't match the template TLxxxx
	    if ($line =~ /\/RAF\/(TL.*)\// && $line !~ /TLxxxx/)
		{
		$tl_num = $1;
		print "Found the TL dir in the header: ".$tl_num."\n";
		}

	    # Grab the column headings from the file.  Assume they start
	    # with " fltno".
	    if ($line =~ / fltno/)
	        {
		$header = $line;
	        }


	    # If line matches zero or one spaces followed by RF,TF, or FF, 
	    # process..
	    if ($line =~ /^ ?[RFT]F/) 
		{
		# We found a file to touch, so save it as a file object
	        my $testfile = new TLfile();
	        $testfile->taplog($taplog);
		$testfile->pathTemplate($path_template);
		$testfile->TLnumber($tl_num);
		$testfile->header($header);
		$testfile->taplogRecord($line);

		# Get the MSS path to the file.
		$testfile->get_path_to_file();
		if ($testfile->path eq "none") {next};

		# mstouch and msretention the file.
		# This can be run in production mode
		# $prod="1" or in echo mode $prod=="0"
		$testfile->touch($prod);

		# If there was a cos-blocked template, try to touch the
		# file as a 'C' file as well.
		if ($cosblocked == 1)
		    {
		    $testfile->path($testfile->path.'C');
		    $testfile->touch($prod);
		    }
		}
	    }
	close(TAPLOG);
	}
    close(FILELIST);
    }
print "#  Completed at ".localtime()."\n";

################################################################################
# Subroutines
################################################################################
sub usage 
{
	print "Usage: $0 [-r] purgelist.<date>\n";
	print "purgelist should contain a list of files (complete path)\n";
	print "received from 'SCD email' that will be purged, i.e.\n";
	print "	/RAF/TL0635/489A01\n";
	print "Note: A # at the beginning of a line will stop that line from";
	print " being processed\n";
	print "-r when you are ready to actually submit command, else you are";
	print " in echo mode\n";
	exit(1);
}
