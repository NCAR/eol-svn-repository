#!/usr/bin/perl

use strict 'vars';
use POSIX; # Perl interface to IEEE Std 1003.1
################################################################################
# Convert columnar ascii UCI data with time in the first column and 0
# lines of header to netCDF format.
################################################################################

&main();

############
# main loop
############

sub main {
    # Usage
    if (@ARGV != 0) {
        print "Usage: UCI2nc.pl\n";
        exit(1);
    }

    # List all the files to be processed into an array using Perl file glob fn.
    my $filepattern = "f_[0-9][0-9][0-9][0-9][0-9][0-9]_ascii.txt";
    my @files = glob($filepattern);
    if (@files == ()) {
        print "There are no $filepattern files in the processing ".
              "dir. Exiting\n";
        exit(1);
    }
    
    #Loop through the files and process each individually
    foreach my $file (@files) {
        print "Processing $file\n"; 

	# Parse the date from the filename
	my $date = "20".substr($file,2,2)."-".substr($file,4,2)."-".substr($file,6,2);

	# Open an intermediate ascii file with a header line appended.
        open (OUTFILE, ">$file.new") or die "Can't open $file.new:$!\n";
	# Write the header line
        print OUTFILE "t ap lat lon hdg wx wy wz ah ta td ".
        " ts ps tas rhoa mr thet tvir thete tirup flip tdl\n";

	# Open the orig ascii file	
        open (FILE, "$file") or die "Can't open $file:$!\n";

	# For each line in the orig file
        while (<FILE>) {
	    # split the line into params
            my @vars = (split ' ',$_); 

	    # Remove the time
	    my $time = shift @vars;

	    # Reformat the time as required by asc2cdf
            my $newtime = sprintf "%5.3f",$time; 

	    # Loop through remaining vars and convert NaN to -32767
	    foreach my $var (@vars) {
		if ($var =~ /NaN/) {$var = -32767;}
	    }
	    # Put the time back
            unshift @vars, $newtime;

	    # Reform the line
            my $line = join(' ',@vars);

	    # Write the line (with reformatted time) to intermediate output ascii file
            print OUTFILE $line."\n";
        }

	# close the output file to purge the buffer
	close OUTFILE;

	# Convert the intermediate ascii file to netCDF
	my $ncfile = $file;
	$ncfile =~ s/_ascii.txt/.nc/;
        print "asc2cdf -r 40 -m -d $date $file.new $ncfile\n";
        system("asc2cdf -r 40 -m -d $date $file.new $ncfile");

	# Remove the temporary intermediate file
	#system("/bin/rm $file.new");
    }
}
