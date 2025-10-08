#!/usr/bin/perl

################################################################################
# POST CABIN, CASF, CIP, and PCASP have erroneous dates for flights that started
# just before midnight, but didn't start collecting data until after midnight. 
# Dates are wrong in the filename, header, and the Date/Time column of the data. 
# Luckily, the erroneous date always occurs as YYMMDD, so this code accepts the 
# filename and old and new YYMMDD and creates a corrected output file. It does 
# NOT overwrite the original file.
################################################################################
$debug = 0;

&main();

############
# main loop
############

sub main {

    # Usage
    if (@ARGV != 4) {
	print "Usage: fixPOSTdates.pl <file> <olddate> <newdate> <flight>".
	      "where dates are like YYMMDD and flight like RF##\n";
	exit(1);
    }

    # Parse command line arguments
    $srcfile = shift(@ARGV);
    $old_date = shift(@ARGV);
    $new_date = shift(@ARGV);
    $flight = shift(@ARGV);

    if ($debug) {
	print "Command line params: date $old_date to $new_date for file $srcfile\n";
    }
    
    # Format check command line args
    # Check that 2nd and 3rd arg are dates
    foreach $arg ($old_date, $new_date) {
	if ($debug) {print "Checking $arg\n";}
        if ($arg !~ /[0-9]{6}/) {
	    print "Usage: dates must be of the format YYMMDD\n";
	    exit(1);
	}
    }
    # Check flight like RF###
    if ($flight !~ /RF[0-9]{2}/) {
	print "Usage: flight must be of the format RF##, i.e. RF01\n";
	exit(1);
    }
    # Check that filename contains old_date.
    if ($srcfile !~ /$old_date/) {
	print "Filename must contain old date\n";
	exit(1);
    }

    # Get time from header of input file
    open (SRCFILE,$srcfile) or die "Can't open $srcfile:$!\n";
    while (<SRCFILE>) {
	if ($_ =~ /DATA COVERAGE/) {
	    chop;
	    s/[A-Z =:;\t\r]*//g; 
	    ($junk,$begintime,$endtime) = split($old_date,$_);
	    if ($old_date == $new_date) {
		print "No date conversion, just rename file\n";
	    } else {
	        print "Convert $old_date $begintime-$old_date $endtime\n".
		      "     to $new_date $begintime-$new_date $endtime\n";
	    }
	    last;
	}
    }
    close (SRCFILE);


    # Build output filename for CABIN files
    $outfile = $srcfile;
    $outfile =~ s/CABIN/CABIN_II/;
    $outfile =~ s/hz/hz_$flight/;
    $outfile =~ s/$old_date/${new_date}_$begintime/;

    print "Converting date $old_date to $new_date for file $srcfile\n";
    print "Creating file $outfile\n";

    open (OUTFILE, ">$outfile") or die "Can't open $outfile:$!\n";
    open (SRCFILE,$srcfile) or die "Can't open $srcfile:$!\n";
    while (<SRCFILE>) {
	s/:$old_date/:$new_date/g;
	s/^$old_date/$new_date/;

	# KLUDGE: For the CABIN data, the units for WDC are incorrect. 
	# They are M/s and should be Deg.
	if (m/^UTC/ && ($srcfile =~ /1hz/)) {
	    @units = split("\t",$_);
	    if ($units[15] == "M/s") {$units[15] = "Deg";}
	    $_ = join("\t",@units);
	}
	if (m/^UTC/ && ($srcfile =~ /10hz/)) {
	    @units = split("\t",$_);
	    if ($units[19] == "M/s") {$units[19] = "Deg";}
	    $_ = join("\t",@units);
	}

	print OUTFILE;
    }
    close (SRCFILE);
    close (OUTFILE);

    system("ls -l $srcfile $outfile");

}
