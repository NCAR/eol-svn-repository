#! /usr/bin/perl -w

##Module-----------------------------------------------------------------
# The Extracter module is used to extract a lower frequency of CLASS
# formatted sounding data into a lower resolution.
#
# @author Joel Clawson
# @version 1.0 Original Creation
##Module-----------------------------------------------------------------
package Extracter;
use strict;
use lib "/work/DPG_HTML/BEST_SW/conversion_modules/Version3";
use Sounding::ClassRecord;

##-----------------------------------------------------------------------
# @signature void extract(String infile, String outfile, int freq)
# <p>Extract the data from the infile at the specified frequency and
# place it into the outfile.</p>
#
# @input $infile The name of the input file.
# @input $outfile The name of the output file.
# @input $freq The frequency of the data in seconds.
##-----------------------------------------------------------------------
sub extract {
    my ($self,$infile,$outfile,$freq) = @_;
    
    open(my $IN,sprintf("<%s",$infile)) or die("Cannot read $infile\n");
    open(my $OUT,sprintf(">%s",$outfile)) or die("Cannot open $outfile\n");
    open(my $LOG,sprintf(">>%s",$self->{"logfile"})) or die("Cannot open logfile\n");

    $self->print_header($IN,$OUT);

    my $expected_time = 0;
    foreach my $line (<$IN>) {
	my $time = (split(' ',$line))[0];

	if ($time < 0) { print($OUT $line); next; }

	if ($time == $expected_time) { 
	    print($OUT $line); 
	    $expected_time += $freq;
	} else {
	    # Handle missing data.
	    while ($expected_time < $time) {
		my $new_record = Sounding::ClassRecord->new($LOG,$infile);
		printf($LOG "%s: No record at time %s.  Creating all missing record.\n",
		       $infile,$expected_time);
		$new_record->setTime($expected_time);
		print($OUT $new_record->toString());
		$expected_time += $freq;
	    }

	    # Handle the case when the missing moves the expected to the read time.
	    if ($time == $expected_time) {
		print($OUT $line);
		$expected_time += $freq;
	    }
	}
    }

    close($LOG);
    close($OUT);
    close($IN);
}

##-----------------------------------------------------------------------
# @signature Extracter new(String logfile)
# <p>Create a new instance of the Extracter class.</p>
#
# @input $logfile The file that will store the log of the extraction.
##-----------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = $invocant || ref($invocant);
    bless($self,$class);

    ($self->{"logfile"}) = @_;
    open(my $LOG,sprintf(">%s",$self->{"logfile"})) or
	die(sprintf("Cannot open the log file: %s\n",$self->{"logfile"}));
    close($LOG);

    return $self;
}

##-----------------------------------------------------------------------
# @signature void print_header(FileHandle IN, FileHandle OUT);
# <p>Print the header from the input file handle to the output file
# handle.</p>
#
# @input $IN The file handle to be read.
# @input $OUT The file handle to be written to.
##-----------------------------------------------------------------------
sub print_header {
    my ($self,$IN,$OUT) = @_;

    for (my $i = 0; $i < 15; $i++) {
        my $line = <$IN>;
        print($OUT $line);
    }
}

1;
