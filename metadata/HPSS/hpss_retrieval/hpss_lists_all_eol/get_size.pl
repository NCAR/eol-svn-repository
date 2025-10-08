#!/usr/bin/perl

# Usage
if (@ARGV == 0) {
    print "Enter a date on the command line of form YYMMDD\n";
    print "e.g. get_size.pl 150101\n";
    print "or enter a file containing a list of dates of the form YYMMDD on the command line\n";
    print "e.g. get_size.pl list\n";
} else {
    if (-f $ARGV[0]) {
	open(DATEFILE,$ARGV[0]) or die "Can't open $ARGV[0]:$!";
	while (<DATEFILE>) {
            $date = $_;
	    chomp $date;
	    &calc_size($date);
	}
    } else {
        $date = $ARGV[0];
	&calc_size($date);
    }
}

sub calc_size
{
    my $date = shift;
    $size=0;
    @files = <*.list>;
    foreach $file (@files) {
        if ($file =~  m/\.$date\.list/) {
	    open(FILE,$file) or die "Can't open $file:$!";
            while (<FILE>) {
	        if (/^-/) {
		    @columns = split('\s+',$_);
		    $size +=$columns[4];
	        }
	    }
        }
    }
    print "20".$date."\t",$size."\n";
}
