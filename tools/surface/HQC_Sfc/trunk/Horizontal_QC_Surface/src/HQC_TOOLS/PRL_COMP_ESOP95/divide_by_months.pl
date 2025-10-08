#!/bin/perl -w

# divide_by_months.pl
#
# A script to read in the sorted output
# of the hrly2daily s/w, and divide it
# up by months: apr, may and june.
#
# 30 sept 96, ds

$YEAR = "95";
$infile = $ARGV[0];
$outfile4 = $YEAR."04.dqc";
$outfile5 = $YEAR."05.dqc";
$outfile6= $YEAR."06.dqc";
$outfile7= $YEAR."07.dqc";
$outfile8= $YEAR."08.dqc";
$outfile9= $YEAR."09.dqc";

print "Opening $infile\n";
print "Opening $outfile4\n";
print "Opening $outfile5\n";
print "Opening $outfile6\n";
print "Opening $outfile7\n";
print "Opening $outfile8\n";
print "Opening $outfile9\n";

open (INFILE, "$infile") || die "Can't open $infile for input.\n";
open (OUTFILE4, ">$outfile4") || die "Can't open $outfile4 for output.\n";
open (OUTFILE5, ">$outfile5") || die "Can't open $outfile5 for output.\n";
open (OUTFILE6, ">$outfile6") || die "Can't open $outfile6 for output.\n";
open (OUTFILE7, ">$outfile7") || die "Can't open $outfile7 for output.\n";
open (OUTFILE8, ">$outfile8") || die "Can't open $outfile8 for output.\n";
open (OUTFILE9, ">$outfile9") || die "Can't open $outfile9 for output.\n";
print "Processing $infile...\n";

while (<INFILE>) {
	if (/^1995\/04/) {
		print OUTFILE4;
	}
	elsif (/^1995\/05/) {
		print OUTFILE5;
	}
	elsif (/^1995\/06/) {
		print OUTFILE6;
	}
	elsif (/^1995\/07/) {
		print OUTFILE7;
	}
	elsif (/^1995\/08/) {
		print OUTFILE8;
	}
	elsif (/^1995\/09/) {
		print OUTFILE9;
	}
	else {
		print "No match for line\n";
	}
}
