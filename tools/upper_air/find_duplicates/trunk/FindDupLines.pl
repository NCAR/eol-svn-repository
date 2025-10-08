#! /usr/bin/perl -w
#
##Module--------------------------------------------------------------------------
# <p>Use this script to find ISF EOL formatted sounding data files with duplicate
# lines (two lines with the identical "Time" values).  
# 
# According to Scot Loehrer, "this is an artifact of the ASPEN software that
# they use to QC the sounding data"(8/20/2009).  It is expected that only 
# one of the two lines will have valid data and the other will have -999.0
# missing values.  If that is ever NOT the case, we need to make sure
# we handle it correctly.</p>
#
# @author Linda Echo-Hawk
# @version Created for T-PARC NRL P-3 2008 dropsonde data conversion
#
# Usage:    FindDupLines.pl <ISF EOL formatted raw data file>
#
# Example:  FindDupLines.pl D20080924_221515.PQC.eol.cls
#
##Module--------------------------------------------------------------------------

	
open (IN, "$ARGV[0]") || die "Can't open for reading\n";

my $fileName = $ARGV[0];
print "Checking File $fileName\n";

my $lastTime = " ";
while (<IN>)
{
    # we want to know the value for the first field (Time)
	my ($time) = ((split(" ", $_,))[0]);

	if ($time eq $lastTime) 	
	{
		if ($time ne "Launch")
		{
			print "Duplicate time '$time' found in $fileName\n";
		}
	}
	else
	{	  
		$lastTime = $time;
	}

}
close IN;


