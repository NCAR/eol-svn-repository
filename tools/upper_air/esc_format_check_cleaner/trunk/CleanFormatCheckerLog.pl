#!/usr/bin/perl -w

##Module-------------------------------------------------------------------------- 
# @author Linda Echo-Hawk
# @version Created for T-PARC Driftsonde data conversion to separate important
#          messages out and only give a count of other informational messages.
#
# Usage:    CleanFormatCheckerLog.pl <log file> <clean output>
#
# Example:  CleanFormatCheckerLog.pl check_format_cnvESC.log.s check_clean.log
#
##Module--------------------------------------------------------------------------
use strict;
use warnings; 

open (IN, "$ARGV[0]") || die "Can't open for reading\n";
open (OUT, ">$ARGV[1]") || die "Can't open file for writing\n";

# my $fileName = $ARGV[0];
# print "Checking File $fileName\n";        

print OUT "The number and type of informational message is shown at the bottom of this file.\n";
print OUT "Any other message is shown after the name of the file within which it occurs.\n\n";

my $countA = 0;
my $countB = 0;
my $countC = 0;
my $countD = 0;
my $countE = 0;
my $countF = 0;
my $countG = 0;
my $countH = 0;

while (<IN>)
{
	my $line = "$_";
	if (/File:/)
	{
        print OUT "$line";
	}
	elsif (/The 'Data' line is expected to be 130/)
	{
		$countH++;
	}
	elsif (/The 'Data' line has a negative zero value/)
	{
		$countA++;
	}
	elsif (/The 'Data' line has a conflicting/)
	{
		$countB++;
	}
	elsif (/The header altitude/)
	{
		$countC++;
	}
	elsif (/The header latitude/)
	{      
		$countD++;
	}
	elsif (/The header longitude/)
	{
		$countE++;
	}
	elsif (/The two latitude values do not match on line 4./)
	{
		$countF++;
	}
 	elsif (/The two longitude values do not match on line 4./)
	{
		$countG++;
	}     
	else
	{
		print OUT "$line";
	}
}
print OUT "\n";
print OUT "$countA occurrences:  The 'Data' line has a negative zero value...\n";
print OUT "$countB occurrences:  The 'Data' line has a conflicting 'Dewpt' 999.0 ...\n";
print OUT "$countC occurrences:  The header altitude ... is not the same as the surface altitude\n";
print OUT "$countD occurrences:  The header latitude ... is not the same as the surface latitude\n";
print OUT "$countE occurrences:  The header longitude ... is not the same as the surface longitude\n";
print OUT "$countF occurrences:  The two latitude values do not match on line 4.\n";
print OUT "$countG occurrences:  The two longitude values do not match on line 4.\n";
print OUT "$countH occurrences:  The 'Data' line is expected to be 130 characters\n";

close IN;

