#!/usr/bin/perl -w

##Module-------------------------------------------------------------------------- 
# @author Linda Echo-Hawk
# @version This script was created for MPEX NSSL Mobile sounding processing.
#          The purpose of the script is to remove the descending sounding data 
#          at the end of the files.  The raw data file was read into an array 
#          which was reversed. The NSSL raw data had a column for comments. 
#          Starting at the end of the file, each line was checked and if it 
#          contained the comment "descent" it was removed from the array. 
#          Once good data was found, the loop ends and the remaining records 
#          are written out to a new raw data file. The print statements in 
#          this program are redirected to a log file.
#       
#
# Usage:    RemoveDescending.pl <orig raw data> <new raw data>
#
# Example:  ReadInWriteOut.pl  NSSL_20130614_1620.txt NSSL_20130614_1620.txt.new >> log.txt
#
##Module--------------------------------------------------------------------------
use strict;
use warnings; 

open (IN, "$ARGV[0]") || die "Can't open for reading\n";
open (OUT, ">$ARGV[1]") || die "Can't open file for writing\n";

my $fileName = $ARGV[0];
# print "Checking File $fileName\n";        

# store the file in an array
my @lines = <IN>;

my $counter = 0;

# check each line of the file
# starting with the last line
# if "descent" is not found, you
# have reached good data and 
# will break out of the loop
foreach my $last_record (reverse(@lines))
{
	if ($last_record =~ /descent/)
	{
		undef($last_record);
		$counter++;
	}
	else
	{
		last;
	}

}

print "$fileName removed descent lines:  $counter\n";

# now create the new version of the raw data file
foreach my $new_line (@lines)
{
	if ($new_line)
	{
		print OUT "$new_line";
	}
}


close IN;



