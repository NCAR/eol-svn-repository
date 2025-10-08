#!/usr/bin/perl -w

##Module-------------------------------------------------------------------------- 
#
# @author LindaEcho-Hawk 11 Nov 2014
# @version Modified this script for the DEEPWAVE DLR Lauder radiosonde data
#          to remove descending sonde data, determined by having a negative
#          or missing ascent rate at the end of the file.
#
# @author Linda Echo-Hawk
# @version This script was created for MPEX NSSL Mobile sounding processing.
#          The purpose of the script is to remove the descending sounding data
#          at the end of the files.  The NSSL raw data had a column with 
#          comments and if the comment contained "descent" (at the end of
#          the file) it was removed from the array. The raw data was read
#          into an array which was reversed.  Then the line was removed if
#          it contained the comment "descent".  Once good data was found,
#          the loop ends and the remaining records are written out to a
#          new raw data file. The print statements in this program are 
#          redirected to a log file.
#       
#
# Usage:    RemoveDescending.pl <orig raw data> <new raw data>
#
# Example:  RemoveDescending.pl  NSSL_20130614_1620.txt NSSL_20130614_1620.txt.new >> log.txt
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
# if negative or missing data is
# found it will be removed from
# the array; otherwise you will
# have reached good data and 
# will break out of the loop
foreach my $last_record (reverse(@lines))
{
	my (@text) = (split(' ',$last_record));

	if (($text[9] < 0.0) || ($text[9] =~ /999.0/))
	{
		undef($last_record);
		$counter++;
	}
	else
	{
		last;
	}

}

print "$fileName removed descending lines:  $counter\n";

# now create the new version of the raw data file
foreach my $new_line (@lines)
{
	if ($new_line)
	{
		print OUT "$new_line";
	}
}


close IN;



