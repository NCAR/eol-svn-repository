#!/usr/bin/perl -w

##Module-------------------------------------------------------------------------- 
# @author Linda Echo-Hawk
# @version VOCALS_2008 BAE-146 netCDF data conversion
#          Create a file of header comments to be added to each sounding 
#          by reading in the *descrip.txt file in each of the raw_data
#          subdirectories and extracting the comment for each sonde.
#
# @date 22 March 2010
#
# Usage:    FindHeaderComment.pl <text description file>
#
# Example:  In the raw_data subdirectory, assuming there are flight
#           directories with a *_descrip.txt file in each one, 
#           first create (touch HeaderInfo.txt) an output file,
#           the do the following: 
#
#           foreach i ( */*.txt )
#           foreach? FindHeaderComment.pl $i >> HeaderInfo.txt
#           foreach? end    
#
#           Place the HeaderInfo.txt file in the ../docs subdirectory
#           when completed, so it can be consumed by the converter script.
#
##Module--------------------------------------------------------------------------
use strict;
use warnings; 

open (IN, "$ARGV[0]") || die "Can't open for reading\n";

my $readNext = 0;

while (<IN>)
{
	my $line = "$_";
	chomp $line;

	if ($readNext)
	{
		$line = trim($line);
		print "$line\n";
		$readNext = 0;
	}
	if (/^Sonde \d/)
	{
		$readNext = 1;
		print "$line "
	}
}

close IN;

##---------------------------------------------------------------------------
# @signature String trim(String line)
# <p>Remove surrounding white space of a String.</p>
# 
# @input $line The String to trim.
# @output $line The trimmed line.
##---------------------------------------------------------------------------
sub trim {
    my $line = shift;
    $line =~ s/^\s+//g;
    $line =~ s/\s+$//g;
    return $line;
}  
 


