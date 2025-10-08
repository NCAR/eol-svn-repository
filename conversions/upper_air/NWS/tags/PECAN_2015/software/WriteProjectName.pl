#! /usr/bin/perl -w
#
##Module--------------------------------------------------------------------------
# WriteProjectName.pl
#
# <p>Use this script to correct project name line in soundings in ESC format.
#
# Project ID:                        DYNAMO
# SHOULD BE:
# Project ID:                        DC3
#
# @author L. Cully
# @version Updated by L. Cully to correct NWS ESC soundings for DC3.
#          Based on original code by L. Echo-Hawk to correct lat/lons
#          for DYNAMO ARM Nauru soundings.
#
# Usage:    WriteProjectName.pl <Input_ESC_formatted_file> 
#
# Input:    Name of input sounding. Sounding is assumed to be in ESC format. 
# Output:   Same name as input file with ".corr" sufix added. 
#
# Example:  WriteProjectName.pl KOUN_20120613111112.cls
#      Note that output file name will be KOUN_20120613111112.cls.corr
#
# Generally accompanied by WriteProjectName.sc script which processes
# all *.cls files in current working directory. The text from that
# script follows. Note that the script diffs the original versus
# the input file. Run "WriteProjectName.sc >& WriteProjectName.log"
#
# WriteProjectName.sc::
#
#   for f in *.cls
#   do
#     echo "--------------------------------------------------------"
#     echo "WriteProjectName.pl $f"
#     WriteProjectName.pl $f
#     diff $f $f.corr
#   done
#   echo "All ESC Class file Wrong Project Name corrected! Run WriteProjectName.pl on all ESC data files."
#
##Module--------------------------------------------------------------------------
use strict;
use warnings;                              

printf "\nWriteProjectName.pl began on ";print scalar localtime;printf "\n";

open (IN, "$ARGV[0]") || die "Can't open for reading: $ARGV[0]\n";
open (OUT, ">$ARGV[0].corr") || die "Can't open file for writing: $ARGV[0].corr\n";    

print "Processing File $ARGV[0]. Output file is $ARGV[0].corr\n";

while (<IN>)
{
	my $line = "$_";
	chomp ($line);
	my $corrected_line = sprintf("Project ID:                        DC3");

	if (/Project ID:/)
	{
		print OUT "$corrected_line\n";
	}
	else 
	{
		print OUT "$line\n";
	}
}
close IN;

printf "\nWriteProjectName.pl end on ";print scalar localtime;printf "\n";
