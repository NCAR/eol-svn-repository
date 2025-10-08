#!/usr/bin/perl -w

##Module-------------------------------------------------------------------------- 
# @author Linda Echo-Hawk
# @version Created to find wind values with incorrect QC flags. This may be 
# due to code which sets all wind values to missing if direction or speed 
# do not fit the required format.
#
# Usage:    FindVals_IncorrectFlags.pl <*.cls.qc> <*.cls.qc.checked>
#           Run in a foreach loop on all *.qc files in the /final directory
#
##Module--------------------------------------------------------------------------
use strict;
use warnings; 

open (IN, "$ARGV[0]") || die "Can't open for reading\n";
open (OUT, ">$ARGV[1]") || die "Can't open file for writing\n";

my $fileName = $ARGV[0];
# print OUT "Checking File $fileName\n";        

my $index = 0;
while (<IN>)
{
	if ($index >= 15)
	{
		my $line = "$_";
    	my @data = split(' ',$line);
		my $press = $data[1];
		my $temp = $data[2];
		my $RH = $data[4];
		my $Ucmp = $data[5];
		my $Vcmp = $data[6];
		my $wind_dir = $data[8];
		my $wind_spd = $data[7];
		my $ascent = $data[9];
		my $Qp = $data[15];
		my $Qt = $data[16];
		my $Qrh = $data[17];
		my $Qu = $data[18];
		my $Qv = $data[19];
		my $Qdz = $data[20];

		if (($press == 9999.0) && ($Qp != 9.0))
		{
			print OUT "$fileName Line $index Qp flag wrong for pressure\n";
		}

		if (($temp == 9999.0) && ($Qt != 9.0))
		{
			print OUT "$fileName Line $index Qt flag wrong for temp\n";
		}

		if (($RH == 999.0) && ($Qrh != 9.0))
		{
			print OUT "$fileName Line $index Qrh flag wrong for RH\n";
		}
		
		if (($Ucmp == 9999.0) && ($Qu != 9.0) ||
			($Vcmp == 9999.0) && ($Qv != 9.0))
		{
			print OUT "$fileName Line $index Ucmp or Vcmp flag wrong\n";
		}
		if (($wind_dir == 999.0) && ($Qu != 9.0))
		{
			print OUT "$fileName Line $index Ucmp flag wrong for wind dir\n";
		}
		if (($wind_dir == 999.0) && ($Qv != 9.0))
		{
			print OUT "$fileName Line $index Vcmp flag wrong for wind dir\n";
		}
		if (($wind_spd == 999.0) && ($Qu != 9.0))
		{
			print OUT "$fileName Line $index Ucmp flag wrong for wind spd\n";
		}
		if (($wind_spd == 999.0) && ($Qv != 9.0))
		{
			print OUT "$fileName Line $index Vcmp flag wrong for wind spd\n";
		}
		if (($ascent == 999.0) && ($Qdz != 9.0))
		{
			print OUT "$fileName Line $index Qdz flag wrong for ascent rate\n";
		}
	}

	$index++;
}
print OUT "\n";

close IN;

