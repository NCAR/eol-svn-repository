#! /usr/bin/perl
use Cwd;
use strict;

my $var = shift @ARGV;
#my $search = shift @ARGV;
my $skipchk = shift @ARGV;
my @files = (<*.nc>,<*.cdf>);
if ($#files == -1) { die "No NetCDF files found"; }

my $mina = 10000;
my $minfile;
my $maxa = -10000;
my $maxfile;
my $maxnega = -10000;
my $minposa = 10000;

for my $file (@files)
{
    #unless ($file =~ /RF/) { next; }
    print "Checking: $file\n";
    my $data = `ncdump -v $var $file`;
    my @lines = split(/\n/, $data);

    my $chk;
    my @values;
    for my $line (@lines) {
	if ($line =~ /\s*$var = ?((-?\d+\.?\d*, ?)+)?\s*/) {
		$chk = 1;
		my @split = split(',', $1);
		for my $val (@split) {if ($val =~ /(-?\d+\.?\d*)/) { push (@values, $1); } }
	}
	if ($chk){
		if ($line =~ /\s*((-?\d+\.?\d*, ?)+)\s*/) {
			my @split = split(',', $1);
			for my $val (@split) {if ($val =~ /(-?\d+\.?\d*)/) { push (@values, $1); } }
		}
	}
}

my $min = 10000;
my $minpos = 10000;
my $maxneg = -10000;
my $max = -10000;
my $lastval = -32767;
my $const = 1;
my $stray = 0;
my @strayVal = ();
my $strayLen = 0;
my $break = 0;
my $ignore = 0;

#print "starting check on: $#values values\n";
$lastval = $values[1];

print "#VALUES: $#values\n";
#next;

for my $value (@values) {

    if ($skipchk eq "")
    {
	#Check if value is "Missing value"
	if ($value == -32767 || $value == 32767 || $value == 0) { next; }

	#check for data break
	#data breaks are when there is a stray value but later values are consistent with the stray
	#Usually happes when there is some missing data so the next non-missing value is too different from the previous non-missing value
	if ($strayLen > 50)
	{
	    #print "Break Check!!";
	    my $okCount = 0;
	    for (my $i = 1; $i <= $#strayVal; $i++) {
		if ($strayVal[$i] < $strayVal[$i-1]+0.1 || $strayVal[$i] > $strayVal[$i-1]-0.1) { $okCount++;}
		else { 
		    if ($okCount > 15) {
			for ($a = $i-$okCount-1; $a < $i; $a++) { 
			    if ($const && $strayVal[$a] != $lastval) { $const = 0; }
			    if ($strayVal[$a] > $max) { $max = $strayVal[$a]; }
			    if ($strayVal[$a] < $min) { $min = $strayVal[$a]; }
			    if ($strayVal[$a] > 0 && $value < $minpos) { $minpos = $strayVal[$a]; }
			    if ($strayVal[$a] < 0 && $value > $maxneg) { $maxneg = $strayVal[$a]; }
			    $stray--;
			    $lastval = $value;
			}
			$break++;
		    }
		    $okCount = 0;
		}
	    }
	    if ($okCount > 15) {
		for ($a = $#strayVal-$okCount-1; $a < $#strayVal; $a++) { 
		    if ($const && $strayVal[$a] != $lastval) { $const = 0; }
		    if ($strayVal[$a] > $max) { $max = $strayVal[$a]; }
		    if ($strayVal[$a] < $min) { $min = $strayVal[$a]; }
		    if ($strayVal[$a] > 0 && $value < $minpos) { $minpos = $strayVal[$a]; }
		    if ($strayVal[$a] < 0 && $value > $maxneg) { $maxneg = $strayVal[$a]; }
		    $stray--;
		    $lastval = $value;
		}
		$break++;
	    }
	    $strayLen = 0;
	    @strayVal = ();
	}

	#Check if value deviated from last value by more than 0.1
	#Also if the value is about the negative of the last value a limit roll over was probabably encountered passing from 180 to -180 LAT
	if (($value > $lastval+0.2 || $value < $lastval-0.2) && (($value < (-1*$lastval-0.1) || $value > (-1*$lastval+0.1)) && ($value > 1 || $value < -1))) { 
	    #print "Stray: $value - $lastval = " . ($value-$lastval) . "\n";
	    $stray++; $strayLen++; push @strayVal, $value; next;
	}
	elsif ( ($value > (-1*$lastval-0.2) && $value < (-1*$lastval+0.2)) && ($value > 1 || $value < -1) ) {
	    print "Limit Break found! MIN/MAX may be invalid, please check MINPOS/MAXNEG\n"
	}

    }
    if ($const && $value != $lastval) { $const = 0; }
    if ($value > $max) { $max = $value; }
    if ($value < $min) { $min = $value; }
    if ($value > 0 && $value < $minpos) { $minpos = $value; }
    if ($value < 0 && $value > $maxneg) { $maxneg = $value; }
    $lastval = $value;
}

#print warning messages
if ($stray > $#values/2) { print "     WARNING: More than half stray values, file omitted!\n"; $ignore =1 ;}
if ($stray > 1) { print "     WARNING: Ignored $stray stray values.\n"; }
elsif ($stray == 1) { print "     WARNING: Ignored $stray stray value.\n"; }

if ($break > 1) { print "     WARNING: $break data breaks were found.\n"; }
elsif ($break == 1) { print "     WARNING: $break data break was found\n"; }

if ($const) { print "     Skipped: $file CONSTANT DATA\n"; }

#update the total min/max with the min/max of this file
if (!$const && !$ignore){
	if ($max > $maxa) { $maxa = $max; $maxfile = $file;}
	if ($min < $mina) { $mina = $min; $minfile = $file;}
	if ($maxneg > $maxnega) { $maxnega = $maxneg;}
	if ($minpos < $minposa) { $minposa = $minpos;}

}
}

#print results
print "\nMAX:$maxa  MIN:$mina\n";
print "MAXfile:$maxfile  MINfile:$minfile\n";

#Can be used when -180/180 LON line has been crossed
print "\nMAXNEG:$maxnega  MINPOS:$minposa\n" unless ($maxnega == -10000 || $minposa == 10000);

