#! /usr/bin/perl -w
#
##Module--------------------------------------------------------------------------
# <p>Use this script to read in a file, determine the UTC date/time from
# the file name, open an output file with the new date/time, and write out the
# file with corrected UTC Release and Nominal Release lines.
#
#
# UTC Release Time (y,m,d,h,m,s):    2008, 08, 01, 08:30:39
# Nominal Release Time (y,m,d,h,m,s):2008, 08, 01, 08:30:39
# NAZE_200808010830.cls
# ISHIGAKI_JIMA_200808030830.cls
#
# @author Linda Echo-Hawk
# @version Created for T-PARC Ishigaki Jima and Naze conversions
#
# Usage:    ConvertToUTC.pl <ESC formatted file> [--qc]
#           --qc     If qc, use the .qc extension
#           --final  Use the final output directory
#
# Example:  ConvertToUTC.pl NAZE_200808010830.cls
#           useful in a foreach loop
#
##Module--------------------------------------------------------------------------
use strict;
use warnings;
# import module to set up command line options
use Getopt::Long;

# read command line arguments
my $result;
# use the *.qc extension
my $qc;
# use the final output directory
my $final;
# "limit:i" i is optional, "limit=i" i is required
$result = GetOptions("qc" => \$qc, "final" => \$final);

my $fileName = $ARGV[0];

print "\nReading File $fileName\n";
open (my $FILE, "$ARGV[0]") || die "Can't open for reading\n";

# parse the file name and determine the 
# new time to create the new output file name
my $year;
my $month;
my $day;
my $hour;
my $min;
# the location for the new files
my $output_dir = "../output";
my $final_dir = "../final";
 
if ($fileName =~ /(\d{4})(\d{2})(\d{2})(\d{2})(\d{2})/)
{
    ($year, $month, $day, $hour, $min) = ($1,$2,$3,$4,$5);
}

my $new_month = 0;
my $new_day = 0;
my $new_hour = 0;
my $sec;

# determine the UTC time
if ($hour < 9)
{
    my $temp = (9 - $hour);
	$new_hour = (24 - $temp);

	if ($day == 1)
	{
	    $new_day = 31;
		$new_month = ($month - 1);
	}
	else
	{
	    $new_day = ($day - 1);
	    $new_month = $month;
	}
    }
else 
{
    $new_hour = $hour - 9;
	$new_day = $day;
	$new_month = $month;
}

my $newDate = sprintf("%04d%02d%02d%02d%02d", $year, $new_month, $new_day, $new_hour, $min);

my $outfile = "";

if ($fileName =~ /ISHIGAKI/)
{
	if ($qc)
	{
		$outfile = sprintf("ISHIGAKI_JIMA_%s.cls.qc", $newDate);
	}
	else
	{
   	    # $outfile = sprintf("%s.test", $fileName);
	    $outfile = sprintf("ISHIGAKI_JIMA_%s.cls", $newDate);
	}
}
else
{
  	$outfile = sprintf("NAZE_%s.cls", $newDate);
}
print "\tNEW FILE $outfile\n";

my $OUT;
# Select the correct output directory
if ($final)
{
	open ($OUT, ">".$final_dir."/".$outfile) || die "Can't open file for writing\n";
}
else
{
	open ($OUT, ">".$output_dir."/".$outfile) || die "Can't open file for writing\n";
}

my @lines = <$FILE>;
close($FILE);
                

my $index = 0;

foreach my $line (@lines)
{
   	if ($line =~ /UTC Release/)
 	{
        # UTC Release Time (y,m,d,h,m,s):    2008, 08, 01, 08:30:39
        chomp($line);
        $sec = (split(":",$line))[3];
        my $new_release_time = sprintf("UTC Release Time (y,m,d,h,m,s):    2008, %02d, %02d, %02d:%02d:%02d", 
			$new_month, $new_day, $new_hour, $min, $sec); 
        print($OUT "$new_release_time\n");
  	}
	elsif ($line =~ /Nominal Release/)
	{
        # Nominal Release Time (y,m,d,h,m,s):2008, 08, 01, 08:30:43
		my $new_nominal_time = sprintf("Nominal Release Time (y,m,d,h,m,s):2008, %02d, %02d, %02d:%02d:%02d", 
			$new_month, $new_day, $new_hour, $min, $sec);
        print($OUT "$new_nominal_time\n");
	}
    else
	{
	   	print($OUT $line);
	}
}


close $OUT;
