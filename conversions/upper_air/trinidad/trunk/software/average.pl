#! /usr/bin/perl -w

##Module-----------------------------------------------------------------
# <p>The module was created by J. Clawson.
#
# @author Joel Clawson
# @version 1.0  Original Creation
#
# @author L. Cully
#   Added header.
#
##Module-----------------------------------------------------------------

package AverageSounding;
use strict;
use Averager;

&main();

sub main {

    if (scalar(@ARGV) != 4) {
	die("Usage:  average.pl input_dir output_dir points freq\n");
    }
    my ($indir,$outdir,$points,$freq) = @ARGV;

    mkdir($outdir) unless (-e $outdir);

    opendir(my $RAW,$indir) or die("Cannot open $indir\n");
    my @files = grep(/\.cls/,readdir($RAW));
    closedir($RAW);

    my $averager = Averager->new(sprintf("%s/%d_point_averager.log",$outdir,$points));
    foreach my $file (sort(@files)) {
	$averager->average(sprintf("%s/%s",$indir,$file),
			   sprintf("%s/%s",$outdir,$file),$points,$freq);
    }
}
