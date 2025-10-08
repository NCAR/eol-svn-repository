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

package ExtractSounding;
use strict;
use Extracter;

&main();

sub main {

    if (scalar(@ARGV) != 3) {
	die("Usage:  extract.pl input_dir output_dir freq\n");
    }
    my ($indir,$outdir,$freq) = @ARGV;

    mkdir($outdir) unless (-e $outdir);

    opendir(my $RAW,$indir) or die("Cannot open $indir\n");
    my @files = grep(/\.cls/,readdir($RAW));
    closedir($RAW);

    my $extracter = Extracter->new(sprintf("%s/%d_second_extracter.log",$outdir,$freq));
    foreach my $file (sort(@files)) {
	$extracter->extract(sprintf("%s/%s",$indir,$file),
			    sprintf("%s/%s",$outdir,$file),$freq);
    }
}
