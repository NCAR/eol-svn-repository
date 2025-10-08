#!/usr/bin/perl
#
use strict;
use File::Find;
use Cwd;

my $cwd = getcwd;
my $dir = 'gv_n677f';
my $outdir = 'c130_n130ar';

my @GVstrings = ("gv_n677f","GV_N677F","platform_id: 350","NSF/NCAR HIAPER GV","_GV_","Tail Number N677F","GVnc");

my @C130strings = ("c130_n130ar","C130_N130AR","platform_id: 130","NSF/NCAR C-130","_C130_","Tail Number N130AR","C130nc");

find(\&process_file, $dir);

sub process_file {
    if (/yml$/) {
        my $file = $cwd."/".$File::Find::name;
	my $outfile = $file;
	$outfile =~ s/$dir/$outdir/;


	open (FILE,"$file") or die "Can't open input file $file:$!\n";
	open (OUTFILE,">$outfile") or die "Can't open output file $outfile:$!\n";
	while (<FILE>) {
	    for (my $i=0;$i<=$#GVstrings;$i++) {
	    	s/$GVstrings[$i]/$C130strings[$i]/g;
	    }
	    print OUTFILE;
	}
	close (FILE);
	close (OUTFILE);
    }
}
