#! /usr/bin/perl -w

use strict;
use Cwd 'abs_path';

if ($#ARGV != 2) {
print<<EOM;
This script will compare local files to hpss files and verify that they copied correctly.

usage: hpss_check.pl localDir RemoteDir [Filter]

LocalDir: local directory containing files in question.
RemoteDir: HPSS directory that should conatin files of equal size.
Filter: All files checked must match this (perl regex) is used.
EOM
exit(1);
}

Check(@ARGV);

#Check("/export/ingest2/ingest/pacdex/OMI-Aura", "/EOL/2007/pacdex/satellite/aura/omi/imagery/level2", 'OMI-Aura_L2.*\.png');
#Check("/export/ingest2/ingest/pacdex/OMI-Aura", "/EOL/2007/pacdex/satellite/aura/omi/imagery/level3", 'OMI-Aura_L3.*\.png');

sub Check {
    my ($path, $mss_path, $filter) = @_;
    die "Local directory $path does not exist." unless (-d $path);

    my $chk = 1;

    my @local_files = `ls -1 $path`;
    chomp @local_files;

    #WHY DOES HSI OUTPUT TO STDERR????
    my @mss_files = grep /^-/, `hsi ls -l $mss_path 2>&1`;
    chomp @mss_files;
    #print "FILE LIST\n";
    #print join "\n", @mss_files;

    foreach my $lfile (@local_files) {
	if ($lfile !~ /$filter/) { next; }
	unless (-s "$path/$lfile") { next; }
	
	#print "$lfile\n";
	my @match = grep /.*$lfile$/, @mss_files; 
	if ( $#match + 1 ) {
	    my @split = split(/\s+/, $match[0]);
	    if (-s $lfile != $split[4]) {
		print "Size Mismatch: $lfile\n";
		$chk = 0;
	    }
	}
	else {
	    print "file not on HPSS: $lfile\n"; 
	    $chk = 0;

	    #Does not apply to HPSS ??
	    #if (length $mfile > 128) { print "\tNAME TOO LONG\n"; }
       	}

    }

    if ($chk) { print "$path is OK\n\n"; }
    else { print "$path is NOT OK\n\n"; }
}
