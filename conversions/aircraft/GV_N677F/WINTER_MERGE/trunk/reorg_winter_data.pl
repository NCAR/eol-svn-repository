#!/usr/bin/perl
############
# Script to read data from the WINTER field project aircraft dir on HPSS,
# where files are stored by instrument, and write them to /net/archive by
# flight so that PIs can download all files for a flight with one click to
# facilitate merge generation.
############

use strict;
use lib "/net/work/lib/perl/mail";
use lib "/net/work/lib/perl/hpss";
use HPSS;
use FindBin qw($Script $Bin);
use File::stat;
use MAIL;
use Cwd;

my @monitors = ("janine\@ucar.edu");
my $log = "I am $Bin/$Script running on ".`/bin/hostname`."\n\n";
my $HPSS_DIR = "/EOL/2015/winter/aircraft/c130_n130ar";
my $local_dir = "/net/archive/data/winter/by_flight";

# Figure out what day today is
our ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) =
                                            localtime(time);
our @abbr = qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);

#Convert date to string in filenames (yyyymmdd)
my $today=sprintf ("%04d%02d%02d",$year+1900,$mon+1,$mday);
$log .= "Today is $today\n\n";

my @lines = HPSS::ls($HPSS_DIR,"-D");
my %filehash;
my %flight = {};
foreach my $line (@lines) {
    # Skip merge files
    if ($line =~ /MERGES/) {next;}
    if ($line =~ /ADS/) {next;}
    if ($line =~ /CAMERA/) {next;}
    if ($line =~ /FIELD_INTERNAL_ONLY/) {next;}
    if ($line =~ /UMD_Cessna/) {next;}
    #print "$line\n";
    if ($line =~ /^d/) {
	# Directory, so look inside
	read_dir($line, $HPSS_DIR);
    }
}

# Look through all local files, and if newer files exist on HPSS,
# download them.
$log .= "Syncing $HPSS_DIR with ".$local_dir."\n\n";
foreach my $date (sort keys %filehash) {
    if ($flight{$date}=~ /_rf0/) {next;}
    my $flightdir = $local_dir."/${date}_".$flight{$date};
    $log .= "Reading $flightdir\n\n";
    if (!(-d $flightdir)) {system(`mkdir $flightdir`);}
    system(`cd $flightdir`);
    foreach my $file (keys %{$filehash{$date}}) {
	my @pathbits = split('/',$file);
	my $localmod;
	my $localfile ="$flightdir/$pathbits[scalar(@pathbits)-1]";
	if (-e $localfile) {
	   $localmod = stat($localfile);
           my ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) =
	       localtime($localmod->mtime); 
           $localmod=sprintf ("%04d%02d%02d",$year+1900,$mon+1,$mday);
        } else {
	   $localmod = "19000101";
        }
	#print "HPSS mod date ${$filehash{$date}{$file}}[0] <=> local mod date $localmod\n";
	if (${$filehash{$date}{$file}}[0] > $localmod) {

	    # If have multiple versions, only get latest version of file.
	    # This area will need to be added to as more versions come in.
	    my $download = 0;
	    if ($file =~ /NONOyO3/) {
		if ($file =~ /R0/) {$download = 1};
	    } elsif ($file =~ /SAGA-AERO/) {
		if ($file =~ /R1/ && $file != /20150223/) {$download = 1};
		if ($file =~ /R2/) {$download = 1};
	    } elsif ($file =~ /UWTOFCIMS1/) {
		if ($file =~ /RC/) {$download = 1};
	    } elsif ($file =~ /RAF-LRT/) {
		if ($file =~ /R1/) {$download = 1};
	    } elsif ($file =~ /CO_C130/ || $file =~ /CO2CH4_C130/) {
		if ($file =~ /R0/) {$download = 1};
	    } elsif ($file =~ /TOGA/) {
		if ($file =~ /R0/) {$download = 1};
	    } elsif ($file =~ /ISAF/) {
		if ($file =~ /R0/) {$download = 1};
	    } elsif ($file =~ /UMDAircraft/) {
		if ($file =~ /R1/) {$download = 1};
	    } elsif ($file =~ /NH3/) {
		if ($file =~ /R0/) {$download = 1};
	    } elsif ($file =~ /ARNOLD/) {
		if ($file =~ /R1/ && $file != /20150307/) {$download = 1};
		if ($file =~ /R0/) {$download = 1};
	    } elsif ($file =~ /GEOSChemNRT/) {
		#Viral not submitting updated date expect in merge.
		{$download = 0}; 
	    } elsif ($file =~ /FLAGG/) {
		#UMD flights are not C130 flights
		{$download = 0}; 
	    } elsif ($file =~ /20150114/) {
		{$download = 0}; 
	    } elsif ($file =~ /Merge/) {
		{$download = 0}; 
	    } else {
		$download = 1;
	    }
	    if ($download) {
		# Remove local file, regardless of version
		my $rmfile = $localfile;
		$rmfile =~ s/_R[A-Z0-9].*$//;
		system(`/bin/rm -f $rmfile*`);

	        $log .= "Retrieve $file: HPSS mod date ${$filehash{$date}{$file}}[0] more recent than".
	          " local mod date $localmod\n\n";
	        my $msg = HPSS::get(\"$file",\"$localfile");
	        if ($msg != //) {$log .= "$msg\n\n";}
	        sleep(15);
	    }
        }
    }
    system(`cd ../`);
}

MAIL::send_mail("WINTER file sync",$log,@monitors);

sub read_dir {
        my $line= shift;
	my $hpss_dir = shift;
	(my @info) = split(' ',$line);
	my $dir =  $info[scalar(@info)-1];
	$log .= "Processing $dir\n\n";
	my @files = HPSS::ls($hpss_dir."/".$dir,"-D");
	foreach my $file (@files) {
	    #print $file."\n";
	    if ($file =~ /^d/) {
		read_dir($file, $hpss_dir."/".$dir);
	    }
	    if ($file =~ /^-/) {
	        (my @fileinfo) = split(' ',$file);
		my $filename = $fileinfo[scalar(@fileinfo)-1];
		my @modifiedDate = ( $fileinfo[scalar(@fileinfo)-2],
		                     $fileinfo[scalar(@fileinfo)-5],
		                     $fileinfo[scalar(@fileinfo)-4]);
		if ($modifiedDate[0] =~ /:/) {$modifiedDate[0] = $year+1900};
		my @month = grep { $abbr[$_] eq $modifiedDate[1]} 0 .. $#abbr;
		$modifiedDate[1] =  @month[0]+1;
		my $modDate = sprintf("%04d%02d%02d",@modifiedDate);
		#print "Found $filename last modified $modDate \n";

		if ($filename =~ m/([0-9]{8})/) {
		    my $date = $1;
		    push @{$filehash{$date}{$hpss_dir."/".$dir."/".$filename}},$modDate;
		    if ($filename =~ m/([rtf]f..)/) {
		        $flight{$date} = $1;
		    }
		    # WINTER exception
		    $flight{"20150222"} = "rf06";
	        }
	    }
	}
}
