#!/usr/bin/perl
use strict;
use Cwd 'abs_path';
print "\n";

#Variable Declarations
our $sdir;
our $tdir;
our $force = 0;
#Helpfile:
if (! defined(@ARGV) ) { print<<EOF;
No directory inputted
usage ./archive.pl [options] SRC_DIR DEST_DIR EXT

	Options:
	-f		Force, does not ask user.
EOF
exit(1)
}

#Commandline interpreter
foreach my $arg (@ARGV) {
	if ($arg eq "-f") { $force = 1; }
}

my $ext = $ARGV[$#ARGV];
$tdir = $ARGV[$#ARGV-1];
$sdir = $ARGV[$#ARGV-2];
if (! -d $sdir) { print "Directory does not exist!\n"; exit(1); }

$sdir = abs_path($sdir);

my @commands;
my @files;
if (-d $sdir) {
	opendir(DIR, $sdir);
	@files = readdir(DIR);
	close(DIR);
	@files = grep(/.*\.$ext/, @files);
	if ($#files < 0) { print "NO FILES FOUND\n"; exit(1); }
	foreach my $file (@files) {
	    push(@commands, "ssh -x bora msput_job -pe 32767 -pr 41113009 -wpwd RAFDMG $sdir/$file mss:$tdir/$file");
	}
	#tell the user what we are going to do
	#before we actually do it is usually best :-P
	my $cont;
	if (!$force) {
		foreach my $line (@commands) {
			print "$line\n";
		}
		print "Total Commands: $#commands\n";
		print "Execute the commands shown? Yes == Enter, No == anything else:"; #ask for permission
		$cont = <STDIN>;
	}
	if ($cont eq "\n" || $force) {
		#if the user has given the go ahead, run!
		foreach my $line (@commands) {
		    print "$line\n";
		    my $result = `$line`; 
		}

	}
}
else {
	print "Directory does not exist!\n";
}
