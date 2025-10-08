#! /usr/bin/perl
use strict;

open FILE, $ARGV[0] or die $!;
binmode FILE;
binmode STDOUT;

my ($buf, $data, $n);
while (($n = read FILE, $data, 1) != 0) {
	if ($data eq "d") { last; }
}
my $line = "";
while (($n = read FILE, $data, 8) != 0) {
	if (substr($data, 0, 1) eq "€") { next; }
	if (substr($data, 0, 1) eq chr(hex '0x00')) { next; }

	$line .= $data;
	if (length($line) >= 80) {chomp($line); print $line . chr(hex "0x0A"); $line = "";}
	if ($data =~ /ENDHD/) {chomp($line); print $line . chr(hex "0x0A"); last;}
}
exit (0);
