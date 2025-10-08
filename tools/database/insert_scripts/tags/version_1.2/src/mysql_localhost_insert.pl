#! /usr/bin/perl -w

use strict;
use lib "/work/software/MySQL/src/lib";
use lib "/net/work/software/MySQL/src/lib";
use MySqlInserter;

&main();

sub main {
    if (scalar(@ARGV) != 1) {
	printf("Usage: mysql_localhost_insert.pl cfg-file\n");
	exit(1);
    }
    my ($cfg_file) = @ARGV;
    my $inserter = MySqlInserter->new();

    $inserter->insert($cfg_file);
}
