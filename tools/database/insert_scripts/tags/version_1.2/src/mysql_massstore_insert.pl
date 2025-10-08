#! /usr/bin/perl -w

use strict;
use lib "/work/software/MySQL/src/lib";
use lib "/net/work/software/MySQL/src/lib";
use MySqlMSSInserter;

&main();

sub main {
    if (scalar(@ARGV) != 1) {
	printf("Usage: mysql_massstore_insert.pl cfg-file\n");
	exit(1);
    }
    my ($cfg_file) = @ARGV;
    my $inserter = MySqlMSSInserter->new();

    $inserter->insert($cfg_file);
}
