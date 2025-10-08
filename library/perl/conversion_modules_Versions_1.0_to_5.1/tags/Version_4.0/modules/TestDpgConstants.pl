#! /usr/bin/perl -w

package TestDpgConstants;
use strict;
use lib ".";
use TestModule;
use DpgConstants qw(:DEFAULT);
our @ISA = ("TestModule");

&main();

sub main {
    my $tester = TestDpgConstants->new();

    $tester->assertValue(6.1121,$ES0,"ES0");
    $tester->assertValue(3.1415926,$PI,"PI");
}
