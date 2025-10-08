#!/usr/bin/perl

#*******************************************************************************************************#
# listfromgets.pl                                                                                       #
#                                                                                                       #
# Generates an hsi ls -P style listing from an hsi gets file. Requires both get file and a hsi ls -P    #
# file that includes listings for the files in the gets file.                                           #
#                                                                                                       #
# Syntax: listfromgets.pl [hsi gets file] [hsi ls -P file that includes listings for files in get file] #
#                                                                                                       #
# Output: hsi ls -P style listing of the files in the gets file.                                        #
#                                                                                                       #
# 24 Jul 14: created by Morgan Garske                                                                   #
#*******************************************************************************************************#

use strict;

my %files;
my %listings;
# generate hashes of listings and files from inputs
while (<>) {
	if (/^FILE\s([\/.-\w]+)\s/) {
		$listings{$1} = $_;
	}
	else {
		chomp;
		$files{$_} = 1;
	}
}

# print listing for each file that was in the gets file
foreach (keys %files) {
	print $listings{$_};
}

exit;
