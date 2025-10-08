#!/usr/bin/perl

#*******************************************************************************************************#
# list_arctic_data.pl                                                                                   #
#                                                                                                       #
# Gathers a listing of all arctic data from the HPSS.                                                   #
#                                                                                                       #
# Syntax: list_arctic_data.pl                                                                           #
#                                                                                                       #
# Input: none                                                                                           #
#                                                                                                       #
# Outpupt: listings of each file in the arctic data domain using the HSI command ls -P. For more        #
#          information on the values listed by this command, refer to the HSI ls command documentation: #                     
#          http://www.mgleicher.us/index.html/hsi/hsi_reference_manual_2/hsi_commands/ls_command.html   #
#                                                                                                       #
# 14 Jul 14: created by Morgan Garske                                                                   #
# 17 Jul 14: updated to ignore hidden datasets.                                                         #
#*******************************************************************************************************#

use strict;
use DBI;
use IPC::Open3;
use File::Spec;
use Symbol qw(gensym);

# hash contains all projects and project id's of interest
my %project_ids = (
	340 => "ACADIS",
	240 => "AMTS",
	108 => "ARC-MIP",
	100 => "ARCSS",
	103 => "ATLAS",
	198 => "BARROW",
	308 => "BASE",
	341 => "BeringSea",
	343 => "BEST",
	342 => "BSIERP",
	312 => "BOREAS",
	102 => "ITEX",
	364 => "PacMARS",
	71 => "SBI",
	73 => "SHEBA",
);

# connect to Zith database through Farskol and prepare statements.
my $dbh = DBI->connect( "DBI:mysql:database=zith9;host=farskol.eol.ucar.edu", "zithview", "look-999", { RaiseError=>1} )
                or die "Unable to connect to database";
my $files_sth = $dbh->prepare('SELECT f.directory, f.filename FROM dataset d 
	                     					JOIN file f ON d.id=f.dataset_id 
	                     					JOIN dataset_project dp ON d.id=dp.dataset_id AND dp.project_id = ? AND f.directory LIKE \'/EOL/%\' AND d.visible = 1')
							or die "Couldn't prepare statement: " . $dbh->errstr;

my %directories;
my %files;


# get directories and files into a hash to avoid double-listing of files/directories
foreach (keys %project_ids) {
	$files_sth->execute($_);
	my @data;
	while (@data = $files_sth->fetchrow_array()) {
			$directories{$data[0]} = 1;
			$files{$data[0] . "/" . $data[1]} = 1;
	}
}



my $temp = "$$.tmp";
open LIST, '>', 'EOL_arctic_list.txt' or die "Could not open EOL_arctic_list.txt $!";
my $n = 1;

# write complete listing to a temporary file, then add listings to list file only if
# they are in the file listing from the database and have not already been seen. 
foreach(keys %directories) {
	print "listing directory $n of " . keys (%directories) . ": $_\n";
	`hsi ls -P $_ >& $temp`;
	open TEMP, '<', $temp or die "Could not open $temp: $!";
	while (<TEMP>) {
		if (/^FILE\s([\w\/.-]+)/) {
			if ($files{$1}) {
				print LIST $_;
				$files{$_} = 0;
			}
		}
	}
	close TEMP;
	$n++;
}
unlink $temp;
close LIST;
exit;