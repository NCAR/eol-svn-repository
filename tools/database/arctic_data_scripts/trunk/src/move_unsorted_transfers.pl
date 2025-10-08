#!/usr/bin/perl

#*******************************************************************************************************#
# move_unsorted_transfers.pl                                                                            #
#                                                                                                       #
# Moves files transferred from HPSS to [dataset_id/data directories.                                    #
#                                                                                                       #
# Syntax: move_unsorted_transfers.pl [project name]                                                     #
#                                                                                                       #
# Output: failed_moves.txt                                                                              #
#         List of moves that failed. Lists starting and destination directories.                        #
#                                                                                                       #
# 29 Jul 14: created by Morgan Garske                                                                   #
#*******************************************************************************************************#

use strict;
use DBI;
use File::Copy "mv";

# hash contains all projects and project id's of interest
my %project_ids = (
	"ACADIS" => 340,
	"AMTS" => 240,
	"ARC-MIP" => 108,
	"ARCSS" => 100,
	"ATLAS" => 103,
	"BARROW" => 198,
	"BASE" => 308,
	"BeringSea" => 341,
	"BEST" => 343,
	"BSIERP" => 342,
	"BOREAS" => 312,
	"ITEX" => 102,
	"PacMARS" => 364,
	"SBI" => 71,
	"SHEBA" => 73,
);

my $project;
if (@ARGV == 1 && $project_ids{$ARGV[0]}) {
	$project = $ARGV[0];
}
else {
	my @valid_projects = sort keys %project_ids;
	print "Usage: move_unsorted_transfers.pl [project: @valid_projects]\n";
	exit;
}

# connect to Zith database through Farskol and prepare statements.
my $dbh = DBI->connect( "DBI:mysql:database=zith9;host=farskol.eol.ucar.edu", "zithview", "look-999", { RaiseError=>1} )
                or die "Unable to connect to database";
my $files_sth = $dbh->prepare('SELECT d.archive_ident, f.filename FROM dataset d 
	                     					JOIN file f ON d.id=f.dataset_id 
	                     					JOIN dataset_project dp ON d.id=dp.dataset_id AND dp.project_id = ? 
	                     					AND d.visible = 1 AND f.visible = 1 AND f.directory LIKE \'/EOL/%\'')
							or die "Couldn't prepare statement: " . $dbh->errstr;

my %file_dataset;

# get files to move for specified project
my @data;
$files_sth->execute($project_ids{$project});
while (@data = $files_sth->fetchrow_array()) {
	$file_dataset{$data[1]} = $data[0];
}

my $id;
# Move files to respective directories based on dataset.
open FAILS, '>', 'failed_moves.txt' or die 'could not open the failed_moves.txt file $!';
my @local_files  = glob "*";
foreach (@local_files) {
	my $dataset = $file_dataset{$_};
	print "moving $_ to ../$dataset/data/$_\n";
	mv($_, "../$dataset/data/$_") or print FAILS "$_ to $dataset/data/$_\n";
}	
close FAILS;

exit;
