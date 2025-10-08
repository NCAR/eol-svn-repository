#!/usr/bin/perl

#*******************************************************************************************************#
# get_arctic_data.pl                                                                                    #
#                                                                                                       #
# Generates a list of hsi gets for each file in the specified project.                                  #
#                                                                                                       #
# Syntax: get_arctic_data.pl [project name]                                                             #
#                                                                                                       #
# 15 Jul 14: created by Morgan Garske                                                                   #
#*******************************************************************************************************#

use strict;
use DBI;

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
	print "Usage: get_arctic_data.pl [project: @valid_projects]\n";
	exit;
}

my $dbh = DBI->connect( "DBI:mysql:database=zith9;host=farskol.eol.ucar.edu", "zithview", "look-999", { RaiseError=>1} )
                or die "Unable to connect to database";
my $files_sth = $dbh->prepare('SELECT d.archive_ident, f.directory, f.filename FROM dataset d 
	                     					JOIN file f ON d.id=f.dataset_id 
	                     					JOIN dataset_project dp ON d.id=dp.dataset_id AND dp.project_id = ? AND f.directory LIKE \'/EOL/%\' AND d.visible = 1')
							or die "Couldn't prepare statement: " . $dbh->errstr;


my %dataset_files;
$files_sth->execute($project_ids{$project});
my @data;
# Generate a hash of arrays for a list of files associated with a dataset
while (@data = $files_sth->fetchrow_array()) {
	push @{ $dataset_files{$data[0]} }, $data[1] . "/" . $data[2];
}

my %batches;
my $id;
# Generate hsi get transfers with the dataset name as the batch delimeter
foreach (sort keys %dataset_files) {
	my %files;
	$id = $_;
	$batches{$id} = "get <<_$id" . "_\n";
	foreach (@{$dataset_files{$id}}) {
		unless ($files{$_}) {
			$files{$_} = 1;
			$batches{$id} .= "$_\n";
		}
	}
	$batches{$id} .= "_$id" . "_\n";
	print $batches{$id};
}
