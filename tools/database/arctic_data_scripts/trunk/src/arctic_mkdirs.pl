#!/usr/bin/perl

#*******************************************************************************************************#
# mk_arctic_dirs.pl                                                                                     #
#                                                                                                       #
# Makes a directory for each dataset belonging to the specified project in the working directory        #
#                                                                                                       #
# Syntax: mk_arctic_dirs.pl [project name]                                                              #
#                                                                                                       #
# 18 Jul 14: created by Morgan Garske                                                                   #
#                                                                                                       #
# 29 Dec 14: no longer creates the meta/ folder. Metadata files will be contained in base directory     #
#*******************************************************************************************************#

use strict;
use DBI;
use File::Path qw(make_path);

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
	print "Usage: mk_arctic_dirs.pl [project: @valid_projects]\n";
	exit;
}

# connect to Zith database through Farskol and prepare statements.
my $dbh = DBI->connect( "DBI:mysql:database=zith9;host=farskol.eol.ucar.edu", "zithview", "look-999", { RaiseError=>1} )
                or die "Unable to connect to database";
my $files_sth = $dbh->prepare('SELECT d.archive_ident FROM dataset d 
	                     					JOIN dataset_project dp ON d.id=dp.dataset_id AND dp.project_id = ? AND d.visible = 1')
							or die "Couldn't prepare statement: " . $dbh->errstr;

my %datasets;

# get datasets for project specified
my @data;
$files_sth->execute($project_ids{$project});
while (@data = $files_sth->fetchrow_array()) {
		$datasets{$data[0]} = 1;
}

my %current_dirs = map {$_ => 1} glob "*";

# make directory structure for each dataset
foreach(keys %datasets) {
	unless ($current_dirs{$_}) {
		make_path $_;
		make_path ("$_/data", "$_/doc")
	}
}
exit;
