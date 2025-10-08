#!/usr/bin/perl

#*******************************************************************************************************#
# check_arctic_data.pl                                                                                  #
#                                                                                                       #
# Checks for missing or extra files for the specified dataset. Must be run in the top-level directory   #
# for the desired project. Outputs the names of missing or extra files for each dataset                 #
#                                                                                                       #
# Syntax: check_arctic_data.pl [project name]                                                           #
#                                                                                                       #
# 01 Aug 14: created by Morgan Garske                                                                   #
#*******************************************************************************************************#

use strict;

use File::Find;
use DBI;

my @local_files;
sub list_files {
	my $dir = shift;
	find (\&wanted, $dir);

	sub wanted {
		if (-f) {
			push @local_files, $_;
		}
	}
}

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
	print "Usage: check_arctic_data.pl [project: @valid_projects]\n";
	exit;
}

# connect to Zith database through Farskol and prepare statements.
my $dbh = DBI->connect( "DBI:mysql:database=zith9;host=farskol.eol.ucar.edu", "zithview", "look-999", { RaiseError=>1} )
                or die "Unable to connect to database";
my $files_sth = $dbh->prepare('SELECT d.archive_ident, f.filename FROM dataset d 
	                     					JOIN file f ON d.id=f.dataset_id 
	                     					JOIN dataset_project dp ON d.id=dp.dataset_id WHERE dp.project_id = ? 
	                     					AND d.visible = 1 AND f.visible = 1')
							or die "Couldn't prepare statement: " . $dbh->errstr;

my %dataset_files;
my @data;
# get list of files for each dataset
$files_sth->execute($project_ids{$project});
while (@data = $files_sth->fetchrow_array()) {
	$dataset_files{@data[0]}{@data[1]} += 1;
}

# check generated hash against local files for each dataset
foreach (sort keys %dataset_files) {
	my $dataset = $_;
	@local_files = ();
	&list_files($dataset);
	foreach (@local_files) {
		if ($dataset_files{$dataset}{$_}) {
			$dataset_files{$dataset}{$_} -= 1;
		}
		else {
			print "Extra file found for dataset $dataset: $_\n";
		}
	}
	foreach (keys %{$dataset_files{$dataset}}) {
		if ($dataset_files{$dataset}{$_}) {
			print "File missing for dataset $dataset: $_\n";
		}
	}
}
