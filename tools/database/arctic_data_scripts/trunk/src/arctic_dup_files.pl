#!/usr/bin/perl

####################################################################################
# arctic_dup_files.pl                                                              #
# Determines which arctic data files belong to multiple datasets and to which      #
# datasets those files belong.                                                     #
#                                                                                  #
# Syntax: arctic_dup_files.pl                                                      #
#                                                                                  #
# Input: none                                                                      #
#                                                                                  #
# Output: prints a group of datasets, then the files that are shared by all those  #
#         datasets for each group of files that share multiple datasets.           #
#                                                                                  #
# 16 Jul 14: created by Morgan Garske                                              #
# 17 Jul 14: updated to ignore hidden datasets.                                    #
####################################################################################

use strict;
use DBI;
use List::Util qw(sum);

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
my $files_sth = $dbh->prepare('SELECT d.archive_ident, f.directory, f.filename, f.size_kb FROM dataset d 
	                     					JOIN file f ON d.id=f.dataset_id 
	                     					JOIN dataset_project dp ON d.id=dp.dataset_id AND dp.project_id = ? AND d.visible = 1')
							or die "Couldn't prepare statement: " . $dbh->errstr;


my %file_datasets;
my %file_sizes;
foreach (keys %project_ids) {
	$files_sth->execute($_);
	my @data;
	while (@data = $files_sth->fetchrow_array()) {
		my $file = $data[1] . "/" . $data[2];
		$file_sizes{$file} = $data[3];
		unless ($file_datasets{$file} =~ $data[0]){
			$file_datasets{$file} .= "$data[0], ";
		}
	}
}

my %dataset_files;
foreach(keys %file_datasets) {
	# match multiple datasets (separated by ", ") in format ##.###, ###.###, ##.ARCSS### or ###.ARCSS###.
	if ($file_datasets{$_} =~ /((\d{2,3}\.(ARCSS)?\d{3},\s){2,})/) {
		$dataset_files{$1} .= "    $_\n";
	}
}

foreach(sort keys %dataset_files) {
	my $files = $dataset_files{$_};
	s/,\s$//;
	print "Files belonging to datasets $_:\n$files";
}

my $HPSS_dup_size;
my $local_dup_size;
foreach(sort keys %dataset_files) {
	my @files = split ("\n", $dataset_files{$_});
	s/,\s$//;
	my @datasets = split(', ', $_);
	my $dupes = @datasets - 1;
	foreach (@files) {
		my $file = $_;
		$file =~ s/^\s+|\s+$//g;
		if (/\/EOL\//){ 
			$HPSS_dup_size += $file_sizes{$file} * $dupes;
		}
		elsif (/\/net\//) {
			$local_dup_size += $file_sizes{$file} * $dupes;
		}
		else {
			print "$_\n";
		}
	}
}

print "Size of extra local: $local_dup_size\nSize of extra HPSS: $HPSS_dup_size\n";

exit;
