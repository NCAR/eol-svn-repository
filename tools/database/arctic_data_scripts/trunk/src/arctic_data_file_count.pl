#!/usr/bin/perl

####################################################################################
# arctic_data_file_count.pl                                                        #
# Gathers information on arctic data projects by project and as a whole.           #
#                                                                                  #
# Syntax: hpss_file_count.pl                                                       #
#                                                                                  #
# Input: none                                                                      #
#                                                                                  #
# Output: "|" separated values describing project, total number of datasets,       #
#         datasets with files on the HPSS, number of files (total, on localhost,   #
#         on HPSS), and size of files (total, on localhost, on HPSS) in KB. These  #
#         values are output for each project and for arctic data as a whole. Note  #
#         that totals for arctic data as a whole will not be the sum of values for #
#         each individual project, as some files belong to multiple datasets and   #
#         some datasets belong to multiple projects.                               #
#                                                                                  #
# 08 Jul 14: created by Morgan Garske                                              #
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
my $files_sth = $dbh->prepare('SELECT d.archive_ident, f.directory, f.filename, f.size_kb, dp.project_id FROM dataset d 
	                     					JOIN file f ON d.id=f.dataset_id 
	                     					JOIN dataset_project dp ON d.id=dp.dataset_id AND dp.project_id = ? AND d.visible = 1')
							or die "Couldn't prepare statement: " . $dbh->errstr;
my $datasets_sth = $dbh->prepare('SELECT d.archive_ident FROM dataset d
								  JOIN dataset_project dp ON d.id=dp.dataset_id AND dp.project_id = ? AND d.visible = 1')
							or die "Couldn't prepare statement: " . $dbh->errstr;

# hashes will keep track of datasets and files they are not double-counted (e.g., 
# when one dataset/file belongs to multiple projects/datasets)
my %all_datasets;
my %all_datasets_HPSS;

my %all_files_local;
my %all_files_HPSS;
my %all_directories_HPSS;

my %dataset_directories;
print "Project|Datasets (total)|Datasets with files on HPSS|Files (total)|Files (local)|Files (HPSS)|Size (total) - KB|Size (local) - KB|Size (HPSS) - KB\n";
# Gather information by project and print the result. Meanwhile, keep statistics on arctic data as a whole
foreach (keys %project_ids) {
	my $files_local = 0;
	my $files_HPSS = 0;
	my $size_local = 0;
	my $size_HPSS = 0;
	$files_sth->execute($_);
	$datasets_sth->execute($_);
	my $files_total = $files_sth->rows;
	my @data;
	my $number_of_datasets = $datasets_sth->rows;
	my $HPSS_dataset_count = 0;
	my %project_datasets;
	my %project_datasets_HPSS;
	while (@data = $files_sth->fetchrow_array()) {
		my $file = $data[1] . "/" . $data[2];
		if ($data[1] =~ /\/net\// ) {
			$project_datasets{$data[0]} = 1;
			$files_local++;
			$size_local += $data[3];
			unless (exists $all_files_local{$file}) {
				$all_files_local{$file} = $data[3];
			}
			unless (exists $all_datasets{$data[0]}) {
				$all_datasets{$data[0]} = 1;
			}
		}
		elsif ($data[1] =~ /\/EOL\//) {
			$all_directories_HPSS{$data[1]} = 1;
			$project_datasets{$data[0]} = 1;
			$project_datasets_HPSS{$data[0]} = 1;
			$files_HPSS++;
			$size_HPSS += $data[3];
			unless (exists $all_files_HPSS{$file}) {
				$all_files_HPSS{$file} = $data[3];
			}
			# else {
			# 	print "$file, $data[0], $data[3]\n";
			# }
			unless (exists $all_datasets_HPSS{$data[0]}) {
				$all_datasets_HPSS{$data[0]} = $data[3];
				$all_datasets{$data[0]} = 1;
			}
		}
		else {
			print "$data[1], $data[3]\n";
		}
	}
	my $size_project = $size_local + $size_HPSS;
	$HPSS_dataset_count = keys %project_datasets_HPSS;
	print "$project_ids{$_}|$number_of_datasets|$HPSS_dataset_count|$files_total|$files_local|$files_HPSS|$size_project|$size_local|$size_HPSS\n";
	# %all_files_HPSS = {};
}

my $total_datasets = keys %all_datasets;
my $total_datasets_HPSS = keys %all_datasets_HPSS;
my $total_files_local = keys %all_files_local;
my $total_files_HPSS = keys %all_files_HPSS;
my $all_files_total = $total_files_local + $total_files_HPSS;
my $total_size_local = sum(values %all_files_local);
my $total_size_HPSS = sum(values %all_files_HPSS);
my $all_size_total = $total_size_local + $total_size_HPSS;
print "Total|$total_datasets|$total_datasets_HPSS|$all_files_total|$total_files_local|$total_files_HPSS|$all_size_total|$total_size_local|$total_size_HPSS\n";


exit;