#!/usr/bin/perl

#*******************************************************************************************************#
# copy_arctic_files.pl                                                                                  #
#                                                                                                       #
# Copies localhost files to dataset directories for the specified project. Must be run in the project   #
# directory for the desired project (e.g. ARCSS/, ATLAS/, etc.)                                         #
# Warning: files will be copied to [dataset_id]/data. If there is already data in the data folder       #
# within a directory structure, the files will still be copied even if those files already exist in the #
# directory structure, and will be duplicated. Because of this, it is not reccommended to run this      #
# file unless first copying over files for an entire project to create a staging area for the cloud     #
# upload. If you wish to update the project directory for new datasets, it is better to run             #
# arctic_mkdirs.pl to create the dataset directories and then run check_arctic_data.pl to see which     #
# files need to be copied over, and then copy the necessary data files over, while trying to maintain   #
# the directory structure in the archive.                                                               #
#                                                                                                       #
# Syntax: copy_local_arctic_files.pl [project name]                                                     #
#                                                                                                       #
# Output: failed_copies.txt                                                                             #
#         List of copies that failed. Includes original and destination directory                       #
#                                                                                                       #
# 18 Jul 14: created by Morgan Garske                                                                   #
# 10 Nov 14: using system copy for the -u flag which only copies if newer or does not exist in          #
#            destination                                                                                #
#*******************************************************************************************************#

use strict;
use DBI;
use File::Copy "cp";

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
	print "Usage: copy_arctic_files.pl [project: @valid_projects]\n";
	exit;
}

# connect to Zith database through Farskol and prepare statements.
my $dbh = DBI->connect( "DBI:mysql:database=zith9;host=farskol.eol.ucar.edu", "zithview", "look-999", { RaiseError=>1} )
                or die "Unable to connect to database";
my $files_sth = $dbh->prepare('SELECT d.archive_ident, f.directory, f.filename, f.purpose FROM dataset d 
	                     					JOIN file f ON d.id=f.dataset_id 
	                     					JOIN dataset_project dp ON d.id=dp.dataset_id AND dp.project_id = ? 
	                     					AND d.visible = 1 AND f.visible = 1 AND f.directory LIKE \'/net/%\'')
							or die "Couldn't prepare statement: " . $dbh->errstr;

my %dataset_files;
my %file_purpose;
my %file_name;

# get files to copy for specified project
my @data;
$files_sth->execute($project_ids{$project});
while (@data = $files_sth->fetchrow_array()) {
	my $file = $data[1] . "/" . $data[2];
	$file_purpose{$file} = $data[3];
	$file_name{$file} = $data[2];
	push @{$dataset_files{$data[0]}}, $file;
}

my $id;

open FAILS, '>', 'failed_copies.txt' or die 'could not open the failed_copies.txt file $!';
foreach (sort keys %dataset_files) {
	$id = $_;
	print "starting dataset $id\n";
	foreach (@{$dataset_files{$id}}){
		my $purpose = $file_purpose{$_};
		my $filename = $file_name{$_};
		print "copying $_ to $id/$purpose/$filename\n";
		print `cp $_ $id/$purpose/$filename -vu`;
		#cp($_, "$id/$purpose/$filename") or print FAILS "$_ to $id/$purpose/$filename\n";
	}
}	
close FAILS;

exit;
