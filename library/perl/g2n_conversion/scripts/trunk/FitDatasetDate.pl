#! /usr/bin/perl -w
###
#Updates CODIAC files in a specified dataset_id from data conatined in a taplog
#Created By: Seam Stroble
###


use strict;
use lib "/h/eol/stroble/scripts/MySQL/lib";
use lib "/net/work/Projects/g2n_conversion/scripts";
use MySqlDatabase;
use MySqlDataset;
use Date::Calc qw(:all);
my $msg = "";
my $force = 0;

if ($#ARGV < 0)
{
print<<EOM;
This script will read a taplog and update the date/time range of files in codiac with the event set to the flight number for a specified dataset.

usage FitDatasetDate dataset_id

dataset_id: dataset_id from CODIAC (ex: 120.001 )

options:
	-f Forces dataset to fit data (Will contract DOI to fit data exactly)
EOM

exit(1);
}


my $dataset_id = $ARGV[0];
if (defined $ARGV[1] && $ARGV[1] eq "-f") { $force = 1; } 

my $database = MySqlDatabase->new("zediupdate","change-456");
#$database->setHost("merlot.eol.ucar.edu");
$database->connect();

#load files from CODIAC
($msg, my %data) = $database->selectFull("file","*","dataset_id=\"$dataset_id\"");
if ($msg ne "") { print "$msg\nDatabase Rolled back!\n"; $database->rollback(); $database->disconnect(); exit(1); }
unless ($#{$data{"row"}}+1 >= 1) { print "No files found in CODIAC!\n"; $database->disconnect(); exit(1); }


#figure out how the data is layed out
my $filenameIndex = -1;
my $begin_dateIndex = -1;
my $end_dateIndex = -1;
my $eventIndex = -1;
my $sizeIndex = -1;
my $file_idIndex = -1;
for (my $i = 0; $i <= $#{$data{"name"}}; $i++) {

	if (@{$data{"name"}}[$i] eq "filename") { $filenameIndex = $i; }
	if (@{$data{"name"}}[$i] eq "begin_date") { $begin_dateIndex = $i; }
	if (@{$data{"name"}}[$i] eq "end_date") { $end_dateIndex = $i; }
	if (@{$data{"name"}}[$i] eq "event") { $eventIndex = $i; }
	if (@{$data{"name"}}[$i] eq "size_kb") { $sizeIndex = $i; }
	if (@{$data{"name"}}[$i] eq "file_id") { $file_idIndex = $i; }
}


#load Dataset
my $datasetUpdated = 0;
my $dataset = MySqlDataset->new($dataset_id);
$dataset->selectDataset($database);

#Determine Dataset DOI
my @begin_date;
my @end_date;
if ($force) {
    @begin_date = (9999,12,31,23,59,59);
    @end_date = (0,0,0,0,0,0);
}
else {
    @begin_date = split(/[ :-]/, $dataset->getBeginDate());
    @end_date = split(/[ :-]/, $dataset->getEndDate());
}

for (my $i = 0; $i <= $#{$data{"row"}}; $i++) {
    my @file_begin_date = split(/[ :-]/, $data{"row"}[$i][$begin_dateIndex]);
    my $chk = 0;
    for (my $b = 0; $b <= $#begin_date; $b++) {
	if ($file_begin_date[$b] < $begin_date[$b]) { $chk = 1; last; }
	if ($file_begin_date[$b] > $begin_date[$b]) { last; }
    }
    if ($chk) { @begin_date = @file_begin_date; }

    my @file_end_date = split(/[ :-]/, $data{"row"}[$i][$end_dateIndex]);
    $chk = 0;
    for (my $b = 0; $b <= $#end_date; $b++) {
	if ($file_end_date[$b] > $end_date[$b]) { $chk = 1; last; }
	if ($file_end_date[$b] < $end_date[$b]) { last; }
    }
    if ($chk) { @end_date = @file_end_date; }
}

print "\nOld BeginDate: " . $dataset->getBeginDate() . "\n";
print "Old EndDate:   " . $dataset->getEndDate() . "\n\n";
print "New BeginDate: $begin_date[0]-$begin_date[1]-$begin_date[2] $begin_date[3]:$begin_date[4]:$begin_date[5]\n";
print "New EndDate:   $end_date[0]-$end_date[1]-$end_date[2] $end_date[3]:$end_date[4]:$end_date[5]\n";

$dataset->setBeginDate(@begin_date);
$dataset->setEndDate(@end_date);
$dataset->updateDataset($database);

print "\n#To update the dataset, press Enter.\n";
print "#To cancel the update, enter any value and press Enter.\n";
print "#>>";
my $result = <STDIN>;
chomp $result;
if ($result eq "")
{
	print $database->commit();
}
else
{
    	print "User has selected to cancel, database rolledback!\n";
	print $database->rollback();
}

print $database->disconnect();
exit(0);
