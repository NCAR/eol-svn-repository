#! /usr/bin/perl -w
###
#Updates CODIAC files in a specified dataset_id from data conatined in a taplog
###


use strict;
use lib "/h/eol/stroble/scripts/MySQL/lib";
use lib "/net/work/Projects/g2n_conversion/scripts";
use MySqlDatabase;
use MySqlDataset;
use Date::Calc qw(:all);
my $msg = "";
my $ignoreSplit = 0;

if ($#ARGV < 1)
{
print<<EOM;
This script will read a taplog and update the date/time range of files in codiac with the event set to the flight number for a specified dataset.

usage Taplog2CODIAC.pl taplog dataset_id [options]

taplog: filename of a taoplog in /net/www/raf/Catalog (ex: taplog.lrt.876.html )
dataset_id: dataset_id from CODIAC (ex: 120.001 )

Options:
	-i Ignore Splitting (ie. if taplog has RF01 A and B but CODIAC has A,B,C all three codiac files will be set to the combined date range of A and B in the taplog)

EOM

exit(1);
}
my $taplog = $ARGV[0];
my $dataset_id = $ARGV[1];
for (my $a = 2; $a <= $#ARGV; $a++)
{
	if ($ARGV[$a] eq "-i") { $ignoreSplit = 1; }
}

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


#load taplog
unless (-e "/net/www/raf/Catalog/$taplog") {print "Taplog not found!\n"; $database->desconnect(); exit(1); }
open FILE, "/net/www/raf/Catalog/$taplog";

#load Dataset
my $datasetUpdated = 0;
my $dataset = MySqlDataset->new($dataset_id);
$dataset->selectDataset($database);

#compare taplog to CODIAC
print "\n";
my @events = ();
while (<FILE>)
{
	if ($_ =~ /\<[Aa][^>]*\>\s*(\S{3,7})\s+(\d+\/\d+\/\d+)\s+(\d+:\d+:\d+)\??\s+(\d+:\d+:\d+)\??/)
	{
	    chomp(my $fltno = $1);

	    chomp(my $datetemp = $2);
	    chomp(my $start_time = $3);
	    chomp(my $end_time = $4);

	    my @datesplit = split('/',$datetemp);

	    if (length($datesplit[0]) == 1) { $datesplit[0] = "0" . $datesplit[0]; }

	    my $date = "$datesplit[2]-$datesplit[0]-$datesplit[1]";

	    my @begin_date = ($datesplit[2], $datesplit[0], $datesplit[1], split(/:/,$start_time));
	    my @end_date = ($datesplit[2], $datesplit[0], $datesplit[1], split(/:/,$end_time));

	    #START: BEGIN DATE CORRECTION
	    if ($begin_date[3] > 23) {
		#Reduce the hour
		splice(@begin_date, 3, 1, $begin_date[3]-24);

		#Add a day
		@begin_date = Add_Delta_YMDHMS(@begin_date, 0, 0, 1, 0, 0, 0);
	    }
	    #END: BEGIN DATE CORRECTION

	    #START: END DATE CORRECTION
	    if ($end_date[3] > 23) {
		#Reduce the hour
		splice(@end_date, 3, 1, $end_date[3]-24);

		#Add a day
		@end_date = Add_Delta_YMDHMS(@end_date, 0, 0, 1, 0, 0, 0);
	    }

	    if (join('',Delta_YMDHMS(@begin_date, @end_date)) =~ /^0*-/) { 
		@end_date = Add_Delta_YMDHMS(@end_date, 0, 0, 1, 0, 0, 0);
	    }
	    #STOP: END DATE CORRECTION

	    push @events, [$fltno, \@begin_date, \@end_date];
	}
}

my $changes = 0;

print "begin_date, end_date, event, (taplog events used)\n";
for (my $i = 0; $i <= $#{$data{"row"}}; $i++) {
    my @file = @{@{$data{"row"}}[$i]};

    my $chk = 0;
    my @begin_date = ();
    my @end_date = ();

    my @matches = ();
    unless ($ignoreSplit) {
	foreach (@events) {
	    if ($$_[0] eq $file[$eventIndex]) {
		push @matches, $$_[0];
		$chk = 1;
		@begin_date = @{$$_[1]};
		@end_date = @{$$_[2]};
	    }
	}
    }

    #OMG THE CODE REPETITIION!!!
    unless ($chk || $ignoreSplit) {
	foreach (@events) {
	    if ($$_[0] eq substr($file[$eventIndex], 0, length($$_[0]))) {
		push @matches, $$_[0];
		if ($chk) { #Multiple Matches on this one loop
		    if (join('', Delta_YMDHMS(@begin_date, @{$$_[1]})) =~ /^0*-/) { @begin_date = @{$$_[1]}; }
		    if (join('', Delta_YMDHMS(@{$$_[2]}, @end_date)) =~ /^0*-/) { @end_date = @{$$_[2]}; }
		}
		else {
		    $chk = 1;
		    @begin_date = @{$$_[1]};
		    @end_date = @{$$_[2]};
		}
	    }
	}
    }
    unless ($chk || $ignoreSplit) {
	foreach (@events) {
	    if (substr($$_[0],0,length($file[$eventIndex])) eq $file[$eventIndex]) {
		push @matches, $$_[0];
		if ($chk) { #Multiple Matches on this one loop
		    if (join('', Delta_YMDHMS(@begin_date, @{$$_[1]})) =~ /^0*-/) { @begin_date = @{$$_[1]}; }
		    if (join('', Delta_YMDHMS(@{$$_[2]}, @end_date)) =~ /^0*-/) { @end_date = @{$$_[2]}; }
		}
		else {
		    $chk = 1;
		    @begin_date = @{$$_[1]};
		    @end_date = @{$$_[2]};
		}
	    }
	}
    }
    unless ($chk) {
	foreach (@events) {
	    if (substr($$_[0],0,4) eq substr($file[$eventIndex],0,4)) {
		push @matches, $$_[0];
		if ($chk) { #Multiple Matches on this one loop
		    if (join('', Delta_YMDHMS(@begin_date, @{$$_[1]})) =~ /^0*-/) { @begin_date = @{$$_[1]}; }
		    if (join('', Delta_YMDHMS(@{$$_[2]}, @end_date)) =~ /^0*-/) { @end_date = @{$$_[2]}; }
		}
		else {
		    $chk = 1;
		    @begin_date = @{$$_[1]};
		    @end_date = @{$$_[2]};
		}
	    }
	}
    }
    unless ($chk) { "Print Unable to find a match for: " . $file[$filenameIndex] . "\n"; next; }



    #Check to see if dataset DOI needs to be expanded
    if (join('', Delta_YMDHMS(split(/:|-|\s/,$dataset->getBeginDate()), @begin_date)) =~ /^0*-/) {
	$datasetUpdated = 1;
	$dataset->setBeginDate(@begin_date);
    }
    if (join('', Delta_YMDHMS(@end_date, split(/:|-|\s/, $dataset->getEndDate()))) =~ /^0*-/) {
	$datasetUpdated = 1;
	$dataset->setEndDate(@end_date);
    }

    foreach (@begin_date) { if ($_ < 10) { $_ = "0" . ($_+0); } }
    foreach (@end_date) { if ($_ < 10) { $_ = "0" . ($_+0); } }

    my $begin_date = "$begin_date[0]-$begin_date[1]-$begin_date[2] $begin_date[3]:$begin_date[4]:$begin_date[5]";
    my $end_date = "$end_date[0]-$end_date[1]-$end_date[2] $end_date[3]:$end_date[4]:$end_date[5]";

    print "$begin_date, $end_date, $file[$eventIndex], (" . join(',',@matches) . ")\n";

    $changes++;
    $msg = $database->update("file", "begin_date='$begin_date', end_date='$end_date'", "file_id=" . $file[$file_idIndex]);
    if ($msg ne "") { print "$msg\nDatabase Rolled back!\n"; $database->rollback(); $database->disconnect(); exit(1); }
}

if ($datasetUpdated) {
    print "dataset, begin_date='" . $dataset->getBeginDate() . "', end_date='". $dataset->getEndDate() . "'\n";
    $dataset->updateDataset($database);
}

print "$changes changes are ready to be made\n";
if ($#{$data{"row"}}+1 > $changes) { print "WARNING: " . ($#{$data{"row"}}+1-$changes) . " CODIAC files were not updated!"; }
print "\n\n#To update the files, press Enter.\n";
print "#To cancel the update, enter any value and press Enter.\n\n";
print "#>>";
my $result = <STDIN>;
chomp $result;
if ($result eq "")
{
	print $database->commit();
	print "Sucessfully made changes!\n";
}
else
{
    	print "User has selected to cancel, database rolledback!\n";
	print $database->rollback();
}

print $database->disconnect();
exit(0);
