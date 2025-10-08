#! /usr/bin/perl -w

package TestRecordMap;
use strict;
use lib ".";
use TestModule;
use RecordMap;
use Record;
our @ISA = ("TestModule");

&main();

my $WARNING;

sub main {
    my $tester = TestRecordMap->new();
    open($WARNING, ">recordmap.warn");

    my $records = RecordMap->new();

    my @list = buildRecords();

    foreach my $rec (@list) {
	$records->addRecord($rec);
    }

    $tester->assertValue(4, scalar($records->getAllRecords()), "Record Count");
    $tester->assertValue(1, scalar($records->getDuplicateRecords()),
			 "Duplicate Record Count");

    $tester->assertValue(1, $records->hasRecord("Station", "Network",
						"2004/07/14", "15:00"),
			 "Has Record");
    $tester->assertValue(0, $records->hasRecord("Station", "Network",
						"2004/07/14", "15:01"),
			 "Doesn't Have Record");

    $tester->assertString("2004/07/14", $records->getRecord("Station", 
							    "Network",
							    "2004/07/14",
							    "15:00")->
			  getNominalDate(), "Get Record");
    
    $records->clear();
    $tester->assertValue(0, scalar($records->getAllRecords()),
			 "Clear Records");
    $tester->assertValue(0, scalar($records->getDuplicateRecords()),
			 "Clear Dupes");


    close($WARNING);
    unlink("recordmap.warn");
}

sub buildRecords {
    my @list = ();

    my $record = Record->new($WARNING);
    $record->setReadingTime("2004/07/14","YYYY/MM/DD","15:00","HH:MM",0,0);
    push(@list, $record);

    $record = Record->new($WARNING);
    $record->setReadingTime("2004/07/14","YYYY/MM/DD","15:00","HH:MM",0,0);
    push(@list, $record);

    $record = Record->new($WARNING);
    $record->setReadingTime("2004/07/14","YYYY/MM/DD","16:00","HH:MM",0,0);
    push(@list, $record);

    $record = Record->new($WARNING);
    $record->setReadingTime("2004/07/14","YYYY/MM/DD","17:00","HH:MM",0,0);
    push(@list, $record);

    $record = Record->new($WARNING);
    $record->setReadingTime("2004/07/14","YYYY/MM/DD","18:00","HH:MM",0,0);
    push(@list, $record);

    return @list;
}
