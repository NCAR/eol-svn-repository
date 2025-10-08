#! /usr/bin/perl -w

package TestPrecipDailyRecord;
use strict;
use lib "/net/work/lib/perl/Utilities";
use lib "/net/work/lib/perl/Station";
use lib "/net/work/lib/perl/Surface";
use DpgConstants qw(:DEFAULT);
use DpgDate qw(:DEFAULT);
use PrecipDailyRecord;
use Station;
use QCFConstants;
use TestModule;
our @ISA = ("TestModule");

&main();

sub main {
    my $tester = TestPrecipDailyRecord->new();
    my $record = PrecipDailyRecord->new(*STDOUT);

    $tester->testDefaultValues($record);
    $tester->testMetadata($record);
    $tester->testGettersSetters($record);
    $tester->testToString($record);
}

sub testDefaultValues {
    my ($self,$record) = @_;

    $self->assertString("9999/99",$record->getDate(),"Default Date");
    $self->assertString("Network",$record->getNetworkId(),"Default Network");
    $self->assertString("Station",$record->getStationId(),"Default Station");
    $self->assertValue(-99.99999,$record->getLatitude(),"Default Latitude");
    $self->assertValue(-999.99999,$record->getLongitude(),"Default Longitude");
    $self->assertValue(0,$record->getOccurence(),"Default Occurence");

    for (my $i = 1; $i <= 31; $i++) {
	$self->assertValue($MISSING,$record->getPrecip($i),
			   "Default Value $i");
	$self->assertValue($PRECIP_MISSING_CODE,$record->getPrecipFlag($i),
			   "Default Precip Flag $i");
	$self->assertString($MISSING_FLAG,$record->getQCFFlag($i),
			    "Default QCF Flag $i");
    }
}

sub testGettersSetters {
    my ($self,$record) = @_;
    $record->setDate("20050505","YYYYMMDD");

    for (my $i = 1; $i <= 31; $i++) {
	$self->assertValue($MISSING,$record->getPrecip($i));
	$self->assertString($MISSING_FLAG,$record->getQCFFlag($i));
	$self->assertValue(99,$record->getObservationHour($i));

	$record->setPrecip($i,$i % 24,$i,"mm");
	$self->assertValue($i,$record->getPrecip($i),"Set Value");
	$self->assertValue($PRECIP_NO_QUALIFIER_CODE,$record->getPrecipFlag($i),
			   "Default Set Value Flag");
	$self->assertString($UNCHECKED_FLAG,$record->getQCFFlag($i),
			    "Default Set Value QCF");
	$self->assertString($i % 24,$record->getObservationHour($i),
			    "Set Observation Hour");

	$record->setPrecipFlag($i,$i % 4);
	$record->setQCFFlag($i,$BAD_FLAG);

	$self->assertValue($i % 4,$record->getPrecipFlag($i),
			   "Manual Set Precip Flag");
	$self->assertString($BAD_FLAG,$record->getQCFFlag($i),
			    "Manual Set QCF Flag");
    }
}

sub testMetadata {
    my ($self,$record) = @_;
    my $station = Station->new("NewStn","NewNet");
    $station->setLatitude(45,"DD");
    $station->setLongitude(-33,"-DD");

    $record = PrecipDailyRecord->new(*STDOUT,$station);
    $self->assertString("9999/99",$record->getDate(),"Station Date");
    $self->assertString("NewNet",$record->getNetworkId(),"Station Network");
    $self->assertString("NewStn",$record->getStationId(),"Station Station");
    $self->assertValue(45,$record->getLatitude(),"Station Latitude");
    $self->assertValue(-33,$record->getLongitude(),"Station Longitude");
    $self->assertValue(0,$record->getOccurence(),"Station Occurence");

    $record->setDate("20050505","YYYYMMDD");
    $self->assertString("2005/05",$record->getDate(),"Set Date");

    $record->setNetworkId("MyNet");
    $self->assertString("MyNet",$record->getNetworkId(),"Set Network");
    $record->setStationId("MyStn");
    $self->assertString("MyStn",$record->getStationId(),"Set Station");

    $record->setLatitude(21.4,"DDDD");
    $self->assertValue(21.4,$record->getLatitude(),"Set Latitude");
    $record->setLongitude(-93.421,"-DDDDDD");
    $self->assertValue(-93.421,$record->getLongitude(),"Set Longitude");

    $record->setOccurence(3);
    $self->assertValue(3,$record->getOccurence(),"Set Occurence");
}

sub testToString {
    my ($self,$record) = @_;
    
    $self->assertString("2005/05 Network    Station          -99.99999  -999.99999   0    1.00 1 B  1    2.00 2 B  2    3.00 3 B  3    4.00 0 B  4    5.00 1 B  5    6.00 2 B  6    7.00 3 B  7    8.00 0 B  8    9.00 1 B  9   10.00 2 B 10   11.00 3 B 11   12.00 0 B 12   13.00 1 B 13   14.00 2 B 14   15.00 3 B 15   16.00 0 B 16   17.00 1 B 17   18.00 2 B 18   19.00 3 B 19   20.00 0 B 20   21.00 1 B 21   22.00 2 B 22   23.00 3 B 23   24.00 0 B  0   25.00 1 B  1   26.00 2 B  2   27.00 3 B  3   28.00 0 B  4   29.00 1 B  5   30.00 2 B  6   31.00 3 B  7\n",$record->toString(),"Test toString");
}











