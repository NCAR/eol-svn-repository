#! /usr/bin/perl -w

package Precip::TestPrecip15minRecord;
use strict;
use lib "..";
use DpgConstants qw(:DEFAULT);
use DpgDate qw(:DEFAULT);
use Precip15minRecord;
use Station::Station;
use Surface::QCFConstants;
use TestModule;
our @ISA = ("TestModule");

&main();

sub main {
    my $tester = Precip::TestPrecip15minRecord->new();
    my $record = Precip::Precip15minRecord->new(*STDOUT);

    $tester->testDefaultValues($record);
    $tester->testMetadata($record);
    $tester->testGettersSetters($record);
    $tester->testToString($record);
    $tester->testIsNominal($record);
}

sub testDefaultValues {
    my ($self,$record) = @_;

    $self->assertString("9999/99/99",$record->getDate(),"Default Date");
    $self->assertString("00:00:00",$record->getTime(),"Default Time");
    $self->assertString("Network",$record->getNetworkId(),"Default Network");
    $self->assertString("Station",$record->getStationId(),"Default Station");
    $self->assertValue(-999.99999,$record->getLatitude(),"Default Latitude");
    $self->assertValue(-999.99999,$record->getLongitude(),"Default Longitude");
    $self->assertValue(0,$record->getOccurence(),"Default Occurence");

    my ($date,$time) = ("2005/05/05","00:00");
    while ($date eq "2005/05/05") {
	$self->assertValue($MISSING,$record->getPrecip($time,"HH:MM"),
			   "Default Value $time");
	$self->assertValue($PRECIP_MISSING_CODE,$record->getPrecipFlag($time,"HH:MM"),
			   "Default Precip Flag $time");
	$self->assertString($MISSING_FLAG,$record->getQCFFlag($time,"HH:MM"),
			    "Default QCF Flag $time");

	($date,$time) = adjustDateTime($date,"YYYY/MM/DD",$time,"HH:MM",0,0,15,0);
    }
}

sub testGettersSetters {
    my ($self,$record) = @_;
    $record->setReadingTime("20050505","YYYYMMDD");

    my ($date,$time) = ($record->getDate(),"00:00");
    while ($date eq $record->getDate()) {

	$self->assertValue($MISSING,$record->getPrecip($time,"HH:MM"));
	$self->assertString($MISSING_FLAG,$record->getQCFFlag($time,"HH:MM"));

	my ($hour,$min) = split(/:/,$time);
	$record->setPrecip($time,"HH:MM",$hour + $min/100, "mm");

	$self->assertValue($hour + $min/100,$record->getPrecip($time,"HH:MM"),"Set Value");
	$self->assertValue($PRECIP_NO_QUALIFIER_CODE,$record->getPrecipFlag($time,"HH:MM"),
			   "Default Set Value Flag");
	$self->assertString($UNCHECKED_FLAG,$record->getQCFFlag($time,"HH:MM"),"Default Set Value QCF");

	$record->setPrecipFlag($time,"HH:MM",$min % 4);
	$record->setQCFFlag($time,"HH:MM",$BAD_FLAG);

	$self->assertValue($min % 4,$record->getPrecipFlag($time,"HH:MM"),"Manual Set Precip Flag");
	$self->assertString($BAD_FLAG,$record->getQCFFlag($time,"HH:MM"),"Manual Set QCF Flag");


	($date,$time) = adjustDateTime($date,"YYYY/MM/DD",$time,"HH:MM",0,0,15,0);
    }
}

sub testIsNominal {
    my ($self,$record) = @_;

    for (my $i = 0; $i < 60; $i++) {
	$self->assertValue($i % 15 ? $FALSE : $TRUE,
			   $record->isNominalTime(sprintf("%04d",$i),"HHMM"),
			   "isNominal($i)");
    }
}

sub testMetadata {
    my ($self,$record) = @_;
    my $station = Station::Station->new("NewStn","NewNet");
    $station->setLatitude(45,"DD");
    $station->setLongitude(-33,"-DD");

    $record = Precip::Precip15minRecord->new(*STDOUT,$station);
    $self->assertString("9999/99/99",$record->getDate(),"Station Date");
    $self->assertString("00:00:00",$record->getTime(),"Station Time");
    $self->assertString("NewNet",$record->getNetworkId(),"Station Network");
    $self->assertString("NewStn",$record->getStationId(),"Station Station");
    $self->assertValue(45,$record->getLatitude(),"Station Latitude");
    $self->assertValue(-33,$record->getLongitude(),"Station Longitude");
    $self->assertValue(0,$record->getOccurence(),"Station Occurence");

    $record->setReadingTime("20050505","YYYYMMDD",0,23,0);
    $self->assertString("2005/05/05",$record->getDate(),"Set Date");
    $self->assertString("00:23:00",$record->getTime(),"Set Time");

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
    
    $self->assertString("2005/05/05 00:00:00 Network    Station         -999.99999  -999.99999   0    0.00 0 B    0.15 3 B    0.30 2 B    0.45 1 B    1.00 0 B    1.15 3 B    1.30 2 B    1.45 1 B    2.00 0 B    2.15 3 B    2.30 2 B    2.45 1 B    3.00 0 B    3.15 3 B    3.30 2 B    3.45 1 B    4.00 0 B    4.15 3 B    4.30 2 B    4.45 1 B    5.00 0 B    5.15 3 B    5.30 2 B    5.45 1 B    6.00 0 B    6.15 3 B    6.30 2 B    6.45 1 B    7.00 0 B    7.15 3 B    7.30 2 B    7.45 1 B    8.00 0 B    8.15 3 B    8.30 2 B    8.45 1 B    9.00 0 B    9.15 3 B    9.30 2 B    9.45 1 B   10.00 0 B   10.15 3 B   10.30 2 B   10.45 1 B   11.00 0 B   11.15 3 B   11.30 2 B   11.45 1 B   12.00 0 B   12.15 3 B   12.30 2 B   12.45 1 B   13.00 0 B   13.15 3 B   13.30 2 B   13.45 1 B   14.00 0 B   14.15 3 B   14.30 2 B   14.45 1 B   15.00 0 B   15.15 3 B   15.30 2 B   15.45 1 B   16.00 0 B   16.15 3 B   16.30 2 B   16.45 1 B   17.00 0 B   17.15 3 B   17.30 2 B   17.45 1 B   18.00 0 B   18.15 3 B   18.30 2 B   18.45 1 B   19.00 0 B   19.15 3 B   19.30 2 B   19.45 1 B   20.00 0 B   20.15 3 B   20.30 2 B   20.45 1 B   21.00 0 B   21.15 3 B   21.30 2 B   21.45 1 B   22.00 0 B   22.15 3 B   22.30 2 B   22.45 1 B   23.00 0 B   23.15 3 B   23.30 2 B   23.45 1 B\n",$record->toString(),"Test ToString");
}





