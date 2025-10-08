#! /usr/bin/perl -w

package Precip::TestPrecipHourlyRecord;
use strict;
use lib "..";
use DpgConstants qw(:DEFAULT);
use DpgDate qw(:DEFAULT);
use Precip::PrecipHourlyRecord;
use Station::Station;
use Surface::QCFConstants;
use TestModule;
our @ISA = ("TestModule");

&main();

sub main {
    my $tester = Precip::TestPrecipHourlyRecord->new();
    my $record = Precip::PrecipHourlyRecord->new(*STDOUT);

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
	$record->setPrecip($time,"HH:MM",$hour, "mm");

	$self->assertValue($hour,$record->getPrecip($time,"HH:MM"),"Set Value");
	$self->assertValue($PRECIP_NO_QUALIFIER_CODE,$record->getPrecipFlag($time,"HH:MM"),
			   "Default Set Value Flag");
	$self->assertString($UNCHECKED_FLAG,$record->getQCFFlag($time,"HH:MM"),
			    "Default Set Value QCF");

	$record->setPrecipFlag($time,"HH:MM",$min % 4);
	$record->setQCFFlag($time,"HH:MM",$BAD_FLAG);

	$self->assertValue($min % 4,$record->getPrecipFlag($time,"HH:MM"),
			   "Manual Set Precip Flag");
	$self->assertString($BAD_FLAG,$record->getQCFFlag($time,"HH:MM"),
			    "Manual Set QCF Flag");


	($date,$time) = adjustDateTime($date,"YYYY/MM/DD",$time,"HH:MM",0,1,0,0);
    }
}

sub testIsNominal {
    my ($self,$record) = @_;

    for (my $i = 0; $i < 1440; $i++) {
	my $hour = $i / 60;
	my $min = $i - $hour * 60;
	$self->assertValue($min % 60 ? $FALSE : $TRUE,
			   $record->isNominalTime(sprintf("%02d%02d",$hour,$min),"HHMM"),
			   "isNominal($i)");
    }
}

sub testMetadata {
    my ($self,$record) = @_;
    my $station = Station::Station->new("NewStn","NewNet");
    $station->setLatitude(45,"DD");
    $station->setLongitude(-33,"-DD");

    $record = Precip::PrecipHourlyRecord->new(*STDOUT,$station);
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
    
    $self->assertString("2005/05/05 00:00:00 Network    Station         -999.99999  -999.99999   0    0.00 0 B    1.00 0 B    2.00 0 B    3.00 0 B    4.00 0 B    5.00 0 B    6.00 0 B    7.00 0 B    8.00 0 B    9.00 0 B   10.00 0 B   11.00 0 B   12.00 0 B   13.00 0 B   14.00 0 B   15.00 0 B   16.00 0 B   17.00 0 B   18.00 0 B   19.00 0 B   20.00 0 B   21.00 0 B   22.00 0 B   23.00 0 B\n",$record->toString(),"Test toString");
}











