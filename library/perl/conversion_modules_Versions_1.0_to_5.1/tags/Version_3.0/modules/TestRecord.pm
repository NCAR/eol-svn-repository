#! /usr/bin/perl -w

package TestRecord;
use strict;
use lib ".";
use DpgDate;
use Record;
use TestModule;
use Station::Station;
our @ISA = ("TestModule");

&main();

sub main {
    my $tester = TestRecord->new();

    $tester->testNew();
    $tester->testDateTime();
    $tester->testVerbose();

    $tester->testLatitude();
    $tester->testLongitude();
    $tester->testElevation();

    $tester->testStationId();
    $tester->testNetworkId();
    $tester->testCSE();
    $tester->testReferenceSite();

    $tester->testOccurence();
    $tester->testIsSpecial();
}

sub testCSE {
    my $self = shift;
    my $record = Record->new(*STDOUT);

    $self->assertString("CSE",$record->getCSEId(),"cse: default");
    $record->setCSEId("MyCSE");
    $self->assertString("MyCSE",$record->getCSEId(),"cse: value");
}

sub testDateTime {
    my $self = shift;
    my $record = Record->new(*STDOUT);

    $self->assertString("9999/99/99",$record->getActualDate(),"act date: default");
    $self->assertString("99:99",$record->getActualTime(),"act time: default");
    $self->assertString("9999/99/99",$record->getNominalDate(),"nom date: default");
    $self->assertString("99:99",$record->getNominalTime(),"nom time: default");

    $record->setReadingTime("20050215","YYYYMMDD","074236","HHMMSS",6);
    $self->assertString("2005/02/15",$record->getActualDate(),"act date: reading");
    $self->assertString("13:42",$record->getActualTime(),"act time: reading");
    $self->assertString("2005/02/15",$record->getNominalDate(),"nom date: reading");
    $self->assertString("13:42",$record->getNominalTime(),"nom time: reading");

    $record->setNominalDate("16-02-2005","DD-MM-YYYY");
    $record->setNominalTime("14:00:00","HH:MM:SS");
    $self->assertString("2005/02/15",$record->getActualDate(),"act date: nominal");
    $self->assertString("13:42",$record->getActualTime(),"act time: nominal");
    $self->assertString("2005/02/16",$record->getNominalDate(),"nom date: nominal");
    $self->assertString("14:00",$record->getNominalTime(),"nom time: nominal");

    $record->setActualDate("20040623","YYYYMMDD");
    $record->setActualTime("324","HMM");
    $self->assertString("2004/06/23",$record->getActualDate(),"act date: actual");
    $self->assertString("03:24",$record->getActualTime(),"act time: actual");
    $self->assertString("2005/02/16",$record->getNominalDate(),"nom date: nominal");
    $self->assertString("14:00",$record->getNominalTime(),"nom time: nominal");

    $record->setNominalDate("99999999","YYYYMMDD");
    $record->setNominalTime("9999","HHMM");
    $self->assertString("2004/06/23",$record->getActualDate(),"act date: nominal missing");
    $self->assertString("03:24",$record->getActualTime(),"act time: nominal missing");
    $self->assertString("2004/06/23",$record->getNominalDate(),"nom date: nominal missing");
    $self->assertString("03:24",$record->getNominalTime(),"nom time: nominal missing");

    $record->setActualDate("99999999","YYYYMMDD");
    $record->setActualTime("9999","HHMM");
    $self->assertString("9999/99/99",$record->getActualDate(),"act date: actual missing");
    $self->assertString("99:99",$record->getActualTime(),"act time: actual missing");
    $self->assertString("9999/99/99",$record->getNominalDate(),"nom date: actual missing");
    $self->assertString("99:99",$record->getNominalTime(),"nom time: actual missing");
}

sub testElevation {
    my $self = shift;
    my $record = Record->new(*STDOUT);

    $self->assertValue($record->getMissing(),$record->getElevation(),"elevation: default");
    $record->setElevation(1143.2,"m");
    $self->assertValue(1143.2,$record->getElevation(),"elevation: value");
}

sub testIsSpecial {
    my $self = shift;
    my $station = Station::Station->new();
    my $record = Record->new(*STDOUT,$station);
    $record->setReadingTime("20050215","YYYYMMDD","1200","HHMM",0);

    $self->assertValue(0,$record->isSpecial(),"special: default");
    $record->setIsSpecial(1);
    $self->assertValue(1,$record->isSpecial(),"special: set");
    $record->setIsSpecial(undef());
    $self->assertValue(0,$record->isSpecial(),"special: undef");
    
    $station->setReportingFrequency("hourly");
    $self->assertValue(0,$record->isSpecial(),"special: hourly on hour");
    $record->setReadingTime("20050215","YYYYMMDD","1230","HHMM",0);
    $self->assertValue(1,$record->isSpecial(),"special: hourly not on hour");
}

sub testLatitude {
    my $self = shift;
    my $record = Record->new(*STDOUT);
    
    $self->assertValue(-999.99999,$record->getLatitude(),"latitude: default");
    $record->setLatitude("14.32","DDDDD");
    $self->assertValue(14.32,$record->getLatitude(),"latitude: value");
}

sub testLongitude {
    my $self = shift;
    my $record = Record->new(*STDOUT);

    $self->assertValue(-999.99999,$record->getLongitude(),"longitude: default");
    $record->setLongitude("38.32","DDDDD");
    $self->assertValue(38.32,$record->getLongitude(),"longitude: value");
}

sub testNetworkId {
    my $self = shift;
    my $record = Record->new(*STDOUT);
    
    $self->assertString("Network",$record->getNetworkId(),"network: default");
    $record->setNetworkId("MyNet");
    $self->assertString("MyNet",$record->getNetworkId(),"network: default");
}

sub testNew {
    my $self = shift;
    my $station = Station::Station->new("MyId","MyNetwork");
    $station->setCSEId("MyCSE");
    $station->setReferenceSiteId("MyRefSite");
    $station->setLatitude("10.34","DDDDD");
    $station->setLongitude("-103.243","-DDDDDDD");
    $station->setElevation(432,"m");
    my $record = Record->new(*STDOUT,$station);

    $self->assertString("MyId",$record->getStationId(),"new: station id");
    $self->assertString("MyNetwork",$record->getNetworkId(),"new: network id");
    $self->assertString("MyCSE",$record->getCSEId(),"new: cse id");
    $self->assertString("MyRefSite",$record->getReferenceSiteId(),"new: ref site id");
    $self->assertValue(10.34,$record->getLatitude(),"new: latitude");
    $self->assertValue(-103.243,$record->getLongitude(),"new: longitude");
    $self->assertValue(432,$record->getElevation(),"new: elevation");
}

sub testOccurence {
    my $self = shift;
    my $record = Record->new(*STDOUT);

    $self->assertValue(0,$record->getOccurence(),"occurence: default");
    $record->setOccurence(1);
    $self->assertValue(1,$record->getOccurence(),"occurence: value");
}

sub testReferenceSite {
    my $self = shift;
    my $record = Record->new(*STDOUT);

    $self->assertString("Ref_Site",$record->getReferenceSiteId(),"ref site: default");
    $record->setReferenceSiteId("MyRefSite");
    $self->assertString("MyRefSite",$record->getReferenceSiteId(),"ref site: value");
}

sub testStationId {
    my $self = shift;
    my $record = Record->new(*STDOUT);

    $self->assertString("Station",$record->getStationId(),"station id: default");
    $record->setStationId("MyStn");
    $self->assertString("MyStn",$record->getStationId(),"station id: value");
}

sub testVerbose {
    my $self = shift;
    my $record = Record->new(*STDOUT);

    $self->assertValue(1,$record->getVerbose(),"verbose: default");
    $record->setVerbose(0);
    $self->assertValue(0,$record->getVerbose(),"verbose: value");
}

1;
