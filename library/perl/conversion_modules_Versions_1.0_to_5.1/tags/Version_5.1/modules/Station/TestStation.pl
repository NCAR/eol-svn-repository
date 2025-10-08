#! /usr/bin/perl -w

package TestStation;
use strict;
use lib "..";
use TestModule;
use Station::Station;
our @ISA = ("TestModule");

&main();

sub main {
    my $tester = TestStation->new();

    $tester->testNew();
    
    $tester->testBeginDate();
    $tester->testEndDate();
    $tester->testInsertDate();

    $tester->testCSEId();
    $tester->testStationId();
    $tester->testReferenceSiteId();
    $tester->testStationName();

    $tester->testNetworkName();
    $tester->testNetworkIdNumber();
    $tester->testPlatformIdNumber();
    $tester->testReportingFrequency();

    $tester->testElevation();
    $tester->testLatitude();
    $tester->testLongitude();
    $tester->testLatLongAccuracy();
    $tester->testStateCode();

    $tester->testCountry();
    $tester->testCountyCode();
    $tester->testDaylightSavings();
    $tester->testIsCommissioned();
    $tester->testMobilityFlag();
    $tester->testOccurence();
    $tester->testUTC_Offset();

    $tester->testToString();
}

sub testBeginDate {
    my $self = shift;
    my $station = Station::Station->new();

    $self->assertString("99999999",$station->getBeginDate(),"beginDate: default");
    $station->setBeginDate("2004/05/12","YYYY/MM/DD");
    $self->assertString("20040512",$station->getBeginDate(),"beginDate: YYYY/MM/DD");
    $station->setBeginDate("12-13-2003","MM-DD-YYYY");
    $self->assertString("20031213",$station->getBeginDate(),"beginDate: MM-DD-YYYY");
}

sub testCountry {
    my $self = shift;
    my $station = Station::Station->new();

    $self->assertString("US",$station->getCountry(),"country: default");
    $station->setCountry("MX");
    $self->assertString("MX",$station->getCountry(),"country: new value");
}

sub testCountyCode {
    my $self = shift;
    my $station = Station::Station->new();

    $self->assertString("???",$station->getCountyCode(),"county code: default");
    $station->setCountyCode(11);
    $self->assertString("11",$station->getCountyCode(),"county code: new value");
}

sub testCSEId {
    my $self = shift;
    my $station = Station::Station->new();

    $self->assertString("CSE",$station->getCSEId(),"CSE: default");
    $station->setCSEId("MyCSE");
    $self->assertString("MyCSE",$station->getCSEId(),"CSE: new value");
}

sub testDaylightSavings {
    my $self = shift;
    my $station = Station::Station->new();

    $self->assertString("n",$station->getDaylightSavingsSwitch(),"daylight: default");
    $station->setDaylightSavingsSwitch("y");
    $self->assertString("y",$station->getDaylightSavingsSwitch(),"daylight: new value");
}

sub testElevation {
    my $self = shift;
    my $station = Station::Station->new();

    $self->assertValue(-9999.9,$station->getElevation(),"elevation: default");
    $station->setElevation(111.23,"m");
    $self->assertValue(111.23,$station->getElevation(),"elevation: meters");
    $station->setElevation(62,"ft");
    $self->assertValue(18.8976,$station->getElevation(),"elevation: feet");
}

sub testEndDate {
    my $self = shift;
    my $station = Station::Station->new();

    $self->assertString("00000000",$station->getEndDate(),"endDate: default");
    $station->setEndDate("2004/05/12","YYYY/MM/DD");
    $self->assertString("20040512",$station->getEndDate(),"endDate: YYYY/MM/DD");
    $station->setEndDate("12-13-2003","MM-DD-YYYY");
    $self->assertString("20031213",$station->getEndDate(),"endDate: MM-DD-YYYY");
}

sub testInsertDate {
    my $self = shift;
    my $station = Station::Station->new();

    $self->assertString("99999999",$station->getBeginDate(),"insertDate: begin default");
    $self->assertString("00000000",$station->getEndDate(),"insertDate: end default");
    $station->insertDate("2004/05/12","YYYY/MM/DD");
    $self->assertString("20040512",$station->getBeginDate(),"insertDate: begin YYYY/MM/DD");
    $self->assertString("20040512",$station->getEndDate(),"insertDate: end YYYY/MM/DD");
    $station->insertDate("20041224","YYYYMMDD");
    $self->assertString("20040512",$station->getBeginDate(),"insertDate: begin no change");
    $self->assertString("20041224",$station->getEndDate(),"insertDate: end changed");
    $station->insertDate("2003-12-04","YYYY-DD-MM");
    $self->assertString("20030412",$station->getBeginDate(),"insertDate: begin changed");
    $self->assertString("20041224",$station->getEndDate(),"insertDate: end no change");    
}

sub testIsCommissioned {
    my $self = shift;
    my $station = Station::Station->new();

    $self->assertString("(N)",$station->getIsCommissioned(),"commissioned: default");
    $station->setIsCommissioned("Y");
    $self->assertString("(Y)",$station->getIsCommissioned(),"commissioned: new value");
}

sub testLatitude {
    my $self = shift;
    my $station = Station::Station->new();
    
    $self->assertValue(-99.99999,$station->getLatitude(),"latitude: default");
    $station->setLatitude("10.384","DDDDDD");
    $self->assertValue(10.384,$station->getLatitude(),"latitude: D");
    $station->setLatitude("-10 30 00","-DD MM SS");
    $self->assertValue(-10.5,$station->getLatitude(),"latitude: -DMS")
}

sub testLatLongAccuracy {
    my $self = shift;
    my $station = Station::Station->new();
    
    $self->assertValue(1,$station->getLatLongAccuracy(),"accuracy: default");
    $station->setLatLongAccuracy(3);
    $self->assertValue(3,$station->getLatLongAccuracy(),"accuracy: new value");
}

sub testLongitude {
    my $self = shift;
    my $station = Station::Station->new();
    
    $self->assertValue(-999.99999,$station->getLongitude(),"longitude: default");
    $station->setLongitude("10.384","DDDDDD");
    $self->assertValue(10.384,$station->getLongitude(),"longitude: D");
    $station->setLongitude("-10 30 00","-DD MM SS");
    $self->assertValue(-10.5,$station->getLongitude(),"longitude: -DMS")
}

sub testMobilityFlag {
    my $self = shift;
    my $station = Station::Station->new();
    
    $self->assertString("f",$station->getMobilityFlag(),"mobility: default");
    $station->setMobilityFlag("m");
    $self->assertString("m",$station->getMobilityFlag(),"mobility: mobile");
}

sub testNetworkIdNumber {
    my $self = shift;
    my $station = Station::Station->new();

    $self->assertValue(-1,$station->getNetworkIdNumber(),"net id number: default");
    $station->setNetworkIdNumber(11);
    $self->assertValue(11,$station->getNetworkIdNumber(),"net id number: new value");
}

sub testNetworkName {
    my $self = shift;
    my $station = Station::Station->new();

    $self->assertString("Network",$station->getNetworkName(),"network: default");
    $station->setNetworkName("MyNetName");
    $self->assertString("MyNetName",$station->getNetworkName(),"network: new value");
}

sub testNew {
    my $self = shift;
    my $station;

    $station = Station::Station->new();
    $self->assertValue(1,$station->getVerbose(),"New(): verbose");
    $self->assertString("Station",$station->getStationId(),"New(): station id");
    $self->assertString("Network",$station->getNetworkName(),"New(): network");

    $station = Station::Station->new(0);
    $self->assertValue(0,$station->getVerbose(),"New(0): verbose");
    $self->assertString("Station",$station->getStationId(),"New(0): station id");
    $self->assertString("Network",$station->getNetworkName(),"New(0): network");

    $station = Station::Station->new("MyStation","MyNetwork");
    $self->assertValue(1,$station->getVerbose(),"New(station,network): verbose");
    $self->assertString("MyStation",$station->getStationId(),
			"New(station,network): station id");
    $self->assertString("MyNetwork",$station->getNetworkName(),
			"New(station,network): network");

    $station = Station::Station->new("MyStation","MyNetwork",0);
    $self->assertValue(0,$station->getVerbose(),
		       "New(station,network,verbose): verbose");
    $self->assertString("MyStation",$station->getStationId(),
			"New(station,network,verbose): station id");
    $self->assertString("MyNetwork",$station->getNetworkName(),
			"New(station,network,verbose): network");

    $station = Station::Station->new("MyStation","MyRefId","MyCSE",0);
    $self->assertValue(0,$station->getVerbose(),
		       "New(station,refid,cse,verbose): verbose");
    $self->assertString("MyStation",$station->getStationId(),
			"New(station,refid,cse,verbose): station id");
    $self->assertString("MyRefId",$station->getReferenceSiteId(),
			"New(station,refid,cse,verbose): ref site");
    $self->assertString("MyCSE",$station->getCSEId(),
			"New(station,refid,cse,verbose): cse id");
}

sub testOccurence {
    my $self = shift;
    my $station = Station::Station->new();

    $self->assertValue(0,$station->getOccurence(),"occurence: default");
    $station->setOccurence(8);
    $self->assertValue(8,$station->getOccurence(),"occurence: new value");
}

sub testPlatformIdNumber {
    my $self = shift;
    my $station = Station::Station->new();

    $self->assertValue(-1,$station->getPlatformIdNumber(),"platform id: default");
    $station->setPlatformIdNumber(11);
    $self->assertValue(11,$station->getPlatformIdNumber(),"platform id: new value");
}

sub testReferenceSiteId {
    my $self = shift;
    my $station = Station::Station->new();

    $self->assertString("Ref_Site",$station->getReferenceSiteId(),"ref site id: default");
    $station->setReferenceSiteId("MyRefId");
    $self->assertString("MyRefId",$station->getReferenceSiteId(),"ref site id: new value");
}

sub testReportingFrequency {
    my $self = shift;
    my $station = Station::Station->new();

    $self->assertString("xxxxxx",$station->getReportingFrequency(),"report freq: default");
    $station->setReportingFrequency("hourly");
    $self->assertString("hourly",$station->getReportingFrequency(),"report freq: new value");
}

sub testStateCode {
    my $self = shift;
    my $station = Station::Station->new();

    $self->assertValue(99,$station->getStateCode(),"state code: default");
    $station->setStateCode("CO");
    $self->assertValue(8,$station->getStateCode(),"state code: colorado");
	$station->setStateCode("MN","US");
	$self->assertValue(27,$station->getStateCode(),"state code: minnesota");
}

sub testStationId {
    my $self = shift;
    my $station = Station::Station->new();

    $self->assertString("Station",$station->getStationId(),"stationID: default");
    $station->setStationId("MyStationId");
    $self->assertString("MyStationId",$station->getStationId(),"stationID: new value");
}

sub testStationName {
    my $self = shift;
    my $station = Station::Station->new();

    $self->assertString("Description",$station->getStationName(),"station name: default");
    $station->setStationName("NewStationName");
    $self->assertString("NewStationName",$station->getStationName(),"station name: new value");
}

sub testToString {
    my $self = shift;
    my $station = Station::Station->new();

    $self->assertString("Station           -1  -99.99999  -999.99999   0     1 Description                                    (N) 99999999 00000000 US 99 ???   0.00 n   -1 xxxxxx            -9999.9 f\n",$station->toString(),"toString: default");
    $station->setStationId("NewStation");
    $station->setStationName("Somewhere Outthere");
    $station->setLatitude(42.31,"DDDDD");
    $station->setLongitude(-93.22,"-DDDDD");
    $station->setElevation(313,"m");
    $station->insertDate("20050210","YYYYMMDD");
    $station->setReportingFrequency("1 minute");
    $station->setNetworkIdNumber(11);
    $station->setPlatformIdNumber(312);
    $station->setStateCode("CO");
    $self->assertString("NewStation        11   42.31000   -93.22000   0     1 Somewhere Outthere                             (N) 20050210 20050210 US 08 ???   0.00 n  312 1 minute            313.0 f\n",$station->toString(),"toString: values");
}

sub testUTC_Offset {
    my $self = shift;
    my $station = Station::Station->new();

    $self->assertValue(0,$station->getUTC_Offset(),"utc offset: default");
    $station->setUTC_Offset(42);
    $self->assertValue(42,$station->getUTC_Offset(),"utc offset: default");
}
