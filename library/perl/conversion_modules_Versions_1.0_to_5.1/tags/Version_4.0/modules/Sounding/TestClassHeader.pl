#! /usr/bin/perl -w

package Sounding::TestClassHeader;
use strict;
use lib "..";
use TestModule;
use Sounding::ClassHeader;
our @ISA = ("TestModule");

&main();

sub main {
    my $tester = Sounding::TestClassHeader->new();

    $tester->testId();
    $tester->testProject();
    $tester->testSite();
    $tester->testType();

    $tester->testActual();
    $tester->testNominal();

    $tester->testLongitude();
    $tester->testLatitude();
    $tester->testAltitude();

    $tester->testVariableParams();

    $tester->testToString();
}

sub testActual {
    my $self = shift;
    my $header = Sounding::ClassHeader->new();

    $self->assertString("9999, 99, 99",$header->getActualDate(),
			"actual date: default");
    $self->assertString("99:99:99",$header->getActualTime(),
			"actual time: default");
    $header->setNominalRelease("20050214","YYYYMMDD","120000","HHMMSS",0);
    $self->assertString("2005, 02, 14",$header->getActualDate(),
			"actual date: nominal set");
    $self->assertString("12:00:00",$header->getActualTime(),
			"actual time: nominal set");
    $header->setActualRelease("20041219","YYYYMMDD","172833","HHMMSS",0);
    $self->assertString("2004, 12, 19",$header->getActualDate(),
			"actual date: actual set");
    $self->assertString("17:28:33",$header->getActualTime(),
			"actual time: actual set");
    
}

sub testAltitude {
    my $self = shift;
    my $header = Sounding::ClassHeader->new();

    $self->assertFormatString("999.0",$header->getAltitude(),"altitude: default");
    $header->setAltitude(112.2,"m");
    $self->assertFormatString("112.2",$header->getAltitude(),"altitude: value");
}

sub testId {
    my $self = shift;
    my $header = Sounding::ClassHeader->new();

    $self->assertString("Station",$header->getId(),"id: default");
    $header->setId("MyID");
    $self->assertString("MyID",$header->getId(),"id: value");
}

sub testLatitude {
    my $self = shift;
    my $header = Sounding::ClassHeader->new();

    $self->assertValue(99.0,$header->getLatitude(),"latitude: default");
    $header->setLatitude("12.324","DDDDDD");
    $self->assertValue(12.324,$header->getLatitude(),"latitude: value");
}

sub testLongitude {
    my $self = shift;
    my $header = Sounding::ClassHeader->new();

    $self->assertValue(999.0,$header->getLongitude(),"longitude: default");
    $header->setLongitude("-87 30","-DD MM");
    $self->assertValue(-87.5,$header->getLongitude(),"longitude: value");
}

sub testNew {
    my $self = shift;
    my $station = Station::Station->new();
    my $header = Sounding::ClassSounding->new($station);

    $self->assertString("Station",$header->getId(),"new: id default");
    $self->assertString("Station Description",$header->getSite(),"new: site default");
    $self->assertValue(99.0,$header->getLatitude(),"new: latitude default");
    $self->assertValue(999.0,$header->getLongitude(),"new: longitude default");
    $self->assertValue(999.0,$header->getAltitude(),"new: altitude default");

    $station->setStationId("MyID");
    $station->setStationName("Somewhere Overhere");
    $station->setLatitude("32.34","DDDDD");
    $station->setLongitude("-103.48","-DDDDDD");
    $station->setElevation(423,"m");

    $self->assertString("MyID",$header->getId(),"new: id value");
    $self->assertString("MyID Somewhere Overhere",$header->getSite(),"new: site value");
    $self->assertValue(32.34,$header->getLatitude(),"new: latitude value");
    $self->assertValue(-103.48,$header->getLongitude(),"new: longitude value");
    $self->assertValue(423.0,$header->getAltitude(),"new: altitude value");
}

sub testNominal {
    my $self = shift;
    my $header = Sounding::ClassHeader->new();

    $self->assertString("9999, 99, 99",$header->getNominalDate(),
			"nominal date: default");
    $self->assertString("99:99:99",$header->getNominalTime(),
			"nominal time: default");
    $header->setNominalRelease("20050214","YYYYMMDD","120000","HHMMSS",0);
    $self->assertString("2005, 02, 14",$header->getNominalDate(),
			"nominal date: nominal set");
    $self->assertString("12:00:00",$header->getNominalTime(),
			"nominal time: nominal set");
}

sub testProject {
    my $self = shift;
    my $header = Sounding::ClassHeader->new();
    
    $self->assertString("",$header->getProject(),"project: default");
    $header->setProject("NAME");
    $self->assertString("NAME",$header->getProject(),"project: new value");
}

sub testSite {
    my $self = shift;
    my $header = Sounding::ClassHeader->new();

    $self->assertString("Station Description",$header->getSite(),"site: default");
    $header->setSite("MySiteLocation");
    $self->assertString("MySiteLocation",$header->getSite(),"site: value");
}

sub testToString {
    my $self = shift;
    my $header = Sounding::ClassHeader->new();
    $header->setId("MyId");
    $header->setSite("Somewhere");
    $header->setType("Sounding Type");
    $header->setProject("This Project");
    $header->setLatitude("32.45","DDDDD");
    $header->setLongitude("-102.34","-DDDDDD");
    $header->setAltitude(111.1,"m");
    $header->setActualRelease("20050214","YYYYMMDD","113456","HHMMSS",0);
    $header->setNominalRelease("20050214","YYYYMMDD","12","HH",0);
    $header->setLine("Radiosonde Serial Number:","84683764");
    $header->setLine("Radiosonde Manufacturer:","Maker");

    $self->assertString("Data Type:                         Sounding Type\nProject ID:                        This Project\nRelease Site Type/Site ID:         Somewhere\nRelease Location (lon,lat,alt):    102 20.40'W, 32 27.00'N, -102.340, 32.450, 111.1\nUTC Release Time (y,m,d,h,m,s):    2005, 02, 14, 11:34:56\nRadiosonde Manufacturer:           Maker\nRadiosonde Serial Number:          84683764\n/\n/\n/\n/\nNominal Release Time (y,m,d,h,m,s):2005, 02, 14 12:00:00\n Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat    Ele   Azi   Alt    Qp   Qt   Qrh  Qu   Qv   QdZ\n  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg    deg   deg    m    code code code code code code\n------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----\n",$header->toString(),"toString test");
}

sub testType {
    my $self = shift;
    my $header = Sounding::ClassHeader->new();

    $self->assertString("",$header->getType(),"type: default");
    $header->setType("Sounding Data");
    $self->assertString("Sounding Data",$header->getType(),"type: new value");
}

sub testVariableParams {
    my $self = shift;
    my $header = Sounding::ClassHeader->new();

    $self->assertString("Ele",$header->getVariableParameterName(1),
			"var 1 name: default");
    $self->assertString("Azi",$header->getVariableParameterName(2),
			"var 2 name: default");
    $self->assertString("deg",$header->getVariableParameterUnit(1),
			"var 1 unit: default");
    $self->assertString("deg",$header->getVariableParameterUnit(2),
			"var 2 unit: default");
    $header->setVariableParameter(1,"Rng","km");
    $header->setVariableParameter(2,"Smt","drt");
    $self->assertString("Rng",$header->getVariableParameterName(1),
			"var 1 name: value");
    $self->assertString("Smt",$header->getVariableParameterName(2),
			"var 2 name: value");
    $self->assertString("km",$header->getVariableParameterUnit(1),
			"var 1 unit: value");
    $self->assertString("drt",$header->getVariableParameterUnit(2),
			"var 2 unit: value");
}
