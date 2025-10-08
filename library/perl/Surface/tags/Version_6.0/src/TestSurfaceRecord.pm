#! /usr/bin/perl -w



package TestSurfaceRecord;
use strict;
use lib "/net/work/lib/perl/Utilities";
use TestRecord;
use SurfaceRecord;
our @ISA = ("TestRecord");

&main();

sub main {
    my $tester = TestSurfaceRecord->new();

    $tester->testAltimeter();
    $tester->testCalcSeaLevelPressure();
    $tester->testDewPoint();
    $tester->testGustSpeed();
    $tester->testIncomingLongwave();
    $tester->testIncomingPAR();
    $tester->testIncomingShortwave();
    $tester->testNetRadiation();
    $tester->testOutgoingLongwave();
    $tester->testOutgoingShortwave();
    $tester->testOutgoingPAR();
    $tester->testPrecip();
    $tester->testPresentWeather();
    $tester->testPressure();
    $tester->testRelativeHumidity();
    $tester->testSeaLevelPressure();
    $tester->testSkinTemperature();
    $tester->testSnowDepth();
    $tester->testSpecificHumidity();
    $tester->testTemperature();
    $tester->testVisibility();
    $tester->testWindComponents();
    $tester->testWindSpeedDirection();
}

sub testAltimeter {
    my $self = shift;
    my $record = SurfaceRecord->new(*STDOUT);

    $self->assertValue($record->getMissing(),$record->getAltimeter(),"altimeter: default");
    $record->setAltimeter(45.23,"mbar");
    $self->assertValue(45.23,$record->getAltimeter(),"altimeter: value");
}

sub testCalcSeaLevelPressure {
    my $self = shift;
    my $record = SurfaceRecord->new(*STDOUT);

    $self->assertValue($record->getMissing(),$record->getCalcSeaLevelPressure(),"cslp: default");
    $record->setTemperature(23.5,"C");
    $record->setPressure(983,"mbar");
    $record->setDewPoint(20.1,"C");
    $record->setElevation(723,"m");
    $self->assertValue(1068.240647,$record->getCalcSeaLevelPressure(),"cslp: value");

    $record->setTemperature(undef(),"C");
    $self->assertValue($record->getMissing(),$record->getCalcSeaLevelPressure(),"cslp: no temp");
    $record->setTemperature(23.5,"C");

    $record->setPressure(undef(),"mbar");
    $self->assertValue($record->getMissing(),$record->getCalcSeaLevelPressure(),"cslp: no pressure");
    $record->setPressure(983,"mbar");

    $record->setDewPoint(undef(),"C");
    $self->assertValue($record->getMissing(),$record->getCalcSeaLevelPressure(),"cslp: no dew point");
    $record->setDewPoint(20.1,"C");

    $record->setElevation(undef(),"m");
    $self->assertValue($record->getMissing(),$record->getCalcSeaLevelPressure(),"cslp: no elevation");
    $record->setElevation(723,"m");
    $self->assertValue(1068.240647,$record->getCalcSeaLevelPressure(),"cslp: sanity check");

    $record->setCalcSeaLevelPressure(1013.55,"mbar");
    $self->assertValue(1013.55,$record->getCalcSeaLevelPressure(),"cslp: set manual");
    $record->setCalcSeaLevelPressure($record->getMissing(),"mbar");
    $self->assertValue(1068.240647,$record->getCalcSeaLevelPressure(),"cslp: unset manual, re-calc");
}

sub testDewPoint {
    my $self = shift;
    my $record = SurfaceRecord->new(*STDOUT);

    $self->assertValue($record->getMissing(),$record->getDewPoint(),"dew point: default");
    $record->setTemperature(23.4,"C");
    $record->setRelativeHumidity(83);
    $self->assertValue(20.350332,$record->getDewPoint(),"dew point: calculated");
    
    $record->setTemperature(undef(),"C");
    $self->assertValue($record->getMissing(),$record->getDewPoint(),"dew point: no temp");
    $record->setTemperature(23.4,"C");

    $record->setRelativeHumidity(undef());
    $self->assertValue($record->getMissing(),$record->getDewPoint(),"dew point: no rh");
    $record->setRelativeHumidity(83);
    $self->assertValue(20.350332,$record->getDewPoint(),"dew point: sanity check");

    $record->setDewPoint(76.23,"C");
    $self->assertValue(76.23,$record->getDewPoint(),"dew point: manual set");
    $record->setDewPoint($record->getMissing(),"C");
    $self->assertValue(20.350332,$record->getDewPoint(),"dew point: set missing, re-calc");
}

sub testGustSpeed {
    my $self = shift;
    my $record = SurfaceRecord->new(*STDOUT);

    $self->assertValue($record->getMissing(),$record->getGustSpeed(),"gust: default");
    $record->setGustSpeed(13.2,"m/s");
    $self->assertValue(13.2,$record->getGustSpeed(),"gust: set value");
    $record->setGustSpeed($record->getMissing(),"m/s");
    $self->assertValue($record->getMissing(),$record->getGustSpeed(),"gust: set missing");
}

sub testIncomingShortwave {
    my $self = shift;
    my $record = SurfaceRecord->new(*STDOUT);

    $self->assertValue($record->getMissing(),$record->getIncomingShortwave(),"in short: default");
    $record->setIncomingShortwave(87.2,"w/m2");
    $self->assertValue(87.2,$record->getIncomingShortwave(),"in short: set value");
    $record->setIncomingShortwave($record->getMissing(),"w/m2");
    $self->assertValue($record->getMissing(),$record->getIncomingShortwave(),"in short: set missing");
}

sub testIncomingLongwave {
    my $self = shift;
    my $record = SurfaceRecord->new(*STDOUT);

    $self->assertValue($record->getMissing(),$record->getIncomingLongwave(),"in long: default");
    $record->setIncomingLongwave(87.2,"w/m2");
    $self->assertValue(87.2,$record->getIncomingLongwave(),"in long: set value");
    $record->setIncomingLongwave($record->getMissing(),"w/m2");
    $self->assertValue($record->getMissing(),$record->getIncomingLongwave(),"in long: set missing");
}

sub testIncomingPAR {
    my $self = shift;
    my $record = SurfaceRecord->new(*STDOUT);

    $self->assertValue($record->getMissing(),$record->getIncomingPAR(),"in PAR: default");
    $record->setIncomingPAR(87.2,"w/m2");
    $self->assertValue(87.2,$record->getIncomingPAR(),"in PAR: set value");
    $record->setIncomingPAR($record->getMissing(),"w/m2");
    $self->assertValue($record->getMissing(),$record->getIncomingPAR(),"in PAR: set missing");
}

sub testNetRadiation {
    my $self = shift;
    my $record = SurfaceRecord->new(*STDOUT);

    $self->assertValue($record->getMissing(),$record->getNetRadiation(),"net rad: default");
    $record->setIncomingShortwave(10,"w/m2");
    $record->setOutgoingShortwave(3,"w/m2");
    $record->setIncomingLongwave(6,"w/m2");
    $record->setOutgoingLongwave(2,"w/m2");
    $self->assertValue(11,$record->getNetRadiation(),"net rad: calculated");

    $record->setIncomingShortwave(undef(),"w/m2");
    $self->assertValue($record->getMissing(),$record->getNetRadiation(),"net rad: no in-short");
    $record->setIncomingShortwave(10,"w/m2");

    $record->setOutgoingShortwave(undef(),"w/m2");
    $self->assertValue($record->getMissing(),$record->getNetRadiation(),"net rad: no out-short");
    $record->setOutgoingShortwave(3,"w/m2");

    $record->setIncomingLongwave(undef(),"w/m2");
    $self->assertValue($record->getMissing(),$record->getNetRadiation(),"net rad: no in-long");
    $record->setIncomingLongwave(6,"w/m2");

    $record->setOutgoingLongwave(undef(),"w/m2");
    $self->assertValue($record->getMissing(),$record->getNetRadiation(),"net rad: no out-long");
    $record->setOutgoingLongwave(2,"w/m2");

    $self->assertValue(11,$record->getNetRadiation(),"net rad: sanity check");
    $record->setNetRadiation(32,"w/m2");
    $self->assertValue(32,$record->getNetRadiation(),"net rad: manual set");
    $record->setNetRadiation($record->getMissing(),"w/m2");
    $self->assertValue(11,$record->getNetRadiation(),"net rad: set missing, re-calc");
}

sub testOutgoingShortwave {
    my $self = shift;
    my $record = SurfaceRecord->new(*STDOUT);

    $self->assertValue($record->getMissing(),$record->getOutgoingShortwave(),"out short: default");
    $record->setOutgoingShortwave(87.2,"w/m2");
    $self->assertValue(87.2,$record->getOutgoingShortwave(),"out short: set value");
    $record->setOutgoingShortwave($record->getMissing(),"w/m2");
    $self->assertValue($record->getMissing(),$record->getOutgoingShortwave(),"out short: set missing");
}

sub testOutgoingLongwave {
    my $self = shift;
    my $record = SurfaceRecord->new(*STDOUT);

    $self->assertValue($record->getMissing(),$record->getOutgoingLongwave(),"out long: default");
    $record->setOutgoingLongwave(87.2,"w/m2");
    $self->assertValue(87.2,$record->getOutgoingLongwave(),"out long: set value");
    $record->setOutgoingLongwave($record->getMissing(),"w/m2");
    $self->assertValue($record->getMissing(),$record->getOutgoingLongwave(),"out long: set missing");
}

sub testOutgoingPAR {
    my $self = shift;
    my $record = SurfaceRecord->new(*STDOUT);

    $self->assertValue($record->getMissing(),$record->getOutgoingPAR(),"out PAR: default");
    $record->setOutgoingPAR(87.2,"w/m2");
    $self->assertValue(87.2,$record->getOutgoingPAR(),"out PAR: set value");
    $record->setOutgoingPAR($record->getMissing(),"w/m2");
    $self->assertValue($record->getMissing(),$record->getOutgoingPAR(),"out PAR: set missing");
}

sub testPrecip {
    my $self = shift;
    my $record = SurfaceRecord->new(*STDOUT);
    
    $self->assertValue($record->getMissing(),$record->getPrecip(),"precip: default");
    $record->setPrecip(3.2,"mm");
    $self->assertValue(3.2,$record->getPrecip(),"precip: set value");
    $record->setPrecip($record->getMissing(),"mm");
    $self->assertValue($record->getMissing(),$record->getPrecip(),"precip: set missing");
}

sub testPresentWeather {
    my $self = shift;
    my $record = SurfaceRecord->new(*STDOUT);

    $self->assertValue(-999,$record->getPresentWeather(),"present wx: default");
    $record->setPresentWeather(11);
    $self->assertValue(11,$record->getPresentWeather(),"present wx: set value");
    $record->setPresentWeather(-999);
    $self->assertValue(-999,$record->getPresentWeather(),"present wx: set missing");
}

sub testPressure {
    my $self = shift;
    my $record = SurfaceRecord->new(*STDOUT);

    $self->assertValue($record->getMissing(),$record->getPressure(),"pressure: default");
    $record->setAltimeter(873.7,"mbar");
    $record->setElevation(443.2,"m");
    $self->assertValue(827.499872,$record->getPressure(),"pressure: calc from altimeter");

    $record->setAltimeter(undef(),"mbar");
    $self->assertValue($record->getMissing(),$record->getPressure(),"pressure: no altimeter");
    $record->setAltimeter(873.7,"mbar");
    
    $record->setElevation(undef(),"m");
    $self->assertValue($record->getMissing(),$record->getPressure(),"pressure: no elevation");
    $record->setElevation(443.2,"m");
    $self->assertValue(827.499872,$record->getPressure(),"pressure: sanity check");

    $record->setPressure(987.23,"mbar");
    $self->assertValue(987.23,$record->getPressure(),"pressure: set value");
    $record->setPressure($record->getMissing(),"mbar");
    $self->assertValue(827.499872,$record->getPressure(),"pressure: set missing, re-calc");
}

sub testRelativeHumidity {
    my $self = shift;
    my $record = SurfaceRecord->new(*STDOUT);

    $self->assertValue($record->getMissing(),$record->getRelativeHumidity(),"rh: default");
    $record->setTemperature(21.3,"C");
    $record->setDewPoint(19.8,"C");
    $self->assertValue(91.158779,$record->getRelativeHumidity(),"rh: calculated");

    $record->setTemperature(undef(),"C");
    $self->assertValue($record->getMissing(),$record->getRelativeHumidity(),"rh: no temp");
    $record->setTemperature(21.3,"C");
    
    $record->setDewPoint(undef(),"C");
    $self->assertValue($record->getMissing(),$record->getRelativeHumidity(),"rh: no dew point");
    $record->setDewPoint(19.8,"C");
    $self->assertValue(91.158779,$record->getRelativeHumidity(),"rh: sanity check");

    $record->setRelativeHumidity(43.2);
    $self->assertValue(43.2,$record->getRelativeHumidity(),"rh: set value");
    $record->setRelativeHumidity($record->getMissing());
    $self->assertValue(91.158779,$record->getRelativeHumidity(),"rh: set missing, re-calc");
}

sub testSeaLevelPressure {
    my $self = shift;
    my $record = SurfaceRecord->new(*STDOUT);

    $self->assertValue($record->getMissing(),$record->getSeaLevelPressure(),"slp: default");
    $record->setSeaLevelPressure(1013.21,"mbar");
    $self->assertValue(1013.21,$record->getSeaLevelPressure(),"slp: set value");
    $record->setSeaLevelPressure($record->getMissing(),"mbar");
    $self->assertValue($record->getMissing(),$record->getSeaLevelPressure(),"slp: set missing");
}

sub testSkinTemperature {
    my $self = shift;
    my $record = SurfaceRecord->new(*STDOUT);

    $self->assertValue($record->getMissing(),$record->getSkinTemperature(),"skin temp: default");
    $record->setSkinTemperature(23.1,"C");
    $self->assertValue(23.1,$record->getSkinTemperature(),"skin temp: set value");
    $record->setSkinTemperature($record->getMissing(),"C");
    $self->assertValue($record->getMissing(),$record->getSkinTemperature(),"skin temp: set missing");
}

sub testSnowDepth {
    my $self = shift;
    my $record = SurfaceRecord->new(*STDOUT);

    $self->assertValue($record->getMissing(),$record->getSnowDepth(),"snow depth: default");
    $record->setSnowDepth(3.2,"cm");
    $self->assertValue(3.2,$record->getSnowDepth(),"snow depth: default");
    $record->setSnowDepth($record->getMissing(),"cm");
    $self->assertValue($record->getMissing(),$record->getSnowDepth(),"snow depth: set missing");
}

sub testSpecificHumidity {
    my $self = shift;
    my $record = SurfaceRecord->new(*STDOUT);

    $self->assertValue($record->getMissing(),$record->getSpecificHumidity(),"spec humid: default");
    $record->setPressure(982.1,"mbar");
    $record->setDewPoint(18.2,"C");
    $self->assertValue(.013335,$record->getSpecificHumidity(),"spec humid: calculated");

    $record->setPressure(undef(),"mbar");
    $self->assertValue($record->getMissing(),$record->getSpecificHumidity(),"spec humid: no pressure");
    $record->setPressure(982.1,"mbar");

    $record->setDewPoint(undef(),"C");
    $self->assertValue($record->getMissing(),$record->getSpecificHumidity(),"spec humid: no dew point");
    $record->setDewPoint(18.2,"C");
    $self->assertValue(.013335,$record->getSpecificHumidity(),"spec humid: sanity check");

    $record->setSpecificHumidity(.873,"g/kg");
    $self->assertValue(.873,$record->getSpecificHumidity(),"spec humid: set value");
    $record->setSpecificHumidity($record->getMissing(),"kg/kg");
    $self->assertValue(.013335,$record->getSpecificHumidity(),"spec humid: set missing, re-calc");
}

sub testTemperature {
    my $self = shift;
    my $record = SurfaceRecord->new(*STDOUT);

    $self->assertValue($record->getMissing(),$record->getTemperature(),"temp: default");
    $record->setTemperature(21.4,"C");
    $self->assertValue(21.4,$record->getTemperature(),"temp: set value");
    $record->setTemperature($record->getMissing(),"C");
    $self->assertValue($record->getMissing(),$record->getTemperature(),"temp: set missing");
}

sub testVisibility {
    my $self = shift;
    my $record = SurfaceRecord->new(*STDOUT);
    
    $self->assertValue($record->getMissing(),$record->getVisibility(),"vis: default");
    $record->setVisibility(33232.1,"m");
    $self->assertValue(33232.1,$record->getVisibility(),"vis: set value");
    $record->setVisibility($record->getMissing(),"m");
    $self->assertValue($record->getMissing(),$record->getVisibility(),"vis: set missing");
}

sub testWindComponents {
    my $self = shift;
    my $record = SurfaceRecord->new(*STDOUT);

    $self->assertValue($record->getMissing(),$record->getWindUComponent(),"ucomp: default");
    $self->assertValue($record->getMissing(),$record->getWindVComponent(),"vcomp: default");

    $record->setWindSpeed(3.4,"m/s");
    $record->setWindDirection(231.2);
    $self->assertValue(2.649749,$record->getWindUComponent(),"ucomp: calculated");
    $self->assertValue(2.130453,$record->getWindVComponent(),"vcomp: calculated");
    
    $record->setWindSpeed($record->getMissing(),"m/s");
    $self->assertValue($record->getMissing(),$record->getWindUComponent(),"ucomp: no wind speed");
    $self->assertValue($record->getMissing(),$record->getWindVComponent(),"vcomp: no wind speed");
    $record->setWindSpeed(3.4,"m/s");

    $record->setWindDirection($record->getMissing());
    $self->assertValue($record->getMissing(),$record->getWindUComponent(),"ucomp: no wind direction");
    $self->assertValue($record->getMissing(),$record->getWindVComponent(),"vcomp: no wind direction");
    $record->setWindDirection(231.2);
    $self->assertValue(2.649749,$record->getWindUComponent(),"ucomp: sanity check");
    $self->assertValue(2.130453,$record->getWindVComponent(),"vcomp: sanity check");

    $record->setWindUComponent(-2.1,"m/s");
    $record->setWindVComponent(-1.8,"m/s");
    $self->assertValue(-2.1,$record->getWindUComponent(),"ucomp: set value");
    $self->assertValue(-1.8,$record->getWindVComponent(),"vcomp: set value");

    $record->setWindUComponent($record->getMissing(),"m/s");
    $record->setWindVComponent($record->getMissing(),"m/s");
    $self->assertValue(2.649749,$record->getWindUComponent(),"ucomp: set missing, re-calc");
    $self->assertValue(2.130453,$record->getWindVComponent(),"vcomp: set missing, re-calc");
}

sub testWindSpeedDirection {
    my $self = shift;
    my $record = SurfaceRecord->new(*STDOUT);

    $self->assertValue($record->getMissing(),$record->getWindSpeed(),"wind speed: default");
    $self->assertValue($record->getMissing(),$record->getWindDirection(),"wind direction: default");

    $record->setWindUComponent(-1.2,"m/s");
    $record->setWindVComponent(-.8,"m/s");
    $self->assertValue(1.442221,$record->getWindSpeed(),"wind speed: calculated");
    $self->assertValue(56.309932,$record->getWindDirection(),"wind direction: calculated");

    $record->setWindUComponent($record->getMissing(),"m/s");
    $self->assertValue($record->getMissing(),$record->getWindSpeed(),"wind speed: no uwind");
    $self->assertValue($record->getMissing(),$record->getWindDirection(),"wind direction: no uwind");
    $record->setWindUComponent(-1.2,"m/s");

    $record->setWindVComponent($record->getMissing(),"m/s");
    $self->assertValue($record->getMissing(),$record->getWindSpeed(),"wind speed: no vwind");
    $self->assertValue($record->getMissing(),$record->getWindDirection(),"wind direction: no vwind");
    $record->setWindVComponent(-.8,"m/s");

    $self->assertValue(1.442221,$record->getWindSpeed(),"wind speed: sanity check");
    $self->assertValue(56.309932,$record->getWindDirection(),"wind direction: sanity check");    

    $record->setWindSpeed(3.2,"m/s");
    $record->setWindDirection(109.1);
    $self->assertValue(3.2,$record->getWindSpeed(),"wind speed: set value");
    $self->assertValue(109.1,$record->getWindDirection(),"wind direction: set value");

    $record->setWindSpeed($record->getMissing(),"m/s");
    $record->setWindDirection($record->getMissing());
    $self->assertValue(1.442221,$record->getWindSpeed(),"wind speed: set missing, re-calc");
    $self->assertValue(56.309932,$record->getWindDirection(),"wind direction: set missing, re-calc");
}

1;






