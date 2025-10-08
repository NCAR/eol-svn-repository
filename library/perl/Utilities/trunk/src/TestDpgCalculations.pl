#! /usr/bin/perl -w

package TestDpgCalculations;
use strict;
use lib "..";
use DpgCalculations qw(:DEFAULT);
use TestModule;
our @ISA = ("TestModule");

&main();

sub main {
    my $tester = TestDpgCalculations->new();

    $tester->testAirDensity();
    $tester->testAltitude();
    $tester->testDewPoint();
    $tester->testMixingRatio();
    $tester->testPressureFromAltimeter();
    $tester->testRelativeHumidity();
    $tester->testSeaLevelPressure();
    $tester->testSpecificHumidity();
    $tester->testVaporPressure();
    $tester->testVirtualTemperature();
    $tester->testWinds();
}

sub testAirDensity {
    my $self = shift;

    $self->assertUndef(calculateAirDensity(undef(),21,0),"air dens: press == undef");
    $self->assertUndef(calculateAirDensity(984,undef(),0),"air dens: temp == undef");

    $self->assertValue(.163243,calculateAirDensity(984,21,0),"air dens: value");
}

sub testAltitude {
    my $self = shift;

    $self->assertUndef(calculateAltitude(undef(),-7.9,-29.4,6252.0,474.9,-8.6,-30.0,0),"alt: last_press == undef");
    $self->assertUndef(calculateAltitude(480.2,undef(),-29.4,6252.0,474.9,-8.6,-30.0,0),"alt: last_temp == undef");
    $self->assertUndef(calculateAltitude(480.2,-7.9,-29.4,undef(),474.9,-8.6,-30.0,0),"alt: last_alt == undef");
    $self->assertUndef(calculateAltitude(480.2,-7.9,-29.4,6252.0,undef(),-8.6,-30.0,0),"alt: this_press == undef");
    $self->assertUndef(calculateAltitude(480.2,-7.9,-29.4,6252.0,474.9,undef(),-30.0,0),"alt: this_temp == undef");

    $self->assertValue(6338.282877,calculateAltitude(480.2,-7.9,-29.4,6252.0,474.9,-8.6,-30.0,0),"alt: no missing values");
    $self->assertValue(6338.167183,calculateAltitude(480.2,-7.9,undef(),6252,474.9,-8.6,-30.0,0),"alt: last_dewpt == undef");
    $self->assertValue(6338.173015,calculateAltitude(480.2,-7.9,-29.4,6252.0,474.9,-8.6,undef(),0),"alt: this_dewpt == undef");
    $self->assertValue(6338.057616,calculateAltitude(480.2,-7.9,undef(),6252.0,474.9,-8.6,undef(),0),"alt: this_dewpt == last_dewpt == undef");
}

sub testDewPoint {
    my $self = shift;

    # Test Temperature values
    $self->assertUndef(calculateDewPoint(undef(),50,0),"dp: temp == undef");
    $self->assertValue(-108.022776,calculateDewPoint(-105,50,0),"dp: temp == -105");
    $self->assertValue(-103.242412,calculateDewPoint(-100,50,0),"dp: temp == -100");
    $self->assertValue(-9.191308,calculateDewPoint(0,50,0),"dp: temp == 0");
    $self->assertValue(81.988404,calculateDewPoint(100,50,0),"dp: temp == 100");
    $self->assertValue(86.474372,calculateDewPoint(105,50,0),"dp: temp == 105");

    # Test RH Values
    $self->assertUndef(calculateDewPoint(32.1,undef(),0),"dp: RH == undef");
    $self->assertUndef(calculateDewPoint(32.1,0,0),"dp: RH == 0%");
    $self->assertValue(32.1,calculateDewPoint(32.1,100,0),"dp: RH == 100%");
    $self->assertValue(32.980913,calculateDewPoint(32.1,105.1,0),"dp: RH > 100%");
}

sub testMixingRatio {
    my $self = shift;

    $self->assertUndef(calculateMixingRatio(undef(),433.2,0),"mixr: temp == undef");
    $self->assertUndef(calculateMixingRatio(23.1,undef(),0),"mixr: vapr == undef");

    $self->assertValue(-.656978,calculateMixingRatio(23.1,433.2,0),"mixr: value");
}

sub testPressureFromAltimeter {
    my $self = shift;
    
    $self->assertUndef(calculatePressureFromAltimeter(undef(),111.1,0),"press: alt == undef");
    $self->assertUndef(calculatePressureFromAltimeter(942.1,undef(),0),"press: elev == undef");
    $self->assertUndef(calculatePressureFromAltimeter(0,1582,0),"press: alt cause imaginary");

    $self->assertValue(815.764798,calculatePressureFromAltimeter(986,1562,0),"press: value");
}

sub testRelativeHumidity {
    my $self = shift;

    $self->assertUndef(calculateRelativeHumidity(undef(),32.1,0),"rh: temp == undef");
    $self->assertUndef(calculateRelativeHumidity(32.1,undef(),0),"rh: dewpt == undef");
    $self->assertValue(100,calculateRelativeHumidity(32.1,32.1,0),"rh: temp == dewpt");
    $self->assertValue(92.868085,calculateRelativeHumidity(32.1,30.8,0),"rh: temp > dewpt");
    $self->assertValue(105.212968,calculateRelativeHumidity(32.1,33,5,0),"rh: temp < dewpt");
}

sub testSeaLevelPressure {
    my $self = shift;
    
    $self->assertUndef(calculateSeaLevelPressure(undef(),432.1,32.1,21.3,0),"slp: press == undef");
    $self->assertUndef(calculateSeaLevelPressure(987.9,undef(),32.1,21.3,0),"slp: elev == undef");
    $self->assertUndef(calculateSeaLevelPressure(987.9,432.1,21.3,undef(),0),"slp: temp == undef");
    $self->assertUndef(calculateSeaLevelPressure(987.9,432.1,undef(),32.1,0),"slp: dewpt == undef");

    $self->assertValue(303.416391,calculateSeaLevelPressure(299.99,100,11,23,0),"slp: small pressure");
    $self->assertValue(1213.890224,calculateSeaLevelPressure(1200.01,100,11,23,0),"slp: large pressure");

    $self->assertValue(959.998898,calculateSeaLevelPressure(960,-.01,11,23,0),"slp: neg elev");
    
    $self->assertValue(979.066047,calculateSeaLevelPressure(960,100,11,-100.01,0),"slp: small temp");
    $self->assertValue(968.790857,calculateSeaLevelPressure(960,100,11,100.01,0),"slp: large temp");

    $self->assertValue(971.150723,calculateSeaLevelPressure(960,100,-100.01,23,0),"slp: small dewpt");
    $self->assertValue(966.510388,calculateSeaLevelPressure(960,100,100.01,23,0),"slp: large dewpt");

    $self->assertValue(971.075066,calculateSeaLevelPressure(960,100,15,23,0),"slp: good value");
}

sub testSpecificHumidity {
    my $self = shift;
    
    $self->assertUndef(calculateSpecificHumidity(undef(),23,0),"sh: press == undef");
    $self->assertUndef(calculateSpecificHumidity(987,undef(),0),"sh: dewpt == undef");

    $self->assertValue(.017909,calculateSpecificHumidity(986,23,0),"sh: value");
}

sub testVaporPressure {
    my $self = shift;

    $self->assertUndef(calculateVaporPressure(undef(),0),"vapor: temp == undef");
    $self->assertUndef(calculateVaporPressure(-243.5,0),"vapor: temp == -243.5");
    $self->assertValue(.000027,calculateVaporPressure(-100,0),"vapor: temp == -100");
    $self->assertValue(6.112100,calculateVaporPressure(0,0),"vapor: temp == 0");
    $self->assertValue(42.456449,calculateVaporPressure(30,0),"vapor: temp == 30");
    $self->assertValue(1047.723733,calculateVaporPressure(100,0),"vapor: temp == 100");
}

sub testVirtualTemperature {
    my $self = shift;

    $self->assertUndef(calculateVirtualTemperature(undef(),.543,0),"virt temp: temp == undef");
    $self->assertUndef(calculateVirtualTemperature(323.1,undef(),0),"virt temp: mixr == undef");
    $self->assertUndef(calculateVirtualTemperature(323.1,-1,0),"virt temp: mixr == -1");

    $self->assertValue(379.208123,calculateVirtualTemperature(323.1,.4,0),"virt temp: value");
}

sub testWinds {
    my $self = shift;
    
    my ($u,$v) = calculateUVfromWind(undef(),15,0);
    $self->assertUndef($u,"u: spd == undef");
    $self->assertUndef($v,"v: spd == undef");
    ($u,$v) = calculateUVfromWind(10,undef(),0);
    $self->assertUndef($u,"u: dir == undef");
    $self->assertUndef($v,"v: dir == undef");

    ($u,$v) = calculateUVfromWind(-1,93,0);
    $self->assertUndef($u,"u: spd < 0");
    $self->assertUndef($v,"v: spd < 0");
    ($u,$v) = calculateUVfromWind(18,-1,0);
    $self->assertUndef($u,"u: dir < 0");
    $self->assertUndef($v,"v: dir < 0");
    ($u,$v) = calculateUVfromWind(19,360,0);
    $self->assertValue(0,$u,"u: dir > 360");
    $self->assertValue(-19,$v,"v: dir > 360");

    
    ($u, $v) = calculateUVfromWind(18, 92, 0);
    $self->assertValue(-17.9890348863,$u,"u: value");
    $self->assertValue(.628190910645,$v,"v: value");


    my ($spd,$dir) = calculateWindFromUV(undef(),.6,0);

    ($spd,$dir) = calculateWindFromUV(13,undef(),0);

    ($spd, $dir) = calculateWindFromUV(0,0,0);
    $self->assertValue(0,$spd,"(0,0) wind speed");
    $self->assertValue(0, $dir, "(0,0) wind direction");
    ($spd, $dir) = calculateWindFromUV(10, 0);
    $self->assertValue(10, $spd, "(10,0) wind speed");
    $self->assertValue(270, $dir, "(10,0) wind direction");
    ($spd, $dir) = calculateWindFromUV(-10, 0);
    $self->assertValue(10, $spd, "(-10,0) wind speed");
    $self->assertValue(90, $dir, "(-10,0) wind direction");

    ($spd,$dir) = calculateWindFromUV(-17.9890348863,.628190910645);
    $self->assertValue(18, $spd, "(-17.9,.628) wind speed");
    $self->assertValue(92, $dir, "(-17.9,.628) wind direction");
}
