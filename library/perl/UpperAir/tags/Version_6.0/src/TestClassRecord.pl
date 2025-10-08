#! /usr/bin/perl -w

package TestClassRecord;
use strict;
use lib "/net/work/lib/perl/Utilities";
use TestModule;
use ClassRecord;
use ClassConstants qw(:DEFAULT);
our @ISA = ("TestModule");
$| = 1;

my $WARN;
my $warning = "warning.txt";

&main();

sub main {
    my $tester = TestClassRecord->new();

    open($WARN,">$warning") or die("Cannot open $warning file\n");

    $tester->testTime();
    $tester->testPressure();
    $tester->testTemperature();
    $tester->testDewPoint();
    $tester->testRelativeHumidity();
    $tester->testWindComponents();
    $tester->testWindParts();
    $tester->testAscensionRate();
    $tester->testLongitude();
    $tester->testLatitude();
    $tester->testVariableParams();
    $tester->testAltitude();

    close($WARN);

    if (system("diff $warning TEST_CLASS_SOUNDING.txt") == 0) {
	unlink($warning);
    } else {
	printf("File $warning does not match TEST_CLASS_SOUNDING.txt\n");
    }
}

sub testAltitude {
    my $self = shift;
    my $record = ClassRecord->new($WARN,$warning);

    $self->assertValue(99999.0,$record->getAltitude(),"altitude: default");
    $record->setAltitude(832.4,"m");
    $self->assertValue(832.4,$record->getAltitude(),"altitude: value");
    $record->setAltitude(99999.0,"m");
    $self->assertValue(99999.0,$record->getAltitude(),"altitude: missing");
  
}

sub testAscensionRate {
    my $self = shift;
    my $prev = ClassRecord->new($WARN,$warning);
    my $record = ClassRecord->new($WARN,$warning,$prev);

    $self->assertValue(999.0,$record->getAscensionRate(),"asc rate: default");
    $self->assertValue($MISSING_FLAG,$record->getAscensionRateFlag(),
		       "asc rate flag: default");
    $record->setAscensionRateFlag($BAD_FLAG);
    $self->assertValue($MISSING_FLAG,$record->getAscensionRateFlag(),
		       "asc rate flag: bad missing flag no missing value");
    $record->setAscensionRateFlag(undef());

    $prev->setTime(0);
    $prev->setAltitude(584,"m");
    $record->setTime(10);
    $record->setAltitude(684,"m");
    $self->assertValue(10,$record->getAscensionRate(),"asc rate: calculated");
    $self->assertValue($UNCHECKED_FLAG,$record->getAscensionRateFlag(),
		       "asc rate flag: calculated");

    $record->setAltitude(undef(),"m");
    $self->assertValue(999.0,$record->getAscensionRate(),"asc rate: no altitude");
    $self->assertValue($MISSING_FLAG,$record->getAscensionRateFlag(),"asc rate flag: no altitude");

    $record->setTime(undef());
    $record->setAltitude(452,"m");
    $self->assertValue(999.0,$record->getAscensionRate(),"asc rate: no time");
    $self->assertValue($MISSING_FLAG,$record->getAscensionRateFlag(),"asc rate flag: no time");

    $record->setTime(10);
    $prev->setAltitude(undef(),"m");
    $self->assertValue(999.0,$record->getAscensionRate(),"asc rate: no prev altitude");
    $self->assertValue($MISSING_FLAG,$record->getAscensionRateFlag(),"asc rate flag: no prev altitude");

    $prev->setAltitude(324,"m");
    $prev->setTime(undef());
    $self->assertValue(999.0,$record->getAscensionRate(),"asc rate: no prev time");
    $self->assertValue($MISSING_FLAG,$record->getAscensionRateFlag(),"asc rate flag: no prev time");

    $prev->setTime(0);
    $record->setAscensionRate(8.7,"m/s");
    $self->assertValue(8.7,$record->getAscensionRate(),"asc rate: set value");
    $self->assertValue($UNCHECKED_FLAG,$record->getAscensionRateFlag(),
		       "asc rate flag: set value");
    $record->setAscensionRateFlag($MISSING_FLAG);
    $self->assertValue($UNCHECKED_FLAG,$record->getAscensionRateFlag(),
		       "asc rate flag: missing flag, not missing value");
    $record->setAscensionRateFlag(undef());

    $record->setAscensionRate(999.0,"m/s");
    $self->assertValue(12.8,$record->getAscensionRate(),"asc rate: missing re-calc");
    $self->assertValue($UNCHECKED_FLAG,$record->getAscensionRateFlag(),
		       "asc rate flag: missing re-calc");
}

sub testLatitude {
    my $self = shift;
    my $record = ClassRecord->new($WARN,$warning);

    $self->assertValue(999.000,$record->getLatitude(),"latitude: default");
    $record->setLatitude(45.32,"DDDDD");
    $self->assertValue(45.32,$record->getLatitude(),"latitude: set value");
    $record->setLatitude("999.000","DDDDDDD");
    $self->assertValue(999.000,$record->getLatitude(),"latitude: set missing");
}

sub testLongitude {
    my $self = shift;
    my $record = ClassRecord->new($WARN,$warning);

    $self->assertValue(9999.000,$record->getLongitude(),"longitude: default");
    $record->setLongitude("-102.38","-DDDDDD");
    $self->assertValue(-102.38,$record->getLongitude(),"longitude: set value");
    $record->setLongitude("9999.000","DDDDDDDD");
    $self->assertValue(9999.000,$record->getLongitude(),"longitude: set missing");
}

sub testDewPoint {
    my $self = shift;
    my $record = ClassRecord->new($WARN,$warning);

    $self->assertValue(999.0,$record->getDewPoint(),"dew point: default");
    $record->setTemperature(-42.2,"C");
    $self->assertValue(999.0,$record->getDewPoint(),"dew point: can't calc");
    $record->setRelativeHumidity(67.9);
    $self->assertValue(-45.781112,$record->getDewPoint(),"dew point: calculated");
    $record->setDewPoint(-39.1,"C");
    $self->assertValue(-39.1,$record->getDewPoint(),"dew point: value");
    $record->setDewPoint(999.0,"C");
    $self->assertValue(-45.781112,$record->getDewPoint(),"dew point: set missing, still calc");
    $record->setTemperature(999.0,"C");
    $self->assertValue(999.0,$record->getDewPoint(),"dew point: set missing no calc");
}

sub testPressure {
    my $self = shift;
    my $record = ClassRecord->new($WARN,$warning);

    $self->assertValue(9999.0,$record->getPressure(),"pressure: default");
    $self->assertValue($MISSING_FLAG,$record->getPressureFlag(),
		       "pressure flag: default");
    $record->setPressure(983.2,"mbar");
    $self->assertValue(983.2,$record->getPressure(),"pressure: value");
    $self->assertValue($UNCHECKED_FLAG,$record->getPressureFlag(),
		       "pressure flag: calculated");
    $record->setPressureFlag($MISSING_FLAG);
    $self->assertValue($UNCHECKED_FLAG,$record->getPressureFlag(),
		       "pressure flag: set missing flag no missing value");

    $record->setPressureFlag($BAD_FLAG);
    $self->assertValue($BAD_FLAG,$record->getPressureFlag(),
		       "pressure flag: set");

    $record->setPressure(9999.0,"mbar");
    $self->assertValue(9999.0,$record->getPressure(),"pressure: set missing");
    $self->assertValue($MISSING_FLAG,$record->getPressureFlag(),
		       "pressure: set missing");
}

sub testRelativeHumidity {
    my $self = shift;
    my $record = ClassRecord->new($WARN,$warning);
    
    $self->assertValue(999.0,$record->getRelativeHumidity(),"rh: default");
    $self->assertValue($MISSING_FLAG,$record->getRelativeHumidityFlag(),
		       "rh flag: default");
    $record->setTemperature(-42.3,"C");
    $self->assertValue(999.0,$record->getRelativeHumidity(),"rh: can't calc");
    $self->assertValue($MISSING_FLAG,$record->getRelativeHumidityFlag(),
		       "rh flag: can't calc");
    $record->setRelativeHumidityFlag($BAD_FLAG);
    $self->assertValue($MISSING_FLAG,$record->getRelativeHumidityFlag(),
		       "rh flag: not missing flag, missing value");
    $record->setRelativeHumidityFlag(undef());

    $record->setDewPoint(-38.2,"C");
    $self->assertValue(153.276505,$record->getRelativeHumidity(),"rh: calculated");
    $self->assertValue($UNCHECKED_FLAG,$record->getRelativeHumidityFlag(),
		       "rh flag: calculated");
    $record->setRelativeHumidity(34.6);
    $self->assertValue(34.6,$record->getRelativeHumidity(),"rh: set rh");
    $self->assertValue($UNCHECKED_FLAG,$record->getRelativeHumidityFlag(),
		       "rh flag: set rh");
    $record->setRelativeHumidityFlag($BAD_FLAG);
    $self->assertValue($BAD_FLAG,$record->getRelativeHumidityFlag(),
		       "rh flag: set flag");
    $record->setRelativeHumidityFlag(undef());

    $record->setRelativeHumidity(999.0);
    $self->assertValue(153.276505,$record->getRelativeHumidity(),"rh: missing, re-calc");
    $self->assertValue($UNCHECKED_FLAG,$record->getRelativeHumidityFlag(),
		       "rh flag: missing, re-calc");
    $record->setRelativeHumidityFlag($MISSING_FLAG);
    $self->assertValue($UNCHECKED_FLAG,$record->getRelativeHumidityFlag(),
		       "rh flag: missing flag, not missing value");
    
    $record->setTemperature(999.0,"C");
    $self->assertValue(999.0,$record->getRelativeHumidity(),"rh: set missing, no calc");
    $self->assertValue($MISSING_FLAG,$record->getRelativeHumidityFlag(),
		       "rh flag: set missing, no calc");
}

sub testTemperature {
    my $self = shift;
    my $record = ClassRecord->new($WARN,$warning);

    $self->assertValue(999.0,$record->getTemperature(),"temp: default");
    $self->assertValue($MISSING_FLAG,$record->getTemperatureFlag(),
		       "temp flag: default");
    $record->setTemperature(-32.0,"C");
    $self->assertValue(-32.0,$record->getTemperature(),"temp: value");
    $self->assertValue($UNCHECKED_FLAG,$record->getTemperatureFlag(),
		       "temp flag: calculated");
    $record->setTemperatureFlag($MISSING_FLAG);
    $self->assertValue($UNCHECKED_FLAG,$record->getTemperatureFlag(),
		       "temp flag: set missing no missing value");

    $record->setTemperatureFlag($BAD_FLAG);
    $self->assertValue($BAD_FLAG,$record->getTemperatureFlag(),
		       "temp flag: set");
    $record->setTemperature(999.0,"C");
    $self->assertValue(999.0,$record->getTemperature(),"temp: set missing");
    $self->assertValue($MISSING_FLAG,$record->getTemperatureFlag(),
			 "temp flag: set missing, no missing flag");
}

sub testTime {
    my $self = shift;
    my $record = ClassRecord->new($WARN,$warning);

    $self->assertValue(9999.0,$record->getTime(),"time: default");
    $record->setTime(342.0);
    $self->assertValue(342,$record->getTime(),"time: seconds");
    $record->setTime(2,42);
    $self->assertValue(162.0,$record->getTime(),"time: min sec");
    $record->setTime(9999.0);
    $self->assertValue(9999.0,$record->getTime(),"time: set missing");
}

sub testVariableParams {
    my $self = shift;
    my $record = ClassRecord->new($WARN,$warning);

    $self->assertValue(999.0,$record->getVariableValue(1),"var param 1: default");
    $self->assertValue(999.0,$record->getVariableValue(2),"var param 2: default");
    $record->setVariableValue(1,85);
    $record->setVariableValue(2,3.2);
    $self->assertValue(85.0,$record->getVariableValue(1),"var param 1: set value");
    $self->assertValue(3.2,$record->getVariableValue(2),"var param 2: set value");
    $record->setVariableValue(1,999.0);
    $record->setVariableValue(2,999.0);
    $self->assertValue(999.0,$record->getVariableValue(1),"var param 1: missing");
    $self->assertValue(999.0,$record->getVariableValue(2),"var param 2: missing");
}

sub testWindComponents {
    my $self = shift;
    my $record = ClassRecord->new($WARN,$warning);

    $self->assertValue(9999.0,$record->getUWindComponent(),"uwind: default");
    $self->assertValue(9999.0,$record->getVWindComponent(),"vwind: default");
    $self->assertValue($MISSING_FLAG,$record->getUWindComponentFlag(),
		       "uwind flag: default");
    $self->assertValue($MISSING_FLAG,$record->getVWindComponentFlag(),
		       "vwind flag: default");

    $record->setUWindComponentFlag($BAD_FLAG);
    $record->setVWindComponentFlag($BAD_FLAG);
    $self->assertValue($MISSING_FLAG,$record->getUWindComponentFlag(),
		       "uwind flag: bad not missing flag");
    $self->assertValue($MISSING_FLAG,$record->getVWindComponentFlag(),
		       "vwind flag: bad not missing flag");

    $record->setWindSpeed(3.2,"m/s");
    $self->assertValue(9999.0,$record->getUWindComponent(),"uwind: can't calc");
    $self->assertValue(9999.0,$record->getVWindComponent(),"vwind: can't calc");
    $self->assertValue($MISSING_FLAG,$record->getUWindComponentFlag(),
		       "uwind flag: can't calc");
    $self->assertValue($MISSING_FLAG,$record->getVWindComponentFlag(),
		       "vwind flag: can't calc");

    $record->setWindDirection(142.0);
    $self->assertValue(-1.970117,$record->getUWindComponent(),"uwind: calculated");
    $self->assertValue(2.521634,$record->getVWindComponent(),"vwind: calculated");
    $self->assertValue($UNCHECKED_FLAG,$record->getUWindComponentFlag(),
		       "uwind flag: calculated");
    $self->assertValue($UNCHECKED_FLAG,$record->getVWindComponentFlag(),
		       "vwind flag: calculated");

    $record->setUWindComponent(-4.2,"m/s");
    $record->setVWindComponent(-1.3,"m/s");
    $self->assertValue(-4.2,$record->getUWindComponent(),"uwind: set");
    $self->assertValue(-1.3,$record->getVWindComponent(),"vwind: set");
    $self->assertValue($UNCHECKED_FLAG,$record->getUWindComponentFlag(),
		       "uwind flag: calculated from set");
    $self->assertValue($UNCHECKED_FLAG,$record->getVWindComponentFlag(),
		       "vwind flag: calculated from set");

    $record->setUWindComponentFlag($BAD_FLAG);
    $record->setVWindComponentFlag($BAD_FLAG);
    $self->assertValue($BAD_FLAG,$record->getUWindComponentFlag(),
		       "uwind flag: set flag");
    $self->assertValue($BAD_FLAG,$record->getVWindComponentFlag(),
		       "vwind flag: set flag");

    $record->setUWindComponentFlag($MISSING_FLAG);
    $record->setVWindComponentFlag($MISSING_FLAG);
    $self->assertValue($UNCHECKED_FLAG,$record->getUWindComponentFlag(),
		       "uwind flag: bad missing flag");
    $self->assertValue($UNCHECKED_FLAG,$record->getVWindComponentFlag(),
		       "vwind flag: bad missing flag");

    $record->setUWindComponent(9999.0,"m/s");
    $record->setVWindComponent(9999.0,"m/s");
    $self->assertValue(-1.970117,$record->getUWindComponent(),"uwind: calculated");
    $self->assertValue(2.521634,$record->getVWindComponent(),"vwind: calculated");
    $self->assertValue($UNCHECKED_FLAG,$record->getUWindComponentFlag(),
		       "uwind flag: missing recalculated");
    $self->assertValue($UNCHECKED_FLAG,$record->getVWindComponentFlag(),
		       "vwind flag: missing recalculated");

    $record->setWindDirection(999.0);
    $self->assertValue(9999.0,$record->getUWindComponent(),"uwind: default");
    $self->assertValue(9999.0,$record->getVWindComponent(),"vwind: default");
    $self->assertValue($MISSING_FLAG,$record->getUWindComponentFlag(),
		       "uwind flag: missing can't recalc");
    $self->assertValue($MISSING_FLAG,$record->getVWindComponentFlag(),
		       "vwind flag: missing can't recalc");

}

sub testWindParts {
    my $self = shift;
    my $record = ClassRecord->new($WARN,$warning);

    $self->assertValue(999.0,$record->getWindSpeed(),"windspd: default");
    $self->assertValue(999.0,$record->getWindDirection(),"winddir: default");

    $record->setUWindComponent(-4.2,"m/s");
    $self->assertValue(999.0,$record->getWindSpeed(),"windspd: can't calc");
    $self->assertValue(999.0,$record->getWindDirection(),"winddir: can't calc");

    $record->setVWindComponent(-1.2,"m/s");
    $self->assertValue(4.368066,$record->getWindSpeed(),"windspd: calculated");
    $self->assertValue(74.054604,$record->getWindDirection(),"winddir: calculated");
    
    $record->setWindSpeed(7.2,"m/s");
    $record->setWindDirection(42.0);
    $self->assertValue(7.2,$record->getWindSpeed(),"windspd: set");
    $self->assertValue(42.0,$record->getWindDirection(),"winddir: set");

    $record->setWindSpeed(999.0,"m/s");
    $record->setWindDirection(999.0);
    $self->assertValue(4.368066,$record->getWindSpeed(),"windspd: missing calc");
    $self->assertValue(74.054604,$record->getWindDirection(),"winddir: missing calc");

    $record->setUWindComponent(9999.0,"m/s");
    $self->assertValue(999.0,$record->getWindSpeed(),"windspd: missing no-calc");
    $self->assertValue(999.0,$record->getWindDirection(),"winddir: missing no-calc");
}
