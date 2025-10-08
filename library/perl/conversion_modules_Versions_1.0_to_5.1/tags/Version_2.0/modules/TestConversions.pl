#! /usr/bin/perl -w

package TestConversions;
use strict;
use lib ".";
use Conversions;
use TestModule;
our @ISA = ("TestModule");
&main();

sub main {
  my $tester = TestConversions->new();

  $tester->calcDewPoint();
  $tester->calcRelHumid();
  $tester->calcSLP();
  $tester->calcSpecHumid();
  $tester->calcStnPress();
  $tester->calcUVwinds();

  $tester->daysFeb();
  $tester->convJulian();
  $tester->formats();
  $tester->valids();
  $tester->compDates();
  $tester->adjustDateTime();

  $tester->convLatLong();

  $tester->convArea();
  $tester->convLength();
  $tester->convTemp();
  $tester->convVolume();
  $tester->convVelocity();
  $tester->convTime();
  $tester->convRad();
  $tester->convPress();
  $tester->convFlow();

  $tester->convAngles();

  $tester->unixTime();
  $tester->stateCode();
}

sub adjustDateTime {
  my $self = shift;

  my ($date, $time) = Conversions::adjustDateTime("2004/05/20","15:20",1,3,13);
  $self->assertString("2004/05/21", $date, "Good date");
  $self->assertString("18:33", $time, "Good time");

  ($date, $time) = Conversions::adjustDateTime("2004/12/31", "23:59",0,0,1);
  $self->assertString("2005/01/01", $date, "Next Year");
  $self->assertString("00:00", $time, "Next Minute and Hour");

  ($date, $time) = Conversions::adjustDateTime("2004/01/01", "00:00",0,0,-1);
  $self->assertString("2003/12/31", $date, "Previous Year");
  $self->assertString("23:59", $time, "Previous Minute and Hour");

  ($date, $time) = Conversions::adjustDateTime("2004/05/20", "19:32",0,1,-60);
  $self->assertString("2004/05/20", $date, "Matching offset date");
  $self->assertString("19:32", $time, "Matching offset time");
}

sub calcDewPoint {
  my $self = shift;

  # Test Missing Temperature Value
  $self->assertString(Conversions::getMissing(), Conversions::calculateDewPoint(Conversions::getMissing(), 45), "Missing Temp");

  # Test Missing Relative Humidity Value
  $self->assertString(Conversions::getMissing(), Conversions::calculateDewPoint(11, Conversions::getMissing()), "Missing RH");

  # Test Too Big Temperature Value
  $self->assertFormatString(72.77, Conversions::calculateDewPoint(100.01, 34, 0), "Temp Too Big");

  # Test Too Small Temperature Value
  $self->assertFormatString(-104.99, Conversions::calculateDewPoint(-100.01, 34, 0), "Temp Too Small");

  # Test Too Big Relative Humidity
  $self->assertFormatString(11.00, Conversions::calculateDewPoint(11, 100.01, 0), "RH Too Big");

  # Test Too Small Relative Humidity
  $self->assertFormatString(-69.52, Conversions::calculateDewPoint(11, 0.04, 0), "RH Too Small");

  # Test the Dew Point Calculation
  $self->assertFormatString("15.83", Conversions::calculateDewPoint(23, 64, 0), "Dew Point");

  # Test 100% Relative Humidity
  $self->assertFormatString("23", Conversions::calculateDewPoint(23, 100, 0), "100% RH");
}

sub calcRelHumid {
  my $self = shift;

  # Test Missing Temperature Value
  $self->assertString(Conversions::getMissing(), Conversions::calculateRelativeHumidity(Conversions::getMissing(), 45), "Missing Temp");

  # Test Missing Dew Point Value
  $self->assertString(Conversions::getMissing(), Conversions::calculateRelativeHumidity(11, Conversions::getMissing()), "Missing Dew Point");

  # Test Too Big Temperature Value
  $self->assertFormatString(5.08, Conversions::calculateRelativeHumidity(100.01, 34, 0), "Temp Too Big");

  # Test Too Small Temperature Value
  $self->assertFormatString(194476444.06, Conversions::calculateRelativeHumidity(-100.01, 34, 0), "Temp Too Small");

  # Test Too Big Dew Point
  $self->assertFormatString(7989.69, Conversions::calculateRelativeHumidity(11, 100.01, 0), "Dew Point Too Big");

  # Test Too Small Dew Point
  $self->assertFormatString(0.00, Conversions::calculateRelativeHumidity(11, -100.01, 0), "Dew Point Too Small");

  # Test Dew Point > Temperature
  $self->assertFormatString(214.10, Conversions::calculateRelativeHumidity(11, 23, 0), "Temp < Dew Point");

  # Test the Relative Humidity Calculation
  $self->assertFormatString("46.71", Conversions::calculateRelativeHumidity(23, 11, 0), "Relative Humidity");

  # Test 100% Relative Humidity
  $self->assertFormatString("100", Conversions::calculateRelativeHumidity(23, 23, 0), "Equal Temp and Dew Point");
}

sub calcSLP {
  my $self = shift;

  # Test missing Pressure
  $self->assertString(Conversions::getMissing(), Conversions::calculateSeaLevelPressure(Conversions::getMissing(), 100, 11, 23, 0), "Missing Pressure");

  # Test missing Elevation
  $self->assertString(Conversions::getMissing(), Conversions::calculateSeaLevelPressure(960, Conversions::getMissing(), 11, 23, 0), "Missing Elevation");

  # Test missing Dew Point
  $self->assertString(Conversions::getMissing(), Conversions::calculateSeaLevelPressure(960, 100, Conversions::getMissing(), 23, 0), "Missing Dew Point");

  # Test missing Temperature
  $self->assertString(Conversions::getMissing(), Conversions::calculateSeaLevelPressure(960, 100, 11, Conversions::getMissing(), ), "Missing Temperature");

  # Too Small Pressure
  $self->assertFormatString(303.42, Conversions::calculateSeaLevelPressure(299.99, 100, 11, 23, 0), "Too Small Pressure");

  # Too High Pressure
  $self->assertFormatString(1213.89, Conversions::calculateSeaLevelPressure(1200.01, 100, 11, 23, 0), "Too High Pressure");

  # Negative Elevation
  $self->assertFormatString(960.00, Conversions::calculateSeaLevelPressure(960, -.01, 11, 23, 0), "Negative Elevation");

  # Too Small Dew Point
  $self->assertFormatString(971.15, Conversions::calculateSeaLevelPressure(960, 100, -100.01, 23, 0), "Too Small Dew Point");

  # Too High Dew Point
  $self->assertFormatString(966.51, Conversions::calculateSeaLevelPressure(960, 100, 100.01, 23, 0), "Too High Dew Point");
  
  # Too Small Temperature
  $self->assertFormatString(979.07, Conversions::calculateSeaLevelPressure(960, 100, 11, -100.01, 0), "Too Small Temperature");

  # Too High Temperature
  $self->assertFormatString(968.79, Conversions::calculateSeaLevelPressure(960, 100, 11, 100.01, 0), "Too High Temperature");

  # Calculation of SLP
  $self->assertFormatString("971.09", Conversions::calculateSeaLevelPressure(960, 100, 11, 23, 0), "Calc SLP");
}

sub calcSpecHumid {
  my $self = shift;

  $self->assertFormatString(Conversions::getMissing(), Conversions::calculateSpecificHumidity(Conversions::getMissing(), 23, 0), "Missing pressure SH");

  $self->assertFormatString(Conversions::getMissing(), Conversions::calculateSpecificHumidity(987, Conversions::getMissing(), 0), "Missing dew point SH");

  $self->assertFormatString(.20, Conversions::calculateSpecificHumidity(99, 23, 0), "Too small pressure SH");

  $self->assertFormatString(.01, Conversions::calculateSpecificHumidity(1501, 23, 0), "Too big pressure SH");
  
  $self->assertFormatString(.00, Conversions::calculateSpecificHumidity(987, -101, 0), "Too small dew point SH");

  $self->assertFormatString(1.17, Conversions::calculateSpecificHumidity(987, 101, 0), "Too big dew point SH");

  $self->assertFormatString(".017939524522", Conversions::calculateSpecificHumidity(986, 23), "Valid calculation");
}

sub calcStnPress {
  my $self = shift;

  $self->assertFormatString(Conversions::getMissing(), Conversions::calculateStationPressure(Conversions::getMissing(), 1582), "Missing altimeter StnPress");
  $self->assertFormatString(Conversions::getMissing(), Conversions::calculateStationPressure(982, Conversions::getMissing()), "Missing elevation StnPress");

  $self->assertFormatString(Conversions::getMissing(), Conversions::calculateStationPressure(0, 1582, 0), "Imaginary value");

  $self->assertFormatString("815.708912847", Conversions::calculateStationPressure(986, 1562, 0), "Station Pressure");
}


sub calcUVwinds {
  my $self = shift;

  $self->assertFormatString(Conversions::getMissing(), Conversions::calculateUVfromWind(Conversions::getMissing(), 15, 0), "Missing wind speed UV");
  $self->assertFormatString(Conversions::getMissing(), Conversions::calculateUVfromWind(10, Conversions::getMissing(), 0), "Missing wind direction UV");

  $self->assertFormatString(Conversions::getMissing(), Conversions::calculateUVfromWind(-1, 93, 0), "Too small wind speed UV");
  $self->assertFormatString(Conversions::getMissing(), Conversions::calculateUVfromWind(18, -1, 0), "Too small wind direction UV");
  $self->assertFormatString(Conversions::getMissing(), Conversions::calculateUVfromWind(18, 361, 0), "Too big wind direction UV");

  my ($u, $v) = Conversions::calculateUVfromWind(18, 92, 0);
  $self->assertFormatString("-17.9890348863", $u, "ucomponent UV");
  $self->assertFormatString(".628190910645", $v, "vcomponent UV");



  $self->assertFormatString(Conversions::getMissing(), Conversions::calculateWindFromUV(Conversions::getMissing(), .6, 0), "Missing ucomp for wind");
  $self->assertFormatString(Conversions::getMissing(), Conversions::calculateWindFromUV("-17.9", Conversions::getMissing(), 0), "Missing vcomp for wind");

  my ($spd, $dir) = Conversions::calculateWindFromUV(0, 0);
  $self->assertFormatString(0, $spd, "(0,0) wind speed");
  $self->assertFormatString(0, $dir, "(0,0) wind direction");
  ($spd, $dir) = Conversions::calculateWindFromUV(10, 0);
  $self->assertFormatString(10, $spd, "(10,0) wind speed");
  $self->assertFormatString(270, $dir, "(10,0) wind direction");
  ($spd, $dir) = Conversions::calculateWindFromUV(-10, 0);
  $self->assertFormatString(10, $spd, "(-10,0) wind speed");
  $self->assertFormatString(90, $dir, "(-10,0) wind direction");

  ($spd,$dir) = Conversions::calculateWindFromUV(-17.9890348863,.628190910645);
  $self->assertFormatString(18, $spd, "(-17.9,.628) wind speed");
  $self->assertFormatString(92, $dir, "(-17.9,.628) wind direction");

}



sub compDates {
  my $self = shift;

  my $start = "2004/05/14";
  my $end = "2004/05/15";
  my $double = "2004/05/14";

  $self->assertValue(-1, Conversions::compareDates($start, $end), "Start<End");
  $self->assertValue(1, Conversions::compareDates($end, $start), "End>Start");
  $self->assertValue(0, Conversions::compareDates($start, $double), 
		     "Start=Start");
  $self->assertUndef(Conversions::compareDates("20040405", $end),
		     "Bad date format");
}


sub convAngles {
  my $self = shift;
  my $pi = Conversions::pi();

  $self->assertFormatString("0", Conversions::degToRad(0), "0 -> rad");
  $self->assertFormatString($pi, Conversions::degToRad(180), "180 -> rad");
  $self->assertFormatString(2*$pi, Conversions::degToRad(360), "360 -> rad");

  $self->assertFormatString("0", Conversions::radToDeg(0), "0 -> deg");
  $self->assertFormatString("180", Conversions::radToDeg($pi), "PI -> deg");
  $self->assertFormatString("360", Conversions::radToDeg(2*$pi), "2PI -> deg");
}


sub convArea {
  my $self = shift;
  
  $self->assertFormatString(640, Conversions::convertArea(1,"mile2","acres"),
			    "mile2 -> acres");
  $self->assertFormatString(144, Conversions::convertArea(1, "ft2", "in2"),
			    "ft2 -> in2");
  $self->assertFormatString(10000, Conversions::convertArea(1, "m2", "cm2"),
			    "m2 -> cm2");

  $self->assertFormatString(1, Conversions::convertArea(1,"acres","acres"),
			    "acres -> acres");
  $self->assertFormatString(1, Conversions::convertArea(1, "cm2", "cm2"),
			    "cm2 -> cm2");
  $self->assertFormatString(1, Conversions::convertArea(1, "ft2", "ft2"),
			    "ft2 -> ft2");
  $self->assertFormatString(1, Conversions::convertArea(1, "in2", "in2"),
			    "in2 -> in2");
  $self->assertFormatString(1, Conversions::convertArea(1, "km2", "km2"),
			    "km2 -> km2");
  $self->assertFormatString(1, Conversions::convertArea(1, "m2", "m2"),
			    "m2 -> m2");
  $self->assertFormatString(1, Conversions::convertArea(1, "mm2", "mm2"),
			    "mm2 -> mm2");
  $self->assertFormatString(1, Conversions::convertArea(1, "mile2", "mile2"),
			    "mile2 -> mile2");
  $self->assertFormatString(1, Conversions::convertArea(1, "yd2", "yd2"),
			    "yd2 -> yd2");
  
}


sub convJulian {
  my $self = shift;
  my ($month, $day);

  ($month, $day) = Conversions::convertJulian(2003, 1);
  $self->assertValue(1, $month, "2003 001 -> 2003/01/01 month");
  $self->assertValue(1, $day, "2003 001 -> 2003/01/01 day");
  ($month, $day) = Conversions::convertJulian(2003, 365);
  $self->assertValue(12, $month, "2003 365 -> 2003/12/31 month");
  $self->assertValue(31, $day, "2003 365 -> 2003/12/31 day");
  ($month, $day) = Conversions::convertJulian(2004, 1);
  $self->assertValue(1, $month, "2004 001 -> 2004/01/01 month");
  $self->assertValue(1, $day, "2004 001 -> 2004/01/01 day");
  ($month, $day) = Conversions::convertJulian(2004, 366);
  $self->assertValue(12, $month, "2004 365 -> 2004/12/31 month");
  $self->assertValue(31, $day, "2004 365 -> 2004/12/31 day");

  $self->assertValue(1, Conversions::convertJulian(2003,1,1), 
		     "2003/01/01 -> 001");
  $self->assertValue(365, Conversions::convertJulian(2003,31,12), 
		     "2003/12/31 -> 365");
  $self->assertValue(1, Conversions::convertJulian(2004,1,1), 
		     "2004/01/01 -> 001");
  $self->assertValue(366, Conversions::convertJulian(2004,31,12), 
		     "2004/12/31 -> 366");
}


sub convLatLong {
  my $self = shift;
  
  $self->assertFormatString("76.111",
			    Conversions::convertLatLong(76.111, "DDDDDD"),
			    "D -> _");
  $self->assertFormatString("76.111",
			    Conversions::convertLatLong(76.111, "DDDDDD", "D"),
			    "D -> D");
  $self->assertFormatString("76.5041666666667",
			    Conversions::convertLatLong("76 30 15","DD MM SS"),
			    "DMS -> D");
  my ($deg, $min, $sec) = Conversions::convertLatLong("76 30 15", "DD MM SS",
						      "DMS");
  $self->assertString("76 30 15",
		      sprintf("%d %d %d", $deg, $min, $sec),
		      "DMS -> DMS");
  ($deg, $min, $sec) = Conversions::convertLatLong("76.5041667", "DDDDDDDDDD",
						   "DMS");
  $self->assertString("76 30 15",
		      sprintf("%d %d %d", $deg, $min, $sec),
		      "D -> DMS");
  
}

sub convLength {
  my $self = shift;

  # Common Conversion Factors
  $self->assertFormatString(100, Conversions::convertLength(1,"m","cm"), 
			    "m->cm");
  $self->assertFormatString(2.54, Conversions::convertLength(1,"in","cm"), 
			    "in->cm");
  $self->assertFormatString(5280, Conversions::convertLength(1,"mile","ft"),
			    "mile->ft");
  $self->assertFormatString(1000, Conversions::convertLength(1,"km","m"), 
			    "km->m");
  $self->assertFormatString(12, Conversions::convertLength(1,"ft","in"), 
			    "ft->in");

  # Make sure all the conversions undo themselves
  $self->assertFormatString(1, Conversions::convertLength(1, "cm", "cm"),
			    "cm->cm");
  $self->assertFormatString(1, Conversions::convertLength(1, "dm", "dm"),
			    "dm->dm");
  $self->assertFormatString(1, Conversions::convertLength(1, "ft", "ft"),
			    "ft->ft");
  $self->assertFormatString(1, Conversions::convertLength(1, "hi", "hi"),
			    "hi->hi");
  $self->assertFormatString(1, Conversions::convertLength(1, "Hft", "Hft"),
			    "Hft->Hft");
  $self->assertFormatString(1, Conversions::convertLength(1, "in", "in"),
			    "in->in");
  $self->assertFormatString(1, Conversions::convertLength(1, "km", "km"),
			    "km->km");
  $self->assertFormatString(1, Conversions::convertLength(1, "m", "m"),
			    "m->m");
  $self->assertFormatString(1, Conversions::convertLength(1, "mile", "mile"),
			    "mile->mile");
  $self->assertFormatString(1, Conversions::convertLength(1, "mm", "mm"),
			    "mm->mm");
  $self->assertFormatString(1, Conversions::convertLength(1, "nmile", "nmile"),
			    "nmile->nmile");
  $self->assertFormatString(1, Conversions::convertLength(1, "ti", "ti"),
			    "ti->ti");
  $self->assertFormatString(1, Conversions::convertLength(1, "yd", "yd"),
			    "yd->yd");
}

sub convTemp {
  my $self = shift;

  $self->assertFormatString("100", 
			    Conversions::convertTemperature(100, "C", "C"),
			    "C -> C");
  $self->assertFormatString("212", 
			    Conversions::convertTemperature(100, "C", "F"),
			    "C -> F");
  $self->assertFormatString("373.15", 
			    Conversions::convertTemperature(100,"C","K"),
			    "C -> K");
  $self->assertFormatString("373.15",
			    Conversions::convertTemperature(212, "F", "K"),
			    "F -> K");
#  Conversions::convertTemperature(100, "C", "M");
}


sub convVolume {
  my $self = shift;

  $self->assertFormatString(1, Conversions::convertVolume(1000000,"cm3","m3"),
			    "cm3 -> m3");
  $self->assertFormatString(12**3, Conversions::convertVolume(1,"ft3","in3"),
			    "ft3 -> m3");
  $self->assertFormatString(1, Conversions::convertVolume(1, "cm3", "ml"),
			    "cm3 -> ml");
  $self->assertFormatString(1, Conversions::convertVolume(1000, "ml", "liter"),
			    "ml -> liter");
  $self->assertFormatString(1, Conversions::convertVolume(4, "qt", "gal"),
			    "qt -> gal");
  $self->assertFormatString(1, Conversions::convertVolume(2, "pt", "qt"),
			    "pt -> qt");
  
  $self->assertFormatString(1, Conversions::convertVolume(1, "af", "af"),
			    "af -> af");
  $self->assertFormatString(1, Conversions::convertVolume(1, "cm3", "cm3"),
			    "cm3 -> cm3");
  $self->assertFormatString(1, Conversions::convertVolume(1, "ft3", "ft3"),
			    "ft3 -> ft3");
  $self->assertFormatString(1, Conversions::convertVolume(1, "gal", "gal"),
			    "gal -> gal");
  $self->assertFormatString(1, Conversions::convertVolume(1, "in3", "in3"),
			    "in3 -> in3");
  $self->assertFormatString(1, Conversions::convertVolume(1, "liter", "liter"),
			    "liter -> liter");
  $self->assertFormatString(1, Conversions::convertVolume(1, "m3", "m3"),
			    "m3 -> m3");
  $self->assertFormatString(1, Conversions::convertVolume(1, "ml", "ml"),
			    "ml -> ml");
  $self->assertFormatString(1, Conversions::convertVolume(1, "oz", "oz"),
			    "oz -> oz");
  $self->assertFormatString(1, Conversions::convertVolume(1, "pt", "pt"),
			    "pt -> pt");
  $self->assertFormatString(1, Conversions::convertVolume(1, "qt", "qt"),
			    "qt -> qt");
}

sub convVelocity {
  my $self = shift;

  $self->assertFormatString(.3048,Conversions::convertVelocity(1,"ft/s","m/s"),
			    "ft/s -> m/s");
  $self->assertFormatString(4.4,Conversions::convertVelocity(3,"mph","ft/s"),
			    "mph -> ft/s");
  $self->assertFormatString(3.6,Conversions::convertVelocity(1,"m/s","km/hr"),
			    "m/s -> km/hr");

  $self->assertFormatString(1, Conversions::convertVelocity(1,"ft/s","ft/s"),
			    "ft/s -> ft/s");
  $self->assertFormatString(1, Conversions::convertVelocity(1,"km/hr","km/hr"),
			    "km/hr -> km/hr");
  $self->assertFormatString(1, Conversions::convertVelocity(1,"knot", "knot"),
			    "knot -> knot");
  $self->assertFormatString(1, Conversions::convertVelocity(1, "m/s", "m/s"),
			    "m/s", "m/s");
  $self->assertFormatString(1, Conversions::convertVelocity(1,"mi/hr","mi/hr"),
			    "mi/hr -> mi/hr");
  $self->assertFormatString(1, Conversions::convertVelocity(1, "mph", "mph"),
			    "mph -> mph");
}

sub convTime {
  my $self = shift;

  $self->assertFormatString(24, Conversions::convertTime(1, "day", "hr"),
			    "day -> hr");
  $self->assertFormatString(60, Conversions::convertTime(1, "min", "sec"),
			    "min -> sec");
  $self->assertFormatString(60, Conversions::convertTime(1, "hr", "min"),
			    "hr -> min");

  $self->assertFormatString(1, Conversions::convertTime(1, "day", "day"),
			    "day -> day");
  $self->assertFormatString(1, Conversions::convertTime(1, "hr", "hr"),
			    "hr -> hr");
  $self->assertFormatString(1, Conversions::convertTime(1, "min", "min"),
			    "min -> min");
  $self->assertFormatString(1, Conversions::convertTime(1, "sec", "sec"),
			    "sec -> sec");
}

sub convRad {
  my $self = shift;

  $self->assertFormatString(10, Conversions::convertRadiation(1, "langly", "w/m2"), "langly -> w/m2");

  $self->assertFormatString(1, Conversions::convertRadiation(1, "langly", "langly"), "langly -> langly");
  $self->assertFormatString(1, Conversions::convertRadiation(1, "w/m2", "w/m2"), "w/m2 -> w/m2");
}

sub convPress {
  my $self = shift;

  $self->assertFormatString(1, Conversions::convertPressure(1013.25, "mb", "atm"), "mb -> atm");
  $self->assertFormatString(1, Conversions::convertPressure(101.325, "kPa", "atm"), "kPa -> atm");
  $self->assertFormatString(1, Conversions::convertPressure(1000, "mbar", "bar"), "mbar -> bar");
  $self->assertFormatString(1, Conversions::convertPressure(1000, "Pa", "kPa"), "Pa -> kPa");

  $self->assertFormatString(1, Conversions::convertPressure(1, "atm", "atm"), "atm -> atm");
  $self->assertFormatString(1, Conversions::convertPressure(1, "bar", "bar"), "bar -> bar");
  $self->assertFormatString(1, Conversions::convertPressure(1, "dyne/cm2", "dyne/cm2"), "dyne/cm2 -> dyne/cm2");
  $self->assertFormatString(1, Conversions::convertPressure(1, "hPa", "hPa"), "hPa -> hPa");
  $self->assertFormatString(1, Conversions::convertPressure(1, "inHg", "inHg"), "inHg -> inHg");
  $self->assertFormatString(1, Conversions::convertPressure(1, "kPa", "kPa"), "kPa -> kPa");
  $self->assertFormatString(1, Conversions::convertPressure(1, "mb", "mb"), "mb -> mb");
  $self->assertFormatString(1, Conversions::convertPressure(1, "mbar", "mbar"), "mbar -> mbar");
  $self->assertFormatString(1, Conversions::convertPressure(1, "mmHg", "mmHg"), "mmHg -> mmHg");
  $self->assertFormatString(1, Conversions::convertPressure(1, "Pa", "Pa"), "Pa -> Pa");
}

sub convFlow {
  my $self = shift;

}


sub daysFeb {
  my $self = shift;

  $self->assertValue(28, Conversions::daysInFeb(2003), "2003 Feb");
  $self->assertValue(29, Conversions::daysInFeb(2004), "2004 Feb");
  $self->assertValue(29, Conversions::daysInFeb(2000), "2000 Feb");
  $self->assertValue(28, Conversions::daysInFeb(1900), "1900 Feb");
}

sub formats {
  my $self = shift;

  $self->assertString("2004/05/19", 
		      Conversions::formatDate("2004/05/19","YYYY/MM/DD"),
		      "YYYY/MM/DD");
  $self->assertString("2004/05/19",
		      Conversions::formatDate("5-19-2004", "M-DD-YYYY"),
		      "M-DD-YYYY");
  $self->assertString("2004/12/31",
		      Conversions::formatDate("2004366", "YYYYJJJ"),
		      "YYYYJJJ");

  $self->assertString("23:59",
		      Conversions::formatTime("23:59", "HH:MM"), "HH:MM");
  $self->assertString("23:59",
		      Conversions::formatTime("2359", "HHMM"), "HHMM");
  $self->assertString("23:00",
		      Conversions::formatTime("23", "HH"), "HH");
}

sub stateCode {
  my $self = shift;

  $self->assertValue(27, Conversions::getStateCode("MN"));
  $self->assertString("MN", Conversions::getStateCode(27));
}

sub unixTime {
  
}

sub valids {
  my $self = shift;

  $self->assertValue(1, Conversions::validDate("2004/05/19"), "Valid date");
  $self->assertValue(0, Conversions::validDate("04/05/19"),"Invalid date fmt");
  $self->assertValue(0, Conversions::validDate("2004/00/19"), "0 month");
  $self->assertValue(0, Conversions::validDate("2004/13/19"), "13 month");
  $self->assertValue(0, Conversions::validDate("2003/06/00"), "0 day");
  $self->assertValue(0, Conversions::validDate("2003/06/31"), "6/31 day");
  $self->assertValue(0, Conversions::validDate("2003/02/29"), "bad leap year");

  $self->assertValue(1, Conversions::validTime("00:00"), "Valid time");
  $self->assertValue(0, Conversions::validTime("0000"), "Invalid time format");
  $self->assertValue(0, Conversions::validTime("-1:00"), "negative hour");
  $self->assertValue(0, Conversions::validTime("24:00"), "24 hour");
  $self->assertValue(0, Conversions::validTime("00:-1"), "negative min");
  $self->assertValue(0, Conversions::validTime("00:60"), "60 minute");  
}

1;
