#! /usr/bin/perl -w

package TestDpgConversions;
use strict;
use lib ".";
use DpgConstants qw(:DEFAULT);
use DpgConversions qw(:DEFAULT);
use TestModule;
our @ISA = ("TestModule");
&main();

sub main {
  my $tester = TestDpgConversions->new();

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
  $tester->stateCode();
}

sub convAngles {
  my $self = shift;

  $self->assertValue("0", convertAngle(0,"deg","rad"), "0 -> rad");
  $self->assertValue($PI, convertAngle(180,"deg","rad"), "180 -> rad");
  $self->assertValue(2*$PI, convertAngle(360,"deg","rad"), "360 -> rad");

  $self->assertValue("0", convertAngle(0,"rad","deg"), "0 -> deg");
  $self->assertValue("180", convertAngle($PI,"rad","deg"), "PI -> deg");
  $self->assertValue("360", convertAngle(2*$PI,"rad","deg"), "2PI -> deg");
}


sub convArea {
  my $self = shift;
  
  $self->assertValue(640, convertArea(1,"mile2","acres"),
			    "mile2 -> acres");
  $self->assertValue(144, convertArea(1, "ft2", "in2"),
			    "ft2 -> in2");
  $self->assertValue(10000, convertArea(1, "m2", "cm2"),
			    "m2 -> cm2");

  $self->assertValue(1, convertArea(1,"acres","acres"),
			    "acres -> acres");
  $self->assertValue(1, convertArea(1, "cm2", "cm2"),
			    "cm2 -> cm2");
  $self->assertValue(1, convertArea(1, "ft2", "ft2"),
			    "ft2 -> ft2");
  $self->assertValue(1, convertArea(1, "in2", "in2"),
			    "in2 -> in2");
  $self->assertValue(1, convertArea(1, "km2", "km2"),
			    "km2 -> km2");
  $self->assertValue(1, convertArea(1, "m2", "m2"),
			    "m2 -> m2");
  $self->assertValue(1, convertArea(1, "mm2", "mm2"),
			    "mm2 -> mm2");
  $self->assertValue(1, convertArea(1, "mile2", "mile2"),
			    "mile2 -> mile2");
  $self->assertValue(1, convertArea(1, "yd2", "yd2"),
			    "yd2 -> yd2");
  
}


sub convLatLong {
  my $self = shift;
  
  $self->assertValue("76.111",
			    convertLatLong(76.111,"DDDDDD","D"),
			    "D -> D");
  $self->assertValue("76.5041666666667",
			    convertLatLong("76 30 15","DD MM SS","D"),
			    "DMS -> D");
  my ($deg, $min, $sec) = convertLatLong("76 30 15", "DD MM SS",
						      "DMS");
  $self->assertString("76 30 15",
		      sprintf("%d %d %d", $deg, $min, $sec),
		      "DMS -> DMS");
  ($deg, $min, $sec) = convertLatLong("76.5041667", "DDDDDDDDDD",
						   "DMS");
  $self->assertString("76 30 15",
		      sprintf("%d %d %d", $deg, $min, $sec),
		      "D -> DMS");

  $self->assertString("-76 30.25",sprintf("%s %s",convertLatLong("-76 30 15","-DD MM SS","DM")),"DMS -> DM");
}

sub convLength {
  my $self = shift;

  # Common Conversion Factors
  $self->assertValue(100, convertLength(1,"m","cm"), 
			    "m->cm");
  $self->assertValue(2.54, convertLength(1,"in","cm"), 
			    "in->cm");
  $self->assertValue(5280, convertLength(1,"mile","ft"),
			    "mile->ft");
  $self->assertValue(1000, convertLength(1,"km","m"), 
			    "km->m");
  $self->assertValue(12, convertLength(1,"ft","in"), 
			    "ft->in");

  # Make sure all the conversions undo themselves
  $self->assertValue(1, convertLength(1, "cm", "cm"),
			    "cm->cm");
  $self->assertValue(1, convertLength(1, "dm", "dm"),
			    "dm->dm");
  $self->assertValue(1, convertLength(1, "ft", "ft"),
			    "ft->ft");
  $self->assertValue(1, convertLength(1, "hi", "hi"),
			    "hi->hi");
  $self->assertValue(1, convertLength(1, "Hft", "Hft"),
			    "Hft->Hft");
  $self->assertValue(1, convertLength(1, "in", "in"),
			    "in->in");
  $self->assertValue(1, convertLength(1, "km", "km"),
			    "km->km");
  $self->assertValue(1, convertLength(1, "m", "m"),
			    "m->m");
  $self->assertValue(1, convertLength(1, "mile", "mile"),
			    "mile->mile");
  $self->assertValue(1, convertLength(1, "mm", "mm"),
			    "mm->mm");
  $self->assertValue(1, convertLength(1, "nmile", "nmile"),
			    "nmile->nmile");
  $self->assertValue(1, convertLength(1, "ti", "ti"),
			    "ti->ti");
  $self->assertValue(1, convertLength(1, "yd", "yd"),
			    "yd->yd");
}

sub convTemp {
  my $self = shift;

  $self->assertValue("100", 
			    convertTemperature(100, "C", "C"),
			    "C -> C");
  $self->assertValue("212", 
			    convertTemperature(100, "C", "F"),
			    "C -> F");
  $self->assertValue("373.15", 
			    convertTemperature(100,"C","K"),
			    "C -> K");
  $self->assertValue("373.15",
			    convertTemperature(212, "F", "K"),
			    "F -> K");
#  convertTemperature(100, "C", "M");
}


sub convVolume {
  my $self = shift;

  $self->assertValue(1, convertVolume(1000000,"cm3","m3"),"cm3 -> m3");
  $self->assertValue(12**3, convertVolume(1,"ft3","in3"),"ft3 -> m3");
  $self->assertValue(1, convertVolume(1, "cm3", "ml"),"cm3 -> ml");
  $self->assertValue(1, convertVolume(1000, "ml", "liter"),"ml -> liter");
  $self->assertValue(1, convertVolume(4, "qt", "gal"),"qt -> gal");
  $self->assertValue(1, convertVolume(2, "pt", "qt"),"pt -> qt");
  
  $self->assertValue(1, convertVolume(1, "af", "af"),"af -> af");
  $self->assertValue(1, convertVolume(1, "cm3", "cm3"),"cm3 -> cm3");
  $self->assertValue(1, convertVolume(1, "ft3", "ft3"),"ft3 -> ft3");
  $self->assertValue(1, convertVolume(1, "gal", "gal"),"gal -> gal");
  $self->assertValue(1, convertVolume(1, "in3", "in3"),"in3 -> in3");
  $self->assertValue(1, convertVolume(1, "liter", "liter"),"liter -> liter");
  $self->assertValue(1, convertVolume(1, "m3", "m3"),"m3 -> m3");
  $self->assertValue(1, convertVolume(1, "ml", "ml"),"ml -> ml");
  $self->assertValue(1, convertVolume(1, "oz", "oz"),"oz -> oz");
  $self->assertValue(1, convertVolume(1, "pt", "pt"),"pt -> pt");
  $self->assertValue(1, convertVolume(1, "qt", "qt"),"qt -> qt");
}

sub convVelocity {
  my $self = shift;

  $self->assertValue(.3048,convertVelocity(1,"ft/s","m/s"),"ft/s -> m/s");
  $self->assertValue(4.4,convertVelocity(3,"mph","ft/s"),"mph -> ft/s");
  $self->assertValue(3.6,convertVelocity(1,"m/s","km/hr"),"m/s -> km/hr");

  $self->assertValue(1, convertVelocity(1,"ft/s","ft/s"),"ft/s -> ft/s");
  $self->assertValue(1, convertVelocity(1,"km/hr","km/hr"),"km/hr -> km/hr");
  $self->assertValue(1, convertVelocity(1,"knot", "knot"),"knot -> knot");
  $self->assertValue(1, convertVelocity(1, "m/s", "m/s"),"m/s", "m/s");
  $self->assertValue(1, convertVelocity(1,"mi/hr","mi/hr"),"mi/hr -> mi/hr");
  $self->assertValue(1, convertVelocity(1, "mph", "mph"),"mph -> mph");
}

sub convTime {
  my $self = shift;

  $self->assertValue(24, convertTime(1, "day", "hr"),"day -> hr");
  $self->assertValue(60, convertTime(1, "min", "sec"),"min -> sec");
  $self->assertValue(60, convertTime(1, "hr", "min"),"hr -> min");

  $self->assertValue(1, convertTime(1, "day", "day"),"day -> day");
  $self->assertValue(1, convertTime(1, "hr", "hr"),"hr -> hr");
  $self->assertValue(1, convertTime(1, "min", "min"),"min -> min");
  $self->assertValue(1, convertTime(1, "sec", "sec"),"sec -> sec");
}

sub convRad {
  my $self = shift;

  $self->assertValue(10, convertRadiation(1, "langly", "w/m2"), "langly -> w/m2");

  $self->assertValue(1, convertRadiation(1, "langly", "langly"), "langly -> langly");
  $self->assertValue(1, convertRadiation(1, "w/m2", "w/m2"), "w/m2 -> w/m2");
}

sub convPress {
  my $self = shift;

  $self->assertValue(1, convertPressure(1013.25, "mb", "atm"), "mb -> atm");
  $self->assertValue(1, convertPressure(101.325, "kPa", "atm"), "kPa -> atm");
  $self->assertValue(1, convertPressure(1000, "mbar", "bar"), "mbar -> bar");
  $self->assertValue(1, convertPressure(1000, "Pa", "kPa"), "Pa -> kPa");

  $self->assertValue(1, convertPressure(1, "atm", "atm"), "atm -> atm");
  $self->assertValue(1, convertPressure(1, "bar", "bar"), "bar -> bar");
  $self->assertValue(1, convertPressure(1, "dyne/cm2", "dyne/cm2"), "dyne/cm2 -> dyne/cm2");
  $self->assertValue(1, convertPressure(1, "hPa", "hPa"), "hPa -> hPa");
  $self->assertValue(1, convertPressure(1, "inHg", "inHg"), "inHg -> inHg");
  $self->assertValue(1, convertPressure(1, "kPa", "kPa"), "kPa -> kPa");
  $self->assertValue(1, convertPressure(1, "mb", "mb"), "mb -> mb");
  $self->assertValue(1, convertPressure(1, "mbar", "mbar"), "mbar -> mbar");
  $self->assertValue(1, convertPressure(1, "mmHg", "mmHg"), "mmHg -> mmHg");
  $self->assertValue(1, convertPressure(1, "Pa", "Pa"), "Pa -> Pa");
}

sub convFlow {
  my $self = shift;

}


sub stateCode {
  my $self = shift;

  $self->assertValue(27, getStateCodeFromFile("MN"));
  $self->assertString("MN", getStateCodeFromFile(27));
}

1;
