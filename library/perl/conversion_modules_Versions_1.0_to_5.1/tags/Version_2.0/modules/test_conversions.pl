#! /usr/bin/perl -w

#---------------------------------------------------------------
# This is a script that tests the conversions and formatting in
# the Conversions.pm module.  This should be run after any
# modification to the Conversions.pm module.  It will check
# to see if the modification broke something that would not be
# expected.  The modification should also be added to the suite of
# tests to ensure that it does what it is expected to do.  Also, if 
# a bug is found in the module, the tests will need to be fixed
# to correct for the bug.
#
# Last Updated: 7/3/2003 JEC
# Created: 6/29/2003 JEC
#---------------------------------------------------------------
use strict;
use lib ".";
use Conversions;

&main();

# Run all of the tests.
sub main() {
    my $missing = Conversions::getMissing();
    &testCalculateDewPoint($missing);
    &testCalculateSeaLevelPressure($missing);
    &testCompareDates();
    &testConvertDate();
    &testConvertJulian();
    &testConvertLatLong();
    &testConvertLength();
    &testConvertPressure();
    &testConvertTemperature();
    &testConvertTime();
    &testConvertVelocity();
    &testDaysInFeb();
    &testGetNextDay();
    &testValidDate();
    &testValidTime();
}

sub testCalculateDewPoint() {
    my $missing = shift;
    printf "Testing Conversions::calculateDewPoint()\n";

    my $result = Conversions::calculateDewPoint($missing, 67.0);
    if ($result ne $missing) {
	printf("\tExpected %s, but was %s\n", $missing, $result);
    }

    $result = Conversions::calculateDewPoint(22.1, $missing);
    if ($result ne $missing) {
	printf("\tExpected %s, but was %s\n", $missing, $result);
    }

    $result = Conversions::calculateDewPoint(-101, 50);
    if ($result ne $missing) {
	printf("\tExpected %s, but was %s\n", $missing, $result);
    }

    $result = Conversions::calculateDewPoint(101, 50);
    if ($result ne $missing) {
	printf("\tExpected %s, but was %s\n", $missing, $result);
    }

    $result = Conversions::calculateDewPoint(23, 0);
    if ($result ne $missing) {
	printf("\tExpected %s, but was %s\n", $missing, $result);
    }

    $result = Conversions::calculateDewPoint(23, 101);
    if ($result ne $missing) {
	printf("\tExpected %s, but was %s\n", $missing, $result);
    }

    $result = sprintf("%20.10f", Conversions::calculateDewPoint(22.1, 67.0));
    if (sprintf("%20.10f", "15.6924342762555") ne $result) {
	printf("\tExpected %s, but was %s\n", 15.6924342762555, $result);
    }
}

sub testCalculateSeaLevelPressure {
    my $result;
    
    printf("***** WARNING *****  Tests for Conversions::calculateSeaLevelPressure() not implemented\n");
}

sub testCompareDates {
    my $date1 = "2001/12/31";
    my $date2 = "2002/06/30";
    my $date3 = "2002/07/01";
    my $result;

    printf("Testing Conversions::compareDates()\n");

    $result = Conversions::compareDates($date1, $date2);
    if (-1 != $result) {
	printf("\tExpected %d, but was %d\n", -1, $result);
    }

    $result = Conversions::compareDates($date1, $date3);
    if (-1 != $result) {
	printf("\tExpected %d, but was %d\n", -1, $result);
    }

    $result = Conversions::compareDates($date2, $date3);
    if (-1 != $result) {
	printf("\tExpected %d, but was %d\n", -1, $result);
    }

    $result = Conversions::compareDates($date1, $date1);
    if (0 != $result) {
	printf("\tExpected %d, but was %d\n", 0, $result);
    }

    $result = Conversions::compareDates($date2, $date2);
    if (0 != $result) {
	printf("\tExpected %d, but was %d\n", 0, $result);
    }

    $result = Conversions::compareDates($date3, $date3);
    if (0 != $result) {
	printf("\tExpected %d, but was %d\n", 0, $result);
    }
    
    $result = Conversions::compareDates($date2, $date1);
    if (1 != $result) {
	printf("\tExpected %d, but was %d\n", 0, $result);
    }
    
    $result = Conversions::compareDates($date3, $date1);
    if (1 != $result) {
	printf("\tExpected %d, but was %d\n", 0, $result);
    }
    
    $result = Conversions::compareDates($date3, $date2);
    if (1 != $result) {
	printf("\tExpected %d, but was %d\n", 0, $result);
    }
    
}

sub testConvertDate {
    my $result;

    printf("Testing Conversions::convertDate()\n");

    # The following command should die
    #$result = Conversions::convertDate("020202", "YYMMDD");

    $result = Conversions::convertDate("2003/6/30", "YYYY/M/DD");
    if ($result ne "2003/06/30") {
	printf("\tExpected %s, but was %s\n", "2003/06/30", $result);
    }

    $result = Conversions::convertDate("2003/6/3", "YYYY/M/D");
    if ($result ne "2003/06/03") {
	printf("\tExpected %s, but was %s\n", "2003/06/03", $result);
    }

    $result = Conversions::convertDate("200363", "YYYYMD");
    if ($result ne "2003/06/03") {
	printf("\tExpected %s, but was %s\n", "2003/06/03", $result);
    }

    $result = Conversions::convertDate("6-30-2003", "M-DD-YYYY");
    if ($result ne "2003/06/30") {
	printf("\tExpected %s, but was %s\n", "2003/06/30", $result);
    }

    $result = Conversions::convertDate("2003001", "YYYYJJJ");
    if ($result ne "2003/01/01") {
	printf("\tExpected %s, but was %s\n", "2003/01/01", $result);
    }

    $result = Conversions::convertDate("2003365", "YYYYJJJ");
    if ($result ne "2003/12/31") {
	printf("\tExpected %s, but was %s\n", "2003/12/31", $result);
    }
}

sub testConvertJulian {
    my $month;
    my $day;

    printf("Testing Conversions::convertJulian()\n");
    
    ($month, $day) = Conversions::convertJulian(2003, 181);
    if ($month != 06 || $day != 30) {
	printf("\tExpected %d/%d, but was %d/%d\n", 6, 30, $month, $day);
    }

    ($month, $day) = Conversions::convertJulian(2004, 181);
    if ($month != 6 || $day != 29) {
	printf("\tExpected %d/%d, but was %d/%d\n", 6, 29, $month, $day);
    }

    ($month, $day) = Conversions::convertJulian(2000, 181);
    if ($month != 06 || $day != 29) {
	printf("\tExpected %d/%d, but was %d/%d\n", 6, 29, $month, $day);
    }

    ($month, $day) = Conversions::convertJulian(2100, 365);
    if ($month != 12 || $day != 31) {
	printf("\tExpected %d/%d, but was %d/%d\n", 12, 31, $month, $day);
    }
}

sub testConvertLatLong {
    my $result;

    printf("Testing Conversions::convertLatLong()\n");

    $result = sprintf("%11.5f",
		    Conversions::convertLatLong(67.09865, "DDDDDDDD"));
    if ($result ne sprintf("%11.5f", "67.09865")) {
	printf("\tExpected %11.5f, but was %s\n", "67.09865", $result);
    }

    $result = sprintf("%11.5f",
		    Conversions::convertLatLong("67 09.865", "DD MMMMMM"));
    if ($result ne sprintf("%11.5f", "67.1644166667")) {
	printf("\tExpected %11.5f, but was %s\n", "67.1644166667", $result);
    }

    $result = sprintf("%11.5f",
		    Conversions::convertLatLong("67 09 44", "DD MM SS"));
    if ($result ne sprintf("%11.5f", "67.1622222222")) {
	printf("\tExpected %11.5f, but was %s\n", "67.1622222222", $result);
    }
    $result = sprintf("%11.5f",
		    Conversions::convertLatLong("-67 09 44", "-DD MM SS"));
    if ($result ne sprintf("%11.5f", "-67.1622222222")) {
	printf("\tExpected %11.5f, but was %s\n", "-67.1622222222", $result);
    }
}

sub testConvertLength {
    my $result;

    printf("Testing Conversions::convertLength()\n");

    $result = sprintf("%20.10f", Conversions::convertLength(1, "cm", "in"));
    if ($result ne sprintf("%20.10f", ".393700787402")) {
	printf("\tExpected %20.10f, but was %20.10f\n", ".393700787402", $result);
    }

    $result = sprintf("%20.10f", Conversions::convertLength(10, "mm", "in"));
    if ($result ne sprintf("%20.10f", ".393700787402")) {
	printf("\tExpected %20.10f, but was %20.10f\n", ".393700787402", $result);
    }

    $result = sprintf("%20.10f", Conversions::convertLength(.01, "m", "in"));
    if ($result ne sprintf("%20.10f", ".393700787402")) {
	printf("\tExpected %20.10f, but was %20.10f\n", ".393700787402", $result);
    }

    $result = sprintf("%20.10f", Conversions::convertLength(.00001, 
							      "km", "in"));
    if ($result ne sprintf("%20.10f", ".393700787402")) {
	printf("\tExpected %20.10f, but was %20.10f\n", ".393700787402", $result);
    }

    $result = sprintf("%20.10f", Conversions::convertLength(12, "in", "cm"));
    if ($result ne sprintf("%20.10f", "30.48")) {
	printf("\tExpected %20.10f, but was %20.10f\n", "30.48", $result);
    }

    $result = sprintf("%20.10f", Conversions::convertLength(1, "ft", "cm"));
    if ($result ne sprintf("%20.10f", "30.48")) {
	printf("\tExpected %20.10f, but was %20.10f\n", "30.48", $result);
    }
    
    $result = sprintf("%20.10f", Conversions::convertLength(1, "yd", "cm"));
    if ($result ne sprintf("%20.10f", "91.44")) {
	printf("\tExpected %20.10f, but was %20.10f\n", "91.44", $result);
    }

    $result = sprintf("%20.10f", Conversions::convertLength(1, "mile", "ft"));
    if ($result ne sprintf("%20.10f", "5280")) {
	printf("\tExpected %20.10f, but was %20.10f\n", "5280", $result);
    }

    $result = sprintf("%20.10f", Conversions::convertLength(1, "st-mile", "ft"));
    if ($result ne sprintf("%20.10f", "5280")) {
	printf("\tExpected %20.10f, but was %20.10f\n", "5280", $result);
    }

    $result = sprintf("%20.10f", Conversions::convertLength(5280, "ft", "mile"));
    if ($result ne sprintf("%20.10f", "1")) {
	printf("\tExpected %20.10f, but was %20.10f\n", "1", $result);
    }

    $result = sprintf("%20.10f", Conversions::convertLength(5280, "ft", "st-mile"));
    if ($result ne sprintf("%20.10f", "1")) {
	printf("\tExpected %20.10f, but was %20.10f\n", "1", $result);
    }

    $result = sprintf("%20.10f", Conversions::convertLength(3, "ft", "yd"));
    if ($result ne sprintf("%20.10f", "1")) {
	printf("\tExpected %20.10f, but was %20.10f\n", "1", $result);
    }

    $result = sprintf("%20.10f", Conversions::convertLength(1000, "m", "km"));
    if ($result ne sprintf("%20.10f", "1")) {
	printf("\tExpected %20.10f, but was %20.10f\n", "1", $result);
    }

    $result = sprintf("%20.10f", Conversions::convertLength(1, "km", "m"));
    if ($result ne sprintf("%20.10f", "1000")) {
	printf("\tExpected %20.10f, but was %20.10f\n", "1000", $result);
    }

    $result = sprintf("%20.10f", Conversions::convertLength(1, "cm", "mm"));
    if ($result ne sprintf("%20.10f", "10")) {
	printf("\tExpected %20.10f, but was %20.10f\n", "10", $result);
    }
}

sub testConvertPressure {
    my $result;

    printf("Testing Conversions::convertPressure()\n");

    $result = sprintf("%20.10f", Conversions::convertPressure(1, "atm", "bar"));
    if ($result ne sprintf("%20.10f", "1.01325")) {
	printf("\tExpected %20.10f, but was %20.10f\n", "1.01325", $result);
    }

    $result = sprintf("%20.10f", Conversions::convertPressure(1, "atm", "Pa"));
    if ($result ne sprintf("%20.10f", "101325")) {
	printf("\tExpected %20.10f, but was %20.10f\n", "101325", $result);
    }

    $result = sprintf("%20.10f", Conversions::convertPressure(1, "atm", "kPa"));
    if ($result ne sprintf("%20.10f", "101.325")) {
	printf("\tExpected %20.10f, but was %20.10f\n", "101.325", $result);
    }

    $result = sprintf("%20.10f", Conversions::convertPressure(1, "atm", "mmHg"));
    if ($result ne sprintf("%20.10f", "760")) {
	printf("\tExpected %20.10f, but was %20.10f\n", "760", $result);
    }

    $result = sprintf("%20.10f", Conversions::convertPressure(1, "atm", "inHg"));
    if ($result ne sprintf("%20.10f", "29.9212598425")) {
	printf("\tExpected %20.10f, but was %20.10f\n", "29.9212598425", $result);
    }

    $result = sprintf("%20.10f", Conversions::convertPressure(1, "atm", "mb"));
    if ($result ne sprintf("%20.10f", "1013.25")) {
	printf("\tExpected %20.10f, but was %20.10f\n", "1013.25", $result);
    }

    $result = sprintf("%20.10f", Conversions::convertPressure(1, "atm", "mbar"));
    if ($result ne sprintf("%20.10f", "1013.25")) {
	printf("\tExpected %20.10f, but was %20.10f\n", "1013.25", $result);
    }

    $result = sprintf("%20.10f", Conversions::convertPressure(1, "bar", "atm"));
    if ($result ne sprintf("%20.10f", ".986923266716")) {
	printf("\tExpected %20.10f, but was %20.10f\n", ".986923266716", $result);
    }

    $result = sprintf("%20.10f", Conversions::convertPressure(1, "mmHg", "bar"));
    if ($result ne sprintf("%20.10f", ".001333223684")) {
	printf("\tExpected %20.10f, but was %20.10f\n", ".001333223684", $result);
    }

    $result = sprintf("%20.10f", Conversions::convertPressure(1, "inHg", "bar"));
    if ($result ne sprintf("%20.10f", ".033863881579")) {
	printf("\tExpected %20.10f, but was %20.10f\n", ".033863881579", $result);
    }

    $result = sprintf("%20.10f", Conversions::convertPressure(1, "Pa", "bar"));
    if ($result ne sprintf("%20.10f", ".00001")) {
	printf("\tExpected %20.10f, but was %20.10f\n", ".00001", $result);
    }

    $result = sprintf("%20.10f", Conversions::convertPressure(1, "kPa", "bar"));
    if ($result ne sprintf("%20.10f", ".01")) {
	printf("\tExpected %20.10f, but was %20.10f\n", ".01", $result);
    }

    $result = sprintf("%20.10f", Conversions::convertPressure(1, "mb", "bar"));
    if ($result ne sprintf("%20.10f", ".001")) {
	printf("\tExpected %20.10f, but was %20.10f\n", ".001", $result);
    }

    $result = sprintf("%20.10f", Conversions::convertPressure(1, "mbar", "bar"));
    if ($result ne sprintf("%20.10f", ".001")) {
	printf("\tExpected %20.10f, but was %20.10f\n", ".001", $result);
    }
}

sub testConvertTemperature {
    my $result;

    printf("Testing Conversions::convertTemperature()\n");

    $result = Conversions::convertTemperature(0, "C", "F");
    if ($result != 32) {
	printf("\tExpected %d, but was %d\n", 32, $result);
    }

    $result = Conversions::convertTemperature(0, "C", "K");
    if ($result != 273.15) {
	printf("\tExpected %d, but was %d\n", 273.15, $result);
    }

    $result = Conversions::convertTemperature(32, "F", "K");
    if ($result != 273.15) {
	printf("\tExpected %d, but was %d\n", 273.15, $result);
    }

    $result = Conversions::convertTemperature(32, "F", "C");
    if ($result != 0) {
	printf("\tExpected %d, but was %d\n", 0, $result);
    }

    $result = Conversions::convertTemperature(273.15, "K", "F");
    if ($result != 32) {
	printf("\tExpected %d, but was %d\n", 32, $result);
    }

    $result = Conversions::convertTemperature(273.15, "K", "C");
    if ($result != 0) {
	printf("\tExpected %d, but was %d\n", 0, $result);
    }
}

sub testConvertTime {
    my $result;

    printf("Testing Conversions::convertTime()\n");
        
    $result = Conversions::convertTime("4.30", "H.MM");
    if ($result ne "04:30") {
	printf("\tExpected %s, but was %s\n", "04:30", $result);
    }

    $result = Conversions::convertTime("23:59", "HH:MM");
    if ($result ne "23:59") {
	printf("\tExpected %s, but was %s\n", "23:59", $result);
    }
}

sub testConvertVelocity {
    my $result;

    printf("Testing Conversions::convertVelocity()\n");

    $result = sprintf("%20.10f", Conversions::convertVelocity(1, "m/s", "mph"));
    if ($result ne sprintf("%20.10f", "2.23693629205")) {
	printf("\tExpected %20.10f, but was %20.10f\n", "2.23693629205", $result);
    }

    $result = sprintf("%20.10f", Conversions::convertVelocity(1, "m/s", "mi/hr"));
    if ($result ne sprintf("%20.10f", "2.23693629205")) {
	printf("\tExpected %20.10f, but was %20.10f\n", "2.23693629205", $result);
    }

    $result = sprintf("%20.10f", Conversions::convertVelocity(1, "m/s", "km/hr"));
    if ($result ne sprintf("%20.10f", "3.6")) {
	printf("\tExpected %20.10f, but was %20.10f\n", "3.6", $result);
    }

    $result = sprintf("%20.10f", Conversions::convertVelocity(1, "m/s", "ft/s"));
    if ($result ne sprintf("%20.10f", "3.28083989501")) {
	printf("\tExpected %20.10f, but was %20.10f\n", "3.28083989501", $result);
    }

    $result = sprintf("%20.10f", Conversions::convertVelocity(1, "ft/s", "m/s"));
    if ($result ne sprintf("%20.10f", ".3048")) {
	printf("\tExpected %20.10f, but was %20.10f\n", ".3048", $result);
    }

    $result = sprintf("%20.10f", Conversions::convertVelocity(1, "mi/hr", "mph"));
    if ($result ne sprintf("%20.10f", "1")) {
	printf("\tExpected %20.10f, but was %20.10f\n", "1", $result);
    }

    $result = sprintf("%20.10f", Conversions::convertVelocity(1, "mph", "m/s"));
    if ($result ne sprintf("%20.10f", ".44704")) {
	printf("\tExpected %20.10f, but was %20.10f\n", ".44704", $result);
    }

    $result = sprintf("%20.10f", Conversions::convertVelocity(1, "mph", "km/hr"));
    if ($result ne sprintf("%20.10f", "1.609344")) {
	printf("\tExpected %20.10f, but was %20.10f\n", "1.609344", $result);
    }
}

sub testDaysInFeb {
    my $result;

    print("Testing Conversions::days_in_feb()\n");
    
    $result = Conversions::days_in_feb(2003);
    if ($result != 28) {
	printf("\tExpected %d, but was %d\n", 28, $result);
    }

    $result = Conversions::days_in_feb(2000);
    if ($result != 29) {
	printf("\tExpected %d, but was %d\n", 29, $result);
    }

    $result = Conversions::days_in_feb(2004);
    if ($result != 29) {
	printf("\tExpected %d, but was %d\n", 29, $result);
    }

    $result = Conversions::days_in_feb(2100);
    if ($result != 28) {
	printf("\tExpected %d, but was %d\n", 28, $result);
    }
}

sub testGetNextDay {
    my $result;

    printf("Testing Conversions::getNextDay()\n");
    
    $result = Conversions::getNextDay("2003/06/28");
    if ($result ne "2003/06/29") {
	printf("\tExpected %s, but was %s\n", "2003/06/29", $result);
    }
    
    $result = Conversions::getNextDay("2003/02/28");
    if ($result ne "2003/03/01") {
	printf("\tExpected %s, but was %s\n", "2003/03/01", $result);
    }
    
    $result = Conversions::getNextDay("2004/02/28");
    if ($result ne "2004/02/29") {
	printf("\tExpected %s, but was %s\n", "2004/02/29", $result);
    }
    
    $result = Conversions::getNextDay("2003/12/31");
    if ($result ne "2004/01/01") {
	printf("\tExpected %s, but was %s\n", "2004/01/01", $result);
    }
}

sub testValidDate {
    print ("Testing Conversions::validDate()\n");

    if (!Conversions::validDate("2003/01/01")) {
	printf("\tDate %s is a valid date, but validDate said it was not.\n",
	       "2003/01/01");
    }
    if (Conversions::validDate("2003/01/32")) {
	printf("\tDate %s is not a valid date, but validDate said it was.\n",
	       "2003/01/32");	
    }
    if (Conversions::validDate("2003/02/00")) {
	printf("\tDate %s is not a valid date, but validDate said it was.\n",
	       "2003/02/00");
    }
    if (Conversions::validDate("2003/02/29")) {
	printf("\tDate %s is not a valid date, but validDate said it was.\n",
	       "2003/02/29");
    }
    if (!Conversions::validDate("2000/02/29")) {
	printf("\tDate %s is a valid date, but validDate said it was not.\n",
	       "2000/02/29");
    }
}

sub testValidTime {
    print ("Testing Conversions::validTime()\n");

    if (!Conversions::validTime("00:00")) {
	printf("\tTime %s is a valid time, but validTime said it was not.\n",
	       "00:00");
    }
    if (!Conversions::validTime("23:59")) {
	printf("\tTime %s is a valid time, but validTime said it was not.\n",
	       "23:59");
    }
    if (Conversions::validTime("00:60")) {
	printf("\tTime %s is not a valid time, but validTime said it was.\n",
	       "00:60");	
    }
    if (Conversions::validTime("24:00")) {
	printf("\tTime %s is not a valid time, but validTime said it was.\n",
	       "2003/02/00");
    }
}


