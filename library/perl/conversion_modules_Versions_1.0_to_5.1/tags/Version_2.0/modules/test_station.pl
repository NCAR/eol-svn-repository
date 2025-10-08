#! /usr/bin/perl

#----------------------------------------------------------------------
# This is a collection of tests used for testing the Station.pm
# module.
#
#----------------------------------------------------------------------
use strict;
use lib ".";
use Conversions;
use Station;

&main();

# Run all of the tests.
sub main() {
    my $testStation = Station->new(0);
    &testDefaults($testStation);
    &testAccessors($testStation);
    &testQCF_String($testStation);
}

sub testAccessors {
    my $station = shift;

    printf("Testing StationId accessors\n");
    $station->setStationId("AbCXyZ");
    if ($station->getStationId() ne "AbCXyZ") {
	printf("\tExpected %s, but was %s\n", "AbCXyZ", 
	       $station->getStationId());	
    }

    printf("Testing NetworkIdNumber accessors\n");
    $station->setNetworkIdNumber(11);
    if ($station->getNetworkIdNumber() != 11) {
	printf("\tExpected %d, but was %d\n", 11, 
	       $station->getNetworkIdNumber());	
    }

    printf("Testing Latitude accessors\n");
    $station->setLatitude("40.0", "DDDD");
    if (sprintf("%20.10f", $station->getLatitude()) ne 
	sprintf("%20.10f", "40.0")) {
	printf("\tExpected %20.10f, but was %20.10f\n", "40.0", 
	       $station->getLatitude());	
    }

    printf("Testing Longitude accessors\n");
    $station->setLongitude("-100.0", "-DDDDD");
    if (sprintf("%20.10f", $station->getLongitude()) ne 
	sprintf("%20.10f", "-100.0")) {
	printf("\tExpected %20.10f, but was %20.10f\n", "-100.0", 
	       $station->getLongitude());	
    }

    printf("Testing Occurence accessors\n");
    $station->setOccurence(11);
    if ($station->getOccurence() != 11) {
	printf("\tExpected %d, but was %d\n", 11, $station->getOccurence());
    }

    printf("Testing LatLongAccuracy accessors\n");
    $station->setLatLongAccuracy(11);
    if ($station->getLatLongAccuracy() != 11) {
	printf("\tExpected %d, but was %d\n", 11, 
	       $station->getLatLongAccuracy());
    }

    printf("Testing StationName accessors\n");
    $station->setStationName("Andromeda 11, Naples");
    if ($station->getStationName() ne "Andromeda 11, Naples") {
	printf("\tExpected %s, but was %s\n", "Andromeda 11, Naples", 
	       $station->getStationName());	
    }

    printf("Testing IsCommissioned accessors\n");
    #$station->setIsCommissioned("d"); # Should die.
    $station->setIsCommissioned("y");
    if ($station->getIsCommissioned() ne "(Y)") {
	printf("\tExpected %s, but was %s\n", "(Y)", 
	       $station->getIsCommissioned());	
    }

    printf("Testing Date accessors\n");
    $station->insertDate("712003", "MDYYYY");
    if ($station->getBeginDate() ne "20030701") {
	printf("\tExpected %s, but was %s\n", "20030701", 
	       $station->getBeginDate());	
    }
    if ($station->getEndDate() ne "20030701") {
	printf("\tExpected %s, but was %s\n", "20030701", 
	       $station->getEndDate());	
    }
    $station->insertDate("612003", "MDYYYY");
    if ($station->getBeginDate() ne "20030601") {
	printf("\tExpected %s, but was %s\n", "20030601", 
	       $station->getBeginDate());	
    }
    if ($station->getEndDate() ne "20030701") {
	printf("\tExpected %s, but was %s\n", "20030701", 
	       $station->getEndDate());	
    }
    $station->insertDate("812003", "MDYYYY");
    if ($station->getBeginDate() ne "20030601") {
	printf("\tExpected %s, but was %s\n", "20030601", 
	       $station->getBeginDate());	
    }
    if ($station->getEndDate() ne "20030801") {
	printf("\tExpected %s, but was %s\n", "20030801", 
	       $station->getEndDate());	
    }

    printf("Testing Country accessors\n");
    $station->setCountry("AU");
    if ($station->getCountry() ne "AU") {
	printf("\tExpected %s, but was %s\n", "AU", 
	       $station->getCountry());	
    }

    printf("Testing CountyCode accessors\n");
    $station->setCountyCode("Chi");
    if ($station->getCountyCode() ne "Chi") {
	printf("\tExpected %s, but was %s\n", "Chi", 
	       $station->getCountyCode());	
    }

    printf("Testing UTC_Offset accessors\n");
    $station->setUTC_Offset(11);
    if ($station->getUTC_Offset() != 11) {
	printf("\tExpected %d, but was %d\n", 11, 
	       $station->getUTC_Offset());	
    }

    printf("Testing DaylightSavingsSwitch accessors\n");
    #    $station->setDaylightSavingsSwitch("D"); # should die
    $station->setDaylightSavingsSwitch("Y");
    if ($station->getDaylightSavingsSwitch() ne "y") {
	printf("\tExpected %s, but was %s\n", "y", 
	       $station->getDaylightSavingsSwitch());	
    }

    printf("Testing PlatformIdNumber accessors\n");
    $station->setPlatformIdNumber(11);
    if ($station->getPlatformIdNumber() != 11) {
	printf("\tExpected %d, but was %d\n", 11, 
	       $station->getPlatformIdNumber());	
    }

    printf("Testing ReportingFrequency accessors\n");
    $station->setReportingFrequency("hourly");
    if ($station->getReportingFrequency() ne "hourly") {
	printf("\tExpected %s, but was %s\n", "hourly", 
	       $station->getReportingFrequency());	
    }

    printf("Testing Elevation accessors\n");
    $station->setElevation(1111.11, "m");
    if (sprintf("%20.10f", $station->getElevation()) ne 
	sprintf("%20.10f", 1111.11)) {
	printf("\tExpected %20.10f, but was %20.10f\n", 1111.11, 
	       $station->getElevation());	
    }

    printf("Testing MobilityFlag accessors\n");
    #    $station->setMobilityFlag("r"); # should die
    $station->setMobilityFlag("f");
    if ($station->getMobilityFlag() ne "f") {
	printf("\tExpected %s, but was %s\n", "f", 
	       $station->getMobilityFlat());	
    }
}

sub testDefaults {
    my $station = shift;
    printf("Testing default station values.\n");

    if ($station->getStationId() ne "StnId") {
	printf("\tExpected %s, but was %s\n", "StnId", 
	       $station->getStationId());
    }
    if ($station->getNetworkIdNumber() != 0) {
	printf("\tExpected %d, but was %d\n", 0, 
	       $station->getNetworkIdNumber());
    }
    if ($station->getLatitude() ne "-999.99999") {
	printf("\tExpected %s, but was %s\n", "-999.99999", 
	       $station->getLatitude());
    }
    if ($station->getLongitude() ne "-999.99999") {
	printf("\tExpected %s, but was %s\n", "-999.99999", 
	       $station->getLongitude());
    }
    if ($station->getOccurence() != 0) {
	printf("\tExpected %d, but was %d\n", 0, $station->getOccurence());
    }
    if ($station->getLatLongAccuracy() != 4) {
	printf("\tExpected %d, but was %d\n", 4, 
	       $station->getLatLongAccuracy());
    }
    if ($station->getStationName() ne "Station Name") {
	printf("\tExpected %s, but was %s\n", "Station Name", 
	       $station->getStationName());
    }
    if ($station->getIsCommissioned() ne "(N)") {
	printf("\tExpected %s, but was %s\n", "(N)", 
	       $station->getIsCommissioned());	
    }
    if ($station->getBeginDate() != 99999999) {
	printf("\tExpected %d, but was %d\n", 99999999, 
	       $station->getBeginDate());
    }
    if ($station->getEndDate() != 00000000) {
	printf("\tExpected %d, but was %d\n", 00000000, 
	       $station->getEndDate());
    }
    if ($station->getCountry() ne "US") {
	printf("\tExpected %s, but was %s\n", "US", $station->getCountry());
    }
    if ($station->getCountyCode() ne "???") {
	printf("\tExpected %s, but was %s\n", "???", 
	       $station->getCountyCode());	
    }
    if ($station->getUTC_Offset() != 0) {
	printf("\tExpected %d, but was %d\n", 0, $station->getUTC_Offset());
    }
    if ($station->getDaylightSavingsSwitch() ne "n") {
	printf("\tExpected %s, but was %s\n", "n", 
	       $station->getDaylightSavingsSwitch());	
    }
    if ($station->getPlatformIdNumber() != 0) {
	printf("\tExpected %d, but was %d\n", 0, 
	       $station->getPlatformIdNumber());
    }
    if ($station->getReportingFrequency() ne "xxxxx") {
	printf("\tExpected %s, but was %s\n", "xxxxx", 
	       $station->getReportingFrequency());	
    }
    if ($station->getElevation() ne "-9999.9") {
	printf("\tExpected %s, but was %s\n", "-9999.9", 
	       $station->getElevation());	
    }
    if ($station->getMobilityFlag() ne "f") {
	printf("\tExpected %s, but was %s\n", "f", 
	       $station->getMobilityFlag());	
    }
}

sub testQCF_String {
    my $station = shift;
    my $expected = "AbCXyZ            11   40.00000  -100.00000  11    11 Andromeda 11, Naples                           (Y) 20030601 20030801 AU    Chi  11.00 y   11 hourly             1111.1 f\n";

    printf("Testing toQCF_String\n");
    if ($expected ne $station->toQCF_String()) {
	printf("\tThe formating for the station is not correct.\n");
	printf("Expected:\n");
	printf($expected);
	printf("Was:\n");
	print($station->toQCF_String());
    }
}
