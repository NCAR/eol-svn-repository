#! /usr/bin/perl -w

package TestPositionedStationMap;
use strict;
use lib ".";
use TestModule;
use PositionedStationMap;
use Station;
our @ISA = ("TestModule");

&main();

sub main {
    my $tester = TestPositionedStationMap->new();

    my @stations = buildStations();

    my $psm = PositionedStationMap->new();

    $psm->addStation($stations[0]);
    $psm->addStation($stations[1]);
    $psm->addStation($stations[2]);

    $tester->assertValue(1, $psm->hasStation("ID 1", "NET A", 0, 0), 
			 "ID 1, NET A 0 0");
    $tester->assertValue(1, $psm->hasStation("ID 2", "NET A", 10, 10), 
			 "ID 2, NET A 10 10");
    $tester->assertValue(0, $psm->hasStation("ID 3", "NET A", 10, 90), 
			 "ID 3, NET A 10 90");
    $tester->assertValue(0, $psm->hasStation("ID 2", "NET A", 10, 90), 
			 "ID 2, NET A 10 90");

    $tester->assertString($stations[0]->toQCF_String(),
			 $psm->getStation("ID 1","NET A",0,0)->toQCF_String(), 
			  "Get 1 A");

    $tester->assertValue(3, scalar($psm->getAllStations()),"Get All Stations");

    $psm->clear();
    $tester->assertValue(0, scalar($psm->getAllStations()),"Clear Stations");
}

sub buildStations {
    my @stations = ();

    my $station = Station->new("ID 1", "NET A");
    $station->setLatitude(0, "D");
    $station->setLongitude(0, "D");
    $station->setElevation(0, "m");
    push(@stations, $station);

    $station = Station->new("ID 2", "NET A");
    $station->setLatitude(10, "DD");
    $station->setLongitude(10, "DD");
    $station->setElevation(10, "m");
    push(@stations, $station);

    $station = Station->new("ID 1", "NET B");
    $station->setLatitude(0, "D");
    $station->setLongitude(0, "D");
    $station->setElevation(0, "m");
    push(@stations, $station);

    $station = Station->new("ID 2", "NET A");
    $station->setLatitude(90, "DD");
    $station->setLongitude(90, "DD");
    $station->setElevation(90, "m");
    push(@stations, $station);

    return @stations;
}
