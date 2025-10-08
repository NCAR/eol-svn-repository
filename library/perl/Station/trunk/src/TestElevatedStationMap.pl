#! /usr/bin/perl -w

package TestElevatedStationMap;
use strict;
use lib "/net/work/lib/perl/Utilities";
use TestModule;
use ElevatedStationMap;
use Station;
our @ISA = ("TestModule");

&main();

sub main {
    my $tester = TestElevatedStationMap->new();

    my @stations = buildStations();

    my $esm = ElevatedStationMap->new();

    $esm->addStation($stations[0]);
    $esm->addStation($stations[1]);
    $esm->addStation($stations[2]);

    $tester->assertValue(1, $esm->hasStation("ID 1", "NET A", 0, 0, 0), 
			 "ID 1, NET A 0 0 0");
    $tester->assertValue(1, $esm->hasStation("ID 2", "NET A", 10, 10, 10), 
			 "ID 2, NET A 10 10 10");
    $tester->assertValue(0, $esm->hasStation("ID 3", "NET A", 10, 90, 10), 
			 "ID 3, NET A 10 90 10");
    $tester->assertValue(0, $esm->hasStation("ID 2", "NET A", 10, 90, 0), 
			 "ID 2, NET A 10 90 0");

    $tester->assertString($stations[0]->toString(),
			 $esm->getStation("ID 1","NET A",0,0,0)->toString(), 
			  "Get 1 A");

    $tester->assertValue(3, scalar($esm->getAllStations()),"Get All Stations");

    $esm->clear();
    $tester->assertValue(0, scalar($esm->getAllStations()),"Clear Stations");
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
