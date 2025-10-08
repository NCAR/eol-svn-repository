#! /usr/bin/perl -w

package Station::TestSimpleStationMap;
use strict;
use lib "..";
use TestModule;
use Station::SimpleStationMap;
use Station::Station;
our @ISA = ("TestModule");

&main();

sub main {
    my $tester = Station::TestSimpleStationMap->new();

    my @stations = buildStations();

    my $ssm = Station::SimpleStationMap->new();

    $ssm->addStation($stations[0]);
    $ssm->addStation($stations[1]);
    $ssm->addStation($stations[2]);

    $tester->assertValue(1, $ssm->hasStation("ID 1", "NET A"), "ID 1, NET A");
    $tester->assertValue(1, $ssm->hasStation("ID 2", "NET A"), "ID 2, NET A");
    $tester->assertValue(0, $ssm->hasStation("ID 3", "NET A"), "ID 3, NET A");

    $tester->assertString($stations[0]->toString(),
			  $ssm->getStation("ID 1", "NET A")->toString(), 
			  "Get 1 A");

    $tester->assertValue(3, scalar($ssm->getAllStations()),"Get All Stations");

    $ssm->clear();
    $tester->assertValue(0, scalar($ssm->getAllStations()),"Clear Stations");
}

sub buildStations {
    my @stations = ();

    my $station = Station::Station->new("ID 1", "NET A");
    $station->setLatitude(0, "D");
    $station->setLongitude(0, "D");
    $station->setElevation(0, "m");
    push(@stations, $station);

    $station = Station::Station->new("ID 2", "NET A");
    $station->setLatitude(10, "DD");
    $station->setLongitude(10, "DD");
    $station->setElevation(10, "m");
    push(@stations, $station);

    $station = Station::Station->new("ID 1", "NET B");
    $station->setLatitude(0, "D");
    $station->setLongitude(0, "D");
    $station->setElevation(0, "m");
    push(@stations, $station);

    $station = Station::Station->new("ID 2", "NET A");
    $station->setLatitude(90, "DD");
    $station->setLongitude(90, "DD");
    $station->setElevation(90, "m");
    push(@stations, $station);

    return @stations;
}
