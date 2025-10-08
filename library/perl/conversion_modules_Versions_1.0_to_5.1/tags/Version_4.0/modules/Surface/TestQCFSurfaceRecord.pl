#! /usr/bin/perl -w

package Surface::TestQCFSurfaceRecord;
use strict;
use lib "..";
use Surface::QCFConstants qw(:DEFAULT);
use Surface::QCFSurfaceRecord;
use Surface::TestSurfaceRecord;
our @ISA = ("Surface::TestSurfaceRecord");
&main();

my $WARNING;

sub main {
    my $tester = Surface::TestQCFSurfaceRecord->new();

    open($WARNING, ">warning.test") or die("Can't open warning file.\n");

    $tester->testConstants();

    $tester->testNominalDate();
    $tester->testNominalTime();
    $tester->testActualDate();
    $tester->testActualTime();
    $tester->testNetworkId();
    $tester->testStationId();
    $tester->testLatitude();
    $tester->testLongitude();
    $tester->testOccurence();
    $tester->testElevation();
    $tester->testPressure();
    $tester->testSLPressure();
    $tester->testCalcSLPressure();
    $tester->testTemperature();
    $tester->testDewPoint();
    $tester->testWindSpeed();
    $tester->testWindDirection();
    $tester->testPrecip();
    $tester->testGustSpeed();
    $tester->testPresentWeather();
    $tester->testVisibility();
    $tester->testCeilingHeight(1);
    $tester->testCloudAmount(1);
    $tester->testCeilingHeight(2);
    $tester->testCloudAmount(2);
    $tester->testCeilingHeight(3);
    $tester->testCloudAmount(3);

    $tester->testSmallValues();
    $tester->testLargeValues();

    close($WARNING);

    if (system("diff warning.test TEST_QCF_SURFACE.txt") == 0) {
	unlink("warning.test");
    } else {
	printf("File warning.test does not match TEST_QCF_SURFACE.txt\n");
    }
}


sub testLargeValues {
    my $self = shift;
    my $record = Surface::QCFSurfaceRecord->new($WARNING);
    $record->setVerbose(0);

    $record->setNominalDate("2004/06/31", "YYYY/MM/DD");
    $self->assertString("2004/06/31", $record->getNominalDate(),
			"Large Nominal Date");
    $record->setNominalTime("24:24", "HH:MM");
    $self->assertString("24:24", $record->getNominalTime(),
			"Large Nominal Time");
    $record->setActualDate("2004/06/31", "YYYY/MM/DD");
    $self->assertString("2004/06/31", $record->getActualDate(),
			"Large Actual Date");
    $record->setActualTime("24:24", "HH:MM");
    $self->assertString("24:24", $record->getActualTime(),
			"Large Actual Time");
    $record->setNetworkId("ExtraLong_BAD_NetworkId");
    $self->assertString("ExtraLong_BAD_NetworkId", $record->getNetworkId(),
			"Extra Long Network Id");
    $record->setStationId("ExtraLong_LARGE_StationId");
    $self->assertString("ExtraLong_LARGE_StationId", $record->getStationId(),
			"Extra Long Station Id");
    $record->setLatitude("90.1", "DDDD");
    $self->assertString("90.1", $record->getLatitude(), "Latitude to Large");
    $record->setLongitude("180.1", "DDDDD");
    $self->assertString("180.1",$record->getLongitude(),"Longitude to Large");
    $record->setOccurence(1111);
    $self->assertValue(1111, $record->getOccurence(), "Large Occurence");
    $record->setElevation(9000.01, "m");
    $self->assertFormatString("9000.01", $record->getElevation(), 
			      "Large Elevation");

    $record->setPressure(1200.01, "mbar");
    $self->assertFormatString("1200.01", $record->getPressure(),
			      "Large Station Pressure");
    $record->setSeaLevelPressure(1200.01, "mbar");
    $self->assertFormatString("1200.01", $record->getSeaLevelPressure(),
			      "Large Sea Level Pressure");

    $record->setTemperature(100.01, "C");
    $self->assertFormatString("100.01", $record->getTemperature(),
			      "Large Temperature");
    $record->setDewPoint(100.01, "C");
    $self->assertFormatString(100.01,$record->getDewPoint(),"Large Dew Point");


    $record->setWindSpeed(200.01, "m/s");
    $self->assertFormatString(200.01, $record->getWindSpeed(), 
			      "Large Wind Speed");
    $record->setWindDirection(360.01);
    $self->assertFormatString(360.01, $record->getWindDirection(),
			      "Large Wind Direction");
    $record->setGustSpeed(10000, "m/s");
    $self->assertFormatString(10000, $record->getGustSpeed(),
			      "Large Gust Speed");

    $record->setPresentWeather(1000);
    $self->assertFormatString(1000, $record->getPresentWeather(),
			      "Large Present Weather");
    $record->setVisibility(160000.01, "m");
    $self->assertFormatString(160000.01, $record->getVisibility(),
			      "Large Visibility");

    $record->setCeilingHeight(10000, 16, 1);
    $self->assertFormatString(10000, $record->getCeilingHeight(1),
			      "Large Ceiling Height");
    $self->assertValue(16, $record->getCeilingHeightCode(1),
		       "Large Ceiling Height Code");
    $record->setCloudAmount(16, 1);
    $self->assertValue(16, $record->getCloudAmount(1),
		       "Large Cloud Amount");

    $record->toQCF_String();
}


sub testSmallValues {
    my $self = shift;
    my $record = Surface::QCFSurfaceRecord->new($WARNING);
    $record->setVerbose(0);

    $record->setNominalDate("2004/06/00", "YYYY/MM/DD");
    $self->assertString("2004/06/00", $record->getNominalDate(),
			"Small Nominal Date");
    $record->setActualDate("2004/06/00", "YYYY/MM/DD");
    $self->assertString("2004/06/00", $record->getActualDate(),
			"Small Actual Date");
    $record->setNetworkId("ExtraLong_BAD_NetworkId");
    $self->assertString("ExtraLong_BAD_NetworkId", $record->getNetworkId(),
			"Extra Long Network Id");
    $record->setStationId("ExtraLong_SMALL_StationId");
    $self->assertString("ExtraLong_SMALL_StationId", $record->getStationId(),
			"Extra Long Station Id");
    $record->setLatitude("-90.1", "-DDDD");
    $self->assertString("-90.1", $record->getLatitude(), "Latitude to Small");
    $record->setLongitude("-180.1", "-DDDDD");
    $self->assertString("-180.1",$record->getLongitude(),"Longitude to Small");
    $record->setElevation(-200.01, "m");
    $self->assertFormatString("-200.01", $record->getElevation(), 
			      "Small Elevation");
    

    $record->setPressure(799.99, "mbar");
    $self->assertFormatString("799.99", $record->getPressure(),
			      "Small Station Pressure");
    $record->setSeaLevelPressure(799.99, "mbar");
    $self->assertFormatString(799.99, $record->getSeaLevelPressure(),
			      "Small Sea Level Pressure");

    $record->setTemperature(-100.01, "C");
    $self->assertFormatString(-100.01, $record->getTemperature(),
			      "Small Temperature");
    $record->setDewPoint(-100.01, "C");
    $self->assertFormatString("-100.01", $record->getDewPoint(),
			      "Small Dew Point");
    
    $record->setWindSpeed(-0.01, "m/s");
    $self->assertFormatString(-0.01, $record->getWindSpeed(), 
			      "Small Wind Speed");
    $record->setWindDirection(-.01);
    $self->assertFormatString(-.01, $record->getWindDirection(),
			      "Small Wind Direction");
    $record->setGustSpeed(-.01, "m/s");
    $self->assertFormatString(-.01, $record->getGustSpeed(),
			      "Small Gust Speed");

    $record->setPresentWeather(-1);
    $self->assertFormatString(-1, $record->getPresentWeather(),
			      "Small Present Weather");
    $record->setVisibility(-.01, "m");
    $self->assertFormatString(-.01, $record->getVisibility(),
			      "Small Visibility");

    $record->setCeilingHeight(-.01, -1, 1);
    $self->assertFormatString(-.01, $record->getCeilingHeight(1),
			      "Small Ceiling Height");
    $self->assertValue(-1, $record->getCeilingHeightCode(1),
		       "Small Ceiling Height Code");
    $record->setCloudAmount(-1, 1);
    $self->assertValue(-1, $record->getCloudAmount(1),
		       "Small Cloud Amount");

    $record->toQCF_String();
}




sub testConstants {
    my $self = shift;
    my $record = Surface::QCFSurfaceRecord->new($WARNING);
    
    $self->assertString("-999.99", $MISSING, "Missing Value");
    $self->assertString("B", $BAD_FLAG, "Bad Flag");
    $self->assertString("D", $DUBIOUS_FLAG, "Dubious Flag");
    $self->assertString("E", $ESTIMATE_FLAG, "Estimate Flag");
    $self->assertString("X", $GLITCH_FLAG, "Glitch Flag");
    $self->assertString("M", $MISSING_FLAG, "Missing Flag");
    $self->assertString("C", $NEG_PRECIP_FLAG,"Negative Precip Flag");
    $self->assertString("N", $NO_READING_FLAG, "No Reading Flag");
    $self->assertString("T", $TRACE_PRECIP_FLAG, "Trace Precip Flag");
    $self->assertString("C", $VALUE_DOES_NOT_FIT_FLAG,"Value Does Not Fit Flag");
}

sub testActualDate {
    my $self = shift;
    my $record = Surface::QCFSurfaceRecord->new($WARNING);
    
    $self->assertString("9999/99/99", $record->getActualDate(), 
			"Default Actual Date");
    $record->setActualDate("2004-04-06", "YYYY-DD-MM");    
    $self->assertString("2004/06/04", $record->getActualDate(), 
			"Actual Date");
}

sub testActualTime {
    my $self = shift;
    my $record = Surface::QCFSurfaceRecord->new($WARNING);
    
    $self->assertString("99:99", $record->getActualTime(), 
			"Default Actual Time");
    $record->setActualTime("12:24", "HH:MM");    
    $self->assertString("12:24", $record->getActualTime(), 
			"Actual Time");
}

sub testCalcSLPressure {
    my $self = shift;
    my $record = Surface::QCFSurfaceRecord->new($WARNING);

    $self->assertFormatString($MISSING,
			      $record->getCalcSeaLevelPressure(),
			      "Default Calc Sea Level Pressure");
    $self->assertString($INCALCUABLE_FLAG,
			$record->getCalcSeaLevelPressureFlag(),
			"Default Calc Sea Level Pressure Flag");
    $record->setPressure(913, "mbar");
    $record->setElevation(566, "m");
    $record->setDewPoint(23, "C");
    $record->setTemperature(33, "C");
    $self->assertFormatString(972.17, $record->getCalcSeaLevelPressure(),
			      "Calc Sea Level Pressure");
    $self->assertString($UNCHECKED_FLAG,
			$record->getCalcSeaLevelPressureFlag(),
			"Calc Calc Sea Level Pressure Flag");
}

sub testCeilingHeight {
    my $self = shift;
    my $height = shift;
    my $record = Surface::QCFSurfaceRecord->new($WARNING);

    $self->assertFormatString($MISSING, 
			      $record->getCeilingHeight($height),
			      "Default Ceiling Height $height");
    $self->assertValue(15, $record->getCeilingHeightCode($height),
		       "Default Ceiling Height Code $height");
    $self->assertString($MISSING_FLAG,
			$record->getCeilingHeightFlag($height),
			"Default Ceiling Height Flag $height");
    $record->setCeilingHeight(34, 11, $height);
    $self->assertFormatString(34, $record->getCeilingHeight($height),
			      "Ceiling Height $height");
    $self->assertValue(11, $record->getCeilingHeightCode($height),
		       "Ceiling Height Code $height");
    $self->assertString($UNCHECKED_FLAG,
			$record->getCeilingHeightFlag($height),
			"Calc Ceiling Height Flag $height");
    $record->setCeilingHeightFlag($ESTIMATE_FLAG, $height);
    $self->assertString($ESTIMATE_FLAG,
			$record->getCeilingHeightFlag($height),
			"Set Ceiling Height Flag $height");
}

sub testCloudAmount {
    my $self = shift;
    my $height = shift;
    my $record = Surface::QCFSurfaceRecord->new($WARNING);

    $self->assertValue(15, $record->getCloudAmount($height),
		       "Default Cloud Amount $height");
    $self->assertString($MISSING_FLAG,
			$record->getCloudAmountFlag($height),
			"Default Cloud Amount Flag $height");
    $record->setCloudAmount(11, $height);
    $self->assertValue(11, $record->getCloudAmount($height),
		       "Cloud Amount $height");
    $self->assertString($UNCHECKED_FLAG,
			$record->getCloudAmountFlag($height),
			"Calc Cloud Amount Flag $height");
    $record->setCloudAmountFlag($ESTIMATE_FLAG, $height);
    $self->assertString($ESTIMATE_FLAG,
			$record->getCloudAmountFlag($height),
			"Set Cloud Amount Flag $height");
}

sub testDewPoint {
    my $self = shift;
    my $record = Surface::QCFSurfaceRecord->new($WARNING);

    $self->assertFormatString($MISSING, $record->getDewPoint(),
			      "Default Dew Point");
    $self->assertString($INCALCUABLE_FLAG, 
			$record->getDewPointFlag(), "Default Dew Point Flag");
    $record->setTemperature(23, "C");
    $record->setRelativeHumidity(50);
    $self->assertFormatString("12.03",$record->getDewPoint(),"Calc Dew Point");
    $self->assertString($UNCHECKED_FLAG,
			$record->getDewPointFlag(),"Calc Dew Point Calc Flag");
    $record->setDewPoint(34, "C");
    $self->assertFormatString(34, $record->getDewPoint(), "Dew Point");
    $self->assertString($UNCHECKED_FLAG,
			$record->getDewPointFlag(), "Calc Dew Point Flag");
    $record->setDewPointFlag($ESTIMATE_FLAG);
    $self->assertString($ESTIMATE_FLAG, $record->getDewPointFlag(),
			"Set Dew Point Flag");
}

sub testElevation {
    my $self = shift;
    my $record = Surface::QCFSurfaceRecord->new($WARNING);

    $self->assertString($MISSING, $record->getElevation(),
			"Default Elevation");
    $record->setElevation(111.11, "m");
    $self->assertString("111.11", $record->getElevation(), "Elevation");
}

sub testGustSpeed {
    my $self = shift;
    my $record = Surface::QCFSurfaceRecord->new($WARNING);

    $self->assertFormatString($MISSING, $record->getGustSpeed(),
			      "Default Gust Speed");
    $self->assertString(" ", $record->getGustMarker(), "Default Gust Marker");
    $self->assertString($MISSING_FLAG, $record->getGustSpeedFlag(),
			"Default Gust Speed Flag");
    $record->setWindSpeed(11, "m/s");
    $record->setGustSpeed(34, "m/s");
    $self->assertFormatString(34, $record->getGustSpeed(), "Gust Speed");
    $self->assertString("G", $record->getGustMarker(), "Gust Marker");
    $self->assertString($UNCHECKED_FLAG, 
			$record->getGustSpeedFlag(), "Calc Gust Speed Flag");
    $record->setGustSpeedFlag($ESTIMATE_FLAG);
    $self->assertString($ESTIMATE_FLAG, 
			$record->getGustSpeedFlag(), "Set Gust Speed Flag");
}

sub testLatitude {
    my $self = shift;
    my $record = Surface::QCFSurfaceRecord->new($WARNING);

    $self->assertString("-999.99999", $record->getLatitude(),
			"Default Latitude");
    $record->setLatitude("40", "DD");
    $self->assertString("40", $record->getLatitude(), "Latitude");
}

sub testLongitude {
    my $self = shift;
    my $record = Surface::QCFSurfaceRecord->new($WARNING);

    $self->assertString("-999.99999", $record->getLongitude(),
			"Default Longitude");
    $record->setLongitude("40", "DD");
    $self->assertString("40", $record->getLongitude(), "Longitude");
}

sub testNetworkId {
    my $self = shift;
    my $record = Surface::QCFSurfaceRecord->new($WARNING);

    $self->assertString("Network", $record->getNetworkId(), "Default Network");
    $record->setNetworkId("TestNetwork");
    $self->assertString("TestNetwork", $record->getNetworkId(), "Network Id");
}

sub testNominalDate {
    my $self = shift;
    my $record = Surface::QCFSurfaceRecord->new($WARNING);
    
    $self->assertString("9999/99/99", $record->getNominalDate(), 
			"Default Nominal Date");
    $record->setNominalDate("2004-04-06", "YYYY-DD-MM");    
    $self->assertString("2004/06/04", $record->getNominalDate(), 
			"Nominal Date");
}

sub testNominalTime {
    my $self = shift;
    my $record = Surface::QCFSurfaceRecord->new($WARNING);
    
    $self->assertString("99:99", $record->getNominalTime(), 
			"Default Nominal Time");
    $record->setNominalTime("12:24", "HH:MM");    
    $self->assertString("12:24", $record->getNominalTime(), 
			"Nominal Time");
}

sub testOccurence {
    my $self = shift;
    my $record = Surface::QCFSurfaceRecord->new($WARNING);

    $self->assertValue(0, $record->getOccurence(), "Default Occurence");
    $record->setOccurence(11);
    $self->assertValue(11, $record->getOccurence(), "Occurence");
}

sub testPrecip {
    my $self = shift;
    my $record = Surface::QCFSurfaceRecord->new($WARNING);
    
    $self->assertFormatString($MISSING, $record->getPrecip(),
			      "Default Precip");
    $self->assertString($MISSING_FLAG, $record->getPrecipFlag(),
			"Default Precip Flag");
    $record->setPrecip(11, "mm");
    $self->assertFormatString(11, $record->getPrecip(), "Precip");
    $self->assertString($UNCHECKED_FLAG, $record->getPrecipFlag(),
			"Calc Precip Flag");
    $record->setPrecipFlag($ESTIMATE_FLAG);
    $self->assertString($ESTIMATE_FLAG, $record->getPrecipFlag(),
			"Set Precip Flag");
}

sub testPresentWeather {
    my $self = shift;
    my $record = Surface::QCFSurfaceRecord->new($WARNING);

    $self->assertValue(-999, $record->getPresentWeather(), 
		       "Default Present Weather");
    $self->assertString($MISSING_FLAG, 
			$record->getPresentWeatherFlag(),
			"Default Present Weather Flag");
    $record->setPresentWeather(253);
    $self->assertValue(253, $record->getPresentWeather(), "Present Weather");
    $self->assertString($UNCHECKED_FLAG,
			$record->getPresentWeatherFlag(),
			"Calc Present Weather Flag");
    $record->setPresentWeatherFlag($ESTIMATE_FLAG);
    $self->assertString($ESTIMATE_FLAG,
			$record->getPresentWeatherFlag(),
			"Set Present Weather Flag");
}

sub testPressure {
    my $self = shift;
    my $record = Surface::QCFSurfaceRecord->new($WARNING);

    $self->assertFormatString($MISSING, $record->getPressure(),
			      "Default Station Pressure");
    $self->assertString($MISSING_FLAG, $record->getPressureFlag(),
			"Default Station Pressure Flag");
    $record->setPressure(1013, "mbar");
    $self->assertFormatString(1013,$record->getPressure(),"Station Pressure");
    $self->assertString($UNCHECKED_FLAG,$record->getPressureFlag(),
			"Calc Station Pressure Flag");
    $record->setPressureFlag($ESTIMATE_FLAG);
    $self->assertString($ESTIMATE_FLAG, $record->getPressureFlag(),
			"Set Station Pressure Flag");
}

sub testSLPressure {
    my $self = shift;
    my $record = Surface::QCFSurfaceRecord->new($WARNING);
    
    $self->assertFormatString($MISSING, 
			      $record->getSeaLevelPressure(),
			      "Default Sea Level Pressure");
    $self->assertString($MISSING_FLAG,
			$record->getSeaLevelPressureFlag(),
			"Default Sea Level Pressure Flag");
    $record->setSeaLevelPressure(1013, "mbar");
    $self->assertFormatString(1013, $record->getSeaLevelPressure(),
			      "Sea Level Pressure");
    $self->assertString($UNCHECKED_FLAG,
			$record->getSeaLevelPressureFlag(),
			"Calc Sea Level Pressure Flag");
    $record->setSeaLevelPressureFlag($ESTIMATE_FLAG);
    $self->assertString($ESTIMATE_FLAG,
			$record->getSeaLevelPressureFlag(),
			"Set Sea Level Pressure Flag");
}

sub testStationId {
    my $self = shift;
    my $record = Surface::QCFSurfaceRecord->new($WARNING);

    $self->assertString("Station", $record->getStationId(), "Default Station");
    $record->setStationId("TestStation");
    $self->assertString("TestStation", $record->getStationId(), "Station Id");
}

sub testTemperature {
    my $self = shift;
    my $record = Surface::QCFSurfaceRecord->new($WARNING);

    $self->assertFormatString($MISSING, $record->getTemperature(),
			      "Default Temperature");
    $self->assertString($MISSING_FLAG, 
			$record->getTemperatureFlag(), 
			"Default Temperature Flag");
    $record->setTemperature(23, "C");
    $self->assertFormatString(23, $record->getTemperature(), "Temperature");
    $self->assertString($UNCHECKED_FLAG,
			$record->getTemperatureFlag(),"Calc Temperature Flag");
    $record->setTemperatureFlag($ESTIMATE_FLAG);
    $self->assertString($ESTIMATE_FLAG, 
			$record->getTemperatureFlag(), "Set Temperature Flag");
}

sub testVisibility {
    my $self = shift;
    my $record = Surface::QCFSurfaceRecord->new($WARNING);

    $self->assertFormatString($MISSING, $record->getVisibility(),
			      "Default Visibility");
    $self->assertString($MISSING_FLAG,$record->getVisibilityFlag(),
			"Default Visibility Flag");
    $record->setVisibility(10, "m");
    $self->assertFormatString(10, $record->getVisibility(), "Visibility");
    $self->assertString($UNCHECKED_FLAG,
			$record->getVisibilityFlag(), "Calc Visibility Flag");
    $record->setVisibilityFlag($ESTIMATE_FLAG);
    $self->assertString($ESTIMATE_FLAG,
			$record->getVisibilityFlag(), "Set Visibility Flag");
}

sub testWindDirection {
    my $self = shift;
    my $record = Surface::QCFSurfaceRecord->new($WARNING);
    
    $self->assertFormatString($MISSING,
			      $record->getWindDirection(),
			      "Default Wind Direction");
    $self->assertString($MISSING_FLAG,
			$record->getWindDirectionFlag(),
			"Default Wind Direction Flag");
    $record->setWindDirection(243);
    $self->assertFormatString(243, $record->getWindDirection(),
			      "Wind Direction");
    $self->assertString($UNCHECKED_FLAG,
			$record->getWindDirectionFlag(),
			"Calc Wind Direction Flag");
    $record->setWindDirectionFlag($ESTIMATE_FLAG,
				  $record->getWindDirectionFlag(),
				  "Set Wind Direction Flag");
}

sub testWindSpeed {
    my $self = shift;
    my $record = Surface::QCFSurfaceRecord->new($WARNING);

    $self->assertFormatString($MISSING, $record->getWindSpeed(),
			      "Default Wind Speed");
    $self->assertString($MISSING_FLAG,$record->getWindSpeedFlag(),
			"Default Wind Speed Flag");
    $record->setWindSpeed(12, "m/s");
    $self->assertFormatString(12, $record->getWindSpeed(), "Wind Speed");
    $self->assertString($UNCHECKED_FLAG,
			$record->getWindSpeedFlag(), "Calc Wind Speed Flag");
    $record->setWindSpeedFlag($ESTIMATE_FLAG);
    $self->assertString($ESTIMATE_FLAG,$record->getWindSpeedFlag(),
			"Set Wind Speed Flag");
}

1;

