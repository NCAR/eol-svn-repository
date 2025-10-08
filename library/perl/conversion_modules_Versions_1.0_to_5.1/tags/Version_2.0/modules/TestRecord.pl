#! /usr/bin/perl -w

#----------------------------------------------------------------------
# This is a collection of tests used for testing the Record.pm
# module.
#
#----------------------------------------------------------------------
use strict;
use lib ".";
use Station;
use Record;
use Conversions;

&main();

sub main {
    my $station = Station->new(0);
    $station->setNetworkName("IDS");
    $station->setStationId("AbCXyZ");
    $station->setLatitude("40", "DD");
    $station->setLongitude("-100", "-DDD");
    $station->setElevation(1111.11, "m");
    $station->setReportingFrequency("hourly");

    my $data = Record->new($station);

    testPressure($data);
    testSeaLevelPressure($data);
    testCalculateSeaLevelPressure($data);
    testTemperature($data);
    testDewPoint($data);
    testWindSpeed($data);
    testWindDirection($data);
    testPrecip($data);
    testGust($data);
    testWx($data);
    testVisibility($data);
    testCeilingHeight($data);
    testCloudAmount($data);
}

sub assertFloat {
    my $type = shift;
    my $expected = sprintf("%20.10f", shift);
    my $value = sprintf("%20.10f", shift);
    assertString($type, $expected, $value);
}

sub assertString {
    my $type = shift;
    my $expected = shift;
    my $value = shift;
    if ($expected ne $value) {
	printf("\t%s had expected value of %s, but was %s\n", $type, $expected,
	       $value);
    }
}

sub testCalculateSeaLevelPressure {
    my $data = shift;
    my $missing = Conversions::getMissing();

    print ("Testing CalculateSeaLevelPressure Not Yet Implemented\n");

}

sub testCeilingHeight {
    my $data = shift;
    my $missing = Conversions::getMissing();
    
    print ("Testing CeilingHeight...\n");
    assertFloat("CeilingHeight 1", $missing, $data->getCeilingHeight(1));
    assertFloat("CeilingHeight 1", 15, $data->getCeilingHeightCode(1));
    assertString("CeilingHeight 1", 'M', $data->getCeilingHeightFlag(1));

    $data->setCeilingHeight(11, 3, 1);
    assertFloat("CeilingHeight 2", 11, $data->getCeilingHeight(1));
    assertFloat("CeilingHeight 2", 3, $data->getCeilingHeightCode(1));
    assertString("CeilingHeight 2", 'U', $data->getCeilingHeightFlag(1));

    $data->setCeilingHeight($missing, 15, 1);
    assertFloat("CeilingHeight 3", $missing, $data->getCeilingHeight(1));
    assertFloat("CeilingHeight 3", 15, $data->getCeilingHeightCode(1));
    assertString("CeilingHeight 3", 'M', $data->getCeilingHeightFlag(1));

    $data->setCeilingHeight(11, 3, 1, Conversions::getEstimateFlag());
    assertFloat("CeilingHeight 4", 11, $data->getCeilingHeight(1));
    assertFloat("CeilingHeight 4", 3, $data->getCeilingHeightCode(1));
    assertString("CeilingHeight 4", 'E', $data->getCeilingHeightFlag(1));

# Will Die:    $data->setCeilingHeight(11, 3, 1, 'A');
# Will Die:    $data->setCeilingHeight(11, 3, 1, 'M');
# Will Die:    $data->setCeilingHeight($missing, 15, 1, 'U');
# Will Die:    $data->setCeilingHeight($missing, 2, 1);
# Will Die:    $data->setCeilingHeight(11, 15, 1);
}

sub testCloudAmount {
    my $data = shift;
    my $missing = 15;

    print ("Testing CloudAmount...\n");
    assertFloat("CloudAmount 1", 15, $data->getCloudAmount(1));
    assertString("CloudAmount 1", 'M', $data->getCloudAmountFlag(1));

    $data->setCloudAmount(3, 1);
    assertFloat("CloudAmount 2", 3, $data->getCloudAmount(1));
    assertString("CloudAmount 2", 'U', $data->getCloudAmountFlag(1));

    $data->setCloudAmount(15, 1);
    assertFloat("CloudAmount 3", 15, $data->getCloudAmount(1));
    assertString("CloudAmount 3", 'M', $data->getCloudAmountFlag(1));

    $data->setCloudAmount(3, 1, Conversions::getEstimateFlag());
    assertFloat("CloudAmount 4", 3, $data->getCloudAmount(1));
    assertString("CloudAmount 4", 'E', $data->getCloudAmountFlag(1));

# Will Die:    $data->setCloudAmount(3, 1, 'A');
# Will Die:    $data->setCloudAmount(3, 1, 'M');
# Will Die:    $data->setCloudAmount(15, 1, 'U');
}

sub testDewPoint {
    my $data = shift;
    my $missing = Conversions::getMissing();

    print ("Testing DewPoint...\n");
    assertFloat("DewPoint 1", $missing, $data->getDewPoint());
    assertString("DewPoint 1", 'M', $data->getDewPointFlag());

    $data->setDewPoint(11, "C");
    assertFloat("DewPoint 2", 11, $data->getDewPoint());
    assertString("DewPoint 2", 'U', $data->getDewPointFlag());

    $data->setDewPoint($missing, "C");
    assertFloat("DewPoint 3", $missing, $data->getDewPoint());
    assertString("DewPoint 3", 'M', $data->getDewPointFlag());

    $data->setDewPoint(11, "C", Conversions::getEstimateFlag());
    assertFloat("DewPoint 4", 11, $data->getDewPoint());
    assertString("DewPoint 4", 'E', $data->getDewPointFlag());

# Will Die:    $data->setDewPoint(11, "C", 'A');
# Will Die:    $data->setDewPoint(11, "C", 'M');
# Will Die:    $data->setDewPoint($missing, "C", 'U');

    print ("Testing DewPoint Calculations not yet implemented\n");
}

sub testGust {
    my $data = shift;
    my $missing = Conversions::getMissing();

    print ("Testing Gust Speed...\n");
    assertFloat("GustSpeed 1", $missing, $data->getGustSpeed());
    assertString("GustSpeed 1", 'M', $data->getGustSpeedFlag());
    assertString("GustSpeed 1", ' ', $data->getGustMarker());

    $data->setGustSpeed(11, "m/s");
    assertFloat("GustSpeed 2", 11, $data->getGustSpeed());
    assertString("GustSpeed 2", 'U', $data->getGustSpeedFlag());
    assertString("GustSpeed 2", 'G', $data->getGustMarker());

    $data->setGustSpeed($missing, "m/s");
    assertFloat("GustSpeed 3", $missing, $data->getGustSpeed());
    assertString("GustSpeed 3", 'M', $data->getGustSpeedFlag());
    assertString("GustSpeed 3", ' ', $data->getGustMarker());

    $data->setGustSpeed(11, "m/s", Conversions::getEstimateFlag());
    assertFloat("GustSpeed 4", 11, $data->getGustSpeed());
    assertString("GustSpeed 4", 'E', $data->getGustSpeedFlag());
    assertString("GustSpeed 4", 'G', $data->getGustMarker());


# Will Die:    $data->setGustSpeed(11, "m/s", 'A');
# Will Die:    $data->setGustSpeed(11, "m/s", 'M');
# Will Die:    $data->setGustSpeed($missing, "m/s", 'U');
}

sub testPrecip {
    my $data = shift;
    my $missing = Conversions::getMissing();

    print ("Testing Precip...\n");

    assertFloat("Precip 1", $missing, $data->getPrecip());
    assertString("Precip 1", 'M', $data->getPrecipFlag());

    $data->setPrecip(11, "mm");
    assertFloat("Precip 2", 11, $data->getPrecip());
    assertString("Precip 2", 'U', $data->getPrecipFlag());

    $data->setPrecip($missing, "mm");
    assertFloat("Precip 3", $missing, $data->getPrecip());
    assertString("Precip 3", 'M', $data->getPrecipFlag());

    $data->setPrecip(11, "mm", Conversions::getEstimateFlag());
    assertFloat("Precip 4", 11, $data->getPrecip());
    assertString("Precip 4", 'E', $data->getPrecipFlag());

# Will Die:    $data->setPrecip(11, "mm", 'A');
# Will Die:    $data->setPrecip(11, "mm", 'M');
# Will Die:    $data->setPrecip($missing, "mm", 'U');
}

sub testPressure {
    my $data = shift;
    my $missing = Conversions::getMissing();

    print ("Testing Pressure...\n");

    assertFloat("Pressure 1", $missing, $data->getPressure());
    assertString("Pressure 1", 'M', $data->getPressureFlag());

    $data->setPressure(1111, "mb");
    assertFloat("Pressure 2", 1111, $data->getPressure());
    assertString("Pressure 2", 'U', $data->getPressureFlag());

    $data->setPressure($missing, "mb");
    assertFloat("Pressure 3", $missing, $data->getPressure());
    assertString("Pressure 3", 'M', $data->getPressureFlag());

    $data->setPressure(1111, "mb", Conversions::getEstimateFlag());
    assertFloat("Pressure 4", 1111, $data->getPressure());
    assertString("Pressure 4", 'E', $data->getPressureFlag());

# Will Die:    $data->setPressure(1111, "mb", 'A');
# Will Die:    $data->setPressure(1111, "mb", 'M');
# Will Die:    $data->setPressure($missing, "mb", 'U');
}

sub testSeaLevelPressure {
    my $data = shift;
    my $missing = Conversions::getMissing();

    print ("Testing SeaLevelPressure...\n");

    assertFloat("SeaLevelPressure 1", $missing, $data->getSeaLevelPressure());
    assertString("SeaLevelPressure 1", 'M', $data->getSeaLevelPressureFlag());

    $data->setSeaLevelPressure(1111, "mb");
    assertFloat("SeaLevelPressure 2", 1111, $data->getSeaLevelPressure());
    assertString("SeaLevelPressure 2", 'U', $data->getSeaLevelPressureFlag());

    $data->setSeaLevelPressure($missing, "mb");
    assertFloat("SeaLevelPressure 3", $missing, $data->getSeaLevelPressure());
    assertString("SeaLevelPressure 3", 'M', $data->getSeaLevelPressureFlag());

    $data->setSeaLevelPressure(1111, "mb", Conversions::getEstimateFlag());
    assertFloat("SeaLevelPressure 4", 1111, $data->getSeaLevelPressure());
    assertString("SeaLevelPressure 4", 'E', $data->getSeaLevelPressureFlag());

# Will Die:    $data->setSeaLevelPressure(1111, "mb", 'A');
# Will Die:    $data->setSeaLevelPressure(1111, "mb", 'M');
# Will Die:    $data->setSeaLevelPressure($missing, "mb", 'U');
}

sub testTemperature {
    my $data = shift;
    my $missing = Conversions::getMissing();

    print ("Testing Temperature...\n");

    assertFloat("Temperature 1", $missing, $data->getTemperature());
    assertString("Temperature 1", 'M', $data->getTemperatureFlag());

    $data->setTemperature(11, "C");
    assertFloat("Temperature 2", 11, $data->getTemperature());
    assertString("Temperature 2", 'U', $data->getTemperatureFlag());

    $data->setTemperature($missing, "C");
    assertFloat("Temperature 3", $missing, $data->getTemperature());
    assertString("Temperature 3", 'M', $data->getTemperatureFlag());

    $data->setTemperature(11, "C", Conversions::getEstimateFlag());
    assertFloat("Temperature 4", 11, $data->getTemperature());
    assertString("Temperature 4", 'E', $data->getTemperatureFlag());

# Will Die:    $data->setTemperature(11, "C", 'A');
# Will Die:    $data->setTemperature(11, "C", 'M');
# Will Die:    $data->setTemperature($missing, "C", 'U');
}

sub testVisibility {
    my $data = shift;
    my $missing = Conversions::getMissing();

    print ("Testing Visibility...\n");

    assertFloat("Visibility 1", $missing, $data->getVisibility());
    assertString("Visibility 1", 'M', $data->getVisibilityFlag());

    $data->setVisibility(1111, "m");
    assertFloat("Visibility 2", 1111, $data->getVisibility());
    assertString("Visibility 2", 'U', $data->getVisibilityFlag());

    $data->setVisibility($missing, "m");
    assertFloat("Visibility 3", $missing, $data->getVisibility());
    assertString("Visibility 3", 'M', $data->getVisibilityFlag());

    $data->setVisibility(1111, "m", Conversions::getEstimateFlag());
    assertFloat("Visibility 4", 1111, $data->getVisibility());
    assertString("Visibility 4", 'E', $data->getVisibilityFlag());

# Will Die:    $data->setVisibility(1111, "m", 'A');
# Will Die:    $data->setVisibility(1111, "m", 'M');
# Will Die:    $data->setVisibility($missing, "m", 'U');
}

sub testWindDirection {
    my $data = shift;
    my $missing = Conversions::getMissing();

    print ("Testing Wind Direction...\n");

    assertFloat("WindDirection 1", $missing, $data->getWindDirection());
    assertString("WindDirection 1", 'M', $data->getWindDirectionFlag());

    $data->setWindDirection(11);
    assertFloat("WindDirection 2", 11, $data->getWindDirection());
    assertString("WindDirection 2", 'U', $data->getWindDirectionFlag());

    $data->setWindDirection($missing);
    assertFloat("WindDirection 3", $missing, $data->getWindDirection());
    assertString("WindDirection 3", 'M', $data->getWindDirectionFlag());

    $data->setWindDirection(11, Conversions::getEstimateFlag());
    assertFloat("WindDirection 4", 11, $data->getWindDirection());
    assertString("WindDirection 4", 'E', $data->getWindDirectionFlag());

# Will Die:    $data->setWindDirection(11, 'A');
# Will Die:    $data->setWindDirection(11, 'M');
# Will Die:    $data->setWindDirection($missing, 'U');
}

sub testWindSpeed {
    my $data = shift;
    my $missing = Conversions::getMissing();

    print ("Testing Wind Speed...\n");

    assertFloat("WindSpeed 1", $missing, $data->getWindSpeed());
    assertString("WindSpeed 1", 'M', $data->getWindSpeedFlag());

    $data->setWindSpeed(11, "m/s");
    assertFloat("WindSpeed 2", 11, $data->getWindSpeed());
    assertString("WindSpeed 2", 'U', $data->getWindSpeedFlag());

    $data->setWindSpeed($missing, "m/s");
    assertFloat("WindSpeed 3", $missing, $data->getWindSpeed());
    assertString("WindSpeed 3", 'M', $data->getWindSpeedFlag());

    $data->setWindSpeed(11, "m/s", Conversions::getEstimateFlag());
    assertFloat("WindSpeed 4", 11, $data->getWindSpeed());
    assertString("WindSpeed 4", 'E', $data->getWindSpeedFlag());

# Will Die:    $data->setWindSpeed(11, "m/s", 'A');
# Will Die:    $data->setWindSpeed(11, "m/s", 'M');
# Will Die:    $data->setWindSpeed($missing, "m/s", 'U');
}

sub testWx {
    my $data = shift;
    my $missing = -999;

    print ("Testing Present Weather...\n");

    assertFloat("PresentWeather 1", $missing, $data->getPresentWeather());
    assertString("PresentWeather 1", 'M', $data->getPresentWeatherFlag());

    $data->setPresentWeather(11);
    assertFloat("PresentWeather 2", 11, $data->getPresentWeather());
    assertString("PresentWeather 2", 'U', $data->getPresentWeatherFlag());

    $data->setPresentWeather($missing);
    assertFloat("PresentWeather 3", $missing, $data->getPresentWeather());
    assertString("PresentWeather 3", 'M', $data->getPresentWeatherFlag());

    $data->setPresentWeather(11, Conversions::getEstimateFlag());
    assertFloat("PresentWeather 4", 11, $data->getPresentWeather());
    assertString("PresentWeather 4", 'E', $data->getPresentWeatherFlag());

# Will Die:    $data->setPresentWeather(11, 'A');
# Will Die:    $data->setPresentWeather(11, 'M');
# Will Die:    $data->setPresentWeather($missing, 'U');
}
