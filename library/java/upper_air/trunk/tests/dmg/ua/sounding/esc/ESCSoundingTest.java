package dmg.ua.sounding.esc;

import static dmg.ua.sounding.esc.ESCSounding.*;
import static dmg.ua.sounding.esc.ESCSoundingRecord.*;
import static dmg.util.LengthUtils.*;
import static dmg.util.PressureUtils.*;
import static dmg.util.TemperatureUtils.*;
import static dmg.util.TimeUtils.*;
import static dmg.util.VelocityUtils.*;
import static org.junit.Assert.*;
import dmg.station.*;
import dmg.ua.sounding.*;
import dmg.util.*;

import org.junit.*;

public class ESCSoundingTest extends SoundingTest<ESCSounding> {
	
	@Override
	public ESCSounding newInstance(Station station) { 
		return new ESCSounding(station);
	}
	
	@Test public void tostring() throws CalculationWarning, ConversionException,
	DateTimeException, InvalidFlagException, InvalidValueException,
	InvalidValueWarning {
		assertEquals("toString: default", 
				"Data Type:                         \nProject ID:                        \nRelease Site Type/Site ID:         \nRelease Location (lon,lat,alt):    9999 00.00'E, 999 00.00'N, 9999.000, 999.000, 99999.0\nUTC Release Time (y,m,d,h,m,s):    0000, 00, 00, 00:00:00\n/\n/\n/\n/\n/\n/\nNominal Release Time (y,m,d,h,m,s):0000, 00, 00, 00:00:00\n Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat   Ele   Azi    Alt    Qp   Qt   Qrh  Qu   Qv   QdZ\n  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     m    code code code code code code\n------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----", sounding.toString());
		
		sounding.setActualRelease(2006, 3, 8, 23, 2, 0, UTC);
		sounding.setAltitude(2.0, METERS);
		sounding.setDataType("National Weather Service Sounding.");
		sounding.setLatitude(37.7);
		sounding.setLongitude(-122.2);
		sounding.setNominalRelease(2006, 3, 9, 0, 0, 0, UTC);
		sounding.setProjectId("0");
		sounding.setReleaseDirection(ASCENDING);
		sounding.setStationDescription("OAK Oakland, CA");
		sounding.setStationId("OAK");
		sounding.setVariableMeasurement1("Ele");
		sounding.setVariableMeasurement2("Azi");
		sounding.setVariableUnit1("deg");
		sounding.setVariableUnit2("deg");
		
		sounding.setHeaderLine(6, "Ascension No:", "1175");
		sounding.setHeaderLine(7, "Radiosonde Serial Number:", "84966655.CSN");
		sounding.setHeaderLine(8, "Radiosonde Manufacturer:", "VIZ B2");
		
		assertEquals("toString: no data", "Data Type:                         National Weather Service Sounding./Ascending\nProject ID:                        0\nRelease Site Type/Site ID:         OAK Oakland, CA\nRelease Location (lon,lat,alt):    122 12.00'W, 37 42.00'N, -122.200, 37.700, 2.0\nUTC Release Time (y,m,d,h,m,s):    2006, 03, 08, 23:02:00\nAscension No:                      1175\nRadiosonde Serial Number:          84966655.CSN\nRadiosonde Manufacturer:           VIZ B2\n/\n/\n/\nNominal Release Time (y,m,d,h,m,s):2006, 03, 09, 00:00:00\n Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat   Ele   Azi    Alt    Qp   Qt   Qrh  Qu   Qv   QdZ\n  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     m    code code code code code code\n------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----", sounding.toString());
		
		ESCSoundingRecord first = new ESCSoundingRecord();
		first.setTime(0.0);
		first.setPressure(1021.6, MILLIBARS);
		first.setTemperature(13.0, CELCIUS);
		first.setDewPoint(6.1, CELCIUS);
		first.setRelativeHumidity(63.0);
		first.setUComponent(4.1, METERS_PER_SECOND);
		first.setVComponent(0.0, METERS_PER_SECOND);
		first.setWindSpeed(4.1, METERS_PER_SECOND);
		first.setWindDirection(180.0);
		first.setLongitude(-122.2);
		first.setLatitude(37.7);
		first.setVariableField1(46.7);
		first.setVariableField2(287.6);
		first.setAltitude(2.0, METERS);
		first.setPressureFlag(QUESTIONABLE_FLAG);
		first.setTemperatureFlag(QUESTIONABLE_FLAG);
		first.setRelativeHumidityFlag(QUESTIONABLE_FLAG);
		first.setUComponentFlag(GOOD_FLAG);
		first.setVComponentFlag(GOOD_FLAG);		
		sounding.add(first);
		
		ESCSoundingRecord second = new ESCSoundingRecord();
		second.setTime(6.0);
		second.setPressure(1014.2, MILLIBARS);
		second.setTemperature(11.8, CELCIUS);
		second.setDewPoint(5.1, CELCIUS);
		second.setRelativeHumidity(63.6);
		second.setUComponent(4.1, METERS_PER_SECOND);
		second.setVComponent(-0.0, METERS_PER_SECOND);
		second.setWindSpeed(4.1, METERS_PER_SECOND);
		second.setWindDirection(180.0);
		second.setAscentRate(10.2, METERS_PER_SECOND);
		second.setLongitude(-122.2);
		second.setLatitude(37.7);
		second.setVariableField1(36.3);
		second.setVariableField2(275.3);
		second.setAltitude(63.0, METERS);
		second.setPressureFlag(QUESTIONABLE_FLAG);
		second.setTemperatureFlag(QUESTIONABLE_FLAG);
		second.setRelativeHumidityFlag(QUESTIONABLE_FLAG);
		second.setUComponentFlag(ESTIMATE_FLAG);
		second.setVComponentFlag(ESTIMATE_FLAG);
		second.setAscentRateFlag(UNCHECKED_FLAG);
		sounding.add(second);
		
		ESCSoundingRecord third = new ESCSoundingRecord();
		third.setTime(12.0);
		third.setPressure(1010.3, MILLIBARS);
		third.setTemperature(11.4, CELCIUS);
		third.setDewPoint(5.2, CELCIUS);
		third.setRelativeHumidity(65.6);
		third.setUComponent(4.1, METERS_PER_SECOND);
		third.setVComponent(-0.1, METERS_PER_SECOND);
		third.setWindSpeed(4.1, METERS_PER_SECOND);
		third.setWindDirection(271.4);
		third.setAscentRate(5.3, METERS_PER_SECOND);
		third.setLongitude(-122.199);
		third.setLatitude(37.7);
		third.setVariableField1(27.0);
		third.setVariableField2(282.1);
		third.setAltitude(95.0, METERS);
		third.setPressureFlag(QUESTIONABLE_FLAG);
		third.setTemperatureFlag(GOOD_FLAG);
		third.setRelativeHumidityFlag(GOOD_FLAG);
		third.setUComponentFlag(ESTIMATE_FLAG);
		third.setVComponentFlag(ESTIMATE_FLAG);
		third.setAscentRateFlag(UNCHECKED_FLAG);
		sounding.add(third);
		
		assertEquals("toString: with data", "Data Type:                         National Weather Service Sounding./Ascending\nProject ID:                        0\nRelease Site Type/Site ID:         OAK Oakland, CA\nRelease Location (lon,lat,alt):    122 12.00'W, 37 42.00'N, -122.200, 37.700, 2.0\nUTC Release Time (y,m,d,h,m,s):    2006, 03, 08, 23:02:00\nAscension No:                      1175\nRadiosonde Serial Number:          84966655.CSN\nRadiosonde Manufacturer:           VIZ B2\n/\n/\n/\nNominal Release Time (y,m,d,h,m,s):2006, 03, 09, 00:00:00\n Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat   Ele   Azi    Alt    Qp   Qt   Qrh  Qu   Qv   QdZ\n  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     m    code code code code code code\n------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----\n   0.0 1021.6  13.0   6.1  63.0    4.1    0.0   4.1 180.0 999.0 -122.200  37.700  46.7 287.6     2.0  2.0  2.0  2.0  1.0  1.0  9.0\n   6.0 1014.2  11.8   5.1  63.6    4.1    0.0   4.1 180.0  10.2 -122.200  37.700  36.3 275.3    63.0  2.0  2.0  2.0  4.0  4.0 99.0\n  12.0 1010.3  11.4   5.2  65.6    4.1   -0.1   4.1 271.4   5.3 -122.199  37.700  27.0 282.1    95.0  2.0  1.0  1.0  4.0  4.0 99.0", sounding.toString());
	}
	
	
	@Test public void variableMeasurement1() throws InvalidValueException {
		assertNull("Var Measurement 1: default", 
				sounding.getVariableMeasurement1());
		sounding.setVariableMeasurement1("Ele");
		assertEquals("Var Measurement 1: value", "Ele", 
				sounding.getVariableMeasurement1());
		sounding.setVariableMeasurement1(null);
		assertNull("Var Measurement 1: null", 
				sounding.getVariableMeasurement1());
		sounding.setVariableMeasurement1("");
		assertNull("Var Measurement 1: empty string", 
				sounding.getVariableMeasurement1());
		sounding.setVariableMeasurement1("  ");
		assertNull("Var Measurement 1: sequence of spaces", 
				sounding.getVariableMeasurement1());
	}
	
	@Test (expected = InvalidValueException.class)
	public void variableMeasurement1TooBig() throws InvalidValueException {
		sounding.setVariableMeasurement1("12345");
	}

	@Test public void variableMeasurement2() throws InvalidValueException {
		assertNull("Var Measurement 2: default", 
				sounding.getVariableMeasurement2());
		sounding.setVariableMeasurement2("Azi");
		assertEquals("Var Measurement 2: value", "Azi", 
				sounding.getVariableMeasurement2());
		sounding.setVariableMeasurement2(null);
		assertNull("Var Measurement 2: null", 
				sounding.getVariableMeasurement2());
		sounding.setVariableMeasurement2("");
		assertNull("Var Measurement 2: empty string", 
				sounding.getVariableMeasurement2());
		sounding.setVariableMeasurement2("  ");
		assertNull("Var Measurement 2: sequence of spaces", 
				sounding.getVariableMeasurement2());
	}
	
	@Test (expected = InvalidValueException.class)
	public void variableMeasurement2TooBig() throws InvalidValueException {
		sounding.setVariableMeasurement2("12345");
	}
	
	@Test public void variableUnit1() throws InvalidValueException {
		assertNull("Var Unit 1: default", sounding.getVariableUnit1());
		sounding.setVariableUnit1("Deg");
		assertEquals("Var Unit 1: value", "Deg", sounding.getVariableUnit1());
		sounding.setVariableUnit1(null);
		assertNull("Var Unit 1: null", sounding.getVariableUnit1());
		sounding.setVariableUnit1("");
		assertNull("Var Unit 1: empty string", sounding.getVariableUnit1());
		sounding.setVariableUnit1("  ");
		assertNull("Var Unit 1: sequence of spaces", 
				sounding.getVariableUnit1());
	}
	
	@Test (expected = InvalidValueException.class)
	public void variableUnit1TooBig() throws InvalidValueException {
		sounding.setVariableUnit1("12345");
	}

	@Test public void variableUnit2() throws InvalidValueException {
		assertNull("Var Unit 2: default", sounding.getVariableUnit2());
		sounding.setVariableUnit2("Deg");
		assertEquals("Var Unit 2: value", "Deg", sounding.getVariableUnit2());
		sounding.setVariableUnit2(null);
		assertNull("Var Unit 2: null", sounding.getVariableUnit2());
		sounding.setVariableUnit2("");
		assertNull("Var Unit 2: empty string", sounding.getVariableUnit2());
		sounding.setVariableUnit2("  ");
		assertNull("Var Unit 2: sequence of spaces",
				sounding.getVariableUnit2());
	}
	
	@Test (expected = InvalidValueException.class)
	public void variableUnit2TooBig() throws InvalidValueException {
		sounding.setVariableUnit2("12345");
	}
}
