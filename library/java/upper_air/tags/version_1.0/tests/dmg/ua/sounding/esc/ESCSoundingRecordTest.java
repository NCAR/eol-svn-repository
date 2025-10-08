package dmg.ua.sounding.esc;

import static dmg.ua.sounding.esc.ESCSoundingRecord.*;
import static dmg.util.LengthUtils.*;
import static dmg.util.PressureUtils.*;
import static dmg.util.TemperatureUtils.*;
import static dmg.util.VelocityUtils.*;
import static org.junit.Assert.*;

import java.io.*;

import dmg.ua.sounding.*;
import dmg.util.*;

import org.junit.*;

public class ESCSoundingRecordTest extends 
SoundingRecordTest<ESCSoundingRecord> {

	public ESCSoundingRecord newInstance(ESCSoundingRecord previousRecord) {
		return new ESCSoundingRecord(previousRecord);
	}
	
	@Before
	public void setUp() throws Exception {
		previousRecord = new ESCSoundingRecord();
		previousRecord.setTime(0.0);
		previousRecord.setAltitude(1000.0, METERS);
		record = new ESCSoundingRecord(previousRecord);
	}

	@Test public void altitudeMissingValue() throws CalculationWarning,
	ConversionException, InvalidValueWarning {
		assertNull("Altitude (miss): default", record.getAltitude());
		record.setAltitude(1000.0, METERS);
		assertEquals("Altitude (miss): value", 1000.0, record.getAltitude(), 
				THRESHOLD);
		record.setAltitude(99999.0, METERS);
		assertNull("Altitude (miss): missing value", record.getAltitude());
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void altitudeTooBig() throws CalculationWarning, ConversionException,
	InvalidValueWarning {
		record.setAltitude(99999.95, METERS);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void altitudeTooSmall() throws CalculationWarning,
	ConversionException, InvalidValueWarning {
		record.setAltitude(-9999.95, METERS);
	}
	
	@Test public void ascentRateMissingValue() throws CalculationWarning,
	ConversionException, InvalidValueWarning {
		assertNull("Ascent Rate (miss): default", record.getAscentRate());
		record.setAscentRate(10.0, METERS_PER_SECOND);
		assertEquals("Ascent Rate (miss): value", 10.0, record.getAscentRate(),
				THRESHOLD);
		record.setAscentRate(999.0, METERS_PER_SECOND);
		assertNull("Ascent Rate (miss): missing value", record.getAscentRate());
	}
	
	@Test public void ascentRateFlag() throws CalculationWarning,
	ConversionException, InvalidFlagException, InvalidValueWarning {
		assertEquals("Ascent Rate Flag: default missing", MISSING_FLAG,
				record.getAscentRateFlag());
		record.setAscentRate(10.0, METERS_PER_SECOND);
		assertEquals("Ascent Rate Flag: default value", UNCHECKED_FLAG, 
				record.getAscentRateFlag());
		record.setAscentRateFlag(GOOD_FLAG);
		assertEquals("Ascent Rate Flag: set flag", GOOD_FLAG,
				record.getAscentRateFlag());
		record.setAscentRateFlag(null);
		assertEquals("Ascent Rate Flag: set flag null", UNCHECKED_FLAG,
				record.getAscentRateFlag());
		record.setAscentRate(null, METERS_PER_SECOND);
		assertEquals("Ascent Rate Flag: nulled value", MISSING_FLAG,
				record.getAscentRateFlag());
	}

	@Test (expected = InvalidFlagException.class)
	public void ascentRateFlagMissingFlag() throws CalculationWarning,
	ConversionException, InvalidFlagException, InvalidValueWarning {
		record.setAscentRate(10.0, METERS_PER_SECOND);
		record.setAscentRateFlag(MISSING_FLAG);
	}
	
	@Test (expected = InvalidFlagException.class)
	public void ascentRateFlagMissingValue() throws InvalidFlagException {
		record.setAscentRateFlag(GOOD_FLAG);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void ascentRateTooBig() throws CalculationWarning,
	ConversionException, InvalidValueWarning {
		record.setAscentRate(1000.0, METERS_PER_SECOND);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void ascentRateTooSmall() throws CalculationWarning,
	ConversionException, InvalidValueWarning {
		record.setAscentRate(-100.0, METERS_PER_SECOND);
	}
	
	@Test public void dewPointMissingValue() throws CalculationWarning,
	ConversionException, InvalidValueWarning {
		assertNull("Dew Point (miss): default", record.getDewPoint());
		record.setDewPoint(10.0, CELCIUS);
		assertEquals("Dew Point (miss): value", 10.0, record.getDewPoint(),
				THRESHOLD);
		record.setDewPoint(999.0, CELCIUS);
		assertNull("Dew Point (miss): missing value", record.getDewPoint());
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void dewPointTooBig() throws CalculationWarning,
	ConversionException, InvalidValueWarning {
		record.setDewPoint(1000.0, CELCIUS);
	}
	
	@Test public void dewPointTooSmall() throws CalculationWarning,
	ConversionException, InvalidValueWarning {
		record.setDewPoint(-100.0, CELCIUS);
		assertEquals("Dew Point (neg)", -99.9, record.getDewPoint(), THRESHOLD);
	}
	
	@Test public void flagComparison() {
		assertTrue("UNCHECKED, UNCHECKED", 
				UNCHECKED_FLAG.compareTo(UNCHECKED_FLAG) == 0);
		assertTrue("UNCHECKED, MISSING", 
				UNCHECKED_FLAG.compareTo(MISSING_FLAG) < 0);
		assertTrue("UNCHECKED, BAD", 
				UNCHECKED_FLAG.compareTo(BAD_FLAG) < 0);
		assertTrue("UNCHECKED, QUESTIONABLE", 
				UNCHECKED_FLAG.compareTo(QUESTIONABLE_FLAG) < 0);
		assertTrue("UNCHECKED, ESTIMATE", 
				UNCHECKED_FLAG.compareTo(ESTIMATE_FLAG) < 0);
		assertTrue("UNCHECKED, GOOD", 
				UNCHECKED_FLAG.compareTo(GOOD_FLAG) < 0);

		assertTrue("MISSING, UNCHECKED", 
				MISSING_FLAG.compareTo(UNCHECKED_FLAG) > 0);
		assertTrue("MISSING, MISSING", 
				MISSING_FLAG.compareTo(MISSING_FLAG) == 0);
		assertTrue("MISSING, BAD", 
				MISSING_FLAG.compareTo(BAD_FLAG) < 0);
		assertTrue("MISSING, QUESTIONABLE", 
				MISSING_FLAG.compareTo(QUESTIONABLE_FLAG) < 0);
		assertTrue("MISSING, ESTIMATE", 
				MISSING_FLAG.compareTo(ESTIMATE_FLAG) < 0);
		assertTrue("MISSING, GOOD",
				MISSING_FLAG.compareTo(GOOD_FLAG) < 0);

		assertTrue("BAD, UNCHECKED", BAD_FLAG.compareTo(UNCHECKED_FLAG) > 0);
		assertTrue("BAD, MISSING", BAD_FLAG.compareTo(MISSING_FLAG) > 0);
		assertTrue("BAD, BAD", BAD_FLAG.compareTo(BAD_FLAG) == 0);
		assertTrue("BAD, QUESTIONABLE", 
				BAD_FLAG.compareTo(QUESTIONABLE_FLAG) < 0);
		assertTrue("BAD, ESTIMATE", BAD_FLAG.compareTo(ESTIMATE_FLAG) < 0);
		assertTrue("BAD, GOOD", BAD_FLAG.compareTo(GOOD_FLAG) < 0);

		assertTrue("QUESTIONABLE, UNCHECKED", 
				QUESTIONABLE_FLAG.compareTo(UNCHECKED_FLAG) > 0);
		assertTrue("QUESTIONABLE, MISSING",
				QUESTIONABLE_FLAG.compareTo(MISSING_FLAG) > 0);
		assertTrue("QUESTIONABLE, BAD",
				QUESTIONABLE_FLAG.compareTo(BAD_FLAG) > 0);
		assertTrue("QUESTIONABLE, QUESTIONABLE",
				QUESTIONABLE_FLAG.compareTo(QUESTIONABLE_FLAG) == 0);
		assertTrue("QUESTIONABLE, ESTIMATE",
				QUESTIONABLE_FLAG.compareTo(ESTIMATE_FLAG) < 0);
		assertTrue("QUESTIONABLE, GOOD", 
				QUESTIONABLE_FLAG.compareTo(GOOD_FLAG) < 0);

		assertTrue("ESTIMATE, UNCHECKED", 
				ESTIMATE_FLAG.compareTo(UNCHECKED_FLAG) > 0);
		assertTrue("ESTIMATE, MISSING", 
				ESTIMATE_FLAG.compareTo(MISSING_FLAG) > 0);
		assertTrue("ESTIMATE, BAD", ESTIMATE_FLAG.compareTo(BAD_FLAG) > 0);
		assertTrue("ESTIMATE, QUESTIONABLE", 
				ESTIMATE_FLAG.compareTo(QUESTIONABLE_FLAG) > 0);
		assertTrue("ESTIMATE, ESTIMATE", 
				ESTIMATE_FLAG.compareTo(ESTIMATE_FLAG) == 0);
		assertTrue("ESTIMATE, GOOD", ESTIMATE_FLAG.compareTo(GOOD_FLAG) < 0);

		assertTrue("GOOD, UNCHECKED", GOOD_FLAG.compareTo(UNCHECKED_FLAG) > 0);
		assertTrue("GOOD, MISSING", GOOD_FLAG.compareTo(MISSING_FLAG) > 0);
		assertTrue("GOOD, BAD", GOOD_FLAG.compareTo(BAD_FLAG) > 0);
		assertTrue("GOOD, QUESTIONABLE", 
				GOOD_FLAG.compareTo(QUESTIONABLE_FLAG) > 0);
		assertTrue("GOOD, ESTIMATE", GOOD_FLAG.compareTo(ESTIMATE_FLAG) > 0);
		assertTrue("GOOD, GOOD", GOOD_FLAG.compareTo(GOOD_FLAG) == 0);
	}
	
	@Test public void flagDegrade() {
		assertEquals("Degrade: UNCHECKED", 
				UNCHECKED_FLAG, UNCHECKED_FLAG.degrade());
		assertEquals("Degrade: MISSING", 
				MISSING_FLAG, MISSING_FLAG.degrade());
		assertEquals("Degrade: BAD", BAD_FLAG, BAD_FLAG.degrade());
		assertEquals("Degrade: QUESTIONABLE", 
				BAD_FLAG, QUESTIONABLE_FLAG.degrade());
		assertEquals("Degrade: ESTIMATE", 
				QUESTIONABLE_FLAG, ESTIMATE_FLAG.degrade());
		assertEquals("Degrade: GOOD", 
				QUESTIONABLE_FLAG, GOOD_FLAG.degrade());
	}
	
	@Test public void flagIsWorse() {
		assertFalse("UNCHECKED, UNCHECKED", 
				UNCHECKED_FLAG.isWorseFlag(UNCHECKED_FLAG));
		assertTrue("UNCHECKED, MISSING", 
				UNCHECKED_FLAG.isWorseFlag(MISSING_FLAG));
		assertTrue("UNCHECKED, BAD", 
				UNCHECKED_FLAG.isWorseFlag(BAD_FLAG));
		assertTrue("UNCHECKED, QUESTIONABLE", 
				UNCHECKED_FLAG.isWorseFlag(QUESTIONABLE_FLAG));
		assertTrue("UNCHECKED, ESTIMATE", 
				UNCHECKED_FLAG.isWorseFlag(ESTIMATE_FLAG));
		assertTrue("UNCHECKED, GOOD", UNCHECKED_FLAG.isWorseFlag(GOOD_FLAG));

		assertFalse("MISSING, UNCHECKED", 
				MISSING_FLAG.isWorseFlag(UNCHECKED_FLAG));
		assertFalse("MISSING, MISSING", MISSING_FLAG.isWorseFlag(MISSING_FLAG));
		assertTrue("MISSING, BAD", MISSING_FLAG.isWorseFlag(BAD_FLAG));
		assertTrue("MISSING, QUESTIONABLE", 
				MISSING_FLAG.isWorseFlag(QUESTIONABLE_FLAG));
		assertTrue("MISSING, ESTIMATE", 
				MISSING_FLAG.isWorseFlag(ESTIMATE_FLAG));
		assertTrue("MISSING, GOOD", MISSING_FLAG.isWorseFlag(GOOD_FLAG));

		assertFalse("BAD, UNCHECKED", BAD_FLAG.isWorseFlag(UNCHECKED_FLAG));
		assertFalse("BAD, MISSING", BAD_FLAG.isWorseFlag(MISSING_FLAG));
		assertFalse("BAD, BAD", BAD_FLAG.isWorseFlag(BAD_FLAG));
		assertTrue("BAD, QUESTIONABLE", 
				BAD_FLAG.isWorseFlag(QUESTIONABLE_FLAG));
		assertTrue("BAD, ESTIMATE", BAD_FLAG.isWorseFlag(ESTIMATE_FLAG));
		assertTrue("BAD, GOOD", BAD_FLAG.isWorseFlag(GOOD_FLAG));

		assertFalse("QUESTIONABLE, UNCHECKED", 
				QUESTIONABLE_FLAG.isWorseFlag(UNCHECKED_FLAG));
		assertFalse("QUESTIONABLE, MISSING", 
				QUESTIONABLE_FLAG.isWorseFlag(MISSING_FLAG));
		assertFalse("QUESTIONABLE, BAD", 
				QUESTIONABLE_FLAG.isWorseFlag(BAD_FLAG));
		assertFalse("QUESTIONABLE, QUESTIONABLE", 
				QUESTIONABLE_FLAG.isWorseFlag(QUESTIONABLE_FLAG));
		assertTrue("QUESTIONABLE, ESTIMATE", 
				QUESTIONABLE_FLAG.isWorseFlag(ESTIMATE_FLAG));
		assertTrue("QUESTIONABLE, GOOD", 
				QUESTIONABLE_FLAG.isWorseFlag(GOOD_FLAG));

		assertFalse("ESTIMATE, UNCHECKED", 
				ESTIMATE_FLAG.isWorseFlag(UNCHECKED_FLAG));
		assertFalse("ESTIMATE, MISSING", 
				ESTIMATE_FLAG.isWorseFlag(MISSING_FLAG));
		assertFalse("ESTIMATE, BAD", ESTIMATE_FLAG.isWorseFlag(BAD_FLAG));
		assertFalse("ESTIMATE, QUESTIONABLE", 
				ESTIMATE_FLAG.isWorseFlag(QUESTIONABLE_FLAG));
		assertFalse("ESTIMATE, ESTIMATE", 
				ESTIMATE_FLAG.isWorseFlag(ESTIMATE_FLAG));
		assertTrue("ESTIMATE, GOOD", ESTIMATE_FLAG.isWorseFlag(GOOD_FLAG));

		assertFalse("GOOD, UNCHECKED", GOOD_FLAG.isWorseFlag(UNCHECKED_FLAG));
		assertFalse("GOOD, MISSING", GOOD_FLAG.isWorseFlag(MISSING_FLAG));
		assertFalse("GOOD, BAD", GOOD_FLAG.isWorseFlag(BAD_FLAG));
		assertFalse("GOOD, QUESTIONABLE", 
				GOOD_FLAG.isWorseFlag(QUESTIONABLE_FLAG));
		assertFalse("GOOD, ESTIMATE", GOOD_FLAG.isWorseFlag(ESTIMATE_FLAG));
		assertFalse("GOOD, GOOD", GOOD_FLAG.isWorseFlag(GOOD_FLAG));
	}

	@Test public void pressureMissingValue() throws CalculationWarning,
	ConversionException, InvalidValueWarning {
		assertNull("Pressure (miss): default", record.getPressure());
		record.setPressure(1000.0, MILLIBARS);
		assertEquals("Pressure (miss): value", 1000.0, record.getPressure(),
				THRESHOLD);
		record.setPressure(9999.0, MILLIBARS);
		assertNull("Pressure (miss): missing value", record.getPressure());
	}
	
	@Test public void pressureFlag() throws CalculationWarning,
	ConversionException, InvalidFlagException, InvalidValueWarning {
		assertEquals("Pressure Flag: default missing", MISSING_FLAG,
				record.getPressureFlag());
		record.setPressure(1000.0, MILLIBARS);
		assertEquals("Pressure Flag: default value", UNCHECKED_FLAG, 
				record.getPressureFlag());
		record.setPressureFlag(GOOD_FLAG);
		assertEquals("Pressure Flag: set flag", GOOD_FLAG,
				record.getPressureFlag());
		record.setPressureFlag(null);
		assertEquals("Pressure Flag: set flag null", UNCHECKED_FLAG,
				record.getPressureFlag());
		record.setPressure(null, MILLIBARS);
		assertEquals("Pressure Flag: nulled value", MISSING_FLAG, 
				record.getPressureFlag());
	}

	@Test (expected = InvalidFlagException.class)
	public void pressureFlagMissingFlag() throws CalculationWarning,
	ConversionException, InvalidFlagException, InvalidValueWarning {
		record.setPressure(1000.0, MILLIBARS);
		record.setPressureFlag(MISSING_FLAG);
	}
	
	@Test (expected = InvalidFlagException.class)
	public void pressureFlagMissingValue() throws InvalidFlagException {
		record.setPressureFlag(GOOD_FLAG);
	}

	@Test (expected = InvalidValueWarning.class)
	public void pressureTooBig() throws CalculationWarning,
	ConversionException, InvalidValueWarning {
		record.setPressure(10000.0, MILLIBARS);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void pressureTooSmall() throws CalculationWarning,
	ConversionException, InvalidValueWarning {
		record.setPressure(-1000.0, MILLIBARS);
	}
	
	@Test public void relativeHumidityMissingValue() throws CalculationWarning,
	InvalidValueWarning {
		assertNull("RelativeHumidity (miss): default", 
				record.getRelativeHumidity());
		record.setRelativeHumidity(10.0);
		assertEquals("RelativeHumidity (miss): value", 10.0, 
				record.getRelativeHumidity(), THRESHOLD);
		record.setRelativeHumidity(999.0);
		assertNull("RelativeHumidity (miss): missing value", 
				record.getRelativeHumidity());
	}
	
	@Test public void relativeHumidityFlag() throws CalculationWarning,
	InvalidFlagException, InvalidValueWarning {
		assertEquals("RelativeHumidity Flag: default missing", MISSING_FLAG,
				record.getRelativeHumidityFlag());
		record.setRelativeHumidity(10.0);
		assertEquals("RelativeHumidity Flag: default value", UNCHECKED_FLAG, 
				record.getRelativeHumidityFlag());
		record.setRelativeHumidityFlag(GOOD_FLAG);
		assertEquals("RelativeHumidity Flag: set flag", GOOD_FLAG, 
				record.getRelativeHumidityFlag());
		record.setRelativeHumidityFlag(null);
		assertEquals("RelativeHumidity Flag: set flag null", UNCHECKED_FLAG,
				record.getRelativeHumidityFlag());
		record.setRelativeHumidity(null);
		assertEquals("RelativeHumidity Flag: nulled value", MISSING_FLAG,
				record.getRelativeHumidityFlag());
	}

	@Test (expected = InvalidFlagException.class)
	public void relativeHumidityFlagMissingFlag() throws CalculationWarning,
	InvalidFlagException, InvalidValueWarning {
		record.setRelativeHumidity(10.0);
		record.setRelativeHumidityFlag(MISSING_FLAG);
	}
	
	@Test (expected = InvalidFlagException.class)
	public void relativeHumidityFlagMissingValue() throws InvalidFlagException {
		record.setRelativeHumidityFlag(GOOD_FLAG);
	}

	@Test (expected = InvalidValueWarning.class)
	public void relativeHumidityTooBig() throws CalculationWarning,
	InvalidValueWarning {
		record.setRelativeHumidity(1000.0);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void relativeHumidityTooSmall() throws CalculationWarning,
	InvalidValueWarning {
		record.setRelativeHumidity(-100.0);
	}

	@Test public void temperatureMissingValue() throws CalculationWarning,
	ConversionException, InvalidValueWarning {
		assertNull("Temperature (miss): default", record.getTemperature());
		record.setTemperature(20.0, CELCIUS);
		assertEquals("Temperature (miss): value", 20.0, 
				record.getTemperature(), THRESHOLD);
		record.setTemperature(999.0, CELCIUS);
		assertNull("Temperature (miss): missing value", 
				record.getTemperature());
	}
	
	@Test public void temperatureFlag() throws CalculationWarning,
	ConversionException, InvalidFlagException, InvalidValueWarning {
		assertEquals("Temperature Flag: default missing", MISSING_FLAG,
				record.getTemperatureFlag());
		record.setTemperature(20.0, CELCIUS);
		assertEquals("Temperature Flag: default value", UNCHECKED_FLAG, 
				record.getTemperatureFlag());
		record.setTemperatureFlag(GOOD_FLAG);
		assertEquals("Temperature Flag: set flag", GOOD_FLAG,
				record.getTemperatureFlag());
		record.setTemperatureFlag(null);
		assertEquals("Temperature Flag: set flag null", UNCHECKED_FLAG,
				record.getTemperatureFlag());
		record.setTemperature(null, CELCIUS);
		assertEquals("Temperature Flag: nulled value", MISSING_FLAG, 
				record.getTemperatureFlag());
	}

	@Test (expected = InvalidFlagException.class)
	public void temperatureFlagMissingFlag() throws CalculationWarning,
	ConversionException, InvalidFlagException, InvalidValueWarning {
		record.setTemperature(20.0, CELCIUS);
		record.setTemperatureFlag(MISSING_FLAG);
	}
	
	@Test (expected = InvalidFlagException.class)
	public void temperatureFlagMissingValue() throws InvalidFlagException {
		record.setTemperatureFlag(GOOD_FLAG);
	}

	@Test (expected = InvalidValueWarning.class)
	public void temperatureTooBig() throws CalculationWarning,
	ConversionException, InvalidValueWarning {
		record.setTemperature(1000.0, CELCIUS);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void temperatureTooSmall() throws CalculationWarning,
	ConversionException, InvalidValueWarning {
		record.setTemperature(-100.0, CELCIUS);
	}

	@Test public void timeMissingValue() throws CalculationWarning,
	InvalidValueWarning {
		assertNull("Time (miss): default", record.getTime());
		record.setTime(20.0);
		assertEquals("Time (miss): value", 20.0, record.getTime(), THRESHOLD);
		record.setTime(9999.0);
		assertNull("Time (miss): missing value", record.getTime());
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void timeTooBig() throws CalculationWarning, InvalidValueWarning {
		record.setTime(10000.0);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void timeTooSmall() throws CalculationWarning, InvalidValueWarning {
		record.setTime(-1000.0);
	}

	@Test public void tostring() throws CalculationWarning, ConversionException,
	InvalidFlagException, InvalidValueException, InvalidValueWarning {
		assertEquals("Empty Record", "9999.0 9999.0 999.0 999.0 999.0 9999.0 9999.0 999.0 999.0 999.0 9999.000 999.000 999.0 999.0 99999.0  9.0  9.0  9.0  9.0  9.0  9.0", record.toString());
		
		record.setTime(0.0);
		record.setPressure(1000.0, MILLIBARS);
		record.setTemperature(15.0, CELCIUS);
		record.setDewPoint(12.0, CELCIUS);
		record.setRelativeHumidity(95.0);
		record.setUComponent(4.2, METERS_PER_SECOND);
		record.setVComponent(2.4, METERS_PER_SECOND);
		record.setWindSpeed(5.1, METERS_PER_SECOND);
		record.setWindDirection(234.0);
		record.setAscentRate(8.1, METERS_PER_SECOND);
		record.setLatitude(10.234);
		record.setLongitude(-123.456);
		record.setAltitude(14352.5, METERS);
		record.setVariableField1(93.3);
		record.setVariableField2(87.2);
		record.setPressureFlag(GOOD_FLAG);
		record.setTemperatureFlag(QUESTIONABLE_FLAG);
		record.setRelativeHumidityFlag(BAD_FLAG);
		record.setUComponentFlag(ESTIMATE_FLAG);
		record.setVComponentFlag(ESTIMATE_FLAG);
		record.setAscentRateFlag(UNCHECKED_FLAG);
		
		assertEquals("Full Record", "   0.0 1000.0  15.0  12.0  95.0    4.2    2.4   5.1 234.0   8.1 -123.456  10.234  93.3  87.2 14352.5  1.0  2.0  3.0  4.0  4.0 99.0", record.toString());
	}

	@Test public void uComponentMissingValue() throws CalculationWarning,
	ConversionException, InvalidValueWarning {
		assertNull("UComponent (miss): default", record.getUComponent());
		record.setUComponent(20.0, METERS_PER_SECOND);
		assertEquals("UComponent (miss): value", 20.0, record.getUComponent(),
				THRESHOLD);
		record.setUComponent(9999.0, METERS_PER_SECOND);
		assertNull("UComponent (miss): missing value", record.getUComponent());
	}
	
	@Test public void uComponentFlag() throws CalculationWarning,
	ConversionException, InvalidFlagException, InvalidValueWarning {
		assertEquals("UComponent Flag: default missing", MISSING_FLAG, 
				record.getUComponentFlag());
		record.setUComponent(20.0, METERS_PER_SECOND);
		assertEquals("UComponent Flag: default value", UNCHECKED_FLAG, 
				record.getUComponentFlag());
		record.setUComponentFlag(GOOD_FLAG);
		assertEquals("UComponent Flag: set flag", GOOD_FLAG, 
				record.getUComponentFlag());
		record.setUComponentFlag(null);
		assertEquals("UComponent Flag: set flag null", UNCHECKED_FLAG,
				record.getUComponentFlag());
		record.setUComponent(null, METERS_PER_SECOND);
		assertEquals("UComponent Flag: nulled value", MISSING_FLAG,
				record.getUComponentFlag());
	}

	@Test (expected = InvalidFlagException.class)
	public void uComponentFlagMissingFlag() throws CalculationWarning,
	ConversionException, InvalidFlagException, InvalidValueWarning {
		record.setUComponent(20.0, METERS_PER_SECOND);
		record.setUComponentFlag(MISSING_FLAG);
	}
	
	@Test (expected = InvalidFlagException.class)
	public void uComponentFlagMissingValue() throws InvalidFlagException {
		record.setUComponentFlag(GOOD_FLAG);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void uComponentTooBig() throws CalculationWarning,
	ConversionException, InvalidValueWarning {
		record.setUComponent(10000.0, METERS_PER_SECOND);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void uComponentTooSmall() throws CalculationWarning,
	ConversionException, InvalidValueWarning {
		record.setUComponent(-1000.0, METERS_PER_SECOND);
	}

	@Test public void variableField1() throws InvalidValueException {
		assertNull("Variable Field 1: default", record.getVariableField1());
		record.setVariableField1(10.0);
		assertEquals("Variable Field 1: value", 10.0, 
				record.getVariableField1(), THRESHOLD);
		record.setVariableField1(null);
		assertNull("Variable Field 1: null", record.getVariableField1());
	}
	
	@Test public void variableField1MissingValue() throws 
	InvalidValueException {
		assertNull("Var Field 1 (miss): default", record.getVariableField1());
		record.setVariableField1(20.0);
		assertEquals("Var Field 1 (miss): value", 20.0, 
				record.getVariableField1());
		record.setVariableField1(999.0);
		assertNull("Var Field 1 (miss): missing value", 
				record.getVariableField1());
	}
	
	@Test (expected = InvalidValueException.class)
	public void variableField1TooBig() throws InvalidValueException {
		record.setVariableField1(1000.0);
	}
	
	@Test (expected = InvalidValueException.class)
	public void variableField1TooSmall() throws InvalidValueException {
		record.setVariableField1(-100.0);
	}
	
	@Test public void variableField2() throws InvalidValueException {
		assertNull("Variable Field 2: default", record.getVariableField2());
		record.setVariableField2(10.0);
		assertEquals("Variable Field 2: value", 10.0, 
				record.getVariableField2(), THRESHOLD);
		record.setVariableField2(null);
		assertNull("Variable Field 2: null", record.getVariableField2());
	}
	
	@Test public void variableField2MissingValue() throws 
	InvalidValueException {
		assertNull("Var Field 2 (miss): default", record.getVariableField2());
		record.setVariableField2(20.0);
		assertEquals("Var Field 2 (miss): value", 20.0, 
				record.getVariableField2());
		record.setVariableField2(999.0);
		assertNull("Var Field 2 (miss): missing value", 
				record.getVariableField2());
	}
	
	@Test (expected = InvalidValueException.class)
	public void variableField2TooBig() throws InvalidValueException {
		record.setVariableField2(1000.0);
	}
	
	@Test (expected = InvalidValueException.class)
	public void variableField2TooSmall() throws InvalidValueException {
		record.setVariableField2(-100.0);
	}
	
	@Test public void vComponentMissingValue() throws CalculationWarning,
	ConversionException, InvalidValueWarning {
		assertNull("VComponent (miss): default", record.getVComponent());
		record.setVComponent(20.0, METERS_PER_SECOND);
		assertEquals("VComponent (miss): value", 20.0, record.getVComponent(), 
				THRESHOLD);
		record.setVComponent(9999.0, METERS_PER_SECOND);
		assertNull("VComponent (miss): missing value", record.getVComponent());
	}
	
	@Test public void vComponentFlag() throws CalculationWarning,
	ConversionException, InvalidFlagException, InvalidValueWarning {
		assertEquals("VComponent Flag: default missing", MISSING_FLAG,
				record.getVComponentFlag());
		record.setVComponent(20.0, METERS_PER_SECOND);
		assertEquals("VComponent Flag: default value", UNCHECKED_FLAG, 
				record.getVComponentFlag());
		record.setVComponentFlag(GOOD_FLAG);
		assertEquals("VComponent Flag: set flag", GOOD_FLAG,
				record.getVComponentFlag());
		record.setVComponentFlag(null);
		assertEquals("VComponent Flag: set flag null", UNCHECKED_FLAG,
				record.getVComponentFlag());
		record.setVComponent(null, METERS_PER_SECOND);
		assertEquals("VComponent Flag: nulled value", MISSING_FLAG,
				record.getVComponentFlag());
	}

	@Test (expected = InvalidFlagException.class)
	public void vComponentFlagMissingFlag() throws CalculationWarning,
	ConversionException, InvalidFlagException, InvalidValueWarning {
		record.setVComponent(20.0, METERS_PER_SECOND);
		record.setVComponentFlag(MISSING_FLAG);
	}
	
	@Test (expected = InvalidFlagException.class)
	public void vComponentFlagMissingValue() throws InvalidFlagException {
		record.setVComponentFlag(GOOD_FLAG);
	}

	@Test (expected = InvalidValueWarning.class)
	public void vComponentTooBig() throws CalculationWarning,
	ConversionException, InvalidValueWarning {
		record.setVComponent(10000.0, METERS_PER_SECOND);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void vComponentTooSmall() throws CalculationWarning,
	ConversionException, InvalidValueWarning {
		record.setVComponent(-1000.0, METERS_PER_SECOND);
	}

	@Test public void windDirectionMissingValue() throws CalculationWarning,
	ConversionException, InvalidValueWarning {
		assertNull("WindSpeed (miss): default", record.getWindSpeed());
		record.setWindSpeed(20.0, METERS_PER_SECOND);
		assertEquals("WindSpeed (miss): value", 20.0, record.getWindSpeed(), 
				THRESHOLD);
		record.setWindSpeed(999.0, METERS_PER_SECOND);
		assertNull("WindSpeed (miss): missing value", record.getWindSpeed());
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void windDirectionTooBig() throws CalculationWarning,
	InvalidValueWarning {
		record.setWindDirection(1000.0);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void windDirectionTooSmall() throws CalculationWarning,
	InvalidValueWarning {
		record.setWindDirection(-1000.0);
	}

	@Test public void windSpeedMissingValue() throws CalculationWarning,
	ConversionException, InvalidValueWarning {
		assertNull("WindSpeed (miss): default", record.getWindSpeed());
		record.setWindSpeed(20.0, METERS_PER_SECOND);
		assertEquals("WindSpeed (miss): value", 20.0, record.getWindSpeed(),
				THRESHOLD);
		record.setWindSpeed(999.0, METERS_PER_SECOND);
		assertNull("WindSpeed (miss): missing value", record.getWindSpeed());
	}

	@Test (expected = InvalidValueWarning.class)
	public void windSpeedTooBig() throws CalculationWarning,
	ConversionException, InvalidValueWarning {
		record.setWindSpeed(1000.0, METERS_PER_SECOND);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void windSpeedTooSmall() throws CalculationWarning,
	ConversionException, InvalidValueWarning {
		record.setWindSpeed(-100.0, METERS_PER_SECOND);
	}
	
	
	
	@Test public void stringConstructor() throws 
	NumberFormatException, IOException {

	    ESCSoundingRecord rec = new ESCSoundingRecord("   0.0 1021.6  13.0   6.1  63.0    4.1    0.0   4.1 180.0  10.2 -122.200  37.700  46.7 287.6     2.0  2.0  2.0  2.0  1.0  1.0 99.0", 0);
		assertEquals("String Constructor: time", 0.0, rec.getTime(), 
				THRESHOLD);
		assertEquals("String Constructor: press", 1021.6, rec.getPressure(),
				THRESHOLD);
		assertEquals("String Constructor: temp", 13.0, rec.getTemperature(), 
				THRESHOLD);
		assertEquals("String Constructor: dew pt", 6.1, rec.getDewPoint(), 
				THRESHOLD);
		assertEquals("String Constructor: rh", 63.0, rec.getRelativeHumidity(), 
				THRESHOLD);
		assertEquals("String Constructor: ucomp", 4.1, rec.getUComponent(), 
				THRESHOLD);
		assertEquals("String Constructor: vcomp", 0.0, rec.getVComponent(), 
				THRESHOLD);
		assertEquals("String Constructor: speed", 4.1, rec.getWindSpeed(), 
				THRESHOLD);
		assertEquals("String Constructor: dir", 180.0, rec.getWindDirection(),
				THRESHOLD);
		assertEquals("String Constructor: rate", 10.2, rec.getAscentRate(), 
				THRESHOLD);
		assertEquals("String Constructor: lon", -122.2, rec.getLongitude(), 
				THRESHOLD);
		assertEquals("String Constructor: lat", 37.7, rec.getLatitude(),
				THRESHOLD);
		assertEquals("String Constructor: var1", 46.7, rec.getVariableField1(),
				THRESHOLD);
		assertEquals("String Constructor: var2", 287.6, rec.getVariableField2(), 
				THRESHOLD);
		assertEquals("String Constructor: alt", 2.0, rec.getAltitude(),
				THRESHOLD);
		assertEquals("String Constructor: press flag", QUESTIONABLE_FLAG, 
				rec.getPressureFlag());
		assertEquals("String Constructor: temp flag", QUESTIONABLE_FLAG, 
				rec.getTemperatureFlag());
		assertEquals("String Constructor: rh flag", QUESTIONABLE_FLAG,
				rec.getRelativeHumidityFlag());
		assertEquals("String Constructor: u flag", GOOD_FLAG, 
				rec.getUComponentFlag());
		assertEquals("String Constructor: v flag", GOOD_FLAG, 
				rec.getVComponentFlag());
		assertEquals("String Constructor: rate flag", UNCHECKED_FLAG, 
				rec.getAscentRateFlag());
	}
	
	@Test (expected = IOException.class)
	public void stringConstructorBadTokenCount() throws 
	IOException, NumberFormatException {
	    new ESCSoundingRecord("9999.0 9999.0 999.0 999.0 999.0", 0);
	}
	
	@Test public void stringConstructorMissingValues() throws 
	NumberFormatException, IOException {
	    ESCSoundingRecord rec = new ESCSoundingRecord("9999.0 9999.0 999.0 999.0 999.0 9999.0 9999.0 999.0 999.0 999.0 9999.000 999.000 999.0 999.0 99999.0  9.0  9.0  9.0  9.0  9.0  9.0", 0);
		assertNull("String Constructor: miss time", rec.getTime());
		assertNull("String Constructor: miss press", rec.getPressure());
		assertNull("String Constructor: miss temp", rec.getTemperature());
		assertNull("String Constructor: miss dew pt", rec.getDewPoint());
		assertNull("String Constructor: miss rh", rec.getRelativeHumidity());
		assertNull("String Constructor: miss ucomp", rec.getUComponent());
		assertNull("String Constructor: miss vcomp", rec.getVComponent());
		assertNull("String Constructor: miss speed", rec.getWindSpeed());
		assertNull("String Constructor: miss dir", rec.getWindDirection());
		assertNull("String Constructor: miss rate", rec.getAscentRate());
		assertNull("String Constructor: miss lon", rec.getLongitude());
		assertNull("String Constructor: miss lat", rec.getLatitude());
		assertNull("String Constructor: miss var1", rec.getVariableField1());
		assertNull("String Constructor: miss var2", rec.getVariableField2());
		assertNull("String Constructor: miss alt", rec.getAltitude());
		assertEquals("String Constructor: miss press flag", MISSING_FLAG, 
				rec.getPressureFlag());
		assertEquals("String Constructor: miss temp flag", MISSING_FLAG, 
				rec.getTemperatureFlag());
		assertEquals("String Constructor: miss rh flag", MISSING_FLAG, 
				rec.getRelativeHumidityFlag());
		assertEquals("String Constructor: miss u flag", MISSING_FLAG, 
				rec.getUComponentFlag());
		assertEquals("String Constructor: miss v flag", MISSING_FLAG, 
				rec.getVComponentFlag());
		assertEquals("String Constructor: miss rate flag", MISSING_FLAG, 
				rec.getAscentRateFlag());
	}

	@Test (expected = NumberFormatException.class)
	public void stringConstructorAltitudeNotANumber() throws IOException, NumberFormatException {
	    new ESCSoundingRecord("9999.0 9999.0 999.0 999.0 999.0 9999.0 999.0 999.0 999.0 999.0 9999.000 999.000 999.0 999.0 9X999.0  9.0  9.0  9.0  9.0  9.0  9.0", 0);
	}
	
	@Test (expected = IOException.class)
	public void stringConstructorAltitudeOutOfRange() throws NumberFormatException, IOException {
	    new ESCSoundingRecord("99999.0 9999.0 999.0 999.0 999.0 9999.0 9999.0 999.0 999.0 999.0 9999.000 999.000 999.0 999.0 -99999.0  9.0  9.0  9.0  9.0  9.0  9.0", 0);
	}

	@Test (expected = NumberFormatException.class)
	public void stringConstructorAscentRateFlagNotANumber() throws IOException, NumberFormatException {
	    new ESCSoundingRecord("9999.0 9999.0 999.0 999.0 999.0 9999.0 9999.0 999.0 999.0 999.0 9999.000 999.000 999.0 999.0 99999.0  9.0  9.0  9.0  9.0  9.0  X.0", 0);
	}
	
	@Test (expected = IOException.class)
	public void stringConstructorAscentRateFlagOutOfRange() throws NumberFormatException, IOException {
	    new ESCSoundingRecord("99999.0 9999.0 999.0 999.0 999.0 9999.0 9999.0 999.0 999.0 999.0 9999.000 999.000 999.0 999.0 99999.0  9.0  9.0  9.0  9.0  9.0  8.0", 0);
	}

	@Test (expected = NumberFormatException.class)
	public void stringConstructorAscentRateNotANumber() throws IOException, NumberFormatException {
	    new ESCSoundingRecord("9999.0 9999.0 999.0 999.0 999.0 9999.0 999.0 999.0 999.0 9X9.0 9999.000 999.000 999.0 999.0 99999.0  9.0  9.0  9.0  9.0  9.0  9.0", 0);
	}
	
	@Test (expected = IOException.class)
	public void stringConstructorAscentRateOutOfRange() throws NumberFormatException, IOException {
	    new ESCSoundingRecord("99999.0 9999.0 999.0 999.0 999.0 9999.0 9999.0 999.0 999.0 -999.0 9999.000 999.000 999.0 999.0 99999.0  9.0  9.0  9.0  9.0  9.0  9.0", 0);
	}

	@Test (expected = NumberFormatException.class)
	public void stringConstructorDewPointNotANumber() throws IOException, NumberFormatException {
	    new ESCSoundingRecord("9999.0 9999.0 999.0 9X9.0 999.0 9999.0 9999.0 999.0 999.0 999.0 9999.000 999.000 999.0 999.0 99999.0  9.0  9.0  9.0  9.0  9.0  9.0", 0);
	}
	
	@Test (expected = IOException.class)
	public void stringConstructorDewPointOutOfRange() throws NumberFormatException, IOException {
	    new ESCSoundingRecord("99999.0 9999.0 999.0 -999.0 999.0 9999.0 9999.0 999.0 999.0 999.0 9999.000 999.000 999.0 999.0 99999.0  9.0  9.0  9.0  9.0  9.0  9.0", 0);
	}

	@Test (expected = NumberFormatException.class)
	public void stringConstructorLatitudeNotANumber() throws IOException, NumberFormatException {
	    new ESCSoundingRecord("9999.0 9999.0 999.0 999.0 999.0 9999.0 999.0 999.0 999.0 999.0 9999.000 9X9.000 999.0 999.0 99999.0  9.0  9.0  9.0  9.0  9.0  9.0", 0);
	}
	
	@Test (expected = IOException.class)
	public void stringConstructorLatitudeOutOfRange() throws NumberFormatException, IOException {
	    new ESCSoundingRecord("99999.0 9999.0 999.0 999.0 999.0 9999.0 9999.0 999.0 999.0 999.0 9999.000 -99.000 999.0 999.0 99999.0  9.0  9.0  9.0  9.0  9.0  9.0", 0);
	}

	@Test (expected = NumberFormatException.class)
	public void stringConstructorLongitudeNotANumber() throws IOException, NumberFormatException {
	    new ESCSoundingRecord("9999.0 9999.0 999.0 999.0 999.0 9999.0 999.0 999.0 999.0 999.0 9X99.000 999.000 999.0 999.0 99999.0  9.0  9.0  9.0  9.0  9.0  9.0", 0);
	}
	
	@Test (expected = IOException.class)
	public void stringConstructorLongitudeOutOfRange() throws NumberFormatException, IOException {
	    new ESCSoundingRecord("99999.0 9999.0 999.0 999.0 999.0 9999.0 9999.0 999.0 999.0 999.0 -199.000 999.000 999.0 999.0 99999.0  9.0  9.0  9.0  9.0  9.0  9.0", 0);
	}

	@Test (expected = NumberFormatException.class)
	public void stringConstructorPressureFlagNotANumber() throws IOException, NumberFormatException {
	    new ESCSoundingRecord("9999.0 9999.0 999.0 999.0 999.0 9999.0 999.0 999.0 999.0 999.0 9999.000 999.000 999.0 999.0 99999.0  X9.0  9.0  9.0  9.0  9.0  9.0", 0);
	}
	
	@Test (expected = IOException.class)
	public void stringConstructorPressureFlagOutOfRange() throws NumberFormatException, IOException {
	    new ESCSoundingRecord("99999.0 9999.0 999.0 999.0 999.0 9999.0 9999.0 999.0 999.0 999.0 9999.000 999.000 999.0 999.0 99999.0  8.0  9.0  9.0  9.0  9.0  9.0", 0);
	}

	@Test (expected = NumberFormatException.class)
	public void stringConstructorPressureNotANumber() throws IOException, NumberFormatException {
	    new ESCSoundingRecord("9999.0 9.99.0 999.0 999.0 999.0 9999.0 9999.0 999.0 999.0 999.0 9999.000 999.000 999.0 999.0 99999.0  9.0  9.0  9.0  9.0  9.0  9.0", 0);
	}
	
	@Test (expected = IOException.class)
	public void stringConstructorPressureOutOfRange() throws NumberFormatException, IOException {
	    new ESCSoundingRecord("99999.0 -9999.0 999.0 999.0 999.0 9999.0 9999.0 999.0 999.0 999.0 9999.000 999.000 999.0 999.0 99999.0  9.0  9.0  9.0  9.0  9.0  9.0", 0);
	}

	@Test (expected = NumberFormatException.class)
	public void stringConstructorRelativeHumidityFlagNotANumber() throws IOException, NumberFormatException {
	    new ESCSoundingRecord("9999.0 9999.0 999.0 999.0 999.0 9999.0 999.0 999.0 999.0 999.0 9999.000 999.000 999.0 999.0 99999.0  9.0  9.0  X.0  9.0  9.0  9.0", 0);
	}
	
	@Test (expected = IOException.class)
	public void stringConstructorRelativeHumidityFlagOutOfRange() throws NumberFormatException, IOException {
	    new ESCSoundingRecord("99999.0 9999.0 999.0 999.0 999.0 9999.0 9999.0 999.0 999.0 999.0 9999.000 999.000 999.0 999.0 99999.0  9.0  9.0  8.0  9.0  9.0  9.0", 0);
	}

	@Test (expected = NumberFormatException.class)
	public void stringConstructorRelativeHumidityeNotANumber() throws IOException, NumberFormatException {
	    new ESCSoundingRecord("9999.0 9999.0 999.0 999.0 9X9.0 9999.0 9999.0 999.0 999.0 999.0 9999.000 999.000 999.0 999.0 99999.0  9.0  9.0  9.0  9.0  9.0  9.0", 0);
	}
	
	@Test (expected = IOException.class)
	public void stringConstructorRelativeHumidityOutOfRange() throws NumberFormatException, IOException {
	    new ESCSoundingRecord("99999.0 9999.0 999.0 999.0 -999.0 9999.0 9999.0 999.0 999.0 999.0 9999.000 999.000 999.0 999.0 99999.0  9.0  9.0  9.0  9.0  9.0  9.0", 0);
	}

	@Test (expected = NumberFormatException.class)
	public void stringConstructorTemperatureFlagNotANumber() throws IOException, NumberFormatException {
	    new ESCSoundingRecord("9999.0 9999.0 999.0 999.0 999.0 9999.0 999.0 999.0 999.0 999.0 9999.000 999.000 999.0 999.0 99999.0  9.0  X.0  9.0  9.0  9.0  9.0", 0);
	}
	
	@Test (expected = IOException.class)
	public void stringConstructorTemperatureFlagOutOfRange() throws NumberFormatException, IOException {
	    new ESCSoundingRecord("99999.0 9999.0 999.0 999.0 999.0 9999.0 9999.0 999.0 999.0 999.0 9999.000 999.000 999.0 999.0 99999.0  9.0  8.0  9.0  9.0  9.0  9.0", 0);
	}

	@Test (expected = NumberFormatException.class)
	public void stringConstructorTemperatureNotANumber() throws IOException, NumberFormatException {
	    new ESCSoundingRecord("9999.0 9999.0 9X9.0 999.0 999.0 9999.0 9999.0 999.0 999.0 999.0 9999.000 999.000 999.0 999.0 99999.0  9.0  9.0  9.0  9.0  9.0  9.0", 0);
	}
	
	@Test (expected = IOException.class)
	public void stringConstructorTemperatureOutOfRange() throws NumberFormatException, IOException {
	    new ESCSoundingRecord("99999.0 9999.0 -999.0 999.0 999.0 9999.0 9999.0 999.0 999.0 999.0 9999.000 999.000 999.0 999.0 99999.0  9.0  9.0  9.0  9.0  9.0  9.0", 0);
	}

	@Test (expected = NumberFormatException.class)
	public void stringConstructorTimeNotANumber() throws IOException, NumberFormatException {
	    new ESCSoundingRecord("9X.9.0 9999.0 999.0 999.0 999.0 9999.0 9999.0 999.0 999.0 999.0 9999.000 999.000 999.0 999.0 99999.0  9.0  9.0  9.0  9.0  9.0  9.0", 0);
	}
	
	@Test (expected = IOException.class)
	public void stringConstructorTimeOutOfRange() throws NumberFormatException, IOException {
	    new ESCSoundingRecord("99999.0 9999.0 999.0 999.0 999.0 9999.0 9999.0 999.0 999.0 999.0 9999.000 999.000 999.0 999.0 99999.0  9.0  9.0  9.0  9.0  9.0  9.0", 0);
	}

	@Test (expected = NumberFormatException.class)
	public void stringConstructorUComponentFlagNotANumber() throws IOException, NumberFormatException {
	    new ESCSoundingRecord("9999.0 9999.0 999.0 999.0 999.0 9999.0 999.0 999.0 999.0 999.0 9999.000 999.000 999.0 999.0 99999.0  9.0  9.0  9.0  X.0  9.0  9.0", 0);
	}
	
	@Test (expected = IOException.class)
	public void stringConstructorUComponentFlagOutOfRange() throws NumberFormatException, IOException {
	    new ESCSoundingRecord("99999.0 9999.0 999.0 999.0 999.0 9999.0 9999.0 999.0 999.0 999.0 9999.000 999.000 999.0 999.0 99999.0  9.0  9.0  9.0  8.0  9.0  9.0", 0);
	}

	@Test (expected = NumberFormatException.class)
	public void stringConstructorUComponentNotANumber() throws IOException, NumberFormatException {
	    new ESCSoundingRecord("9999.0 9999.0 999.0 999.0 999.0 9X99.0 9999.0 999.0 999.0 999.0 9999.000 999.000 999.0 999.0 99999.0  9.0  9.0  9.0  9.0  9.0  9.0", 0);
	}
	
	@Test (expected = IOException.class)
	public void stringConstructorUComponentOutOfRange() throws NumberFormatException, IOException {
	    new ESCSoundingRecord("99999.0 9999.0 999.0 999.0 999.0 -9999.0 9999.0 999.0 999.0 999.0 9999.000 999.000 999.0 999.0 99999.0  9.0  9.0  9.0  9.0  9.0  9.0", 0);
	}

	@Test (expected = NumberFormatException.class)
	public void stringConstructorVComponentFlagNotANumber() throws IOException, NumberFormatException {
	    new ESCSoundingRecord("9999.0 9999.0 999.0 999.0 999.0 9999.0 999.0 999.0 999.0 999.0 9999.000 999.000 999.0 999.0 99999.0  9.0  9.0  9.0  9.0  X.0  9.0", 0);
	}
	
	@Test (expected = IOException.class)
	public void stringConstructorVComponentFlagOutOfRange() throws NumberFormatException, IOException {
	    new ESCSoundingRecord("99999.0 9999.0 999.0 999.0 999.0 9999.0 9999.0 999.0 999.0 999.0 9999.000 999.000 999.0 999.0 99999.0  9.0  9.0  9.0  9.0  8.0  9.0", 0);
	}

	@Test (expected = NumberFormatException.class)
	public void stringConstructorVComponentNotANumber() throws IOException, NumberFormatException {
	    new ESCSoundingRecord("9999.0 9999.0 999.0 999.0 999.0 9999.0 9X99.0 999.0 999.0 999.0 9999.000 999.000 999.0 999.0 99999.0  9.0  9.0  9.0  9.0  9.0  9.0", 0);
	}
	
	@Test (expected = IOException.class)
	public void stringConstructorvComponentOutOfRange() throws NumberFormatException, IOException {
	    new ESCSoundingRecord("99999.0 9999.0 999.0 999.0 999.0 9999.0 -9999.0 999.0 999.0 999.0 9999.000 999.000 999.0 999.0 99999.0  9.0  9.0  9.0  9.0  9.0  9.0", 0);
	}

	@Test (expected = NumberFormatException.class)
	public void stringConstructorVariable1NotANumber() throws IOException, NumberFormatException {
	    new ESCSoundingRecord("9999.0 9999.0 999.0 999.0 999.0 9999.0 999.0 999.0 999.0 999.0 9999.000 999.000 X99.0 999.0 99999.0  9.0  9.0  9.0  9.0  9.0  9.0", 0);
	}
	
	@Test (expected = IOException.class)
	public void stringConstructorVariable1OutOfRange() throws NumberFormatException, IOException {
	    new ESCSoundingRecord("99999.0 9999.0 999.0 999.0 999.0 9999.0 9999.0 999.0 999.0 999.0 9999.000 999.000 -999.0 999.0 99999.0  9.0  9.0  9.0  9.0  9.0  9.0", 0);
	}

	@Test (expected = NumberFormatException.class)
	public void stringConstructorVariable2NotANumber() throws IOException, NumberFormatException {
	    new ESCSoundingRecord("9999.0 9999.0 999.0 999.0 999.0 9999.0 999.0 999.0 999.0 999.0 9999.000 999.000 999.0 9X9.0 99999.0  9.0  9.0  9.0  9.0  9.0  9.0", 0);
	}
	
	@Test (expected = IOException.class)
	public void stringConstructorVariable2OutOfRange() throws NumberFormatException, IOException {
	    new ESCSoundingRecord("99999.0 9999.0 999.0 999.0 999.0 9999.0 9999.0 999.0 999.0 999.0 9999.000 999.000 999.0 -999.0 99999.0  9.0  9.0  9.0  9.0  9.0  9.0", 0);
	}

	@Test (expected = NumberFormatException.class)
	public void stringConstructorWindDirectionNotANumber() throws IOException, NumberFormatException {
	    new ESCSoundingRecord("9999.0 9999.0 999.0 999.0 999.0 9999.0 999.0 999.0 99X.0 999.0 9999.000 999.000 999.0 999.0 99999.0  9.0  9.0  9.0  9.0  9.0  9.0", 0);
	}
	
	@Test (expected = IOException.class)
	public void stringConstructorWindDirectionOutOfRange() throws NumberFormatException, IOException {
	    new ESCSoundingRecord("99999.0 9999.0 999.0 999.0 999.0 9999.0 9999.0 999.0 -999.0 999.0 9999.000 999.000 999.0 999.0 99999.0  9.0  9.0  9.0  9.0  9.0  9.0", 0);
	}

	@Test (expected = NumberFormatException.class)
	public void stringConstructorWindSpeedNotANumber() throws IOException, NumberFormatException {
	    new ESCSoundingRecord("9999.0 9999.0 999.0 999.0 999.0 9999.0 999.0 X99.0 999.0 999.0 9999.000 999.000 999.0 999.0 99999.0  9.0  9.0  9.0  9.0  9.0  9.0", 0);
	}
	
	@Test (expected = IOException.class)
	public void stringConstructorWindSpeedOutOfRange() throws NumberFormatException, IOException {
	    new ESCSoundingRecord("99999.0 9999.0 999.0 999.0 999.0 9999.0 9999.0 -999.0 999.0 999.0 9999.000 999.000 999.0 999.0 99999.0  9.0  9.0  9.0  9.0  9.0  9.0", 0);
	}

}
