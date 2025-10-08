package dmg.ua.sounding;

import static dmg.util.LengthUtils.*;
import static dmg.util.PositionUtils.*;
import static dmg.util.PressureUtils.*;
import static dmg.util.TemperatureUtils.*;
import static dmg.util.VelocityUtils.*;
import static org.junit.Assert.*;
import dmg.util.*;
import dmg.util.PositionUtils.*;

import org.junit.*;

public abstract class SoundingRecordTest<T extends SoundingRecord> {
	
	protected static final Double THRESHOLD = Math.pow(1, -10);
	
	protected T previousRecord, record;

    //@SuppressWarnings("unchecked")
    @Before	public void setUp() throws Exception {
	previousRecord = newInstance(null);
	previousRecord.setTime(0.0);
	previousRecord.setAltitude(1000.0, METERS);
	record = newInstance(previousRecord);
    }
    
    public abstract T newInstance(T previousRecord);
    
    @Test public void altitude() throws CalculationWarning, ConversionException,
					InvalidValueWarning {
		assertNull("Altitude: default", record.getAltitude());
		record.setAltitude(10.0, METERS);
		assertEquals("Altitude: set m", 10.0, record.getAltitude(), THRESHOLD);
		record.setAltitude(1.0, KILOMETERS);
		assertEquals("Altitude: set diff unit", 1000.0, record.getAltitude(),
				THRESHOLD);
		record.setAltitude(0.0, METERS);
		assertEquals("Altitude: zero", 0.0, record.getAltitude(), THRESHOLD);
		record.setAltitude(-100.0, METERS);
		assertEquals("Altitude: negative", -100.0, record.getAltitude(),
				THRESHOLD);
		record.setAltitude(null, METERS);
		assertNull("Altitude: null", record.getAltitude());
	}
	
	@Test (expected = ConversionException.class)
	public void altitudeNullUnit() throws CalculationWarning,
	ConversionException, InvalidValueWarning {
		record.setAltitude(10.0, null);
	}
	
	@Test public void ascentRate() throws CalculationWarning,
	ConversionException, InvalidValueWarning {
		assertNull("Ascent Rate: default", record.getAscentRate());
		record.setAscentRate(10.0, METERS_PER_SECOND);
		assertEquals("Ascent Rate: positive", 10.0, record.getAscentRate(), 
				THRESHOLD);
		record.setAscentRate(0.0, METERS_PER_SECOND);
		assertEquals("Ascent Rate: zero", 0.0, record.getAscentRate(), 
				THRESHOLD);
		record.setAscentRate(-10.0, METERS_PER_SECOND);
		assertEquals("Ascent Rate: negative", -10.0, record.getAscentRate(), 
				THRESHOLD);
		record.setAscentRate(null, METERS_PER_SECOND);
		assertNull("Ascent Rate: null", record.getAscentRate());
	}
	
	@Test public void ascentRateCalculation() throws CalculationWarning,
	ConversionException, InvalidValueWarning  {
		assertNull("Ascent Rate: sanity check", record.getAscentRate());
		
		record.setTime(5.0);
		record.setAltitude(1100.0, METERS);
		assertEquals("Ascent Rate: calculated", 20.0, record.getAscentRate(), 
				THRESHOLD);
		record.setAscentRate(10.0, METERS_PER_SECOND);
		assertEquals("Ascent Rate: manual override", 10.0, 
				record.getAscentRate(), THRESHOLD);
		record.setTime(4.0);
		assertEquals("Ascent Rate: time change while in manual override", 10.0,
				record.getAscentRate(), THRESHOLD);
		record.setAltitude(1200.0, METERS);
		assertEquals("Ascent Rate: altitude change while in manual override",
				10.0, record.getAscentRate(), THRESHOLD);
		previousRecord.setTime(-1.0);
		assertEquals("Ascent Rate: previous time change in manual override",
				10.0, record.getAscentRate(), THRESHOLD);
		previousRecord.setAltitude(1100.0, METERS);
		assertEquals("Ascent Rate: previous altitude change in manual override",
				10.0, record.getAscentRate(), THRESHOLD);
		record.setAscentRate(null, METERS_PER_SECOND);
		assertEquals("Ascent Rate: reallow calculation", 20.0, 
				record.getAscentRate(), THRESHOLD);
	}
	
	@Test public void ascentRateCalculationCurrentAltitude() throws
	CalculationWarning, ConversionException, InvalidValueWarning {
		record.setTime(5.0);
		assertNull("Ascent Rate: alt - sanity", record.getAscentRate());
		
		record.setAltitude(1100.0, METERS);
		assertEquals("Ascent Rate: alt - calc", 20.0, record.getAscentRate(),
				THRESHOLD);
		record.setAltitude(1200.0, METERS);
		assertEquals("Ascent Rate: alt - changed", 40.0, record.getAscentRate(),
				THRESHOLD);
		record.setAltitude(1200.0, METERS);
		assertEquals("Ascent Rate: alt - changed same", 40.0, 
				record.getAscentRate(), THRESHOLD);
		record.setAltitude(null, METERS);
		assertNull("Ascent Rate: alt - changed null", record.getAscentRate());
	}
	
	@Test public void ascentRateCalculationCurrentTime() throws
	CalculationWarning, ConversionException, InvalidValueWarning {
		record.setAltitude(1100.0, METERS);
		assertNull("Ascent Rate: time - sanity", record.getAscentRate());
		
		record.setTime(5.0);
		assertEquals("Ascent Rate: time - calc", 20.0, record.getAscentRate(), 
				THRESHOLD);
		record.setTime(2.5);
		assertEquals("Ascent Rate: time - changed", 40.0, 
				record.getAscentRate(), THRESHOLD);
		record.setTime(2.5);
		assertEquals("Ascent Rate: time - changed same", 40.0, 
				record.getAscentRate(), THRESHOLD);
		record.setTime(null);
		assertNull("Ascent Rate: time - changed null", 
				record.getAscentRate());
	}
	
	@Test public void ascentRateCalculationPreviousAltitude() throws
	CalculationWarning, ConversionException, InvalidValueWarning {
		record.setTime(5.0);
		record.setAltitude(1100.0, METERS);
		previousRecord.setAltitude(null, METERS);
		assertNull("Ascent Rate: prev alt - sanity", record.getAscentRate());
		
		previousRecord.setAltitude(1000.0, METERS);
		assertEquals("Ascent Rate: prev alt - calc", 20.0,
				record.getAscentRate(), THRESHOLD);
		previousRecord.setAltitude(900.0, METERS);
		assertEquals("Ascent Rate: prev alt - changed", 40.0, 
				record.getAscentRate(), THRESHOLD);
		previousRecord.setAltitude(900.0, METERS);
		assertEquals("Ascent Rate: prev alt - changed same", 40.0, 
				record.getAscentRate(), THRESHOLD);
		previousRecord.setAltitude(null, METERS);
		assertNull("Ascent Rate: prev alt - changed null",
				record.getAscentRate());
	}
	
	@Test public void ascentRateCalculationPreviousTime() throws
	CalculationWarning, ConversionException, InvalidValueWarning {
		record.setAltitude(1100.0, METERS);
		record.setTime(5.0);
		previousRecord.setTime(null);
		assertNull("Ascent Rate: prev time - sanity", record.getAscentRate());
		
		previousRecord.setTime(0.0);
		assertEquals("Ascent Rate: prev time - calc", 20.0, 
				record.getAscentRate(), THRESHOLD);
		previousRecord.setTime(2.5);
		assertEquals("Ascent Rate: prev time - changed", 40.0,
				record.getAscentRate(), THRESHOLD);
		previousRecord.setTime(2.5);
		assertEquals("Ascent Rate: prev time - changed same", 40.0,
				record.getAscentRate(), THRESHOLD);
		previousRecord.setTime(null);
		assertNull("Ascent Rate: prev time - changed null",
				record.getAscentRate());
	}
	
	@Test (expected = ConversionException.class)
	public void ascentRateNullUnit() throws CalculationWarning,
	ConversionException, InvalidValueWarning {
		record.setAscentRate(10.0, null);
	}
	
	@Test public void dewPoint() throws CalculationWarning,
	ConversionException, InvalidValueWarning {
		assertNull("Dew Point: default", record.getDewPoint());
		
		record.setDewPoint(12.0, CELCIUS);
		assertEquals("Dew Point: set value", 12.0, record.getDewPoint(), 
				THRESHOLD);
		record.setDewPoint(32.0, FAHRENHEIT);
		assertEquals("Dew Point: set diff unit", 0.0, record.getDewPoint(), 
				THRESHOLD);
		record.setDewPoint(0.0, CELCIUS);
		assertEquals("Dew Point: zero", 0.0, record.getDewPoint(), THRESHOLD);
		record.setDewPoint(-10.0, CELCIUS);
		assertEquals("Dew Point: negative", -10.0, record.getDewPoint(), 
				THRESHOLD);
		record.setDewPoint(null, CELCIUS);
		assertNull("Dew Point: set null", record.getDewPoint());
	}
	
	@Test public void dewPointCalculation() throws CalculationWarning,
	ConversionException, InvalidValueWarning {
		assertNull("DP Calc: sanity check", record.getDewPoint());
		
		record.setTemperature(15.0, CELCIUS);
		record.setRelativeHumidity(100.0);
		assertEquals("DP Calc: calculated", 15.0, record.getDewPoint(), 
				THRESHOLD);
		record.setDewPoint(11.11, CELCIUS);
		assertEquals("DP Calc: manual override", 11.11, record.getDewPoint(), 
				THRESHOLD);
		record.setTemperature(10.0, CELCIUS);
		assertEquals("DP Calc: temp change in override", 11.11, 
				record.getDewPoint(), THRESHOLD);
		record.setRelativeHumidity(50.0);
		assertEquals("DP Calc: rh change in override", 11.11, 
				record.getDewPoint(), THRESHOLD);
		record.setDewPoint(null, CELCIUS);
		assertEquals("DP Calc: dew point set to null", 0.05367608149, 
				record.getDewPoint(), THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void dewPointCalculationInvalidTemperature() throws
	CalculationWarning, ConversionException, InvalidValueWarning {
		record.setRelativeHumidity(10.0);
		record.setTemperature(-243.5, CELCIUS);
	}
	
	@Test (expected = CalculationWarning.class)
	public void dewPointCalculationNonPositiveRH() throws CalculationWarning,
	ConversionException, InvalidValueWarning {
		record.setTemperature(10.0, CELCIUS);
		record.setRelativeHumidity(0.0);
	}
	
	@Test public void dewPointCalculationRelativeHumidity() throws
	CalculationWarning, ConversionException, InvalidValueWarning {
		record.setTemperature(10.0, CELCIUS);
		assertEquals("DP Calc: temp set sanity check", 10.0, 
				record.getTemperature(), THRESHOLD);
		assertNull("DP Calc: temp only set", record.getDewPoint());
		
		record.setRelativeHumidity(100.0);
		assertEquals("DP Calc: rh now set", 10.0, record.getDewPoint(), 
				THRESHOLD);
		record.setRelativeHumidity(50.0);
		assertEquals("DP Calc: change rh", 0.05367608149, record.getDewPoint(),
				THRESHOLD);
		record.setRelativeHumidity(50.0);
		assertEquals("DP Calc: change rh same", 0.05367608149, 
				record.getDewPoint(), THRESHOLD);

		// Make sure a bad rh causes the calculated dew point to be null.
		try { record.setRelativeHumidity(0.0); }
		catch (CalculationWarning e) {
			assertNull("DP Calc: bad rh value", record.getDewPoint());
			record.setRelativeHumidity(50.0);
		}
		
		record.setRelativeHumidity(null);
		assertNull("DP Calc: change rh null", record.getDewPoint());
	}
	
	@Test public void dewPointCalculationTemperature() throws
	CalculationWarning, ConversionException, InvalidValueWarning {
		record.setRelativeHumidity(100.0);
		assertEquals("DP Calc: rh set sanity check", 100.0, 
				record.getRelativeHumidity(), THRESHOLD);
		assertNull("DP Calc: dew point - rh only set", 
				record.getDewPoint());
		
		record.setTemperature(10.0, CELCIUS);
		assertEquals("DP Calc: dew point - temp now set", 10.0,
				record.getDewPoint(), THRESHOLD);
		record.setTemperature(0.0, CELCIUS);
		assertEquals("DP Calc: dew point - change temp", 0.0, 
				record.getDewPoint(), THRESHOLD);
		record.setTemperature(32.0, FAHRENHEIT);
		assertEquals("DP Calc: dew point - change temp same", 0.0,
				record.getDewPoint(), THRESHOLD);
		
		// Make sure a bad temp value causes the calculated dew point to be null
		try { record.setTemperature(-243.5, CELCIUS); }
		catch (InvalidValueWarning e) {
			assertNull("DP Calc: bad temp change", record.getDewPoint());
			record.setTemperature(0.0, CELCIUS);
		}		
		
		record.setTemperature(null, CELCIUS);
		assertNull("DP Calc: dew point - change temp null", 
				record.getDewPoint());
	}
	
	@Test (expected = ConversionException.class)
	public void dewPointNullUnit() throws CalculationWarning,
	ConversionException, InvalidValueWarning {
		record.setDewPoint(0.0, null);
	}
	
	@Test public void latitudeDefaultValue() {
		assertNull("Latitude: default value", record.getLatitude());
	}

	@Test public void latitudeDegreeDirectionLowerBoundary() throws
	ConversionException, InvalidValueWarning {
		record.setLatitude(-90.0, NORTH);
		assertEquals("Latitude: -90 N", -90.0, record.getLatitude(), THRESHOLD);
		record.setLatitude(90.0, SOUTH);
		assertEquals("Latitude:  90 S", -90.0, record.getLatitude(), THRESHOLD);
	}

	@Test (expected = InvalidValueWarning.class)
	public void latitudeDegreeDirectionLowerBoundaryViolationNorth() throws
	ConversionException, InvalidValueWarning {
		record.setLatitude(-90.1, NORTH);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void latitudeDegreeDirectionLowerBoundaryViolationSouth() throws
	ConversionException, InvalidValueWarning {
		record.setLatitude(90.1, SOUTH);
	}
	
	@Test public void latitudeDegreeDirectionNullDegrees() throws
	ConversionException, InvalidValueWarning {
		record.setLatitude(null, NORTH);
		assertNull("Latitude: null N", record.getLatitude());
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDegreeDirectionNullDirection() throws
	ConversionException, InvalidValueWarning {
		record.setLatitude(30.0, (LatitudeUnit)null);
	}
	
	@Test public void latitudeDegreeDirectionUpperBoundary() throws
	ConversionException, InvalidValueWarning {
		record.setLatitude(90.0, NORTH);
		assertEquals("Latitude:  90 N",  90.0, record.getLatitude(), THRESHOLD);
		record.setLatitude(-90.0, SOUTH);
		assertEquals("Latitude: -90 S",  90.0, record.getLatitude(), THRESHOLD);
	}

	@Test (expected = InvalidValueWarning.class)
	public void latitudeDegreeDirectionUpperBoundaryViolationNorth() throws
	ConversionException, InvalidValueWarning {
		record.setLatitude(90.1, NORTH);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void latitudeDegreeDirectionUpperBoundaryViolationSouth() throws
	ConversionException, InvalidValueWarning {
		record.setLatitude(-90.1, SOUTH);
	}
	
	@Test public void latitudeDegreeLowerBoundary() throws InvalidValueWarning {
		record.setLatitude(-90.0);
		assertEquals("Latitude: -90", -90.0, record.getLatitude(), THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void latitudeDegreeLowerBoundaryViolation() throws
	InvalidValueWarning {
		record.setLatitude(-90.1);
	}
	
	@Test public void latitudeDegreeNullDegrees() throws InvalidValueWarning {
		record.setLatitude(null);
		assertNull("Latitude: null", record.getLatitude());
	}
	
	@Test public void latitudeDegreeUpperBoundary() throws InvalidValueWarning {
		record.setLatitude(90.0);
		assertEquals("Latitude:  90",  90.0, record.getLatitude(), THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void latitudeDegreeUpperBoundaryViolation() throws
	InvalidValueWarning {
		record.setLatitude(90.1);
	}
	
	@Test public void latitudeDMDirectionLowerBoundary() throws
	ConversionException, InvalidValueWarning {
		record.setLatitude(-90.0, 0.0, NORTH);
		assertEquals("Latitude: -90 00 N", -90.0, record.getLatitude(), 
				THRESHOLD);
		record.setLatitude(90.0, 0.0, SOUTH);
		assertEquals("Latitude:  90 00 S", -90.0, record.getLatitude(),
				THRESHOLD);
	}

	@Test (expected = InvalidValueWarning.class)
	public void latitudeDMDirectionLowerBoundaryViolationNorth() throws
	ConversionException, InvalidValueWarning {
		record.setLatitude(-90.0, 0.1, NORTH);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void latitudeDMDirectionLowerBoundaryViolationSouth() throws
	ConversionException, InvalidValueWarning {
		record.setLatitude(90.0, 0.1, SOUTH);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMDirectionMinuteLowerBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		record.setLatitude(30.0, -0.1, NORTH);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMDirectionMinuteUpperBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		record.setLatitude(30.0, 60.1, NORTH);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMDirectionNullDegrees() throws
	ConversionException, InvalidValueWarning {
		record.setLatitude(null, 30.0, NORTH);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMDirectionNullDirection() throws ConversionException,
	InvalidValueWarning {
		record.setLatitude(30.0, 30.0, (LatitudeUnit)null);
	}
		
	@Test (expected = ConversionException.class)
	public void latitudeDMDirectionNullMinutes() throws ConversionException,
	InvalidValueWarning {
		record.setLatitude(30.0, null, NORTH);
	}
	
	@Test public void latitudeDMDirectionUpperBoundary() throws
	ConversionException, InvalidValueWarning {
		record.setLatitude(90.0, 0.0, NORTH);
		assertEquals("Latitude:  90 00 N",  90.0, record.getLatitude(),
				THRESHOLD);
		record.setLatitude(-90.0, 0.0, SOUTH);
		assertEquals("Latitude: -90 00 S",  90.0, record.getLatitude(),
				THRESHOLD);
	}

	@Test (expected = InvalidValueWarning.class)
	public void latitudeDMDirectionUpperBoundaryViolationNorth() throws
	ConversionException, InvalidValueWarning {
		record.setLatitude(90.0, 0.1, NORTH);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void latitudeDMDirectionUpperBoundaryViolationSouth() throws
	ConversionException, InvalidValueWarning {
		record.setLatitude(-90.0, 0.1, SOUTH);
	}
	
	@Test public void latitudeDMLowerBoundary() throws ConversionException,
	InvalidValueWarning {
		record.setLatitude(-90.0, 0.0);
		assertEquals("Latitude: -90 00", -90.0, record.getLatitude(), 
				THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void latitudeDMLowerBoundaryViolation() throws ConversionException,
	InvalidValueWarning {
		record.setLatitude(-90.0, 0.1);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMMinuteLowerBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		record.setLatitude(30.0, -0.1);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMMinuteUpperBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		record.setLatitude(30.0, 60.1);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMNullDegrees() throws ConversionException,
	InvalidValueWarning {
		record.setLatitude(null, 30.0);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMNullMinutes() throws ConversionException,
	InvalidValueWarning {
		record.setLatitude(30.0, (Double)null);
	}
	
	@Test public void latitudeDMSDirectionLowerBoundary() throws
	ConversionException, InvalidValueWarning {
		record.setLatitude(-90.0, 0.0, 0.0, NORTH);
		assertEquals("Latitude: -90 00 00 N", -90.0, record.getLatitude(), 
				THRESHOLD);
		record.setLatitude(90.0, 0.0, 0.0, SOUTH);
		assertEquals("Latitude:  90 00 00 S", -90.0, record.getLatitude(),
				THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void latitudeDMSDirectionLowerBoundaryViolationNorth() throws
	ConversionException, InvalidValueWarning {
		record.setLatitude(-90.0, 0.0, 0.1, NORTH);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void latitudeDMSDirectionLowerBoundaryViolationSouth() throws
	ConversionException, InvalidValueWarning {
		record.setLatitude(90.0, 0.0, 0.1, SOUTH);
	}

	@Test (expected = ConversionException.class)
	public void latitudeDMSDirectionMinuteLowerBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		record.setLatitude(30.0, -0.1, 30.0, NORTH);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMSDirectionMinuteUpperBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		record.setLatitude(30.0, 60.1, 30.0, NORTH);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMSDirectionNullDegrees() throws ConversionException,
	InvalidValueWarning {
		record.setLatitude(null, 30.0, 30.0, NORTH);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMSDirectionNullDirection() throws ConversionException,
	InvalidValueWarning {
		record.setLatitude(30.0, 30.0, 30.0, null);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMSDirectionNullMinutes() throws ConversionException,
	InvalidValueWarning {
		record.setLatitude(30.0, null, 30.0, NORTH);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMSDirectionNullSeconds() throws ConversionException,
	InvalidValueWarning {
		record.setLatitude(30.0, 30.0, null, NORTH);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMSDirectionSecondLowerBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		record.setLatitude(30.0, 30.0, -0.1, NORTH);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMSDirectionSecondUpperBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		record.setLatitude(30.0, 30.0, 60.1, NORTH);
	}
	
	@Test public void latitudeDMSDirectionUpperBoundary() throws
	ConversionException, InvalidValueWarning {
		record.setLatitude(90.0, 0.0, 0.0, NORTH);
		assertEquals("Latitude:  90 00 00 N",  90.0, record.getLatitude(), 
				THRESHOLD);
		record.setLatitude(-90.0, 0.0, 0.0, SOUTH);
		assertEquals("Latitude: -90 00 00 S",  90.0, record.getLatitude(), 
				THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void latitudeDMSDirectionUpperBoundaryViolationNorth() throws
	ConversionException, InvalidValueWarning {
		record.setLatitude(90.0, 0.0, 0.1, NORTH);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void latitudeDMSDirectionUpperBoundaryViolationSouth() throws
	ConversionException, InvalidValueWarning {
		record.setLatitude(-90.0, 0.0, 0.1, SOUTH);
	}
		
	@Test public void latitudeDMSLowerBoundary() throws ConversionException,
	InvalidValueWarning {
		record.setLatitude(-90.0, 0.0, 0.0);
		assertEquals("Latitude: -90 00 00", -90.0, record.getLatitude(),
				THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void latitudeDMSLowerBoundaryViolation() throws ConversionException,
	InvalidValueWarning {
		record.setLatitude(-90.0, 0.0, 0.1);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMSMinuteLowerBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		record.setLatitude(30.0, -0.1, 30.0);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMSMinuteUpperBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		record.setLatitude(30.0, 60.1, 30.0);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMSNullDegrees() throws ConversionException,
	InvalidValueWarning {
		record.setLatitude(null, 30.0, 30.0);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMSNullMinutes() throws ConversionException,
	InvalidValueWarning {
		record.setLatitude(30.0, null, 30.0);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMSNullSeconds() throws ConversionException,
	InvalidValueWarning {
		record.setLatitude(30.0, 30.0, (Double)null);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMSSecondLowerBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		record.setLatitude(30.0, 30.0, -0.1);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMSSecondUpperBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		record.setLatitude(30.0, 30.0, 60.1);
	}
	
	@Test public void latitudeDMSUpperBoundary() throws ConversionException,
	InvalidValueWarning {
		record.setLatitude(90.0, 0.0, 0.0);
		assertEquals("Latitude:  90 00 00",  90.0, record.getLatitude(), 
				THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void latitudeDMSUpperBoundaryViolationNorth() throws
	ConversionException, InvalidValueWarning {
		record.setLatitude(90.0, 0.0, 0.1);
	}
	
	@Test public void latitudeDMUpperBoundary() throws ConversionException,
	InvalidValueWarning {
		record.setLatitude(90.0, 0.0);
		assertEquals("Latitude:  90 00",  90.0, record.getLatitude(), 
				THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void latitudeDMUpperBoundaryViolation() throws ConversionException,
	InvalidValueWarning {
		record.setLatitude(90.0, 0.1);
	}
	
	@Test public void longitudeDefaultValue() {
		assertNull("Longitude: default value", record.getLongitude());
	}

	@Test public void longitudeDegreeDirectionLowerBoundary() throws
	ConversionException, InvalidValueWarning {
		record.setLongitude(-180.0, EAST);
		assertEquals("Longitude: -180 E", -180.0, record.getLongitude(), 
				THRESHOLD);
		record.setLongitude(180.0, WEST);
		assertEquals("Longitude:  180 W", -180.0, record.getLongitude(),
				THRESHOLD);
	}

	@Test (expected = InvalidValueWarning.class)
	public void longitudeDegreeDirectionLowerBoundaryViolationNorth() throws
	ConversionException, InvalidValueWarning {
		record.setLongitude(-180.1, EAST);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void longitudeDegreeDirectionLowerBoundaryViolationSouth() throws
	ConversionException, InvalidValueWarning {
		record.setLongitude(180.1, WEST);
	}
	
	@Test public void longitudeDegreeDirectionNullDegrees() throws
	ConversionException, InvalidValueWarning {
		record.setLongitude(null, EAST);
		assertNull("Longitude: null E", record.getLongitude());
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDegreeDirectionNullDirection() throws
	ConversionException, InvalidValueWarning {
		record.setLongitude(30.0, (LongitudeUnit)null);
	}
	
	@Test public void longitudeDegreeDirectionUpperBoundary() throws
	ConversionException, InvalidValueWarning {
		record.setLongitude(180.0, EAST);
		assertEquals("Longitude:  180 E",  180.0, record.getLongitude(), 
				THRESHOLD);
		record.setLongitude(-180.0, WEST);
		assertEquals("Longitude: -180 W",  180.0, record.getLongitude(), 
				THRESHOLD);
	}

	@Test (expected = InvalidValueWarning.class)
	public void longitudeDegreeDirectionUpperBoundaryViolationNorth() throws
	ConversionException, InvalidValueWarning {
		record.setLongitude(180.1, EAST);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void longitudeDegreeDirectionUpperBoundaryViolationSouth() throws
	ConversionException, InvalidValueWarning {
		record.setLongitude(-180.1, WEST);
	}
	
	@Test public void longitudeDegreeLowerBoundary() throws
	InvalidValueWarning {
		record.setLongitude(-180.0);
		assertEquals("Longitude: -180", -180.0, record.getLongitude(), 
				THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void longitudeDegreeLowerBoundaryViolation() throws
	InvalidValueWarning {
		record.setLongitude(-180.1);
	}
	
	@Test public void longitudeDegreeNullDegrees() throws InvalidValueWarning {
		record.setLongitude(null);
		assertNull("Longitude: null", record.getLongitude());
	}
	
	@Test public void longitudeDegreeUpperBoundary() throws
	InvalidValueWarning {
		record.setLongitude(180.0);
		assertEquals("Longitude:  180",  180.0, record.getLongitude(), 
				THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void longitudeDegreeUpperBoundaryViolation() throws
	InvalidValueWarning {
		record.setLongitude(180.1);
	}
	
	@Test public void longitudeDMDirectionLowerBoundary() throws
	ConversionException, InvalidValueWarning {
		record.setLongitude(-180.0, 0.0, EAST);
		assertEquals("Longitude: -180 00 E", -180.0, record.getLongitude(), 
				THRESHOLD);
		record.setLongitude(180.0, 0.0, WEST);
		assertEquals("Longitude:  180 00 W", -180.0, record.getLongitude(),
				THRESHOLD);
	}

	@Test (expected = InvalidValueWarning.class)
	public void longitudeDMDirectionLowerBoundaryViolationNorth() throws
	ConversionException, InvalidValueWarning {
		record.setLongitude(-180.0, 0.1, EAST);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void longitudeDMDirectionLowerBoundaryViolationSouth() throws
	ConversionException, InvalidValueWarning {
		record.setLongitude(180.0, 0.1, WEST);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMDirectionMinuteLowerBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		record.setLongitude(30.0, -0.1, EAST);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMDirectionMinuteUpperBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		record.setLongitude(30.0, 60.1, EAST);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMDirectionNullDegrees() throws ConversionException,
	InvalidValueWarning {
		record.setLongitude(null, 30.0, EAST);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMDirectionNullDirection() throws ConversionException,
	InvalidValueWarning {
		record.setLongitude(30.0, 30.0, (LongitudeUnit)null);
	}
		
	@Test (expected = ConversionException.class)
	public void longitudeDMDirectionNullMinutes() throws ConversionException,
	InvalidValueWarning {
		record.setLongitude(30.0, null, EAST);
	}
	
	@Test public void longitudeDMDirectionUpperBoundary() throws
	ConversionException, InvalidValueWarning {
		record.setLongitude(180.0, 0.0, EAST);
		assertEquals("Longitude:  180 00 E",  180.0, record.getLongitude(), 
				THRESHOLD);
		record.setLongitude(-180.0, 0.0, WEST);
		assertEquals("Longitude: -180 00 W",  180.0, record.getLongitude(),
				THRESHOLD);
	}

	@Test (expected = InvalidValueWarning.class)
	public void longitudeDMDirectionUpperBoundaryViolationNorth() throws
	ConversionException, InvalidValueWarning {
		record.setLongitude(180.0, 0.1, EAST);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void longitudeDMDirectionUpperBoundaryViolationSouth() throws
	ConversionException, InvalidValueWarning {
		record.setLongitude(-180.0, 0.1, WEST);
	}
	
	@Test public void longitudeDMLowerBoundary() throws ConversionException,
	InvalidValueWarning {
		record.setLongitude(-180.0, 0.0);
		assertEquals("Longitude: -180 00", -180.0, record.getLongitude(), 
				THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void longitudeDMLowerBoundaryViolation() throws ConversionException,
	InvalidValueWarning {
		record.setLongitude(-180.0, 0.1);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMMinuteLowerBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		record.setLongitude(30.0, -0.1);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMMinuteUpperBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		record.setLongitude(30.0, 60.1);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMNullDegrees() throws ConversionException,
	InvalidValueWarning {
		record.setLongitude(null, 30.0);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMNullMinutes() throws ConversionException,
	InvalidValueWarning {
		record.setLongitude(30.0, (Double)null);
	}
	
	@Test public void longitudeDMSDirectionLowerBoundary() throws
	ConversionException, InvalidValueWarning {
		record.setLongitude(-180.0, 0.0, 0.0, EAST);
		assertEquals("Longitude: -180 00 00 E", -180.0, record.getLongitude(),
				THRESHOLD);
		record.setLongitude(180.0, 0.0, 0.0, WEST);
		assertEquals("Longitude:  180 00 00 W", -180.0, record.getLongitude(),
				THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void longitudeDMSDirectionLowerBoundaryViolationNorth() throws
	ConversionException, InvalidValueWarning {
		record.setLongitude(-180.0, 0.0, 0.1, EAST);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void longitudeDMSDirectionLowerBoundaryViolationSouth() throws
	ConversionException, InvalidValueWarning {
		record.setLongitude(180.0, 0.0, 0.1, WEST);
	}

	@Test (expected = ConversionException.class)
	public void longitudeDMSDirectionMinuteLowerBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		record.setLongitude(30.0, -0.1, 30.0, EAST);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMSDirectionMinuteUpperBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		record.setLongitude(30.0, 60.1, 30.0, EAST);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMSDirectionNullDegrees() throws
	ConversionException, InvalidValueWarning {
		record.setLongitude(null, 30.0, 30.0, EAST);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMSDirectionNullDirection() throws
	ConversionException, InvalidValueWarning {
		record.setLongitude(30.0, 30.0, 30.0, null);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMSDirectionNullMinutes() throws
	ConversionException, InvalidValueWarning {
		record.setLongitude(30.0, null, 30.0, EAST);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMSDirectionNullSeconds() throws ConversionException,
	InvalidValueWarning {
		record.setLongitude(30.0, 30.0, null, EAST);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMSDirectionSecondLowerBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		record.setLongitude(30.0, 30.0, -0.1, EAST);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMSDirectionSecondUpperBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		record.setLongitude(30.0, 30.0, 60.1, EAST);
	}
	
	@Test public void longitudeDMSDirectionUpperBoundary() throws
	ConversionException, InvalidValueWarning {
		record.setLongitude(180.0, 0.0, 0.0, EAST);
		assertEquals("Longitude:  180 00 00 E",  180.0, record.getLongitude(),
				THRESHOLD);
		record.setLongitude(-180.0, 0.0, 0.0, WEST);
		assertEquals("Longitude: -180 00 00 W",  180.0, record.getLongitude(), 
				THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void longitudeDMSDirectionUpperBoundaryViolationNorth() throws
	ConversionException, InvalidValueWarning {
		record.setLongitude(180.0, 0.0, 0.1, EAST);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void longitudeDMSDirectionUpperBoundaryViolationSouth() throws
	ConversionException, InvalidValueWarning {
		record.setLongitude(-180.0, 0.0, 0.1, WEST);
	}
		
	@Test public void longitudeDMSLowerBoundary() throws ConversionException,
	InvalidValueWarning {
		record.setLongitude(-180.0, 0.0, 0.0);
		assertEquals("Longitude: -180 00 00", -180.0, record.getLongitude(), 
				THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void longitudeDMSLowerBoundaryViolation() throws ConversionException,
	InvalidValueWarning {
		record.setLongitude(-180.0, 0.0, 0.1);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMSMinuteLowerBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		record.setLongitude(30.0, -0.1, 30.0);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMSMinuteUpperBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		record.setLongitude(30.0, 60.1, 30.0);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMSNullDegrees() throws ConversionException,
	InvalidValueWarning {
		record.setLongitude(null, 30.0, 30.0);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMSNullMinutes() throws ConversionException,
	InvalidValueWarning {
		record.setLongitude(30.0, null, 30.0);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMSNullSeconds() throws ConversionException,
	InvalidValueWarning {
		record.setLongitude(30.0, 30.0, (Double)null);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMSSecondLowerBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		record.setLongitude(30.0, 30.0, -0.1);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMSSecondUpperBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		record.setLongitude(30.0, 30.0, 60.1);
	}
	
	@Test public void longitudeDMSUpperBoundary() throws ConversionException,
	InvalidValueWarning {
		record.setLongitude(180.0, 0.0, 0.0);
		assertEquals("Longitude:  180 00 00",  180.0, record.getLongitude(), 
				THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void longitudeDMSUpperBoundaryViolationNorth() throws
	ConversionException, InvalidValueWarning {
		record.setLongitude(180.0, 0.0, 0.1);
	}
	
	@Test public void longitudeDMUpperBoundary() throws ConversionException,
	InvalidValueWarning {
		record.setLongitude(180.0, 0.0);
		assertEquals("Longitude:  180 00",  180.0, record.getLongitude(), 
				THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void longitudeDMUpperBoundaryViolation() throws ConversionException,
	InvalidValueWarning {
		record.setLongitude(180.0, 0.1);
	}

	@Test public void pressure() throws CalculationWarning, ConversionException,
	InvalidValueWarning {
		assertNull("Pressure: default", record.getPressure());
		record.setPressure(1000.0, MILLIBARS);
		assertEquals("Pressure: set m", 1000.0, record.getPressure(), 
				THRESHOLD);
		record.setPressure(1.0, ATMOSPHERES);
		assertEquals("Pressure: set diff unit", 1013.25, record.getPressure(), 
				THRESHOLD);
		record.setPressure(0.0, MILLIBARS);
		assertEquals("Pressure: zero", 0.0, record.getPressure(), THRESHOLD);
		record.setPressure(-100.0, MILLIBARS);
		assertEquals("Pressure: negative", -100.0, record.getPressure(), 
				THRESHOLD);
		record.setPressure(null, MILLIBARS);
		assertNull("Pressure: null", record.getPressure());
	}
	
	@Test (expected = ConversionException.class)
	public void pressureNullUnit() throws CalculationWarning,
	ConversionException, InvalidValueWarning {
		record.setPressure(1000.0, null);
	}
	
	@Test public void relativeHumidity() throws CalculationWarning,
	InvalidValueWarning {
		assertNull("RH: default", record.getRelativeHumidity());
		
		record.setRelativeHumidity(12.0);
		assertEquals("RH: set value", 12.0, record.getRelativeHumidity(), 
				THRESHOLD);
		record.setRelativeHumidity(0.0);
		assertEquals("RH: zero", 0.0, record.getRelativeHumidity(), THRESHOLD);
		record.setRelativeHumidity(-10.0);
		assertEquals("RH: negative", -10.0, record.getRelativeHumidity(), 
				THRESHOLD);
		record.setRelativeHumidity(null);
		assertNull("RH: set null", record.getRelativeHumidity());
	}
	
	@Test public void relativeHumidityCalculation() throws CalculationWarning,
	ConversionException, InvalidValueWarning {
		assertNull("RH Calc: sanity check", record.getDewPoint());
		
		record.setTemperature(15.0, CELCIUS);
		record.setDewPoint(15.0, CELCIUS);
		assertEquals("RH Calc: calculated", 100.0, record.getRelativeHumidity(),
				THRESHOLD);
		record.setRelativeHumidity(11.11);
		assertEquals("RH Calc: manual override", 11.11, 
				record.getRelativeHumidity(), THRESHOLD);
		record.setTemperature(10.0, CELCIUS);
		assertEquals("RH Calc: temp change in override", 11.11, 
				record.getRelativeHumidity(), THRESHOLD);
		record.setDewPoint(10.0, CELCIUS);
		assertEquals("RH Calc: dp change in override", 11.11, 
				record.getRelativeHumidity(), THRESHOLD);
		record.setRelativeHumidity(null);
		assertEquals("RH Calc: rh set to null", 100.0, 
				record.getRelativeHumidity(), THRESHOLD);
	}
	
	@Test public void relativeHumidityCalculationDewPoint() throws
	CalculationWarning, ConversionException, InvalidValueWarning {
		record.setTemperature(10.0, CELCIUS);
		assertEquals("RH Calc: temp set sanity check", 10.0, 
				record.getTemperature(), THRESHOLD);
		assertNull("RH Calc: temp only set", record.getRelativeHumidity());
		
		record.setDewPoint(10.0, CELCIUS);
		assertEquals("RH Calc: dp now set", 100.0, record.getRelativeHumidity(),
				THRESHOLD);
		record.setDewPoint(15.0, CELCIUS);
		assertEquals("RH Calc: change dp", 138.8601424483, 
				record.getRelativeHumidity(), THRESHOLD);
		record.setDewPoint(15.0, CELCIUS);
		assertEquals("RH Calc: change dp same", 138.8601424483, 
				record.getRelativeHumidity(), THRESHOLD);
		
		// Make sure the bad dp value causes the rh to be null.
		try { record.setDewPoint(-243.5, CELCIUS); }
		catch (CalculationWarning e) {
			assertNull("RH Calc: bad dp", record.getRelativeHumidity());
			record.setDewPoint(15.0, CELCIUS);
		}
		
		record.setDewPoint(null, CELCIUS);
		assertNull("RH Calc: change dp null", record.getRelativeHumidity());
	}
	
	@Test (expected = CalculationWarning.class)
	public void relativeHumidityCalculationInvalidDewPoint() throws
	CalculationWarning, ConversionException, InvalidValueWarning {
		record.setTemperature(15.0, CELCIUS);
		record.setDewPoint(-243.5, CELCIUS);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void relativeHumidityCalculationInvalidTemperature() throws
	CalculationWarning, ConversionException, InvalidValueWarning {
		record.setDewPoint(12.0, CELCIUS);
		record.setTemperature(-243.5, CELCIUS);
	}
	
	@Test public void relativeHumidityCalculationTemperature() throws
	CalculationWarning, ConversionException, InvalidValueWarning {
		record.setDewPoint(15.0, CELCIUS);
		assertEquals("RH Calc: dew point set sanity check", 15.0, 
				record.getDewPoint(), THRESHOLD);
		assertNull("RH Calc: rh - dp only set", record.getRelativeHumidity());
		
		record.setTemperature(15.0, CELCIUS);
		assertEquals("RH Calc: rh - temp now set", 100.0, 
				record.getRelativeHumidity(), THRESHOLD);
		record.setTemperature(0.0, CELCIUS);
		assertEquals("RH Calc: rh - change temp", 278.8039027806, 
				record.getRelativeHumidity(), THRESHOLD);
		record.setTemperature(32.0, FAHRENHEIT);
		assertEquals("RH Calc: rh - change temp same", 278.8039027806, 
				record.getRelativeHumidity(), THRESHOLD);

		// Make sure the bad temp value causes the rh to be null.
		try { record.setTemperature(-243.5, CELCIUS); }
		catch (InvalidValueWarning e) {
			assertNull("RH Calc: bad temp", record.getRelativeHumidity());
			record.setTemperature(0.0, CELCIUS);
		}
		
		record.setTemperature(null, CELCIUS);
		assertNull("RH Calc: rh - change temp null", 
				record.getRelativeHumidity());
	}
	
	@Test public void temperature() throws CalculationWarning,
	ConversionException, InvalidValueWarning {
		assertNull("Temperature: default", record.getTemperature());
		record.setTemperature(10.0, CELCIUS);
		assertEquals("Temperature: set C", 10.0, record.getTemperature(), 
				THRESHOLD);
		record.setTemperature(32.0, FAHRENHEIT);
		assertEquals("Temperature: set F", 0.0, record.getTemperature(), 
				THRESHOLD);
		record.setTemperature(-10.0, CELCIUS);
		assertEquals("Temperature: negative", -10.0, record.getTemperature(),
				THRESHOLD);
		record.setTemperature(0.0, CELCIUS);
		assertEquals("Temperature: zero", 0.0, record.getTemperature(),
				THRESHOLD);
		record.setTemperature(null, CELCIUS);
		assertNull("Temperature: null", record.getTemperature());
	}
	
	@Test (expected = ConversionException.class)
	public void temperatureNullUnit() throws CalculationWarning,
	ConversionException, InvalidValueWarning {
		record.setTemperature(10.0, null);
	}
	
	@Test public void time() throws CalculationWarning, ConversionException,
	InvalidValueWarning {
		assertNull("Time: default", record.getTime());
		record.setTime(10.0);
		assertEquals("Time: positive", 10.0, record.getTime());
		record.setTime(0.0);
		assertEquals("Time: zero", 0.0, record.getTime());
		record.setTime(-1.0);
		assertEquals("Time: negative", -1.0, record.getTime());
		record.setTime(null);
		assertNull("Time: null", record.getTime());
		record.setTime(1.0, 1.0);
		assertEquals("Time: minutes second", 61.0, record.getTime());
	}
	
	@Test (expected = ConversionException.class)
	public void timeNullMinute() throws CalculationWarning,
	ConversionException, InvalidValueWarning {
		record.setTime(null, 0.0);
	}
	
	@Test (expected = ConversionException.class)
	public void timeNullSecond() throws CalculationWarning,
	ConversionException, InvalidValueWarning {
		record.setTime(1.0, null);
	}
	
	@Test (expected = ConversionException.class)
	public void uComponentNullUnit() throws CalculationWarning,
	ConversionException, InvalidValueWarning  {
		record.setUComponent(10.0, null);
	}
	
	@Test (expected = ConversionException.class)
	public void vComponentNullUnit() throws CalculationWarning,
	ConversionException, InvalidValueWarning {
		record.setVComponent(10.0, null);
	}
	
	@Test public void uvComponents() throws CalculationWarning,
	ConversionException, InvalidValueWarning {
		// Test the default u/v wind value.
		assertNull("U Wind: default",record.getUComponent());
		assertNull("V Wind: default",record.getVComponent());
		
		// Test a valid value.
		record.setUComponent(10.0, METERS_PER_SECOND);
		record.setVComponent(10.0, METERS_PER_SECOND);
		assertEquals("U Wind: value",10.0,record.getUComponent(),THRESHOLD);
		assertEquals("V Wind: value",10.0,record.getVComponent(),THRESHOLD);
		// Test a valid value with different units
		record.setUComponent(10.0, FEET_PER_SECOND);
		record.setVComponent(5.0, FEET_PER_SECOND);
		assertEquals("U Wind: diff unit",3.048,record.getUComponent(),
				THRESHOLD);
		assertEquals("V Wind: diff unit",1.524,record.getVComponent(),
				THRESHOLD);
		// Test setting a null U value.
		record.setUComponent(null, METERS_PER_SECOND);
		assertNull("UComponent: null U",record.getUComponent());
		assertEquals("VComponent: null U",1.524,record.getVComponent(),
				THRESHOLD);
		// Test setting a null V value.
		record.setVComponent(null,METERS_PER_SECOND);
		assertNull("UComponent: null V",record.getUComponent());
		assertNull("VComponent: null V",record.getVComponent());
	}
	
	@Test public void uvComponentCalculations() throws CalculationWarning,
	ConversionException, InvalidValueWarning {
		// Test the default u/v values.
		assertNull("U Wind Calc: default",record.getUComponent());
		assertNull("V Wind Calc: default",record.getVComponent());
		
		// Both values set
		record.setWindSpeed(10.0, METERS_PER_SECOND);
		record.setWindDirection(180.0);
		assertEquals("U Wind Calc: two real values",0.0,record.getUComponent(),
				THRESHOLD);
		assertEquals("V Wind Calc: two real values",10.0,record.getVComponent(),
				THRESHOLD);
		
		// Set to a bad wind speed
		try {
			record.setWindSpeed(-1.0, METERS_PER_SECOND);
		} catch (CalculationWarning e) {
			assertNull("U Wind Calc: negative wind speed", 
					record.getUComponent());
			assertNull("V Wind Calc: negative wind speed",
					record.getVComponent());
			record.setWindSpeed(10.0, METERS_PER_SECOND);
		}
		
		// Set to a negative wind direction
		try {
			record.setWindDirection(-1.0);
		} catch (CalculationWarning e) {
			assertNull("U Wind Calc: negative wind direction", 
					record.getUComponent());
			assertNull("V Wind Calc: negative wind direction", 
					record.getVComponent());
			record.setWindDirection(180.0);
		}
		
		// Set to a bad wind direction
		try {
			record.setWindDirection(360.1);
		} catch (CalculationWarning e) {
			assertNull("U Wind Calc: over positive wind direction", 
					record.getUComponent());
			assertNull("V Wind Calc: over positive wind direction", 
					record.getVComponent());
			record.setWindDirection(180.0);
		}
		
		// Change only the wind speed
		record.setWindSpeed(15.0, METERS_PER_SECOND);
		assertEquals("U Wind Calc: change speed",0.0,record.getUComponent(),
				THRESHOLD);
		assertEquals("V Wind Calc: change speed",15.0,record.getVComponent(),
				THRESHOLD);
		
		// Change only the wind direction
		record.setWindDirection(0.0);
		assertEquals("U Wind Calc: change direction",0.0,
				record.getUComponent(),THRESHOLD);
		assertEquals("V Wind Calc: change direction",-15.0,
				record.getVComponent(),THRESHOLD);
		
		// Null the wind speed
		record.setWindSpeed(null, METERS_PER_SECOND);
		assertNull("U Wind Calc: null speed",record.getUComponent());
		assertNull("V Wind Calc: null speed",record.getVComponent());
		
		// Put the wind speed back
		record.setWindSpeed(15.0, METERS_PER_SECOND);
		assertEquals("U Wind Calc: return speed",0.0,record.getUComponent(),
				THRESHOLD);
		assertEquals("V Wind Calc: return speed",-15.0,record.getVComponent(),
				THRESHOLD);
		
		// Null the wind direction
		record.setWindDirection(null);
		assertNull("U Wind Calc: null direction",record.getUComponent());
		assertNull("V Wind Calc: null direction",record.getVComponent());
		
		// Put the direction back
		record.setWindDirection(0.0);
		assertEquals("U Wind Calc: return direction",0.0,
				record.getUComponent(),THRESHOLD);
		assertEquals("V Wind Calc: return direction",-15.0,
				record.getVComponent(),THRESHOLD);
		
		// Manually set the U Value
		record.setUComponent(2.0, METERS_PER_SECOND);
		assertEquals("U Wind Calc: manual U only",2.0,record.getUComponent(),
				THRESHOLD);
		assertNull("V Wind Calc: manual U only",record.getVComponent());
		
		// Manually set the V Value
		record.setUComponent(null, METERS_PER_SECOND);
		record.setVComponent(6.0, METERS_PER_SECOND);
		assertNull("U Wind Calc: manual V only",record.getUComponent());
		assertEquals("V Wind Calc: manual V only",6.0,record.getVComponent(),
				THRESHOLD);
		
		// Manually set both U and V
		record.setUComponent(10.0, METERS_PER_SECOND);
		record.setVComponent(10.0, METERS_PER_SECOND);
		assertEquals("U Wind Calc: manual both",10.0,record.getUComponent(),
				THRESHOLD);
		assertEquals("V Wind Calc: manual both",10.0,record.getVComponent(),
				THRESHOLD);
		
		// Change the wind speed after a manual change
		record.setWindSpeed(1.5, METERS_PER_SECOND);
		assertEquals("U Wind Calc: speed change after manual",10.0,
				record.getUComponent(),THRESHOLD);
		assertEquals("V Wind Calc: speed change after manual",10.0,
				record.getVComponent(),THRESHOLD);
		
		// Change the wind direction after a manual change
		record.setWindDirection(180.0);
		assertEquals("U Wind Calc: direction change after manual",10.0,
				record.getUComponent(),THRESHOLD);
		assertEquals("V Wind Calc: direction change after manual",10.0,
				record.getVComponent(),THRESHOLD);
		
		// Change the U and V back to null to make sure the calcuation starts 
		// again.
		record.setUComponent(null, METERS_PER_SECOND);
		record.setVComponent(null, METERS_PER_SECOND);
		assertEquals("U Wind Calc: nulled u and v",0.0,
				record.getUComponent(),THRESHOLD);
		assertEquals("V Wind Calc: nulled u and v",1.5,
				record.getVComponent(),THRESHOLD);
	}
	
	@Test (expected = CalculationWarning.class)
	public void uvComponentCalculationsNegativeWindDirection() throws
	CalculationWarning, ConversionException, InvalidValueWarning {
		record.setWindSpeed(5.0, METERS_PER_SECOND);
		record.setWindDirection(-1.0);
	}
	
	@Test (expected = CalculationWarning.class)
	public void uvComponentCalculationsNegativeWindSpeed() throws
	CalculationWarning, ConversionException, InvalidValueWarning {
		record.setWindDirection(4.0);
		record.setWindSpeed(-1.0, METERS_PER_SECOND);
	}
	
	@Test (expected = CalculationWarning.class)
	public void uvComponentCalculationsOverPositiveWindDirection() throws
	CalculationWarning, ConversionException, InvalidValueWarning {
		record.setWindSpeed(5.0, METERS_PER_SECOND);
		record.setWindDirection(360.1);
	}
	
	@Test public void windSpeedAndDirection() throws CalculationWarning,
	ConversionException, InvalidValueWarning {
		// Test the default wind values.
		assertNull("Wind Speed: default",record.getWindSpeed());
		assertNull("Wind Direction: default",record.getWindDirection());
		// Test a valid value.
		record.setWindSpeed(10.0, METERS_PER_SECOND);
		record.setWindDirection(10.0);
		assertEquals("Wind Speed: value",10.0,record.getWindSpeed(),THRESHOLD);
		assertEquals("Wind Direction: value",10.0,record.getWindDirection(),
				THRESHOLD);
		// Test a valid value with different units
		record.setWindSpeed(10.0, FEET_PER_SECOND);
		assertEquals("Wind Speed: diff unit",3.048,record.getWindSpeed(),
				THRESHOLD);
		assertEquals("Wind Direction: diff unit",10.0,record.getWindDirection(),
				THRESHOLD);
		// Test setting a null wind speed value.
		record.setWindSpeed(null, METERS_PER_SECOND);
		assertNull("Wind Speed: null wind speed",record.getWindSpeed());
		assertEquals("Wind Direction: null wind speed",10.0,
				record.getWindDirection(),THRESHOLD);
		// Test setting a null wind direction value.
		record.setWindDirection(null);
		assertNull("Wind Speed: null wind direction",record.getWindSpeed());
		assertNull("Wind Direction: null wind direction",
				record.getWindDirection());
	}
	
	@Test public void windSpeedAndDirectionCalculations() throws
	CalculationWarning, ConversionException, InvalidValueWarning {
		// Test the default u/v values.
		assertNull("Wind Speed Calc: default",record.getWindSpeed());
		assertNull("Wind Direction Calc: default",record.getWindDirection());
		
		// Both values set
		record.setUComponent(0.0, METERS_PER_SECOND);
		record.setVComponent(10.0, METERS_PER_SECOND);
		assertEquals("Wind Speed Calc: two real values",10.0,
				record.getWindSpeed(),THRESHOLD);
		assertEquals("Wind Direction Calc: two real values",180.0,
				record.getWindDirection(),THRESHOLD);
		
		// Change only the U component
		record.setUComponent(15.0, METERS_PER_SECOND);
		assertEquals("Wind Speed Calc: change U",18.0277563773,
				record.getWindSpeed(),THRESHOLD);
		assertEquals("Wind Direction Calc: change U",236.309932474,
				record.getWindDirection(),THRESHOLD);
		
		// Change only the V component
		record.setVComponent(0.0, METERS_PER_SECOND);
		assertEquals("Wind Speed Calc: change V",15.0,record.getWindSpeed(),
				THRESHOLD);
		assertEquals("Wind Direction Calc: change V",270.0,
				record.getWindDirection(),THRESHOLD);
		
		// Null the U component
		record.setUComponent(null, METERS_PER_SECOND);
		assertNull("Wind Speed Calc: null U",record.getWindSpeed());
		assertNull("Wind Direction Calc: null U",record.getWindDirection());
		
		// Put the U component back
		record.setUComponent(15.0, METERS_PER_SECOND);
		assertEquals("Wind Speed Calc: return U",15.0,record.getWindSpeed(),
				THRESHOLD);
		assertEquals("Wind Direction Calc: return U",270.0,
				record.getWindDirection(),THRESHOLD);
		
		// Null the v component
		record.setVComponent(null, METERS_PER_SECOND);
		assertNull("Wind Speed Calc: null V",record.getWindSpeed());
		assertNull("Wind Direction Calc: null V",record.getWindDirection());
		
		// Put the V component back
		record.setVComponent(0.0, METERS_PER_SECOND);
		assertEquals("Wind Speed Calc: return V",15.0,record.getWindSpeed(),
				THRESHOLD);
		assertEquals("Wind Direction Calc: return V",270.0,
				record.getWindDirection(),THRESHOLD);
		
		// Manually set the speed
		record.setWindSpeed(2.0, METERS_PER_SECOND);
		assertEquals("Wind Speed Calc: manual speed",2.0,record.getWindSpeed(),
				THRESHOLD);
		assertNull("Wind Direction Calc: manual speed",
				record.getWindDirection());
		
		// Manually set the direction
		record.setWindSpeed(null, METERS_PER_SECOND);
		record.setWindDirection(6.0);
		assertNull("Wind Speed Calc: manual direction",record.getWindSpeed());
		assertEquals("Wind Direction Calc: manual direction",6.0,
				record.getWindDirection(),THRESHOLD);
		
		// Manually set both speed and direction
		record.setWindSpeed(10.0, METERS_PER_SECOND);
		record.setWindDirection(10.0);
		assertEquals("Wind Speed Calc: manual both",10.0,record.getWindSpeed(),
				THRESHOLD);
		assertEquals("Wind Direction Calc: manual both",10.0,
				record.getWindDirection(),THRESHOLD);
		
		// Change the u component after a manual change
		record.setUComponent(15.0, METERS_PER_SECOND);
		assertEquals("Wind Speed Calc: U change after manual",10.0,
				record.getWindSpeed(),THRESHOLD);
		assertEquals("Wind Direction Calc: U change after manual",10.0,
				record.getWindDirection(),THRESHOLD);
		
		// Change the v component after a manual change
		record.setVComponent(0.0, METERS_PER_SECOND);
		assertEquals("Wind Speed Calc: V change after manual",10.0,
				record.getWindSpeed(),THRESHOLD);
		assertEquals("Wind Direction Calc: V change after manual",10.0,
				record.getWindDirection(),THRESHOLD);
		
		// Change the speed and direction back to null to make sure the 
		// calcuation starts again.
		record.setWindSpeed(null, METERS_PER_SECOND);
		record.setWindDirection(null);
		assertEquals("Wind Speed Calc: nulled speed and direction",15.0,
				record.getWindSpeed(),THRESHOLD);
		assertEquals("Wind Direction Calc: nulled speed and direction",270.0,
				record.getWindDirection(),THRESHOLD);
	}
	
	@Test (expected = ConversionException.class)
	public void windSpeedNullUnit() throws CalculationWarning,
	ConversionException, InvalidValueWarning {
		record.setWindSpeed(10.0, null);
	}
}
