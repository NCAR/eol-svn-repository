package dmg.station;

import static dmg.util.LengthUtils.*;
import static dmg.util.PositionUtils.*;
import static dmg.util.TimeUtils.*;
import static org.junit.Assert.*;

import dmg.util.*;
import dmg.util.PositionUtils.*;

import java.io.*;
import java.util.*;
import org.junit.*;

public class StationTest  {

	private static final Double THRESHOLD = Math.pow(1, -10);
	
	private Calendar december, january, march;
	private Station station;
	
	@Before public void setup() throws Exception {
		station = new Station();
		january = buildDate(2007, 1, 1, UTC);
		march = buildDate(2007, 3, 23, UTC);
		december = buildDate(2007, 12, 31, UTC);
	}
	
	@Test public void beginDate() throws DateTimeException {
		assertNull("Begin Date: default value", station.getBeginDate());
		station.setBeginDate(buildDate(2007, 3, 23, UTC));
		assertEquals("Begin Date: calendar", march, station.getBeginDate());
		station.setBeginDate(null);
		assertNull("Begin Date: null", station.getBeginDate());
		station.setBeginDate(2007, 3, 23, UTC);
		assertEquals("Begin Date: YYYY, MM, DD, tz", march, 
				station.getBeginDate());
		station.setBeginDate(2007, 365, UTC);
		assertEquals("Begin Day: YYYY, JJJ, tz", december, 
				station.getBeginDate());
	}
	
	@Test (expected = DateTimeException.class)
	public void beginDateDayLowerBoundaryViolation() throws DateTimeException {
		station.setBeginDate(2007, 1, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void beginDateDayUpperBoundaryViolation() throws DateTimeException {
		station.setBeginDate(2007, 1, 32, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void beginDateJulianLowerBoundaryViolation() throws 
	DateTimeException {
		station.setBeginDate(2007, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void beginDateJulianNullTimeZone() throws DateTimeException {
		station.setBeginDate(2007, 1, null);
	}
	
	@Test (expected = DateTimeException.class)
	public void beginDateJulianUpperBoundaryViolation() throws 
	DateTimeException {
		station.setBeginDate(2007, 366, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void beginDateMonthLowerBoundaryViolation() throws 
	DateTimeException {
		station.setBeginDate(2007, 0, 1, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void beginDateMonthUpperBoundaryViolation() throws 
	DateTimeException {
		station.setBeginDate(2007, 13, 1, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void beginDateNullTimeZone() throws DateTimeException {
		station.setBeginDate(2007, 1, 1, null);
	}
	
	@Test public void commissioned() {
		assertFalse("Commissioned: default", station.isCommissioned());
		station.setCommissioned(true);
		assertTrue("Commissioned: true", station.isCommissioned());
		station.setCommissioned(false);
		assertFalse("Commissioned: false", station.isCommissioned());
	}
	
	@Test public void countyCode() throws InvalidValueException {
		assertNull("County Code: default", station.getCountyCode());
		station.setCountyCode("ABC");
		assertEquals("County Code: value", "ABC", station.getCountyCode());
		station.setCountyCode(null);
		assertNull("County Code: null", station.getCountyCode());
	}
	
	@Test (expected = InvalidValueException.class)
	public void countyCodeTooLong() throws InvalidValueException {
		station.setCountyCode("ABCD");
	}
	
	@Test (expected = InvalidValueException.class)
	public void countyCodeTooShort() throws InvalidValueException {
		station.setCountyCode("");
	}

	@Test public void daylightSavings() {
		assertFalse("DST: default", station.isDaylightSavingsUsed());
		station.setDaylightSavingsUsed(true);
		assertTrue("DST: true", station.isDaylightSavingsUsed());
		station.setDaylightSavingsUsed(false);
		assertFalse("DST: false", station.isDaylightSavingsUsed());
	}
	
	@Test public void description() throws InvalidValueException {
		assertNull("Description: default", station.getDescription());
		station.setDescription("A New Desc Value");
		assertEquals("Description: new value", "A New Desc Value", 
				station.getDescription());
		station.setDescription(
				"1234567890123456789012345678901234567890123456");
		assertEquals("Description: max length", 
				"1234567890123456789012345678901234567890123456", 
				station.getDescription());
		station.setDescription(null);
		assertNull("Description: null", 
				station.getDescription());
	}
	
	@Test (expected = InvalidValueException.class)
	public void descriptionTooLong() throws InvalidValueException {
		station.setDescription(
				"12345678901234567890123456789012345678901234567");
	}
	
	@Test (expected = InvalidValueException.class)
	public void descriptionTooShort() throws InvalidValueException {
		station.setDescription("");
	}
	
	@Test public void elevation() throws ConversionException, 
	InvalidValueException, RestrictedOperationException {
		assertNull("Elevation: default value", station.getElevation());
		station.setElevation(10.0, METERS);
		assertEquals("Elevation: new METERS value", 10.0, 
				station.getElevation(), THRESHOLD);
		station.setElevation(100.0, KILOMETERS);
		assertEquals("Elevation: new km value", 100000.0, 
				station.getElevation(), THRESHOLD);
		station.setElevation(null, METERS);
		assertNull("Elevation: null", station.getElevation());
		station.setElevation(-9999.84, METERS);
		assertEquals("Elevation: lower boundary", -9999.84, 
				station.getElevation(), THRESHOLD);
		station.setElevation(9999999.94, METERS);
		assertEquals("Elevation: upper boundary", 9999999.94, 
				station.getElevation(), THRESHOLD);
	}
	
	@Test (expected = InvalidValueException.class)
	public void elevationLowerBoundaryViolation() throws ConversionException, 
	InvalidValueException, RestrictedOperationException {
		station.setElevation(-9999.85, METERS);
	}
	
	@Test (expected = ConversionException.class)
	public void elevationNullUnit() throws ConversionException, 
	InvalidValueException, RestrictedOperationException {
		station.setElevation(10.0, null);
	}
	
	@Test (expected = InvalidValueException.class)
	public void elevationUpperBoundaryViolation() throws ConversionException,
	InvalidValueException, RestrictedOperationException {
		station.setElevation(9999999.95, METERS);
	}
	
	@Test public void endDate() throws DateTimeException {
		assertNull("End Date: default value", station.getEndDate());
		station.setEndDate(buildDate(2007, 3, 23, UTC));
		assertEquals("End Date: calendar", march, station.getEndDate());
		station.setEndDate(null);
		assertNull("End Date: null", station.getEndDate());
		station.setEndDate(2007, 3, 23, UTC);
		assertEquals("End Date: YYYY, MM, DD, tz", march, station.getEndDate());
		station.setEndDate(2007, 365, UTC);
		assertEquals("End Day: YYYY, JJJ, tz", december, station.getEndDate());
	}
	
	@Test (expected = DateTimeException.class)
	public void endDateDayLowerBoundaryViolation() throws DateTimeException {
		station.setEndDate(2007, 1, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void endDateDayUpperBoundaryViolation() throws DateTimeException {
		station.setEndDate(2007, 1, 32, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void endDateJulianLowerBoundaryViolation() throws DateTimeException {
		station.setEndDate(2007, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void endDateJulianNullTimeZone() throws DateTimeException {
		station.setEndDate(2007, 1, null);
	}
	
	@Test (expected = DateTimeException.class)
	public void endDateJulianUpperBoundaryViolation() throws DateTimeException {
		station.setEndDate(2007, 366, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void endDateMonthLowerBoundaryViolation() throws DateTimeException {
		station.setEndDate(2007, 0, 1, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void endDateMonthUpperBoundaryViolation() throws DateTimeException {
		station.setEndDate(2007, 13, 1, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void endDateNullTimeZone() throws DateTimeException {
		station.setEndDate(2007, 1, 1, null);
	}
	
	@Test public void frequency() throws InvalidValueException {
		assertNull("Frequency: default", station.getFrequency());
		station.setFrequency("1 minute");
		assertEquals("Frequency: value", "1 minute", station.getFrequency());
		station.setFrequency("123456789012345");
		assertEquals("Frequency: max length", "123456789012345", 
				station.getFrequency());
		station.setFrequency(null);
		assertNull("Frequency: null", station.getFrequency());
	}
	
	@Test (expected = InvalidValueException.class)
	public void frequencyTooLong() throws InvalidValueException {
		station.setFrequency("1234567890123456");
	}
	
	@Test (expected = InvalidValueException.class)
	public void frequencyTooShort() throws InvalidValueException {
		station.setFrequency("");
	}
	
	@Test public void insertDateValues() throws DateTimeException {
		assertNull("Insert Date: default begin date", station.getBeginDate());
		assertNull("Insert Date: default end date", station.getEndDate());
		
		station.insertDate(march);
		assertEquals("Insert Date: begin march", march, station.getBeginDate());
		assertEquals("Insert Date: end march", march, station.getEndDate());
		
		station.insertDate(december);
		assertEquals("Insert Date: begin december", march, 
				station.getBeginDate());
		assertEquals("Insert Date: end december", december, 
				station.getEndDate());
		
		station.insertDate(january);
		assertEquals("Insert Date: begin january", january, 
				station.getBeginDate());
		assertEquals("Insert Date: end january", december, 
				station.getEndDate());
		
		station.insertDate(2006, 12, 31, UTC);
		assertEquals("Insert Date: YYYY, MM, DD, tz (begin dec)", 
				buildDate(2006, 12, 31, UTC), station.getBeginDate());
		assertEquals("Insert Date: YYYY, MM, DD, tz (end dec)", december,
				station.getEndDate());

		station.insertDate(2008, 1, 1, UTC);
		assertEquals("Insert Date: YYYY, MM, DD, tz (begin jan)", 
				buildDate(2006, 12, 31, UTC), station.getBeginDate());
		assertEquals("Insert Date: YYYY, MM, DD, tz (end jan)", 
				buildDate(2008, 1, 1, UTC), station.getEndDate());

		station.insertDate(2006, 200, UTC);
		assertEquals("Insert Date: YYYY, JJJ, tz (begin 200)", 
				buildJulianDate(2006, 200, UTC), station.getBeginDate());
		assertEquals("Insert Date: YYYY, JJJ, tz (end 200)", 
				buildDate(2008, 1, 1, UTC), station.getEndDate());

		station.insertDate(2008, 5, UTC);
		assertEquals("Insert Date: YYYY, JJJ, tz (begin 5)", 
				buildJulianDate(2006, 200, UTC), station.getBeginDate());
		assertEquals("Insert Date: YYYY, JJJ, tz (end 5)", 
				buildDate(2008, 1, 5, UTC), station.getEndDate());
	}
	
	@Test (expected = DateTimeException.class)
	public void insertDateNullCalendar() throws DateTimeException {
		station.insertDate(null);
	}
	
	@Test (expected = DateTimeException.class)
	public void insertDateJulianNullTimeZone() throws DateTimeException {
		station.insertDate(2007, 1, null);
	}
	
	@Test (expected = DateTimeException.class)
	public void insertDateNullTimeZone() throws DateTimeException {
		station.insertDate(2007, 3, 24, null);
	}

	@Test public void latitudeDefaultValue() {
		assertNull("Latitude: default value", station.getLatitude());
	}

	@Test public void latitudeDegreeDirectionLowerBoundary() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLatitude(-90.0, NORTH);
		assertEquals("Latitude: -90 N", -90.0, station.getLatitude(), 
				THRESHOLD);
		station.setLatitude(90.0, SOUTH);
		assertEquals("Latitude:  90 S", -90.0, station.getLatitude(), 
				THRESHOLD);
	}

	@Test (expected = InvalidValueWarning.class)
	public void latitudeDegreeDirectionLowerBoundaryViolationNorth() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLatitude(-90.1, NORTH);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void latitudeDegreeDirectionLowerBoundaryViolationSouth() throws
	ConversionException, InvalidValueException, InvalidValueWarning,
	RestrictedOperationException {
		station.setLatitude(90.1, SOUTH);
	}
	
	@Test public void latitudeDegreeDirectionNullDegrees() throws 
	ConversionException, InvalidValueException, InvalidValueWarning,
	RestrictedOperationException {
		station.setLatitude(null, NORTH);
		assertNull("Latitude: null N", station.getLatitude());
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDegreeDirectionNullDirection() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLatitude(30.0, (LatitudeUnit)null);
	}
	
	@Test public void latitudeDegreeDirectionUpperBoundary() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLatitude(90.0, NORTH);
		assertEquals("Latitude:  90 N",  90.0, station.getLatitude(), 
				THRESHOLD);
		station.setLatitude(-90.0, SOUTH);
		assertEquals("Latitude: -90 S",  90.0, station.getLatitude(), 
				THRESHOLD);
	}

	@Test (expected = InvalidValueWarning.class)
	public void latitudeDegreeDirectionUpperBoundaryViolationNorth() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLatitude(90.1, NORTH);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void latitudeDegreeDirectionUpperBoundaryViolationSouth() throws 
	ConversionException, InvalidValueException, InvalidValueWarning,
	RestrictedOperationException {
		station.setLatitude(-90.1, SOUTH);
	}
	
	@Test public void latitudeDegreeLowerBoundary() throws ConversionException, 
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		station.setLatitude(-90.0);
		assertEquals("Latitude: -90", -90.0, station.getLatitude(), THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void latitudeDegreeLowerBoundaryViolation() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLatitude(-90.1);
	}
	
	@Test public void latitudeDegreeNullDegrees() throws ConversionException, 
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		station.setLatitude(null);
		assertNull("Latitude: null", station.getLatitude());
	}
	
	@Test public void latitudeDegreeUpperBoundary() throws ConversionException,
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		station.setLatitude(90.0);
		assertEquals("Latitude:  90",  90.0, station.getLatitude(), THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void latitudeDegreeUpperBoundaryViolation() throws 
	ConversionException, InvalidValueException, InvalidValueWarning,
	RestrictedOperationException {
		station.setLatitude(90.1);
	}
	
	@Test public void latitudeDMDirectionLowerBoundary() throws 
	ConversionException, InvalidValueException, InvalidValueWarning,
	RestrictedOperationException {
		station.setLatitude(-90.0, 0.0, NORTH);
		assertEquals("Latitude: -90 00 N", -90.0, station.getLatitude(), 
				THRESHOLD);
		station.setLatitude(90.0, 0.0, SOUTH);
		assertEquals("Latitude:  90 00 S", -90.0, station.getLatitude(), 
				THRESHOLD);
	}

	@Test (expected = InvalidValueWarning.class)
	public void latitudeDMDirectionLowerBoundaryViolationNorth() throws 
	ConversionException, InvalidValueException, InvalidValueWarning,
	RestrictedOperationException {
		station.setLatitude(-90.0, 0.1, NORTH);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void latitudeDMDirectionLowerBoundaryViolationSouth() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLatitude(90.0, 0.1, SOUTH);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMDirectionMinuteLowerBoundaryViolation() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLatitude(30.0, -0.1, NORTH);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMDirectionMinuteUpperBoundaryViolation() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {	
		station.setLatitude(30.0, 60.1, NORTH);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMDirectionNullDegrees() throws ConversionException, 
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		station.setLatitude(null, 30.0, NORTH);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMDirectionNullDirection() throws ConversionException, 
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		station.setLatitude(30.0, 30.0, (LatitudeUnit)null);
	}
		
	@Test (expected = ConversionException.class)
	public void latitudeDMDirectionNullMinutes() throws ConversionException,
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		station.setLatitude(30.0, null, NORTH);
	}
	
	@Test public void latitudeDMDirectionUpperBoundary() throws 
	ConversionException, InvalidValueException, InvalidValueWarning,
	RestrictedOperationException {
		station.setLatitude(90.0, 0.0, NORTH);
		assertEquals("Latitude:  90 00 N",  90.0, station.getLatitude(), 
				THRESHOLD);
		station.setLatitude(-90.0, 0.0, SOUTH);
		assertEquals("Latitude: -90 00 S",  90.0, station.getLatitude(), 
				THRESHOLD);
	}

	@Test (expected = InvalidValueWarning.class)
	public void latitudeDMDirectionUpperBoundaryViolationNorth() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLatitude(90.0, 0.1, NORTH);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void latitudeDMDirectionUpperBoundaryViolationSouth() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLatitude(-90.0, 0.1, SOUTH);
	}
	
	@Test public void latitudeDMLowerBoundary() throws ConversionException, 
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		station.setLatitude(-90.0, 0.0);
		assertEquals("Latitude: -90 00", -90.0, station.getLatitude(), 
				THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void latitudeDMLowerBoundaryViolation() throws ConversionException,
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		station.setLatitude(-90.0, 0.1);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMMinuteLowerBoundaryViolation() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLatitude(30.0, -0.1);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMMinuteUpperBoundaryViolation() throws 
	ConversionException, InvalidValueException, InvalidValueWarning,
	RestrictedOperationException {
		station.setLatitude(30.0, 60.1);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMNullDegrees() throws ConversionException, 
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		station.setLatitude(null, 30.0);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMNullMinutes() throws ConversionException, 
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		station.setLatitude(30.0, (Double)null);
	}
	
	@Test public void latitudeDMSDirectionLowerBoundary() throws 
	ConversionException, InvalidValueException, InvalidValueWarning,
	RestrictedOperationException {
		station.setLatitude(-90.0, 0.0, 0.0, NORTH);
		assertEquals("Latitude: -90 00 00 N", -90.0, station.getLatitude(), 
				THRESHOLD);
		station.setLatitude(90.0, 0.0, 0.0, SOUTH);
		assertEquals("Latitude:  90 00 00 S", -90.0, station.getLatitude(), 
				THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void latitudeDMSDirectionLowerBoundaryViolationNorth() throws 
	ConversionException, InvalidValueException, InvalidValueWarning,
	RestrictedOperationException {
		station.setLatitude(-90.0, 0.0, 0.1, NORTH);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void latitudeDMSDirectionLowerBoundaryViolationSouth() throws 
	ConversionException, InvalidValueException, InvalidValueWarning,
	RestrictedOperationException {
		station.setLatitude(90.0, 0.0, 0.1, SOUTH);
	}

	@Test (expected = ConversionException.class)
	public void latitudeDMSDirectionMinuteLowerBoundaryViolation() throws 
	ConversionException, InvalidValueException, InvalidValueWarning,
	RestrictedOperationException {
		station.setLatitude(30.0, -0.1, 30.0, NORTH);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMSDirectionMinuteUpperBoundaryViolation() throws 
	ConversionException, InvalidValueException, InvalidValueWarning,
	RestrictedOperationException {
		station.setLatitude(30.0, 60.1, 30.0, NORTH);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMSDirectionNullDegrees() throws ConversionException,
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		station.setLatitude(null, 30.0, 30.0, NORTH);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMSDirectionNullDirection() throws ConversionException, 
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		station.setLatitude(30.0, 30.0, 30.0, null);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMSDirectionNullMinutes() throws ConversionException, 
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		station.setLatitude(30.0, null, 30.0, NORTH);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMSDirectionNullSeconds() throws ConversionException, 
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		station.setLatitude(30.0, 30.0, null, NORTH);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMSDirectionSecondLowerBoundaryViolation() throws 
	ConversionException, InvalidValueException, InvalidValueWarning,
	RestrictedOperationException {
		station.setLatitude(30.0, 30.0, -0.1, NORTH);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMSDirectionSecondUpperBoundaryViolation() throws 
	ConversionException, InvalidValueException, InvalidValueWarning,
	RestrictedOperationException {
		station.setLatitude(30.0, 30.0, 60.1, NORTH);
	}
	
	@Test public void latitudeDMSDirectionUpperBoundary() throws 
	ConversionException, InvalidValueException, InvalidValueWarning,
	RestrictedOperationException {
		station.setLatitude(90.0, 0.0, 0.0, NORTH);
		assertEquals("Latitude:  90 00 00 N",  90.0, station.getLatitude(), 
				THRESHOLD);
		station.setLatitude(-90.0, 0.0, 0.0, SOUTH);
		assertEquals("Latitude: -90 00 00 S",  90.0, station.getLatitude(), 
				THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void latitudeDMSDirectionUpperBoundaryViolationNorth() throws 
	ConversionException, InvalidValueException, InvalidValueWarning,
	RestrictedOperationException {
		station.setLatitude(90.0, 0.0, 0.1, NORTH);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void latitudeDMSDirectionUpperBoundaryViolationSouth() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLatitude(-90.0, 0.0, 0.1, SOUTH);
	}
		
	@Test public void latitudeDMSLowerBoundary() throws ConversionException,
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		station.setLatitude(-90.0, 0.0, 0.0);
		assertEquals("Latitude: -90 00 00", -90.0, station.getLatitude(), 
				THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void latitudeDMSLowerBoundaryViolation() throws ConversionException, 
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		station.setLatitude(-90.0, 0.0, 0.1);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMSMinuteLowerBoundaryViolation() throws 
	ConversionException, InvalidValueException, InvalidValueWarning,
	RestrictedOperationException {
		station.setLatitude(30.0, -0.1, 30.0);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMSMinuteUpperBoundaryViolation() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLatitude(30.0, 60.1, 30.0);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMSNullDegrees() throws ConversionException, 
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		station.setLatitude(null, 30.0, 30.0);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMSNullMinutes() throws ConversionException, 
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		station.setLatitude(30.0, null, 30.0);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMSNullSeconds() throws ConversionException, 
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		station.setLatitude(30.0, 30.0, (Double)null);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMSSecondLowerBoundaryViolation() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLatitude(30.0, 30.0, -0.1);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMSSecondUpperBoundaryViolation() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLatitude(30.0, 30.0, 60.1);
	}
	
	@Test public void latitudeDMSUpperBoundary() throws ConversionException, 
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		station.setLatitude(90.0, 0.0, 0.0);
		assertEquals("Latitude:  90 00 00",  90.0, station.getLatitude(), 
				THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void latitudeDMSUpperBoundaryViolationNorth() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLatitude(90.0, 0.0, 0.1);
	}
	
	@Test public void latitudeDMUpperBoundary() throws ConversionException, 
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		station.setLatitude(90.0, 0.0);
		assertEquals("Latitude:  90 00",  90.0, station.getLatitude(), 
				THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void latitudeDMUpperBoundaryViolation() throws ConversionException, 
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		station.setLatitude(90.0, 0.1);
	}
	
	@Test public void locationAccuracy() throws InvalidValueException {
		assertEquals("Accuracy: default", 0, station.getLocationAccuracy());
		station.setLocationAccuracy(3);
		assertEquals("Accuracy: value", 3, station.getLocationAccuracy());
		station.setLocationAccuracy(0);
		assertEquals("Accuracy: lower bound", 0, station.getLocationAccuracy());
		station.setLocationAccuracy(9);
		assertEquals("Accuracy: upper bound", 9, station.getLocationAccuracy());
	}
	
	@Test (expected = InvalidValueException.class)
	public void locationAccuracyLowerBoundaryViolation() throws 
	InvalidValueException {
		station.setLocationAccuracy(-1);
	}
	
	@Test (expected = InvalidValueException.class)
	public void locationAccuracyUpperBoundaryViolation() throws 
	InvalidValueException {
		station.setLocationAccuracy(10);
	}

	@Test public void longitudeDefaultValue() {
		assertNull("Longitude: default value", station.getLongitude());
	}

	@Test public void longitudeDegreeDirectionLowerBoundary() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLongitude(-180.0, EAST);
		assertEquals("Longitude: -180 E", -180.0, station.getLongitude(), 
				THRESHOLD);
		station.setLongitude(180.0, WEST);
		assertEquals("Longitude:  180 W", -180.0, station.getLongitude(), 
				THRESHOLD);
	}

	@Test (expected = InvalidValueWarning.class)
	public void longitudeDegreeDirectionLowerBoundaryViolationNorth() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLongitude(-180.1, EAST);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void longitudeDegreeDirectionLowerBoundaryViolationSouth() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLongitude(180.1, WEST);
	}
	
	@Test public void longitudeDegreeDirectionNullDegrees() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLongitude(null, EAST);
		assertNull("Longitude: null E", station.getLongitude());
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDegreeDirectionNullDirection() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLongitude(30.0, (LongitudeUnit)null);
	}
	
	@Test public void longitudeDegreeDirectionUpperBoundary() throws
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLongitude(180.0, EAST);
		assertEquals("Longitude:  180 E",  180.0, station.getLongitude(), 
				THRESHOLD);
		station.setLongitude(-180.0, WEST);
		assertEquals("Longitude: -180 W",  180.0, station.getLongitude(), 
				THRESHOLD);
	}

	@Test (expected = InvalidValueWarning.class)
	public void longitudeDegreeDirectionUpperBoundaryViolationNorth() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLongitude(180.1, EAST);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void longitudeDegreeDirectionUpperBoundaryViolationSouth() 
	throws ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLongitude(-180.1, WEST);
	}
	
	@Test public void longitudeDegreeLowerBoundary() throws 
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		station.setLongitude(-180.0);
		assertEquals("Longitude: -180", -180.0, station.getLongitude(), 
				THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void longitudeDegreeLowerBoundaryViolation() throws 
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		station.setLongitude(-180.1);
	}
	
	@Test public void longitudeDegreeNullDegrees() throws InvalidValueException,
	InvalidValueWarning, RestrictedOperationException {
		station.setLongitude(null);
		assertNull("Longitude: null", station.getLongitude());
	}
	
	@Test public void longitudeDegreeUpperBoundary() throws 
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		station.setLongitude(180.0);
		assertEquals("Longitude:  180",  180.0, station.getLongitude(), 
				THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void longitudeDegreeUpperBoundaryViolation() throws 
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		station.setLongitude(180.1);
	}
	
	@Test public void longitudeDMDirectionLowerBoundary() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLongitude(-180.0, 0.0, EAST);
		assertEquals("Longitude: -180 00 E", -180.0, station.getLongitude(),
				THRESHOLD);
		station.setLongitude(180.0, 0.0, WEST);
		assertEquals("Longitude:  180 00 W", -180.0, station.getLongitude(), 
				THRESHOLD);
	}

	@Test (expected = InvalidValueWarning.class)
	public void longitudeDMDirectionLowerBoundaryViolationNorth() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLongitude(-180.0, 0.1, EAST);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void longitudeDMDirectionLowerBoundaryViolationSouth() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLongitude(180.0, 0.1, WEST);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMDirectionMinuteLowerBoundaryViolation() throws
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLongitude(30.0, -0.1, EAST);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMDirectionMinuteUpperBoundaryViolation() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLongitude(30.0, 60.1, EAST);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMDirectionNullDegrees() throws ConversionException, 
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		station.setLongitude(null, 30.0, EAST);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMDirectionNullDirection() throws ConversionException, 
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		station.setLongitude(30.0, 30.0, (LongitudeUnit)null);
	}
		
	@Test (expected = ConversionException.class)
	public void longitudeDMDirectionNullMinutes() throws ConversionException, 
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		station.setLongitude(30.0, null, EAST);
	}
	
	@Test public void longitudeDMDirectionUpperBoundary() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLongitude(180.0, 0.0, EAST);
		assertEquals("Longitude:  180 00 E",  180.0, station.getLongitude(), 
				THRESHOLD);
		station.setLongitude(-180.0, 0.0, WEST);
		assertEquals("Longitude: -180 00 W",  180.0, station.getLongitude(),
				THRESHOLD);
	}

	@Test (expected = InvalidValueWarning.class)
	public void longitudeDMDirectionUpperBoundaryViolationNorth() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLongitude(180.0, 0.1, EAST);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void longitudeDMDirectionUpperBoundaryViolationSouth() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLongitude(-180.0, 0.1, WEST);
	}
	
	@Test public void longitudeDMLowerBoundary() throws ConversionException, 
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		station.setLongitude(-180.0, 0.0);
		assertEquals("Longitude: -180 00", -180.0, station.getLongitude(), 
				THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void longitudeDMLowerBoundaryViolation() throws ConversionException, 
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		station.setLongitude(-180.0, 0.1);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMMinuteLowerBoundaryViolation() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLongitude(30.0, -0.1);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMMinuteUpperBoundaryViolation() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLongitude(30.0, 60.1);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMNullDegrees() throws ConversionException,
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		station.setLongitude(null, 30.0);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMNullMinutes() throws ConversionException, 
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		station.setLongitude(30.0, (Double)null);
	}
	
	@Test public void longitudeDMSDirectionLowerBoundary() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLongitude(-180.0, 0.0, 0.0, EAST);
		assertEquals("Longitude: -180 00 00 E", -180.0, station.getLongitude(), 
				THRESHOLD);
		station.setLongitude(180.0, 0.0, 0.0, WEST);
		assertEquals("Longitude:  180 00 00 W", -180.0, station.getLongitude(),
				THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void longitudeDMSDirectionLowerBoundaryViolationNorth() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLongitude(-180.0, 0.0, 0.1, EAST);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void longitudeDMSDirectionLowerBoundaryViolationSouth() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLongitude(180.0, 0.0, 0.1, WEST);
	}

	@Test (expected = ConversionException.class)
	public void longitudeDMSDirectionMinuteLowerBoundaryViolation() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLongitude(30.0, -0.1, 30.0, EAST);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMSDirectionMinuteUpperBoundaryViolation() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLongitude(30.0, 60.1, 30.0, EAST);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMSDirectionNullDegrees() throws ConversionException,
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		station.setLongitude(null, 30.0, 30.0, EAST);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMSDirectionNullDirection() throws ConversionException,
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		station.setLongitude(30.0, 30.0, 30.0, null);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMSDirectionNullMinutes() throws ConversionException,
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		station.setLongitude(30.0, null, 30.0, EAST);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMSDirectionNullSeconds() throws ConversionException, 
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		station.setLongitude(30.0, 30.0, null, EAST);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMSDirectionSecondLowerBoundaryViolation() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLongitude(30.0, 30.0, -0.1, EAST);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMSDirectionSecondUpperBoundaryViolation() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLongitude(30.0, 30.0, 60.1, EAST);
	}
	
	@Test public void longitudeDMSDirectionUpperBoundary() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLongitude(180.0, 0.0, 0.0, EAST);
		assertEquals("Longitude:  180 00 00 E",  180.0, station.getLongitude(), 
				THRESHOLD);
		station.setLongitude(-180.0, 0.0, 0.0, WEST);
		assertEquals("Longitude: -180 00 00 W",  180.0, station.getLongitude(),
				THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void longitudeDMSDirectionUpperBoundaryViolationNorth() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLongitude(180.0, 0.0, 0.1, EAST);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void longitudeDMSDirectionUpperBoundaryViolationSouth() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLongitude(-180.0, 0.0, 0.1, WEST);
	}
		
	@Test public void longitudeDMSLowerBoundary() throws ConversionException, 
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		station.setLongitude(-180.0, 0.0, 0.0);
		assertEquals("Longitude: -180 00 00", -180.0, station.getLongitude(), 
				THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void longitudeDMSLowerBoundaryViolation() throws ConversionException,
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		station.setLongitude(-180.0, 0.0, 0.1);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMSMinuteLowerBoundaryViolation() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLongitude(30.0, -0.1, 30.0);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMSMinuteUpperBoundaryViolation() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLongitude(30.0, 60.1, 30.0);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMSNullDegrees() throws ConversionException, 
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		station.setLongitude(null, 30.0, 30.0);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMSNullMinutes() throws ConversionException, 
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		station.setLongitude(30.0, null, 30.0);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMSNullSeconds() throws ConversionException, 
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		station.setLongitude(30.0, 30.0, (Double)null);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMSSecondLowerBoundaryViolation() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLongitude(30.0, 30.0, -0.1);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMSSecondUpperBoundaryViolation() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLongitude(30.0, 30.0, 60.1);
	}
	
	@Test public void longitudeDMSUpperBoundary() throws ConversionException, 
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		station.setLongitude(180.0, 0.0, 0.0);
		assertEquals("Longitude:  180 00 00",  180.0, station.getLongitude(), 
				THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void longitudeDMSUpperBoundaryViolationNorth() throws 
	ConversionException, InvalidValueException, InvalidValueWarning, 
	RestrictedOperationException {
		station.setLongitude(180.0, 0.0, 0.1);
	}
	
	@Test public void longitudeDMUpperBoundary() throws ConversionException, 
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		station.setLongitude(180.0, 0.0);
		assertEquals("Longitude:  180 00",  180.0, station.getLongitude(), 
				THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void longitudeDMUpperBoundaryViolation() throws ConversionException, 
	InvalidValueException, InvalidValueWarning, RestrictedOperationException {
		station.setLongitude(180.0, 0.1);
	}
	
	@Test public void mobile() {
		assertFalse("Mobile: default",station.isMobile());
		station.setMobile(true);
		assertTrue("Mobile: true",station.isMobile());
		station.setMobile(false);
		assertFalse("Mobile: false",station.isMobile());
	}
	
	@Test public void networkId() throws InvalidValueException {
		assertEquals("Network Id: default value", 99, station.getNetworkId());
		station.setNetworkId(0);
		assertEquals("Network Id: lower boundary", 0, station.getNetworkId());
		station.setNetworkId(9999);
		assertEquals("Network Id: upper boundary", 9999, station.getNetworkId());
	}
	
	@Test (expected = InvalidValueException.class)
	public void networkIdLowerBoundaryViolation() throws InvalidValueException {
		station.setNetworkId(-1);
	}
	
	@Test (expected = InvalidValueException.class)
	public void networkIdUpperBoundaryViolation() throws InvalidValueException {
		station.setNetworkId(10000);
	}
	
	@Test public void networkName() throws InvalidValueException, 
	RestrictedOperationException {
		assertEquals("Network Name: default value", "Network", 
				station.getNetworkName());
		station.setNetworkName("NewNet");
		assertEquals("Network Name: new value", "NewNet", 
				station.getNetworkName());
	}
	
	@Test (expected = InvalidValueException.class)
	public void networkNameNullName() throws InvalidValueException, 
	RestrictedOperationException {
		station.setNetworkName(null);
	}
	
	@Test public void occurence() throws InvalidValueException {
		assertEquals("Occurence: default", 0, station.getOccurence());
		station.setOccurence(999);
		assertEquals("Occurence: upper bound", 999, station.getOccurence());
		station.setOccurence(0);
		assertEquals("Occurence: lower bound", 0, station.getOccurence());
	}
	
	@Test (expected = InvalidValueException.class)
	public void occurenceLowerBoundViolation() throws InvalidValueException {
		station.setOccurence(-1);
	}
	
	@Test (expected = InvalidValueException.class)
	public void occurenceUpperBoundViolation() throws InvalidValueException {
		station.setOccurence(1000);
	}
	
	@Test public void platform() throws InvalidValueException {
		assertEquals("Platform: default", 999, station.getPlatform());
		station.setPlatform(0);
		assertEquals("Platform: lower bound", 0, station.getPlatform());
		station.setPlatform(9999);
		assertEquals("Platform: upper bound", 9999, station.getPlatform());
	}
	
	@Test (expected = InvalidValueException.class)
	public void platformLowerBoundaryViolation() throws InvalidValueException {
		station.setPlatform(-1);
	}
	
	@Test (expected = InvalidValueException.class)
	public void platformUpperBoundaryViolation() throws InvalidValueException {
		station.setPlatform(10000);
	}
	
	@Test public void stateCountry() throws ConversionException, IOException {
		assertEquals("Country: default", "XX", station.getCountry());
		assertEquals("State: default", 99, station.getStateCode());
		
		station.setState("US", "CO");
		assertEquals("Country: value", "US", station.getCountry());
		assertEquals("State: value", 8, station.getStateCode());
	}
	
	@Test (expected = ConversionException.class)
	public void stateCountryNullCountry() throws ConversionException,
	IOException {
		station.setState(null, "CO");
	}
	
	@Test (expected = ConversionException.class)
	public void stateCountryNullState() throws ConversionException, 
	IOException {
		station.setState("US", null);
	}
	
	@Test (expected = ConversionException.class)
	public void stateCountryUnknownCountry() throws ConversionException,
	IOException {
		station.setState("Asia", "CO");
	}
	
	@Test (expected = ConversionException.class)
	public void stateCountryUnknownState() throws ConversionException,
	IOException {
		station.setState("US", "Asia");
	}
	
	@Test public void stationId() throws InvalidValueException, 
	RestrictedOperationException {
		assertEquals("Station Id: default value", "Station",
				station.getStationId());
		station.setStationId("NewId");
		assertEquals("Station Id: new value", "NewId",
				station.getStationId());
		station.setStationId("123456789012345");
		assertEquals("Station Id: max characters", "123456789012345",
				station.getStationId());
	}
	
	@Test (expected = InvalidValueException.class)
	public void stationIdCharacterOverflow() throws InvalidValueException, 
	RestrictedOperationException {
		station.setStationId("1234567890123456");
	}
	
	@Test (expected = InvalidValueException.class)
	public void stationIdNullId() throws InvalidValueException, 
	RestrictedOperationException {
		station.setStationId(null);
	}
	
	@Test public void tostring() throws ConversionException, DateTimeException,
	InvalidValueException, InvalidValueWarning, IOException, 
	RestrictedOperationException {
		assertEquals("toString: default","Station           99  -99.99999  -999.99999   0     0 Default Station Description                    (N) 99999999 99999999 XX 99 ???   0.00 n  999 xxxxxx            -9999.9 f",station.toString());

		// Build a new station to test
		station.setBeginDate(2007, 1, 1, UTC);
		station.setCommissioned(true);
		station.setDaylightSavingsUsed(true);
		station.setDescription("New Station Information");
		station.setElevation(1111.1, METERS);
		station.setEndDate(2007, 3, 31, UTC);
		station.setFrequency("hourly");
		station.setLatitude(11.111);
		station.setLocationAccuracy(2);
		station.setLongitude(-111.111);
		station.setMobile(true);
		station.setNetworkId(15);
		station.setNetworkName("NewNet");
		station.setOccurence(1);
		station.setPlatform(111);
		station.setState("US", "CO");
		station.setStationId("COstn1");
		station.setUTCoffset(-7);			
		assertEquals("toString: values","COstn1            15   11.11100  -111.11100   1     2 New Station Information                        (Y) 20070101 20070331 US 08 ???  -7.00 y  111 hourly             1111.1 m",station.toString());
	}
	
	@Test public void utcOffset() throws InvalidValueException {
		assertEquals("UTC: default", 0.0, station.getUTCoffset(), .01);
		station.setUTCoffset(-99.0);
		assertEquals("UTC: lower bound", -99.0, station.getUTCoffset(), .01);
		station.setUTCoffset(99.0);
		assertEquals("UTC: upper bound", 99.0, station.getUTCoffset(), .01);
		station.setUTCoffset(7.25);
		assertEquals("UTC: decimal", 7.25, station.getUTCoffset(), .01);
	}
	
	@Test (expected = InvalidValueException.class)
	public void utcOffsetLowerBoundViolation() throws InvalidValueException {
		station.setUTCoffset(-99.01);
	}

	@Test (expected = InvalidValueException.class)
	public void utcOffsetUpperBoundViolation() throws InvalidValueException {
		station.setUTCoffset(99.01);
	}
}
