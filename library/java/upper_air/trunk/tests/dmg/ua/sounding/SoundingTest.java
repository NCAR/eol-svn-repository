package dmg.ua.sounding;

import static dmg.util.LengthUtils.*;
import static dmg.util.PositionUtils.*;
import static dmg.util.TimeUtils.*;
import static org.junit.Assert.*;
import dmg.station.*;
import dmg.util.*;
import dmg.util.PositionUtils.*;

import java.util.*;

import org.junit.*;


public abstract class SoundingTest<T extends Sounding<?>> {

	protected static final Double THRESHOLD = Math.pow(1, -10);
	
	private Calendar december, march;
	private Station nullStation;
	protected T sounding, nullStationSounding, stationSounding;
	
	public abstract T newInstance(Station station);
	
	@Before public void setup() throws Exception {
		sounding = newInstance(null);
		march = buildDate(2007, 3, 23, UTC);
		december = buildDate(2007, 12, 31, UTC);
		
		nullStation = new Station("NULL", "NULL");
		nullStationSounding = newInstance(nullStation);
		
		Station station = new Station("NW1", "NW");
		station.setDescription("Northwest Station");
		station.setLatitude(10.0);
		station.setLongitude(-100.0);
		station.setElevation(10.0, METERS);
		stationSounding = newInstance(station);
	}
	
	@Test public void actualRelease() throws DateTimeException  {
		assertNull("Actual Release: default value", sounding.getActualDate());
		sounding.setActualRelease(buildDate(2007, 3, 23, UTC));
		assertEquals("Actual Release: calendar", march, 
				sounding.getActualDate());
		sounding.setActualRelease(null);
		assertNull("Actual Release: null", sounding.getActualDate());
		sounding.setActualRelease(2007, 3, 23, 0, 0, 0, UTC);
		assertEquals("Actual Release: YYYY, MM, DD, tz", march, 
				sounding.getActualDate());
		sounding.setActualRelease(2007, 365, 0, 0, 0, UTC);
		assertEquals("Actual Release: YYYY, JJJ, tz", december,
				sounding.getActualDate());
		sounding.setActualRelease(2007, 3, 23, 0, 0, 0, MOUNTAIN_STANDARD_TIME);
		assertEquals("Actual Release: different time zone", 
				buildDate(2007, 3, 23, 7, 0, 0, 0, UTC), 
				sounding.getActualDate());
	}
	
	@Test (expected = DateTimeException.class)
	public void actualReleaseDayLowerBoundaryViolation() 
	throws DateTimeException {
		sounding.setActualRelease(2007, 1, 0, 0, 0, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void actualReleaseDayUpperBoundaryViolation() throws 
	DateTimeException {
		sounding.setActualRelease(2007, 1, 32, 0, 0, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void actualReleaseHourLowerBoundaryViolation() throws 
	DateTimeException {
		sounding.setActualRelease(2007, 1, 1, -1, 0, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void actualReleaseHourUpperBoundaryViolation() throws 
	DateTimeException {
		sounding.setActualRelease(2007, 1, 1, 25, 0, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void actualReleaseJulianLowerBoundaryViolation() throws 
	DateTimeException {
		sounding.setActualRelease(2007, 0, 0, 0, 0, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void actualReleaseJulianNullTimeZone() throws DateTimeException {
		sounding.setActualRelease(2007, 1, 0, 0, 0, 0, null);
	}
	
	@Test (expected = DateTimeException.class)
	public void actualReleaseJulianUpperBoundaryViolation() throws 
	DateTimeException {
		sounding.setActualRelease(2007, 366, 0, 0, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void actualReleaseMinuteLowerBoundaryViolation() throws 
	DateTimeException {
		sounding.setActualRelease(2007, 1, 1, 0, -1, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void actualReleaseMinuteUpperBoundaryViolation() throws 
	DateTimeException {
		sounding.setActualRelease(2007, 1, 1, 0, 61, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void actualReleaseMonthLowerBoundaryViolation() throws 
	DateTimeException {
		sounding.setActualRelease(2007, 0, 1, 0, 0, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void actualReleaseMonthUpperBoundaryViolation() throws
	DateTimeException {
		sounding.setActualRelease(2007, 13, 1, 0, 0, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void actualReleaseNullTimeZone() throws DateTimeException {
		sounding.setActualRelease(2007, 1, 1, 0, 0, 0, null);
	}
	
	@Test (expected = DateTimeException.class)
	public void actualReleaseSecondLowerBoundaryViolation() throws
	DateTimeException {
		sounding.setActualRelease(2007, 1, 1, 0, 0, -1, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void actualReleaseSecondUpperBoundaryViolation() throws
	DateTimeException {
		sounding.setActualRelease(2007, 1, 1, 0, 0, 61, UTC);
	}
	
	@Test public void altitude() throws ConversionException,
	InvalidValueException, RestrictedOperationException {
		assertNull("Altitude: default value", sounding.getAltitude());
		sounding.setAltitude(10.0, METERS);
		assertEquals("Altitude: new METERS value", 10.0, sounding.getAltitude(),
				THRESHOLD);
		sounding.setAltitude(100.0, KILOMETERS);
		assertEquals("Altitude: new km value", 100000.0, sounding.getAltitude(),
				THRESHOLD);
		sounding.setAltitude(null, METERS);
		assertNull("Altitude: null", sounding.getAltitude());
		
		assertEquals("Altitude: set station", 10.0, 
				stationSounding.getAltitude());
		
		assertNull("Altitude: null station", nullStationSounding.getAltitude());
		nullStation.setElevation(234.0, METERS);
		assertEquals("Altitude: null station value", 234.0, 
				nullStationSounding.getAltitude());
		
		nullStationSounding.setAltitude(111.11, METERS);
		assertEquals("Altitude: manual override", 111.11, 
				nullStationSounding.getAltitude());
		nullStationSounding.setAltitude(null, METERS);
		assertEquals("Altitude: manual reset", 234.0, 
				nullStationSounding.getAltitude());
	}
	
	@Test (expected = ConversionException.class)
	public void altitudeNullUnit() throws ConversionException  {
		sounding.setAltitude(20.0, null);
	}
	
	@Test public void compare() throws DateTimeException, InvalidValueWarning {
		T first = newInstance(null);
		T second = newInstance(null);
		
		assertTrue("Compare: all default", first.compareTo(second) == 0);
		assertTrue("Compare: all default reflexive", 
				second.compareTo(first) == 0);
		
		// Test on the data types (fifth comparison)
		first.setDataType("Type1");
		assertTrue("Compare: (type) value null", first.compareTo(second) > 0);
		assertTrue("Compare: (type) null value", second.compareTo(first) < 0);
		second.setDataType("Type");
		assertTrue("Compare: (type) value value", first.compareTo(second) > 0);
		
		// Test on the station id (fourth comparison)
		first.setStationId("Id");
		assertTrue("Compare: (id) value null", first.compareTo(second) > 0);
		assertTrue("Compare: (id) null value", second.compareTo(first) < 0);
		second.setStationId("Id");
		assertTrue("Compare: (id) equal value (use type)", 
				first.compareTo(second) > 0);
		second.setStationId("Id1");
		assertTrue("Compare: (id) value value", first.compareTo(second) < 0);
		
		// Test on the longitude (third comparison)
		first.setLongitude(0.0);
		assertTrue("Compare: (lon) value null", first.compareTo(second) > 0);
		assertTrue("Compare: (lon) null value", second.compareTo(first) < 0);
		second.setLongitude(0.0);
		assertTrue("Compare: (lon) equal value (use id)", 
				first.compareTo(second) < 0);
		second.setLongitude(-10.0);
		assertTrue("Compare: (lon) value value", first.compareTo(second) > 0);
		
		// Test on the latitude (second comparision)
		first.setLatitude(0.0);
		assertTrue("Compare: (lat) value null", first.compareTo(second) > 0);
		assertTrue("Compare: (lat) null value", second.compareTo(first) < 0);
		second.setLatitude(0.0);
		assertTrue("Compare: (lat) equal value (use lon)", 
				first.compareTo(second) > 0);
		second.setLatitude(10.0);
		assertTrue("Compare: (lat) value value", first.compareTo(second) < 0);
		
		// Test on the nominal date (first comparison)
		first.setNominalRelease(2007, 3, 28, 0, 0, 0, UTC);
		assertTrue("Compare: (date) value null", first.compareTo(second) > 0);
		assertTrue("Compare: (date) null value", second.compareTo(first) < 0);
		second.setNominalRelease(2007, 3, 28, 0, 0, 0, UTC);
		assertTrue("Compare: (date) equal value (use lat)", 
				first.compareTo(second) < 0);
		second.setNominalRelease(2006, 3, 28, 0, 0, 0, UTC);
		assertTrue("Compare: (date) value value", first.compareTo(second) > 0);
	}
	
	@Test public void dataType() {
		assertNull("Data Type: default", sounding.getDataType());
		sounding.setDataType("NWS Sounding");
		assertEquals("Data Type: value", "NWS Sounding", 
				sounding.getDataType());
		sounding.setDataType(null);
		assertNull("Data Type: null", sounding.getDataType());
	}
	
	@Test public void headerLine() throws InvalidValueException {
		assertNull("Header Line: default", sounding.getHeaderLine(1));
		sounding.setHeaderLine(1, "Label:", "Content");
		assertEquals("Header Line: label", "Label:", 
				sounding.getHeaderLine(1).getLabel());
		assertEquals("Header Line: content", "Content", 
				sounding.getHeaderLine(1).getContent());
		sounding.setHeaderLine(1, "Label:", null);
		assertEquals("Header Line: label", "Label:", 
				sounding.getHeaderLine(1).getLabel());
		assertNull("Header Line: content null", 
				sounding.getHeaderLine(1).getContent());
		
		sounding.clearHeaderLine(1);
		assertNull("Header Line: cleared", sounding.getHeaderLine(1));
	}
	
	@Test (expected = InvalidValueException.class)
	public void headerLineNullLabel() throws InvalidValueException {
		sounding.setHeaderLine(1, null, "content");
	}
	
	@Test public void latitudeDefaultValue() {
		assertNull("Latitude: default value", sounding.getLatitude());
	}

	@Test public void latitudeDegreeDirectionLowerBoundary() throws
	ConversionException, InvalidValueWarning {
		sounding.setLatitude(-90.0, NORTH);
		assertEquals("Latitude: -90 N", -90.0, sounding.getLatitude(),
				THRESHOLD);
		sounding.setLatitude(90.0, SOUTH);
		assertEquals("Latitude:  90 S", -90.0, sounding.getLatitude(), 
				THRESHOLD);
	}

	@Test (expected = InvalidValueWarning.class)
	public void latitudeDegreeDirectionLowerBoundaryViolationNorth() throws
	ConversionException, InvalidValueWarning {
		sounding.setLatitude(-90.1, NORTH);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void latitudeDegreeDirectionLowerBoundaryViolationSouth() throws
	ConversionException, InvalidValueWarning {
		sounding.setLatitude(90.1, SOUTH);
	}
	
	@Test public void latitudeDegreeDirectionNullDegrees() throws
	ConversionException, InvalidValueWarning {
		sounding.setLatitude(null, NORTH);
		assertNull("Latitude: null N", sounding.getLatitude());
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDegreeDirectionNullDirection() throws
	ConversionException, InvalidValueWarning {
		sounding.setLatitude(30.0, (LatitudeUnit)null);
	}
	
	@Test public void latitudeDegreeDirectionUpperBoundary() throws
	ConversionException, InvalidValueWarning {
		sounding.setLatitude(90.0, NORTH);
		assertEquals("Latitude:  90 N",  90.0, sounding.getLatitude(),
				THRESHOLD);
		sounding.setLatitude(-90.0, SOUTH);
		assertEquals("Latitude: -90 S",  90.0, sounding.getLatitude(), 
				THRESHOLD);
	}

	@Test (expected = InvalidValueWarning.class)
	public void latitudeDegreeDirectionUpperBoundaryViolationNorth() throws
	ConversionException, InvalidValueWarning {
		sounding.setLatitude(90.1, NORTH);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void latitudeDegreeDirectionUpperBoundaryViolationSouth() throws
	ConversionException, InvalidValueWarning {
		sounding.setLatitude(-90.1, SOUTH);
	}
	
	@Test public void latitudeDegreeLowerBoundary() throws InvalidValueWarning {
		sounding.setLatitude(-90.0);
		assertEquals("Latitude: -90", -90.0, sounding.getLatitude(), THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void latitudeDegreeLowerBoundaryViolation() throws
	InvalidValueWarning {
		sounding.setLatitude(-90.1);
	}
	
	@Test public void latitudeDegreeNullDegrees() throws InvalidValueWarning {
		sounding.setLatitude(null);
		assertNull("Latitude: null", sounding.getLatitude());
	}
	
	@Test public void latitudeDegreeUpperBoundary() throws InvalidValueWarning {
		sounding.setLatitude(90.0);
		assertEquals("Latitude:  90",  90.0, sounding.getLatitude(), THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void latitudeDegreeUpperBoundaryViolation() throws
	InvalidValueWarning {
		sounding.setLatitude(90.1);
	}
	
	@Test public void latitudeDMDirectionLowerBoundary() throws
	ConversionException, InvalidValueWarning {
		sounding.setLatitude(-90.0, 0.0, NORTH);
		assertEquals("Latitude: -90 00 N", -90.0, sounding.getLatitude(),
				THRESHOLD);
		sounding.setLatitude(90.0, 0.0, SOUTH);
		assertEquals("Latitude:  90 00 S", -90.0, sounding.getLatitude(), 
				THRESHOLD);
	}

	@Test (expected = InvalidValueWarning.class)
	public void latitudeDMDirectionLowerBoundaryViolationNorth() throws
	ConversionException, InvalidValueWarning {
		sounding.setLatitude(-90.0, 0.1, NORTH);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void latitudeDMDirectionLowerBoundaryViolationSouth() throws
	ConversionException, InvalidValueWarning {
		sounding.setLatitude(90.0, 0.1, SOUTH);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMDirectionMinuteLowerBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		sounding.setLatitude(30.0, -0.1, NORTH);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMDirectionMinuteUpperBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		sounding.setLatitude(30.0, 60.1, NORTH);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMDirectionNullDegrees() throws ConversionException,
	InvalidValueWarning {
		sounding.setLatitude(null, 30.0, NORTH);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMDirectionNullDirection() throws ConversionException,
	InvalidValueWarning {
		sounding.setLatitude(30.0, 30.0, (LatitudeUnit)null);
	}
		
	@Test (expected = ConversionException.class)
	public void latitudeDMDirectionNullMinutes() throws ConversionException,
	InvalidValueWarning {
		sounding.setLatitude(30.0, null, NORTH);
	}
	
	@Test public void latitudeDMDirectionUpperBoundary() throws
	ConversionException, InvalidValueWarning {
		sounding.setLatitude(90.0, 0.0, NORTH);
		assertEquals("Latitude:  90 00 N",  90.0, sounding.getLatitude(),
				THRESHOLD);
		sounding.setLatitude(-90.0, 0.0, SOUTH);
		assertEquals("Latitude: -90 00 S",  90.0, sounding.getLatitude(),
				THRESHOLD);
	}

	@Test (expected = InvalidValueWarning.class)
	public void latitudeDMDirectionUpperBoundaryViolationNorth() throws
	ConversionException, InvalidValueWarning {
		sounding.setLatitude(90.0, 0.1, NORTH);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void latitudeDMDirectionUpperBoundaryViolationSouth() throws
	ConversionException, InvalidValueWarning {
		sounding.setLatitude(-90.0, 0.1, SOUTH);
	}
	
	@Test public void latitudeDMLowerBoundary() throws ConversionException,
	InvalidValueWarning {
		sounding.setLatitude(-90.0, 0.0);
		assertEquals("Latitude: -90 00", -90.0, sounding.getLatitude(),
				THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void latitudeDMLowerBoundaryViolation() throws ConversionException,
	InvalidValueWarning {
		sounding.setLatitude(-90.0, 0.1);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMMinuteLowerBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		sounding.setLatitude(30.0, -0.1);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMMinuteUpperBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		sounding.setLatitude(30.0, 60.1);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMNullDegrees() throws ConversionException,
	InvalidValueWarning {
		sounding.setLatitude(null, 30.0);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMNullMinutes() throws ConversionException,
	InvalidValueWarning {
		sounding.setLatitude(30.0, (Double)null);
	}
	
	@Test public void latitudeDMSDirectionLowerBoundary() throws
	ConversionException, InvalidValueWarning {
		sounding.setLatitude(-90.0, 0.0, 0.0, NORTH);
		assertEquals("Latitude: -90 00 00 N", -90.0, sounding.getLatitude(),
				THRESHOLD);
		sounding.setLatitude(90.0, 0.0, 0.0, SOUTH);
		assertEquals("Latitude:  90 00 00 S", -90.0, sounding.getLatitude(), 
				THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void latitudeDMSDirectionLowerBoundaryViolationNorth() throws
	ConversionException, InvalidValueWarning {
		sounding.setLatitude(-90.0, 0.0, 0.1, NORTH);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void latitudeDMSDirectionLowerBoundaryViolationSouth() throws
	ConversionException, InvalidValueWarning {
		sounding.setLatitude(90.0, 0.0, 0.1, SOUTH);
	}

	@Test (expected = ConversionException.class)
	public void latitudeDMSDirectionMinuteLowerBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		sounding.setLatitude(30.0, -0.1, 30.0, NORTH);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMSDirectionMinuteUpperBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		sounding.setLatitude(30.0, 60.1, 30.0, NORTH);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMSDirectionNullDegrees() throws ConversionException,
	InvalidValueWarning {
		sounding.setLatitude(null, 30.0, 30.0, NORTH);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMSDirectionNullDirection() throws ConversionException,
	InvalidValueWarning {
		sounding.setLatitude(30.0, 30.0, 30.0, null);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMSDirectionNullMinutes() throws ConversionException,
	InvalidValueWarning {
		sounding.setLatitude(30.0, null, 30.0, NORTH);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMSDirectionNullSeconds() throws ConversionException,
	InvalidValueWarning {
		sounding.setLatitude(30.0, 30.0, null, NORTH);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMSDirectionSecondLowerBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		sounding.setLatitude(30.0, 30.0, -0.1, NORTH);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMSDirectionSecondUpperBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		sounding.setLatitude(30.0, 30.0, 60.1, NORTH);
	}
	
	@Test public void latitudeDMSDirectionUpperBoundary() throws
	ConversionException, InvalidValueWarning {
		sounding.setLatitude(90.0, 0.0, 0.0, NORTH);
		assertEquals("Latitude:  90 00 00 N",  90.0, sounding.getLatitude(), 
				THRESHOLD);
		sounding.setLatitude(-90.0, 0.0, 0.0, SOUTH);
		assertEquals("Latitude: -90 00 00 S",  90.0, sounding.getLatitude(), 
				THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void latitudeDMSDirectionUpperBoundaryViolationNorth() throws
	ConversionException, InvalidValueWarning {
		sounding.setLatitude(90.0, 0.0, 0.1, NORTH);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void latitudeDMSDirectionUpperBoundaryViolationSouth() throws
	ConversionException, InvalidValueWarning {
		sounding.setLatitude(-90.0, 0.0, 0.1, SOUTH);
	}
		
	@Test public void latitudeDMSLowerBoundary() throws ConversionException,
	InvalidValueWarning {
		sounding.setLatitude(-90.0, 0.0, 0.0);
		assertEquals("Latitude: -90 00 00", -90.0, sounding.getLatitude(),
				THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void latitudeDMSLowerBoundaryViolation() throws ConversionException,
	InvalidValueWarning {
		sounding.setLatitude(-90.0, 0.0, 0.1);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMSMinuteLowerBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		sounding.setLatitude(30.0, -0.1, 30.0);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMSMinuteUpperBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		sounding.setLatitude(30.0, 60.1, 30.0);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMSNullDegrees() throws ConversionException,
	InvalidValueWarning {
		sounding.setLatitude(null, 30.0, 30.0);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMSNullMinutes() throws ConversionException,
	InvalidValueWarning {
		sounding.setLatitude(30.0, null, 30.0);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMSNullSeconds() throws ConversionException,
	InvalidValueWarning {
		sounding.setLatitude(30.0, 30.0, (Double)null);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMSSecondLowerBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		sounding.setLatitude(30.0, 30.0, -0.1);
	}
	
	@Test (expected = ConversionException.class)
	public void latitudeDMSSecondUpperBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		sounding.setLatitude(30.0, 30.0, 60.1);
	}
	
	@Test public void latitudeDMSUpperBoundary() throws ConversionException,
	InvalidValueWarning {
		sounding.setLatitude(90.0, 0.0, 0.0);
		assertEquals("Latitude:  90 00 00",  90.0, sounding.getLatitude(), 
				THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void latitudeDMSUpperBoundaryViolationNorth() throws
	ConversionException, InvalidValueWarning {
		sounding.setLatitude(90.0, 0.0, 0.1);
	}
	
	@Test public void latitudeDMUpperBoundary() throws ConversionException,
	InvalidValueWarning {
		sounding.setLatitude(90.0, 0.0);
		assertEquals("Latitude:  90 00",  90.0, sounding.getLatitude(),
				THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void latitudeDMUpperBoundaryViolation() throws ConversionException,
	InvalidValueWarning {
		sounding.setLatitude(90.0, 0.1);
	}
	
	@Test public void latitudeFromStation() throws InvalidValueWarning,
	RestrictedOperationException {
		assertEquals("Latitude: set station", 10.0, 
				stationSounding.getLatitude());
		
		assertNull("Latitude: null station", nullStationSounding.getLatitude());
		nullStation.setLatitude(33.0);
		assertEquals("Latitude: null station value", 33.0,
				nullStationSounding.getLatitude());
		
		nullStationSounding.setLatitude(11.11);
		assertEquals("Latitude: manual override", 11.11, 
				nullStationSounding.getLatitude());
		nullStationSounding.setLatitude(null);
		assertEquals("Latitude: manual reset", 33.0,
				nullStationSounding.getLatitude());
	}

	@Test public void longitudeDefaultValue() {
		assertNull("Longitude: default value", sounding.getLongitude());
	}

	@Test public void longitudeDegreeDirectionLowerBoundary() throws
	ConversionException, InvalidValueWarning {
		sounding.setLongitude(-180.0, EAST);
		assertEquals("Longitude: -180 E", -180.0, sounding.getLongitude(),
				THRESHOLD);
		sounding.setLongitude(180.0, WEST);
		assertEquals("Longitude:  180 W", -180.0, sounding.getLongitude(), 
				THRESHOLD);
	}

	@Test (expected = InvalidValueWarning.class)
	public void longitudeDegreeDirectionLowerBoundaryViolationNorth() throws
	ConversionException, InvalidValueWarning {
		sounding.setLongitude(-180.1, EAST);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void longitudeDegreeDirectionLowerBoundaryViolationSouth() throws
	ConversionException, InvalidValueWarning {
		sounding.setLongitude(180.1, WEST);
	}
	
	@Test public void longitudeDegreeDirectionNullDegrees() throws
	ConversionException, InvalidValueWarning {
		sounding.setLongitude(null, EAST);
		assertNull("Longitude: null E", sounding.getLongitude());
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDegreeDirectionNullDirection() throws
	ConversionException, InvalidValueWarning {
		sounding.setLongitude(30.0, (LongitudeUnit)null);
	}
	
	@Test public void longitudeDegreeDirectionUpperBoundary() throws
	ConversionException, InvalidValueWarning {
		sounding.setLongitude(180.0, EAST);
		assertEquals("Longitude:  180 E",  180.0, sounding.getLongitude(),
				THRESHOLD);
		sounding.setLongitude(-180.0, WEST);
		assertEquals("Longitude: -180 W",  180.0, sounding.getLongitude(),
				THRESHOLD);
	}

	@Test (expected = InvalidValueWarning.class)
	public void longitudeDegreeDirectionUpperBoundaryViolationNorth() throws
	ConversionException, InvalidValueWarning {
		sounding.setLongitude(180.1, EAST);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void longitudeDegreeDirectionUpperBoundaryViolationSouth() throws
	ConversionException, InvalidValueWarning {
		sounding.setLongitude(-180.1, WEST);
	}
	
	@Test public void longitudeDegreeLowerBoundary() throws
	InvalidValueWarning {
		sounding.setLongitude(-180.0);
		assertEquals("Longitude: -180", -180.0, sounding.getLongitude(), 
				THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void longitudeDegreeLowerBoundaryViolation() throws
	InvalidValueWarning {
		sounding.setLongitude(-180.1);
	}
	
	@Test public void longitudeDegreeNullDegrees() throws InvalidValueWarning {
		sounding.setLongitude(null);
		assertNull("Longitude: null", sounding.getLongitude());
	}
	
	@Test public void longitudeDegreeUpperBoundary() throws
	InvalidValueWarning {
		sounding.setLongitude(180.0);
		assertEquals("Longitude:  180",  180.0, sounding.getLongitude(),
				THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void longitudeDegreeUpperBoundaryViolation() throws
	InvalidValueWarning {
		sounding.setLongitude(180.1);
	}
	
	@Test public void longitudeDMDirectionLowerBoundary() throws
	ConversionException, InvalidValueWarning {
		sounding.setLongitude(-180.0, 0.0, EAST);
		assertEquals("Longitude: -180 00 E", -180.0, sounding.getLongitude(), 
				THRESHOLD);
		sounding.setLongitude(180.0, 0.0, WEST);
		assertEquals("Longitude:  180 00 W", -180.0, sounding.getLongitude(), 
				THRESHOLD);
	}

	@Test (expected = InvalidValueWarning.class)
	public void longitudeDMDirectionLowerBoundaryViolationNorth() throws
	ConversionException, InvalidValueWarning {
		sounding.setLongitude(-180.0, 0.1, EAST);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void longitudeDMDirectionLowerBoundaryViolationSouth() throws
	ConversionException, InvalidValueWarning {
		sounding.setLongitude(180.0, 0.1, WEST);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMDirectionMinuteLowerBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		sounding.setLongitude(30.0, -0.1, EAST);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMDirectionMinuteUpperBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		sounding.setLongitude(30.0, 60.1, EAST);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMDirectionNullDegrees() throws ConversionException,
	InvalidValueWarning {
		sounding.setLongitude(null, 30.0, EAST);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMDirectionNullDirection() throws ConversionException,
	InvalidValueWarning {
		sounding.setLongitude(30.0, 30.0, (LongitudeUnit)null);
	}
		
	@Test (expected = ConversionException.class)
	public void longitudeDMDirectionNullMinutes() throws ConversionException,
	InvalidValueWarning {
		sounding.setLongitude(30.0, null, EAST);
	}
	
	@Test public void longitudeDMDirectionUpperBoundary() throws
	ConversionException, InvalidValueWarning {
		sounding.setLongitude(180.0, 0.0, EAST);
		assertEquals("Longitude:  180 00 E",  180.0, sounding.getLongitude(),
				THRESHOLD);
		sounding.setLongitude(-180.0, 0.0, WEST);
		assertEquals("Longitude: -180 00 W",  180.0, sounding.getLongitude(),
				THRESHOLD);
	}

	@Test (expected = InvalidValueWarning.class)
	public void longitudeDMDirectionUpperBoundaryViolationNorth() throws
	ConversionException, InvalidValueWarning {
		sounding.setLongitude(180.0, 0.1, EAST);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void longitudeDMDirectionUpperBoundaryViolationSouth() throws
	ConversionException, InvalidValueWarning {
		sounding.setLongitude(-180.0, 0.1, WEST);
	}
	
	@Test public void longitudeDMLowerBoundary() throws ConversionException,
	InvalidValueWarning {
		sounding.setLongitude(-180.0, 0.0);
		assertEquals("Longitude: -180 00", -180.0, sounding.getLongitude(),
				THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void longitudeDMLowerBoundaryViolation() throws ConversionException,
	InvalidValueWarning {
		sounding.setLongitude(-180.0, 0.1);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMMinuteLowerBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		sounding.setLongitude(30.0, -0.1);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMMinuteUpperBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		sounding.setLongitude(30.0, 60.1);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMNullDegrees() throws ConversionException,
	InvalidValueWarning {
		sounding.setLongitude(null, 30.0);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMNullMinutes() throws ConversionException,
	InvalidValueWarning {
		sounding.setLongitude(30.0, (Double)null);
	}
	
	@Test public void longitudeDMSDirectionLowerBoundary() throws
	ConversionException, InvalidValueWarning {
		sounding.setLongitude(-180.0, 0.0, 0.0, EAST);
		assertEquals("Longitude: -180 00 00 E", -180.0, sounding.getLongitude(),
				THRESHOLD);
		sounding.setLongitude(180.0, 0.0, 0.0, WEST);
		assertEquals("Longitude:  180 00 00 W", -180.0, sounding.getLongitude(),
				THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void longitudeDMSDirectionLowerBoundaryViolationNorth() throws
	ConversionException, InvalidValueWarning {
		sounding.setLongitude(-180.0, 0.0, 0.1, EAST);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void longitudeDMSDirectionLowerBoundaryViolationSouth() throws
	ConversionException, InvalidValueWarning {
		sounding.setLongitude(180.0, 0.0, 0.1, WEST);
	}

	@Test (expected = ConversionException.class)
	public void longitudeDMSDirectionMinuteLowerBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		sounding.setLongitude(30.0, -0.1, 30.0, EAST);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMSDirectionMinuteUpperBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		sounding.setLongitude(30.0, 60.1, 30.0, EAST);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMSDirectionNullDegrees() throws ConversionException,
	InvalidValueWarning {
		sounding.setLongitude(null, 30.0, 30.0, EAST);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMSDirectionNullDirection() throws ConversionException,
	InvalidValueWarning {
		sounding.setLongitude(30.0, 30.0, 30.0, null);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMSDirectionNullMinutes() throws ConversionException,
	InvalidValueWarning {
		sounding.setLongitude(30.0, null, 30.0, EAST);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMSDirectionNullSeconds() throws ConversionException,
	InvalidValueWarning {
		sounding.setLongitude(30.0, 30.0, null, EAST);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMSDirectionSecondLowerBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		sounding.setLongitude(30.0, 30.0, -0.1, EAST);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMSDirectionSecondUpperBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		sounding.setLongitude(30.0, 30.0, 60.1, EAST);
	}
	
	@Test public void longitudeDMSDirectionUpperBoundary() throws
	ConversionException, InvalidValueWarning {
		sounding.setLongitude(180.0, 0.0, 0.0, EAST);
		assertEquals("Longitude:  180 00 00 E",  180.0, 
				sounding.getLongitude(), THRESHOLD);
		sounding.setLongitude(-180.0, 0.0, 0.0, WEST);
		assertEquals("Longitude: -180 00 00 W",  180.0, 
				sounding.getLongitude(), THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void longitudeDMSDirectionUpperBoundaryViolationNorth() throws
	ConversionException, InvalidValueWarning {
		sounding.setLongitude(180.0, 0.0, 0.1, EAST);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void longitudeDMSDirectionUpperBoundaryViolationSouth() throws
	ConversionException, InvalidValueWarning {
		sounding.setLongitude(-180.0, 0.0, 0.1, WEST);
	}
		
	@Test public void longitudeDMSLowerBoundary() throws ConversionException,
	InvalidValueWarning {
		sounding.setLongitude(-180.0, 0.0, 0.0);
		assertEquals("Longitude: -180 00 00", -180.0, sounding.getLongitude(),
				THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void longitudeDMSLowerBoundaryViolation() throws ConversionException,
	InvalidValueWarning {
		sounding.setLongitude(-180.0, 0.0, 0.1);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMSMinuteLowerBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		sounding.setLongitude(30.0, -0.1, 30.0);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMSMinuteUpperBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		sounding.setLongitude(30.0, 60.1, 30.0);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMSNullDegrees() throws ConversionException,
	InvalidValueWarning {
		sounding.setLongitude(null, 30.0, 30.0);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMSNullMinutes() throws ConversionException,
	InvalidValueWarning {
		sounding.setLongitude(30.0, null, 30.0);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMSNullSeconds() throws ConversionException,
	InvalidValueWarning {
		sounding.setLongitude(30.0, 30.0, (Double)null);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMSSecondLowerBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		sounding.setLongitude(30.0, 30.0, -0.1);
	}
	
	@Test (expected = ConversionException.class)
	public void longitudeDMSSecondUpperBoundaryViolation() throws
	ConversionException, InvalidValueWarning {
		sounding.setLongitude(30.0, 30.0, 60.1);
	}
	
	@Test public void longitudeDMSUpperBoundary() throws ConversionException,
	InvalidValueWarning {
		sounding.setLongitude(180.0, 0.0, 0.0);
		assertEquals("Longitude:  180 00 00",  180.0, sounding.getLongitude(),
				THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void longitudeDMSUpperBoundaryViolationNorth() throws
	ConversionException, InvalidValueWarning {
		sounding.setLongitude(180.0, 0.0, 0.1);
	}
	
	@Test public void longitudeDMUpperBoundary() throws ConversionException,
	InvalidValueWarning {
		sounding.setLongitude(180.0, 0.0);
		assertEquals("Longitude:  180 00",  180.0, sounding.getLongitude(),
				THRESHOLD);
	}
	
	@Test (expected = InvalidValueWarning.class)
	public void longitudeDMUpperBoundaryViolation() throws ConversionException,
	InvalidValueWarning {
		sounding.setLongitude(180.0, 0.1);
	}
	
	@Test public void longitudeFromStation() throws InvalidValueWarning,
	RestrictedOperationException {
		assertEquals("Longitude: set station", -100.0, 
				stationSounding.getLongitude());
		
		assertNull("Longitude: null station", 
				nullStationSounding.getLongitude());
		nullStation.setLongitude(33.0);
		assertEquals("Longitude: null station value", 33.0, 
				nullStationSounding.getLongitude());
		
		nullStationSounding.setLongitude(11.11);
		assertEquals("Longitude: manual override", 11.11, 
				nullStationSounding.getLongitude());
		nullStationSounding.setLongitude(null);
		assertEquals("Longitude: manual reset", 33.0,
				nullStationSounding.getLongitude());
	}

	@Test public void nominalRelease() throws DateTimeException {
		assertNull("Nominal Release: default value", sounding.getNominalDate());
		sounding.setNominalRelease(buildDate(2007, 3, 23, UTC));
		assertEquals("Nominal Release: calendar", march, 
				sounding.getNominalDate());
		sounding.setNominalRelease(null);
		assertNull("Nominal Release: null", sounding.getNominalDate());
		sounding.setNominalRelease(2007, 3, 23, 0, 0, 0, UTC);
		assertEquals("Nominal Release: YYYY, MM, DD, tz", march, 
				sounding.getNominalDate());
		sounding.setNominalRelease(2007, 365, 0, 0, 0, UTC);
		assertEquals("Nominal Day: YYYY, JJJ, tz", december, 
				sounding.getNominalDate());
		sounding.setNominalRelease(2007, 3, 23, 0, 0, 0, 
				MOUNTAIN_STANDARD_TIME);
		assertEquals("Nominal Release: different time zone", 
				buildDate(2007, 3, 23, 7, 0, 0, 0, UTC), 
				sounding.getNominalDate());
	}
	
	@Test (expected = DateTimeException.class)
	public void nominalReleaseDayLowerBoundaryViolation() 
	throws DateTimeException {
		sounding.setNominalRelease(2007, 1, 0, 0, 0, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void nominalReleaseDayUpperBoundaryViolation()
	throws DateTimeException {
		sounding.setNominalRelease(2007, 1, 32, 0, 0, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void nominalReleaseHourLowerBoundaryViolation() 
	throws DateTimeException {
		sounding.setNominalRelease(2007, 1, 1, -1, 0, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void nominalReleaseHourUpperBoundaryViolation() 
	throws DateTimeException {
		sounding.setNominalRelease(2007, 1, 1, 25, 0, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void nominalReleaseJulianLowerBoundaryViolation() 
	throws DateTimeException {
		sounding.setNominalRelease(2007, 0, 0, 0, 0, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void nominalReleaseJulianNullTimeZone() 
	throws DateTimeException {
		sounding.setNominalRelease(2007, 1, 0, 0, 0, 0, null);
	}
	
	@Test (expected = DateTimeException.class)
	public void nominalReleaseJulianUpperBoundaryViolation() 
	throws DateTimeException {
		sounding.setNominalRelease(2007, 366, 0, 0, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void nominalReleaseMinuteLowerBoundaryViolation() 
	throws DateTimeException {
		sounding.setNominalRelease(2007, 1, 1, 0, -1, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void nominalReleaseMinuteUpperBoundaryViolation() 
	throws DateTimeException {
		sounding.setNominalRelease(2007, 1, 1, 0, 61, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void nominalReleaseMonthLowerBoundaryViolation() 
	throws DateTimeException {
		sounding.setNominalRelease(2007, 0, 1, 0, 0, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void nominalReleaseMonthUpperBoundaryViolation() throws
	DateTimeException {
		sounding.setNominalRelease(2007, 13, 1, 0, 0, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void nominalReleaseNullTimeZone() throws DateTimeException {
		sounding.setNominalRelease(2007, 1, 1, 0, 0, 0, null);
	}
	
	@Test (expected = DateTimeException.class)
	public void nominalReleaseSecondLowerBoundaryViolation() throws 
	DateTimeException {
		sounding.setNominalRelease(2007, 1, 1, 0, 0, -1, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void nominalReleaseSecondUpperBoundaryViolation() throws
	DateTimeException {
		sounding.setNominalRelease(2007, 1, 1, 0, 0, 61, UTC);
	}
	
	@Test public void projectId() {
		assertNull("Project: default", sounding.getProjectId());
		sounding.setProjectId("Project");
		assertEquals("Project: value", "Project", sounding.getProjectId());
		sounding.setProjectId(null);
		assertNull("Project: null", sounding.getProjectId());
	}
	
	@Test public void releaseDirection() {
		assertNull("Direction: default", sounding.getReleaseDirection());
		sounding.setReleaseDirection(Sounding.ASCENDING);
		assertEquals("Direction: radiosonde", "Ascending", 
				sounding.getReleaseDirection());
		sounding.setReleaseDirection(Sounding.DESCENDING);
		assertEquals("Direction: dropsonde", "Descending", 
				sounding.getReleaseDirection());
		sounding.setReleaseDirection(null);
		assertNull("Direction: null", sounding.getReleaseDirection());
	}
	
	@Test public void stationDescription() {
		assertNull("Station Description: default", 
				sounding.getStationDescription());
		sounding.setStationDescription("Stn Desc");
		assertEquals("Station Description: value", "Stn Desc", 
				sounding.getStationDescription());
	}
	
	@Test public void stationDescriptionFromStation() throws
	InvalidValueException {
		assertEquals("Station Description: from station", "Northwest Station", 
				stationSounding.getStationDescription());
		
		assertNull("Station Description: null station", 
				nullStationSounding.getStationDescription());
		nullStation.setDescription("Null Station Desc");
		assertEquals("Station Description: change station desc in station", 
				"Null Station Desc", nullStationSounding.getStationDescription());
		
		nullStationSounding.setStationDescription("Manual Override");
		assertEquals("Station Description: manual override", "Manual Override",
				nullStationSounding.getStationDescription());
		nullStationSounding.setStationDescription(null);
		assertEquals("Station Description: manual reset", "Null Station Desc",
				nullStationSounding.getStationDescription());
	}
	
	@Test public void stationId() {
		assertNull("Station Id: default", sounding.getStationId());
		sounding.setStationId("StnID");
		assertEquals("Station Id: set value", "StnID", sounding.getStationId());
	}
	
	@Test public void stationIdFromStation() throws InvalidValueException,
	RestrictedOperationException {
		assertEquals("Station Id: from station", "NW1", 
				stationSounding.getStationId());

		assertEquals("Station Id: null station", "NULL", 
				nullStationSounding.getStationId());
		nullStation.setStationId("NewId");
		assertEquals("Station Id: change station id in station", "NewId",
				nullStationSounding.getStationId());

		nullStationSounding.setStationId("Manual Override");
		assertEquals("Station ID: manual override", "Manual Override", 
				nullStationSounding.getStationId());
		nullStationSounding.setStationId(null);
		assertEquals("Station ID: manual reset", "NewId", 
				nullStationSounding.getStationId());
	}
}
