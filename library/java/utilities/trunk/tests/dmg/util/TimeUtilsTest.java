package dmg.util;

import static dmg.util.TimeUtils.*;
import static org.junit.Assert.*;

import java.util.*;
import org.junit.*;

/**
 * <p>The TimeUtilsTest class is a collection of JUnit tests for the TimeUtils
 * class.</p>
 * 
 * @see dmg.util.TimeUtils
 * 
 * @author Joel Clawson
 */
public class TimeUtilsTest {
	
	private static final Double THRESHOLD = Math.pow(1, -10);
	
	@Test public void buildDateFull() throws DateTimeException {
		Calendar date = buildDate(2007, 3, 21, 12, 15, 46, 100, UTC);
		assertEquals("Full Date: year", 2007, date.get(Calendar.YEAR));
		assertEquals("Full Date: month", Calendar.MARCH, 
				date.get(Calendar.MONTH));
		assertEquals("Full Date: day", 21, date.get(Calendar.DAY_OF_MONTH));
		assertEquals("Full Date: hour", 12, date.get(Calendar.HOUR_OF_DAY));
		assertEquals("Full Date: minute", 15, date.get(Calendar.MINUTE));
		assertEquals("Full Date: second", 46, date.get(Calendar.SECOND));
		assertEquals("Full Date: millisecond", 100, 
				date.get(Calendar.MILLISECOND));		
	}
	
	@Test (expected = DateTimeException.class)
	public void buildDateFullNullTimeZone() throws DateTimeException {
		buildDate(2007, 1, 1, 0, 0, 0, 0, null);
	}
	
	@Test (expected = DateTimeException.class)
	public void buildDateFullViolateDayLowerBound() throws DateTimeException {
		buildDate(2007, 1, 0, 0, 0, 0, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void buildDateFullViolateDayUpperBound() throws DateTimeException {
		buildDate(2007, 1, 32, 0, 0, 0, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void buildDateFullViolateHourLowerBound() throws DateTimeException {
		buildDate(2007, 1, 1, -1, 0, 0, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void buildDateFullViolateHourUpperBound() throws DateTimeException {
		buildDate(2007, 1, 1, 25, 0, 0, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void buildDateFullViolateMillisecondLowerBound() 
	throws DateTimeException {
		buildDate(2007, 1, 1, 0, 0, 0, -1, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void buildDateFullViolateMillisecondUpperBound() 
	throws DateTimeException {
		buildDate(2007, 1, 1, 0, 0, 0, 1001, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void buildDateFullViolateMinuteLowerBound()
	throws DateTimeException {
		buildDate(2007, 1, 1, 0, -1, 0, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void buildDateFullViolateMinuteUpperBound() 
	throws DateTimeException {
		buildDate(2007, 1, 1, 0, 61, 0, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void buildDateFullViolateMonthLowerBound() throws DateTimeException {
		buildDate(2007, 0, 1, 0, 0, 0, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void buildDateFullViolateMonthUpperBound() throws DateTimeException {
		buildDate(2007, 13, 1, 0, 0, 0, 0, UTC);
	}

	@Test (expected = DateTimeException.class)
	public void buildDateFullViolateSecondLowerBound() 
	throws DateTimeException {
		buildDate(2007, 1, 1, 0, 0, -1, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void buildDateFullViolateSecondUpperBound() 
	throws DateTimeException {
		buildDate(2007, 1, 1, 0, 0, 61, 0, UTC);
	}

	@Test public void buildDateMillisecond() throws DateTimeException {
		Calendar date = buildDate(7429482941l, UTC);
		assertEquals("ms Date: year", 1970, date.get(Calendar.YEAR));
		assertEquals("ms Date: month", Calendar.MARCH, 
				date.get(Calendar.MONTH));
		assertEquals("ms Date: day", 27, date.get(Calendar.DAY_OF_MONTH));
		assertEquals("ms Date: hour", 23, date.get(Calendar.HOUR_OF_DAY));
		assertEquals("ms Date: minute", 44, date.get(Calendar.MINUTE));
		assertEquals("ms Date: second", 42, date.get(Calendar.SECOND));
		assertEquals("ms Date: millisecond", 941, 
				date.get(Calendar.MILLISECOND));		
	}
	
	@Test (expected = DateTimeException.class)
	public void buildDateMillisecondsNullTimeZone() throws DateTimeException {
		buildDate(89484928492l, null);
	}
	
	@Test public void buildDateSimple() throws DateTimeException {
		Calendar date = buildDate(2007, 3, 21, UTC);
		assertEquals("Simple Date: year", 2007, date.get(Calendar.YEAR));
		assertEquals("Simple Date: month", Calendar.MARCH, 
				date.get(Calendar.MONTH));
		assertEquals("Simple Date: day", 21, date.get(Calendar.DAY_OF_MONTH));
		assertEquals("Simple Date: hour", 0, date.get(Calendar.HOUR_OF_DAY));
		assertEquals("Simple Date: minute", 0, date.get(Calendar.MINUTE));
		assertEquals("Simple Date: second", 0, date.get(Calendar.SECOND));
		assertEquals("Simple Date: millisecond", 0, 
				date.get(Calendar.MILLISECOND));
	}
	
	@Test (expected = DateTimeException.class)
	public void buildDateSimpleNullTimeZone() throws DateTimeException {
		buildDate(2007, 1, 1, null);
	}
	
	@Test (expected = DateTimeException.class)
	public void buildDateSimpleViolateDayLowerBound() throws DateTimeException {
		buildDate(2007, 1, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void buildDateSimpleVioldateDayUpperBound() 
	throws DateTimeException {
		buildDate(2007, 1, 32, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void buildDateSimpleViolateMonthLowerBound() 
	throws DateTimeException {
		buildDate(2007, 0, 1, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void buildDateSimpleViolateMonthUpperBound() 
	throws DateTimeException {
		buildDate(2007, 13, 1, UTC);
	}

	@Test public void buildDateTime() throws DateTimeException {
		Calendar date = buildDate(2007, 3, 21, 12, 15, 46, 
				MOUNTAIN_STANDARD_TIME);
		assertEquals("Time Date: year", 2007, date.get(Calendar.YEAR));
		assertEquals("Time Date: month", Calendar.MARCH, 
				date.get(Calendar.MONTH));
		assertEquals("Time Date: day", 21, date.get(Calendar.DAY_OF_MONTH));
		assertEquals("Time Date: hour", 12, date.get(Calendar.HOUR_OF_DAY));
		assertEquals("Time Date: minute", 15, date.get(Calendar.MINUTE));
		assertEquals("Time Date: second", 46, date.get(Calendar.SECOND));
		assertEquals("Time Date: millisecond", 0, 
				date.get(Calendar.MILLISECOND));		
	}
	
	@Test (expected = DateTimeException.class)
	public void buildDateTimeNullTimeZone() throws DateTimeException {
		buildDate(2007, 1, 1, 0, 0, 0, null);
	}
	
	@Test (expected = DateTimeException.class)
	public void buildDateTimeViolateDayLowerBound() throws DateTimeException {
		buildDate(2007, 1, 0, 0, 0, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void buildDateTimeViolateDayUpperBound() throws DateTimeException {
		buildDate(2007, 1, 32, 0, 0, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void buildDateTimeViolateHourLowerBound() throws DateTimeException {
		buildDate(2007, 1, 1, -1, 0, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void buildDateTimeViolateHourUpperBound() throws DateTimeException {
		buildDate(2007, 1, 1, 25, 0, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void buildDateTimeViolateMinuteLowerBound() 
	throws DateTimeException {
		buildDate(2007, 1, 1, 0, -1, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void buildDateTimeViolateMinuteUpperBound() 
	throws DateTimeException {
		buildDate(2007, 1, 1, 0, 61, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void buildDateTimeViolateMonthLowerBound() throws DateTimeException {
		buildDate(2007, 0, 1, 0, 0, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void buildDateTimeViolateMonthUpperBound() throws DateTimeException {
		buildDate(2007, 13, 1, 0, 0, 0, UTC);
	}

	@Test (expected = DateTimeException.class)
	public void buildDateTimeViolateSecondLowerBound() 
	throws DateTimeException {
		buildDate(2007, 1, 1, 0, 0, -1, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void buildDateTimeViolateSecondUpperBound() 
	throws DateTimeException {
		buildDate(2007, 1, 1, 0, 0, 61, UTC);
	}
	
	@Test public void buildJulianDateFull() throws DateTimeException {
		Calendar date = buildJulianDate(2007, 80, 12, 15, 46, 100, UTC);
		assertEquals("Full Julian Date: year", 2007, date.get(Calendar.YEAR));
		assertEquals("Full Julian Date: month", Calendar.MARCH, 
				date.get(Calendar.MONTH));
		assertEquals("Full Julian Date: day", 21, 
				date.get(Calendar.DAY_OF_MONTH));
		assertEquals("Full Julian Date: hour", 12, 
				date.get(Calendar.HOUR_OF_DAY));
		assertEquals("Full Julian Date: minute", 15, date.get(Calendar.MINUTE));
		assertEquals("Full Julian Date: second", 46, date.get(Calendar.SECOND));
		assertEquals("Full Julian Date: millisecond", 100, 
				date.get(Calendar.MILLISECOND));		
	}
	
	@Test (expected = DateTimeException.class)
	public void buildJulianDateFullNullTimeZone() throws DateTimeException {
		buildJulianDate(2007, 1, 0, 0, 0, 0, null);
	}
	
	@Test (expected = DateTimeException.class)
	public void buildJulianDateFullViolateDayLowerBound() 
	throws DateTimeException {
		buildJulianDate(2007, 0, 0, 0, 0, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void buildJulianDateFullViolateDayUpperBound() 
	throws DateTimeException {
		buildJulianDate(2007, 366, 0, 0, 0, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void buildJulianDateFullViolateHourLowerBound() 
	throws DateTimeException {
		buildJulianDate(2007, 1, -1, 0, 0, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void buildJulianDateFullViolateHourUpperBound() 
	throws DateTimeException {
		buildJulianDate(2007, 1, 25, 0, 0, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void buildJulianDateFullViolateMillisecondLowerBound() 
	throws DateTimeException {
		buildJulianDate(2007, 1, 0, 0, 0, -1, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void buildJulianDateFullViolateMillisecondUpperBound() 
	throws DateTimeException {
		buildJulianDate(2007, 1, 0, 0, 0, 1001, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void buildJulianDateFullViolateMinuteLowerBound() 
	throws DateTimeException {
		buildJulianDate(2007, 1, 0, -1, 0, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void buildJulianDateFullViolateMinuteUpperBound() 
	throws DateTimeException {
		buildJulianDate(2007, 1, 0, 61, 0, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void buildJulianDateFullViolateSecondLowerBound() 
	throws DateTimeException {
		buildJulianDate(2007, 1, 0, 0, -1, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void buildJulianDateFullViolateSecondUpperBound() 
	throws DateTimeException {
		buildJulianDate(2007, 1, 0, 0, 61, 0, UTC);
	}

	@Test public void buildJulianDateSimple() throws DateTimeException {
		Calendar date = buildJulianDate(2007, 80, MOUNTAIN_STANDARD_TIME);
		assertEquals("Simple Julian Date: year", 2007, date.get(Calendar.YEAR));
		assertEquals("Simple Julian Date: month", Calendar.MARCH, 
				date.get(Calendar.MONTH));
		assertEquals("Simple Julian Date: day", 21, 
				date.get(Calendar.DAY_OF_MONTH));
		assertEquals("Simple Julian Date: hour", 0, 
				date.get(Calendar.HOUR_OF_DAY));
		assertEquals("Simple Julian Date: minute", 0, 
				date.get(Calendar.MINUTE));
		assertEquals("Simple Julian Date: second", 0, 
				date.get(Calendar.SECOND));
		assertEquals("Simple Julian Date: millisecond", 0, 
				date.get(Calendar.MILLISECOND));		
	}
	
	@Test (expected = DateTimeException.class)
	public void buildJulianDateSimpleNullTimeZone() throws DateTimeException {
		buildJulianDate(2007, 1, null);
	}
	
	@Test (expected = DateTimeException.class)
	public void buildJulianDateSimpleViolateDayLowerBound() 
	throws DateTimeException {
		buildJulianDate(2007, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void buildJulianDateSimpleVioldateDayUpperBound() 
	throws DateTimeException {
		buildJulianDate(2007, 366, UTC);
	}
	
	@Test public void buildJulianDateTime() throws DateTimeException {
		Calendar date = buildJulianDate(2007, 80, 12, 15, 46,
				MOUNTAIN_STANDARD_TIME);
		assertEquals("Julian Time Date: year", 2007, date.get(Calendar.YEAR));
		assertEquals("Julian Time Date: month", Calendar.MARCH, 
				date.get(Calendar.MONTH));
		assertEquals("Julian Time Date: day", 21, 
				date.get(Calendar.DAY_OF_MONTH));
		assertEquals("Julian Time Date: hour", 12, 
				date.get(Calendar.HOUR_OF_DAY));
		assertEquals("Julian Time Date: minute", 15, date.get(Calendar.MINUTE));
		assertEquals("Julian Time Date: second", 46, date.get(Calendar.SECOND));
		assertEquals("Julian Time Date: millisecond", 0,
				date.get(Calendar.MILLISECOND));		
	}
	
	@Test (expected = DateTimeException.class)
	public void buildJulianDateTimeNullTimeZone() throws DateTimeException {
		buildJulianDate(2007, 1, 0, 0, 0, null);
	}
	
	@Test (expected = DateTimeException.class)
	public void buildJulianDateTimeViolateDayLowerBound() 
	throws DateTimeException {
		buildJulianDate(2007, 0, 0, 0, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void buildJulianDateTimeViolateDayUpperBound() 
	throws DateTimeException {
		buildJulianDate(2007, 366, 0, 0, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void buildJulianDateTimeViolateHourLowerBound() 
	throws DateTimeException {
		buildJulianDate(2007, 1, -1, 0, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void buildJulianDateTimeViolateHourUpperBound() 
	throws DateTimeException {
		buildJulianDate(2007, 1, 25, 0, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void buildJulianDateTimeViolateMinuteLowerBound()
	throws DateTimeException {
		buildJulianDate(2007, 1, 0, -1, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void buildJulianDateTimeViolateMinuteUpperBound()
	throws DateTimeException {
		buildJulianDate(2007, 1, 0, 61, 0, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void buildJulianDateTimeViolateSecondLowerBound() 
	throws DateTimeException {
		buildJulianDate(2007, 1, 0, 0, -1, UTC);
	}
	
	@Test (expected = DateTimeException.class)
	public void buildJulianDateTimeViolateSecondUpperBound() 
	throws DateTimeException {
		buildJulianDate(2007, 1, 0, 0, 61, UTC);
	}
	
	@Test (expected = ConversionException.class)
	public void convertTimeNullInputUnit() throws ConversionException {
		convertTime(10.0, null, SECONDS);
	}
	
	@Test (expected = ConversionException.class)
	public void convertTimeNullOutputUnit() throws ConversionException {
		convertTime(10.0, SECONDS, null);
	}
	
	@Test public void convertTimeNullValue() throws ConversionException { 
		assertNull("convertTime null value", convertTime(null, SECONDS, HOURS));
	}
	
	@Test public void dayConversions() throws ConversionException {
		assertEquals("Days -> Seconds",864000.0,
				convertTime(10.0,DAYS,SECONDS), THRESHOLD);
		assertEquals("Days -> Minutes",14400.0,
				convertTime(10.0,DAYS,MINUTES), THRESHOLD);
		assertEquals("Days -> Hours",240.0,
				convertTime(10.0,DAYS,HOURS), THRESHOLD);
		assertEquals("Days -> Days",10.0,
				convertTime(10.0,DAYS,DAYS), THRESHOLD);
	}
	
	@Test public void hourConversions() throws ConversionException {
		assertEquals("Hours -> Seconds",36000.0,
				convertTime(10.0,HOURS,SECONDS), THRESHOLD);
		assertEquals("Hours -> Minutes",600.0,
				convertTime(10.0,HOURS,MINUTES), THRESHOLD);
		assertEquals("Hours -> Hours",10.0,
				convertTime(10.0,HOURS,HOURS), THRESHOLD);
		assertEquals("Hours -> Days",.4166666667,
				convertTime(10.0,HOURS,DAYS), THRESHOLD);
	}
	
	@Test public void minuteConversions() throws ConversionException {
		assertEquals("Minutes -> Seconds",600.0,
				convertTime(10.0,MINUTES,SECONDS), THRESHOLD);
		assertEquals("Minutes -> Minutes",10.0,
				convertTime(10.0,MINUTES,MINUTES), THRESHOLD);
		assertEquals("Minutes -> Hours",.1666666667,
				convertTime(10.0,MINUTES,HOURS), THRESHOLD);
		assertEquals("Minutes -> Days",.0069444444,
				convertTime(10.0,MINUTES,DAYS), THRESHOLD);
	}
	
	@Test public void secondConversions() throws ConversionException {
		assertEquals("Seconds -> Seconds",5.0,
				convertTime(5.0,SECONDS,SECONDS), THRESHOLD);
		assertEquals("Seconds -> Minutes",2.0,
				convertTime(120.0,SECONDS,MINUTES), THRESHOLD);
		assertEquals("Seconds -> Hours",2.0,
				convertTime(7200.0,SECONDS,HOURS), THRESHOLD);
		assertEquals("Seconds -> Days",2.0,
				convertTime(172800.0,SECONDS,DAYS), THRESHOLD);
	}
	
	@Test public void switchZones() throws ConversionException, 
	DateTimeException {
		assertEquals("Switch Time Zone", 
				buildDate(2007, 3, 21, 20, 54, 16, UTC), 
				switchToTimeZone(buildDate(2007, 3, 21, 14, 54, 16,
						MOUNTAIN_DAYLIGHT_TIME), UTC));
		assertEquals("Switch Time Zone", 
				buildDate(2006, 3, 21, 21, 54, 16, UTC), 
				switchToTimeZone(buildDate(2006, 3, 21, 14, 54, 16,
						MOUNTAIN_DAYLIGHT_TIME), UTC));
	}
	
	@Test public void switchZonesNullDate() throws ConversionException {
		assertNull("Switch Time Zones: null date", switchToTimeZone(null, UTC));
	}
	
	@Test (expected = ConversionException.class) 
	public void switchZonesNullTimeZone() 
	throws ConversionException, DateTimeException {
		switchToTimeZone(buildDate(2007, 1, 1, MOUNTAIN_STANDARD_TIME), null);
	}
	
	@Test public void timeZoneCDT() {
		assertEquals("CDT Time Zone offset",-21600000,
				CENTRAL_DAYLIGHT_TIME.getRawOffset());
		assertEquals("CDT Time Zone DST",3600000,
				CENTRAL_DAYLIGHT_TIME.getDSTSavings());
	}

	@Test public void timeZoneCST() {
		assertEquals("CST Time Zone offset",-21600000,
				CENTRAL_STANDARD_TIME.getRawOffset());
		assertEquals("CST Time Zone DST",0,
				CENTRAL_STANDARD_TIME.getDSTSavings());
	}
	
	@Test public void timeZoneEDT() {
		assertEquals("EDT Time Zone offset",-18000000,
				EASTERN_DAYLIGHT_TIME.getRawOffset());
		assertEquals("EDT Time Zone DST",3600000,
				EASTERN_DAYLIGHT_TIME.getDSTSavings());
	}

	@Test public void timeZoneEST() {
		assertEquals("EST Time Zone offset",-18000000,
				EASTERN_STANDARD_TIME.getRawOffset());
		assertEquals("EST Time Zone DST",0,
				EASTERN_STANDARD_TIME.getDSTSavings());
	}
	
	@Test public void timeZoneMDT() {
		assertEquals("MDT Time Zone offset",-25200000,
				MOUNTAIN_DAYLIGHT_TIME.getRawOffset());
		assertEquals("MDT Time Zone DST",3600000,
				MOUNTAIN_DAYLIGHT_TIME.getDSTSavings());
	}

	@Test public void timeZoneMST() {
		assertEquals("MST Time Zone offset",-25200000,
				MOUNTAIN_STANDARD_TIME.getRawOffset());
		assertEquals("MST Time Zone DST",0,
				MOUNTAIN_STANDARD_TIME.getDSTSavings());
	}
	
	@Test public void timeZonePDT() {
		assertEquals("PDT Time Zone offset",-28800000,
				PACIFIC_DAYLIGHT_TIME.getRawOffset());
		assertEquals("PDT Time Zone DST",3600000,
				PACIFIC_DAYLIGHT_TIME.getDSTSavings());
	}

	@Test public void timeZonePST() {
		assertEquals("PST Time Zone offset",-28800000,
				PACIFIC_STANDARD_TIME.getRawOffset());
		assertEquals("PST Time Zone DST",0,PACIFIC_STANDARD_TIME.getDSTSavings());
	}
	
	@Test public void timeZoneUTC() {
		assertEquals("UTC Time Zone offset",0,UTC.getRawOffset());
		assertEquals("UTC Time Zone dst",0,UTC.getDSTSavings());
	}
}
