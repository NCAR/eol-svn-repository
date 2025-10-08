package dmg.util;

import java.util.*;

/**
 * <p>The TimeUtils class is a collection of functions and constants used to
 * manipulation date/time values.</p>
 *
 * @author Joel Clawson
 */
public final class TimeUtils {
	
    /**
     * A constant to define the Central Daylight Time Zone.  This defines an 
     * offest of 5 hours during DST and 6 hours during standard time from GMT.
     */
    public static final TimeZone CENTRAL_DAYLIGHT_TIME = TimeZone.getTimeZone("US/Central");
	
    /**
     * A constant to define the Central Standard Time Zone.  This defines all 
     * times as 6 hours offset from GMT.
     */
    public static final TimeZone CENTRAL_STANDARD_TIME = TimeZone.getTimeZone("Etc/GMT+6");

    /**
     * A constant to define the Eastern Daylight Time Zone.  This defines an 
     * offset of 4 hours during DST and 5 hours during standard time from GMT.
     */
    public static final TimeZone EASTERN_DAYLIGHT_TIME = TimeZone.getTimeZone("US/Eastern");

    /**
     * A constant to define the Eastern Standard Time Zone.  This defines all
     * times as 5 hours offset from GMT.
     */	
    public static final TimeZone EASTERN_STANDARD_TIME = TimeZone.getTimeZone("Etc/GMT+5");
	
    /**
     * A constant to define Greenwich Mean Time.
     */
    public static final TimeZone GMT = TimeZone.getTimeZone("Etc/GMT+0");
	
    /**
     * A constant to define the Mountain Daylight Time Zone.  This defines an 
     * offset of 6 hours during DST and 7 hours during standard time from GMT.
     */
    public static final TimeZone MOUNTAIN_DAYLIGHT_TIME = TimeZone.getTimeZone("US/Mountain");
	
    /**
     * A constant to define the Mountain Standard Time Zone.  This defines all 
     * times as 7 hours offset from GMT.
     */
    public static final TimeZone MOUNTAIN_STANDARD_TIME = TimeZone.getTimeZone("Etc/GMT+7");
	
    /**
     * A constant to define the Pacific Daylight Time Zone.  This defines an 
     * offset of 7 hours during DST and 8 hours during standard time from GMT.
     */
    public static final TimeZone PACIFIC_DAYLIGHT_TIME = TimeZone.getTimeZone("US/Pacific");
	
    /**
     * A constant to define the Pacific Standard Time Zone.  This defines all
     * times as 8 hours offset from GMT.
     */
    public static final TimeZone PACIFIC_STANDARD_TIME = TimeZone.getTimeZone("Etc/GMT+8");
	
    /**
     * A constant to define Coordinated Universal Time.  This is an alias for GMT.
     * @see #GMT
     */
    public static final TimeZone UTC = GMT;
	
    /**
     * The constant to define a day unit of time.
     */
    public static final TimeUnit DAYS = (new TimeUtils()).new TimeUnit("day");
	
    /**
     * The constant to define an hour unit of time.
     */
    public static final TimeUnit HOURS = (new TimeUtils()).new TimeUnit("hour");
    
    /**
     * The constant to define a minute unit of time.
     */
    public static final TimeUnit MINUTES = (new TimeUtils()).new TimeUnit("min");
	
    /**
     * The constant to define a second unit of time.
     */
    public static final TimeUnit SECONDS = (new TimeUtils()).new TimeUnit("sec");
	
    /**
     * Create a new instance of a TimeUtils.  It is private to prevent other
     * classes from creating instances of the class.
     */
    private TimeUtils() {}
    
    /**
     * Create a new date from the number of milliseconds after 1970/01/01 00:00:00.
     * @param milliseconds The number of milliseconds after 1970/01/01 00:00:00.
     * @param timeZone The time zone the date is in.
     * @return A Calendar that represents the date.
     * @throws DateTimeException if the time zone value is <code>null</code>.
     */
    public static Calendar buildDate(long milliseconds, TimeZone timeZone) 
	throws DateTimeException
    {
	// Make sure a time zone is sent in
	if (timeZone == null) {
	    throw new DateTimeException("The time zone was null in call to buildDate.");
	}
	
	Calendar date = Calendar.getInstance(timeZone);
	date.setTimeInMillis(milliseconds);
	return date;
    }
    
    /**
     * Create a new date for the specified date and time zone.
     * @param year The year of the date.
     * @param month The month of the year.
     * @param day The day of the month.
     * @param timeZone The time zone the date is in.
     * @return A Calendar that represents this date.
     * @throws DateTimeException if the month is not a valid month value, or the day is 
     * not a valid day for the month, or the time zone is <code>null</code>.
     */
    public static Calendar buildDate(int year, int month, int day, TimeZone timeZone)
	throws DateTimeException
    {
	return buildDate(year, month, day, 0, 0, 0, timeZone);
    }
    
    /**
     * Create a new date for the specified date, time, and time zone.
     * @param year The year of the date.
     * @param month The month of the year.
     * @param day The day of the month.
     * @param hour The hour of the day.
     * @param minute The minute of the hour.
     * @param second The seconds of the minute.
     * @param timeZone The time zone the date is in.
     * @return A Calendar that represents the specified date.
     * @throws DateTimeException if the month is not a valid month value,
     * the day is not a valid day for the month, the hour is not a valid hour 
     * value, the minute is not a valid minute value, the second is not a valid 
     * second value, or the time zone is <code>null</code>.
     */
    public static Calendar buildDate(int year, int month, int day, int hour, 
				     int minute, int second, TimeZone timeZone) 
	throws DateTimeException
    {
	return buildDate(year, month, day, hour, minute, second, 0, timeZone);
    }
    
    /**
     * Create a new date for the specified date, time and time zone.
     * @param year The year of the date.
     * @param month The month of the year.
     * @param day The day of the month.
     * @param hour The hour of the day.
     * @param minute The minute of the hour.
     * @param second The second of the minute.
     * @param millisecond The millisecond of the second.
     * @param timeZone The time zone the date is in.
     * @return A Calendar that represents the specified date.
     * @throws DateTimeException if the month is not a valid month value,
     * the day is not a valid day for the month, the hour is not a valid hour 
     * value, the minute is not a valid minute value, the second is not a valid 
     * second value, the millisecond is not a valid millisecond value, or the 
     * time zone is <code>null</code>.
     */
    public static Calendar buildDate(int year, int month, int day, int hour, 
				     int minute, int second, int millisecond, TimeZone timeZone) 
	throws DateTimeException
    {
	// Make sure a time zone is sent in
	if (timeZone == null) {
	    throw new DateTimeException("The time zone was null in call to buildDate.");
	}
	
	month--; // Adjust the month to 0 based for calendar.
	Calendar date = Calendar.getInstance(timeZone);
	date.set(Calendar.YEAR, year);
	
	// Make sure that the month is valid.
	if (month < date.getActualMinimum(Calendar.MONTH) || 
	    date.getActualMaximum(Calendar.MONTH) < month) {
	    throw new DateTimeException(String.format("The month of %d is not valid.  " +
						      "It must be between 1 and 12.",month + 1));
	}
	date.set(Calendar.MONTH, month);
	
	// Make sure that the day of the month is valid.
	if (day < date.getActualMinimum(Calendar.DAY_OF_MONTH) || 
	    date.getActualMaximum(Calendar.DAY_OF_MONTH) < day) {
	    throw new DateTimeException(String.format("The day of %d is not valid for month %d.  " +
						      "It must be between %d and %d.",
						      day,month + 1,date.getActualMinimum(Calendar.DAY_OF_MONTH),
						      date.getActualMaximum(Calendar.DAY_OF_MONTH)));
	}
	date.set(Calendar.DAY_OF_MONTH, day);
	
	// Make sure that the hour is valid.
	if (hour < date.getActualMinimum(Calendar.HOUR_OF_DAY) || 
	    date.getActualMaximum(Calendar.HOUR_OF_DAY) + 1 < hour) {
	    throw new DateTimeException(String.format("The hour of %d is not valid.  " +
						      "It must be between %d and %d.",
						      hour,date.getActualMinimum(Calendar.HOUR_OF_DAY),
						      date.getActualMaximum(Calendar.HOUR_OF_DAY) + 1));
	}
	date.set(Calendar.HOUR_OF_DAY, hour);
	
	// Make sure that the minute is valid.
	if (minute < date.getActualMinimum(Calendar.MINUTE) || 
	    date.getActualMaximum(Calendar.MINUTE) + 1 < minute) {
	    throw new DateTimeException(String.format("The minute of %d is not valid.  " +
						      "It must be between %d and %d.",
						      minute,date.getActualMinimum(Calendar.MINUTE),
						      date.getActualMaximum(Calendar.MINUTE) + 1));
	}
	date.set(Calendar.MINUTE, minute);
	
	// Make sure that the second is valid.
	if (second < date.getActualMinimum(Calendar.SECOND) || 
	    date.getActualMaximum(Calendar.SECOND) + 1 < second) {
	    throw new DateTimeException(String.format("The second of %d is not valid.  " +
						      "It must be betweend %d and %d.",
						      second,date.getActualMinimum(Calendar.SECOND),
						      date.getActualMaximum(Calendar.SECOND) + 1));
	}
	date.set(Calendar.SECOND, second);
	
	// Make sure that the millisecond is valid.
	if (millisecond < date.getActualMinimum(Calendar.MILLISECOND) || 
	    date.getActualMaximum(Calendar.MILLISECOND) + 1 < millisecond) {
	    throw new DateTimeException(String.format("The millisecond of %d is not valid.  " +
						      "It must be between %d and %d.",
						      millisecond,date.getActualMinimum(Calendar.MILLISECOND),
						      date.getActualMaximum(Calendar.MILLISECOND) + 1));
	}
	date.set(Calendar.MILLISECOND, millisecond);
	
	// Everything is okay, so return the date.
	return date;
    }
    
    /**
     * Create a new date for the specified julian date and time zone.
     * @param year The year of the date.
     * @param julian The day of the year.
     * @param timeZone The time zone the date is in.
     * @return A Calendar that represents the date.
     * @throws DateTimeException if the day of the year is not with in 
     * the valid range for the year or the time zone is <code>null</code>.
     */
    public static Calendar buildJulianDate(int year, int julian, TimeZone timeZone)
	throws DateTimeException
    {
	return buildJulianDate(year, julian, 0, 0, 0, timeZone);
    }
    
    /**
     * Create a new date for the specified julian date, time, and time zone.
     * @param year The year of the date.
     * @param julian The day of the year.
     * @param hour The hour of the day.
     * @param minute The minute of the hour.
     * @param second The second of the minute.
     * @param timeZone The time zone the date is in.
     * @return A Calendar that represents the date.
     * @throws DateTimeException if the day of the year is not with in 
     * the valid range for the year, the hour is not a valid hour value, the 
     * minute is not a valid minute value, the second is not a valid second 
     * value, or the time zone is <code>null</code>. 
     */
    public static Calendar buildJulianDate(int year, int julian, int hour, 
					   int minute, int second, TimeZone timeZone) 
	throws DateTimeException
    {
	return buildJulianDate(year, julian, hour, minute, second, 0, timeZone);
    }
    
    /**
     * Create a new date for the specified julian date, time, and time zone.
     * @param year The year of the date.
     * @param julian The day of the year.
     * @param hour The hour of the day.
     * @param minute The minute of the hour.
     * @param second The second of the minute.
     * @param millisecond The milliseconds of the second
     * @param timeZone The time zone the date is in.
     * @return A Calendar that represents the date.
     * @throws DateTimeException if the day of the year is not with in 
     * the valid range for the year, the hour is not a valid hour value, the 
     * minute is not a valid minute value, the second is not a valid second 
     * value, the millisecond is not a valid millisecond value, or the time zone
     *  is <code>null</code>. 
     */
    public static Calendar buildJulianDate(int year, int julian, int hour, int minute,
					   int second, int millisecond, TimeZone timeZone) 
	throws DateTimeException
    {
	Calendar date = Calendar.getInstance();
	date.set(Calendar.YEAR, year);
	
	// Make sure that the day of the month is valid.
	if (julian < date.getActualMinimum(Calendar.DAY_OF_YEAR) || 
	    date.getActualMaximum(Calendar.DAY_OF_YEAR) < julian) {
	    throw new DateTimeException(String.format("The day of %d is not valid for year %d.  " +
						      "It must be between %d and %d.",
						      julian,year,date.getActualMinimum(Calendar.DAY_OF_YEAR),
						      date.getActualMaximum(Calendar.DAY_OF_YEAR)));
	}
	date.set(Calendar.DAY_OF_YEAR, julian);
	
	return buildDate(year, date.get(Calendar.MONTH) + 1, 
			 date.get(Calendar.DAY_OF_MONTH), hour, minute, second, 
			 millisecond, timeZone);
    }
    
    /**
     * Convert a value in one unit of time to another unit of time.
     * @param value The value to be converted.
     * @param inUnit The unit of time of the initial value.
     * @param outUnit The desired unit of time.
     * @return The value in the desired output units or <code>null</code> if 
     * the input value was <code>null</code>.  
     * @throws ConversionException if either the input or output unit values are 
     * <code>null</code>.
     */
    public static Double convertTime(Double value, TimeUnit inUnit, TimeUnit outUnit)
	throws ConversionException
    {
	// Just return the value if it is null
	if (value == null) { return null; }
	
	// Make sure the input unit is valid.
	if (inUnit == null) {
	    throw new ConversionException("convertTime","inUnit",
					  "The input unit was null.");
	}
	// Make sure the output unit is valid.
	if (outUnit == null) {
	    throw new ConversionException("convertTime","outUnit",
					  "The output unit was null.");
	}
	
	// If the input and output units are the same, don't convert and just
	// return the value.
	if (inUnit.equals(outUnit)) { return value; }
	
	// Convert the value to hours
	if (inUnit.equals(DAYS)) { value = value * 24; }
	else if (inUnit.equals(MINUTES)) { value = value / 60; }
	else if (inUnit.equals(SECONDS)) { value = value / 3600; }
	else {} // Already in hours
	
		// Convert the value (now in hours) to the output units.
	if (outUnit.equals(DAYS)) { value = value / 24; }
	else if (outUnit.equals(MINUTES)) { value = value * 60; }
	else if (outUnit.equals(SECONDS)) { value = value * 3600; }
	else {} // Already in hours
	
	return value;
    }
    
    /**
     * Create a new Calendar instance that represents the date in the specified time zone.
     * @param date The date to be shifted to the time zone.
     * @param timeZone The time zone to shift the date to.
     * @return A new Calendar for the data in the time zone or <code>null</code>
     * if the date is <code>null</code>.
     * @throws ConversionException if the time zone is <code>null</code>.
     */
    public static Calendar switchToTimeZone(Calendar date, TimeZone timeZone) 
	throws ConversionException
    {
	if (date == null) { return date; }
	if (timeZone == null) {
	    throw new ConversionException("switchToTimeZone","time zone",
					  "The time zone value was null.");
	}
	
	Calendar adjusted = Calendar.getInstance(timeZone);
	adjusted.setTime(date.getTime());
	return adjusted;
    }
    
    /**
     * The TimeUnit class is a MeasurementUnit for defining a unit of time.  
     * It is a public class to be available to all other classes, with the 
     * constructors being private to only allow the containing class to generate
     *  new instances of the class.
     */
    public class TimeUnit extends MeasurementUnit<TimeUnit> {
	
	/**
	 * Create a new instance of a TimeUnit.
	 * @param name The name of the time unit.
	 */
	private TimeUnit(String name) { super(name); }
    }
}
