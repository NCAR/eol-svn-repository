package dmg.ua.sounding;

import static dmg.util.LengthUtils.*;
import static dmg.util.PositionUtils.*;
import static dmg.util.TimeUtils.*;
import dmg.station.*;
import dmg.util.*;
import dmg.util.LengthUtils.*;
import dmg.util.PositionUtils.*;

import java.util.*;

/**
 * <p>The Sounding class is the default definintion of a Sounding.  It only 
 * contains the common values for a Sounding and does not contain any quality
 * control values/flags.</p>
 *
 * @author Joel Clawson
 */
public class Sounding<T extends SoundingRecord> implements Comparable<Sounding<T>> {
    
    /**
     * A constant to use for the direction of radiosondes or other sondes 
     * released from the ground.
     */
    public static final String ASCENDING = "Ascending";
    
    /**
     * A constant to use for the direction of dropsondes.
     */
    public static final String DESCENDING = "Descending";
    
    /**
     * A constant to use for the direction of unknown sounding directions.
     */
    public static final String UNSPECIFIED = "Unspecified";
    
    private Calendar actualDate, nominalDate;
    private Double altitude, latitude, longitude;
    private List<T> records;
    private Map<Integer, HeaderEntry> headerLines;
    private Station station;
    private String direction, projectId, stationDescription, stationId, type;
    
    /**
     * Create a new instance of a Sounding.
     */
    public Sounding() { this(null); }
    
    /**
     * Create a new instance of a Sounding.
     * @param station The Station where the Sounding was released.
     */
    public Sounding(Station station) {
	this.station = station;
	headerLines = new TreeMap<Integer, HeaderEntry>();
	records = new ArrayList<T>();
    }
    
    /**
     * Add a record to the Sounding.
     * @param record The record to be added to the Sounding.
     */
    public void add(T record) { records.add(record); }
    
    /**
     * Remove the specified header line from the sounding.
     * @param index The index of the header line to be removed.
     */
    public void clearHeaderLine(int index) { headerLines.remove(index); }
    
    /**
     * Compare this Sounding with another sounding for sort order.  A Sounding 
     * is compared on the nominal date, latitude, longitude, station id, and 
     * data type in that order.  The comparison will only continue if the 
     * previous comparisions in the Sounding are equal.
     * @param sounding The Sounding to be compared with this Sounding.
     * @return A negative integer, zero, or positive integer if this sounding is
     * less than, equal to, or greater than the specified Sounding.
     * @see java.util.Comparable#compareTo(T)
     */
    public int compareTo(Sounding<T> sounding) {
	int value = ComparisonUtils.compare(getNominalDate(), sounding.getNominalDate());
	if (value == 0) {
	    value = ComparisonUtils.compare(getLatitude(), sounding.getLatitude());
	    if (value == 0) {
		value = ComparisonUtils.compare(getLongitude(), sounding.getLongitude());
		if (value == 0) {
		    value = ComparisonUtils.compare(getStationId(), sounding.getStationId());
		    if (value == 0) {
			value = ComparisonUtils.compare(getDataType(), 
							sounding.getDataType());
		    }
		}
	    }
	}
	return value;
    }
    
    /**
     * Get the actual release date and time of the sounding.
     * @return The actual release date and time of the sounding in UTC.
     */
    public Calendar getActualDate() { return actualDate; }
    
    /**
     * Get the altitude the sounding was released at.
     * @return The altitude in meters or <code>null</code> if the altitude is 
     * not set and not set in the Station.
     */
    public Double getAltitude() {
	return altitude == null && station != null ? station.getElevation() : altitude;
    }
    
    /**
     * Get the data type of the Sounding.  The default value is <code>null</code>.
     * @return The data type of the Sounding.
     */
    public String getDataType() { return type; }
    
    /**
     * Get the entry that contains the header line data.  The default value is
     * <code>null</code> if the line is not defined.
     * @param index The index of the header line to be retreived.
     * @return The entry containing the label and content of the header line.
     */
    public HeaderEntry getHeaderLine(int index) {
	return headerLines.get(index);
    }
    
    /**
     * Get the latitude of the sounding in degrees north of the equator.  The 
     * default value is <code>null</code>.
     * @return The latitude in degrees.
     */
    public Double getLatitude() {
	return latitude == null && station != null ? station.getLatitude() : latitude;
    }
    
    /**
     * Get the longitude of the sounding in degrees east of the prime meridian. 
     * The default value is <code>null</code>.
     * @return The longitude in degrees.
     */
    public Double getLongitude() { 
	return longitude == null && station != null ? station.getLongitude() : longitude;
    }
    
    /**
     * Get the nominal release date and time of the sounding.
     * @return The nominal release date and time of the sounding in UTC.
     */
    public Calendar getNominalDate() { return nominalDate; }
    
    /**
     * Get the project identifier for the Project that released the Sounding.  
     * The default value is <code>null</code>.
     * @return The project identifier for the Sounding.
     */
    public String getProjectId() { return projectId; }
    
    /**
     * Get the list of records for the Sounding.
     * @return The Sounding's records.
     */
    public List<T> getRecords() { return records; }
    
    /**
     * Get the direction the Sounding was released.  The default value is <code>null</code>.
     * @return The direction the Sounding was released.
     */
    public String getReleaseDirection() { return direction; }
	
    /**
     * Get the description of the Station that released the Sounding.
     * @return The Sounding's Station description.
     */
    public String getStationDescription() {
	return stationDescription == null && station != null ? 
	    station.getDescription() : stationDescription;
    }
    
    /**
     * Get the identifier of the Station which released the sounding.  The 
     * default value is <code>null</code>.
     * @return The station identifier which released the Sounding.
     */
    public String getStationId() {
	return stationId == null && station != null ? station.getStationId() : stationId;
    }
    
    /**
     * Set the actual release date and time of the sounding.  This will convert
     * the date and time to the UTC time zone.
     * @param actualDate The actual release date and time.
     * @see dmg.util.TimeUtils#switchToTimeZone(Calendar, TimeZone)
     */
    public void setActualRelease(Calendar actualDate) {
	try { this.actualDate = switchToTimeZone(actualDate, UTC); }
	// This can never occur because UTC is not null.
	catch (ConversionException e) {}
    }
	
    /**
     * Set the actual release date and time of the sounding.  This will convert
     * the date and time to the UTC time zone.
     * @param year The year of the actual release date and time.
     * @param julian The day of the year of the actual release date.
     * @param hour The hour of the release.
     * @param minute The minute of the release.
     * @param second The second of the release.
     * @param timeZone The time zone of the release.
     * @throws DateTimeException if the time zone is <code>null</code> or
     * one of the arguments is out of range.
     * @see dmg.util.TimeUtils#buildJulianDate(int, int, int, int, int, TimeZone)
     */
    public void setActualRelease(int year, int julian, int hour, int minute, 
				 int second, TimeZone timeZone) throws DateTimeException {
	setActualRelease(buildJulianDate(year, julian, hour, minute, second, 
					 timeZone));
    }
    
    /**
     * Set the actual release date and time of the sounding.  This will convert
     * the date and time to the UTC time zone.
     * @param year The year of the release.
     * @param month The month of the release.
     * @param day The day of the month of the release.
     * @param hour The hour of the release.
     * @param minute The minute of the release.
     * @param second The second of the release.
     * @param timeZone The time zone of the release.
     * @throws DateTimeException if the time zone is <code>null</code> or
     * one of the arguments is out of range.
     * @see dmg.util.TimeUtils#buildDate(int, int, int, int, int, int, TimeZone)
     */
    public void setActualRelease(int year, int month, int day, int hour, 
				 int minute, int second, TimeZone timeZone)
	throws DateTimeException 
    {
	setActualRelease(buildDate(year, month, day, hour, minute, second, timeZone));
    }
    
    /**
     * Set the release altitude of the sounding.
     * @param altitude The altitude the sounding was released at.
     * @param unit The unit of measurement of the altitude value.
     * @throws ConversionException if the length unit is <code>null</code>.
     * @see dmg.util.LengthUtils#convertLength(Double, dmg.util.LengthUtils.LengthUnit, dmg.util.LengthUtils.LengthUnit)
     */
    public void setAltitude(Double altitude, LengthUnit unit) throws ConversionException {
	this.altitude = convertLength(altitude, unit, METERS);
    }
    
    /**
     * Set the data type of the Sounding.  This is usually the network name.
     * @param type The data type for the Sounding.
     */
    public void setDataType(String type) { this.type = type; }
    
    /**
     * Set the information for a header line for the Sounding.
     * @param index The line number index for the line.
     * @param label The label for the header line.
     * @param content The content for the header line.
     * @throws InvalidValueException if the label is <code>null</code>.
     */
    public void setHeaderLine(int index, String label, String content) 
	throws InvalidValueException 
    {
	// The header line label cannot be null.
	if (label == null) {
	    throw new InvalidValueException("label", "null", 
					    "The label for the header line was null.");
	}
	
	headerLines.put(index, new HeaderEntry(label, content));
    }
    
    /**
     * Set the latitude for the sounding relative to north of the equator.
     * @param degrees The degrees of the latitude.
     * @throws InvalidValueWarning if the calculated latitude is not between 
     * -90 and 90 degrees.
     * @see #setLatitude(Double, dmg.util.PositionUtils.LatitudeUnit)
     */
    public void setLatitude(Double degrees) throws InvalidValueWarning {
	try { setLatitude(degrees, NORTH); }		
	// This can never happen because a null degree is allowed and the case
	// where setLatitude(Double, LatitudeUnit) throws it for the direction
	// cannot happen unless there is a programming error.
		catch (ConversionException e) {}
    }
    
    /**
     * Set the latitude for the sounding relative to north of the equator.
     * @param degrees The degrees of the latitude.
     * @param minutes The minutes of the latitude.
     * @throws ConversionException if any of the arguments are 
     * <code>null</code> or if the minutes value does not fall within the valid
     * range for the value.
     * @throws InvalidValueWarning if the calculated latitude is not 
     * between -90 and 90 degrees.
     * @see #setLatitude(Double, Double, Double)
     */
    public void setLatitude(Double degrees, Double minutes) 
	throws ConversionException, InvalidValueWarning 
    {
	setLatitude(degrees, minutes, 0.0);
    }
    
    /**
     * Set the latitude for the sounding.
     * @param degrees The degrees of the latitude.
     * @param direction The direction of the latitude relative to the equator.
     * @throws ConversionException if the direction is <code>null</code>.
     * @throws InvalidValueWarning if the calculated latitude is not between 
     * -90 and 90 degrees.
     */
    public void setLatitude(Double degrees, LatitudeUnit direction) 
	throws ConversionException, InvalidValueWarning 
    {
	if (degrees == null) { setLatitude(null, null, null, null); }
	else { setLatitude(degrees, 0.0, direction); }
    }
    
    /**
     * Set the latitude for the sounding relative to north of the equator.
     * @param degrees The degrees of the latitude.
     * @param minutes The minutes of the latitude.
     * @param seconds The seconds of the latitude.
     * @throws ConversionException if any of the arguments are 
     * <code>null</code> or if the minutes or seconds value does not fall 
     * within the valid range for the value.
     * @throws InvalidValueWarning if the calculated latitude is not between 
     * -90 and 90 degrees.
     * @see #setLatitude(Double, Double, Double, dmg.util.PositionUtils.LatitudeUnit)
     */
    public void setLatitude(Double degrees, Double minutes, Double seconds) 
	throws ConversionException, InvalidValueWarning 
    {
	setLatitude(degrees, minutes, seconds, NORTH);
    }
	
    /**
     * Set the latitude for the sounding.
     * @param degrees The degrees of the latitude.
     * @param minutes The minutes of the latitude.
     * @param direction The direction of the latitude relative to the equator.
     * @throws ConversionException if any of the arguments are 
     * <code>null</code> or the minutes does not fall within the valid range 
     * for the value.
     * @throws InvalidValueWarning if the calculated latitude is not between 
     * -90 and 90 degrees.
     * @see #setLatitude(Double, Double, Double, dmg.util.PositionUtils.LatitudeUnit)
     */
    public void setLatitude(Double degrees, Double minutes, LatitudeUnit direction)
	throws ConversionException, InvalidValueWarning 
    {
	setLatitude(degrees, minutes, 0.0, direction);
    }
    
    /**
     * Set the latitude for the sounding.
     * @param degrees The degrees of the latitude.
     * @param minutes The minutes of the latitude.
     * @param seconds The seconds of the latitude.
     * @param direction The direction of the latitude relative to the equator.
     * @throws ConversionException if any of the arguments are 
     * <code>null</code> or the minutes or seconds value do not fall within 
     * the valid range for the value.
     * @throws InvalidValueWarning if the calculated latitude is not between 
     * -90 and 90 degrees.
     * @see dmg.util.PositionUtils#toDegrees(Double, Double, Double, dmg.util.PositionUtils.LatitudeUnit)
     */
    public void setLatitude(Double degrees, Double minutes, Double seconds, 
			    LatitudeUnit direction) 
	throws ConversionException, InvalidValueWarning 
    {
	if (degrees != null || minutes != null || seconds != null || 
	    direction != null) {
	    // Calculate the new latitude value.
	    latitude = toDegrees(degrees, minutes, seconds, direction);
	} else {
	    latitude = null;
	}
    }
    
    /**
     * Set the longitude for the sounding relative to north of the prime meridian.
     * @param degrees The degrees of the longitude.
     * @throws InvalidValueWarning if the calculated longitude is not between 
     * -180 and 180 degrees.
     * @see #setLongitude(Double, dmg.util.PositionUtils.LongitudeUnit)
     */
    public void setLongitude(Double degrees) throws InvalidValueWarning {
	try { setLongitude(degrees, EAST); }		
	// This can never happen because a null degree is allowed and the case
	// where setLongitude(Double, LongitudeUnit) throws it for the direction
	// cannot happen unless there is a programming error.
	catch (ConversionException e) {}
    }
    
    /**
     * Set the longitude for the sounding relative to north of the prime meridian.
     * @param degrees The degrees of the longitude.
     * @param minutes The minutes of the longitude.
     * @throws ConversionException if any of the arguments are 
     * <code>null</code> or if the minutes value does not fall within the 
     * valid range for the value.
     * @throws InvalidValueWarning if the calculated longitude is not between 
     * -180 and 180 degrees.
     * @see #setLongitude(Double, Double, Double)
     */
    public void setLongitude(Double degrees, Double minutes) 
	throws ConversionException, InvalidValueWarning 
    {
	setLongitude(degrees, minutes, 0.0);
    }
    
    /**
     * Set the longitude for the sounding.
     * @param degrees The degrees of the longitude.
     * @param direction The direction of the longitude relative to the prime meridian.
     * @throws ConversionException if the direction is <code>null</code>.
     * @throws InvalidValueWarning if the calculated longitude is not between 
     * -180 and 180 degrees.
     */
    public void setLongitude(Double degrees, LongitudeUnit direction)
	throws ConversionException, InvalidValueWarning 
    {
	if (degrees == null) { setLongitude(null, null, null, null); }
	else { setLongitude(degrees, 0.0, direction); }
    }
    
    /**
     * Set the longitude for the sounding relative to north of the prime meridian.
     * @param degrees The degrees of the longitude.
     * @param minutes The minutes of the longitude.
     * @param seconds The seconds of the longitude.
     * @throws ConversionException if any of the arguments are 
     * <code>null</code> or if the minutes or seconds value does not fall
     * within the valid range for the value.
     * @throws InvalidValueWarning if the calculated longitude is not between 
     * -180 and 180 degrees.
     * @see #setLongitude(Double, Double, Double, dmg.util.PositionUtils.LongitudeUnit)
     */
    public void setLongitude(Double degrees, Double minutes, Double seconds) 
	throws ConversionException, InvalidValueWarning 
    {
	setLongitude(degrees, minutes, seconds, EAST);
    }
    
    /**
     * Set the longitude for the sounding.
     * @param degrees The degrees of the longitude.
     * @param minutes The minutes of the longitude.
     * @param direction The direction of the longitude relative to the prime meridian.
     * @throws ConversionException if any of the arguments are 
     * <code>null</code> or the minutes does not fall within the valid range 
     * for the value.
     * @throws InvalidValueWarning if the calculated longitude is not between 
     * -180 and 180 degrees.
     * @see #setLongitude(Double, Double, Double, dmg.util.PositionUtils.LongitudeUnit)
     */
    public void setLongitude(Double degrees, Double minutes, LongitudeUnit direction)
	throws ConversionException, InvalidValueWarning 
    {
	setLongitude(degrees, minutes, 0.0, direction);
    }
    
    /**
     * Set the longitude for the sounding.
     * @param degrees The degrees of the longitude.
     * @param minutes The minutes of the longitude.
     * @param seconds The seconds of the longitude.
     * @param direction The direction of the longitude relative to the prime meridian.
     * @throws ConversionException if any of the arguments are 
     * <code>null</code> or the minutes or seconds value do not fall within 
     * the valid range for the value.
     * @throws InvalidValueWarning if the calculated longitude is not between
     * -180 and 180 degrees.
     * @see dmg.util.PositionUtils#toDegrees(Double, Double, Double, dmg.util.PositionUtils.LongitudeUnit)
     */
    public void setLongitude(Double degrees, Double minutes, Double seconds, 
			     LongitudeUnit direction) 
	throws ConversionException, InvalidValueWarning 
    {
	if (degrees != null || minutes != null || seconds != null || direction != null) {
	    // Calculate the new longitude value.
	    longitude = toDegrees(degrees, minutes, seconds, direction);
	} else {
	    longitude = null;
	}
    }
    
    /**
     * Set the nominal release date and time of the sounding.  This will convert
     * the date and time to the UTC time zone.
     * @param nominalDate The nominal release date and time.
     * @see dmg.util.TimeUtils#switchToTimeZone(Calendar, TimeZone)
     */
    public void setNominalRelease(Calendar nominalDate) {
	try { this.nominalDate = switchToTimeZone(nominalDate, UTC); }
	// This can never occur because UTC is not null.
	catch (ConversionException e) {}
    }
    
    /**
     * Set the nominal release date and time of the sounding.  This will convert
     * the date and time to the UTC time zone.
     * @param year The year of the nominal release date and time.
     * @param julian The day of the year of the nominal release date.
     * @param hour The hour of the release.
     * @param minute The minute of the release.
     * @param second The second of the release.
     * @param timeZone The time zone of the release.
     * @throws DateTimeException if the time zone is <code>null</code> or
     * one of the arguments is out of range.
     * @see dmg.util.TimeUtils#buildJulianDate(int, int, int, int, int, TimeZone)
     */
    public void setNominalRelease(int year, int julian, int hour, int minute, 
				  int second, TimeZone timeZone) throws DateTimeException 
    {
	setNominalRelease(buildJulianDate(year, julian, hour, minute, second, timeZone));
    }
    
    /**
     * Set the nominal release date and time of the sounding.  This will convert
     * the date and time to the UTC time zone.
     * @param year The year of the release.
     * @param month The month of the release.
     * @param day The day of the month of the release.
     * @param hour The hour of the release.
     * @param minute The minute of the release.
     * @param second The second of the release.
     * @param timeZone The time zone of the release.
     * @throws DateTimeException if the time zone is <code>null</code> or
     * one of the arguments is out of range.
     * @see dmg.util.TimeUtils#buildDate(int, int, int, int, int, int, TimeZone)
     */
    public void setNominalRelease(int year, int month, int day, int hour, 
				  int minute, int second, TimeZone timeZone) 
	throws DateTimeException 
    {
	setNominalRelease(buildDate(year, month, day, hour, minute, second, timeZone));
    }
    
    /**
     * Set the identifier for the project that the Sounding was released for.
     * @param projectId The project that released the Sounding.
     */
    public void setProjectId(String projectId) { this.projectId = projectId; }
    
    /**
     * Set the direction the Sounding was released in.
     * @param direction The release direction of the Sounding.
     * @see #ASCENDING
     * @see #DESCENDING
     * @see #UNSPECIFIED
     */
    public void setReleaseDirection(String direction) { 
	this.direction = direction;
    }
    
    /**
     * Set the description of the Station that released the Sounding.
     * @param description The Station's description.
     */
    public void setStationDescription(String description) {
	this.stationDescription = description;
    }
    
    /**
     * Set the identifier for the location where the Sounding was released.
     * @param stationId The Station identifier where the Station was released.
     */
    public void setStationId(String stationId) { this.stationId = stationId; }
    
    /**
     * Sort the records in this sounding using the specified sorting algorithm.
     * @param comparator The comparator containing the sorting algorithm.
     */
    public void sortRecord(SoundingRecordComparator<T> comparator) {
    	Collections.sort(records, comparator);
    }
    
    
    /**
     * The HeaderEntry class is a container for holding the parts of a header 
     * line.
     *
     * @author Joel Clawson
     */
    public class HeaderEntry {
	
	private String label, content;
	
	/**
	 * Create a new instance of a HeaderEntry.
	 * @param label The label of the header line.
	 * @param content The content of the header line.
	 */
	private HeaderEntry(String label, String content) {
	    this.label = label;
	    this.content = content;
	}
	
	/**
	 * Get the header line content in this entry.
	 * @return The header line content.
	 */
	public String getContent() { return content; }
	
	/**
	 * Get the header line label in this entry.
	 * @return The header line label.
	 */
	public String getLabel() { return label; }
    }
}
