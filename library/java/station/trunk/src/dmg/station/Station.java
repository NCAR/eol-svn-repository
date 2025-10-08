package dmg.station;

import static dmg.util.LengthUtils.*;
import static dmg.util.PositionUtils.*;
import static dmg.util.TimeUtils.*;
import dmg.util.*;
import dmg.util.LengthUtils.*;
import dmg.util.PositionUtils.*;

import java.io.*;
import java.util.*;

/**
 * <p>The Station class is the representation of a Station for the stationCD.out
 * formatted files.</p>
 * <p>The Station class implements a modified form of the Observable pattern.  
 * It is a Restrictor that prevents an event from occurring unless all of the 
 * Restrictors allow the event to occur.</p>
 * @see dmg.station.StationRestrictor
 *
 * @author Joel Clawson
 */
public class Station implements Comparable<Station> {
	
    private boolean commissioned, daylightSavings, mobile;
    private double utcOffset;
    private int accuracy, networkId, occurence, platformId, stateCode;
    private Calendar beginDate, endDate;
    private Double elevation, latitude, longitude;
    private List<StationRestrictor> restrictors;
    private String countyCode, country, description, frequency, networkName;
    private String stationId;
    
    /**
     * Create a new instance of a Station.
     */
    public Station() { 
	initialize();
	
	// Set these manually so the exceptions won't be thrown.
	stationId = "Station";
	networkName = "Network";
    }
    
    /**
     * Create a new instance of a Station.
     * @param stationId The identifier for the Station.
     * @param networkName The name of the network the Station is in.
     * @throws InvalidValueException if either the station id or network name
     * cannot be set.
     * @see #setNetworkName(String)
     * @see #setStationId(String)
     */
    public Station(String stationId, String networkName) throws InvalidValueException {
	initialize();
	
	try {
	    setStationId(stationId);
	    setNetworkName(networkName);
	}
	// This can never occur because a StationRestrictor can never have been 
	// added to the list to restrict the change.
	catch (RestrictedOperationException e) {}
    }
    
    /**
     * Initialize the station with the default values.
     */
    private void initialize() {
	restrictors = new ArrayList<StationRestrictor>();
	
	networkId = 99;
	platformId = 999;
	stateCode = 99;
	country = "XX";
    }
    
    /**
     * Add the specified StationRestrictor to the list to be tested when the 
     * Station changes.
     * @param restrictor The listener to be notified of Station changes.
     */
    public synchronized void addStationRestrictor(StationRestrictor restrictor){
	if (!restrictors.contains(restrictor)) { restrictors.add(restrictor); }
    }
    
    /**
     * Compare a Station to this Station for sort order.  It uses the default 
     * String representation to compare the Stations.
     * @param station The Station to compare to this Station.
     * @return A negative integer, zero, or a positive integer if this station 
     * is less than, equal to, or greater than the specified Station.
     */
    public int compareTo(Station station) { 
	return toString().compareTo(station.toString()); 
    }
    
    /**
     * Get the date that data began being collected for the Station.
     * The default value is <code>null</code>.
     * @return The earliest date the Station contains data.
     */
    public Calendar getBeginDate() { return beginDate; }
    
    /**
     * Get the county code for the Station.  The default value is
     * <code>null</code>.
     * @return The Station's county code.
     */
    public String getCountyCode() { return countyCode; }
    
    /**
     * Get the country where the Station is located.
     * @return The country where the Staton is located.
     */
    public String getCountry() { return country; }
    
    /**
     * Get the description of the Station.  The default value is 
     * <code>null</code>.
     * @return The Station's description.
     */
    public String getDescription() { return description; }
    
    /**
     * Get the elevation of the Station.  The default value is 
     * <code>null</code>.
     * @return The Station's elevation.
     */
    public Double getElevation() { return elevation; }
    
    /**
     * Get the date that data stopped being collected for the Station.
     * The default value is <code>null</code>.
     * @return The last date the Station contains data.
     */
    public Calendar getEndDate() { return endDate; }
    
    /**
     * Get the data reporting frequency at the Station.
     * @return The data reporting frequency.
     */
    public String getFrequency() { return frequency; }
    
    /**
     * Get the latitude of the station in degrees north of the equator.  The 
     * default value is <code>null</code>.
     * @return The latitude in degrees.
     */
    public Double getLatitude() { return latitude; }
    
    /**
     * Get the number of digits the latitude and longitude is accurate to.
     * @return The accuracy of the Station location.
     */
    public int getLocationAccuracy() { return accuracy; }
    
    /**
     * Get the longitude of the station in degrees east of the prime meridian.  
     * The default value is <code>null</code>.
     * @return The longitude in degrees.
     */
    public Double getLongitude() { return longitude; }
    
    /**
     * Get the identifier for the Station's network.  The default value is 
     * <code>99</code>.
     * @return The Station's network's identifier.
     */
    public int getNetworkId() { return networkId; }
    
    /**
     * Get the name of the network the Station is in.  The default value is 
     * <code>Network</code>.
     * @return The network name.
     */
    public String getNetworkName() { return networkName; }
    
    /**
     * Get the occurence count for the Station.
     * @return The occurence count.
     */
    public int getOccurence() { return occurence; }
    
    /**
     * Get the identifier for the platform for the Station.
     * @return The Station's platform identifier.
     */
    public int getPlatform() { return platformId; }
    
    /**
     * Get the code number that identifies the state where the Station is located.
     * @return The state code for the Station.
     */
    public int getStateCode() { return stateCode; }
    
    /**
     * Get the identifier of the Station.  The default value is 
     * <code>Station</code>.
     * @return The Station's identifier.
     */
    public String getStationId() { return stationId; }
    
    /**
     * Get the number of hours the Station is offset from UTC time.  The default
     * value is 0.
     * @return The hours offset from UTC.
     */
    public double getUTCoffset() { return utcOffset; }
    
    /**
     * Insert a date into the Station and adjust the begin and end dates accordingly.
     * @param date The date being inserted to the Station.
     * @throws DateTimeException if the date is <code>null</code>.
     */
    public void insertDate(Calendar date) throws DateTimeException {
	// Can't handle a null date, so throw an exception.
	if (date == null) {
	    throw new DateTimeException("The date to insertDate was null.");
	}
	
	// Determine if the specified date is earlier than the current 
	// begin date.
	if (getBeginDate() == null || date.compareTo(getBeginDate()) < 0) {
	    setBeginDate(date);
	}
	
	// Determine if the specified date is later than the current end date.
	if (getEndDate() == null || date.compareTo(getEndDate()) > 0) {
	    setEndDate(date);
	}
    }
    
    /**
     * Insert a date the Station collected data and adjust the begin and end 
     * dates accordingly.
     * @param year The year the Station collected data.
     * @param julian The day of the year the Station collected data.
     * @param timeZone The time zone the date is in (should be UTC).
     * @throws DateTimeException if the time zone is <code>null</code> or any of
     * the date values are out of their valid range.
     * @see dmg.util.TimeUtils#buildJulianDate(int, int, TimeZone)
     */
    public void insertDate(int year, int julian, TimeZone timeZone) 
	throws DateTimeException 
    {
	insertDate(buildJulianDate(year, julian, timeZone));
    }
    
    /**
     * Insert a date the Station collected data and adjust the begin and end 
     * dates accordingly.
     * @param year The year the Station collected data.
     * @param month The month (1-12) the Station collected data.
     * @param day The day the Station collected data.
     * @param timeZone The time zone the date is in (should be UTC).
     * @throws DateTimeException if the time zone is <code>null</code> or any of
     * the date values are out of their valid range.
     * @see dmg.util.TimeUtils#buildDate(int, int, int, TimeZone)
     */
    public void insertDate(int year, int month, int day, TimeZone timeZone) 
	throws DateTimeException 
    {
	insertDate(buildDate(year, month, day, timeZone));
    }
    
    /**
     * Determine if this Station is commissioned.  The default value is 
     * <code>false</code>.
     * @return <code>true</code> if the Station is commissioned, 
     * <code>false</code> if it is not.
     */
    public boolean isCommissioned() { return commissioned; }
    
    /**
     * Determine if this Station uses daylight savings time.  The default value 
     * is <code>false</code>.
     * @return <code>true</code> if the Station uses daylight savings time, 
     * <code>false</code> if does not.
     */
    public boolean isDaylightSavingsUsed() { return daylightSavings; }
    
    /**
     * Determine if this Station moves and does not remain in a fixed location.
     * The default value is <code>false</code>.
     * @return <code>true</code> if the Station moves, <code>false</code> if the
     * Station remains in a single location.
     */
    public boolean isMobile() { return mobile; }
    
    /**
     * Remove the specified restrictor from the list to be tested when the 
     * Station changes.
     * @param restrictor The restrictor that no longer needs to be tested.
     */
    public synchronized void removeStationRestrictor(
						     StationRestrictor restrictor) {
	restrictors.remove(restrictor);
    }
    
    /**
     * Set the date the Station began collecting data.
     * @param beginDate The earliest date the Station contains data.
     */
    public void setBeginDate(Calendar beginDate) { this.beginDate = beginDate; }
    
    /**
     * Set the date the Station began collecting data.
     * @param year THe year the Station began collecting data.
     * @param julian The day of the year.
     * @param timeZone THe time zone the date is in (should be UTC).
     * @throws DateTimeException if the time zone is <code>null</code> of if the
     * julian day is out of the valid range.
     * @see dmg.util.TimeUtils#buildJulianDate(int, int, TimeZone)
     */
    public void setBeginDate(int year, int julian, TimeZone timeZone) 
	throws DateTimeException 
    {
	setBeginDate(buildJulianDate(year, julian, timeZone));
    }
    
    /**
     * Set the date the Station began collecting data.
     * @param year The year the Station began collecting data.
     * @param month The month (1-12) the Station began collecting data.
     * @param day The day the Station began collecting data.
     * @param timeZone The time zone the date is in (should be UTC).
     * @throws DateTimeException if the time zone is <code>null</code> or any of
     * the date values are out of their valid range.
     * @see dmg.util.TimeUtils#buildDate(int, int, int, TimeZone)
     */
    public void setBeginDate(int year, int month, int day, TimeZone timeZone) 
	throws DateTimeException 
    {
	setBeginDate(buildDate(year, month, day, timeZone));
    }
    
    /**
     * Set the flag that marks the Station as commissioned.
     * @param flag The commissioned flag for the Station.
     */
    public void setCommissioned(boolean flag) {
	this.commissioned = flag;
    }
    
    /**
     * Set the county code for the county the Station is in.
     * @param countyCode The Station's county code.
     * @throws InvalidValueException if the county code is the empty String 
     * or is over 3 characters long.
     */
    public void setCountyCode(String countyCode) throws InvalidValueException  {
	if (countyCode != null && (countyCode.length() == 0 || countyCode.length() > 3)) {
	    throw new InvalidValueException("county code", countyCode,
					    "The county code must have a length between 1 and 3 " +
					    "characters.");
	}
	this.countyCode = countyCode;
    }
    
    /**
     * Set the flag that defines if the Station uses daylight savings time.
     * @param flag <code>true</code> if the Station uses daylight savings time,
     * <code>false</code> if it does not.
     */
    public void setDaylightSavingsUsed(boolean flag) {
	this.daylightSavings = flag;
    }
    
    /**
     * Set the description for the Station.
     * @param description The Station's description.
     * @throws InvalidValueException if the description is too long for its field.
     */
    public void setDescription(String description) throws InvalidValueException {
	if (description != null && (description.length() == 0 || description.length() > 46)) {
	    throw new InvalidValueException("description",
					    String.format("\"%s\"", description),
					    "The description is over 46 characters in length.");
	}
	this.description = description;
    }
    
    /**
     * Set the elevation of the Station.
     * @param elev The Station's elevation.
     * @param unit The unit of length of the elevation value.
     * @throws ConversionException if there is a problem converting the
     * specified elevation to meters.
     * @throws InvalidValueException if the elevation (in meters) will not 
     * fit in the field.
     * @throws RestrictedOperationException if a StationRestrictor will not 
     * allow the change in the elevation to the new value.
     */
    public void setElevation(Double elev, LengthUnit unit) throws 
	ConversionException, InvalidValueException, RestrictedOperationException 
    {
	// Convert the elevation value to meters.
	Double elevationMeters = convertLength(elev, unit, METERS);
	
	// Special case when both values are null.
	if (elevationMeters == null && elevation == null) { return; }
	
	// Make sure the converted elevation will fit in the field.
	if (elevationMeters != null && 
	    (Double.valueOf(String.format("%.1f",elevationMeters)) <= -9999.9 || 
	     9999999.9 < Double.valueOf(String.format("%.1f",elevationMeters)))) {
	    throw new InvalidValueException("elevation", elev, -9999.9, 9999999.9);
	}
	
	// Only apply the restrictions on ids that have different values.
	if ((elevation == null && elevationMeters != null) || 
	    (elevation != null && elevationMeters == null) || 
	    !elevationMeters.equals(elevation)) {
	    
	    // Apply the restrictions on all of the StationRestrictors
	    synchronized (this) {
		for (StationRestrictor restrictor: restrictors) {
		    restrictor.elevationChangeInProgress(this, elevation, 
							 elevationMeters);
		}
	    }
	    
	    // No restrictions were found, so the change can be finalized.
	    elevation = elevationMeters;
	}
    }
    
    /**
     * Set the final date data was collected for the Station.
     * @param endDate The date data was last collected for the Station.
     */
    public void setEndDate(Calendar endDate) { this.endDate = endDate; }
    
    /**
     * Set the date the Station finished collecting data.
     * @param year The year the Station finished collecting data.
     * @param julian The day of the year.
     * @param timeZone The time zone the date is in (should be UTC).
     * @throws DateTimeException if the time zone is <code>null</code> 
     * of if the julian day is out of the valid range.
     * @see dmg.util.TimeUtils#buildJulianDate(int, int, TimeZone)
     */
    public void setEndDate(int year, int julian, TimeZone timeZone) 
	throws DateTimeException 
    {
	setEndDate(buildJulianDate(year, julian, timeZone));
    }
    
    /**
     * Set the date the Station finished collecting data.
     * @param year The year the Station finished collecting data.
     * @param month The month (1-12) the Station finished collecting data.
     * @param day The day the Station finished collecting data.
     * @param timeZone The time zone the date is in (should be UTC).
     * @throws DateTimeException if the time zone is <code>null</code> or any of
     * the date values are out of their valid range.
     * @see dmg.util.TimeUtils#buildDate(int, int, int, TimeZone)
     */
    public void setEndDate(int year, int month, int day, TimeZone timeZone) 
	throws DateTimeException 
    {
	setEndDate(buildDate(year,month,day,timeZone));
    }
	
    /**
     * Set the frequency data is reported at the Station.
     * @param frequency The data reporting frequency.
     * @throws InvalidValueException if the frequency value is the empty String 
     * or is over 15 characters long.
     */
    public void setFrequency(String frequency) throws InvalidValueException  {
	if (frequency != null && (frequency.length() == 0 || frequency.length() > 15)) {
	    throw new InvalidValueException("frequency", frequency,
					    "The format must be between 0 and 15 characters long.");
	}
	this.frequency = frequency;
    }
    
    /**
     * Set the latitude for the station relative to north of the equator.
     * @param degrees The degrees of the latitude.
     * @throws InvalidValueWarning if the calculated latitude is not between 
     * -90 and 90 degrees.
     * @throws RestrictedOperationException if a StationRestrictor does not 
     * allow the latitude to be changed to the new value.
     * @see #setLatitude(Double, dmg.util.PositionUtils.LatitudeUnit)
     */
    public void setLatitude(Double degrees) 
	throws InvalidValueWarning, RestrictedOperationException 
    {
	try { setLatitude(degrees, NORTH); }		
	// This can never happen because a null degree is allowed and the case
	// where setLatitude(Double, LatitudeUnit) throws it for the direction
	// cannot happen unless there is a programming error.
	catch (ConversionException e) {}
    }
    
    /**
     * Set the latitude for the station relative to north of the equator.
     * @param degrees The degrees of the latitude.
     * @param minutes The minutes of the latitude.
     * @throws ConversionException if any of the arguments are <code>null</code>
     * or if the minutes value does not fall within the valid range for the value.
     * @throws InvalidValueWarning if the calculated latitude is not between 
     * -90 and 90 degrees.
     * @throws RestrictedOperationException if a StationRestrictor does not 
     * allow the latitude to be changed to the new value.
     * @see #setLatitude(Double, Double, Double)
     */
    public void setLatitude(Double degrees, Double minutes) 
	throws ConversionException, InvalidValueWarning, RestrictedOperationException 
    {
	setLatitude(degrees, minutes, 0.0);
    }
    
    /**
     * Set the latitude for the station.
     * @param degrees The degrees of the latitude.
     * @param direction The direction of the latitude relative to the equator.
     * @throws ConversionException if the direction is <code>null</code>.
     * @throws InvalidValueWarning if the calculated latitude is not between 
     * -90 and 90 degrees.
     * @throws RestrictedOperationException if a StationRestrictor does not
     * allow the latitude to be changed to the new value.
     */
    public void setLatitude(Double degrees, LatitudeUnit direction) 
	throws ConversionException, InvalidValueWarning, RestrictedOperationException 
    {
	if (degrees == null) { setLatitude(null, null, null, null); }
	else { setLatitude(degrees, 0.0, direction); }
    }
	
    /**
     * Set the latitude for the station relative to north of the equator.
     * @param degrees The degrees of the latitude.
     * @param minutes The minutes of the latitude.
     * @param seconds The seconds of the latitude.
     * @throws ConversionException if any of the arguments are <code>null</code>
     * or if the minutes or seconds value does not fall within the valid range 
     * for the value.
     * @throws InvalidValueException if the calculated latitude is not between 
     * -90 and 90 degrees.
     * @throws RestrictedOperationException if a StationRestrictor does not 
     * allow the latitude to be changed to the new value.
     * @see #setLatitude(Double, Double, Double, dmg.util.PositionUtils.LatitudeUnit)
     */
    public void setLatitude(Double degrees, Double minutes, Double seconds)
	throws ConversionException, InvalidValueWarning, RestrictedOperationException 
    {
	setLatitude(degrees, minutes, seconds, NORTH);
    }
	
    /**
     * Set the latitude for the station.
     * @param degrees The degrees of the latitude.
     * @param minutes The minutes of the latitude.
     * @param direction The direction of the latitude relative to the equator.
     * @throws ConversionException if any of the arguments are <code>null</code>
     * or the minutes does not fall within the valid range for the value.
     * @throws InvalidValueWarning if the calculated latitude is not between 
     * -90 and 90 degrees.
     * @throws RestrictedOperationException if a StationRestrictor does not 
     * allow the latitude to be changed to the new value.
     * @see #setLatitude(Double, Double, Double, dmg.util.PositionUtils.LatitudeUnit)
     */
    public void setLatitude(Double degrees, Double minutes, LatitudeUnit direction) 
	throws ConversionException, InvalidValueWarning, RestrictedOperationException 
    {
	setLatitude(degrees, minutes, 0.0, direction);
    }
    
    /**
     * Set the latitude for the station.
     * @param degrees The degrees of the latitude.
     * @param minutes The minutes of the latitude.
     * @param seconds The seconds of the latitude.
     * @param direction The direction of the latitude relative to the equator.
     * @throws ConversionException if any of the arguments are <code>null</code>
     * or the minutes or seconds value do not fall within the valid range for the value.
     * @throws InvalidValueWarning if the calculated latitude is not between 
     * -90 and 90 degrees.
     * @throws RestrictedOperationException if a StationRestrictor does not 
     * allow the latitude to be changed to the new value.
     * @see dmg.util.PositionUtils#toDegrees(Double, Double, Double, dmg.util.PositionUtils.LatitudeUnit)
     */
    public void setLatitude(Double degrees, Double minutes, Double seconds, LatitudeUnit direction)
	throws ConversionException, InvalidValueWarning, RestrictedOperationException 
    {
	Double updated = null;
	
	if (degrees != null || minutes != null || seconds != null || 
	    direction != null) {
	    // Calculate the new latitude value.
	    try { updated = toDegrees(degrees, minutes, seconds, direction); }
	    catch (InvalidValueWarning warn) {
		updated = (Double) warn.getValue();
		throw warn;
	    }
	}
	
	// Both values are null, so don't try to apply the restrictions.
	if (updated == null && latitude == null) { return; }
	
	// Only apply the restrictions on latitudes that are different values.
	if ((latitude == null && updated != null) || (latitude != null && updated == null) || 
	    !updated.equals(latitude)) {
	    
	    // Apply the restrictions on all of the StationRestrictors
	    synchronized (this) {
		for (StationRestrictor restrictor: restrictors) {
		    restrictor.latitudeChangeInProgress(this, latitude, updated);
		}
	    }
	    
	    // No restrictions were found, so the change can be finalized
	    latitude = updated;
	}
    }
    
    /**
     * Set the number of digits the latitude and longitude are accurate to.
     * @param accuracy The accuracy of the latitude and longitude.
     * @throws InvalidValueException if the accuracy is not between 0 and 9.
     */
    public void setLocationAccuracy(int accuracy) throws InvalidValueException {
	if (accuracy < 0 || accuracy > 9) {
	    throw new InvalidValueException("accuracy", accuracy, 0, 9);
	}
	this.accuracy = accuracy;
    }
    
    /**
     * Set the longitude for the station relative to north of the prime meridian.
     * @param degrees The degrees of the longitude.
     * @throws InvalidValueWarning if the calculated longitude is not between 
     * -180 and 180 degrees.
     * @throws RestrictedOperationException if a StationRestrictor does not 
     * allow the longitude to be changed to the new value.
     * @see #setLongitude(Double, dmg.util.PositionUtils.LongitudeUnit)
     */
    public void setLongitude(Double degrees) 
	throws InvalidValueWarning, RestrictedOperationException 
    {
	try { setLongitude(degrees, EAST); }		
	// This can never happen because a null degree is allowed and the case
	// where setLongitude(Double, LongitudeUnit) throws it for the direction
	// cannot happen unless there is a programming error.
	catch (ConversionException e) {}
    }
    
    /**
     * Set the longitude for the station relative to north of the prime  meridian.
     * @param degrees The degrees of the longitude.
     * @param minutes The minutes of the longitude.
     * @throws ConversionException if any of the arguments are <code>null</code>
     * or if the minutes value does not fall within the valid range for the value.
     * @throws InvalidValueWarning if the calculated longitude is not between 
     * -180 and 180 degrees.
     * @throws RestrictedOperationException if a StationRestrictor does not 
     * allow the longitude to be changed to the new value.
     * @see #setLongitude(Double, Double, Double)
     */
    public void setLongitude(Double degrees, Double minutes) 
	throws ConversionException, InvalidValueWarning, RestrictedOperationException 
    {
	setLongitude(degrees, minutes, 0.0);
    }
    
    /**
     * Set the longitude for the station.
     * @param degrees The degrees of the longitude.
     * @param direction The direction of the longitude relative to the prime meridian.
     * @throws ConversionException if the direction is <code>null</code>.
     * @throws InvalidValueWarning if the calculated longitude is not between 
     * -180 and 180 degrees.
     * @throws RestrictedOperationException if a StationRestrictor does not 
     * allow the longitude to be changed to the new value.
     */
    public void setLongitude(Double degrees, LongitudeUnit direction) 
	throws ConversionException, InvalidValueWarning, RestrictedOperationException 
    {
	if (degrees == null) { setLongitude(null, null, null, null); }
	else { setLongitude(degrees, 0.0, direction); }
    }
	
    /**
     * Set the longitude for the station relative to north of the prime meridian.
     * @param degrees The degrees of the longitude.
     * @param minutes The minutes of the longitude.
     * @param seconds The seconds of the longitude.
     * @throws ConversionException if any of the arguments are 
     * <code>null</code> or if the minutes or seconds value does not fall 
     * within the valid range for the value.
     * @throws InvalidValueWarning if the calculated longitude is not between 
     * -180 and 180 degrees.
     * @throws RestrictedOperationException if a StationRestrictor does not 
     * allow the longitude to be changed to the new value.
     * @see #setLongitude(Double, Double, Double, dmg.util.PositionUtils.LongitudeUnit)
     */
    public void setLongitude(Double degrees, Double minutes, Double seconds) 
	throws ConversionException, InvalidValueWarning, RestrictedOperationException 
    {
	setLongitude(degrees, minutes, seconds, EAST);
    }
	
    /**
     * Set the longitude for the station.
     * @param degrees The degrees of the longitude.
     * @param minutes The minutes of the longitude.
     * @param direction The direction of the longitude relative to the prime meridian.
     * @throws ConversionException if any of the arguments are 
     * <code>null</code> or the minutes does not fall within the valid range 
     * for the value.
     * @throws InvalidValueWarning if the calculated longitude is not between
     * -180 and 180 degrees.
     * @throws RestrictedOperationException if a StationRestrictor does not 
     * allow the longitude to be changed to the new value.
     * @see #setLongitude(Double, Double, Double, dmg.util.PositionUtils.LongitudeUnit)
     */
    public void setLongitude(Double degrees, Double minutes, LongitudeUnit direction)
	throws ConversionException, InvalidValueWarning, RestrictedOperationException 
    {
	setLongitude(degrees, minutes, 0.0, direction);
    }
    
    /**
     * Set the longitude for the station.
     * @param degrees The degrees of the longitude.
     * @param minutes The minutes of the longitude.
     * @param seconds The seconds of the longitude.
     * @param direction The direction of the longitude relative to the prime meridian.
     * @throws ConversionException if any of the arguments are <code>null</code>
     * or the minutes or seconds value do not fall within the valid range for the value.
     * @throws InvalidValueWarning if the calculated longitude is not between 
     * -180 and 180 degrees.
     * @throws RestrictedOperationException if a StationRestrictor does not 
     * allow the longitude to be changed to the new value.
     * @see dmg.util.PositionUtils#toDegrees(Double, Double, Double, dmg.util.PositionUtils.LongitudeUnit)
     */
    public void setLongitude(Double degrees, Double minutes, Double seconds, 
			     LongitudeUnit direction) 
	throws ConversionException, InvalidValueWarning, RestrictedOperationException 
    {
	Double changed = null;
	
	if (degrees != null || minutes != null || seconds != null || direction != null) {
	    // Calculate the new longitude value.
	    try {
		changed = toDegrees(degrees, minutes, seconds, direction);
	    } catch (InvalidValueWarning warn) {
		changed = (Double) warn.getValue();
		throw warn;
	    }
	}
	
	// Both values are null, so the restriction doesn't need be applied
	if (changed == null && longitude == null) { return; }
	
	// Only apply the restrictions on longitudes that are different values.
	if ((longitude == null && changed != null) || (longitude != null && changed == null) || 
	    !changed.equals(longitude)) {
	    
	    // Apply the restrictions on all of the StationRestrictors
	    synchronized (this) {
		for (StationRestrictor restrictor: restrictors) {
		    restrictor.longitudeChangeInProgress(this, longitude, changed);
		}
	    }
	    
	    // No restrictions were found, so the change can be finalized
	    longitude = changed;
	}
    }
    
    /**
     * Set the flag that marks the Station as mobile.
     * @param flag <code>true</code> if the Station moves, <code>false</code> 
     * if the Station remains in a single location.
     */
    public void setMobile(boolean flag) {
	this.mobile = flag;
    }
    
    /**
     * Set the identification for the Station's network.
     * @param networkId The network identification number.
     * @throws InvalidValueException if the network id number is not between
     * 0 and 9999.
     */
    public void setNetworkId(int networkId) throws InvalidValueException  {
	if (networkId < 0 || networkId > 9999) {
	    throw new InvalidValueException("network id", networkId, 0, 9999);
	}
	this.networkId = networkId;
    }
    
    /**
     * Set the name of the network the Station is in.
     * @param network The network's name.
     * @throws InvalidValueException if the network name is <code>null</code>.
     * @throws RestrictedOperationException if a StationRestrictor prevents 
     * the Station from changing the network name to the new network name.
     */
    public void setNetworkName(String network) 
	throws InvalidValueException, RestrictedOperationException 
    {
	// Make sure the network is not null.
	if (network == null) {
	    throw new InvalidValueException("network name", "null",
					    "The network name was null.");
	}
	
	if (network == null && networkName == null) { return; }
	
	// Only apply the restrictions on networks that have different values.
	if ((networkName == null && network != null) || 
	    (networkName != null && network == null) || 
	    !network.equals(networkName)) {
	    
	    // Apply the restrictions on all of the StationRestrictors
	    synchronized (this) {
		for (StationRestrictor restrictor: restrictors) {
		    restrictor.networkNameChangeInProgress(this, networkName, network);
		}
	    }
	    
	    // No restrictions were found, so the change can be finalized.
	    networkName = network;
	}
    }
    
    /**
     * Set the occurence count for the Station.
     * @param occurence The occurence count.
     * @throws InvalidValueException if the occurence is negative or over 999.
     */
    public void setOccurence(int occurence) throws InvalidValueException {
	if (occurence < 0 || occurence > 999) {
	    throw new InvalidValueException("occurence",occurence,0,999);
	}
	this.occurence = occurence;
    }
    
    /**
     * Set the identifier for the platform for the Station.
     * @param platformId The Station's platform identifier.
     * @throws InvalidValueException if the identifier is negative of over 9999.
     */
    public void setPlatform(int platformId) throws InvalidValueException {
	if (platformId < 0 || platformId > 9999) {
	    throw new InvalidValueException("platform", platformId, 0, 9999); 
	}
	this.platformId = platformId;
    }
    
    /**
     * Set the state and country where the Station is located.
     * @param country The country where the Station is located.
     * @param state The state where the Station is located.
     * @throws ConversionException if there is a problem finding the state 
     * code for the state/country pair.
     * @throws IOException if there is a problem reading the state code file.
     * @see dmg.util.StateCodeMap#getStateCode(String, String)
     */
    public void setState(String country, String state) 
	throws ConversionException, IOException {
	this.stateCode = StateCodeMap.getInstance().getStateCode(country, state);
	this.country = country;
    }
    
    /**
     * Set the identifier for the Station.
     * @param id The Station's identifier.
     * @throws InvalidValueException if the id is <code>null</code> or is 
     * over 15 characters long.
     * @throws RestrictedOperationException if a StationRestrictor prevents 
     * the Station identifier to be changed to the new value.
     */
    public void setStationId(String id) 
	throws InvalidValueException, RestrictedOperationException 
    {
	// Make sure the id is not null.
	if (id == null) {
	    throw new InvalidValueException("station id", "null", 
					    "The station id was null.");
	}
	// Make sure the id is not too big for the field.
	if (id.length() > 15) {
	    throw new InvalidValueException("station id", id,
					    String.format("The station id %s is longer than 15 characters.", id));
	}
	
	// Only apply the restrictions on ids that have different values.
	if ((stationId == null && id != null) || 
	    (stationId != null && id == null) || !id.equals(stationId)) {
	    
	    // Apply the restrictions on all of the StationRestrictors
	    synchronized (this) {
		for (StationRestrictor restrictor: restrictors) {
		    restrictor.stationIdChangeInProgress(this, stationId, id);
		}
	    }
	    
	    // No restrictions were found, so the change can be finalized.
	    stationId = id;
	}
    }
    
    /**
     * Set the number of hours the Station is offset from UTC.
     * @param offset The hours off from UTC.
     * @throws InvalidValueException if the offset does not fall within the expected range.
     */
    public void setUTCoffset(double offset) throws InvalidValueException {
	if (offset < -99.0 || offset > 99.0) {
	    throw new InvalidValueException("offset", offset, -99.0, 99.0);
	}
	this.utcOffset = offset;
    }
    
    /**
     * Create the String represenation of the Station.
     * @return The representation of the Station as a String in the Station List format.
     */
    @Override public String toString() {
	String out = String.format("%-15s %4d %10.5f %11.5f %3d %5d %-46s %-3s %-8s %-8s %-2s %02d %-3s %6.2f %-1s %4d %-15s %9.1f %1s",
				   getStationId(),
				   getNetworkId(),
				   getLatitude() == null ? -99.99999 : getLatitude(),
				   getLongitude() == null ? -999.99999 : getLongitude(),
				   getOccurence(),
				   getLocationAccuracy(),
				   getDescription() == null ? "Default Station Description" : 
				   getDescription(),
				   isCommissioned() ? "(Y)" : "(N)",
				   getBeginDate() == null ? "99999999" : 
				   String.format("%1$tY%1$tm%1$td",getBeginDate()),
				   getEndDate() == null ? "99999999" : 
				   String.format("%1$tY%1$tm%1$td",getEndDate()),
				   getCountry() == null ? "XX" : getCountry(),
				   getStateCode(),
				   getCountyCode() == null ? "???" : getCountyCode(),
				   getUTCoffset(),
				   isDaylightSavingsUsed() ? "y" : "n",
				   getPlatform(),
				   getFrequency() == null ? "xxxxxx" : getFrequency(),
				   getElevation() == null ? -9999.9 : getElevation(),
				   isMobile() ? "m" : "f"
				   );
	
	return out;
    }
}
