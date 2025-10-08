package dmg.ua.sounding;

import static dmg.record.PTHData.*;
import static dmg.util.ComparisonUtils.compare;
import static dmg.util.PositionUtils.*;
import static dmg.util.TimeUtils.*;
import static dmg.util.VelocityUtils.*;
import dmg.util.*;
import dmg.util.LengthUtils.*;
import dmg.util.PositionUtils.*;
import dmg.util.PressureUtils.*;
import dmg.util.TemperatureUtils.*;
import dmg.util.VelocityUtils.*;
import java.util.*;

/**
 * <p>The SoundingRecord class is a basic record for a single level of a
 * Sounding.  While the class is not abstract, it should be extended to
 * create specific types of SoundingRecords.</p>
 *
 * @author Joel Clawson
 */
public abstract class SoundingRecord extends Observable implements Observer {

    /**
     * The constant used by notifyObservers when the Ascent Rate has changed.
     */
    public static final String ASCENT_RATE = "ascent_rate";
    
    /**
     * The constant used by notifyObservers when the Latitude has changed.
     */
    public static final String LATITUDE = "latitude";
    
    /**
     * The constant used by notifyObservers when the Longitude has changed.
     */
    public static final String LONGITUDE = "longitude";
    
    /**
     * The constant used by notifyObservers when the Time has changed.
     */
    public static final String TIME = "time";
    
    private boolean calculationsAllowed, canCalculateAscentRate;
    private Double ascentRate, latitude, longitude, time;
    private SoundingPTHData pth;
    private SoundingRecord previousRecord;
    private SoundingWindData winds;
    
    /**
     * Create a new instance of a SoundingRecord.
     * @param pth The PTHData block to use for this record.
     * @param winds The WindData block to use for the record.
     * @param calculationsAllowed A flag to prevent values from being automatically
     * be calculated.
     */
    public SoundingRecord(SoundingPTHData pth, SoundingWindData winds, 
			  boolean calculationsAllowed) {
	this.pth = pth;
	this.winds = winds;
	canCalculateAscentRate = true;
	this.calculationsAllowed = calculationsAllowed;
	
	pth.addObserver(this);
	winds.addObserver(this);
	this.addObserver(this);
    }
    
    /**
     * Create a new instance of a SoundingRecord.
     * @param pth The PTHData block to use for this record.
     * @param winds The WindData block to use for the record.
     * @param calculationsAllowed A flag to prevent values from being automatically
     * be calculated.
     * @param previousRecord The SoundingRecord that occurred just before this 
     * record.
     */
    public SoundingRecord(SoundingPTHData pth, SoundingWindData winds, 
			  boolean calculationsAllowed, SoundingRecord previousRecord)
    {
	this(pth, winds, calculationsAllowed);
	if (previousRecord != null) {
	    this.previousRecord = previousRecord;
	    this.previousRecord.addObserver(this);
	}
    }
    
    /**
     * Get the altitude for the SoundingRecord.  The default value is <code>null</code>.
     * @return The elevation in meters.
     * @see dmg.record.PTHData#getElevation()
     */
    public Double getAltitude() { return pth.getElevation(); }
	
    /**
     * Get the ascent rate for the SoundingRecord.  The default value is <code>null</code>.
     * @return The ascent rate in m/s.
     */
    public Double getAscentRate() { return ascentRate; }
	
    /**
     * Get the dew point for the SoundingRecord.  This will be a calculated
     * value whenever possible and has not been set manually.  The default
     * value is <code>null</code>.
     * @return The dew point in &deg;C.
     * @see dmg.record.PTHData#getDewPoint()
     */
    public Double getDewPoint() { return pth.getDewPoint(); }

    /**
     * Get the latitude where the SoundingRecord was measured.  The default
     * value is <code>null</code>.
     * @return The latitude in degrees North of the equator.
     */
    public Double getLatitude() { return latitude; }
	
    /**
     * Get the longitude where the SoundingRecord was taken.  The default
     * value is <code>null</code>.
     * @return The longitude in degrees East of the prime meridian.
     */
    public Double getLongitude() { return longitude; }
    
    /**
     * Get the pressure for the SoundingRecord.  The default value is <code>null</code>.
     * @return The pressure in mbars.
     * @see dmg.record.PTHData#getPressure()
     */
    public Double getPressure() { return pth.getPressure(); }
    
    /**
     * Get the relative humidity for the SoundingRecord.  This will be a
     * calculated value whenever possible and has not been set manually.  The
     * default value is <code>null</code>.
     * @return The relative humidity in percent.
     * @see dmg.record.PTHData#getRelativeHumidity()
     */
    public Double getRelativeHumidity() { return pth.getRelativeHumidity(); }
    
    /**
     * Get the temperature for the SoundingRecord.  The default value is
     * <code>null</code>.
     * @return The temperature in &deg;C.
     * @see dmg.record.PTHData#getTemperature()
     */
    public Double getTemperature() { return pth.getTemperature(); }
    
    /**
     * Get the time after release for the SoundingRecord.  The default value is
     * <code>null</code>.
     * @return The time after release in seconds.
     */
    public Double getTime() { return time; }
    
    /**
     * Get the U wind component for the SoundingRecord.  The default value is
     * <code>null</code>.
     * @return The U wind component in m/s.
     * @see dmg.record.WindData#getUComponent()
     */
    public Double getUComponent() { return winds.getUComponent(); }
    
    /**
     * Get the V wind component for the SoundingRecord.  The default value is
     * <code>null</code>.
     * @return The V wind component in m/s.
     * @see dmg.record.WindData#getVComponent()
     */
    public Double getVComponent() { return winds.getVComponent(); }
    
    /**
     * Get the wind direction for the SoundingRecord.  The default value is
     * <code>null</code>.
     * @return The wind direction in degrees.
     */
    public Double getWindDirection() { return winds.getWindDirection(); }
    
    /**
     * Get the wind speed for the SoundingRecord.  The default value is
     * <code>null</code>.
     * @return The wind direction in m/s.
     */
    public Double getWindSpeed() { return winds.getWindSpeed(); }
    
    /**
     * Set the altitude for the SoundingRecord.
     * @param altitude The altitude value.
     * @param unit THe LengthUnit of measurement of the altitude value.
     * @throws CalculationWarning if there is a problem calculating an associated value
     * caused by the altitude change.
     * @throws ConversionException if there is a problem converting the altitude to METERS.
     * @throws InvalidValueWarning if the altitude is not valid for a sounding.
     * @see dmg.record.PTHData#setElevation(Double, dmg.util.LengthUtils.LengthUnit)
     */
    public void setAltitude(Double altitude, LengthUnit unit) 
	throws CalculationWarning, ConversionException, InvalidValueWarning 
    {
	pth.setElevation(altitude, unit);
    }
    
    /**
     * Set the ascent rate for the SoundingRecord.  Setting the ascent rate
     * to <code>null</code> will allow the record to attempt to calculate the
     * ascent rate value.
     * @param rate The ascent rate.
     * @param unit The VelocityUnit of measurement for the ascent rate value.
     * @throws CalculationWarning if there is a problem calculating an associated value
     * caused by the ascent rate change.
     * @throws ConversionException if there is a problem converting the ascent rate to m/s.
     * @throws InvalidValueWarning if the ascent rate is not valid for a sounding.
     */
    public void setAscentRate(Double rate, VelocityUnit unit) 
	throws CalculationWarning, ConversionException, InvalidValueWarning 
    {
	Double newRate = convertVelocity(rate, unit, METERS_PER_SECOND);
	
	Double validRate = validateAscentRate(newRate);
	
	StringBuffer exceptionMessages = new StringBuffer();
	
	if (compare(this.ascentRate, validRate) != 0) {
	    this.ascentRate = validRate;
	    setChanged();
	    notifyObservers(ASCENT_RATE);
	}
	
	canCalculateAscentRate = (this.ascentRate == null);
	
	try { updateAscentRate(); }
	catch (CalculationException e) {
	    exceptionMessages.append("\n").append(e.getMessage());
	}
	
	if (validRate == null && newRate != null) {
	    throw new InvalidValueWarning("ascent rate", rate,
					  "The ascent rate is not an allowed ascent rate." +
					  exceptionMessages.toString());
	}
	
	if (exceptionMessages.length() != 0) {
	    throw new CalculationWarning(exceptionMessages.substring(1));
	}
    }
    
    /**
     * Set the dew point for the SoundingRecord.  Setting the dew point to
     * <code>null</code> will allow the record to attempt to calculate the
     * dew point value.
     * @param dewPoint The dew point value.
     * @param unit The TemperatureUnit of measurement of the dew point value.
     * @throws CalculationWarning if there is a problem calculating an associated value
     * caused by the dew point change.
     * @throws ConversionException if there is a problem converting the dew point to &deg;C.
     * @throws InvalidValueWarning if the dew point is not valid for a sounding.
     * @see dmg.record.PTHData#setDewPoint(Double, dmg.util.TemperatureUtils.TemperatureUnit)
     */
    public void setDewPoint(Double dewPoint, TemperatureUnit unit) 
	throws CalculationWarning, ConversionException, InvalidValueWarning 
    {
	pth.setDewPoint(dewPoint, unit);
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
     * @throws InvalidValueWarning if the calculated latitude is not between 
     * -90 and 90 degrees.
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
     *  -90 and 90 degrees.
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
     * <code>null</code> or the minutes or seconds value do not fall within the
     *  valid range for the value.
     * @throws InvalidValueWarning if the calculated latitude is not between 
     * -90 and 90 degrees.
     * @see dmg.util.PositionUtils#toDegrees(Double, Double, Double, dmg.util.PositionUtils.LatitudeUnit)
     */
    public void setLatitude(Double degrees, Double minutes, Double seconds, 
			    LatitudeUnit direction)
	throws ConversionException, InvalidValueWarning 
    {
	if (degrees != null || minutes != null || seconds != null || direction != null) {
	    // Calculate the new latitude value.
	    latitude = toDegrees(degrees, minutes, seconds, direction);
	} else {
	    latitude = null;
	}
	setChanged();
	notifyObservers(LATITUDE);
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
     * @throws ConversionException if any of the arguments are <code>null</code> or if the
     * minutes value does not fall within the valid range for the value.
     * @throws InvalidValueWarning if the calculated longitude is not between
     *  -180 and 180 degrees.
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
     *  -180 and 180 degrees.
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
     *  -180 and 180 degrees.
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
     * <code>null</code> or the minutes does not fall within the valid range for the value.
     * @throws InvalidValueWarning if the calculated longitude is not between
     *  -180 and 180 degrees.
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
	    // Calculate the new longitude value
	    longitude = toDegrees(degrees, minutes, seconds, direction);
	} else {
	    longitude = null;
	}
	setChanged();
	notifyObservers(LONGITUDE);
    }
    
    /**
     * Set the pressure for the SoundingRecord.
     * @param pressure The pressure value.
     * @param unit The PressureUnit of measurement for the value.
     * @throws CalculationWarning if there is a problem calculating an associated value
     * caused by the pressure change.
     * @throws ConversionException if there is a problem converting the pressure to mbar.
     * @throws InvalidValueWarning if the pressure is not valid for a sounding.
     * @see dmg.record.PTHData#setPressure(Double, dmg.util.PressureUtils.PressureUnit)
     */
    public void setPressure(Double pressure, PressureUnit unit) 
	throws CalculationWarning, ConversionException, InvalidValueWarning 
    {
	pth.setPressure(pressure, unit);
    }
    
    /**
     * Set the relative humidity for the SoundingRecord.  Setting the relative
     * humidity to <code>null</code> will allow the record to attempt to 
     * calculate the relative humidity value.
     * @param rh The relative humidity value.
     * @throws CalculationWarning if there is a problem calculating an associated value
     * caused by the rh change.
     * @throws InvalidValueWarning if the rh is not valid for a sounding.
     * @see dmg.record.PTHData#setRelativeHumidity(Double)
     */
    public void setRelativeHumidity(Double rh) throws CalculationWarning, InvalidValueWarning {
	pth.setRelativeHumidity(rh);
    }
    
    /**
     * Set the temperature for the SoundingRecord.
     * @param temperature The temperature value.
     * @param unit The TemperatureUnit of measurement for the value.
     * @throws CalculationWarning if there is a problem calculating an associated value
     * caused by the temperature change.
     * @throws ConversionException if there is a problem converting the temperature to &deg;C.
     * @throws InvalidValueWarning if the temperature is not valid for a sounding.
     * @see dmg.record.PTHData#setTemperature(Double, dmg.util.TemperatureUtils.TemperatureUnit)
     */
    public void setTemperature(Double temperature, TemperatureUnit unit) 
	throws CalculationWarning, ConversionException, InvalidValueWarning 
    {
	pth.setTemperature(temperature, unit);
    }
    
    /**
     * Set the time after release for the SoundingRecord.
     * @param time The time in seconds after release.
     * @throws CalculationWarning if there is a problem calculating an associated value
     * caused by the time change.
     * @throws InvalidValueWarning if the time is not valid for a sounding.
     */
    public void setTime(Double time) throws CalculationWarning, InvalidValueWarning {
	Double validTime = validateTime(time);
	
	StringBuffer exceptionMessages = new StringBuffer();
	
	if (compare(this.time, validTime) != 0) {
	    this.time = validTime;
	    setChanged();
	    notifyObservers(TIME);
	}
	
	try { updateAscentRate(); }
	catch (CalculationException e) {
	    exceptionMessages.append("\n").append(e.getMessage());
	}
	
	if (validTime == null && time != null) {
	    throw new InvalidValueWarning("time", time,
					  "The time is not an allowed time." +
					  exceptionMessages.toString());
	}
	
	if (exceptionMessages.length() != 0) {
	    throw new CalculationWarning(exceptionMessages.substring(1));
	}
    }
    
    /**
     * Set the time after release for the SoundingRecord.
     * @param minutes The number of minutes after release.
     * @param seconds The number of seconds after the minute.
     * @throws CalculationWarning if there is a problem calculating an associated value
     * caused by the time change.
     * @throws ConversionException if there is a problem converting the time to seconds.
     * @throws InvalidValueWarning if the time is not valid for a sounding.
     */
    public void setTime(Double minutes, Double seconds) 
	throws CalculationWarning, ConversionException, InvalidValueWarning 
    {
	if (minutes == null) {
	    throw new ConversionException("setTime", "minutes",
					  "The minutes were null.");
	}
	if (seconds == null) {
	    throw new ConversionException("setTime", "seconds",
					  "The seconds were null.");
	}
	setTime(convertTime(minutes, MINUTES, SECONDS) + seconds);
    }
    
    /**
     * Set the U wind component for the Sounding Record.  Setting the U and V 
     * wind components to <code>null</code> will allow the record to attempt to
     * calculate the U and V components.
     * @param uComponent The U wind component value.
     * @param unit The VelocityUnit of measurement for the value.
     * @throws CalculationWarning if there is a problem calculating an associated value
     * caused by the U component change.
     * @throws ConversionException if there is a problem converting the u component to m/s.
     * @throws InvalidValueWarning if the u component is not valid for a sounding.
     * @see dmg.record.WindData#setUComponent(Double, dmg.util.VelocityUtils.VelocityUnit)
     */
    public void setUComponent(Double uComponent, VelocityUnit unit) 
	throws CalculationWarning, ConversionException, InvalidValueWarning 
    {
	winds.setUComponent(uComponent, unit);
    }
    
    /**
     * Set the V wind component for the Sounding Record.  Setting the U and V
     * wind components to <code>null</code> will allow the record to attempt to
     * calculate the U and V components.
     * @param vComponent The V wind component value.
     * @param unit The VelocityUnit of measurement for the value.
     * @throws CalculationWarning if there is a problem calculating an associated value
     * caused by the V component change.
     * @throws ConversionException if there is a problem converting the V component to m/s.
     * @throws InvalidValueWarning if the V component is not valid for a sounding.
     * @see dmg.record.WindData#setVComponent(Double, dmg.util.VelocityUtils.VelocityUnit)
     */
    public void setVComponent(Double vComponent, VelocityUnit unit) 
	throws CalculationWarning, ConversionException, InvalidValueWarning 
    {
	winds.setVComponent(vComponent, unit);
    }
    
    /**
     * Set the wind direction for the SoundingRecord.  Setting the wind speed 
     * and direction to <code>null</code> will allow the record to attempt to 
     * calculate the wind speed and direction.
     * @param direction The wind direction value in degrees.
     * @throws CalculationWarning if there is a problem calculating an associated value
     * caused by the wind direction change.
     * @throws InvalidValueWarning if the wind direction is not valid for a sounding.
     * @see dmg.record.WindData#setWindDirection(Double)
     */
    public void setWindDirection(Double direction) 
	throws CalculationWarning, InvalidValueWarning {
	winds.setWindDirection(direction);
    }
    
    /**
     * Set the wind speed for the SoundingRecord.  Setting the wind speed and
     * direction to <code>null</code> will allow the record to attempt to 
     * calculate the wind speed and direction.
     * @param speed The wind speed value.
     * @param unit The VelocityUnit of measurement for the value.
     * @throws CalculationWarning if there is a problem calculating an associated value
     * caused by the wind speed change.
     * @throws ConversionException if there is a problem converting the wind speed to m/s.
     * @throws InvalidValueWarning if the wind speed is not valid for a sounding.
     * @see dmg.record.WindData#setWindSpeed(Double, dmg.util.VelocityUtils.VelocityUnit)
     */
    public void setWindSpeed(Double speed, VelocityUnit unit) 
	throws CalculationWarning, ConversionException, InvalidValueWarning 
    {
	winds.setWindSpeed(speed, unit);
    }
    
    /**
     * Receive notifications from watched items through the Observer interface.
     * @param watched The item being watched.
     * @param key The key specifying what has changed.
     */
    public void update(Observable watched, Object key) {
	if (watched.equals(pth) || watched.equals(winds)) {
	    setChanged();
	    notifyObservers(key);
	} else if (watched.equals(previousRecord) || watched.equals(this)) {
	    if (key.equals(ELEVATION) || key.equals(TIME)) {
		// Can't do anything here
		try { updateAscentRate(); }
		catch (DefaultException e) {}
	    }
	}
    }
    
    /**
     * Update the ascent rate if possible.
     * @throws CalculationException if there is a problem calculating the ascent rate.
     */
    private void updateAscentRate() throws CalculationException {
	if (calculationsAllowed && canCalculateAscentRate) {
	    CalculationException exception = null;
	    
	    if (previousRecord != null && previousRecord.getTime() != null &&
		this.time != null && previousRecord.getTime() != this.time 
		&& previousRecord.getAltitude() != null && this.getAltitude() != null) 
		{
		    this.ascentRate = (this.getAltitude() - previousRecord.getAltitude()) / 
			(this.time - previousRecord.getTime());
		} 
	    else {
		this.ascentRate = null;
	    }
	    
	    this.ascentRate = validateAscentRate(this.ascentRate);
	    
	    setChanged();
	    notifyObservers(ASCENT_RATE);
	    
	    if (exception != null) {
		throw new CalculationException("updateAscentRate", "calculated ascent rate",
					       exception.getMessage());
	    }
	}
    }
    
    /**
     * Determine if the specified ascent rate is valid for the sounding.
     * @param ascentRate The ascent rate to be validated.
     * @return The validated ascent rate value.
     **/
    protected abstract Double validateAscentRate(Double ascentRate);
    
    /**
     * Determine if the specified time is valid for the sounding.
     * @param time The time to be validated.
     * @return The validated time value.
     **/
    protected abstract Double validateTime(Double time);
}
