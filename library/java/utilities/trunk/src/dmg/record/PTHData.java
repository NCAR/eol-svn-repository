package dmg.record;

import static dmg.util.ComparisonUtils.*;
import static dmg.util.HumidityUtils.*;
import static dmg.util.LengthUtils.*;
import static dmg.util.PressureUtils.*;
import static dmg.util.TemperatureUtils.*;
import dmg.util.*;
import dmg.util.LengthUtils.*;
import dmg.util.PressureUtils.*;
import dmg.util.TemperatureUtils.*;
import java.util.*;

/**
 * <p>The PTHData class is a data block for holding the related measurements of
 * dew point, elevation, pressure, relative humidity, sea level pressure, and
 * temperature.</p>
 * <p>The dew point, relative humidity, and sea level pressure values
 * are calculated values when they have not been explicitly set and will be
 * (re)calculated if they are unset with a <code>null</code> value, unless the
 * block has been explicitly told not to calculate the values.  A change
 * to one of the data values that is used by a calculated value will force a
 * recalculation.  Any Exception thrown by a calculation is captured in a
 * CalculationWarning.  This allows the value change to be propogated to all 
 * calculations before being thrown and will allow the program to continue if
 * it wishes without it being a major problem..</p>
 * <p>The PTHData class extends the Observable class.  Every value that changes
 * in the class, sends out a notification of the value that changed using the
 * static keys defined in the class.  Classes that use the PTHData class may 
 * also want to implement the Observer interface to propogate the PTHData
 * notifications as a notification from the using class.</p>
 * 
 * @see java.util.Observable
 * @see java.util.Observer
 *
 * @author Joel Clawson
 */
public abstract class PTHData extends Observable {

    /**
     * The key used by the notifyObservers that the dew point value has changed.
     */
    public static final String DEW_POINT = "dew point";
    
    /**
     * The key used by the notifyObservers that the elevation value has changed.
     */
    public static final String ELEVATION = "elevation";
    
    /**
     * The key used by the notifyObservers that the pressure value has changed.
     */
    public static final String PRESSURE = "pressure";
    
    /**
     * The key used by the notifyObservers that the relative humidity value has 
     * changed.
     */
    public static final String RELATIVE_HUMIDITY = "relative humidity";
    
    /**
     * The key used by the notifyObservers that the sea level pressure value 
     * has changed.
     */
    public static final String SEA_LEVEL_PRESSURE = "sea level pressure";
    
    /**
     * The key used by the notifyObservers that the temperature value has 
     * changed.
     */
    public static final String TEMPERATURE = "temperature";
    
    private boolean calculationsAllowed;
    private boolean canCalculateDewPoint, canCalculateRH, canCalculateSLP;
    private Double dewPoint, elevation, pressure, rh, slpressure, temperature;
	
    /**
     * Create a new instance of a PTHData.
     * @param calculationsAllowed A flag to enable/disable the data block to
     * calculate values when able.
     */
    public PTHData(boolean calculationsAllowed) {
	// Initialize all calculation states to allow calculations
	this.calculationsAllowed = calculationsAllowed;
	canCalculateDewPoint = true;
	canCalculateRH = true;
	canCalculateSLP = true;
    }
    
    /**
     * Get the default unit of length for the length values stored in the data block.
     * @return The default LengthUnit lengths are stored in.
     */
    public abstract LengthUnit getDefaultLengthUnit();
    
    /**
     * Get the default unit of pressure for the pressure values stored in the data block.
     * @return The default PressureUnit pressures are stored in.
     */
    public abstract PressureUnit getDefaultPressureUnit();

    /**
     * Get the default unit of temperature for the temperature values stored in the data block.
     * @return The default TemperatureUnit temperature values are stored in.
     */
    public abstract TemperatureUnit getDefaultTemperatureUnit();
    
    /**
     * Get the dew point value.  This value will be the calculated dew point
     * if it has not been set explicitly.  The default value is <code>null</code>.
     * @return The dew point in the default temperature units.
     */
    public Double getDewPoint() { return dewPoint; }
    
    /**
     * Get the elevation value.  The default value is <code>null</code>.
     * @return THe elevation in the default length units.
     */
    public Double getElevation() { return elevation; }
    
    /**
     * Get the pressure value.  The default value is <code>null</code>.
     * @return The pressure value in the default pressure units.
     */
    public Double getPressure() { return pressure; }
    
    /**
     * Get the relative humidity value.  This will be a calcualted value when
     * possible and not set explicitly.  The default value is <code>null</code>.
     * @return The relative humidity value in percent.
     */
    public Double getRelativeHumidity() { return rh; }
    
    /**
     * Get the sea level pressure value.  This will be a calculated value when
     * possible and not set explicitly.  The default value is <code>null</code>.
     * @return The sea level pressure value in the default pressure unit.
     */
    public Double getSeaLevelPressure() { return slpressure; }
    
    /**
     * Get the temperature value.  The default value is <code>null</code>.
     * @return The temperature value in the default temperature unit.
     */
    public Double getTemperature() { return temperature; }
    
    /**
     * Set the dew point value.
     * @param dewPoint The dew point value.
     * @param unit The temperature unit of the dew point value.
     * @throws CalculationWarning if the relative humidity, dew point, or sea level pressure
     * value attempts to be calculated and it cannot be.
     * @throws ConversionException if the temperature unit is <code>null</code>.
     * @throws InvalidValueWarning if the dew point value (either set or calculated) is not
     * a valid dew point allowed for the data block.
     */
    public void setDewPoint(Double dewPoint, TemperatureUnit unit) 
	throws CalculationWarning, ConversionException, InvalidValueWarning
    {
	Double newDewPoint = convertTemperature(dewPoint, unit, getDefaultTemperatureUnit());
	
	Double validDewPoint = validateDewPoint(newDewPoint);
	
	StringBuffer exceptionMessages = new StringBuffer();
	
	if (compare(this.dewPoint, validDewPoint) != 0) {
	    this.dewPoint = validDewPoint;
	    setChanged();
	    notifyObservers(DEW_POINT);
	}
	
	canCalculateDewPoint = (this.dewPoint == null);
	
	try { updateRelativeHumidity(); }
	catch (CalculationException e) {
	    exceptionMessages.append("\n").append(e.getMessage());
	}
	
	try { updateDewPoint(); }
	catch (CalculationException e) {
	    exceptionMessages.append("\n").append(e.getMessage());
	}
	
	try { updateSeaLevelPressure(); }
	catch (CalculationException e) {
	    exceptionMessages.append("\n").append(e.getMessage());
	}
	
	if (validDewPoint == null && newDewPoint != null) {
	    throw new InvalidValueWarning("dew point", dewPoint,
					  "The dew point is not an allowed dew point." +
					  exceptionMessages.toString());
	}
	
	if (exceptionMessages.length() != 0) {
	    throw new CalculationWarning(exceptionMessages.substring(1));
	}
    }
    
    /**
     * Set the elevation value.
     * @param elevation The elevation value.
     * @param unit The length unit of the elevation value.
     * @throws CalculationWarning if the sea level pressure value attempts to be 
     * calculated and it cannot be.
     * @throws ConversionException if the elevation unit is <code>null</code>.
     * @throws InvalidValueWarning if the elevation value is not a valid dew point 
     * allowed for the data block.
     */
    public void setElevation(Double elevation, LengthUnit unit) 
	throws CalculationWarning, ConversionException, InvalidValueWarning
    {
	Double newElevation = convertLength(elevation, unit, getDefaultLengthUnit());
	
	Double validElevation = validateElevation(newElevation);
	
	StringBuffer exceptionMessages = new StringBuffer();
	
	if (compare(this.elevation, validElevation) != 0) {
	    this.elevation = validElevation;
	    setChanged();
	    notifyObservers(ELEVATION);
	}
	
	try { updateSeaLevelPressure(); }
	catch (CalculationException e) {
	    exceptionMessages.append("\n").append(e.getMessage());
	}
	
	if (validElevation == null && newElevation != null) {
	    throw new InvalidValueWarning("elevation", elevation,
					  "The elevation is not an allowed elevation." +
					  exceptionMessages.toString());
	}
	
	if (exceptionMessages.length() != 0) {
	    throw new CalculationWarning(exceptionMessages.substring(1));
	}
    }
    
    /**
     * Set the pressure value.
     * @param pressure The pressure value.
     * @param unit The pressure unit for the pressure value.
     * @throws CalculationWarning if the sea level pressure value attempts to be calculated 
     * and it cannot be.
     * @throws ConversionException if the pressue unit is <code>null</code>.
     * @throws InvalidValueWarning if the pressure value is not a valid pressure allowed for
     * the data block.
     */
    public void setPressure(Double pressure, PressureUnit unit)
	throws CalculationWarning, ConversionException, InvalidValueWarning
    {
	Double newPressure = convertPressure(pressure, unit, getDefaultPressureUnit());
	
	Double validPressure = validatePressure(newPressure);
	
	StringBuffer exceptionMessages = new StringBuffer();
	
	if (compare(this.pressure, validPressure) != 0) {
	    this.pressure = validPressure;
	    setChanged();
	    notifyObservers(PRESSURE);
	}
	
	try { updateSeaLevelPressure(); }
	catch (CalculationException e) {
	    exceptionMessages.append("\n").append(e.getMessage());
	}
	
	if (validPressure == null && newPressure != null) {
	    throw new InvalidValueWarning("pressure", pressure,
					  "The pressure is not an allowed pressuure." +
					  exceptionMessages.toString());
	}
	
	if (exceptionMessages.length() != 0) {
	    throw new CalculationWarning(exceptionMessages.substring(1));
	}
    }
    
    /**
     * Set the relative humidity value.
     * @param rh The relative humidity value in percent.
     * @throws CalculationWarning if the relative humidity, dew point, or sea level pressure
     * value attempts to be calculated and it cannot be.
     * @throws InvalidValueWarning if the relative humidity value (either set or calculated)
     * is not a valid relative humidity allowed for the data block.
     */
    public void setRelativeHumidity(Double rh) throws CalculationWarning, InvalidValueWarning
    {
	StringBuffer exceptionMessages = new StringBuffer();
	
	Double validRH = validateRelativeHumidity(rh);
	
	if (compare(this.rh, validRH) != 0) {
	    this.rh = validRH;
	    setChanged();
	    notifyObservers(RELATIVE_HUMIDITY);
	}
	
	canCalculateRH = (this.rh == null);
	
	try { updateDewPoint(); }
	catch (CalculationException e) {
	    exceptionMessages.append("\n").append(e.getMessage());
	}
	
	try { updateRelativeHumidity(); }
	catch (CalculationException e) {
	    exceptionMessages.append("\n").append(e.getMessage());
	}
	
	try { updateSeaLevelPressure(); }
	catch (CalculationException e) {
	    exceptionMessages.append("\n").append(e.getMessage());
	}
	
	if (validRH == null && rh != null) {
	    throw new InvalidValueWarning("relative humidity", rh,
					  "The relative humidity is not an allowed relative " +
					  "humidity." + exceptionMessages.toString());
	}
	
	if (exceptionMessages.length() != 0) {
	    throw new CalculationWarning(exceptionMessages.substring(1));
	}
    }
    
    /**
     * Set the temperature value.
     * @param temperature The temperature value.
     * @param unit The temperature unit for the temperature value.
     * @throws CalculationWarning if the relative humidity, dew point, or sea level pressure
     * value attempts to be calculated and it cannot be.
     * @throws ConversionException if the temperature unit is <code>null</code>.
     * @throws InvalidValueWarning if the temperature value is not a valid temperature allowed
     * for the data block.
     */
    public void setTemperature(Double temperature, TemperatureUnit unit)
	throws CalculationWarning, ConversionException, InvalidValueWarning
    {
	Double newTemp = convertTemperature(temperature, unit, getDefaultTemperatureUnit());
	
	StringBuffer exceptionMessages = new StringBuffer();
	
	Double validTemp = validateTemperature(newTemp);
	
	if (compare(this.temperature, validTemp) != 0) {
	    this.temperature = validTemp;
	    setChanged();
	    notifyObservers(TEMPERATURE);
	}
	
	try { updateRelativeHumidity(); }
	catch (CalculationException e) {
	    exceptionMessages.append("\n").append(e.getMessage());
	}
	
	try { updateDewPoint(); }
	catch (CalculationException e) {
	    exceptionMessages.append("\n").append(e.getMessage());
	}
	
	try { updateSeaLevelPressure(); }
	catch (CalculationException e) {
	    exceptionMessages.append("\n").append(e.getMessage());
	}
	
	if (validTemp == null && newTemp != null) {
	    throw new InvalidValueWarning("temperature", temperature,
					  "The temperature is not an allowed temperature" + 
					  exceptionMessages.toString());
	}
	
	if (exceptionMessages.length() != 0) {
	    throw new CalculationWarning(exceptionMessages.substring(1));
	}
    }
    
    /**
     * Update the dew point with the currently stored values if possible.
     * @throws CalculationException if the dew poitn cannot be calculated or if the calculated
     * value is not a valid value within the data block.
     * @see dmg.util.TemperatureUtils#calculateDewPoint(Double, Double)
     */
    private void updateDewPoint() throws CalculationException {
	if (calculationsAllowed && canCalculateDewPoint) {
	    CalculationException exception = null;
	    
	    if (temperature != null && rh != null) {
		try {
		    dewPoint = convertTemperature(calculateDewPoint(convertTemperature(temperature, getDefaultTemperatureUnit(), CELCIUS), rh), CELCIUS, getDefaultTemperatureUnit());
		} catch (CalculationException e) {
		    dewPoint = null;
		    exception = e;
		}
		catch (ConversionException e) {}
	    } else {
		dewPoint = null;
	    }
	    
	    dewPoint = validateDewPoint(dewPoint);
	    
	    setChanged();
	    notifyObservers(DEW_POINT);
	    
	    if (exception != null) {
		throw new CalculationException("updateDewPoint",
					       "calculated dew point",
					       exception.getMessage());
	    }			
	}
    }
    
    /**
     * Update the relative humidity with the currently stored values if  possible.
     * @throws CalculationException if the relative humidity cannot be calculated or if 
     * the calculated  value is not a valid value within the data block.
     * @see dmg.util.HumidityUtils#calculateRelativeHumidity(Double, Double)
     */
    private void updateRelativeHumidity() throws CalculationException {
	if (calculationsAllowed && canCalculateRH) {
	    CalculationException exception = null;
	    
	    if (temperature != null && dewPoint != null) {
		try {
		    rh = calculateRelativeHumidity(convertTemperature(temperature, getDefaultTemperatureUnit(), CELCIUS), convertTemperature(dewPoint, getDefaultTemperatureUnit(), CELCIUS));
		} catch (CalculationException e) {
		    rh = null;
		    exception = e;
		}
		catch (ConversionException e) {}
	    } else {
		rh = null;
	    }
	    
	    rh = validateRelativeHumidity(rh);
	    
	    setChanged();
	    notifyObservers(RELATIVE_HUMIDITY);
	    
	    if (exception != null) {
		throw new CalculationException("updateRelativeHumidity",
					       "calculated relative humidity",
					       exception.getMessage());
	    }
	}
    }
    
    /**
     * Update the sea level pressure value with the currently stored values if  possible.
     * @throws CalculationException if the sea level pressure cannot be calculated or if
     * the calculated value is not a valid value within the data block.
     * @see dmg.util.PressureUtils#calculateSeaLevelPressure(Double, Double, Double, Double)
     */
    private void updateSeaLevelPressure() throws CalculationException {
	if (calculationsAllowed && canCalculateSLP) {
	    CalculationException exception = null;
	    
	    if (temperature != null && dewPoint != null && pressure != null && 
		elevation != null) {
		try {
		    slpressure = convertPressure(calculateSeaLevelPressure(convertPressure(pressure, getDefaultPressureUnit(), MILLIBARS), convertTemperature(temperature, getDefaultTemperatureUnit(), CELCIUS), convertTemperature(dewPoint, getDefaultTemperatureUnit(), CELCIUS), convertLength(elevation, getDefaultLengthUnit(), METERS)), MILLIBARS, getDefaultPressureUnit());
		} catch (CalculationException e) {
		    slpressure = null;
		    exception = e;
		}
		catch (ConversionException e) {}
	    } else {
		slpressure = null;
	    }
	    
	    slpressure = validateSeaLevelPressure(slpressure);
	    
	    setChanged();
	    notifyObservers(SEA_LEVEL_PRESSURE);
	    
	    if (exception != null) {
		throw new CalculationException("updateSeaLevelPressure",
					       "calculated sea level pressure",
					       exception.getMessage());
	    }
	}
    }
    
    /**
     * Validate the specified dew point value to see if it is an allowed value for the
     * current data block.
     * @param dewPoint The dew point to be validated.
     * @return The validated dew point.  This may be an adjustment to the original value
     * or <code>null</code> if it was invalid.
     **/
    protected abstract Double validateDewPoint(Double dewPoint);
    
    /**
     * Validate the specified elevation value to see if it is an allowed value for the
     * current data block.
     * @param elevation The elevation to be validated.
     * @return The validated elevation.  This may be an adjustment to the original value
     * or <code>null</code> if it was invalid.
     **/
    protected abstract Double validateElevation(Double elevation);
    
    /**
     * Validate the specified pressure value to see if it is an allowed value for the
     * current data block.
     * @param pressure The pressure to be validated.
     * @return The validated pressure.  This may be an adjustment to the original value
     * or <code>null</code> if it was invalid.
     **/
    protected abstract Double validatePressure(Double pressure);
    
    /**
     * Validate the specified relative humidity value to see if it is an allowed value for the
     * current data block.
     * @param rh The relative humidity to be validated.
     * @return The validated relative humidity.  This may be an adjustment to the original value
     * or <code>null</code> if it was invalid.
     **/
    protected abstract Double validateRelativeHumidity(Double rh);
    
    /**
     * Validate the specified sea level pressure value to see if it is an allowed value for the
     * current data block.
     * @param slpressure The sea level pressure to be validated.
     * @return The validated sea level pressure.  This may be an adjustment to the original value
     * or <code>null</code> if it was invalid.
     **/
    protected abstract Double validateSeaLevelPressure(Double slpressure);
    
    /**
     * Validate the specified temperature value to see if it is an allowed value for the
     * current data block.
     * @param temperature The temperature to be validated.
     * @return The validated temperature.  This may be an adjustment to the original value
     * or <code>null</code> if it was invalid.
     **/
    protected abstract Double validateTemperature(Double temperature);
}
