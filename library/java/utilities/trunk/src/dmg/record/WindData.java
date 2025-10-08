package dmg.record;

import static dmg.util.ComparisonUtils.compare;
import static dmg.util.VelocityUtils.*;
import dmg.util.*;
import dmg.util.VelocityUtils.*;
import java.util.*;

/**
 * <p>The WindData class is a data block for holding the related measurements of
 * wind speed, wind direction, and the U and V wind components.</p>
 * <p>The wind speed and wind direction or U and V wind component values are 
 * calculated values when they have not been explicitly set and will be
 * (re)calculated if they are unset with a <code>null</code> value, unless the
 * block has been explicitly told not to calculate the values.  A change
 * to one of the data values that is used by a calculated value will force a
 * recalculation.  Any Exception thrown by a calculation is captured in a
 * CalculationWarning.  This allows the value change to be propogated to all 
 * calculations before being thrown and will allow the program to continue if
 * it wishes without it being a major problem..</p>
 * <p>The WindData class extends the Observable class. Every value that changes 
 * in the class, sends out a notification of the value that changed using the
 * static keys defined in the class. Classes that use the WindData class may 
 * also want to implement the Observer interface to propogate the WindData 
 * notifications as a notification from the using class.
 * 
 * @see java.util.Observable
 * @see java.util.Observer
 *
 * @author Joel Clawson
 */
public abstract class WindData extends Observable {

    /**
     * The key used by the notifyObservers that the U Component value has 
     * changed.
     */
    public static final String U_COMPONENT = "u component";
    
    /**
     * The key used by the notifyObservers that the V Component value has 
     * changed.
     */
    public static final String V_COMPONENT = "v component";
    
    /**
     * The key used by the notifyObservers that the Wind Direction value has 
     * changed.
     */
    public static final String WIND_DIRECTION = "wind direction";
    
    /**
     * The key used by the notifyObservers that the Wind Speed value has 
     * changed.
     */
    public static final String WIND_SPEED = "wind speed";
    
    private boolean calculationsAllowed;
    private boolean canCalculateUV, canCalculateWinds;
    private Double direction, speed, uComponent, vComponent;

    /**
     * Create a new instance of a WindData.
     * @param calculationsAllowed A flag that specifies if values should be
     * automatically calculated within this block.
     */
    public WindData(boolean calculationsAllowed) {
	this.calculationsAllowed = calculationsAllowed;
	canCalculateUV = true;
	canCalculateWinds = true;
    }
    
    /**
     * Get the default units of velocity that the wind values store and return.
     * @return The default velocity unit.
     */
    public abstract VelocityUnit getDefaultVelocityUnit();
    
    /**
     * Get the U wind component in the default velocity units.  The default
     * value is <code>null</code>.
     * @return The U wind component in the default velocity units.
     */
    public Double getUComponent() { return uComponent; }
    
    /**
     * Get the V wind component in the default velocity units.  The default
     * value is <code>null</code>.
     * @return The V wind component in the default velocity units.
     */
    public Double getVComponent() { return vComponent; }
    
    /**
     * Get the wind direction in degrees.  The default value is 
     * <code>null</code>.
     * @return The wind direction in degrees.
     */
    public Double getWindDirection() { return direction; }
    
    /**
     * Get the wind speed in the default velocity units.  The default
     * value is <code>null</code>.
     * @return The wind speed in the default velocity units.
     */
    public Double getWindSpeed() { return speed; }
    
    /**
     * Set the U wind component value.  This will attempt to (re)calculate the
     * wind speed and direction along with the U and V components.
     * @param uComponent The U wind component value.
     * @param unit The VelocityUnit of the component value.
     * @throws CalculationWarning if there is a problem calculation any of the wind
     * values in the data block.
     * @throws ConversionException if the VelocityUnit is <code>null</code>.
     * @throws InvalidValueWarning if the U Component value (either set or calculated)
     * is not a valid value allowed by the data block.
     */
    public void setUComponent(Double uComponent, VelocityUnit unit) throws 
	CalculationWarning, ConversionException, InvalidValueWarning
    {
	Double newU = convertVelocity(uComponent, unit, 
				      getDefaultVelocityUnit());
	
	Double validU = validateUComponent(newU);
	
	StringBuffer exceptionMessages = new StringBuffer();
	
	if (compare(this.uComponent, validU) != 0) {
	    this.uComponent = validU;
	    setChanged();
	    notifyObservers(U_COMPONENT);
	    
	    // Need to null out the calculated v component now that the u 
	    // component has been set manually.
	    this.vComponent = canCalculateUV ? null : this.vComponent;
	    setChanged();
	    notifyObservers(V_COMPONENT);
	}
	
	canCalculateUV = 
	    (this.uComponent == null && this.vComponent == null);
	
	try { updateWinds(); }
	catch (CalculationException e) {
			exceptionMessages.append("\n").append(e.getMessage());
	}
	
	try { updateUV(); }
	catch (CalculationException e) {
	    exceptionMessages.append("\n").append(e.getMessage());
	}
	
	if (validU == null && newU != null) {
	    throw new InvalidValueWarning("u component", uComponent,
					  "The U component is not an allowed U component." +
					  exceptionMessages.toString());
	}
	
	if (exceptionMessages.length() != 0) {
	    throw new CalculationWarning(exceptionMessages.substring(1));
	}
    }
    
    /**
     * Set the V wind component value.  This will attempt to (re)calculate the
     * wind speed and direction along with the U and V components.
     * @param vComponent The V wind component value.
     * @param unit The VelocityUnit of the component value.
     * @throws CalculationWarning if there is a problem calculation any of the wind
     * values in the data block.
     * @throws ConversionException if the VelocityUnit is <code>null</code>.
     * @throws InvalidValueWarning if the V Component value (either set or calculated)
     * is not a valid value allowed by the data block.
     */
    public void setVComponent(Double vComponent, VelocityUnit unit) throws 
	CalculationWarning, ConversionException, InvalidValueWarning
    {
	Double newV = convertVelocity(vComponent, unit, getDefaultVelocityUnit());
	
	Double validV = validateVComponent(newV);
	
	StringBuffer exceptionMessages = new StringBuffer();
	
	if (compare(this.vComponent, validV) != 0) {
	    this.vComponent = validV;
	    setChanged();
	    notifyObservers(V_COMPONENT);
	    
	    // Need to null out the calculated u component now that the v 
	    // component has been set manually.
	    this.uComponent = canCalculateUV ? null : this.uComponent;
	    setChanged();
	    notifyObservers(U_COMPONENT);
	}
	
	canCalculateUV = 
	    (this.uComponent == null && this.vComponent == null);
	
	try { updateWinds(); }
	catch (CalculationException e) {
	    exceptionMessages.append("\n").append(e.getMessage());
	}
	
	try { updateUV(); }
	catch (CalculationException e) {
	    exceptionMessages.append("\n").append(e.getMessage());
	}
	
	if (validV == null && newV != null) {
	    throw new InvalidValueWarning("v component", vComponent,
					  "The V component is not an allowed V component." +
					  exceptionMessages.toString());
	}
	
	if (exceptionMessages.length() != 0) {
	    throw new CalculationWarning(exceptionMessages.substring(1));
	}
    }
    
    /**
     * Set the wind direction value.  This will attempt to (re)calculate the
     * U and V components along with the wind speed and direction.
     * @param direction The direction in degrees.
     * @throws CalculationWarning if there is a problem calculation any of the wind
     * values in the data block.
     * @throws InvalidValueWarning if the wind direction value (either set or calculated)
     * is not a valid value allowed by the data block.
     */
    public void setWindDirection(Double direction) 
	throws CalculationWarning, InvalidValueWarning
    {
	Double validDir = validateWindDirection(direction);
	
	StringBuffer exceptionMessages = new StringBuffer();
	
	if (compare(this.direction, validDir) != 0) {
	    this.direction = validDir;
	    setChanged();
	    notifyObservers(WIND_DIRECTION);
	    
	    // Need to null out the calculated speed now that the direction has 
	    // been set manually.
	    this.speed = canCalculateWinds ? null : this.speed;
	    setChanged();
	    notifyObservers(WIND_SPEED);
	}
	
	canCalculateWinds = (this.direction == null && this.speed == null);
	
	try { updateUV(); }
	catch (CalculationException e) {
	    exceptionMessages.append("\n").append(e.getMessage());
	}
	
	try { updateWinds(); }
	catch (CalculationException e) {
	    exceptionMessages.append("\n").append(e.getMessage());
	}
	
	if (validDir == null && direction != null) {
	    throw new InvalidValueWarning("wind direction", direction,
					  "The wind direction is not an allowed direction." +
					  exceptionMessages.toString());
	}
	
	if (exceptionMessages.length() != 0) {
	    throw new CalculationWarning(exceptionMessages.substring(1));
	}
    }
    
    /**
     * Set the wind speed value.  This will attempt to (re)calculate the
     * U and V components along with the wind speed and direction.
     * @param speed The wind speed value.
     * @param unit The VelocityUnit of the wind speed value.
     * @throws CalculationWarning if there is a problem calculation any of the wind
     * values in the data block.
     * @throws ConversionException if the VelocityUnit is <code>null</code>.
     * @throws InvalidValueWarning if the wind speed value (either set or calculated)
     * is not a valid value allowed by the data block.
     */
    public void setWindSpeed(Double speed, VelocityUnit unit) throws 
	CalculationWarning, ConversionException, InvalidValueWarning
    {
	Double newSpeed = convertVelocity(speed, unit, getDefaultVelocityUnit());
	
	Double validSpeed = validateWindDirection(newSpeed);
	
	StringBuffer exceptionMessages = new StringBuffer();
	
	if (compare(this.speed, validSpeed) != 0) {
	    this.speed = validSpeed;
	    setChanged();
	    notifyObservers(WIND_SPEED);
	    
	    // Need to null out the calculated direction now that the speed has 
	    // been set manually.
	    this.direction = canCalculateWinds ? null : this.direction;
	    setChanged();
	    notifyObservers(WIND_DIRECTION);
	}
	
	canCalculateWinds = (this.direction == null && this.speed == null);
	
	try { updateUV(); }
	catch (CalculationException e) {
	    exceptionMessages.append("\n").append(e.getMessage());
	}
	
	try { updateWinds(); }
	catch (CalculationException e) {
	    exceptionMessages.append("\n").append(e.getMessage());
	}
	
	if (validSpeed == null && newSpeed != null) {
	    throw new InvalidValueWarning("wind speed", speed,
					  "The wind speed is not an allowed speed." +
					  exceptionMessages.toString());
	}
	
	if (exceptionMessages.length() != 0) {
	    throw new CalculationWarning(exceptionMessages.substring(1));
	}
    }
    
    /**
     * Calculate the U and V wind components if possible.
     * @throws CalculationException when the U and V wind components cannot be calculated.
     * @see dmg.util.VelocityUtils#calculateUVWinds(Double, Double)
     */
    private void updateUV() throws CalculationException {
	if (calculationsAllowed && canCalculateUV) {
	    CalculationException exception = null;
	    
	    if (direction != null && speed != null) {
		try {
		    Double[] components = calculateUVWinds(convertVelocity(this.speed, getDefaultVelocityUnit(), METERS_PER_SECOND), this.direction);
		    this.uComponent = convertVelocity(components[0], METERS_PER_SECOND, getDefaultVelocityUnit());
		    this.vComponent = convertVelocity(components[1], METERS_PER_SECOND, getDefaultVelocityUnit());
		} catch (CalculationException e) {
		    uComponent = null;
		    vComponent = null;
		    exception = e;
		}
		catch (ConversionException e) {}
	    } else {
		uComponent = null;
		vComponent = null;
	    }
	    
	    uComponent = validateUComponent(uComponent);
	    vComponent = validateVComponent(vComponent);
	    
	    setChanged();
	    notifyObservers(U_COMPONENT);
	    setChanged();
	    notifyObservers(V_COMPONENT);
	    
	    if (exception != null) {
		throw new CalculationException("updateUV", "calculated U & V components",
					       exception.getMessage());
	    }			
	}
    }
    
    /**
     * Calculate the wind speed and direction if possible.
     * @throws CalculationException when the wind speed and direction cannot be calculated.
     * @see dmg.util.VelocityUtils#calculateWinds(Double, Double)
     */
    private void updateWinds() throws CalculationException {
	if (calculationsAllowed && canCalculateWinds) {
	    CalculationException exception = null;
	    
	    if (uComponent != null && vComponent != null) {
		try {
		    Double[] components = calculateWinds(convertVelocity(this.uComponent, getDefaultVelocityUnit(), METERS_PER_SECOND),
							 convertVelocity(this.vComponent, getDefaultVelocityUnit(), METERS_PER_SECOND));
		    this.speed = convertVelocity(components[0], METERS_PER_SECOND, getDefaultVelocityUnit());
		    this.direction = components[1];
		} catch (CalculationException e) {
		    speed = null;
		    direction = null;
		    exception = e;
		}
		catch (ConversionException e) {}
	    } else {
		speed = null;
		direction = null;
	    }
	    
	    direction = validateWindDirection(direction);
	    speed = validateWindSpeed(speed);
	    
	    setChanged();
	    notifyObservers(WIND_DIRECTION);
	    setChanged();
	    notifyObservers(WIND_SPEED);
	    
	    if (exception != null) {
		throw new CalculationException("updateWinds",
					       "calculated wind speed & direction",
					       exception.getMessage());
	    }			
	}
    }
    
    /**
     * Validate the specified U Component value to see if it is an allowed value for the 
     * current data block.
     * @param uComponent The U component to be validated.
     * @return The validated U component value.  This may be an adjustment to the 
     * original value or <code>null</code> if it was invalid.
     */
    protected abstract Double validateUComponent(Double uComponent);
    
    /**
     * Validate the specified V Component value to see if it is an allowed value for the 
     * current data block.
     * @param vComponent The V component to be validated.
     * @return The validated V component value.  This may be an adjustment to the 
     * original value or <code>null</code> if it was invalid.
     */
    protected abstract Double validateVComponent(Double vComponent);
    
    /**
     * Validate the specified direction value to see if it is an allowed value for the 
     * current data block.
     * @param direction The wind direction to be validated.
     * @return The validated wind direction value.  This may be an adjustment to the 
     * original value or <code>null</code> if it was invalid.
     */
    protected abstract Double validateWindDirection(Double direction);
    
    /**
     * Validate the specified wind speed value to see if it is an allowed value for the 
     * current data block.
     * @param speed The wind speed to be validated.
     * @return The validated wind speed value.  This may be an adjustment to the 
     * original value or <code>null</code> if it was invalid.
     */
    protected abstract Double validateWindSpeed(Double speed);
}
