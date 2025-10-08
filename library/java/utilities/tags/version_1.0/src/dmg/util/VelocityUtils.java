package dmg.util;

import static dmg.util.LengthUtils.*;
import static dmg.util.TimeUtils.*;
import static java.lang.Math.*;

/**
 * <p>The VelocityUtils class is a collection of functions and constants used to
 * manipulation velocity/speed values.</p>
 *
 * @author Joel Clawson
 */
public final class VelocityUtils {
	
    /**
     * The constant to define the feet per second (ft/s) unit of velocity.
     */
    public static final VelocityUnit FEET_PER_SECOND = (new VelocityUtils()).new VelocityUnit("ft/s");
	
    /**
     * The constant to define the kilometers per hour (kph, km/hr) unit of velocity.
     */
    public static final VelocityUnit KILOMETERS_PER_HOUR = (new VelocityUtils()).new VelocityUnit("kph");
	
    /**
     * The constant to define the knot unit of velocity.
     */
    public static final VelocityUnit KNOTS = (new VelocityUtils()).new VelocityUnit("knot");
	
    /**
     * The constant to define the meters per second (m/s) unit of velocity.
     */
    public static final VelocityUnit METERS_PER_SECOND = (new VelocityUtils()).new VelocityUnit("m/s");
	
    /**
     * The constant to define the miles per hour (mph, mi/hr) unit of velocity.
     */
    public static final VelocityUnit MILES_PER_HOUR = (new VelocityUtils()).new VelocityUnit("mph");
    
    /**
     * Create a new instance of a VelocityUtils.  It is private to prevent other
     * classes from creating an actual instance of the class.
     */
    private VelocityUtils() {}
    
    /**
     * Calculate the U and V wind components from the wind speed and direction.
     * @param speed The wind speed in m/s.
     * @param direction The wind direction in degrees.
     * @return The U component followed by the V component in m/s.
     * @throws CalculationException when either the speed or direction is 
     * <code>null</code>, the wind speed is negative, or the wind direction is
     *  not in the range of 0-360.
     */
    public static Double[] calculateUVWinds(Double speed, Double direction) 
	throws CalculationException 
    {
	// Make sure there is a valid wind speed to calculate the components.
	if (speed == null) {
	    throw new CalculationException("calculateUVWinds","wind speed",
					   "The wind speed value was null.");
	} else if (speed < 0) {
	    throw new CalculationException("calculateUVWinds","wind speed",
					   String.format("The wind speed of %f is negative.",speed));
	}
	
	// Make sure there is a valid wind direction to calculate the components
	if (direction == null) {
	    throw new CalculationException("calculateUVWinds","wind direction",
					   "The wind direction value was null.");
	} else if (direction < 0 || direction > 360) {
	    throw new CalculationException("calculateUVWinds","wind direction",
					   String.format("The wind direction of %f is not between" +
							 " 0 and 360.",direction));
	}
	
	Double[] values = new Double[2];
	// Calculate the U-Wind Component
	values[0] = -1.0 * Math.sin(toRadians(direction)) * speed;
	// Calculate the V-Wind Component
	values[1] = -1.0 * Math.cos(toRadians(direction)) * speed;
	return values;
    }
    
    /**
     * Calculate the wind speed and direction from the U and V components.
     * @param uComp The U-component in m/s.
     * @param vComp The V-component in m/s.
     * @return The wind speed in m/s followed by the direction in degrees.
     * @throws CalculationException if either the u-component or the 
     * v-component is <code>null</code>.
     */
    public static Double[] calculateWinds(Double uComp, Double vComp) 
	throws CalculationException
    {
	// Make sure there is a valid U component.
	if (uComp == null) {
	    throw new CalculationException("calculateWinds","u-component",
					   "The u-component was null.");
	}
	// Make sure there is a valid V component.
	if (vComp == null) {
	    throw new CalculationException("calculateWinds","v-component",
					   "The v-component was null.");
	}
	
	Double[] winds = new Double[2];
	// Special case the divide by zero that would occur in atan
	if (vComp == 0.0) {
	    if (uComp == 0.0) {
		winds[0] = 0.0;
		winds[1] = 0.0;
	    } else if (uComp < 0.0) {
		winds[0] = -1 * uComp;
		winds[1] = 90.0;
	    } else {
		winds[0] = uComp;
		winds[1] = 270.0;
	    }
	} else {
	    winds[0] = Math.sqrt(Math.pow(uComp, 2) + Math.pow(vComp, 2));
	    winds[1] = Math.toDegrees(Math.atan(uComp / vComp));
	    if (vComp >= 0) { winds[1] += 180.0; }
	    // Make sure the value is between 0 and 360.
	    if (winds[1] < 0) { winds[1] += 360.0; }
	}
	
	return winds;
    }
    
    /**
     * Convert a value in one unit of velocity to another.
     * @param value The value to be converted.
     * @param inUnit The velocity unit of the initial value.
     * @param outUnit The desired velocity unit.
     * @return The value in the desired output unit or <code>null</code> if the
     * initial value was <code>null</code>.
     * @throws ConversionException if either the input or output units 
     * are <code>null</code>.
     */
    public static Double convertVelocity(Double value, VelocityUnit inUnit, 
					 VelocityUnit outUnit) throws ConversionException
    {
	// Just return the value if it is null
	if (value == null) { return null; }
	
	// Make sure the input unit is valid.
	if (inUnit == null) {
	    throw new ConversionException("convertVelocity","inUnit",
					  "The input unit was null.");
	}
	// Make sure the output unit is valid.
	if (outUnit == null) {
	    throw new ConversionException("convertVelocity","outUnit",
					  "The output unit was null.");
	}
	
	// If the input and output units are the same, don't convert and just
	// return the value.
	if (inUnit.equals(outUnit)) { return value; }
	
	// Convert the value to m/s
	if (inUnit.equals(FEET_PER_SECOND)) { 
	    value = convertLength(value,FEET,METERS); 	
	} else if (inUnit.equals(KILOMETERS_PER_HOUR)) { 
	    value = convertLength(value,KILOMETERS,METERS) / 
		convertTime(1.0,HOURS,SECONDS);
	} else if (inUnit.equals(KNOTS)) { value = value * 463 / 900; }
	else if (inUnit.equals(MILES_PER_HOUR)) { 
	    value = convertLength(value,MILES,METERS) / 
		convertTime(1.0,HOURS,SECONDS);
	} else {} // Value is already in m/s
	
	// Convert the value (now in m/s) to the output units.
	if (outUnit.equals(FEET_PER_SECOND)) { 
	    value = convertLength(value,METERS,FEET);
	} else if (outUnit.equals(KILOMETERS_PER_HOUR)) { 
	    value = convertLength(value,METERS,KILOMETERS) / 
		convertTime(1.0,SECONDS,HOURS);
	} else if (outUnit.equals(KNOTS)) { value = value * 900 / 463; }
	else if (outUnit.equals(MILES_PER_HOUR)) { 
	    value = convertLength(value,METERS,MILES) /
		convertTime(1.0,SECONDS,HOURS);
	} else {} // Value is to be m/s
	
	return value;
	}
    
    /**
     * The VeloictyUnit class is a MeasurementUnit for defining a 
     * velocity/speed.  It is a public class to be available to all other 
     * classes, with the constructors being private to only allow the containing
     * class to generate new instances of the class.
     */
    public class VelocityUnit extends MeasurementUnit<VelocityUnit> {
	
	/**
	 * Create a new instance of a VelocityUnit.
	 * @param name The name of the unit of velocity.
	 */
	private VelocityUnit(String name) { super(name); }
    }
}
