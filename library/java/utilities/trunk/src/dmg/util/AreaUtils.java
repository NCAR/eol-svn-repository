package dmg.util;

import static dmg.util.LengthUtils.*;
import static java.lang.Math.pow;

/**
 * <p>The AreaUtils class is a collection of functions and constants used to
 * manipulate area values.</p>
 *
 * @author Joel Clawson
 */
public final class AreaUtils {

    /**
     * The constant to define the acres unit of area.
     */
    public static final AreaUnit ACRES = (new AreaUtils()).new AreaUnit("acres");
    
    /**
     * The constant to define the square centimeter (cm<sup>2</sup>) unit of area.
     */
    public static final AreaUnit SQUARE_CENTIMETERS = (new AreaUtils()).new AreaUnit("cm2");
    
    /**
     * The constant to define the square feet (ft<sup>2</sup>) unit of area.
     */
    public static final AreaUnit SQUARE_FEET = (new AreaUtils()).new AreaUnit("ft2");
	
    /**
     * The constant to define the square inches (in<sup>2</sup>) unit of area.
     */
    public static final AreaUnit SQUARE_INCHES = (new AreaUtils()).new AreaUnit("in2");
	
    /**
     * The constant to define the square kilometers (km<sup>2</sup>) unit of area.
     */
    public static final AreaUnit SQUARE_KILOMETERS = (new AreaUtils()).new AreaUnit("km2");
	
    /**
     * The constant to define the square meters (m<sup>2</sup>) unit of area.
     */
    public static final AreaUnit SQUARE_METERS = (new AreaUtils()).new AreaUnit("m2");
	
    /**
     * The constant to define the square miles (mile<sup>2</sup>) unit of area.
     */
    public static final AreaUnit SQUARE_MILES = (new AreaUtils()).new AreaUnit("mile2");
	
    /**
     * The constant to define the square millimeters (mm<sup>2</sup>) unit of area.
     */
    public static final AreaUnit SQUARE_MILLIMETERS = (new AreaUtils()).new AreaUnit("mm2");
	
    /**
     * The constant to define the square yards (yd<sup>2</sup>) unit of area.
     */
    public static final AreaUnit SQUARE_YARDS = (new AreaUtils()).new AreaUnit("yd2");
	
    /**
     * Create a new instance of an AreaUtils.
     **/
    private AreaUtils() {}
	
    /**
     * Convert a area value from unit of area to another.
     * @param value The value to be converted.
     * @param inUnit The initial area unit of the value.
     * @param outUnit The desired area unit for the value.
     * @return The value in the desired area unit.
     * @throws ConversionException if either the input or output area unit is <code>null</code>.
     */
    public static Double convertArea(Double value, AreaUnit inUnit, AreaUnit outUnit) 
	throws ConversionException
    {
	// Just return the value if it is null
	if (value == null) { return null; }
	
	// Make sure the input unit is valid.
	if (inUnit == null) {
	    throw new ConversionException("convertArea","inUnit",
					  "The input unit was null.");
	}
	// Make sure the output unit is valid.
	if (outUnit == null) {
	    throw new ConversionException("convertArea","outUnit",
					  "The output unit was null.");
	}
	
	// If the input and output units are the same, don't convert and just
	// return the value.
	if (inUnit.equals(outUnit)) { return value; }
	
	// Convert the value to MILLILITERS
	if (inUnit.equals(ACRES)) { 
	    value *= pow(convertLength(1.0, MILES, METERS), 2.0) / 640.0; }
	else if (inUnit.equals(SQUARE_CENTIMETERS)) { 
	    value *= pow(convertLength(1.0, CENTIMETERS, METERS), 2.0); }
	else if (inUnit.equals(SQUARE_FEET)) { 
	    value *= pow(convertLength(1.0, FEET, METERS), 2.0); }
	else if (inUnit.equals(SQUARE_INCHES)) {
	    value *= pow(convertLength(1.0, INCHES, METERS), 2.0); }
	else if (inUnit.equals(SQUARE_KILOMETERS)) {
	    value *= pow(convertLength(1.0, KILOMETERS, METERS), 2.0); }
	else if (inUnit.equals(SQUARE_MILES)) {
	    value *= pow(convertLength(1.0, MILES, METERS), 2.0); }
	else if (inUnit.equals(SQUARE_MILLIMETERS)) {
	    value *= pow(convertLength(1.0, MILLIMETERS, METERS), 2.0); }
	else if (inUnit.equals(SQUARE_YARDS)) { 
	    value *= pow(convertLength(1.0, YARDS, METERS), 2.0); }
	else {} // Value is already in square meters
	
		// Convert the square meters value to the output unit
	if (outUnit.equals(ACRES)) { 
	    value *= 640.0 * pow(convertLength(1.0, METERS, MILES), 2.0); }
	else if (outUnit.equals(SQUARE_CENTIMETERS)) {
	    value *= pow(convertLength(1.0, METERS, CENTIMETERS), 2.0); }
	else if (outUnit.equals(SQUARE_FEET)) { 
	    value *= pow(convertLength(1.0, METERS, FEET), 2.0); }
	else if (outUnit.equals(SQUARE_INCHES)) { 
	    value *= pow(convertLength(1.0, METERS, INCHES), 2.0); }
	else if (outUnit.equals(SQUARE_KILOMETERS)) { 
	    value *= pow(convertLength(1.0, METERS, KILOMETERS), 2.0); }
	else if (outUnit.equals(SQUARE_MILES)) { 
	    value *= pow(convertLength(1.0, METERS, MILES), 2.0); }
	else if (outUnit.equals(SQUARE_MILLIMETERS)) { 
	    value *= pow(convertLength(1.0, METERS, MILLIMETERS), 2.0); }
	else if (outUnit.equals(SQUARE_YARDS)) { 
	    value *= pow(convertLength(1.0, METERS, YARDS), 2.0); }
	else {} // Value is already in square meters
	
	return value;
    }
    
    /**
     * The AreaUnit class is a MeasurementUnit for defining a area.  It is a
     * public class to be available to all other classes, with the constructors 
     * being private to only allow the containing class to generate new 
     * instances of the class.
     */
    public class AreaUnit extends MeasurementUnit<AreaUnit> {
	
	/**
	 * Create a new instance of a AreaUnit.
	 * @param name The name of the unit of area.
	 */
	private AreaUnit(String name) { super(name); }
    }
}
