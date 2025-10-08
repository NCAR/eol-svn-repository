package dmg.util;

/**
 * <p>The RadiationUtils class is a collection of functions and constants used 
 * to manipulate radiation values.</p>
 *
 * @author Joel Clawson
 */
public final class RadiationUtils {

    /**
     * The constant to define the langly unit of radiation.
     */
    public static final RadiationUnit LANGLY = (new RadiationUtils()).new RadiationUnit("langly");
    
    /**
     * The constant to define the watts per square meter (w/m<sup>2</sup>) unit of radiation.
     */
    public static final RadiationUnit WATTS_PER_SQUARE_METER = 
	(new RadiationUtils()).new RadiationUnit("w/m2");
	
    /**
     * Create a new instance of a RadiationUtils.  It is private to prevent other classes
     * from creating an instance.
     */
     private RadiationUtils() {}
     
     /**
     * Convert a radiation value from unit of radiation to another.
     * @param value The value to be converted.
     * @param inUnit The initial radiation unit of the value.
     * @param outUnit The desired radiation unit for the value.
     * @return The value in the desired radiation unit.
     * @throws ConversionException if either the input or output radiation
     * unit is <code>null</code>.
     */
    public static Double convertRadiation(Double value, RadiationUnit inUnit, 
					  RadiationUnit outUnit) throws ConversionException
    {
	// Just return the value if it is null
	if (value == null) { return null; }
	
	// Make sure the input unit is valid.
	if (inUnit == null) {
	    throw new ConversionException("convertRadiation","inUnit",
					  "The input unit was null.");
	}
	// Make sure the output unit is valid.
	if (outUnit == null) {
	    throw new ConversionException("convertRadiation","outUnit",
					  "The output unit was null.");
	}
	
	// If the input and output units are the same, don't convert and just 
	// return the value.
	if (inUnit.equals(outUnit)) { return value; }
	
	// Convert the value to W/m2
	if (inUnit.equals(LANGLY)) { value *= 10.0; }
	else {} // Value is already in w/m2
	
		// Convert the w/m2 value to the output unit
	if (outUnit.equals(LANGLY)) { value *= .1; }
	else {} // Value is already in w/m2
	
	return value;
    }
    
    /**
     * The RadiationUnit class is a MeasurementUnit for defining a radiation.  
     * It is a public class to be available to all other classes, with the 
     * constructors being private to only allow the containing class to generate
     * new instances of the class.
     */
    public class RadiationUnit extends MeasurementUnit<RadiationUnit> {
	
	/**
	 * Create a new instance of a RadiationUnit.
	 * @param name The name of the unit of radiation.
	 */
	private RadiationUnit(String name) { super(name); }
    }
}
