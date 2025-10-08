package dmg.util;

import static dmg.util.VolumeUtils.*;

/**
 * <p>The FlowUtils class is a collection of functions and constants used to
 * manipulate flow values.</p>
 *
 * @author Joel Clawson
 */
public final class FlowUtils {

    /**
     * The constant to define the cubic feet per second (ft<sup>3</sup>/s) unit of flow.
     */
    public static final FlowUnit CUBIC_FEET_PER_SECOND = (new FlowUtils()).new FlowUnit("ft3/s");
	
    /**
     * The constant to define the cubic meters per second (m<sup>3</sup>/s) unit of flow.
     */
    public static final FlowUnit CUBIC_METERS_PER_SECOND = (new FlowUtils()).new FlowUnit("m3/s");
	
    /**
     * Create a new instance of a FlowUtils.
     **/
    private FlowUtils() {}
	
    /**
     * Convert a flow value from unit of flow to another.
     * @param value The value to be converted.
     * @param inUnit The initial flow unit of the value.
     * @param outUnit The desired flow unit for the value.
     * @return The value in the desired flow unit.
     * @throws ConversionException if either the input or output flow unit is 
     * <code>null</code>.
     */
    public static Double convertFlow(Double value, FlowUnit inUnit, FlowUnit outUnit) 
	throws ConversionException
    {
	// Just return the value if it is null
	if (value == null) { return null; }
	
	// Make sure the input unit is valid.
	if (inUnit == null) {
	    throw new ConversionException("convertFlow","inUnit",
					  "The input unit was null.");
	}
	// Make sure the output unit is valid.
	if (outUnit == null) {
	    throw new ConversionException("convertFlow","outUnit",
					  "The output unit was null.");
	}
	
	// If the input and output units are the same, don't convert and just 
	// return the value.
	if (inUnit.equals(outUnit)) { return value; }
	
	// Convert the value to m3/s
	if (inUnit.equals(CUBIC_FEET_PER_SECOND)) {
	    value *= convertVolume(1.0, CUBIC_FEET, CUBIC_METERS); }
	else {} // Value is already in m3/s
	
		// Convert the m3/s value to the output unit
	if (outUnit.equals(CUBIC_FEET_PER_SECOND)) {
	    value *= convertVolume(1.0, CUBIC_METERS, CUBIC_FEET); }
	else {} // Value is already in m3/s
	
	return value;
    }
    
    /**
     * The FlowUnit class is a MeasurementUnit for defining a flow.  It is a
     * public class to be available to all other classes, with the constructors 
     * being private to only allow the containing class to generate new 
     * instances of the class.
     */
    public class FlowUnit extends MeasurementUnit<FlowUnit> {
	
	/**
	 * Create a new instance of a FlowUnit.
	 * @param name The name of the unit of flow.
	 */
	private FlowUnit(String name) { super(name); }
    }
}
