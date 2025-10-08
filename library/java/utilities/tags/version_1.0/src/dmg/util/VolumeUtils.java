package dmg.util;

import static dmg.util.LengthUtils.*;
import static java.lang.Math.pow;

/**
 * <p>The VolumeUtils class is a collection of functions and constants used to
 * manipulate volume values.</p>
 *
 * @author Joel Clawson
 */
public final class VolumeUtils {

	/**
	 * The constant to define the acre feet unit of volume.
	 */
	public static final VolumeUnit ACRE_FEET = 
		(new VolumeUtils()).new VolumeUnit("af");
	
	/**
	 * The constant to define the cubic centimeter (cm<sup>3</sup>) unit of 
	 * volume.
	 */
	public static final VolumeUnit CUBIC_CENTIMETERS = 
		(new VolumeUtils()).new VolumeUnit("cm3");
	
	/**
	 * The constant to define the cubic feet (ft<sup>3</sup> unit of volume.
	 */
	public static final VolumeUnit CUBIC_FEET = 
		(new VolumeUtils()).new VolumeUnit("ft3");
	
	/**
	 * The constant to define the cubic inches (in<sup>3</sup>) unit of volume.
	 */
	public static final VolumeUnit CUBIC_INCHES = 
		(new VolumeUtils()).new VolumeUnit("in3");
	
	/**
	 * The constant to define the cubic meters (m<sup>3</sup>) unit of volume.
	 */
	public static final VolumeUnit CUBIC_METERS = 
		(new VolumeUtils()).new VolumeUnit("m3");
	
	/**
	 * The constant to define the gallons unit of volume.
	 */
	public static final VolumeUnit GALLONS = 
		(new VolumeUtils()).new VolumeUnit("gal");

	/**
	 * The constant to define the liters unit of volume.
	 */
	public static final VolumeUnit LITERS = 
		(new VolumeUtils()).new VolumeUnit("liter");
	
	/**
	 * The constant to define the milliliters (ml) unit of volume.
	 */
	public static final VolumeUnit MILLILITERS = 
		(new VolumeUtils()).new VolumeUnit("ml");
	
	/**
	 * The constant to define the ounces (oz) unit of volume.
	 */
	public static final VolumeUnit OUNCES = 
		(new VolumeUtils()).new VolumeUnit("oz");
	
	/**
	 * The constant to define the pints (pt) unit of volume.
	 */
	public static final VolumeUnit PINTS = 
		(new VolumeUtils()).new VolumeUnit("pt");
	
	/**
	 * The constant to define the quarts (qt) unit of volume.
	 */
	public static final VolumeUnit QUARTS = 
		(new VolumeUtils()).new VolumeUnit("qt");
	
	private VolumeUtils() {}
	
	/**
	 * Convert a volume value from unit of volume to another.
	 * @param value The value to be converted.
	 * @param inUnit The initial volume unit of the value.
	 * @param outUnit The desired volume unit for the value.
	 * @return The value in the desired volume unit.
	 * @throws InvalidArgumentException if either the input or output volume 
	 * unit is <code>null</code>.
	 */
	public static Double convertVolume(Double value, VolumeUnit inUnit, 
			VolumeUnit outUnit) throws ConversionException {
		// Just return the value if it is null
		if (value == null) { return null; }
		
		// Make sure the input unit is valid.
		if (inUnit == null) {
			throw new ConversionException("convertVolume","inUnit",
					"The input unit was null.");
		}
		// Make sure the output unit is valid.
		if (outUnit == null) {
			throw new ConversionException("convertVolume","outUnit",
					"The output unit was null.");
		}
		
		// If the input and output units are the same, don't convert and just
		// return the value.
		if (inUnit.equals(outUnit)) { return value; }
		
		// Convert the value to MILLILITERS
		if (inUnit.equals(ACRE_FEET)) { 
			value *= 43560.0 * pow(convertLength(1.0, FEET, CENTIMETERS), 3.0);
		} else if (inUnit.equals(CUBIC_FEET)) { 
			value *= pow(convertLength(1.0, FEET, CENTIMETERS), 3.0); }
		else if (inUnit.equals(CUBIC_INCHES)) { 
			value *= pow(convertLength(1.0, INCHES, CENTIMETERS), 3.0); }
		else if (inUnit.equals(CUBIC_METERS)) { 
			value *= pow(convertLength(1.0, METERS, CENTIMETERS), 3.0); }
		else if (inUnit.equals(GALLONS)) { value *= (34628947.0 / 9148.0); }
		else if (inUnit.equals(LITERS)) { value *= 1000.0; }
		else if (inUnit.equals(OUNCES)) { value *= (34628947.0 / 1170944.0); }
		else if (inUnit.equals(PINTS)) { value *= (34628947.0 / 73184.0); }
		else if (inUnit.equals(QUARTS)) { value *= (34628947.0 / 36592.0); }
		else {} // Value is already in MILLILITERS
		
		// Convert the MILLILITER value to the output unit
		if (outUnit.equals(ACRE_FEET)) { 
			value *= pow(convertLength(1.0, CENTIMETERS, FEET), 3.0) / 43560.0;
		} else if (outUnit.equals(CUBIC_FEET)) { 
			value *= pow(convertLength(1.0, CENTIMETERS, FEET), 3.0); }
		else if (outUnit.equals(CUBIC_INCHES)) { 
			value *= pow(convertLength(1.0, CENTIMETERS, INCHES), 3.0); }
		else if (outUnit.equals(CUBIC_METERS)) { 
			value *= pow(convertLength(1.0, CENTIMETERS, METERS), 3.0); }
		else if (outUnit.equals(GALLONS)) { value *= (9148.0 / 34628947.0); }
		else if (outUnit.equals(LITERS)) { value = value / 1000.0; }
		else if (outUnit.equals(OUNCES)) { value *= (1170944.0 / 34628947.0); }
		else if (outUnit.equals(PINTS)) { value *= (73184.0 / 34628947.0); }
		else if (outUnit.equals(QUARTS)) { value *= (36592.0 / 34628947.0); }
		else {} // Value is already in MILLILITERS
		
		return value;
	}

	/**
	 * The VolumeUnit class is a MeasurementUnit for defining a volume.  It is a
     * public class to be available to all other classes, with the constructors
     * being private to only allow the containing class to generate new 
     * instances of the class.
	 */
	public class VolumeUnit extends MeasurementUnit<VolumeUnit> {
		
		/**
		 * Create a new instance of a VolumeUnit.
		 * @param name The name of the unit of volume.
		 */
		private VolumeUnit(String name) { super(name); }
	}
}
