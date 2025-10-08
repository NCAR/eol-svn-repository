package dmg.util;

import static dmg.util.PressureUtils.*;
import static dmg.util.TemperatureUtils.*;
import static java.lang.Math.*;

/**
 * <p>The LengthUtils class is a collection of functions and constants used to
 * manipulation length/distance values.</p>
 *
 * @author Joel Clawson
 */
public final class LengthUtils {
    
    /**
     * The constant to define the centimeter (cm) unit of length.
     */
    public static final LengthUnit CENTIMETERS = (new LengthUtils()).new LengthUnit("cm");

    /**
     * The constant to define the decimeter (dm) unit of length.
     */
    public static final LengthUnit DECIMETERS = (new LengthUtils()).new LengthUnit("dm");
	
    /**
     * The constant to define the feet (ft) unit of length.
     */
    public static final LengthUnit FEET = (new LengthUtils()).new LengthUnit("ft");
	
    /**
     * The constant to define the hundredths of an inch (hin) unit of length.
     */
    public static final LengthUnit HUNDREDTHS_INCH = (new LengthUtils()).new LengthUnit("hin");
	
    /**
     * The constant to define the inch (in) unit of length.
     */
    public static final LengthUnit INCHES = (new LengthUtils()).new LengthUnit("in");
	
    /**
     * The constant to define the kilometer (km) unit of length.
     */
    public static final LengthUnit KILOMETERS = (new LengthUtils()).new LengthUnit("km");
	
    /**
     * The constant to define the meter (m) unit of length.
     */
    public static final LengthUnit METERS = (new LengthUtils()).new LengthUnit("m");
	
    /**
     * The constant to define the mile unit of length.
     */
    public static final LengthUnit MILES = (new LengthUtils()).new LengthUnit("mile");
	
    /**
     * The constant to define the millimeter (mm) unit of length.
     */
    public static final LengthUnit MILLIMETERS = (new LengthUtils()).new LengthUnit("mm");
	
    /**
     * The constant to define the nautical mile unit of length.
     */
    public static final LengthUnit NAUTICAL_MILES = (new LengthUtils()).new LengthUnit("nmile");
	
    /**
     * The constant to define the tenths of an inch (tin) unit of length.
     */
    public static final LengthUnit TENTHS_INCH = (new LengthUtils()).new LengthUnit("tin");
	
    /**
     * The constant to define the yard (yd) unit of length.
     */
    public static final LengthUnit YARDS = (new LengthUtils()).new LengthUnit("yd");

    /**
     * Create a new instance of a LengthUtils.  It is private to prevent any
     * other classes from generating a new instance.
     */
    private LengthUtils() {}
	
    /**
     * Calculate the altitude.
     * @param prevPress The pressure at the previous location in mbar.
     * @param prevTemp The temperature at the previous location in &deg;C.
     * @param prevAlt The altitude at the previous location in meters.
     * @param currPress The pressure at the current location in mbar.
     * @param currTemp The temperature at the current location in &deg;C.
     * @return The altitude at the current location in meters.
     * @throws CalculationException if any of the altitude, pressure, or 
     * temperature values are <code>null</code> or one of the values causes a 
     * virtual temperature, mixing ratio, vapor pressure, or air density to not 
     * be calculated.
     * @see dmg.util.PressureUtils#calculateAirDensity(Double, Double)
     * @see dmg.util.PressureUtils#calculateMixingRatio(Double, Double)
     * @see dmg.util.PressureUtils#calculateVaporPressure(Double)
     * @see dmg.util.TemperatureUtils#calculateVirtualTemperature(Double, Double)
     */
    public static Double calculateAltitude(Double prevPress, Double prevTemp, 
					   Double prevAlt, Double currPress, Double currTemp)
	throws CalculationException
    {
	// Make sure there is a valid previous pressure value.
	if (prevPress == null) {
	    throw new CalculationException("calculateAltitude", "previous pressure",
					   "The previous pressure value was null.");
	}
	// Make sure there is a valid previous temperature value.
	if (prevTemp == null) {
	    throw new CalculationException("calculateAltitude", "previous temperature",
					   "The previous temperature value was null.");
	}
	// Make sure there is a valid previous altitude value.
	if (prevAlt == null) {
	    throw new CalculationException("calcualteAltitude", "previous altitude",
					   "The previous altitude value was null.");
	}
	// Make sure there is a valid current pressure value.
	if (currPress == null) {
	    throw new CalculationException("calculateAltitude", "current pressure",
					   "The current pressure value was null.");
	}
	// Make sure there is a valid current temperature value.
	if (currTemp == null) {
	    throw new CalculationException("calculateAltitude", "current temperature",
					   "The current temperature value was null.");
	}
	
	Double prevTempK, currTempK;
	// Convert the temperature values to Kelvin
	try { prevTempK = convertTemperature(prevTemp, CELCIUS, KELVIN); }
	catch (ConversionException e) {
	    throw new CalculationException("calculateAltitude", "temperature conversion", 
					   "The previous temperature value could not be converted to Kelvin.");
	}
	try { currTempK = convertTemperature(currTemp, CELCIUS, KELVIN); }
	catch (ConversionException e) {
	    throw new CalculationException("calculateAltitude", "temperature conversion", 
					   "The current temperature value could not be converted to Kelvin.");
	}
	
	try {
	    // Calculate the values from the previous data values.
	    Double prevVirtTemp = 
		calculateVirtualTemperature(prevTempK, 
					    calculateMixingRatio(prevPress, 
								 calculateVaporPressure(prevTemp)));
	    Double prevAirDensity = calculateAirDensity(prevPress, prevVirtTemp);
	    
	    // Calculate the values from the current data values.
	    Double currVirtTemp = 
		calculateVirtualTemperature(currTempK, 
					    calculateMixingRatio(currPress, 
								 calculateVaporPressure(currTemp)));
	    Double currAirDensity = calculateAirDensity(currPress, currVirtTemp);
	    
	    // Calculate the average virtual temperature.
	    Double avgVirtTemp = (currAirDensity * currVirtTemp + prevAirDensity
				  * prevVirtTemp) / (currAirDensity + prevAirDensity);
	    
	    // Calculate the current altitude.
	    return prevAlt + (R_DRY * avgVirtTemp * log(prevPress / currPress) / GRAVITY);
	} catch (CalculationException e) {
	    throw new CalculationException("calculateAltitude","calculation", e.getMessage());
	}
    }

    /**
     * Convert a value in one unit of length to another unit.
     * @param value The value to be converted.
     * @param inUnit The unit of length of the initial value.
     * @param outUnit The desired unit of length.
     * @return The value in the desired output units or <code>null</code> if the
     * input value was <code>null</code>.
     * @throws ConversionException if either the input or output unit 
     * values are <code>null</code>.
     */
    public static Double convertLength(Double value, LengthUnit inUnit, LengthUnit outUnit)
	throws ConversionException 
    {
	// Just return the value if it is null
	if (value == null) { return null; }
	
	// Make sure the input unit is valid.
	if (inUnit == null) {
	    throw new ConversionException("convertLength","inUnit",
					  "The input unit was null.");
	}
	// Make sure the output unit is valid.
	if (outUnit == null) {
	    throw new ConversionException("convertLength","outUnit",
					  "The output unit was null.");
	}
	
	// If the input and output units are the same, don't convert and just
	// return the value.
	if (inUnit.equals(outUnit)) { return value; }
	
	// Convert the input value to meters
	if (inUnit.equals(CENTIMETERS)) { value = value / 100; }
	else if (inUnit.equals(DECIMETERS)) { value = value / 10; }
	else if (inUnit.equals(FEET)) { value = value * .3048; }
	else if (inUnit.equals(HUNDREDTHS_INCH)) { value = value * .000254; }
	else if (inUnit.equals(INCHES)) { value = value * .0254; }
	else if (inUnit.equals(KILOMETERS)) { value = value * 1000; }
	else if (inUnit.equals(MILES)) { value = value * 1609.344; }
	else if (inUnit.equals(MILLIMETERS)) {	value = value / 1000; }
	else if (inUnit.equals(NAUTICAL_MILES)) { value = value * 1852; }
	else if (inUnit.equals(TENTHS_INCH)) { value = value * .00254; }
	else if (inUnit.equals(YARDS)) { value = value * .9144; } 
	else {} // Value is already in meters
	
		// Convert the value (now in meters) to the output unit
	if (outUnit.equals(CENTIMETERS)) { value = value * 100; }
	else if (outUnit.equals(DECIMETERS)) { value = value * 10; }
	else if (outUnit.equals(FEET)) { value = value / .3048; }
	else if (outUnit.equals(HUNDREDTHS_INCH)) { value = value / .000254; }
	else if (outUnit.equals(INCHES)) { value = value / .0254; }
	else if (outUnit.equals(KILOMETERS)) { value = value / 1000; }
	else if (outUnit.equals(MILES)) { value = value / 1609.344; }
	else if (outUnit.equals(MILLIMETERS)) { value = value * 1000; }
	else if (outUnit.equals(NAUTICAL_MILES)) { value = value / 1852; }
	else if (outUnit.equals(TENTHS_INCH)) { value = value / .00254; }
	else if (outUnit.equals(YARDS)) { value = value / .9144; }
	else {} // Desired value is meters
	
		// The conversion was successul, so return the converted value.
	return value;
    }
    
    /**
     * The LengthUnit class is a MeasurementUnit for defining a length/distance.
     * It is a public class to be available to all other classes, with the 
     * constructors being private to only allow the containing class to generate
     * new instances of the class.
     */
    public class LengthUnit extends MeasurementUnit<LengthUnit> {
	
	/**
	 * Create a new instance of a LengthUnit.
	 * @param name The name of the unit of length.
	 */
	private LengthUnit(String name) { super(name); }
    }
}
