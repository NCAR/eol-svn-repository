package dmg.util;

import static dmg.util.PressureUtils.*;
import static java.lang.Math.*;

/**
 * <p>The TemperatureUtils class is a collection of functions and constants 
 * used to manipulate temperature values.</p>
 *
 * @author Joel Clawson
 */
public final class TemperatureUtils implements GeneralConstants {

    /**
     * The constant to define the &deg;C unit of temperature.
     */
    public static final TemperatureUnit CELCIUS = 
	(new TemperatureUtils()).new TemperatureUnit("C");
    
    /**
     * The constant to define the &deg;F unit of temperature.
     */
    public static final TemperatureUnit FAHRENHEIT = 
    	(new TemperatureUtils()).new TemperatureUnit("F");
    
    /**
     * The constant to define the Kelvin unit of temperature.
     */
    public static final TemperatureUnit KELVIN = 
    	(new TemperatureUtils()).new TemperatureUnit("K");
    
    /**
     * Create a new instance of a TemperatureUtils.  It is private to prevent 
     * any other classes from generating a new instance.
     */
    private TemperatureUtils() {}
    
    /**
     * Calculate the dew point from the temperature and relative humidity.
     * @param temp The temperature value in &deg;C.
     * @param rh The relative humidity in percent.
     * @return The dew point in &deg;C.
     * @throws CalculationException if either the temperature or relative 
     * humidity values are <code>null</code>, the relative humidity is not 
     * positive, or if the temperature is -243.5 (which will cause a divide by 
     * zero in the vapor pressure calculation).
     * @see dmg.util.PressureUtils#calculateVaporPressure(Double)
     */
    public static Double calculateDewPoint(Double temp, Double rh) 
    throws CalculationException {
    	// Make sure the temperature is valid
    	if (temp == null) {
    		throw new CalculationException("calculateDewPoint","temperature",
    				"The temperature value was null.");
    	}
    	// Make sure the rh is valid
    	if (rh == null) {
    		throw new CalculationException("calculateDewPoint", 
    				"relative humidity",
    				"The relative humidity value was null.");
    	} else if (rh <= 0) {
    		throw new CalculationException("calculateDewPoint",
    				"relative humidity",
    				String.format("The relative humidity value %f must be " +
    						"greater than 0.",rh));
    	}

    	// Perform the dew point calculation
    	try {
    		Double logValue = 
    			log((calculateVaporPressure(temp) * rh / 100.0) / ES0);
    		return logValue * 243.5 / (17.67 - logValue);
    	} catch (CalculationException e) {
    		// Wrap the vapor pressure error into a dew point error.
    		throw new CalculationException("calculateDewPoint","calculation",
    				e.getMessage());
    	}
    }

    /**
     * Calculate the virtual temperature.
     * @param temp The temperature value in Kelvin.
     * @param mixRatio The mixing ratio value in mbar/mbar.
     * @return The virtual temperature in Kelvin.
     * @throws CalculationException if either the temperature or mixing 
     * ratio is <code>null</code> or if the mixing ratio is -1.0 (which will 
     * result in a divide by zero error).
     */
    public static Double calculateVirtualTemperature(Double temp, Double mixRatio)
	throws CalculationException
    {
    	// Make sure the temperature is valid
    	if (temp == null) {
    		throw new CalculationException("virtualTemperature",
    				"temperature","The temperature value was null.");
    	}
    	// Make sure the mixing ratio is valid.
    	if (mixRatio == null) {
    		throw new CalculationException("virtualTemperature",
    				"mixingRatio","The mixing ratio value was null.");
    	} else if (mixRatio == -1.0) {
    		throw new CalculationException("virtualTemperature",
    				"mixingRatio","The mixing ratio value was - 1.0 which " +
    						"results in a divide by zero error.");
    	}

    	// Perform the virtual temperature calculation.
    	return temp * (1.0 + mixRatio/EPS) / (1.0 + mixRatio);
    }
    
    /**
     * Convert a value in one unit of temperature to another unit.
     * @param value The value to be converted.
     * @param inUnit The unit of temperature of the initial value.
     * @param outUnit The desired unit of temperature.
     * @return The value in the desired output units or <code>null</code> if 
     * the input value was <code>null</code>.
     * @throws ConversionException if either the input or output unit values 
     * are <code>null</code>.
     */
    public static Double convertTemperature(Double value, TemperatureUnit inUnit,
					    TemperatureUnit outUnit) throws ConversionException
    {
    	// Just return the value if it is null
    	if (value == null) { return null; }
	
    	// Make sure the input unit is valid.
    	if (inUnit == null) {
    		throw new ConversionException("convertTemperature","inUnit",
    				"The input unit was null.");
    	}
    	// Make sure the output unit is valid.
    	if (outUnit == null) {
    		throw new ConversionException("convertTemperature","outUnit",
    				"The output unit was null.");
    	}

    	// If the input and output units are the same, don't convert and just
    	// return the value.
    	if (inUnit.equals(outUnit)) { return value; }

    	// Convert the value to CELCIUS
    	if (inUnit.equals(FAHRENHEIT)) { value = (value - 32) * 5 / 9; }
    	else if (inUnit.equals(KELVIN)) { value = value - 273.15; }
    	else {} // Value is already in CELCIUS

    	// Convert the CELCIUS value to the output unit
    	if (outUnit.equals(FAHRENHEIT)) { value = value * 9 / 5 + 32; }
    	else if (outUnit.equals(KELVIN)) { value = value + 273.15; }
    	else {} // Value is already in CELCIUS

    	// The conversion was successul, so return the converted value.
    	return value;
    }

    /**
     * The TemperatureUnit class is a MeasurementUnit for defining a 
     * temperature.  It is a public class to be available to all other classes,
     * with the constructors being private to only allow the containing class to
     * generate new instances of the class.
     */
    public class TemperatureUnit extends MeasurementUnit<TemperatureUnit> {

    	/**
    	 * Create a new instance of a TemperatureUnit.
    	 * @param name The name of the unit of temperature.
    	 */
    	private TemperatureUnit(String name) { super(name); }
    }
}
