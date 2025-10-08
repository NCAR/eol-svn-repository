package dmg.util;

import static dmg.util.PressureUtils.*;

/**
 * <p>The HumidityUtils class is a collection of functions and constants used to
 * manipulate humidity values.</p>
 *
 * @author Joel Clawson
 */
public final class HumidityUtils {

    /**
     * Create a new instance of a HumidityUtils.  It is private to prevent
     * other classes from creating new instances.
     */
    private HumidityUtils() {}
    
    /**
     * Calculate the relative humidity from the temperature and dew point.
     * @param temp The temperature value in &deg;C.
     * @param dewpt The dew point value in &deg;C.
     * @return The relative humidity in percent.
     * @throws CalculationException if either the temperature or dew point 
     * values are <code>null</code> or the dew point or temperature values cause
     * an exception in the vapor pressure calculation.
     * @see dmg.util.PressureUtils#calculateVaporPressure(Double)
     */
    public static Double calculateRelativeHumidity(Double temp, Double dewpt) 
	throws CalculationException
    {
	// Make sure the temperature is valid.
	if (temp == null) {
	    throw new CalculationException("calculateRelativeHumidity",
					   "temperature","The temperature value was null.");
	}
	// Make sure the dew point is valid.
	if (dewpt == null) {
	    throw new CalculationException("calculateRelativeHumidity",
					   "dew point","The dew point value was null.");
	}
	
	// Perform the relative humidity calculation.
	try {
	    return 100.0 * calculateVaporPressure(dewpt) / 
		calculateVaporPressure(temp);
	} catch (CalculationException e) {
	    throw new CalculationException("calculateRelativeHumidity",
					   "calculation",e.getMessage());
	}
    }
    
    /**
     * Calculate the specific humidity from the pressure and dew point.
     * @param pressure The pressure value in millibars.
     * @param dewPoint The dew point value in &deg;C.
     * @return The specific humidity value.
     * @throws CalculationException if either the pressure or dew point
     * values are <code>null</code> or if the vapor pressure calculation has a 
     * problem.
     * @see dmg.util.PressureUtils#calculateVaporPressure(Double)
     */
    public static Double calculateSpecificHumidity(Double pressure, Double dewPoint) 
	throws CalculationException
    {
	// Make sure there is a valid pressure
	if (pressure == null) {
	    throw new CalculationException("calculateSpecificHumidity",
					   "pressure","The pressure value was null.");
	}
	// Make sure there is a valid dew point
	if (dewPoint == null) {
	    throw new CalculationException("calculateSpecificHumidity",
					   "dew point","The dew point value was null.");
	}
	
	// Rap the CalculationException of a subcalculation of the calculation
	// into a CalcualtionException for the specific humidity.
	try {
	    Double vaporPressure = calculateVaporPressure(dewPoint);
	    return EPS * vaporPressure / (pressure - (.378 * vaporPressure));
	} catch (CalculationException e) {
	    throw new CalculationException("calculateSpecificHumidity",
					   "calculation",e.getMessage());
	}
    }
}
