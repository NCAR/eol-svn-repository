package dmg.util;

import static dmg.util.TemperatureUtils.*;
import static java.lang.Math.*;

/**
 * <p>The PressureUtils class is a collection of functions and constants used in
 * manipulating pressure values.</p>
 * 
 * @author Joel Clawson
 */
public final class PressureUtils implements GeneralConstants {

    /**
     * The constant to define the atmosphere unit of pressure.
     */
    public static final PressureUnit ATMOSPHERES = (new PressureUtils()).new PressureUnit("atm");

    /**
     * The constant to define the bars unit of pressure.
     */
    public static final PressureUnit BARS = (new PressureUtils()).new PressureUnit("bar");

    /**
     * The constant to define the dyne/cm<sup>2</sup> unit of pressure.
     */
    public static final PressureUnit DYNE_CM2 = (new PressureUtils()).new PressureUnit("dyne/cm2");

    /**
     * The constant to define the hectopascals (hPa) unit of pressure.
     */
    public static final PressureUnit HECTOPASCALS = (new PressureUtils()).new PressureUnit("hPa");

    /**
     * The constant to define the inches of mercury (inHg) unit of pressure.
     */
    public static final PressureUnit INCHES_MERCURY = (new PressureUtils()).new PressureUnit("inHg");

    /**
     * The constant to define the kilopascals unit of pressure.
     */
    public static final PressureUnit KILOPASCALS = (new PressureUtils()).new PressureUnit("kPa");

    /**
     * The constant to define the millibars unit of pressure.
     */
    public static final PressureUnit MILLIBARS = (new PressureUtils()).new PressureUnit("mbar");

    /**
     * The constant to define the millimeters of mercury (mmHg) unit of 
     * pressure.
     */
    public static final PressureUnit MILLIMETERS_MERCURY = (new PressureUtils()).new PressureUnit("mmHg");
    
    /**
     * The constant to define the pascals unit of pressure.
     */
    public static final PressureUnit PASCALS = (new PressureUtils()).new PressureUnit("Pa");

    /**
     * Create a new instance of a PressureUtils.  It is private to prevent any
     * other classes from generating a new instance.
     */
    private PressureUtils() {}
    
    /**
     * Calculate the air density from the pressure and temperature.
     * @param pressure The pressure value in mbar.
     * @param temperature The temperature value in Kelvin.
     * @return The air density value in mbar/J/kg.
     * @throws CalculationException if either the pressure or temperature  values are 
     * <code>null</code> or ir the temperature is 0 (which results in a divide by zero error).
     */
    public static Double calculateAirDensity(Double pressure, Double temperature) 
	throws CalculationException
    {
	// Make sure there is a valid pressure value.
	if (pressure == null) {
	    throw new CalculationException("calculateAirDensity","pressure",
					   "The pressure value was null.");
	}
	// Make sure there is a valid temperature value.
	if (temperature == null) {
	    throw new CalculationException("calculateAirDensity","temperature",
					   "The temperature value was null.");
	} else if (temperature == 0) {
	    throw new CalculationException("calculateAirDensity",
					   "temperature","The temperature value was 0 and caused a divide by zero error.");
	}
	
	// Calculate the Air Density.
	return pressure / (R_DRY * temperature);
    }
    
    /**
     * Calculate the mixing ratio from the pressure and vapor pressure.
     * @param press The pressure value in millibars.
     * @param vaporPress The vapor pressure in millibars.
     * @return The calculated mixing ratio in mbar/mbar.
     * @throws CalculationException if either the pressure or vapor pressure is <code>null</code>.
     */
    public static Double calculateMixingRatio(Double press, Double vaporPress)
	throws CalculationException
    {
	// Make sure the pressure is valid
	if (press == null) {
	    throw new CalculationException("calculateMixingRatio","pressure",
					   "The pressure value was null.");
	}
	// Make sure the vapor pressure is valid.
	if (vaporPress == null) {
	    throw new CalculationException("calculateMixingRatio",
					   "vapor pressure","The vapor pressure value was null.");
	}
	
	Double e = vaporPress * (1.001 + (press - 100.0) / 900.0 * .0034);
	return EPS * e / (press - e);
    }
    
    /**
     * Calculate the station pressure from the altimeter and elevation.
     * @param altimeter The altimeter value in mbar.
     * @param elevation The elevation value in meters.
     * @return The station pressure in mbar.
     * @throws CalculationException if either the altimeter or elevation value is 
     * <code>null</code> or if the calculation results in a <code>NaN</code> value.
     */
    public static Double calculatePressure(Double altimeter, Double elevation) 
	throws CalculationException
    {
	// Make sure the altimeter is valid
	if (altimeter == null) {
	    throw new CalculationException("calculatePressure","altimeter",
					   "The altimeter value was null.");
	}
	// Make sure the elevation is valid
	if (elevation == null) {
	    throw new CalculationException("calculatePressure","elevation",
					   "The elevation value was null.");
	}
	
	// Define the exponent constant.
	Double n = PRESSURE_LAPSE * R_DRY / GRAVITY;
	
	// Calculate the pressure
	Double press = pow((pow(altimeter,n) - 
			    ((pow(STANDARD_SEA_LEVEL_PRESSURE,n) * PRESSURE_LAPSE) / 
			     STANDARD_SEA_LEVEL_TEMPERATURE_K) * elevation), (1/n));
	
	if (press.equals(Double.NaN)) {
	    throw new CalculationException("calculatePressure", "pressure",
					   "The calculated pressure value was NaN.");
	}
	
	return press;
    }
    
    /**
     * Calculate the sea level pressure from the station pressure, temperature, 
     * dew point, and elevation.
     * @param press The pressure value in mbars.
     * @param temp The temperature value in &deg;C.
     * @param dewpt The dew point value in &deg;C.
     * @param elev The elevation value in meters.
     * @return The sea level pressure in mbars.
     * @throws CalculationException if any of the pressure, temperature, dew point, or 
     * elevation values are <code>null</code> or if an invalid argument is sent to the
     * calculateVirtualTemperaturefunction or the calculateMixingRatio function.
     * @see #calculateMixingRatio(Double, Double)
     * @see dmg.util.TemperatureUtils#calculateVirtualTemperature(Double, Double)
     */
    public static Double calculateSeaLevelPressure(Double press, Double temp, 
						   Double dewpt, Double elev) 
	throws CalculationException
    {
	// Make sure the pressure is valid
	if (press == null) {
	    throw new CalculationException("calculateSeaLevelPressure",
					   "pressure","The pressure value was null.");
	}
	// Make sure the temperature is valid
	if (temp == null) {
	    throw new CalculationException("calculateSeaLevelPressure",
					   "temperature","The temperature value	was null.");
	}
	// Make sure the dew point is valid.
	if (dewpt == null) {
	    throw new CalculationException("calculateSeaLevelPressure",
					   "dew point","The dew point value was	null.");
	}
	// Make sure the elevation is valid.
	if (elev == null) {
	    throw new CalculationException("calculateSeaLevelPressure",
					   "elevation","The elevation value was	null.");
	}
	
	try {
	    Double virtTempK = 
		calculateVirtualTemperature(convertTemperature(temp,CELCIUS,KELVIN),
					    calculateMixingRatio(press,
								 calculateVaporPressure(dewpt)));
	    Double deltaVTK = PRESSURE_LAPSE * elev;
	    Double avgVirtTemp = virtTempK - deltaVTK / 2.0;
	    return press * exp(GRAVITY * elev / (R_DRY * avgVirtTemp));
	} catch (CalculationException e) {
	    // Wrap the exception from the other calculations into a slp 
	    // exception
	    throw new CalculationException("calculateSeaLevelPressure",
					   "calculation",e.getMessage());
	} catch (ConversionException e) {
	    // Wrap the exception form the conversion into a slp exception
	    throw new CalculationException("calculateSeaLevelPressure",
					   "temperature conversion", e.getMessage());
	}
    }
    
    
    /**
     * Calculate the vapor pressure at the specified temperature.
     * @param temp The temperature in &deg;C.
     * @return The vapor pressure in millibars.
     * @throws CalculationException if the temperature value is <code>null</code> or -243.5 
     * (which will cause a divide by zero in the calculation).
     */
    public static Double calculateVaporPressure(Double temp) throws CalculationException {
	// Make sure the temperature is valid
	if (temp == null) {
	    throw new CalculationException("calculateVaporPressure",
					   "temperature","The temperature was null.");
	}
	// Make sure the temperature will not cause a divide by zero error
	if (temp == -243.5) {
	    throw new CalculationException("calculateVaporPressure",
					   "temperature",
					   "The temperature had a value of -243.5 which "
					   + "caused a divide by zero error.");
	}
	
	// Perform the vapor pressure calculation.
	return ES0 * exp((17.67 * temp) / (temp + 243.5));
    }
    
    
    /**
     * Convert a value in one unit of pressure to another unit.
     * @param value The value to be converted.
     * @param inUnit The unit of pressure of the initial value.
     * @param outUnit The desired unit of pressure.
     * @return The value in the desired output units or <code>null</code> if 
     * the input value was <code>null</code>.
     * @throws ConversionException if either the input or output unit values are 
     * <code>null</code>.
     */
    public static Double convertPressure(Double value, PressureUnit inUnit, PressureUnit outUnit)
	throws ConversionException
    {
	// Just return the value if it is null
	if (value == null) { return null; }
	
	// Make sure the input unit is valid.
	if (inUnit == null) {
	    throw new ConversionException("convertPressure","inUnit",
					  "The input unit was null.");
	}
	// Make sure the output unit is valid.
	if (outUnit == null) {
	    throw new ConversionException("convertPressure","outUnit",
					  "The output unit was null.");
	}
	
	// If the input and output units are the same, don't convert and just 
	// return the value.
	if (inUnit.equals(outUnit)) { return value; }

	// Convert the value to BARS
	if (inUnit.equals(ATMOSPHERES)) { value = value * 1.01325; }
	else if (inUnit.equals(DYNE_CM2)) { value = value / 1000000; }
	else if (inUnit.equals(HECTOPASCALS)) { value = value / 1000; }
	else if (inUnit.equals(INCHES_MERCURY)) { 
	    value = value * 25.73655 / 760; }
	else if (inUnit.equals(KILOPASCALS)) { value = value / 100; }
	else if (inUnit.equals(MILLIBARS)) { value = value / 1000; }
	else if (inUnit.equals(MILLIMETERS_MERCURY)) {
	    value = value * 1.01325 / 760; }
	else if (inUnit.equals(PASCALS)) { value = value / 100000; }
	else{} // Value already in BARS.
	
	// Convert the BARS value to the output units
	if (outUnit.equals(ATMOSPHERES)) { value = value / 1.01325; }
	else if (outUnit.equals(DYNE_CM2)) { value = value * 1000000; }
	else if (outUnit.equals(HECTOPASCALS)) { value = value * 1000; }
	else if (outUnit.equals(INCHES_MERCURY)) { 
	    value = value * 760 / 25.73655; }
	else if (outUnit.equals(KILOPASCALS)) { value = value * 100; }
	else if (outUnit.equals(MILLIBARS)) { value = value * 1000; }
	else if (outUnit.equals(MILLIMETERS_MERCURY)) { 
	    value = value * 760 / 1.01325; }
	else if (outUnit.equals(PASCALS)) { value = value * 100000; }
	else {} // Value already in BARS
	
		// The conversion was successul, so return the converted value.
	return value;
    }
    
    
    /**
     * The PressureUnit class is a MeasurementUnit for defining a pressure.  It
     * is a public class to be available to all other classes, with the 
     * constructors being private to only allow the containing class to generate
     * new instances of the class.
     */
    public class PressureUnit extends MeasurementUnit<PressureUnit> {
	
	/**
	 * Create a new instance of a PressureUnit.
	 * @param name The name of the unit of pressure.
	 */
	private PressureUnit(String name) { super(name); }
    }
}
