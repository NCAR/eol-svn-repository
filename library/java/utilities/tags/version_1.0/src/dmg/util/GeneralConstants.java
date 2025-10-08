package dmg.util;

/**
 * The GeneralConstants interface is a collection of global constants that are not
 * related specifically to any singular group of calculations.  It should be inherited
 * by any class that wants to use one of the constants defined in it.
 *
 * @author Joel Clawson
 **/
public interface GeneralConstants {

    /**
     * Constant used in calculation a virtual temperature.
     */
    public static final Double EPS = .62197;

    /**
     * Constant used in calculating a vapor pressure or dew point.
     */
    public static final Double ES0 = 6.1121;

    /**
     * The constant for the acceleration due to gravity in m/s<sup>2</sup>.
     */
    public static final Double GRAVITY = 9.80616;

    /**
     * The constant for the pressure lapse rate in &deg;C/m.
     */
    public static final Double PRESSURE_LAPSE = .0065;
    
    /**
     * The constant for something to do with dry air in J/kg/K
     */
    public static final Double R_DRY = 287.04;

    /**
     * The constant for the standard sea level pressure in millibars.
     */
    public static final Double STANDARD_SEA_LEVEL_PRESSURE = 1013.25;

    /**
     * Constant for the standard sea level temperature in Kelvin.
     */
    public static final Double STANDARD_SEA_LEVEL_TEMPERATURE_K = 288.15;
}
