package dmg.util;

import static java.lang.Math.*;

/**
 * <p>The PositionUtils class is a collection of functions and constants used in
 * manipulating position values.  The class currently only contains manipulation
 * tools for latitude/longitude values.</p>
 * 
 * @author Joel Clawson
 */
public final class PositionUtils {

    /**
     * The constant to define a latitude north of the equator.
     */
    public static final LatitudeUnit NORTH = (new PositionUtils()).new LatitudeUnit("N");
	
    /**
     * The constant to define a latitude south of the equator.
     */
    public static final LatitudeUnit SOUTH = (new PositionUtils()).new LatitudeUnit("S");
    
    /**
     * The constant to define a longitude east of the prime meridian.
     */
    public static final LongitudeUnit EAST = (new PositionUtils()).new LongitudeUnit("E");
	
    /**
     * The constant to define a longitude wast of the prime meridian.
     */
    public static final LongitudeUnit WEST = (new PositionUtils()).new LongitudeUnit("W");

    /**
     * Create a new instance of a PositionUtils.  It is private to prevent any
     * other class from creating an instance.
     */
    private PositionUtils() {}
    
    /**
     * Convert a DMS value to its decimal degree equivalent.
     * @param degrees The degrees of the DMS value.
     * @param minutes The minutes of the DMS value.
     * @param seconds The seconds of the DMS value.
     * @return The decimal degree value.
     * @throws ConversionException if any of the arguments are null or if 
     * the minutes or seconds do not fall within the valid range for the 
     * argument.
     */
    private static Double toDegrees(Double degrees, Double minutes, Double seconds) 
	throws ConversionException
    {
	// Make sure the degrees is a valid value.
	if (degrees == null) { 
	    throw new ConversionException("toDegrees","degrees",
					  "The degrees value was null.");
	}
	// Make sure the minutes is a valid value.
	if (minutes == null) {
	    throw new ConversionException("toDegrees","minutes",
					  "The minutes value was null.");
	} else if (minutes < 0 || minutes > 60) {
	    throw new ConversionException("toDegrees","minutes",
					  String.format("The minutes value %f was not between 0 " +
							"and 60.",minutes));
	}
	// Make sure the seconds is a valid value.
	if (seconds == null) {
	    throw new ConversionException("toDegrees","seconds",
					  "The seconds value was null.");
	} else if (seconds < 0 || seconds > 60) {
	    throw new ConversionException("toDegrees","seconds",
					  String.format("The seconds value %f was not between 0" +
							" and 60.",seconds));
	}
	
	// The multiplier is used to make sure the minutes and seconds make the
	// decimal degree value more negative if the degree parameter is 
	// negative
	double multiplier = degrees < 0 ? -1.0 : 1.0;		
	
	// Calculate the decimal degree value.
	return degrees + (multiplier * minutes / 60) + 
	    (multiplier * seconds / 3600);
    }

    /**
     * Convert a latitude in degrees, minutes, and seconds to its decimal degree equivalent.
     * @param degrees The degrees of the latitude.
     * @param minutes The minutes of the latitude.
     * @param seconds The seconds of the latitude.
     * @param direction The direction relative to the equator of the latitude.
     * @return The latitude in decimal degrees north of the equator.
     * @throws ConversionException if any of the arguments are null, or if one 
     * of the arguments does not fall within the valid range for the argument.
     * @throws InvalidValueWarning if the calculated latitude is not between -90 and 90 degrees.
     */
    public static Double toDegrees(Double degrees, Double minutes, Double seconds, 
				   LatitudeUnit direction) 
	throws ConversionException, InvalidValueWarning 
    {
	// Make sure the direction is not null
	if (direction == null) {
	    throw new ConversionException("toDegrees (latitude)","direction",
					  "The direction value was null.");
	}
	// Make sure the degrees value is not null
	if (degrees == null) {
	    throw new ConversionException("toDegrees (latitude)","degrees",
					  "The degrees value was null.");
	}
	
	// Calculate the latitude
	Double latitude = toDegrees(direction.equals(NORTH) ? degrees :
				    -1.0 * degrees, minutes, seconds);
	
	// Make sure the latitude falls within the range of -90 and 90.
	if (abs(latitude) > 90) {
	    throw new InvalidValueWarning("latitude", latitude, -90.0, 90.0);
	}
	
	// The latitude was calculate successfully and is within the valid 
	// range.
	return latitude;
    }
	
    /**
     * Convert a longitude in degrees, minutes, and seconds to its decimal degree equivalent.
     * @param degrees The degrees of the longitude.
     * @param minutes The minutes of the longitude.
     * @param seconds The seconds of the longitude.
     * @param direction The direction relative to the prime meridian of the longitude.
     * @return The longitude in decimal degrees east of the prime meridian.
     * @throws ConversionException if any of the arguments are null, or if one 
     * of the arguments does not fall within the valid range for the argument.
     * @throws InvalidValueWarning if the calculated longitude is not between -180 and 180.
     */
    public static Double toDegrees(Double degrees, Double minutes, Double seconds, 
				   LongitudeUnit direction)
	throws ConversionException, InvalidValueWarning
    {
	// Make sure the direction is not null
	if (direction == null) {
	    throw new ConversionException("toDegrees (longitude)","direction",
					  "The direction value was null.");
	}
	// Make sure the degrees value is not null.
	if (degrees == null) {
	    throw new ConversionException("toDegrees (longitude)","degrees",
					  "The degrees value was null.");
	}
	
	// Calculate the longitude.
	Double longitude = toDegrees(direction.equals(EAST) ? degrees : 
				     -1.0 * degrees, minutes, seconds);
	
	// Make sure the longitude falls within the range -180 to 180.
	if (abs(longitude) > 180) {
	    throw new InvalidValueWarning("longitude", longitude,
					  -180.0, 180.0);
	}
	
	// The longitude was calculated successfully and is within the valid 
	// range.
	return longitude;
    }
    
    /**
     * Convert a value in decimal degrees to it equivalent degrees and minutes value.
     * @param degrees The value in degrees to be converted.
     * @return The array containing the degrees than minutes of the value.
     */
    public static Double[] toDegreesMinutes(Double degrees) {
	Double[] parts = new Double[2];
	parts[0] = floor(abs(degrees));
	parts[1] = (abs(degrees) - parts[0]) * 60.0;
	parts[0] *= degrees < 0 ? -1.0 : 1.0;
	return parts;
    }
    
    /**
     * Convert a value in decimal degrees to its equivalent degrees, minutes,and seconds value.
     * @param degrees The value in degrees to be converted.
     * @return The array containing the ordered degrees, minutes, and seconds of the value.
     */
    public static Double[] toDegreesMinutesSeconds(Double degrees) {
	Double[] parts = new Double[3];
	Double[] dm = toDegreesMinutes(degrees);
	parts[0] = dm[0];
	parts[1] = floor(dm[1]);
	parts[2] = (dm[1] - parts[1]) * 60.0;
	return parts;
    }
    
    /**
     * The LatitudeUnit class is a MeasurementUnit for defining a latitude 
     * direction relative to the equator.  It is a public class to be available
     * to all other classes with the constructors being private to only allow
     * the containing class to generate new instances of the class.
     */
    public class LatitudeUnit extends MeasurementUnit<LatitudeUnit> {
	
	/**
	 * Create a new instance of a LatitudeUnit.
	 * @param name The name of the latitude unit.
	 */
	private LatitudeUnit(String name) { super(name); }
    }
    
    /**
     * The LongitudeUnit class is a MeasurementUnit for defining a longitude 
     * direction relative to the prime meridian.  It is a public class to be 
     * available to all other classes, with the constructors being private to 
     * only allow the containing class to generate new instances of the class.
     */
    public class LongitudeUnit extends MeasurementUnit<LongitudeUnit> {
	
	/**
	 * Create a new instance of a LongitudeUnit.
	 * @param name The name of the longitude unit.
	 */
	private LongitudeUnit(String name) { super(name); }
    }
}
