package dmg.util;

/**
 * <p>The ConversionException is a DefaultException that is used when a value
 * cannot be converted from unit of measurement to another unit.</p>
 *
 * @author Joel Clawson
 **/
public class ConversionException extends DefaultException {

    private static final long serialVersionUID = -1979155203532153029L;
    
    /**
     * Create a new instance of a ConversionException.
     * @param function The function that generated this exception.
     * @param parameter The parameter that caused this exception.
     * @param msg The error message for this exception.
     **/
    public ConversionException(String function, String parameter, String msg) {
	super(String.format("Argument %s to %s is invalid.  %s", parameter, function, msg));
    }
}
