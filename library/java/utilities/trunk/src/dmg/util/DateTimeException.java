package dmg.util;

/**
 * <p>The DateTimeException is a DefaultException when there is a problem
 * with a Date/Time value.  This could be in creating a new date, shifting
 * time zones, etc.</p>
 *
 * @author Joel Clawson
 **/
public class DateTimeException extends DefaultException {

    private static final long serialVersionUID = 6373597824702855194L;
    
    /**
     * Create a new instance of a DateTimeException.
     * @param message The error message for this exception.
     **/
    public DateTimeException(String message) { super(message); }
}
