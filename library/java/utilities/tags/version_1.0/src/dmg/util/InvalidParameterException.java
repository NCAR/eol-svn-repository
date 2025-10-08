package dmg.util;

/**
 * <p>The InvalidParameterException is a DefaultException that is used when
 * parsing the command line arguments of a <code>main</code> function for a
 * program.  This is to differentiate between other parameter/argument
 * exceptions in the Java library or used in other user defined libraries.</p>
 *
 * @author Joel Clawson
 **/
public class InvalidParameterException extends DefaultException {

    private static final long serialVersionUID = -4995598488902758117L;
    
    /**
     * Create a new instance of an InvalidParameterException.
     * @param function The name of the function that generated the exception.
     * @param name The name of the parameter that caused the exception.
     * @param msg The error message for the exception.
     **/
    public InvalidParameterException(String function, String name, String msg) {
	super(String.format("%s has an invalid parameter list %s.  %s",
			    function, name, msg));
    }
	
    /**
     * Create a new instance of an InvalidParameterException.
     * @param function The name of the function that generated the exception.
     * @param name The name of the parameter that caused the exception.
     * @param value The value of the parameter that caused the exception.
     * @param msg The error message for the exception.
     **/
    public InvalidParameterException(String function, String name, Object value, String msg) {
	super(String.format("%s has an invalid parameter %s (%s).  %s",
			    function, name, value.toString(), msg));
    }
}
