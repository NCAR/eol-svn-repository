package dmg.util;

/**
 * The InvalidValueException is a DefaultException that is to be used when a
 * value will cause some other object to enter an invalid state.  This is a
 * critical exception.  There is also an InvalidValueWarning that prevents
 * the invalid value from being set and is only a warning.
 * @see InvalidValueWarning
 *
 * @author Joel Clawson
 **/
public class InvalidValueException extends DefaultException {

    private static final long serialVersionUID = -8396579366876482028L;
    
    private Object value;

    /**
     * Create a new instance of an InvalidValueException.
     * @param name The name of the variable that is invalid.
     * @param value The invalid value of the variable.
     * @param msg The error message for the exception.
     **/
    public InvalidValueException(String name, Object value, String msg) {
	super(String.format("%s has an illegal value of %s.  %s", 
			    name, value.toString(), msg));
	this.value = value;
    }
	
    /**
     * Create a new instance of an InvalidValueException.
     * @param name The name of the variable that is invalid.
     * @param value The invalid value of the variable.
     * @param lower The lower boundary allowed for the value.
     * @param upper The upper boundary allowed for the value.
     **/
    public InvalidValueException(String name, Object value, Object lower, Object upper) {
	super(String.format("%s with value %s should have been between "+
			    "%s and %s.", name, value.toString(), lower.toString(),
			    upper.toString()));
	this.value = value;
    }
    
    /**
     * Get the illegal value that generated this exception.
     * @return The value that generated the exception.
     **/
    public Object getValue() { return value; }
}
