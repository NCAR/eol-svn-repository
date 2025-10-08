package dmg.util;

/**
 * The InvalidValueWarning is a DefaultWarning that is to be used when a
 * value will cause some other object to enter an invalid state, but should
 * only generate a warning and not a critical exception.  To generate the
 * critical exception, use an InvalidValueException.
 * @see InvalidValueException
 *
 * @author Joel Clawson
 **/
public class InvalidValueWarning extends DefaultWarning {

    private static final long serialVersionUID = 1055031853493451361L;
    
    private Object value;
    
    /**
     * Create a new instance of an InvalidValueWarning.
     * @param name The name of the variable that is invalid.
     * @param value The invalid value of the variable.
     * @param msg The error message for the exception.
     **/
    public InvalidValueWarning(String name, Object value, String msg) {
	super(String.format("%s has an invalid value of %s.  %s",
			    name, value.toString(), msg));
	this.value = value;
    }
    
    /**
     * Create a new instance of an InvalidValueWarning.
     * @param name The name of the variable that is invalid.
     * @param value The invalid value of the variable.
     * @param lower The lower boundary allowed for the value.
     * @param upper The upper boundary allowed for the value.
     **/
    public InvalidValueWarning(String name, Object value, Object lower, Object upper) {
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
