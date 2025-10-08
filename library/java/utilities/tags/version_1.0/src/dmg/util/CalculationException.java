package dmg.util;

/**
 * <p>The CalculationException is a DefaultException that is to be used when there
 * is a problem with the calculation of a value.  This is a critical error that 
 * should cause a halt.  There is also a CalculationWarning that is a type of
 * RuntimeException that will allow the program to continue to run when it is
 * thrown.</p>
 * @see CalculationWarning
 *
 * @author Joel Clawson
 **/
public class CalculationException extends DefaultException {
    
    private static final long serialVersionUID = -7805865781186298147L;
    
    /**
     * Create a new instance of a CalculationException.
     * @param function The name of the function that threw the exception.
     * @param param The parameter of the calcuation that caused the exception.
     * @param msg The error message for the exception.
     **/
    public CalculationException(String function, String param, String msg) {
	super(String.format("Argument %s to %s is invalid.  %s", param, function, msg));
    }
}
