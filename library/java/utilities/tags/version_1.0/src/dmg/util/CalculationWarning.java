package dmg.util;

/**
 * <p>The CalculationWarning is a DefaultWarning that is thrown when a 
 * calculation cannot be completed, but is not a full blown error and can
 * continue.  If the problem is a full blown error, the CalculationException
 * should be used.</p>
 * @see CalculationException
 *
 * @author Joel Clawson
 **/
public class CalculationWarning extends DefaultWarning {

    private static final long serialVersionUID = 1123458047739748995L;
    
    /**
     * Create a new instance of a CalculationWarning.
     * @param msg The error message for the exception.
     **/
    public CalculationWarning(String msg) { super(msg); }
}
