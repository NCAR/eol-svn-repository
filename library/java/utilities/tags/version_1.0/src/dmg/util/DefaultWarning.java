package dmg.util;

/**
 * <p>The DefaultWarning is a special form of Exception that is thrown as
 * a warning message and not a general Exception.  It is not a RuntimeException
 * because it needs to be handled like a general Exception, but probably will
 * not require any special handling (except maybe for logging purposes).</p>
 *
 * @author Joel Clawson
 **/
public class DefaultWarning extends Exception {

    private static final long serialVersionUID = -7406947855376265616L;
 
    /**
     * Create a new instance of a DefaultWarning.
     **/
    public DefaultWarning() { super(); }
    
    /**
     * Create a new instance of a DefaultWarning.
     * @param msg The error message for the warning.
     **/
    public DefaultWarning(String msg) { super(msg); }
}
