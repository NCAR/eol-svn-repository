package dmg.ml;

/**
 * <p>The MasterListException is a specialized Exception for problems that 
 * occur in the Master List.</p>
 *
 * @author Joel Clawson
 */
public class MasterListException extends java.lang.Exception {

    private Exception exception;
    
    /**
     * Creates a new instance of <code>MasterListException</code> without detail
     * message.
     */
    public MasterListException() {}
    
    /**
     * Constructs an instance of <code>MasterListException</code> with the
     * specified detail message.
     * @param msg the detail message.
     */
    public MasterListException(String msg) { super(msg); }
    
    /**
     * Constructs an instance of <code>MasterListException</code> with the
     * specified detail message.
     * @param msg the detail message.
     * @param e The exception being piped through this exception.
     */
    public MasterListException(String msg, Exception e) {
        super(msg);
        this.exception = e;
    }
    
    /**
     * Get the message details of the exception that generated the 
     * MasterListException
     * @return The message of the exception or the empty String if this 
     * exception was not caused by a seperate exception.
     **/
    public String getDetails() {
        return exception == null ? "" : exception.getMessage();
    }
    
    /**
     * Get the type of exception that generated this MasterListException.
     * @return The name of the exception class that generated this exception.
     **/
    public String getType() {
        return exception == null ? getClass().getSimpleName() : 
            exception.getClass().getSimpleName();
    }
}