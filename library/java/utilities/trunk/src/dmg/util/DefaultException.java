package dmg.util;

/**
 * <p>The DefaultException is an Exception that guarantees that the exception's
 * message will never be <code>null</code>.  Any <code>null</code> message is
 * converted to an empty String.</p>
 * 
 * @author Joel Clawson
 */
public class DefaultException extends Exception {

	private static final long serialVersionUID = -5567012476373026064L;

    /**
     * Get a new instance of a DefaultException.
     */
    public DefaultException() { super(); }

    /**
     * Get a new instance of a DefaultException.
     * @param msg The message for the exception.
     */
    public DefaultException(String msg) { super(msg); }
    
    /**
     * Get the message for the exception.
     * @return The exception's message or the empty String if no message
     * was given.
     */
    @Override
    public String getMessage() {
            String msg = super.getMessage();
            return msg == null ? "" : msg;
    }
}