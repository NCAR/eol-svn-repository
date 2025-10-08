package dmg.util;

/**
 * The RestrictedOperationException is a DefaultException that is thrown by a Restrictor
 * function.  It is used as a notification to a user that a change cannot be made
 * to the Restricted object.
 *
 * @author Joel Clawson
 */
public class RestrictedOperationException extends DefaultException {

	private static final long serialVersionUID = 2603313221905046422L;

	/**
	 * Create a new instance of a RestrictedOperationException.
	 * @param msg The error message.
	 */
	public RestrictedOperationException(String msg) { super(msg); }
}
