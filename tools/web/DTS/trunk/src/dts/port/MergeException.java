package dts.port;

/**
 * The MergeException class is a special exception used during the porting of database
 * when information is being combined into single sets and values do not align properly.
 * 
 * @author jclawson
 */
public class MergeException extends Exception {

	private static final long serialVersionUID = -1724144089829867420L;

	/**
	 * Create a new instance of a MergeException.
	 * @param msg The error message that caused this exception.
	 */
	public MergeException(String msg) { super(msg); }
}
