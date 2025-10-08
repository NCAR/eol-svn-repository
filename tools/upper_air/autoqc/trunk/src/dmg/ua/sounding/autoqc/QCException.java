package dmg.ua.sounding.autoqc;

import dmg.util.DefaultException;

/**
 * <p>The QCException is a DefaultException thrown by the automatic QC program
 * when there is a fatal problem during the QC process.</p>
 * 
 * @author Joel Clawson
 */
public class QCException extends DefaultException {

	private static final long serialVersionUID = 2620215400320563364L;

	/**
	 * Create a new instance of a QCException.
	 * @param msg The message generated for this exception.
	 */
	public QCException(String msg) { super(msg); }
}
