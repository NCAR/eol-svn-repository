package dmg.ua.sounding.autoqc;

/**
 * <p>The QCRuntimeException is a specialized RuntimeException used by the
 * auto QC program.  This is thrown when a sounding has some sort of problem
 * and cannot be QC'ed, but should continue processing the remaining soundings
 * without crashing.</p>
 * 
 * @author Joel Clawson
 */
public class QCRuntimeException extends RuntimeException {

	private static final long serialVersionUID = -9017914186974151892L;

	/**
	 * Create a new instance of a QCRuntimeException.
	 * @param msg The message generated for the exception.
	 */
	public QCRuntimeException(String msg) { super(msg); }
}
