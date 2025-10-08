package dmg.util;

/**
 * The InvalidFlagException is a DefaultException that is to be thrown
 * when there is a mismatch between a value and a flag.
 *
 * @author Joel Clawson
 */
public class InvalidFlagException extends DefaultException {

	private static final long serialVersionUID = 3579394437199407569L;

	/**
	 * Create a new instance of an InvalidFlagException.
	 * @param function The name of the function that threw the exception.
	 * @param value The value for the flag.
	 * @param flag The flag for the value.
	 * @param comment The comment/message why the exception was thrown.
	 */
	public InvalidFlagException(String function, Double value, Double flag, 
			String comment) {
		super(String.format("There was a value/flag mismatch in %s with " +
				"(%f,%f).  %s",function,value,flag,comment));
	}
}
