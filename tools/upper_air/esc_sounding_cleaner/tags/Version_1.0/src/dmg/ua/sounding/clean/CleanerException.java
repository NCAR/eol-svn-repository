package dmg.ua.sounding.clean;

import dmg.util.*;

/**
 * The CleanerException is a wrapper for DefaultExceptions and DefaultWarnings when being used
 * by the ESCSoundingCleaner.  It is a mechanism to make the exception handling easier, especially
 * when most of the exceptions thrown should never occur.
 * 
 * @author Joel Clawson
 */
public class CleanerException extends DefaultException {

	private static final long serialVersionUID = -3345832881271275235L;

	/**
	 * Create a new instance of a CleanerException.
	 * @param task The name of the task being done that caused the exception.
	 * @param e The exception that is being wrapped into a CleanerException.
	 */
	public CleanerException(String task, DefaultException e) {
		super(String.format("Unable to %s because of a %s.  %s", task, e.getClass().getName(), e.getMessage()));
	}
	
	/**
	 * Create a new instance of a CleanerException.
	 * @param task The name of the task being done that caused the warning.
	 * @param e The warning that is being wrapped into a CleanerException.
	 */
	public CleanerException(String task, DefaultWarning e) {
		super(String.format("Unable to %s because of a %s.  %s", task, e.getClass().getName(), e.getMessage()));
	}
}
