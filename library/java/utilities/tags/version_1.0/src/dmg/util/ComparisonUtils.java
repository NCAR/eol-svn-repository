package dmg.util;

import java.util.*;

/**
 * <p>The ComparisonUtils class is a collection of functions used for
 * comparing values.  These functions handle <code>null</code> values
 * without throwing NullPointerExceptions on the objects that will be
 * compared.  A <code>null</code> value is considered to be the smallest
 * possible value.</p>
 *
 * @author Joel Clawson
 */
public final class ComparisonUtils {

	/**
	 * This constructor is private to prevent someone from actually creating
	 * an instance of this class.
	 */
	private ComparisonUtils() {}
	
	/**
	 * Compare two Calendars for sort order while allowing for <code>null</code> values.  A
	 * <code>null</code> value is considered the smallest possible value.
	 * @param first The first value to be compared.
	 * @param second The second value to be compared.
	 * @return A negative number, zero, or positive number if the first value is less,
	 * equal to, or greater than the second value.
	 */
	public static int compare(Calendar first, Calendar second) {
		if (first == null && second == null) { return 0; }
		else if (first == null && second != null) { return -1; }
		else if (first != null && second == null) { return 1; }
		else { return first.compareTo(second); }
	}
	
	/**
	 * Compare two doubles for sort order while allowing for <code>null</code> values.  A
	 * <code>null</code> value is considered the smallest possible value.
	 * @param first The first value to be compared.
	 * @param second The second value to be compared.
	 * @return A negative number, zero, or positive number if the first value is less,
	 * equal to, or greater than the second value.
	 */
	public static int compare(Double first, Double second) {
		if (first == null && second == null) { return 0; }
		else if (first == null && second != null) { return -1; }
		else if (first != null && second == null) { return 1; }
		else { return first.compareTo(second); }
	}

	/**
	 * Compare two strings for sort order while allowing for <code>null</code> values.  A
	 * <code>null</code> value is considered the smallest possible value.
	 * @param first The first value to be compared.
	 * @param second The second value to be compared.
	 * @return A negative number, zero, or positive number if the first value is less,
	 * equal to, or greater than the second value.
	 */
	public static int compare(String first, String second) {
		if (first == null && second == null) { return 0; }
		else if (first == null && second != null) { return -1; }
		else if (first != null && second == null) { return 1; }
		else { return first.compareTo(second); }
	}
}
