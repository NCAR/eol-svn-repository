package dln.util;

import dln.beans.*;

/**
 * The UserEmailComparator is a Comparator to determine the sort order
 * of UserBean instances based on the natural sort order of the UserBeans'
 * email addresses.
 * 
 * @author jclawson
 */
public class UserEmailComparator extends UserComparator {

	/**
	 * Create a new instance of an UserEmailComparator.
	 * @param reverse if the comparator should use a reverse sort during the comparison.
	 */
	public UserEmailComparator(boolean reverse) { super(reverse); }

	/**
	 * Compare two UserBeans for sort order based on their email addresses.
	 * @param first The first UserBean to be compared.
	 * @param second The second UserBean to be compared.
	 * @return A negative integer, zero, or a positive integer if the first bean is
	 * less than, equal to, or greater than the second bean.
	 */
	public int compare(UserBean first, UserBean second) {
		return isReverseSort() ? second.getEmail().compareTo(first.getEmail()) :
			first.getEmail().compareTo(second.getEmail());
	}
}
