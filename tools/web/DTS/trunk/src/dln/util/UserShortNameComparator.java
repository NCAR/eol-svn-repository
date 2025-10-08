package dln.util;

import dln.beans.*;

/**
 * The UserShortNameComparator is a Comparator to determine the sort order
 * of UserBean instances based on the natural sort order of the UserBeans'
 * short names.
 * 
 * @author jclawson
 */
public class UserShortNameComparator extends UserComparator {

	/**
	 * Create a new instance of a UserShortNameComparator.
	 * @param reverse if the comparator should use a reverse sort during the comparison.
	 */
	public UserShortNameComparator(boolean reverse) { super(reverse); }

	/**
	 * Compare two UserBeans for sort order based on their short names.
	 * @param first The first UserBean to be compared.
	 * @param second The second UserBean to be compared.
	 * @return A negative integer, zero, or a positive integer if the first bean is
	 * less than, equal to, or greater than the second bean.
	 */
	public int compare(UserBean first, UserBean second) {
		return isReverseSort() ? second.getShortName().compareTo(first.getShortName()) :
			first.getShortName().compareTo(second.getShortName());
	}
}
