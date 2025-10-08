package dln.util;

import dln.beans.*;

/**
 * The UserActiveComparator is a Comparator to determine the sort order
 * of UserBean instances based on the natural sort order of the UserBeans'
 * active status (and their short names if they have the same status).
 * 
 * @author jclawson
 */
public class UserActiveComparator extends UserComparator {

	/**
	 * Create a new instance of a UserActiveComparator.
	 * @param reverse if the comparator should use a reverse sort during the comparison.
	 */
	public UserActiveComparator(boolean reverse) { super(reverse); }

	/**
	 * Compare two UserBeans for sort order based on their active status.
	 * @param first The first UserBean to be compared.
	 * @param second The second UserBean to be compared.
	 * @return A negative integer, zero, or a positive integer if the first bean is
	 * less than, equal to, or greater than the second bean.
	 */
	public int compare(UserBean first, UserBean second) {
		if (first.isActive() == second.isActive()) {
			return isReverseSort() ? second.getShortName().compareTo(first.getShortName()) :
				first.getShortName().compareTo(second.getShortName());
		} 
		else if ((!second.isActive() && !isReverseSort()) || (!first.isActive() && isReverseSort())) { return -1; }
		else { return 1; }
	}
}
