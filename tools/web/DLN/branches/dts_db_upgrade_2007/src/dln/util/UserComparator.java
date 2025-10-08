package dln.util;

import dln.beans.*;

/**
 * The UserComparator is a generic Comparator to determine the sort order
 * of UserBean instances.
 * 
 * @author jclawson
 */
public abstract class UserComparator extends DefaultComparator<UserBean> {

	/**
	 * Create a new instance of a UserComparator.
	 * @param reverse if the comparator should use a reverse sort during the comparison.
	 */
	public UserComparator(boolean reverse) { super(reverse); }
}
