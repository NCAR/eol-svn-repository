package dln.util;

import dln.beans.*;
import java.util.*;

/**
 * The DatasetComparator is a generic Comparator to determine the sort order
 * of DatasetBean instances.
 * 
 * @author jclawson
 */
public abstract class DatasetComparator extends DefaultComparator<DatasetBean> {
	
	private Map<Integer, UserBean> users;
	
	/**
	 * Create a new instance of a DatasetComparator.
	 * @param reverse if the comparator should use a reverse sort during the comparison.
	 * @param users The mapping of users to use when comparing contacts.
	 */
	public DatasetComparator(boolean reverse, Map<Integer, UserBean> users) {
		super(reverse);
		this.users = users;
	}
	
	/**
	 * Get the mapping of UserBean instances for contact comparisons.
	 * @return The user mapping.
	 */
	protected Map<Integer, UserBean> getUserMap() { return users; }
}
