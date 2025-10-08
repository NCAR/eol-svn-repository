package dln.util;

import dln.beans.*;
import java.util.*;

/**
 * The DatasetLoaderComparator is a Comparator to determine the sort order
 * of DatasetBean instances based on the natural sort order of the DatasetBeans'
 * load contact.
 * 
 * @author jclawson
 */
public class DatasetLoaderComparator extends DatasetComparator {

	/**
	 * Create a new instance of a DatasetLoaderComparator.
	 * @param reverse if the comparator should use a reverse sort during the comparison.
	 * @param users the mapping of the users to use when comparing the load contacts.
	 */
	public DatasetLoaderComparator(boolean reverse, Map<Integer, UserBean> users) { super(reverse, users); }

	/**
	 * Compare two DatasetBeans for sort order based on their load contacts.
	 * @param first The first DatasetBean to be compared.
	 * @param second The second DatasetBean to be compared.
	 * @return A negative integer, zero, or a positive integer if the first bean is
	 * less than, equal to, or greater than the second bean.
	 */
	public int compare(DatasetBean first, DatasetBean second) {
		if (first.getLoader(getUserMap()) == null) { return 1; }
		else if (second.getLoader(getUserMap()) == null) { return -1; }
		else {
			return isReverseSort() ? second.getLoader(getUserMap()).compareTo(first.getLoader(getUserMap())) :
				first.getLoader(getUserMap()).compareTo(second.getLoader(getUserMap()));
		}
	}
}
