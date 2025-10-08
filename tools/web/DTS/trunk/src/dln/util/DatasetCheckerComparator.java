package dln.util;

import dln.beans.*;
import java.util.*;

/**
 * The DatasetNameComparator is a Comparator to determine the sort order
 * of DatasetBean instances based on the natural sort order of the DatasetBeans'
 * check contact.
 * 
 * @author jclawson
 */
public class DatasetCheckerComparator extends DatasetComparator {

	/**
	 * Create a new instance of a DatasetCheckerComparator.
	 * @param reverse if the comparator should use a reverse sort during the comparison.
	 * @param users the mapping of the users to use when comparing the check contacts.
	 */
	public DatasetCheckerComparator(boolean reverse, Map<Integer, UserBean> users) { super(reverse, users); }

	/**
	 * Compare two DatasetBeans for sort order based on their check contacts.
	 * @param first The first DatasetBean to be compared.
	 * @param second The second DatasetBean to be compared.
	 * @return A negative integer, zero, or a positive integer if the first bean is
	 * less than, equal to, or greater than the second bean.
	 */
	public int compare(DatasetBean first, DatasetBean second) {
		if (first.getChecker(getUserMap()) == null) { return 1; }
		else if (second.getChecker(getUserMap()) == null) { return -1; }
		else {
			return isReverseSort() ? second.getChecker(getUserMap()).compareTo(first.getChecker(getUserMap())) :
				first.getChecker(getUserMap()).compareTo(second.getChecker(getUserMap()));
		}
	}
}
