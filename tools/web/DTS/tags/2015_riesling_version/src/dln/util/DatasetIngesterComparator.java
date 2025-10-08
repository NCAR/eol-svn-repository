package dln.util;

import dln.beans.*;
import java.util.*;

/**
 * The DatasetNameComparator is a Comparator to determine the sort order
 * of DatasetBean instances based on the natural sort order of the DatasetBeans'
 * ingest contact.
 * 
 * @author jclawson
 */
public class DatasetIngesterComparator extends DatasetComparator {

	/**
	 * Create a new instance of a DatasetIngesterComparator.
	 * @param reverse if the comparator should use a reverse sort during the comparison.
	 * @param users the mapping of the users to use when comparing the ingest contacts.
	 */
	public DatasetIngesterComparator(boolean reverse, Map<Integer, UserBean> users) { super(reverse, users); }

	/**
	 * Compare two DatasetBeans for sort order based on their ingest contacts.
	 * @param first The first DatasetBean to be compared.
	 * @param second The second DatasetBean to be compared.
	 * @return A negative integer, zero, or a positive integer if the first bean is
	 * less than, equal to, or greater than the second bean.
	 */
	public int compare(DatasetBean first, DatasetBean second) {
		if (first.getIngester(getUserMap()) == null) { return 1; }
		else if (second.getIngester(getUserMap()) == null) { return -1; }
		else {
			return isReverseSort() ? second.getIngester(getUserMap()).compareTo(first.getIngester(getUserMap())) :
				first.getIngester(getUserMap()).compareTo(second.getIngester(getUserMap()));
		}
	}
}
