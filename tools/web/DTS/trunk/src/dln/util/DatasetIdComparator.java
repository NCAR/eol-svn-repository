package dln.util;

import dln.beans.*;

/**
 * The DatasetNameComparator is a Comparator to determine the sort order
 * of DatasetBean instances based on the natural sort order of the DatasetBeans'
 * data set ID.
 * 
 * @author jclawson
 */
public class DatasetIdComparator extends DatasetComparator {

	/**
	 * Create a new instance of a DatasetIdComparator.
	 * @param reverse if the comparator should use a reverse sort during the comparison.
	 */
	public DatasetIdComparator(boolean reverse) { super(reverse, null); }

	/**
	 * Compare two DatasetBeans for sort order based on their data set IDs.
	 * @param first The first DatasetBean to be compared.
	 * @param second The second DatasetBean to be compared.
	 * @return A negative integer, zero, or a positive integer if the first bean is
	 * less than, equal to, or greater than the second bean.
	 */
	public int compare(DatasetBean first, DatasetBean second) {
		return isReverseSort() ? second.getDatasetId().compareTo(first.getDatasetId()) :
			first.getDatasetId().compareTo(second.getDatasetId());
	}

}
