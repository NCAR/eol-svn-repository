package dln.util;

import dln.beans.*;

/**
 * The DatasetNameComparator is a Comparator to determine the sort order
 * of DatasetBean instances based on the natural sort order of the DatasetBeans'
 * name/title.
 * 
 * @author jclawson
 */
public class DatasetNameComparator extends DatasetComparator {

	/**
	 * Create a new instance of a DatasetNameComparator.
	 * @param reverse if the comparator should use a reverse sort during the comparison.
	 */
	public DatasetNameComparator(boolean reverse) { super(reverse, null); }

	/**
	 * Compare two DatasetBeans for sort order based on their names/titles.
	 * @param first The first DatasetBean to be compared.
	 * @param second The second DatasetBean to be compared.
	 * @return A negative integer, zero, or a positive integer if the first bean is
	 * less than, equal to, or greater than the second bean.
	 */
	public int compare(DatasetBean first, DatasetBean second) {
		return isReverseSort() ? second.getName().compareTo(first.getName()) :
			first.getName().compareTo(second.getName());
	}

}
