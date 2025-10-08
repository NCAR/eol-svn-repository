package dln.util;

import dln.beans.*;

/**
 * The DatasetQuestionsComparator is a Comparator to determine the sort order
 * of DatasetBean instances based on the natural sort order of the DatasetBeans'
 * questions/issues status.
 * 
 * @author jclawson
 */
public class DatasetQuestionsComparator extends DatasetComparator {

	/**
	 * Create a new instance of a DatasetQuestionsComparator.
	 * @param reverse if the comparator should use a reverse sort during the comparison.
	 */
	public DatasetQuestionsComparator(boolean reverse) { super(reverse, null); }

	/**
	 * Compare two DatasetBeans for sort order based on their questions status.
	 * @param first The first DatasetBean to be compared.
	 * @param second The second DatasetBean to be compared.
	 * @return A negative integer, zero, or a positive integer if the first bean is
	 * less than, equal to, or greater than the second bean.
	 */
	public int compare(DatasetBean first, DatasetBean second) {
		if (first.hasQuestions() == second.hasQuestions()) {
			return isReverseSort() ? second.compareTo(first) : first.compareTo(second);
		} else {
			return isReverseSort() ? (first.hasQuestions() ? 1 : -1) : (first.hasQuestions() ? -1 : 1);
		}
	}
}